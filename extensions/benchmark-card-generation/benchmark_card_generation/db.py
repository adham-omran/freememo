from __future__ import annotations

import json
import sqlite3
from pathlib import Path
from typing import Any


SCHEMA = """
PRAGMA foreign_keys = ON;
PRAGMA journal_mode = WAL;

CREATE TABLE IF NOT EXISTS runs (
  id INTEGER PRIMARY KEY,
  run_uuid TEXT NOT NULL UNIQUE,
  started_at TEXT NOT NULL,
  finished_at TEXT,
  status TEXT NOT NULL CHECK (status IN ('running', 'completed', 'failed')),
  generator_model_requested TEXT NOT NULL,
  generator_models_resolved TEXT,
  judge_model_requested TEXT NOT NULL,
  judge_models_resolved TEXT,
  dataset_id TEXT NOT NULL,
  dataset_version TEXT NOT NULL,
  dataset_sha256 TEXT NOT NULL,
  dataset_path TEXT NOT NULL,
  pipeline_id TEXT NOT NULL,
  pipeline_sha256 TEXT NOT NULL,
  utility_profile TEXT NOT NULL,
  cards_per_unit INTEGER NOT NULL,
  judge_passes INTEGER NOT NULL,
  random_seed INTEGER NOT NULL,
  unit_count INTEGER NOT NULL,
  units_completed INTEGER NOT NULL DEFAULT 0,
  mean_score REAL,
  ci_low REAL,
  ci_high REAL,
  usable_pct REAL,
  coverage REAL,
  total_cost_usd REAL NOT NULL DEFAULT 0,
  prompt_tokens INTEGER NOT NULL DEFAULT 0,
  completion_tokens INTEGER NOT NULL DEFAULT 0,
  latency_ms INTEGER NOT NULL DEFAULT 0,
  error TEXT,
  config_json TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS units (
  id INTEGER PRIMARY KEY,
  run_id INTEGER NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
  unit_id TEXT NOT NULL,
  source_id TEXT NOT NULL,
  ordinal INTEGER NOT NULL,
  cards_requested INTEGER NOT NULL,
  cards_returned INTEGER NOT NULL,
  score REAL,
  generation_response_json TEXT NOT NULL,
  UNIQUE(run_id, unit_id)
);

CREATE TABLE IF NOT EXISTS cards (
  id INTEGER PRIMARY KEY,
  unit_row_id INTEGER NOT NULL REFERENCES units(id) ON DELETE CASCADE,
  ordinal INTEGER NOT NULL,
  front TEXT NOT NULL,
  back TEXT NOT NULL,
  tier TEXT NOT NULL CHECK (tier IN ('T0', 'T1', 'T2', 'T3')),
  utility REAL NOT NULL,
  criteria_json TEXT NOT NULL,
  UNIQUE(unit_row_id, ordinal)
);

CREATE TABLE IF NOT EXISTS judgments (
  id INTEGER PRIMARY KEY,
  card_id INTEGER NOT NULL REFERENCES cards(id) ON DELETE CASCADE,
  pass_number INTEGER NOT NULL,
  tier TEXT NOT NULL CHECK (tier IN ('T0', 'T1', 'T2', 'T3')),
  rationale TEXT NOT NULL,
  closest_reference TEXT,
  criteria_json TEXT NOT NULL,
  response_json TEXT NOT NULL,
  UNIQUE(card_id, pass_number)
);

CREATE TABLE IF NOT EXISTS api_calls (
  id INTEGER PRIMARY KEY,
  run_id INTEGER NOT NULL REFERENCES runs(id) ON DELETE CASCADE,
  phase TEXT NOT NULL CHECK (phase IN ('generate', 'judge')),
  unit_id TEXT NOT NULL,
  card_ordinal INTEGER,
  pass_number INTEGER,
  model_requested TEXT NOT NULL,
  model_resolved TEXT,
  provider TEXT,
  generation_id TEXT,
  cost_usd REAL NOT NULL,
  prompt_tokens INTEGER NOT NULL,
  completion_tokens INTEGER NOT NULL,
  latency_ms INTEGER NOT NULL,
  response_json TEXT,
  parse_error TEXT
);

CREATE INDEX IF NOT EXISTS runs_history
ON runs(dataset_sha256, generator_model_requested, started_at DESC);
"""


class ResultsDatabase:
    def __init__(self, path: str | Path) -> None:
        self.path = Path(path).expanduser().resolve()
        self.path.parent.mkdir(parents=True, exist_ok=True)
        self.connection = sqlite3.connect(self.path)
        self.connection.row_factory = sqlite3.Row
        self.connection.executescript(SCHEMA)
        self._ensure_column("api_calls", "response_json", "TEXT")
        self._ensure_column("api_calls", "parse_error", "TEXT")

    def _ensure_column(self, table: str, column: str, declaration: str) -> None:
        columns = {
            row["name"] for row in self.connection.execute(f"PRAGMA table_info({table})")
        }
        if column not in columns:
            self.connection.execute(
                f"ALTER TABLE {table} ADD COLUMN {column} {declaration}"
            )
            self.connection.commit()

    def close(self) -> None:
        self.connection.close()

    def __enter__(self) -> "ResultsDatabase":
        return self

    def __exit__(self, *_: object) -> None:
        self.close()

    def insert_run(self, values: dict[str, Any]) -> int:
        columns = ", ".join(values)
        placeholders = ", ".join("?" for _ in values)
        cursor = self.connection.execute(
            f"INSERT INTO runs ({columns}) VALUES ({placeholders})",
            tuple(values.values()),
        )
        self.connection.commit()
        return int(cursor.lastrowid)

    def update_run(self, run_id: int, values: dict[str, Any]) -> None:
        assignments = ", ".join(f"{column} = ?" for column in values)
        self.connection.execute(
            f"UPDATE runs SET {assignments} WHERE id = ?",
            (*values.values(), run_id),
        )
        self.connection.commit()

    def insert_unit(self, values: dict[str, Any]) -> int:
        columns = ", ".join(values)
        placeholders = ", ".join("?" for _ in values)
        cursor = self.connection.execute(
            f"INSERT INTO units ({columns}) VALUES ({placeholders})",
            tuple(values.values()),
        )
        self.connection.commit()
        return int(cursor.lastrowid)

    def insert_card(self, values: dict[str, Any]) -> int:
        columns = ", ".join(values)
        placeholders = ", ".join("?" for _ in values)
        cursor = self.connection.execute(
            f"INSERT INTO cards ({columns}) VALUES ({placeholders})",
            tuple(values.values()),
        )
        self.connection.commit()
        return int(cursor.lastrowid)

    def insert_judgment(self, values: dict[str, Any]) -> None:
        columns = ", ".join(values)
        placeholders = ", ".join("?" for _ in values)
        self.connection.execute(
            f"INSERT INTO judgments ({columns}) VALUES ({placeholders})",
            tuple(values.values()),
        )
        self.connection.commit()

    def insert_api_call(self, values: dict[str, Any]) -> int:
        columns = ", ".join(values)
        placeholders = ", ".join("?" for _ in values)
        cursor = self.connection.execute(
            f"INSERT INTO api_calls ({columns}) VALUES ({placeholders})",
            tuple(values.values()),
        )
        self.connection.commit()
        return int(cursor.lastrowid)

    def update_api_call(self, call_id: int, values: dict[str, Any]) -> None:
        assignments = ", ".join(f"{column} = ?" for column in values)
        self.connection.execute(
            f"UPDATE api_calls SET {assignments} WHERE id = ?",
            (*values.values(), call_id),
        )
        self.connection.commit()

    def delete_incomplete_units(self, run_id: int) -> None:
        self.connection.execute(
            "DELETE FROM units WHERE run_id = ? AND score IS NULL", (run_id,)
        )
        self.connection.commit()

    def completed_units(self, run_id: int) -> list[sqlite3.Row]:
        return self.connection.execute(
            """
            SELECT * FROM units
            WHERE run_id = ? AND score IS NOT NULL
            ORDER BY ordinal
            """,
            (run_id,),
        ).fetchall()

    def completed_tiers(self, run_id: int) -> list[str]:
        rows = self.connection.execute(
            """
            SELECT cards.tier
            FROM cards
            JOIN units ON units.id = cards.unit_row_id
            WHERE units.run_id = ? AND units.score IS NOT NULL
            ORDER BY units.ordinal, cards.ordinal
            """,
            (run_id,),
        ).fetchall()
        return [str(row["tier"]) for row in rows]

    def api_totals(self, run_id: int) -> dict[str, int | float]:
        row = self.connection.execute(
            """
            SELECT
              COALESCE(SUM(cost_usd), 0) AS total_cost_usd,
              COALESCE(SUM(prompt_tokens), 0) AS prompt_tokens,
              COALESCE(SUM(completion_tokens), 0) AS completion_tokens,
              COALESCE(SUM(latency_ms), 0) AS latency_ms
            FROM api_calls
            WHERE run_id = ?
            """,
            (run_id,),
        ).fetchone()
        return dict(row)

    def resolved_models(self, run_id: int, phase: str) -> set[str]:
        rows = self.connection.execute(
            """
            SELECT DISTINCT model_resolved
            FROM api_calls
            WHERE run_id = ? AND phase = ? AND model_resolved IS NOT NULL
            """,
            (run_id, phase),
        ).fetchall()
        return {str(row["model_resolved"]) for row in rows}

    def previous_run(self, run_id: int) -> sqlite3.Row | None:
        return self.connection.execute(
            """
            SELECT previous.*
            FROM runs current
            JOIN runs previous
              ON previous.dataset_sha256 = current.dataset_sha256
             AND previous.generator_model_requested = current.generator_model_requested
             AND previous.judge_model_requested = current.judge_model_requested
             AND previous.pipeline_sha256 = current.pipeline_sha256
             AND previous.utility_profile = current.utility_profile
             AND previous.status = 'completed'
             AND previous.id < current.id
            WHERE current.id = ?
            ORDER BY previous.id DESC
            LIMIT 1
            """,
            (run_id,),
        ).fetchone()

    def run(self, run_id: int) -> sqlite3.Row | None:
        return self.connection.execute("SELECT * FROM runs WHERE id = ?", (run_id,)).fetchone()

    def history(
        self, model: str | None, limit: int, dataset_filter: str | None = None
    ) -> list[sqlite3.Row]:
        conditions: list[str] = []
        params: list[Any] = []
        if model:
            conditions.append("generator_model_requested = ?")
            params.append(model)
        if dataset_filter:
            conditions.append("instr(lower(dataset_id), lower(?)) > 0")
            params.append(dataset_filter)
        where = f"WHERE {' AND '.join(conditions)}" if conditions else ""
        params.append(limit)
        return self.connection.execute(
            f"SELECT * FROM runs {where} ORDER BY started_at DESC LIMIT ?", params
        ).fetchall()

    def leaderboard(self, dataset_filter_value: str | None) -> list[sqlite3.Row]:
        params: tuple[Any, ...] = ()
        dataset_filter = ""
        if dataset_filter_value:
            dataset_filter = "AND instr(lower(r.dataset_id), lower(?)) > 0"
            params = (dataset_filter_value,)
        return self.connection.execute(
            f"""
            SELECT r.*
            FROM runs r
            WHERE r.status = 'completed'
              {dataset_filter}
              AND r.id = (
                SELECT r2.id FROM runs r2
                WHERE r2.status = 'completed'
                  AND r2.dataset_sha256 = r.dataset_sha256
                  AND r2.generator_model_requested = r.generator_model_requested
                  AND r2.judge_model_requested = r.judge_model_requested
                  AND r2.pipeline_sha256 = r.pipeline_sha256
                  AND r2.utility_profile = r.utility_profile
                ORDER BY r2.started_at DESC LIMIT 1
              )
            ORDER BY r.mean_score DESC
            """,
            params,
        ).fetchall()

    def export_run(self, run_id: int) -> dict[str, Any] | None:
        run = self.run(run_id)
        if run is None:
            return None
        units = self.connection.execute(
            "SELECT * FROM units WHERE run_id = ? ORDER BY ordinal", (run_id,)
        ).fetchall()
        result = dict(run)
        result["config"] = json.loads(result.pop("config_json"))
        result["units"] = []
        for unit in units:
            unit_dict = dict(unit)
            cards = self.connection.execute(
                "SELECT * FROM cards WHERE unit_row_id = ? ORDER BY ordinal", (unit["id"],)
            ).fetchall()
            unit_dict["generation_response"] = json.loads(unit_dict.pop("generation_response_json"))
            unit_dict["cards"] = []
            for card in cards:
                card_dict = dict(card)
                card_dict["criteria"] = json.loads(card_dict.pop("criteria_json"))
                judgments = self.connection.execute(
                    "SELECT * FROM judgments WHERE card_id = ? ORDER BY pass_number", (card["id"],)
                ).fetchall()
                card_dict["judgments"] = [dict(judgment) for judgment in judgments]
                for judgment in card_dict["judgments"]:
                    judgment["criteria"] = json.loads(judgment.pop("criteria_json"))
                    judgment["response"] = json.loads(judgment.pop("response_json"))
                unit_dict["cards"].append(card_dict)
            result["units"].append(unit_dict)
        return result
