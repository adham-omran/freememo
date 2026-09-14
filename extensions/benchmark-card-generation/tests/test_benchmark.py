from __future__ import annotations

import json
import io
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT))

from benchmark_card_generation.dataset import DatasetError, load_dataset
from benchmark_card_generation.cli import _parser, _print_models, _print_runs
from benchmark_card_generation.db import ResultsDatabase
from benchmark_card_generation.estimate import display_prices, estimate_cost
from benchmark_card_generation.openrouter import Completion, OpenRouterError, parse_json_object
from benchmark_card_generation.runner import RunConfig, run_benchmark
from benchmark_card_generation.scoring import completion_score, consensus_tier
from benchmark_card_generation.srs_highlights import (
    dataset_document,
    import_srs_highlights,
    parse_reference_content,
)


class FakeClient:
    def __init__(self) -> None:
        self.calls = 0

    def chat(self, model, messages, temperature, max_tokens):
        self.calls += 1
        is_generation = any("Write exactly" in message["content"] for message in messages)
        if is_generation:
            content = json.dumps(
                {"cards": [{"front": "A stable question?", "back": "A stable answer."}]}
            )
        else:
            content = json.dumps(
                {
                    "tier": "T3",
                    "rationale": "The card cues one stable answer.",
                    "closest_reference": "titan-t3",
                    "criteria": {},
                }
            )
        response = {
            "id": f"fake-{self.calls}",
            "model": model,
            "provider": "test-provider",
            "choices": [{"message": {"content": content}}],
            "usage": {"prompt_tokens": 10, "completion_tokens": 5, "cost": 0.001},
        }
        return Completion(
            content=content,
            response=response,
            generation_id=response["id"],
            resolved_model=model,
            provider="test-provider",
            prompt_tokens=10,
            completion_tokens=5,
            cost_usd=0.001,
            latency_ms=2,
        )


class TruncatingClient(FakeClient):
    def __init__(self) -> None:
        super().__init__()
        self.generation_calls = 0

    def chat(self, model, messages, temperature, max_tokens):
        is_generation = any("Write exactly" in message["content"] for message in messages)
        if not is_generation:
            return super().chat(model, messages, temperature, max_tokens)
        self.calls += 1
        self.generation_calls += 1
        if self.generation_calls == 1:
            content = '{\n  "cards": [\n    {"front": "A question?", "back": "unterminated'
            completion_tokens = max_tokens
        else:
            content = json.dumps(
                {"cards": [{"front": "A stable question?", "back": "A stable answer."}]}
            )
            completion_tokens = 5
        response = {
            "id": f"fake-{self.calls}",
            "model": model,
            "provider": "test-provider",
            "choices": [
                {
                    "message": {"content": content},
                    "finish_reason": "length" if self.generation_calls == 1 else "stop",
                }
            ],
            "usage": {
                "prompt_tokens": 10,
                "completion_tokens": completion_tokens,
                "cost": 0.001,
            },
        }
        return Completion(
            content=content,
            response=response,
            generation_id=response["id"],
            resolved_model=model,
            provider="test-provider",
            prompt_tokens=10,
            completion_tokens=completion_tokens,
            cost_usd=0.001,
            latency_ms=2,
        )


class FailingSecondGenerationClient(FakeClient):
    def __init__(self) -> None:
        super().__init__()
        self.generation_calls = 0

    def chat(self, model, messages, temperature, max_tokens):
        is_generation = any("Write exactly" in message["content"] for message in messages)
        if not is_generation:
            return super().chat(model, messages, temperature, max_tokens)
        self.generation_calls += 1
        if self.generation_calls != 2:
            return super().chat(model, messages, temperature, max_tokens)
        self.calls += 1
        content = '{"cards":[{"front":"A question?","back":"unterminated'
        response = {
            "id": f"fake-{self.calls}",
            "model": model,
            "provider": "test-provider",
            "choices": [{"message": {"content": content}, "finish_reason": "length"}],
            "usage": {
                "prompt_tokens": 10,
                "completion_tokens": max_tokens,
                "cost": 0.001,
            },
        }
        return Completion(
            content=content,
            response=response,
            generation_id=response["id"],
            resolved_model=model,
            provider="test-provider",
            prompt_tokens=10,
            completion_tokens=max_tokens,
            cost_usd=0.001,
            latency_ms=2,
        )


class ScoringTests(unittest.TestCase):
    def test_completion_score_charges_for_all_bad_cards(self):
        self.assertEqual(-3.0, completion_score([2.0, -1.0, -1.0, -3.0]))

    def test_consensus_tie_uses_lower_tier(self):
        self.assertEqual("T1", consensus_tier(["T2", "T1"]))


class JsonTests(unittest.TestCase):
    def test_parse_fenced_json(self):
        self.assertEqual({"tier": "T3"}, parse_json_object("```json\n{\"tier\":\"T3\"}\n```"))


class CliTests(unittest.TestCase):
    def test_models_prints_a_price_table_per_million_tokens(self):
        models = [
            {
                "id": "inclusionai/ling-3.0-flash-vl",
                "name": "inclusionAI: Ling 3.0 Flash VL",
                "pricing": {
                    "prompt": "0.00000006",
                    "completion": "0.00000018",
                    "request": "0",
                },
            }
        ]
        output = io.StringIO()
        with redirect_stdout(output):
            _print_models(models)
        lines = output.getvalue().splitlines()
        self.assertIn("Input $/M", lines[0])
        self.assertIn("Output $/M", lines[0])
        self.assertIn("$0.0600", lines[2])
        self.assertIn("$0.1800", lines[2])
        self.assertNotIn("0.00000006", output.getvalue())

    def test_history_accepts_a_dataset_filter(self):
        args = _parser().parse_args(["history", "--filter", "srs-highlights"])
        self.assertEqual("srs-highlights", args.dataset_filter)

    def test_leaderboard_accepts_dataset_filter_and_old_alias(self):
        parser = _parser()
        filtered = parser.parse_args(["leaderboard", "--filter", "srs-highlights"])
        aliased = parser.parse_args(["leaderboard", "--dataset-id", "exact-id"])
        self.assertEqual("srs-highlights", filtered.dataset_filter)
        self.assertEqual("exact-id", aliased.dataset_filter)

    def test_history_prints_the_requested_judge_model(self):
        row = {
            "id": 4,
            "started_at": "2026-09-12T18:27:04+00:00",
            "status": "completed",
            "mean_score": 1.094,
            "usable_pct": 86.5,
            "total_cost_usd": 0.27553,
            "dataset_id": "dataset/test",
            "generator_model_requested": "generator/model",
            "judge_model_requested": "judge/model",
        }
        output = io.StringIO()
        with redirect_stdout(output):
            _print_runs([row])
        self.assertIn("Judge", output.getvalue().splitlines()[0])
        self.assertIn("judge/model", output.getvalue())

    def test_filtered_results_omit_the_dataset_column(self):
        row = {
            "id": 4,
            "started_at": "2026-09-12T18:27:04+00:00",
            "status": "completed",
            "mean_score": 1.094,
            "usable_pct": 86.5,
            "total_cost_usd": 0.27553,
            "dataset_id": "dataset/test",
            "generator_model_requested": "generator/model",
            "judge_model_requested": "judge/model",
        }
        output = io.StringIO()
        with redirect_stdout(output):
            _print_runs([row], include_dataset=False)
        lines = output.getvalue().splitlines()
        self.assertIn("Generator", lines[0])
        self.assertIn("Judge", lines[0])
        self.assertNotIn("Generator / Judge", lines[0])
        self.assertNotIn("Dataset", lines[0])
        self.assertEqual(set(lines[1].replace(" ", "")), {"-"})
        self.assertNotIn("dataset/test", lines[2])

    def test_leaderboard_table_can_omit_total_cost(self):
        row = {
            "id": 4,
            "started_at": "2026-09-12T18:27:04+00:00",
            "status": "completed",
            "mean_score": 1.094,
            "usable_pct": 86.5,
            "total_cost_usd": 0.27553,
            "dataset_id": "dataset/test",
            "generator_model_requested": "generator/model",
            "judge_model_requested": "judge/model",
        }
        output = io.StringIO()
        with redirect_stdout(output):
            _print_runs([row], include_dataset=False, include_cost=False)
        lines = output.getvalue().splitlines()
        self.assertNotIn("Cost", lines[0])
        self.assertNotIn("$0.27553", lines[2])


class DatabaseFilterTests(unittest.TestCase):
    @staticmethod
    def insert_run(
        database: ResultsDatabase,
        *,
        dataset_id: str,
        dataset_sha256: str,
        model: str,
        started_at: str,
        score: float,
    ) -> int:
        return database.insert_run(
            {
                "run_uuid": f"{dataset_sha256}-{model}-{started_at}",
                "started_at": started_at,
                "finished_at": started_at,
                "status": "completed",
                "generator_model_requested": model,
                "judge_model_requested": "judge/model",
                "dataset_id": dataset_id,
                "dataset_version": "1",
                "dataset_sha256": dataset_sha256,
                "dataset_path": "/tmp/dataset.json",
                "pipeline_id": "test-pipeline",
                "pipeline_sha256": "pipeline-sha",
                "utility_profile": "raw-v1",
                "cards_per_unit": 1,
                "judge_passes": 1,
                "random_seed": 1,
                "unit_count": 1,
                "units_completed": 1,
                "mean_score": score,
                "usable_pct": 100.0,
                "config_json": "{}",
            }
        )

    def test_history_filters_dataset_ids_by_case_insensitive_substring(self):
        with tempfile.TemporaryDirectory() as directory:
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                self.insert_run(
                    database,
                    dataset_id="freememo-smoke-test",
                    dataset_sha256="smoke-sha",
                    model="model/a",
                    started_at="2026-01-01T00:00:00+00:00",
                    score=2.0,
                )
                expected_id = self.insert_run(
                    database,
                    dataset_id="laddermedia/SRS-Highlights/grounded/test",
                    dataset_sha256="srs-sha",
                    model="model/b",
                    started_at="2026-01-02T00:00:00+00:00",
                    score=1.0,
                )
                rows = database.history(None, 20, "srs-highlights")
        self.assertEqual([expected_id], [row["id"] for row in rows])

    def test_leaderboard_filters_before_it_ranks_models(self):
        with tempfile.TemporaryDirectory() as directory:
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                self.insert_run(
                    database,
                    dataset_id="freememo-smoke-test",
                    dataset_sha256="smoke-sha",
                    model="model/a",
                    started_at="2026-01-01T00:00:00+00:00",
                    score=3.0,
                )
                expected_id = self.insert_run(
                    database,
                    dataset_id="laddermedia/srs-highlights/grounded/test",
                    dataset_sha256="srs-sha",
                    model="model/b",
                    started_at="2026-01-02T00:00:00+00:00",
                    score=1.0,
                )
                rows = database.leaderboard("SRS-HIGHLIGHTS")
        self.assertEqual([expected_id], [row["id"] for row in rows])


class DatasetTests(unittest.TestCase):
    def test_example_is_valid(self):
        dataset = load_dataset(ROOT / "dataset.example.json")
        self.assertEqual("freememo-smoke-test", dataset.dataset_id)
        self.assertEqual(2, len(dataset.target_units))
        self.assertTrue(dataset.smoke_test)

    def test_missing_tier_is_invalid(self):
        raw = json.loads((ROOT / "dataset.example.json").read_text())
        raw["sources"][0]["target_units"][0]["reference_cards"] = raw["sources"][0][
            "target_units"
        ][0]["reference_cards"][:-1]
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "bad.json"
            path.write_text(json.dumps(raw))
            with self.assertRaises(DatasetError):
                load_dataset(path)

    def test_at_least_two_policy_allows_partial_tiers(self):
        raw = json.loads((ROOT / "dataset.example.json").read_text())
        raw["reference_policy"] = "at-least-two-v1"
        raw["sources"][0]["target_units"][0]["reference_cards"] = raw["sources"][0][
            "target_units"
        ][0]["reference_cards"][:2]
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "partial.json"
            path.write_text(json.dumps(raw))
            dataset = load_dataset(path)
        self.assertEqual("at-least-two-v1", dataset.reference_policy)


class SrsHighlightsImportTests(unittest.TestCase):
    def test_reference_parser_accepts_question_answer_and_cloze(self):
        self.assertEqual(
            ("What is tested?", "One answer."),
            parse_reference_content("Q. What is tested?\nA. One answer."),
        )
        self.assertEqual(
            ("Recall […].", "one thing"),
            parse_reference_content("Cloze: Recall {{c1::one thing}}."),
        )

    def test_dataset_document_filters_split_and_groups_sources(self):
        rows = [
            {
                "task_id": f"unit-{split}",
                "split": split,
                "highlight_id": index,
                "source_url": "https://example.test/source",
                "source_meta": {"title": "Source", "summary": "A source summary."},
                "highlight": f"Highlight {split}",
                "highlight_interpretation": f"Intent {split}",
                "references": [
                    {
                        "id": index * 10 + 1,
                        "content": "Q. A question?\nA. An answer.",
                        "tier": "T1",
                        "task_type": "needs-refactor",
                        "test_target": False,
                    },
                    {
                        "id": index * 10 + 2,
                        "content": "Q. A better question?\nA. A better answer.",
                        "tier": "T3",
                        "task_type": "ready-to-review",
                        "test_target": True,
                    },
                ],
            }
            for index, split in enumerate(("train", "test"), start=1)
        ]
        document = dataset_document(
            rows,
            configuration="grounded",
            split="test",
            revision="abc123",
            source_file="grounded.parquet",
            source_sha256="deadbeef",
        )
        self.assertEqual("laddermedia/srs-highlights/grounded/test", document["dataset_id"])
        self.assertEqual(1, len(document["sources"]))
        self.assertEqual("unit-test", document["sources"][0]["target_units"][0]["unit_id"])

    def test_downloaded_grounded_test_split_imports(self):
        parquet = ROOT / "data/srs-highlights/grounded-00000-of-00001.parquet"
        if not parquet.exists():
            self.skipTest("The downloaded srs-highlights dataset is not present")
        try:
            import pyarrow  # noqa: F401
        except ImportError:
            self.skipTest("pyarrow is not installed")
        with tempfile.TemporaryDirectory() as directory:
            output = Path(directory) / "grounded-test.json"
            _, count = import_srs_highlights(parquet, output, split="test")
            dataset = load_dataset(output)
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                _, summary, _ = run_benchmark(
                    database,
                    FakeClient(),
                    dataset,
                    RunConfig(
                        generator_model="test/generator",
                        judge_model="test/judge",
                        cards_per_unit=1,
                        judge_passes=1,
                        limit=1,
                    ),
                    progress=lambda _: None,
                )
            self.assertEqual(32, count)
            self.assertEqual(32, len(dataset.target_units))
            self.assertEqual("at-least-two-v1", dataset.reference_policy)
            self.assertEqual(
                "784fbaad10a241be61d89cb4e2d1f0e1b6bbbb6b", dataset.version
            )
            self.assertEqual(2.0, summary["mean_score"])

    def test_default_output_name_uses_selected_split(self):
        parquet = ROOT / "data/srs-highlights/grounded-00000-of-00001.parquet"
        if not parquet.exists():
            self.skipTest("The downloaded srs-highlights dataset is not present")
        try:
            import pyarrow  # noqa: F401
        except ImportError:
            self.skipTest("pyarrow is not installed")
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / parquet.name
            source.write_bytes(parquet.read_bytes())
            manifest = json.loads((parquet.parent / "manifest.json").read_text())
            (source.parent / "manifest.json").write_text(json.dumps(manifest))
            output, count = import_srs_highlights(source, split="train")
            self.assertEqual("grounded-train.json", output.name)
            self.assertEqual(180, count)


class EstimateTests(unittest.TestCase):
    def test_estimate_counts_generation_and_judgment_calls(self):
        dataset = load_dataset(ROOT / "dataset.example.json")
        generator = {
            "pricing": {"prompt": "0.000001", "completion": "0.000002", "request": "0"}
        }
        judge = {
            "pricing": {"prompt": "0.000003", "completion": "0.000004", "request": "0"}
        }
        estimate = estimate_cost(
            dataset,
            generator,
            judge,
            cards_per_unit=3,
            judge_passes=3,
        )
        self.assertEqual(2, estimate.generation.calls)
        self.assertEqual(18, estimate.judgment.calls)
        self.assertEqual(60, estimate.maximum_calls)
        self.assertGreater(estimate.total_usd, 0)
        self.assertGreater(estimate.maximum_output_usd, estimate.total_usd)

    def test_prices_are_displayed_per_million_tokens(self):
        prices = display_prices(
            {"pricing": {"prompt": "0.0000015", "completion": "0.000006", "request": "0.01"}}
        )
        self.assertEqual(1.5, prices["prompt_per_million"])
        self.assertEqual(6.0, prices["completion_per_million"])
        self.assertEqual(0.01, prices["request"])


class RunnerTests(unittest.TestCase):
    def test_run_retries_a_truncated_generation_response(self):
        dataset = load_dataset(ROOT / "dataset.example.json")
        client = TruncatingClient()
        config = RunConfig(
            generator_model="test/generator",
            judge_model="test/judge",
            cards_per_unit=1,
            judge_passes=1,
            limit=1,
        )
        with tempfile.TemporaryDirectory() as directory:
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                run_id, summary, _ = run_benchmark(
                    database, client, dataset, config, progress=lambda _: None
                )
                self.assertEqual("completed", database.run(run_id)["status"])
                self.assertEqual(2.0, summary["mean_score"])
                self.assertEqual(3, client.calls)
                failed_calls = database.connection.execute(
                    "SELECT * FROM api_calls WHERE parse_error IS NOT NULL"
                ).fetchall()
                self.assertEqual(1, len(failed_calls))
                self.assertIn("Unterminated string", failed_calls[0]["parse_error"])
                self.assertIn("unterminated", failed_calls[0]["response_json"])

    def test_failed_run_resumes_after_the_last_complete_unit(self):
        dataset = load_dataset(ROOT / "dataset.example.json")
        config = RunConfig(
            generator_model="test/generator",
            judge_model="test/judge",
            cards_per_unit=1,
            judge_passes=1,
            response_retries=0,
        )
        with tempfile.TemporaryDirectory() as directory:
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                with self.assertRaises(OpenRouterError):
                    run_benchmark(
                        database,
                        FailingSecondGenerationClient(),
                        dataset,
                        config,
                        progress=lambda _: None,
                    )
                failed = database.history(None, 1)[0]
                self.assertEqual("failed", failed["status"])
                self.assertEqual(1, failed["units_completed"])

                resume_client = FakeClient()
                run_id, summary, _ = run_benchmark(
                    database,
                    resume_client,
                    dataset,
                    config,
                    progress=lambda _: None,
                    resume_run_id=failed["id"],
                )
                self.assertEqual(failed["id"], run_id)
                self.assertEqual(2, resume_client.calls)
                self.assertEqual("completed", database.run(run_id)["status"])
                self.assertEqual(100.0, summary["coverage"])
                self.assertEqual(0.005, summary["total_cost_usd"])

    def test_run_persists_score_and_previous_comparison(self):
        dataset = load_dataset(ROOT / "dataset.example.json")
        config = RunConfig(
            generator_model="test/generator",
            judge_model="test/judge",
            cards_per_unit=1,
            judge_passes=2,
            limit=1,
        )
        with tempfile.TemporaryDirectory() as directory:
            with ResultsDatabase(Path(directory) / "results.sqlite3") as database:
                first_id, first, previous = run_benchmark(
                    database, FakeClient(), dataset, config, progress=lambda _: None
                )
                self.assertIsNone(previous)
                self.assertEqual(2.0, first["mean_score"])
                self.assertEqual(100.0, first["usable_pct"])
                self.assertEqual("completed", database.run(first_id)["status"])
                self.assertEqual(0.003, database.run(first_id)["total_cost_usd"])

                second_id, _, previous = run_benchmark(
                    database, FakeClient(), dataset, config, progress=lambda _: None
                )
                self.assertEqual(first_id, previous["id"])
                exported = database.export_run(second_id)
                self.assertEqual("T3", exported["units"][0]["cards"][0]["tier"])
                self.assertEqual(2, len(exported["units"][0]["cards"][0]["judgments"]))


if __name__ == "__main__":
    unittest.main()
