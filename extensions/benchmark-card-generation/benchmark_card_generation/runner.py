from __future__ import annotations

import hashlib
import inspect
import json
import random
import uuid
from collections import Counter
from dataclasses import dataclass
from datetime import datetime, timezone
from statistics import mean
from typing import Any, Callable

from .dataset import Dataset, TIERS
from .db import ResultsDatabase
from .openrouter import Completion, OpenRouterClient, OpenRouterError, parse_json_object
from .prompts import PIPELINE_ID, generation_messages, judge_messages
from .scoring import UTILITY_PROFILES, completion_score, confidence_interval_95, consensus_tier
from .settings import GENERATION_MAX_TOKENS, JUDGE_MAX_TOKENS, REPAIR_INSTRUCTION


CRITERIA = (
    "lacks_context",
    "solicits_multiple",
    "shallow",
    "narrow",
    "wordy",
    "giveaway",
    "not_atomic",
    "factual_error",
)


@dataclass(frozen=True)
class RunConfig:
    generator_model: str
    judge_model: str
    cards_per_unit: int = 3
    judge_passes: int = 3
    random_seed: int = 1
    utility_profile: str = "raw-v1"
    limit: int | None = None
    response_retries: int = 2


def _now() -> str:
    return datetime.now(timezone.utc).isoformat(timespec="microseconds")


def _pipeline_hash() -> str:
    source = inspect.getsource(generation_messages) + inspect.getsource(judge_messages)
    return hashlib.sha256(source.encode()).hexdigest()


def _cards_from_response(response: dict[str, Any], expected: int) -> list[dict[str, str]]:
    cards = response.get("cards")
    if not isinstance(cards, list):
        raise OpenRouterError("The generator JSON has no cards array")
    clean: list[dict[str, str]] = []
    for index, card in enumerate(cards):
        if not isinstance(card, dict):
            raise OpenRouterError(f"Card {index + 1} is not an object")
        front = card.get("front")
        back = card.get("back")
        if not isinstance(front, str) or not front.strip():
            raise OpenRouterError(f"Card {index + 1} has no front text")
        if not isinstance(back, str) or not back.strip():
            raise OpenRouterError(f"Card {index + 1} has no back text")
        clean.append({"front": front.strip(), "back": back.strip()})
    if len(clean) != expected:
        raise OpenRouterError(f"The generator returned {len(clean)} cards, but {expected} were requested")
    return clean


def _judgment_from_response(response: dict[str, Any]) -> dict[str, Any]:
    tier = response.get("tier")
    if tier not in TIERS:
        raise OpenRouterError("The judge tier must be T0, T1, T2, or T3")
    rationale = response.get("rationale")
    if not isinstance(rationale, str) or not rationale.strip():
        raise OpenRouterError("The judge response has no rationale")
    criteria = response.get("criteria")
    if not isinstance(criteria, dict):
        raise OpenRouterError("The judge response has no criteria object")
    return {
        "tier": tier,
        "rationale": rationale.strip(),
        "closest_reference": response.get("closest_reference"),
        "criteria": {name: bool(criteria.get(name, False)) for name in CRITERIA},
    }


def _api_call_values(
    run_id: int,
    phase: str,
    unit_id: str,
    model_requested: str,
    completion: Completion,
    card_ordinal: int | None = None,
    pass_number: int | None = None,
) -> dict[str, Any]:
    return {
        "run_id": run_id,
        "phase": phase,
        "unit_id": unit_id,
        "card_ordinal": card_ordinal,
        "pass_number": pass_number,
        "model_requested": model_requested,
        "model_resolved": completion.resolved_model,
        "provider": completion.provider,
        "generation_id": completion.generation_id,
        "cost_usd": completion.cost_usd,
        "prompt_tokens": completion.prompt_tokens,
        "completion_tokens": completion.completion_tokens,
        "latency_ms": completion.latency_ms,
        "response_json": json.dumps(completion.response),
    }


def _finish_reason(completion: Completion) -> str | None:
    choices = completion.response.get("choices")
    if not isinstance(choices, list) or not choices or not isinstance(choices[0], dict):
        return None
    value = choices[0].get("finish_reason")
    return value if isinstance(value, str) else None


def _validate_resume(
    run: Any,
    dataset: Dataset,
    config: RunConfig,
    pipeline_sha256: str,
) -> None:
    if run["status"] == "completed":
        raise ValueError(f"Run {run['id']} is already complete")
    if run["dataset_sha256"] != dataset.sha256:
        raise ValueError("The dataset changed after the failed run")
    if run["pipeline_sha256"] != pipeline_sha256:
        raise ValueError("The generation or judgment prompt changed after the failed run")
    selected_count = min(config.limit or len(dataset.target_units), len(dataset.target_units))
    if run["unit_count"] != selected_count:
        raise ValueError("The selected unit count changed after the failed run")
    expected = {
        "generator_model_requested": config.generator_model,
        "judge_model_requested": config.judge_model,
        "cards_per_unit": config.cards_per_unit,
        "judge_passes": config.judge_passes,
        "random_seed": config.random_seed,
        "utility_profile": config.utility_profile,
    }
    for column, value in expected.items():
        if run[column] != value:
            raise ValueError(f"Run {run['id']} has a different {column}")


def run_benchmark(
    database: ResultsDatabase,
    client: OpenRouterClient,
    dataset: Dataset,
    config: RunConfig,
    progress: Callable[[str], None] = print,
    resume_run_id: int | None = None,
) -> tuple[int, dict[str, Any], dict[str, Any] | None]:
    if config.cards_per_unit < 1:
        raise ValueError("cards_per_unit must be at least 1")
    if config.judge_passes < 1:
        raise ValueError("judge_passes must be at least 1")
    if config.utility_profile not in UTILITY_PROFILES:
        raise ValueError(f"Unknown utility profile: {config.utility_profile}")
    if config.response_retries < 0:
        raise ValueError("response_retries must not be negative")

    units = list(dataset.target_units[: config.limit])
    if not units:
        raise ValueError("The selected dataset range is empty")
    pipeline_sha256 = _pipeline_hash()
    rng = random.Random(config.random_seed)
    utilities = UTILITY_PROFILES[config.utility_profile]
    if resume_run_id is None:
        run_id = database.insert_run(
            {
                "run_uuid": str(uuid.uuid4()),
                "started_at": _now(),
                "status": "running",
                "generator_model_requested": config.generator_model,
                "judge_model_requested": config.judge_model,
                "dataset_id": dataset.dataset_id,
                "dataset_version": dataset.version,
                "dataset_sha256": dataset.sha256,
                "dataset_path": str(dataset.path),
                "pipeline_id": PIPELINE_ID,
                "pipeline_sha256": pipeline_sha256,
                "utility_profile": config.utility_profile,
                "cards_per_unit": config.cards_per_unit,
                "judge_passes": config.judge_passes,
                "random_seed": config.random_seed,
                "unit_count": len(units),
                "config_json": json.dumps(config.__dict__, sort_keys=True),
            }
        )
        completed_units: list[Any] = []
        unit_scores: list[float] = []
        all_tiers: list[str] = []
        generator_models: set[str] = set()
        judge_models: set[str] = set()
        total_cost = 0.0
        prompt_tokens = 0
        completion_tokens = 0
        latency_ms = 0
    else:
        run = database.run(resume_run_id)
        if run is None:
            raise ValueError(f"Run {resume_run_id} does not exist")
        _validate_resume(run, dataset, config, pipeline_sha256)
        run_id = resume_run_id
        database.delete_incomplete_units(run_id)
        completed_units = database.completed_units(run_id)
        for expected_ordinal, completed in enumerate(completed_units, start=1):
            if completed["ordinal"] != expected_ordinal:
                raise ValueError("The completed units are not contiguous")
            expected_unit = units[expected_ordinal - 1]
            if completed["unit_id"] != expected_unit.unit_id:
                raise ValueError("The dataset unit order changed after the failed run")
            if completed["cards_returned"] != config.cards_per_unit:
                raise ValueError("A completed unit has an unexpected card count")
        totals = database.api_totals(run_id)
        unit_scores = [float(unit["score"]) for unit in completed_units]
        all_tiers = database.completed_tiers(run_id)
        generator_models = database.resolved_models(run_id, "generate")
        judge_models = database.resolved_models(run_id, "judge")
        total_cost = float(totals["total_cost_usd"])
        prompt_tokens = int(totals["prompt_tokens"])
        completion_tokens = int(totals["completion_tokens"])
        latency_ms = int(totals["latency_ms"])
        database.update_run(
            run_id,
            {
                "finished_at": None,
                "status": "running",
                "error": None,
                "units_completed": len(completed_units),
                "config_json": json.dumps(config.__dict__, sort_keys=True),
            },
        )

    completed_count = len(completed_units)
    for completed_unit in units[:completed_count]:
        for _ in range(config.cards_per_unit * config.judge_passes):
            references = list(completed_unit.reference_cards)
            rng.shuffle(references)

    def record_call(
        phase: str,
        unit_id: str,
        model: str,
        completion: Completion,
        card_ordinal: int | None = None,
        pass_number: int | None = None,
    ) -> int:
        nonlocal total_cost, prompt_tokens, completion_tokens, latency_ms
        call_id = database.insert_api_call(
            _api_call_values(
                run_id, phase, unit_id, model, completion, card_ordinal, pass_number
            )
        )
        total_cost += completion.cost_usd
        prompt_tokens += completion.prompt_tokens
        completion_tokens += completion.completion_tokens
        latency_ms += completion.latency_ms
        return call_id

    def call_json(
        phase: str,
        unit_id: str,
        model: str,
        messages: list[dict[str, str]],
        max_tokens: int,
        validator: Callable[[dict[str, Any]], Any],
        card_ordinal: int | None = None,
        pass_number: int | None = None,
    ) -> tuple[Completion, Any]:
        attempt_messages = messages
        attempt_max_tokens = max_tokens
        for attempt in range(config.response_retries + 1):
            completion = client.chat(
                model,
                attempt_messages,
                temperature=0.0,
                max_tokens=attempt_max_tokens,
            )
            call_id = record_call(
                phase,
                unit_id,
                model,
                completion,
                card_ordinal,
                pass_number,
            )
            if phase == "generate" and completion.resolved_model:
                generator_models.add(completion.resolved_model)
            if phase == "judge" and completion.resolved_model:
                judge_models.add(completion.resolved_model)
            try:
                return completion, validator(parse_json_object(completion.content))
            except OpenRouterError as error:
                database.update_api_call(call_id, {"parse_error": str(error)})
                if attempt >= config.response_retries:
                    raise
                progress(
                    f"Retry {phase} output for {unit_id} after invalid JSON "
                    f"({attempt + 1}/{config.response_retries})"
                )
                if (
                    _finish_reason(completion) == "length"
                    or completion.completion_tokens >= attempt_max_tokens - 8
                ):
                    attempt_max_tokens *= 2
                attempt_messages = [
                    *messages,
                    {"role": "user", "content": REPAIR_INSTRUCTION},
                ]
        raise AssertionError("The response retry loop did not return")

    try:
        for unit_index, unit in enumerate(
            units[completed_count:], start=completed_count + 1
        ):
            progress(f"[{unit_index}/{len(units)}] Generate cards for {unit.unit_id}")
            generated, cards = call_json(
                "generate",
                unit.unit_id,
                config.generator_model,
                generation_messages(unit, config.cards_per_unit),
                GENERATION_MAX_TOKENS,
                lambda response: _cards_from_response(response, config.cards_per_unit),
            )
            unit_row_id = database.insert_unit(
                {
                    "run_id": run_id,
                    "unit_id": unit.unit_id,
                    "source_id": unit.source_id,
                    "ordinal": unit_index,
                    "cards_requested": config.cards_per_unit,
                    "cards_returned": len(cards),
                    "generation_response_json": json.dumps(generated.response),
                }
            )
            card_utilities: list[float] = []
            for card_index, card in enumerate(cards, start=1):
                progress(
                    f"[{unit_index}/{len(units)}] Judge card {card_index}/{len(cards)}"
                )
                judgments: list[dict[str, Any]] = []
                judgment_responses: list[dict[str, Any]] = []
                for pass_number in range(1, config.judge_passes + 1):
                    references = list(unit.reference_cards)
                    rng.shuffle(references)
                    judged, judgment = call_json(
                        "judge",
                        unit.unit_id,
                        config.judge_model,
                        judge_messages(unit, card, references),
                        JUDGE_MAX_TOKENS,
                        _judgment_from_response,
                        card_ordinal=card_index,
                        pass_number=pass_number,
                    )
                    judgments.append(judgment)
                    judgment_responses.append(judged.response)
                tier = consensus_tier([judgment["tier"] for judgment in judgments])
                criteria = {
                    name: sum(bool(judgment["criteria"][name]) for judgment in judgments)
                    * 2
                    >= len(judgments)
                    for name in CRITERIA
                }
                utility = utilities[tier]
                card_utilities.append(utility)
                all_tiers.append(tier)
                card_id = database.insert_card(
                    {
                        "unit_row_id": unit_row_id,
                        "ordinal": card_index,
                        "front": card["front"],
                        "back": card["back"],
                        "tier": tier,
                        "utility": utility,
                        "criteria_json": json.dumps(criteria, sort_keys=True),
                    }
                )
                for pass_number, (judgment, response) in enumerate(
                    zip(judgments, judgment_responses), start=1
                ):
                    database.insert_judgment(
                        {
                            "card_id": card_id,
                            "pass_number": pass_number,
                            "tier": judgment["tier"],
                            "rationale": judgment["rationale"],
                            "closest_reference": judgment["closest_reference"],
                            "criteria_json": json.dumps(judgment["criteria"], sort_keys=True),
                            "response_json": json.dumps(response),
                        }
                    )
            score = completion_score(card_utilities)
            unit_scores.append(score)
            database.connection.execute(
                "UPDATE units SET score = ? WHERE id = ?", (score, unit_row_id)
            )
            database.connection.commit()
            database.update_run(
                run_id,
                {
                    "units_completed": unit_index,
                    "total_cost_usd": total_cost,
                    "prompt_tokens": prompt_tokens,
                    "completion_tokens": completion_tokens,
                    "latency_ms": latency_ms,
                },
            )

        interval = confidence_interval_95(unit_scores)
        usable_count = sum(tier in ("T2", "T3") for tier in all_tiers)
        summary = {
            "mean_score": mean(unit_scores),
            "ci_low": interval[0] if interval else None,
            "ci_high": interval[1] if interval else None,
            "usable_pct": 100.0 * usable_count / len(all_tiers) if all_tiers else 0.0,
            "coverage": 100.0 * len(unit_scores) / len(units),
            "tier_distribution": dict(Counter(all_tiers)),
            "total_cost_usd": total_cost,
            "prompt_tokens": prompt_tokens,
            "completion_tokens": completion_tokens,
            "latency_ms": latency_ms,
        }
        database.update_run(
            run_id,
            {
                "finished_at": _now(),
                "status": "completed",
                "generator_models_resolved": ",".join(sorted(generator_models)) or None,
                "judge_models_resolved": ",".join(sorted(judge_models)) or None,
                "mean_score": summary["mean_score"],
                "ci_low": summary["ci_low"],
                "ci_high": summary["ci_high"],
                "usable_pct": summary["usable_pct"],
                "coverage": summary["coverage"],
                "total_cost_usd": total_cost,
                "prompt_tokens": prompt_tokens,
                "completion_tokens": completion_tokens,
                "latency_ms": latency_ms,
            },
        )
        previous = database.previous_run(run_id)
        return run_id, summary, dict(previous) if previous else None
    except BaseException as error:
        database.update_run(
            run_id,
            {
                "finished_at": _now(),
                "status": "failed",
                "generator_models_resolved": ",".join(sorted(generator_models)) or None,
                "judge_models_resolved": ",".join(sorted(judge_models)) or None,
                "total_cost_usd": total_cost,
                "prompt_tokens": prompt_tokens,
                "completion_tokens": completion_tokens,
                "latency_ms": latency_ms,
                "error": str(error),
            },
        )
        raise
