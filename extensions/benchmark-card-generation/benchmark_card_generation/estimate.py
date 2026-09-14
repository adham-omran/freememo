from __future__ import annotations

from dataclasses import dataclass
from math import ceil
from typing import Any, Iterable

from .dataset import Dataset
from .prompts import generation_messages, judge_messages
from .settings import GENERATION_MAX_TOKENS, JUDGE_MAX_TOKENS, REPAIR_INSTRUCTION


@dataclass(frozen=True)
class CostLine:
    calls: int
    input_tokens: int
    output_tokens: int
    cost_usd: float


@dataclass(frozen=True)
class CostEstimate:
    generation: CostLine
    judgment: CostLine
    total_usd: float
    maximum_output_usd: float
    chars_per_token: float
    generation_output_tokens_per_card: int
    judge_output_tokens: int
    response_retries: int
    maximum_calls: int
    warnings: tuple[str, ...]


def estimate_message_tokens(messages: Iterable[dict[str, str]], chars_per_token: float) -> int:
    if chars_per_token <= 0:
        raise ValueError("chars_per_token must be greater than zero")
    messages = list(messages)
    character_count = sum(len(message.get("content", "")) for message in messages)
    return ceil(character_count / chars_per_token) + 4 * len(messages) + 2


def _pricing_tier(pricing: Any, input_tokens: int) -> dict[str, Any]:
    if isinstance(pricing, dict):
        return pricing
    if isinstance(pricing, list) and pricing:
        tiers = [tier for tier in pricing if isinstance(tier, dict)]
        if tiers:
            eligible = [tier for tier in tiers if int(tier.get("min_context") or 0) <= input_tokens]
            return max(eligible or tiers[:1], key=lambda tier: int(tier.get("min_context") or 0))
    raise ValueError("The model has no usable pricing data")


def _price(pricing: Any, input_tokens: int, key: str) -> float:
    tier = _pricing_tier(pricing, input_tokens)
    try:
        return float(tier.get(key) or 0.0)
    except (TypeError, ValueError) as error:
        raise ValueError(f"The model has invalid {key} pricing") from error


def _call_cost(pricing: Any, input_tokens: int, output_tokens: int) -> float:
    return (
        input_tokens * _price(pricing, input_tokens, "prompt")
        + output_tokens * _price(pricing, input_tokens, "completion")
        + _price(pricing, input_tokens, "request")
    )


def _maximum_retry_cost(
    pricing: Any,
    input_tokens: int,
    max_output_tokens: int,
    response_retries: int,
    repair_input_tokens: int,
) -> float:
    return sum(
        _call_cost(
            pricing,
            input_tokens + (repair_input_tokens if attempt else 0),
            max_output_tokens * (2**attempt),
        )
        for attempt in range(response_retries + 1)
    )


def display_prices(model: dict[str, Any], input_tokens: int = 0) -> dict[str, float]:
    pricing = model.get("pricing")
    return {
        "prompt_per_million": _price(pricing, input_tokens, "prompt") * 1_000_000,
        "completion_per_million": _price(pricing, input_tokens, "completion") * 1_000_000,
        "request": _price(pricing, input_tokens, "request"),
    }


def estimate_cost(
    dataset: Dataset,
    generator_model: dict[str, Any],
    judge_model: dict[str, Any],
    cards_per_unit: int,
    judge_passes: int,
    limit: int | None = None,
    chars_per_token: float = 4.0,
    generation_output_tokens_per_card: int = 80,
    judge_output_tokens: int = 180,
    estimated_card_chars: int = 240,
    response_retries: int = 2,
) -> CostEstimate:
    if cards_per_unit < 1 or judge_passes < 1:
        raise ValueError("cards_per_unit and judge_passes must be at least 1")
    if generation_output_tokens_per_card < 1 or judge_output_tokens < 1:
        raise ValueError("Estimated output token counts must be at least 1")
    if estimated_card_chars < 2:
        raise ValueError("estimated_card_chars must be at least 2")
    if response_retries < 0:
        raise ValueError("response_retries must not be negative")

    units = list(dataset.target_units[:limit])
    if not units:
        raise ValueError("The selected dataset range is empty")
    generator_pricing = generator_model.get("pricing")
    judge_pricing = judge_model.get("pricing")
    generation_input = 0
    generation_output = 0
    generation_cost = 0.0
    generation_maximum = 0.0
    judgment_input = 0
    judgment_output = 0
    judgment_cost = 0.0
    judgment_maximum = 0.0

    half = estimated_card_chars // 2
    placeholder_card = {"front": "x" * half, "back": "x" * (estimated_card_chars - half)}
    repair_input_tokens = ceil(len(REPAIR_INSTRUCTION) / chars_per_token) + 4
    for unit in units:
        generator_input_tokens = estimate_message_tokens(
            generation_messages(unit, cards_per_unit), chars_per_token
        )
        generator_output_tokens = generation_output_tokens_per_card * cards_per_unit
        generation_input += generator_input_tokens
        generation_output += generator_output_tokens
        generation_cost += _call_cost(
            generator_pricing, generator_input_tokens, generator_output_tokens
        )
        generation_maximum += _maximum_retry_cost(
            generator_pricing,
            generator_input_tokens,
            max(GENERATION_MAX_TOKENS, generator_output_tokens),
            response_retries,
            repair_input_tokens,
        )

        judge_input_tokens = estimate_message_tokens(
            judge_messages(unit, placeholder_card, unit.reference_cards), chars_per_token
        )
        judgment_calls = cards_per_unit * judge_passes
        judgment_input += judge_input_tokens * judgment_calls
        judgment_output += judge_output_tokens * judgment_calls
        judgment_cost += judgment_calls * _call_cost(
            judge_pricing, judge_input_tokens, judge_output_tokens
        )
        judgment_maximum += judgment_calls * _maximum_retry_cost(
            judge_pricing,
            judge_input_tokens,
            max(JUDGE_MAX_TOKENS, judge_output_tokens),
            response_retries,
            repair_input_tokens,
        )

    warnings: list[str] = []
    for label, model in (("generator", generator_model), ("judge", judge_model)):
        pricing = model.get("pricing")
        tier = _pricing_tier(pricing, 0)
        if float(tier.get("internal_reasoning") or 0.0) > 0:
            warnings.append(f"The {label} model has separate internal reasoning charges.")
        if isinstance(pricing, list):
            warnings.append(f"The {label} model uses tiered pricing.")

    generation = CostLine(
        calls=len(units),
        input_tokens=generation_input,
        output_tokens=generation_output,
        cost_usd=generation_cost,
    )
    judgment = CostLine(
        calls=len(units) * cards_per_unit * judge_passes,
        input_tokens=judgment_input,
        output_tokens=judgment_output,
        cost_usd=judgment_cost,
    )
    return CostEstimate(
        generation=generation,
        judgment=judgment,
        total_usd=generation_cost + judgment_cost,
        maximum_output_usd=generation_maximum + judgment_maximum,
        chars_per_token=chars_per_token,
        generation_output_tokens_per_card=generation_output_tokens_per_card,
        judge_output_tokens=judge_output_tokens,
        response_retries=response_retries,
        maximum_calls=(len(units) + judgment.calls) * (response_retries + 1),
        warnings=tuple(warnings),
    )
