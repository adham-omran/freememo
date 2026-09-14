from __future__ import annotations

import json
from typing import Any, Sequence

from .dataset import TargetUnit


PIPELINE_ID = "grounded-qa-v1"


def generation_messages(unit: TargetUnit, cards_per_unit: int) -> list[dict[str, str]]:
    system = (
        "Write spaced-repetition flashcards. Each question must cue one stable, specific answer "
        "after a long delay. Use the reader intent as the target. Use the source only as evidence. "
        "Do not mention the source passage. Return JSON only."
    )
    user = f"""SOURCE:
{unit.source_text}

HIGHLIGHT:
{unit.highlight}

READER INTENT:
{unit.intent}

Write exactly {cards_per_unit} distinct question-and-answer cards.
Return this object: {{"cards":[{{"front":"...","back":"..."}}]}}"""
    return [{"role": "system", "content": system}, {"role": "user", "content": user}]


def judge_messages(
    unit: TargetUnit,
    card: dict[str, str],
    references: Sequence[dict[str, Any]],
) -> list[dict[str, str]]:
    criteria = {
        "lacks_context": False,
        "solicits_multiple": False,
        "shallow": False,
        "narrow": False,
        "wordy": False,
        "giveaway": False,
        "not_atomic": False,
        "factual_error": False,
    }
    system = (
        "Rate one spaced-repetition card. Compare it with the rated cards for the same target. "
        "Do not use an abstract quality scale. Return JSON only."
    )
    user = f"""TIERS
T3: Ready to review. It targets the intent and cues one stable, specific answer.
T2: Needs polish. It targets the intent and remains usable, but has minor friction.
T1: Needs refactor. It looks plausible but is ambiguous, unstable, or allows multiple answers.
T0: Off-target. It targets the wrong detail or contradicts the source.

If an on-target card might not cue a stable answer in six months, use T1.

SOURCE:
{unit.source_text}

HIGHLIGHT:
{unit.highlight}

READER INTENT:
{unit.intent}

RATED REFERENCE CARDS:
{json.dumps(list(references), ensure_ascii=False)}

CANDIDATE:
{json.dumps(card, ensure_ascii=False)}

Return this object:
{{"tier":"T0|T1|T2|T3","rationale":"one sentence","closest_reference":"reference id", "criteria":{json.dumps(criteria)}}}"""
    return [{"role": "system", "content": system}, {"role": "user", "content": user}]
