from __future__ import annotations

from collections import Counter
from math import sqrt
from statistics import mean
from typing import Iterable, Sequence

from .dataset import TIERS


UTILITY_PROFILES = {
    "raw-v1": {"T0": -1.0, "T1": -3.0, "T2": 1.5, "T3": 2.0},
    "memory-machines-posterior-v1": {"T0": -1.10, "T1": -1.43, "T2": 0.50, "T3": 1.02},
}


def consensus_tier(tiers: Sequence[str]) -> str:
    if not tiers:
        raise ValueError("At least one tier is required")
    if any(tier not in TIERS for tier in tiers):
        raise ValueError("All tiers must be T0, T1, T2, or T3")
    counts = Counter(tiers)
    largest = max(counts.values())
    return next(tier for tier in TIERS if counts[tier] == largest)


def completion_score(utilities: Iterable[float]) -> float:
    values = list(utilities)
    if not values:
        return 0.0
    return max(values) + sum(value for value in values if value < 0)


def confidence_interval_95(values: Sequence[float]) -> tuple[float, float] | None:
    if not values:
        return None
    center = mean(values)
    if len(values) == 1:
        return center, center
    variance = sum((value - center) ** 2 for value in values) / (len(values) - 1)
    margin = 1.96 * sqrt(variance) / sqrt(len(values))
    return center - margin, center + margin
