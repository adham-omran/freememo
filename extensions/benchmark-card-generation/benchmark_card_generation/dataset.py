from __future__ import annotations

import hashlib
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any


TIERS = ("T0", "T1", "T2", "T3")
REFERENCE_POLICIES = ("all-tiers-v1", "at-least-two-v1")


class DatasetError(ValueError):
    pass


@dataclass(frozen=True)
class TargetUnit:
    source_id: str
    source_text: str
    source_meta: dict[str, Any]
    unit_id: str
    highlight: str
    intent: str
    reference_cards: tuple[dict[str, Any], ...]


@dataclass(frozen=True)
class Dataset:
    dataset_id: str
    version: str
    path: Path
    sha256: str
    target_units: tuple[TargetUnit, ...]
    smoke_test: bool
    reference_policy: str


def _required_string(value: Any, location: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise DatasetError(f"{location} must be a nonempty string")
    return value


def _canonical_bytes(value: Any) -> bytes:
    return json.dumps(value, ensure_ascii=False, sort_keys=True, separators=(",", ":")).encode()


def load_dataset(path: str | Path) -> Dataset:
    dataset_path = Path(path).expanduser().resolve()
    try:
        raw = json.loads(dataset_path.read_text(encoding="utf-8"))
    except OSError as error:
        raise DatasetError(f"Cannot read dataset: {error}") from error
    except json.JSONDecodeError as error:
        raise DatasetError(f"Dataset is not valid JSON: {error}") from error

    if not isinstance(raw, dict):
        raise DatasetError("The dataset root must be an object")

    dataset_id = _required_string(raw.get("dataset_id"), "dataset_id")
    version = _required_string(raw.get("version"), "version")
    reference_policy = raw.get("reference_policy", "all-tiers-v1")
    if reference_policy not in REFERENCE_POLICIES:
        allowed = ", ".join(REFERENCE_POLICIES)
        raise DatasetError(f"reference_policy must be one of {allowed}")
    sources = raw.get("sources")
    if not isinstance(sources, list) or not sources:
        raise DatasetError("sources must be a nonempty array")

    units: list[TargetUnit] = []
    source_ids: set[str] = set()
    unit_ids: set[str] = set()
    for source_index, source in enumerate(sources):
        where = f"sources[{source_index}]"
        if not isinstance(source, dict):
            raise DatasetError(f"{where} must be an object")
        source_id = _required_string(source.get("source_id"), f"{where}.source_id")
        if source_id in source_ids:
            raise DatasetError(f"Duplicate source_id: {source_id}")
        source_ids.add(source_id)
        source_text = _required_string(source.get("source_text"), f"{where}.source_text")
        source_meta = source.get("source_meta", {})
        if not isinstance(source_meta, dict):
            raise DatasetError(f"{where}.source_meta must be an object")
        target_units = source.get("target_units")
        if not isinstance(target_units, list) or not target_units:
            raise DatasetError(f"{where}.target_units must be a nonempty array")

        for unit_index, unit in enumerate(target_units):
            unit_where = f"{where}.target_units[{unit_index}]"
            if not isinstance(unit, dict):
                raise DatasetError(f"{unit_where} must be an object")
            unit_id = _required_string(unit.get("unit_id"), f"{unit_where}.unit_id")
            if unit_id in unit_ids:
                raise DatasetError(f"Duplicate unit_id: {unit_id}")
            unit_ids.add(unit_id)
            highlight = _required_string(unit.get("highlight"), f"{unit_where}.highlight")
            intent = _required_string(unit.get("intent"), f"{unit_where}.intent")
            references = unit.get("reference_cards")
            if not isinstance(references, list) or not references:
                raise DatasetError(f"{unit_where}.reference_cards must be a nonempty array")

            reference_ids: set[str] = set()
            reference_tiers: set[str] = set()
            clean_references: list[dict[str, Any]] = []
            for reference_index, reference in enumerate(references):
                ref_where = f"{unit_where}.reference_cards[{reference_index}]"
                if not isinstance(reference, dict):
                    raise DatasetError(f"{ref_where} must be an object")
                reference_id = _required_string(reference.get("id"), f"{ref_where}.id")
                if reference_id in reference_ids:
                    raise DatasetError(f"Duplicate reference id in {unit_id}: {reference_id}")
                reference_ids.add(reference_id)
                tier = reference.get("tier")
                if tier not in TIERS:
                    raise DatasetError(f"{ref_where}.tier must be one of {', '.join(TIERS)}")
                reference_tiers.add(tier)
                clean_references.append(
                    {
                        "id": reference_id,
                        "tier": tier,
                        "front": _required_string(reference.get("front"), f"{ref_where}.front"),
                        "back": _required_string(reference.get("back"), f"{ref_where}.back"),
                        "rationale": _required_string(
                            reference.get("rationale"), f"{ref_where}.rationale"
                        ),
                    }
                )
            if reference_policy == "all-tiers-v1":
                missing_tiers = set(TIERS) - reference_tiers
                if missing_tiers:
                    missing = ", ".join(sorted(missing_tiers))
                    raise DatasetError(f"{unit_where} has no reference card for {missing}")
            elif len(clean_references) < 2:
                raise DatasetError(f"{unit_where} must have at least two reference cards")
            units.append(
                TargetUnit(
                    source_id=source_id,
                    source_text=source_text,
                    source_meta=source_meta,
                    unit_id=unit_id,
                    highlight=highlight,
                    intent=intent,
                    reference_cards=tuple(clean_references),
                )
            )

    return Dataset(
        dataset_id=dataset_id,
        version=version,
        path=dataset_path,
        sha256=hashlib.sha256(_canonical_bytes(raw)).hexdigest(),
        target_units=tuple(units),
        smoke_test=bool(raw.get("smoke_test", False)),
        reference_policy=reference_policy,
    )
