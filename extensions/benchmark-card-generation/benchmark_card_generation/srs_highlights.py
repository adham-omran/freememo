from __future__ import annotations

import hashlib
import json
import re
from pathlib import Path
from typing import Any, Iterable

from .dataset import DatasetError, load_dataset


DATASET_ID = "laddermedia/srs-highlights"
DATASET_URL = "https://huggingface.co/datasets/laddermedia/srs-highlights"
SPLITS = ("train", "test", "all")
_QA_PATTERN = re.compile(r"\AQ\.\s*(.*?)\r?\nA\.\s*(.*?)\s*\Z", re.DOTALL)
_CLOZE_PATTERN = re.compile(r"\{\{c\d+::(.*?)(?:::[^{}]*)?\}\}", re.DOTALL)


def _required_text(value: Any, location: str) -> str:
    if not isinstance(value, str) or not value.strip():
        raise DatasetError(f"{location} must be a nonempty string")
    return value.strip()


def parse_reference_content(
    content: Any, location: str = "reference content"
) -> tuple[str, str]:
    text = _required_text(content, location)
    qa_match = _QA_PATTERN.match(text)
    if qa_match:
        return qa_match.group(1).strip(), qa_match.group(2).strip()

    cloze_answers = [match.group(1).strip() for match in _CLOZE_PATTERN.finditer(text)]
    if cloze_answers:
        front = _CLOZE_PATTERN.sub("[…]", text)
        if front.startswith("Cloze:"):
            front = front.removeprefix("Cloze:").strip()
        return front, "; ".join(cloze_answers)

    raise DatasetError(f"{location} is not a supported Q/A or cloze prompt")


def _source_id(source_url: str) -> str:
    return f"source-{hashlib.sha256(source_url.encode()).hexdigest()[:16]}"


def dataset_document(
    rows: Iterable[dict[str, Any]],
    *,
    configuration: str,
    split: str,
    revision: str,
    source_file: str,
    source_sha256: str,
) -> dict[str, Any]:
    if split not in SPLITS:
        raise DatasetError(f"split must be one of {', '.join(SPLITS)}")

    sources_by_url: dict[str, dict[str, Any]] = {}
    unit_ids: set[str] = set()
    selected_rows = 0
    for row_index, row in enumerate(rows):
        row_where = f"rows[{row_index}]"
        if not isinstance(row, dict):
            raise DatasetError(f"{row_where} must be an object")
        row_split = _required_text(row.get("split"), f"{row_where}.split")
        if row_split not in ("train", "test"):
            raise DatasetError(f"{row_where}.split must be train or test")
        if split != "all" and row_split != split:
            continue

        selected_rows += 1
        task_id = _required_text(row.get("task_id"), f"{row_where}.task_id")
        if task_id in unit_ids:
            raise DatasetError(f"Duplicate task_id: {task_id}")
        unit_ids.add(task_id)
        source_url = _required_text(row.get("source_url"), f"{row_where}.source_url")
        source_meta = row.get("source_meta")
        if not isinstance(source_meta, dict):
            raise DatasetError(f"{row_where}.source_meta must be an object")
        source_summary = _required_text(
            source_meta.get("summary"), f"{row_where}.source_meta.summary"
        )
        highlight = _required_text(row.get("highlight"), f"{row_where}.highlight")
        intent = _required_text(
            row.get("highlight_interpretation"), f"{row_where}.highlight_interpretation"
        )
        references = row.get("references")
        if not isinstance(references, list) or len(references) < 2:
            raise DatasetError(f"{row_where}.references must contain at least two prompts")

        clean_references: list[dict[str, str]] = []
        for reference_index, reference in enumerate(references):
            ref_where = f"{row_where}.references[{reference_index}]"
            if not isinstance(reference, dict):
                raise DatasetError(f"{ref_where} must be an object")
            reference_id = str(reference.get("id", "")).strip()
            if not reference_id:
                raise DatasetError(f"{ref_where}.id must not be empty")
            tier = reference.get("tier")
            if tier not in ("T0", "T1", "T2", "T3"):
                raise DatasetError(f"{ref_where}.tier must be T0, T1, T2, or T3")
            front, back = parse_reference_content(
                reference.get("content"), f"{ref_where}.content"
            )
            task_type = _required_text(
                reference.get("task_type"), f"{ref_where}.task_type"
            )
            clean_references.append(
                {
                    "id": reference_id,
                    "tier": tier,
                    "front": front,
                    "back": back,
                    "rationale": f"Memory Machines human rating: {task_type}.",
                    "source_task_type": task_type,
                    "source_test_target": bool(reference.get("test_target", False)),
                }
            )

        existing_source = sources_by_url.get(source_url)
        if existing_source is None:
            imported_meta = dict(source_meta)
            imported_meta.update(
                {
                    "source_url": source_url,
                    "context_kind": "source-summary",
                    "original_split": row_split,
                }
            )
            existing_source = {
                "source_id": _source_id(source_url),
                "source_text": source_summary,
                "source_meta": imported_meta,
                "target_units": [],
            }
            sources_by_url[source_url] = existing_source
        elif existing_source["source_text"] != source_summary:
            raise DatasetError(f"Source metadata changed within {source_url}")

        existing_source["target_units"].append(
            {
                "unit_id": task_id,
                "highlight": highlight,
                "intent": intent,
                "reference_cards": clean_references,
            }
        )

    if selected_rows == 0:
        raise DatasetError(f"The Parquet file contains no {split} rows")

    return {
        "dataset_id": f"{DATASET_ID}/{configuration}/{split}",
        "version": revision,
        "smoke_test": False,
        "reference_policy": "at-least-two-v1",
        "provenance": {
            "dataset_url": DATASET_URL,
            "revision": revision,
            "configuration": configuration,
            "split": split,
            "source_file": source_file,
            "source_sha256": source_sha256,
            "context_kind": "source-summary",
        },
        "sources": list(sources_by_url.values()),
    }


def _configuration(path: Path) -> str:
    marker = "-00000-of-"
    if marker not in path.name:
        raise DatasetError("The Parquet file name does not contain a dataset configuration")
    return path.name.split(marker, 1)[0]


def _file_sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _revision_from_manifest(path: Path, source_sha256: str) -> str | None:
    manifest_path = path.parent / "manifest.json"
    if not manifest_path.exists():
        return None
    try:
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError) as error:
        raise DatasetError(f"Cannot read the dataset manifest: {error}") from error
    files = manifest.get("files")
    if not isinstance(files, list):
        raise DatasetError("The dataset manifest has no files array")
    entry = next(
        (
            item
            for item in files
            if isinstance(item, dict) and item.get("path") == path.name
        ),
        None,
    )
    if entry is None:
        raise DatasetError(f"The dataset manifest does not list {path.name}")
    if entry.get("sha256") != source_sha256:
        raise DatasetError(f"The SHA-256 hash for {path.name} does not match the manifest")
    return _required_text(manifest.get("revision"), "manifest.revision")


def import_srs_highlights(
    parquet_path: str | Path,
    output_path: str | Path | None = None,
    *,
    split: str = "test",
    revision: str | None = None,
) -> tuple[Path, int]:
    source_path = Path(parquet_path).expanduser().resolve()
    if not source_path.is_file():
        raise DatasetError(f"Cannot read Parquet file: {source_path}")
    configuration = _configuration(source_path)
    target_path = (
        Path(output_path).expanduser().resolve()
        if output_path is not None
        else source_path.with_name(f"{configuration}-{split}.json")
    )
    source_sha256 = _file_sha256(source_path)
    manifest_revision = _revision_from_manifest(source_path, source_sha256)
    if revision and manifest_revision and revision != manifest_revision:
        raise DatasetError("The requested revision does not match the dataset manifest")
    selected_revision = revision or manifest_revision
    if not selected_revision:
        raise DatasetError("Set --revision when the Parquet file has no manifest")

    try:
        import pyarrow.parquet as parquet
    except ImportError as error:
        raise DatasetError(
            "The import command needs pyarrow. Install requirements-import.txt."
        ) from error

    try:
        rows = parquet.read_table(source_path).to_pylist()
    except Exception as error:
        raise DatasetError(f"Cannot read Parquet data: {error}") from error
    document = dataset_document(
        rows,
        configuration=configuration,
        split=split,
        revision=selected_revision,
        source_file=source_path.name,
        source_sha256=source_sha256,
    )
    target_path.parent.mkdir(parents=True, exist_ok=True)
    target_path.write_text(
        json.dumps(document, indent=2, ensure_ascii=False) + "\n", encoding="utf-8"
    )
    imported = load_dataset(target_path)
    return target_path, len(imported.target_units)
