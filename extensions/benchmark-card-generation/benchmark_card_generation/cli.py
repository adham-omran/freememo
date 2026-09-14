from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

from .dataset import DatasetError, load_dataset
from .db import ResultsDatabase
from .estimate import display_prices, estimate_cost
from .openrouter import OpenRouterClient, OpenRouterError
from .runner import RunConfig, run_benchmark
from .scoring import UTILITY_PROFILES
from .settings import DEFAULT_RESPONSE_RETRIES
from .srs_highlights import import_srs_highlights


EXTENSION_DIR = Path(__file__).resolve().parent.parent
DEFAULT_DATASET = EXTENSION_DIR / "dataset.example.json"
DEFAULT_DATABASE = EXTENSION_DIR / "results.sqlite3"
DEFAULT_SRS_HIGHLIGHTS = (
    EXTENSION_DIR / "data/srs-highlights/grounded-00000-of-00001.parquet"
)


def _positive_int(value: str) -> int:
    number = int(value)
    if number < 1:
        raise argparse.ArgumentTypeError("value must be at least 1")
    return number


def _nonnegative_int(value: str) -> int:
    number = int(value)
    if number < 0:
        raise argparse.ArgumentTypeError("value must not be negative")
    return number


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="benchmark.py",
        description="Benchmark OpenRouter flashcard generation and store the results in SQLite.",
    )
    parser.add_argument("--db", default=str(DEFAULT_DATABASE), help="SQLite results path")
    subparsers = parser.add_subparsers(dest="command", required=True)

    validate = subparsers.add_parser("validate", help="Validate a benchmark dataset")
    validate.add_argument("dataset", nargs="?", default=str(DEFAULT_DATASET))

    import_srs = subparsers.add_parser(
        "import-srs-highlights", help="Convert srs-highlights Parquet data to benchmark JSON"
    )
    import_srs.add_argument("parquet", nargs="?", default=str(DEFAULT_SRS_HIGHLIGHTS))
    import_srs.add_argument("--split", choices=("train", "test", "all"), default="test")
    import_srs.add_argument("--output")
    import_srs.add_argument("--revision")

    models = subparsers.add_parser("models", help="Find text models on OpenRouter")
    models.add_argument("query", nargs="?", default="")
    models.add_argument("--limit", type=_positive_int, default=20)

    estimate = subparsers.add_parser("estimate", help="Estimate a benchmark cost")
    estimate.add_argument("model", help="OpenRouter model slug to test")
    estimate.add_argument("--judge-model", default=os.getenv("BENCHMARK_JUDGE_MODEL"))
    estimate.add_argument("--dataset", default=str(DEFAULT_DATASET))
    estimate.add_argument("--cards-per-unit", type=_positive_int, default=3)
    estimate.add_argument("--judge-passes", type=_positive_int, default=3)
    estimate.add_argument("--limit", type=_positive_int)
    estimate.add_argument("--chars-per-token", type=float, default=4.0)
    estimate.add_argument("--generation-output-tokens-per-card", type=_positive_int, default=80)
    estimate.add_argument("--judge-output-tokens", type=_positive_int, default=180)
    estimate.add_argument("--estimated-card-chars", type=_positive_int, default=240)
    estimate.add_argument(
        "--response-retries", type=_nonnegative_int, default=DEFAULT_RESPONSE_RETRIES
    )

    run = subparsers.add_parser("run", help="Run a benchmark")
    run.add_argument("model", help="OpenRouter model slug to test")
    run.add_argument("--judge-model", default=os.getenv("BENCHMARK_JUDGE_MODEL"))
    run.add_argument("--dataset", default=str(DEFAULT_DATASET))
    run.add_argument("--cards-per-unit", type=_positive_int, default=3)
    run.add_argument("--judge-passes", type=_positive_int, default=3)
    run.add_argument("--seed", type=int, default=1)
    run.add_argument("--limit", type=_positive_int)
    run.add_argument("--utility-profile", choices=sorted(UTILITY_PROFILES), default="raw-v1")
    run.add_argument(
        "--response-retries", type=_nonnegative_int, default=DEFAULT_RESPONSE_RETRIES
    )

    resume = subparsers.add_parser("resume", help="Resume a failed benchmark run")
    resume.add_argument("run_id", type=_positive_int)
    resume.add_argument(
        "--response-retries", type=_nonnegative_int, default=DEFAULT_RESPONSE_RETRIES
    )

    history = subparsers.add_parser("history", help="Show benchmark runs over time")
    history.add_argument("--model")
    history.add_argument(
        "--filter",
        dest="dataset_filter",
        metavar="TEXT",
        help="Show datasets whose ID contains TEXT",
    )
    history.add_argument("--limit", type=_positive_int, default=20)

    leaderboard = subparsers.add_parser("leaderboard", help="Show the latest result for each model")
    leaderboard.add_argument(
        "--filter",
        "--dataset-id",
        dest="dataset_filter",
        metavar="TEXT",
        help="Show datasets whose ID contains TEXT",
    )

    show = subparsers.add_parser("show", help="Export one complete run as JSON")
    show.add_argument("run_id", type=int)
    return parser


def _key() -> str:
    key = os.getenv("OPENROUTER_API_KEY", "")
    if not key:
        raise ValueError("Set OPENROUTER_API_KEY before this command")
    return key


def _print_table(
    headers: tuple[str, ...], rows: list[tuple[str, ...]], right_aligned: set[int]
) -> None:
    widths = [
        max(len(headers[index]), *(len(row[index]) for row in rows))
        for index in range(len(headers))
    ]

    def line(values: tuple[str, ...]) -> str:
        return "  ".join(
            value.rjust(widths[index]) if index in right_aligned else value.ljust(widths[index])
            for index, value in enumerate(values)
        )

    print(line(headers))
    print("  ".join("-" * width for width in widths))
    for row in rows:
        print(line(row))


def _print_models(models: list[dict]) -> None:
    if not models:
        print("No models found.")
        return
    headers = ("Model ID", "Name", "Input $/M", "Output $/M", "Request $")
    rows: list[tuple[str, ...]] = []
    for model in models:
        prices = display_prices(model)
        rows.append(
            (
                str(model.get("id", "-")),
                str(model.get("name", "-")),
                f"${prices['prompt_per_million']:.4f}",
                f"${prices['completion_per_million']:.4f}",
                f"${prices['request']:.6f}",
            )
        )
    _print_table(headers, rows, {2, 3, 4})


def _print_runs(
    rows: list[object], *, include_dataset: bool = True, include_cost: bool = True
) -> None:
    if not rows:
        print("No benchmark runs found.")
        return
    headers = ["ID", "Started (UTC)", "Status", "Score", "Usable"]
    right_aligned = {0, 3, 4}
    if include_cost:
        headers.append("Cost")
        right_aligned.add(5)
    if include_dataset:
        headers.append("Dataset")
    headers.extend(("Generator", "Judge"))
    table_rows: list[tuple[str, ...]] = []
    for row in rows:
        score = "-" if row["mean_score"] is None else f"{row['mean_score']:.3f}"
        usable = "-" if row["usable_pct"] is None else f"{row['usable_pct']:.1f}%"
        values = [
            str(row["id"]),
            str(row["started_at"]),
            str(row["status"]),
            score,
            usable,
        ]
        if include_cost:
            values.append(f"${row['total_cost_usd']:.5f}")
        if include_dataset:
            values.append(str(row["dataset_id"]))
        values.extend(
            (str(row["generator_model_requested"]), str(row["judge_model_requested"]))
        )
        table_rows.append(tuple(values))
    _print_table(tuple(headers), table_rows, right_aligned)


def _print_summary(
    run_id: int, summary: dict[str, object], previous: dict[str, object] | None
) -> None:
    print(f"Run {run_id} completed.")
    if summary["ci_low"] is None or summary["ci_high"] is None:
        print(f"Score {summary['mean_score']:.3f} (95% CI unavailable)")
    else:
        print(
            f"Score {summary['mean_score']:.3f} "
            f"(95% CI {summary['ci_low']:.3f} to {summary['ci_high']:.3f})"
        )
    print(
        f"Usable {summary['usable_pct']:.1f}% | Coverage {summary['coverage']:.1f}% | "
        f"Cost ${summary['total_cost_usd']:.5f}"
    )
    print(f"Tiers {summary['tier_distribution']}")
    if previous:
        delta = float(summary["mean_score"]) - float(previous["mean_score"])
        print(
            f"Previous comparable run: {previous['id']} at {previous['started_at']}. "
            f"Score change: {delta:+.3f}."
        )
    else:
        print("No earlier comparable run exists.")


def main(argv: list[str] | None = None) -> int:
    args = _parser().parse_args(argv)
    try:
        if args.command == "validate":
            dataset = load_dataset(args.dataset)
            label = "smoke-test" if dataset.smoke_test else "benchmark"
            print(
                f"Valid {label} dataset: {dataset.dataset_id} {dataset.version}, "
                f"{len(dataset.target_units)} target units, SHA-256 {dataset.sha256}"
            )
            return 0

        if args.command == "import-srs-highlights":
            output, unit_count = import_srs_highlights(
                args.parquet,
                args.output,
                split=args.split,
                revision=args.revision,
            )
            print(f"Imported {unit_count} target units to {output}")
            return 0

        if args.command == "models":
            client = OpenRouterClient(_key())
            _print_models(client.models(args.query, args.limit))
            return 0

        if args.command == "estimate":
            if not args.judge_model:
                raise ValueError("Set --judge-model or BENCHMARK_JUDGE_MODEL")
            dataset = load_dataset(args.dataset)
            client = OpenRouterClient(_key())
            generator_model = client.model(args.model)
            judge_model = client.model(args.judge_model)
            estimate = estimate_cost(
                dataset,
                generator_model,
                judge_model,
                cards_per_unit=args.cards_per_unit,
                judge_passes=args.judge_passes,
                limit=args.limit,
                chars_per_token=args.chars_per_token,
                generation_output_tokens_per_card=args.generation_output_tokens_per_card,
                judge_output_tokens=args.judge_output_tokens,
                estimated_card_chars=args.estimated_card_chars,
                response_retries=args.response_retries,
            )
            generator_prices = display_prices(generator_model)
            judge_prices = display_prices(judge_model)
            print(f"Estimated OpenRouter credit cost: ${estimate.total_usd:.6f}")
            print(f"Maximum-output bound:             ${estimate.maximum_output_usd:.6f}")
            print(f"Maximum calls with retries:       {estimate.maximum_calls}")
            print()
            print("Phase       Calls  Input tokens  Output tokens  Estimated cost")
            print(
                f"Generation  {estimate.generation.calls:<5}  "
                f"{estimate.generation.input_tokens:<12}  {estimate.generation.output_tokens:<13}  "
                f"${estimate.generation.cost_usd:.6f}"
            )
            print(
                f"Judgment    {estimate.judgment.calls:<5}  "
                f"{estimate.judgment.input_tokens:<12}  {estimate.judgment.output_tokens:<13}  "
                f"${estimate.judgment.cost_usd:.6f}"
            )
            print()
            print(
                f"Generator price: ${generator_prices['prompt_per_million']:.4f}/M input, "
                f"${generator_prices['completion_per_million']:.4f}/M output, "
                f"${generator_prices['request']:.6f}/request"
            )
            print(
                f"Judge price:     ${judge_prices['prompt_per_million']:.4f}/M input, "
                f"${judge_prices['completion_per_million']:.4f}/M output, "
                f"${judge_prices['request']:.6f}/request"
            )
            print()
            print(
                f"Assumptions: {estimate.chars_per_token:g} characters per input token, "
                f"{estimate.generation_output_tokens_per_card} generation output tokens per card, "
                f"and {estimate.judge_output_tokens} output tokens per judgment."
            )
            print("The estimate excludes OpenRouter credit purchase fees and unlisted provider charges.")
            for warning in estimate.warnings:
                print(f"Warning: {warning}")
            return 0

        with ResultsDatabase(args.db) as database:
            if args.command == "history":
                _print_runs(
                    database.history(args.model, args.limit, args.dataset_filter),
                    include_dataset=not bool(args.dataset_filter),
                )
                return 0
            if args.command == "leaderboard":
                _print_runs(
                    database.leaderboard(args.dataset_filter),
                    include_dataset=not bool(args.dataset_filter),
                    include_cost=False,
                )
                return 0
            if args.command == "show":
                result = database.export_run(args.run_id)
                if result is None:
                    print(f"Run {args.run_id} does not exist.", file=sys.stderr)
                    return 1
                print(json.dumps(result, indent=2, ensure_ascii=False))
                return 0
            if args.command == "resume":
                stored = database.run(args.run_id)
                if stored is None:
                    raise ValueError(f"Run {args.run_id} does not exist")
                try:
                    config_values = json.loads(stored["config_json"])
                except json.JSONDecodeError as error:
                    raise ValueError(f"Run {args.run_id} has invalid configuration JSON") from error
                config_values["response_retries"] = args.response_retries
                config = RunConfig(**config_values)
                dataset = load_dataset(stored["dataset_path"])
                remaining = int(stored["unit_count"]) - int(stored["units_completed"])
                base_calls = remaining * (1 + config.cards_per_unit * config.judge_passes)
                print(
                    f"Resume plan: {remaining} units and about {base_calls} new paid calls."
                )
                print(
                    f"Invalid responses can raise this to {base_calls * (config.response_retries + 1)} calls."
                )
                run_id, summary, previous = run_benchmark(
                    database,
                    OpenRouterClient(_key()),
                    dataset,
                    config,
                    resume_run_id=args.run_id,
                )
                _print_summary(run_id, summary, previous)
                return 0
            if args.command == "run":
                if not args.judge_model:
                    raise ValueError("Set --judge-model or BENCHMARK_JUDGE_MODEL")
                dataset = load_dataset(args.dataset)
                selected_count = min(args.limit or len(dataset.target_units), len(dataset.target_units))
                calls = selected_count * (1 + args.cards_per_unit * args.judge_passes)
                if dataset.smoke_test:
                    print("Warning: this dataset is only a smoke test. Do not publish its score.")
                print(f"Plan: {selected_count} units and about {calls} paid OpenRouter calls.")
                print(
                    f"Invalid responses can raise this to {calls * (args.response_retries + 1)} calls."
                )
                run_id, summary, previous = run_benchmark(
                    database,
                    OpenRouterClient(_key()),
                    dataset,
                    RunConfig(
                        generator_model=args.model,
                        judge_model=args.judge_model,
                        cards_per_unit=args.cards_per_unit,
                        judge_passes=args.judge_passes,
                        random_seed=args.seed,
                        utility_profile=args.utility_profile,
                        limit=args.limit,
                        response_retries=args.response_retries,
                    ),
                )
                _print_summary(run_id, summary, previous)
                return 0
    except (DatasetError, OpenRouterError, ValueError, OSError) as error:
        print(f"Error: {error}", file=sys.stderr)
        return 1
    return 1
