# Card generation benchmark

This extension compares OpenRouter models that generate question-and-answer flashcards. It stores each run in a local SQLite database.

The benchmark uses grounded judgments. The judge compares each generated card with human-rated cards for the same target unit.

The extension does not use the FreeMemo database. It also does not use FreeMemo credits.

## Status

This version is an evaluation harness, not a validated benchmark. It includes a
two-unit synthetic smoke test and a 32-unit human-rated test dataset.

The human-rated dataset supports exploratory model comparisons. Do not publish
its scores until the judge passes human calibration and control checks.

## Requirements

- Python 3.10 or later
- An OpenRouter API key
- An OpenRouter model slug for generation
- A separate OpenRouter model slug for judgment

The benchmark runtime uses only the Python standard library. The dataset import
command also needs PyArrow.

The client uses OpenRouter's [chat completion API](https://openrouter.ai/docs/api/api-reference/chat/send-chat-completion-request) for generation and judgment. The `models` command uses the [models API](https://openrouter.ai/docs/api/api-reference/models/get-models).

## Quick start

Run all commands from this directory.

```bash
export OPENROUTER_API_KEY='...'

python3 benchmark.py validate dataset.example.json
python3 benchmark.py models 'flash'

python3 benchmark.py estimate google/gemini-2.5-flash \
  --judge-model anthropic/claude-sonnet-4.5 \
  --dataset dataset.example.json

python3 benchmark.py run google/gemini-2.5-flash \
  --judge-model anthropic/claude-sonnet-4.5 \
  --dataset dataset.example.json \
  --cards-per-unit 3 \
  --judge-passes 3
```

The model table shows input and output prices in dollars per million tokens. It
also shows the fixed price for each request.

The command prints the expected API call count before the first request. Each target unit needs one generation call and one call per judgment pass for each card.

The `estimate` command reads the current prices from OpenRouter. It makes no paid completion requests.

The estimate includes two values. The practical estimate uses configurable output-token assumptions. The maximum-output bound uses the request limits from the benchmark code.

```bash
python3 benchmark.py estimate GENERATOR_MODEL \
  --judge-model JUDGE_MODEL \
  --dataset my-dataset.json \
  --cards-per-unit 3 \
  --judge-passes 3
```

The estimate approximates input tokens from text length. Actual tokenization, reasoning charges, provider routing, and cache behavior can change the final cost.

After a run, use the stored `usage.cost` values for the actual OpenRouter credit cost.

The run command prints these results:

- The mean completion score and its 95% confidence interval
- The percentage of T2 and T3 cards
- Dataset coverage
- OpenRouter cost, when the response includes it
- The change from the last comparable run

The program records failed runs and all completed API calls. This data gives a cost record when a later request fails.

The runner retries an invalid generation or judgment response two times. It
raises the output-token limit when a response reaches the current limit. Each
retry is a paid API call, and the database includes its cost.

Resume a failed run without repeating its completed units:

```bash
python3 benchmark.py resume RUN_ID
```

For the failed run in the example terminal output, use `python3 benchmark.py
resume 4`. The command keeps its nine completed units and starts with unit 10.
Use `--response-retries 0` to disable malformed-response retries.

## Review past results

```bash
python3 benchmark.py history
python3 benchmark.py history --model google/gemini-2.5-flash
python3 benchmark.py history --filter srs-highlights
python3 benchmark.py leaderboard --filter srs-highlights
python3 benchmark.py show 1
```

Use `--db PATH` before the command to select a different SQLite file.

```bash
python3 benchmark.py --db /tmp/card-benchmark.sqlite3 history
```

The leaderboard shows the newest comparable result for each generator model. A comparable run has the same dataset hash, judge model, prompt hash, and utility profile.
It omits total benchmark cost because judge calls can dominate that value. The
history and run details keep the total cost for auditing.

The history and leaderboard tables show the requested generator and judge
models. Use `show RUN_ID` to inspect resolved model names and provider details.
The `--filter TEXT` option matches any part of a dataset ID without regard to
case. Filtered tables omit the repeated dataset column. The leaderboard also
accepts `--dataset-id` as an alias for this option.

## Dataset design

Copy `dataset.example.json` and change its identifier and version. Each target unit must have one human-rated reference card for every tier.

The repository also contains a pinned copy of the public
[`laddermedia/srs-highlights`](https://huggingface.co/datasets/laddermedia/srs-highlights)
dataset in `data/srs-highlights`. The copy includes all three Parquet files and
an upstream dataset card. Its `manifest.json` records the revision, row counts,
and SHA-256 hashes.

Create the locked test dataset with these commands:

```bash
python3 -m pip install -r requirements-import.txt
python3 benchmark.py import-srs-highlights
python3 benchmark.py validate data/srs-highlights/grounded-test.json
```

The default import reads the pinned `grounded` Parquet file. It selects the 32
test rows and writes `data/srs-highlights/grounded-test.json`. Use this file with
the existing commands:

```bash
python3 benchmark.py estimate GENERATOR_MODEL \
  --judge-model JUDGE_MODEL \
  --dataset data/srs-highlights/grounded-test.json

python3 benchmark.py run GENERATOR_MODEL \
  --judge-model JUDGE_MODEL \
  --dataset data/srs-highlights/grounded-test.json
```

The importer checks the source file against `manifest.json`. It records the
dataset revision and source checksum in the generated JSON. It uses each source
summary as context because the published Parquet file does not contain full
articles.

Use `--split train` to create `grounded-train.json` for development. The importer
uses a split-specific output name unless you set `--output`. Do not use the test
split while you change prompts or judge settings.

The four tiers are:

- T3: Ready to review
- T2: Needs polish
- T1: Needs refactor
- T0: Off-target or factually wrong

The default `all-tiers-v1` policy requires all four tiers. The imported dataset
uses `at-least-two-v1` because the public data does not include every tier for
each highlight. See `dataset.schema.json` for the file shape.

Use 60 to 150 target units for a useful internal comparison. Stratify the data by domain and source type.

Keep generation data separate from reference cards. The generator prompt cannot access the human-rated reference cards.

## Scoring

The default `raw-v1` profile uses these utilities:

| Tier | Utility |
|---|---:|
| T0 | -1.0 |
| T1 | -3.0 |
| T2 | 1.5 |
| T3 | 2.0 |

For one target unit, the completion score is the best card utility plus every negative card utility. This rule rewards one useful card and charges for each bad card the user must inspect.

The optional `memory-machines-posterior-v1` profile copies the published posterior utilities from the Memory Machines report. Use it only for sensitivity analysis.

Do not treat that profile as local judge calibration. A later benchmark version must calculate posterior utilities from a held-out, human-rated calibration set.

## Reproducibility data

Each run stores:

- UTC start and finish times
- Requested and resolved model names
- The provider name and OpenRouter generation ID for each call
- The dataset identifier, version, path, and SHA-256 hash
- The generation and judgment prompt hash
- The random seed, card count, pass count, and utility profile
- Generated cards, each judgment response, token counts, latency, and cost
- Partial progress and the error from a failed run

Run the same fixed dataset at different dates to detect model alias changes or provider changes. Prefer a pinned model slug when OpenRouter offers one.

## Test

```bash
python3 -m unittest discover -s tests -v
```

The tests do not call OpenRouter.

## Current limits

- The harness supports question-and-answer cards only.
- It does not calculate a local judge confusion matrix.
- It does not calculate Cohen's kappa or criterion F1 scores.
- It does not deduplicate semantically similar cards.
- The confidence interval uses a normal approximation. Use bootstrap intervals for a publication-quality report.
- The leaderboard does not calculate Elo or Bradley-Terry scores.

These limits keep the first version small. The stored raw judgments support later scoring changes without new generation calls.
