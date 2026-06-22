# Learn Dashboard — Spec

Status: design + spec complete, implementation pending.
A stats dashboard rendered below the Learn-tab table in `LearnOverview` (`src/freememo/learn_page.cljc`).

## Decisions

| # | Decision | Choice |
|---|---|---|
| Window | Bar-chart timespan | 14 days |
| Event | What counts as "studied" | `advance` events only |
| Unit | Topics-studied counting | distinct topics per day (a topic studied twice/day = 1) |
| Layout | Composition below table | full-width retro chart was upgraded to twin side-by-side charts; stat row below |
| Chart tech | Rendering | SVG via `hyperfiddle.electric-svg3` |
| Teasers | Coming-soon stat style | promise copy + `Soon` badge (P3) |
| Status form | Active/done/dismissed | slim horizontal stacked bar + legend |
| D1 | Streak break rule | Anki-style hold: anchor = today if studied else yesterday; zero today does not break |
| D2 | "Reviews this week" | rolling 7 days |
| D3 | Page overflow | shrink table `visible-rows` so table + dashboard fit one viewport; page does not scroll |

## Composition (top → bottom, below `.table-frame`)

1. KPI strip — `🔥 Streak` · `Reviews this week` · `Reviews all-time`
2. Twin charts side-by-side — `Studied · last 14d` | `Due · next 14d` (shared chart component, today's bar accented in both)
3. Status bar — slim stacked active/done/dismissed + legend
4. Teaser cards — `Total study time` | `Expected study time` (promise + `Soon` badge; no real value computed)

## Data layer (`src/freememo/db.clj`)

| Item | Function | Source | Notes |
|---|---|---|---|
| 1.1 | `get-study-calendar` (new) | `topic_repetitions` | `COUNT(DISTINCT topic_id)` per `DATE(event_at)`, `event_type='advance'`, last 14d |
| 1.2 | `get-review-calendar` (exists, db.clj:2255) | `topics.next_review_at` | call with `days=14`; no new code |
| 1.3 | `get-study-streak` (new) | `topic_repetitions` | consecutive days w/ ≥1 advance; Anki-style anchor |
| 1.4 | `get-review-counts` (new) | `topic_repetitions` | `{:all-time :this-week}`, advance count; week = rolling 7d |
| 1.5 | `get-status-breakdown` (new) | `topics.status` | `{:active :done :dismissed}`; scope `kind!='page' AND staged_delete_id IS NULL`; NEW SQL — `get-queue-summary` does not expose `dismissed` |

All day-bucketing uses server-zone SQL `DATE()`/`CURRENT_DATE`; no per-user timezone (none stored).

## View layer (`src/freememo/learn_page.cljc`)

- `Bar-chart` (new `e/defn`): inputs = 14-elem zero-filled `[{:date :count}]` + accent-today flag; SVG `svg/rect` bars, height = `count/max`, empty days = faint stub, today accented. Guard `max=0`.
- Dashboard container: sibling after `.table-frame` (after learn_page.cljc:181), inside `.page-container`.
- New queries bound via `(e/server (fn refresh user-id))` taking the existing `:refresh` atom (matches `get-queue-summary*`, learn_page.cljc:69) so they recompute on mutation.
- Reduce `visible-rows` (currently 10, learn_page.cljc:108) so table + dashboard fit; final value tuned at render.
- Styling: existing `index.css` tokens only (`.card`, `.section-title`, `--color-primary`, `--color-success`, `--color-bg-subtle`, `--sp-*`); `light-dark()` dark mode; no new deps.

## Out of scope (WONT)

- Total / expected study-time values — no duration data captured anywhere.
- Retention / accuracy — no grade/fail event recorded.
- Per-user timezone.
- `touch` / `postpone` events in any metric (advance-only).

## Implementation-time uncertainties

- `svg/text` availability in `electric-svg3` unverified (`icons.cljc` uses path/line/circle/rect/polyline only); fallback = HTML labels under the SVG.
- Final `visible-rows` value tuned against rendered dashboard height.
