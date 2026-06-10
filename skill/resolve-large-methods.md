# Resolving "Method code too large!" in Electric v3 prod builds

## Symptom

`clj -X:build:prod uberjar` fails during `[:prod] Compiling ...` with:

```
File: src/freememo/library_cards.cljc:421:1
Encountered error when macroexpanding hyperfiddle.electric3/defn.
IndexOutOfBoundsException: Method code too large!
```

The reported location is misleading.
It points at the FIRST `e/defn` in the file, not the oversized one.
During shadow's CLJS compile, lang3's `macroexpand-clj` runs
`(serialized-require (ns-name *ns*))` — a CLJ-side load of the whole namespace.
That load compiles every `e/defn` below, and whichever overflows the JVM's
64KB-per-method limit blows up attributed to the expansion site at the top.

## Why dev passes and prod fails

Plain `clj -M:build:prod -e "(require 'the.ns)"` succeeds on the same code.
In the shadow build, lang3's cljs analyzer atom (`!a`) is populated from the
namespaces shadow already compiled, so client-site code inside each `e/defn`
expands fully and the generated CLJ method is substantially larger than in a
plain require.
A component near the limit in plain mode overflows in shadow mode.
Conclusion: you cannot reproduce by evaluating/requiring — it is a
compile-time issue inside the prod shadow build only. Don't waste time
re-running requires; go straight to splitting.

## Finding the offender without rebuilding

The full build takes minutes per attempt. Instead, AOT-compile the namespace
and rank generated class sizes — relative size in plain mode predicts the
shadow-mode offender:

```bash
mkdir -p /tmp/cmcls
clj -J-Xss8m -M:build:prod -e \
  "(binding [*compile-path* \"/tmp/cmcls\"] (compile 'freememo.library-cards))"
ls -lS /tmp/cmcls/freememo/library_cards* | head
```

Calibration from this incident:

- `LibraryCardsView$fn` at **93KB** plain → overflowed in the shadow build.
- Known-good components (`BibliographyDialog` 50KB, `Typeahead` 36KB) build fine.
- **Rule of thumb: keep every `e/defn`'s plain-compile class under ~50KB; aim ≤40KB.**

Iterate: split → recompile to /tmp → re-rank → repeat until the whale is gone.
Only then pay for the full build.

## How to split

Extract child `e/defn`s and pass everything as **positional args** (never atoms
in maps — CLAUDE.md serialization rule; never `e/Token` inputs through maps —
reactive-loop rule).
Server-sited values (`result`, `cards-vec`, `manifest`) pass through `e/defn`
args fine; siting is preserved, so `(e/server (:cards result))` inside the
child stays server-side.

Splits applied to `library_cards.cljc` (93KB → all ≤36KB):

1. **Row cells out of `LibraryCardRow`**: `RowSelectCell`, `RowDiffCell`,
   `RowContentCells`, `RowDocCell`.
2. **Big view chunks out of `LibraryCardsView`**: `CardsFilterBar`,
   `CardsTableHeader` (sort state watched inside via `!sort-col`/`!sort-dir`),
   `CardsTableBody`, `CardsCountSummary` (takes `result`, destructures counts
   itself).
3. **State machine halves**: `BulkActionRunner` → `BulkPushRunner` +
   `BulkPullRunner`, composed by a thin `BulkActionRunner`.
4. **Token-bearing buttons**: `BulkPushButton`, `BulkPullButton` out of
   `BulkActionBar`; `BulkDeleteButton` out of `BulkDeleteConfirmModal`.
5. **Modal content**: `CardDiffSections` out of `CardDiffModal`.
6. **Layered ownership** — the decisive cut. 93KB → 57KB wasn't enough;
   57→53 wasn't either. The let machinery itself is heavy: each binding,
   watch, `e/server` block, and call argument costs caller bytecode.
   Final layering:
   - `LibraryCardsView`: creates the 9 state atoms + `opts`/`filters-active?`,
     makes ONE call to `CardsQueryRegion`.
   - `CardsQuery`: the `e/server` query with its reactivity-channel watches.
   - `CardsQueryRegion`: everything downstream of `result` (filter bar, edit
     modal, error branch).
   - `CardsSelectionRegion`: owns selection/bulk atoms; mounts `CardsModals`,
     `BulkActionRunner`, summary, bar, `CardsTables`.
   - `CardsTables`: server destructures + header/body calls.

## What does NOT help

- Splitting the `e/defn` the error message points at (it was 12 lines).
- Re-running `require` under different aliases — plain loads always pass.
- Bigger `-Xss` — stack size is unrelated to method size.

## Gotchas

- Moving a section out can orphan let bindings (e.g. `sort-click`) — remove
  them; unused bindings in Electric `let`s are forbidden anyway.
- Components must be defined before their callers in the file (no forward refs).
- A failed build can leave a shadow-cljs server holding port 9630:
  `shadow-cljs already running in project on http://localhost:9630`.
  Kill it before rerunning the build.
- Local atoms moved into a child mounted under a conditional (e.g. selection
  atoms under the query success branch) reset if that branch remounts —
  acceptable here (only flips on query error), but check before moving state.
