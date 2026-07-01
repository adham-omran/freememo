(ns freememo.doc-context
  "Ambient reactive context for the document view + its toolbars.

   Replaces the wide property maps that were threaded from TopicPage down
   through DocumentBody → DocumentColumns/DocumentToolbars → … and the toolbar
   sub-tree. Each value is its own Electric dynamic var (per-value work-skipping:
   a reader re-runs only when a var it reads changes). TopicPage `binding`s the
   base set at the root; intermediate frames re-`binding` the values they derive
   or remap (e.g. topic-id ← page-topic-id) for their sub-tree.

   Names are earmuff-free to match Electric's own e/declare convention
   (hyperfiddle.electric-dom3/node, history4/history); the `dctx/` alias marks
   them as ambient at the read site. Atom refs keep the `!` prefix.

   All vars carry CLIENT values (the whole document tree renders under e/client;
   server-fetched values are already transferred before binding). The two
   genuinely server-registry values — scanning-pages, ocr-errors — are NOT vars
   here; their consumers read (us/get-atom …) directly server-side."
  (:require [hyperfiddle.electric3 :as e]))

;; ── Identity / topic resolution ──────────────────────────────────────────
(e/declare user-id)
(e/declare enc-key)
(e/declare topic-id)
(e/declare page-topic-id)
(e/declare kind)
(e/declare pdf-root-id)
(e/declare root-topic-id)
(e/declare bib-topic-id)
(e/declare queue-ctx)
(e/declare origin)

;; ── Flags ────────────────────────────────────────────────────────────────
(e/declare is-pdf?)
(e/declare is-live?)
(e/declare pdf-root?)
(e/declare pdf-has-file?)
(e/declare has-file?)          ; PdfPane's key, ← pdf-has-file?
(e/declare phone?)
(e/declare reading-mode?)
(e/declare audio?)
(e/declare top-bottom?)
(e/declare is-pdf-page?)
(e/declare llm-enabled?)
(e/declare scanning?)
(e/declare compact?)

;; ── Page / navigation ──────────────────────────────────────────────────────
(e/declare current-page)
(e/declare total)
(e/declare initial-page)
(e/declare target-page)
(e/declare page-number)
(e/declare pdf-page-count)
(e/declare reload-nonce)       ; PdfViewerComponent's key, ← pdf-page-count
(e/declare document-id)        ; ← pdf-root-id

;; ── Content ────────────────────────────────────────────────────────────────
(e/declare effective-content)
(e/declare static-content)
(e/declare content-text)
(e/declare initial-html)
(e/declare citation)
(e/declare page-info)
(e/declare pdf-status)
(e/declare extract-status)
(e/declare card-refresh)

;; ── LLM / generation ─────────────────────────────────────────────────────
(e/declare card-type)
(e/declare card-count-val)
(e/declare use-context)
(e/declare context-window)
(e/declare context-tooltip)
(e/declare context-mode)
(e/declare gen-active?)
(e/declare gen-pending)
(e/declare gen-error)
(e/declare mod-key)
(e/declare unsynced-count)

;; ── Layout / display ─────────────────────────────────────────────────────
(e/declare layout)
(e/declare top-pct)
(e/declare top-split-pct)
(e/declare left-pct)
(e/declare show-bib?)
(e/declare scan-dpi)
(e/declare scanning-pages)
(e/declare ocr-errors)
(e/declare card-font-size)
(e/declare extract-style)
(e/declare variant)
(e/declare audio-topic-id)

;; ── Callbacks / tokens ─────────────────────────────────────────────────────
(e/declare navigate!)
(e/declare toggle-layout!)
(e/declare reset-split!)
(e/declare on-page-change!)
(e/declare on-layout-toggle!)
(e/declare on-total!)
(e/declare on-navigate!)
(e/declare on-imported-navigate!)
(e/declare on-done!)
(e/declare t-layout)
(e/declare layout-save)

;; ── Client atoms (the atom refs themselves, for child mutation) ────────────
(e/declare !show-bib)
(e/declare !current-page)
(e/declare !total)
(e/declare !nav-target)
(e/declare !top-split-pct)
(e/declare !left-pct)
(e/declare !top-pct)
(e/declare !top-pct-save)
(e/declare !use-context)
(e/declare !context-window)
(e/declare !card-type)
(e/declare !card-count)
