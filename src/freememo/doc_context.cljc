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
(e/declare is-score?)
(e/declare is-poem?)
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
(e/declare effective-content-topic-id)  ; topic the shown editor body was fetched for; ≠ page-topic-id ⇒ stale (loading)
(e/declare static-content)
(e/declare content-text)
(e/declare initial-html)
(e/declare citation)
(e/declare page-info)
(e/declare pdf-status)
(e/declare extract-status)

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
(e/declare unsynced-count)

;; ── Layout / display ─────────────────────────────────────────────────────
(e/declare layout)
;; Focus reader. PDF views remove the PDF pane. Supported text topics center
;; their editor. Special topic views do not offer this mode.
(e/declare focus?)
(e/declare focus-available?)
(e/declare focus-scope-id)
;; Hidden editor pane (plans/editor-pane-toggle.md). `has-source-pane?` is true
;; for a PDF, a video and a git forge file — the only views where a pane sits
;; beside the editor. `editor-hidden?` implies it, and never holds with focus?.
(e/declare has-source-pane?)
(e/declare editor-hidden?)
(e/declare toggle-editor-hidden!)
(e/declare t-editor-hidden)
(e/declare editor-hidden-save)
;; Merged reading: the whole PDF as one document with a boundary marker per page
;; (plans/merged-page-reading.md). True only in focus mode on a real PDF.
;; `merged-page-numbers` is what the split needs — the editor cannot report a
;; page whose marker the reader deleted.
(e/declare merged?)
(e/declare merged-page-numbers)
;; The identity of the DOCUMENT the editor holds, which is not always the topic
;; whose id sits in `topic-id`.
;;
;; Per-page mode: the page topic — the two agree.
;; Merged mode: the PDF ROOT. The editor holds every page, so the page the
;; reader has scrolled to must not read as a different document. It did: a
;; scroll moved `topic-id`, the component read that as a topic switch and called
;; `set-content!`, which re-parsed the document and scrolled it back to the top —
;; reporting page 1 again. A scroll fed itself.
;;
;; `topic-id` still follows the page, because a new extract or card must be
;; parented to the page the selection came from.
(e/declare editor-doc-id)
(e/declare top-pct)
(e/declare card-table-open?)
(e/declare top-split-pct)
(e/declare left-pct)
(e/declare transcript-pct)
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
(e/declare set-focus!)
(e/declare reset-split!)
(e/declare set-card-table-open!)
(e/declare toggle-card-table!)
(e/declare on-page-change!)
(e/declare on-layout-toggle!)
(e/declare on-total!)
(e/declare on-navigate!)
(e/declare on-imported-navigate!)
(e/declare on-done!)
(e/declare t-layout)
(e/declare layout-save)
(e/declare t-focus)
(e/declare focus-save)
;; Last-page persistence. DocumentViewState owns the request atom, DocumentBody
;; writes the setting — the pane cannot, because focus reader unmounts it.
(e/declare t-last-page)
(e/declare last-page-save)

;; ── Client atoms (the atom refs themselves, for child mutation) ────────────
(e/declare !show-bib)
(e/declare !current-page)
(e/declare !total)
(e/declare !nav-target)
(e/declare !top-split-pct)
(e/declare !left-pct)
(e/declare !transcript-pct)
(e/declare !top-pct)
(e/declare !top-pct-save)
(e/declare !use-context)
(e/declare !context-window)
(e/declare !card-type)
(e/declare !card-count)

;; ── Video state (kind='video' topics only) ─────────────────────────────────
;; The <video> element is the single playback clock; !video-el publishes the
;; node so the transcript pane can seek it directly (a browser-local effect,
;; no token). See plans/incremental-video.md §4.8.
(e/declare is-video?)
(e/declare video-topic-id)
(e/declare video-title)
(e/declare video-duration-ms)
(e/declare video-last-pos-ms)   ; resume position, read once at mount
(e/declare video-has-audio?)    ; extracted MP3 exists ⇒ a waveform can render

;; ── Code state (files imported from a git forge) ───────────────────────────
;; plans/git-forge-integration.md §4.4. A `code` topic from an older `kg-code`
;; import is NOT is-code?: it has no repo_path and no pane.
(e/declare is-code?)            ; mount the read-only code pane beside the editor
(e/declare code-has-source?)    ; false for an empty leaf — no topic_code row
(e/declare !video-el)           ; the HTMLVideoElement, or nil before mount
(e/declare !video-region)       ; {:start-ms N :end-ms N} or nil — marked range
;; Playhead, republished by the player on every `timeupdate` (3.6 Hz in Chromium,
;; the browser's own throttle) and on every `seeked`:
;;
;;   {:topic-id N   the video this reading belongs to
;;    :ms N|nil     currentTime in ms
;;    :seeks N}     monotonic count of `seeked` events
;;
;; `:topic-id` is what makes the reading safe to consume: navigating video→video
;; does not guarantee that the old player unmounts before the new one mounts, so
;; a consumer compares the id rather than trusting a cleanup on teardown.
;; `:seeks` distinguishes a scrub or a transcript-row click from ordinary
;; playback, which is how the transcript re-arms its follow after the user has
;; scrolled away.
(e/declare !video-playhead)

;; ── Score editor state (kind='score' topics only) ──────────────────────────
(e/declare !score-region)       ; {:start-ms N :end-ms N} or nil — waveform selection
(e/declare !score-pages)        ; {page {:width :height :rects [{:x :y :w :h}]}} — pending rects
(e/declare !score-modal-open?)  ; rect snapshot modal visibility
(e/declare !score-edit)         ; {:group-id N} while editing an existing pair
