(ns freememo.topic-state
  "TopicPage's data-resolution and reactive view-state, factored into their own
   namespace so each piece compiles to a separate JVM method. Electric compiles
   every e/defn to one method (64KB bytecode cap) and inlines same-namespace
   e/defn calls; keeping these three providers cross-namespace stops TopicPage's
   method from overflowing (mirrors freememo.document-body).

   The three providers each return a map keyed by DocumentBody's prop names, so
   TopicPage composes them with a single merge — no prop-map literal of its own:
     ResolveTopic         — server-resolved topic facts + caller passthrough
     DocumentLayoutState  — content/card split + PDF pane layout (client)
     DocumentViewState    — current page, navigation, content, biblio toggle (client)"
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.viewport :as viewport]
   [freememo.bibliography-form :as bibform]
   [clojure.string :as str]
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.db :as db])))

;; ---------------------------------------------------------------------------
;; Server query helpers (moved verbatim from freememo.topic-page)
;; ---------------------------------------------------------------------------

(defn get-topic-overview*
  "Single round-trip resolving a topic id to everything TopicPage needs.
   Returns nil if topic-id is nil or topic doesn't exist."
  [_refresh topic-id]
  #?(:clj
     (when topic-id
       (when-let [topic (db/get-topic topic-id)]
         (let [kind (:topics/kind topic)
               parent-id (:topics/parent_id topic)
               page-number (:topics/page_number topic)
               root-id (or (db/get-root-topic-id topic-id) topic-id)
               root (when (not= root-id topic-id) (db/get-topic root-id))
               root-kind (:topics/kind (or root topic))
               ;; Score topics ride the PDF viewer machinery: their role='main'
               ;; blob is the sheet-music PDF, so they resolve as their own
               ;; pdf-root. Score-specific UI gates on :is-score? on top.
               pdf-root-id (cond
                             (= kind "pdf") topic-id
                             (= kind "score") topic-id
                             (and (= kind "page") (= root-kind "pdf")) root-id
                             :else nil)
               pdf-root-topic (cond (= kind "pdf") topic
                                    (= kind "score") topic
                                    (and (= kind "page") (= root-kind "pdf")) (or root topic)
                                    :else nil)
               video-row (when (= kind "video") (db/get-topic-video topic-id))]
           ;; Scalars only — TopicPage destructures this map client-side, so
           ;; every key crosses the wire on each :refresh tick. Content bodies
           ;; are fetched separately: get-topic-content* (gated off for PDFs)
           ;; and content-toolbar-generate/get-parent-content* (server-side
           ;; at the point of use).
           {:kind kind
            :parent-id parent-id
            :page-number page-number
            :pdf-root-id pdf-root-id
            :is-score? (= kind "score")
            ;; Video: three scalars read once per topic load. duration and
            ;; last-pos come from topic_videos; has-audio? gates the waveform
            ;; strip, which cannot render before ffmpeg has produced the MP3.
            :is-video? (= kind "video")
            :video-duration-ms (:duration_ms video-row)
            :video-last-pos-ms (:last_pos_ms video-row)
            :video-has-audio? (and (some? video-row)
                                (db/topic-file-exists? topic-id "audio"))
            ;; Live Document: drives the viewer's add-photos affordance,
            ;; empty-state, and (via page count) post-append reload.
            :is-live? (boolean (:topics/is_live pdf-root-topic))
            :pdf-has-file? (boolean (and pdf-root-id (db/topic-file-exists? pdf-root-id)))
            :pdf-page-count (when pdf-root-id (db/count-pages pdf-root-id))
            :root-topic-id root-id
            :title (:topics/title topic)
            :root-title (or (:topics/title root) (:topics/title topic))
            :status (or (:topics/status topic) "active")})))
     :cljs nil))

(defn get-topic-content*
  "Topic's own content body. Separate from get-topic-overview* so the
   client-destructured overview stays scalar-sized; PDF topics never fetch
   this (their pane content comes from get-page-text*)."
  [_refresh topic-id]
  #?(:clj (when topic-id
            (or (:topics/content (db/get-topic topic-id)) ""))
     :cljs nil))

(defn get-page-info*
  "For a PDF root and 1-indexed page number, returns
     {:topic-id <page-topic-id> :done <int> :total <int> :remaining-tooltip <str>}.
   Re-runs on :refresh and :meta-refresh."
  [_refresh _meta-refresh pdf-root-id page-number]
  #?(:clj
     (when (and pdf-root-id page-number)
       (let [pages (db/list-pages pdf-root-id)
             remaining (sort (map :topics/page_number
                               (remove #(= "done" (:topics/status %)) pages)))
             current-page (first (filter #(= (:topics/page_number %) page-number) pages))]
         {:topic-id (:topics/id current-page)
          :done (- (count pages) (count remaining))
          :total (count pages)
          ;; Tooltip built server-side — the client only ever displayed this
          ;; string; shipping the raw page-number vec was wire waste.
          :remaining-tooltip
          (cond
            (empty? remaining) "All pages done!"
            (<= (count remaining) 20)
            (str "Remaining: " (str/join ", " remaining))
            :else
            (str "Remaining: " (str/join ", " (take 20 remaining))
              " ... and " (- (count remaining) 20) " more"))}))
     :cljs nil))

(defn get-page-text*
  "PDF page content (HTML). Wraps freememo.page-ocr/get-page-text."
  [_refresh pdf-root-id page-number]
  #?(:clj (when (and pdf-root-id page-number)
            (page/get-page-text pdf-root-id page-number))
     :cljs nil))

(defn get-merged-document*
  "The whole PDF as one document, with a boundary marker per page
   (plans/merged-page-reading.md item 1.1).

   Returns `{:html <merged> :page-numbers [ints]}`, or nil when there is no PDF
   root. The page-number list rides along because the split needs it: the editor
   cannot report a page whose marker the reader deleted.

   Re-fetches on `:refresh`, which is what an OCR scan bumps — a page scanned
   while the mode is on has to appear."
  [_refresh pdf-root-id]
  #?(:clj (when pdf-root-id (db/merged-page-document pdf-root-id))
     :cljs nil))

(defn save-merged-document!*
  "Split a merged document back into its page rows.

   Returns `{:success true :written N}` or `{:success false :error msg}`. Bumps
   no invalidation channel: the existing per-page auto-save bumps none either,
   because `:refresh` would re-fetch and re-initialize Quill mid-typing."
  [pdf-root-id merged page-numbers]
  #?(:clj (try
            (let [r (db/save-merged-pages! pdf-root-id merged page-numbers)]
              {:success true :written (:written r)})
            (catch Exception e
              (tel/error! {:id ::save-merged-document :data {:root pdf-root-id}} e)
              {:success false :error (.getMessage e)}))
     :cljs nil))

(defn get-topic-status*
  "Latest status for a topic id. Re-fetches on :meta-refresh."
  [_meta-refresh topic-id]
  #?(:clj (when topic-id (or (:topics/status (db/get-topic topic-id)) "active"))
     :cljs nil))

(defn default-split-pct []
  #?(:cljs (if (< (.-innerHeight js/window) 900) 50 75)
     :clj 75))

;; ---------------------------------------------------------------------------
;; Prop providers — each returns a map keyed by DocumentBody prop names
;; ---------------------------------------------------------------------------

(e/defn ResolveTopic
  "Server-resolved topic facts plus verbatim passthrough of the caller's args.
   Every value is current-page-independent, so it lives in its own method away
   from the view-state that cycles through the client-owned current page.

   Pre:  topic-id is non-nil (TopicPage guards the nil case before calling).
   Post: returns a map keyed by DocumentBody prop names; :initial-layout and
         :initial-top-pct seed DocumentLayoutState; :refresh/:meta-refresh and
         the topic scalars seed DocumentViewState."
  [user-id enc-key topic-id navigate! llm-enabled? queue-ctx url-page]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          meta-refresh (e/server (e/watch (us/get-atom user-id :meta-refresh)))
          sync-mutations (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-mutations (e/server (e/watch (us/get-atom user-id :card-mutations)))
          card-refresh (+ refresh sync-mutations card-mutations)

          ;; Every read below is a blocking JDBC call. A bare e/server runs it on
          ;; the session's reactive thread, so the whole graph stalls for its
          ;; duration; e/Offload moves it to the thread pool. They also run
          ;; concurrently there instead of one after another.
          card-font-size (e/server (e/Offload #(settings/get-card-font-size user-id)))
          scan-dpi (e/server (e/Offload #(settings/get-scan-dpi user-id)))

          overview (e/server (e/Offload #(get-topic-overview* refresh topic-id)))
          kind (:kind overview)
          pdf-root-id (:pdf-root-id overview)
          is-pdf? (some? pdf-root-id)
          is-score? (boolean (:is-score? overview))
          root-topic-id (:root-topic-id overview)
          ;; Content body fetched separately and gated: PDF panes get their
          ;; text from get-page-text*, so fetching the topic's stored content
          ;; for them was a dead transfer. Score topics ARE pdf-paned but have
          ;; no page children — their editor shows the topic's own content.
          ;; Bundle the body with the topic-id it was fetched for, in ONE server
          ;; value: on a topic switch the pending re-fetch latches the PREVIOUS
          ;; tuple (empty-amb semantics — the value never goes nil; see
          ;; freememo.loading), so a downstream reader can compare the fetched-for
          ;; id against the live topic-id to detect that the shown body is stale.
          static-content+id (e/server (when (or (not is-pdf?) is-score?)
                                        (e/Offload
                                          (fn [] [topic-id (get-topic-content* refresh topic-id)]))))
          static-content (when static-content+id (nth static-content+id 1))
          static-content-topic-id (when static-content+id (nth static-content+id 0))
          ;; initial-page precedence: URL page anchor > persisted last-page > 1
          ;; for a PDF root; a page-topic carries its page number intrinsically.
          ;; The last-page fetch is skipped when the URL already supplies a page.
          initial-page (when is-pdf?
                         (cond
                           (= kind "page") (:page-number overview)
                           (= kind "pdf") (or url-page
                                              (e/server (e/Offload #(settings/get-last-page user-id pdf-root-id)))
                                              1)
                           :else 1))
          initial-layout (e/server
                           (when is-pdf?
                             (e/Offload #(or (settings/get-pdf-layout user-id pdf-root-id) "left-right"))))
          ;; Focus reader — per PDF root, separate from the split orientation.
          initial-focus? (e/server
                           (boolean (when is-pdf?
                                      (e/Offload #(settings/get-pdf-focus user-id pdf-root-id)))))
          initial-top-pct (e/server (e/Offload #(settings/get-card-split user-id)))

          ;; Bibliography target: PDF pages own no bibliography (resolve to the
          ;; document root); every other kind (extract/root/web/epub) is its own
          ;; owner. get-topic-citation* resolves the effective source from here.
          bib-topic-id (if (= kind "page") (or pdf-root-id root-topic-id) topic-id)
          pdf-root? (and is-pdf? (= kind "pdf"))
          citation (e/server (e/Offload #(bibform/get-topic-citation* refresh user-id bib-topic-id)))
          pdf-status (e/server (when pdf-root?
                                 (e/Offload #(get-topic-status* meta-refresh pdf-root-id))))
          scanning-pages (e/server (e/watch (us/get-atom user-id :scanning-pages)))
          ocr-errors (e/server (e/watch (us/get-atom user-id :ocr-errors)))]
      {;; caller passthrough
       :user-id user-id :enc-key enc-key :navigate! navigate!
       :llm-enabled? llm-enabled? :queue-ctx queue-ctx
       ;; refresh clocks (also seed DocumentViewState)
       :refresh refresh :meta-refresh meta-refresh :card-refresh card-refresh
       ;; topic facts
       :overview overview :kind kind :pdf-root-id pdf-root-id :is-pdf? is-pdf?
       :is-score? is-score?
       :is-video? (boolean (:is-video? overview))
       :video-title (:title overview)
       :video-duration-ms (:video-duration-ms overview)
       :video-last-pos-ms (:video-last-pos-ms overview)
       :video-has-audio? (boolean (:video-has-audio? overview))
       :is-live? (:is-live? overview) :pdf-has-file? (:pdf-has-file? overview)
       :pdf-page-count (:pdf-page-count overview) :root-topic-id root-topic-id
       :extract-status (:status overview) :static-content static-content
       :static-content-topic-id static-content-topic-id
       :initial-page initial-page :initial-layout initial-layout :initial-focus? initial-focus?
       :initial-top-pct initial-top-pct
       :bib-topic-id bib-topic-id :pdf-root? pdf-root? :citation citation :pdf-status pdf-status
       ;; settings + server-watched PDF state
       :card-font-size card-font-size :scan-dpi scan-dpi
       :scanning-pages scanning-pages :ocr-errors ocr-errors})))

(e/defn DocumentLayoutState
  "Content↕card-table split and PDF-pane layout — client view-state owned here.
   Seeded from `resolved`'s :initial-layout / :initial-top-pct. Persists the
   split on drag-commit / double-click; layout persistence is handled by
   DocumentBody via the returned :t-layout / :layout-save.

   Pre:  `resolved` carries :user-id, :initial-layout, :initial-top-pct.
   Post: returns the layout/split prop slice for DocumentBody."
  [resolved]
  (e/client
    (let [{:keys [user-id initial-layout initial-focus? initial-top-pct]} resolved
          ;; Content↕card-table split (global per-user, persisted). Seeded from
          ;; settings; nil → client default. Manual drag + double-click persist
          ;; via !top-pct-save; layout toggle resets live but does not persist.
          !top-pct (atom (or initial-top-pct (default-split-pct)))
          top-pct (e/watch !top-pct)
          !top-pct-save (atom nil)
          top-pct-save (e/watch !top-pct-save)
          [t-top-pct _] (e/Token top-pct-save)
          reset-split! (fn []
                         (let [d (default-split-pct)]
                           (reset! !top-pct d)
                           (reset! !top-pct-save d)))

          ;; PDF split layout (persisted per-doc)
          !layout (atom (or initial-layout "left-right"))
          layout (e/watch !layout)
          !layout-save (atom nil)
          layout-save (e/watch !layout-save)
          [t-layout _] (e/Token layout-save)
          ;; Layout toggle — referenced by both the PdfToolbar (above content)
          ;; and the PdfPane region.
          toggle-layout! (fn []
                           (let [new-l (if (= @!layout "left-right")
                                         "top-bottom" "left-right")
                                 new-top-pct (if (= new-l "top-bottom")
                                               80 (default-split-pct))]
                             (reset! !layout new-l)
                             (reset! !layout-save new-l)
                             (reset! !top-pct new-top-pct)))
          ;; Focus reader (persisted per-doc, same shape as !layout). The PDF
          ;; pane unmounts and the editor column centers. Kept apart from
          ;; !layout so the stored orientation survives the mode untouched.
          !focus? (atom (boolean initial-focus?))
          focus? (e/watch !focus?)
          !focus-save (atom nil)
          focus-save (e/watch !focus-save)
          [t-focus _] (e/Token focus-save)
          set-focus! (fn [v]
                       (let [v (boolean v)]
                         (reset! !focus? v)
                         (reset! !focus-save v)))
          !left-pct (atom 50)
          left-pct (e/watch !left-pct)
          !top-split-pct (atom 50)
          top-split-pct (e/watch !top-split-pct)
          ;; Video column's vertical split: the video+waveform stack's share,
          ;; the transcript below takes the rest. Its own atom rather than
          ;; top-split-pct, whose meaning is the PDF pane's share when stacked.
          ;; Session-only, like !left-pct — only top-pct and layout persist.
          !transcript-pct (atom 50)
          transcript-pct (e/watch !transcript-pct)]
      ;; Persist the split on drag-commit / double-click (global per-user).
      (when t-top-pct
        (let [r (e/server (e/Offload #(settings/save-card-split user-id top-pct-save)))]
          (when (some? r)
            (if (:success r) (case r (t-top-pct)) (t-top-pct (:error r))))))
      {:top-pct top-pct :!top-pct !top-pct :!top-pct-save !top-pct-save :reset-split! reset-split!
       :layout layout :layout-save layout-save :t-layout t-layout :toggle-layout! toggle-layout!
       :top-bottom? (= layout "top-bottom")
       :focus? focus? :focus-save focus-save :t-focus t-focus :set-focus! set-focus!
       :left-pct left-pct :!left-pct !left-pct :top-split-pct top-split-pct :!top-split-pct !top-split-pct
       :transcript-pct transcript-pct :!transcript-pct !transcript-pct})))

(e/defn DocumentViewState
  "Current PDF page, same-doc page-jump, effective content, and biblio toggle —
   the client view-state that cycles through the current page (current-page
   feeds the server page-info / page-text fetches).

   Pre:  `resolved` carries :refresh :meta-refresh :is-pdf? :pdf-root-id :kind
         :initial-page :static-content :queue-ctx; `topic-id` is the viewed topic.
   Post: returns the page/content/biblio prop slice for DocumentBody."
  [resolved topic-id focus?]
  (e/client
    (let [{:keys [refresh meta-refresh is-pdf? is-score? pdf-root-id initial-page
                  pdf-page-count static-content static-content-topic-id queue-ctx]} resolved
          ;; Merged reading (plans/merged-page-reading.md). The whole PDF becomes
          ;; one document with a boundary marker per page. Score topics are out:
          ;; they have no page children.
          merged? (boolean (and is-pdf? (not is-score?) focus?))
          ;; Current PDF page (mutated by PdfPane via callback)
          !current-page (atom initial-page)
          current-page (e/watch !current-page)
          ;; PDF page count — surfaced by PdfViewerComponent via on-total!,
          ;; consumed by PdfToolbar's "of N" + nav-button disabled states.
          ;;
          ;; Seeded from the server count (db/count-pages) rather than 0,
          ;; because focus mode unmounts the viewer and on-total! is then the
          ;; only writer that never runs. Both nav buttons disable on
          ;; `(= total 0)`, so an unseeded atom strands them. The viewer
          ;; overwrites this with its own count as soon as it mounts.
          !total (atom (or pdf-page-count 0))
          total (e/watch !total)
          ;; Last-page persistence. The writer is DocumentBody, not PdfPane:
          ;; focus reader unmounts the pane, and a writer that unmounts stops
          ;; recording the pages the reader turns. This atom and its token live
          ;; in the state that outlives the pane.
          ;;
          ;; It starts nil so a mount writes nothing — only a real page change
          ;; requests a save, which is why `on-page-change!` is the single
          ;; funnel below rather than a watch on current-page.
          !last-page-save (atom nil)
          last-page-save (e/watch !last-page-save)
          [t-last-page _] (e/Token last-page-save)
          ;; The one page-change funnel. PdfPaneFrame binds it for the viewer's
          ;; own scroll and jump events; DocumentToolbars binds it for the
          ;; toolbar's arrows and page field. Both were `(reset! !current-page
          ;; p)` written twice, and neither recorded the page for persistence.
          on-page-change! (fn [p]
                            (reset! !current-page p)
                            (reset! !last-page-save p))
          ;; Same-doc page-jump channel from HierarchySidePanel. Hierarchy resets
          ;; {:topic-id pdf-root-id :page n} on sibling-page click; we derive
          ;; target-page for PdfPane and clear nav-target one tick later so the
          ;; viewer doesn't snap back if the user navigates manually.
          !nav-target (atom nil)
          nav-target (e/watch !nav-target)
          target-page (when (and is-pdf? pdf-root-id nav-target
                              (= (:topic-id nav-target) pdf-root-id))
                        (:page nav-target))
          ;; Live page-info — re-derives as user navigates pages. Score topics
          ;; have no page children: skip the fetch, keep the topic's own id.
          ;; Gate inside the thunk, matching merged-doc and text-result below.
          ;; A `when` outside it makes the binding go from NO VALUE to pending
          ;; the first time the gate opens, and a pending value in this map
          ;; annihilates the whole map — see effective-content.
          page-info (e/server
                      (e/Offload #(when (and is-pdf? (not is-score?))
                                    (get-page-info* refresh meta-refresh pdf-root-id current-page))))
          page-topic-id (if (and is-pdf? (not is-score?))
                          (or (:topic-id page-info) topic-id)
                          topic-id)
          ;; Effective content: merged mode -> the whole document; PDF ->
          ;; fetched page text; non-PDF and score -> static (score pages carry
          ;; no per-page text).
          ;;
          ;; The `(e/server …)` form is the OUTERMOST form of each binding. A
          ;; client-sited conditional wrapped around it would site the value on
          ;; the client and materialize the whole result there — measured at
          ;; 155 KB for a search-results frame, and this result is megabytes.
          ;; The two fields are read separately for the same reason: the map is
          ;; never materialized whole.
          ;; The gate lives INSIDE the thunk, and that placement is the whole
          ;; point. `(when merged? (e/Offload …))` transitions this binding from
          ;; NO VALUE to pending the first time the mode turns on, and a pending
          ;; value here makes TopicPage's whole merged prop map pending — so
          ;; DocumentRoot's entire subtree unmounts and rebuilds.
          ;;
          ;; Measured: a focus toggle rebuilt DocumentColumns, ContentColumn and
          ;; EditorPane twice each, destroyed the Quill instance and re-created
          ;; the PDF viewer. A page flip re-runs the sibling `text-result`
          ;; Offload and costs none of that, because e/Offload's latest-wins
          ;; HOLDS the previous value while recomputing — it only goes pending
          ;; when it has no previous value to hold.
          ;;
          ;; Bound unconditionally, the Offload always has a value: nil for
          ;; every PDF outside the mode, which costs no database work because
          ;; the gate is the thunk's first act.
          merged-doc (e/server
                       (e/Offload #(when merged?
                                     (get-merged-document* refresh pdf-root-id))))
          merged-html (e/server (:html merged-doc))
          merged-page-numbers (e/server (:page-numbers merged-doc))
          ;; Gate inside the thunk, for the reason merged-doc above records: a
          ;; `when` outside it makes this binding go from NO VALUE to pending
          ;; when the mode turns OFF, which unmounts DocumentRoot's subtree.
          ;; Leaving the mode cost one rebuild more than entering it until this
          ;; matched.
          text-result (e/server
                        (e/Offload #(when (and is-pdf? (not is-score?) (not merged?))
                                      (get-page-text* refresh pdf-root-id current-page))))
          ;; `e/Reconcile` is what keeps a mode change from tearing down the
          ;; document view. Its docstring: "collapse :grow/:shrink diffs into
          ;; :change diffs based on position … Use case: inhibit undesired
          ;; duplicate effects."
          ;;
          ;; Without it, this `cond` swapping branches emits a shrink of the old
          ;; branch and a grow of the new one in one diff, and that ripples UP
          ;; the DAG — every parent sees a shrink and a grow. `e/on-unmount`
          ;; fires on a shrink/grow but not on a change, so the ripple reached
          ;; RichTextEditorComponent and called `destroy-editor!`. Measured: a
          ;; focus toggle rebuilt DocumentColumns, ContentColumn and EditorPane
          ;; and destroyed the Quill instance.
          effective-content (e/Reconcile
                              (cond
                                merged? merged-html
                                (and is-pdf? (not is-score?))
                                (when (:success text-result) (:text text-result))
                                :else static-content))
          ;; Topic the shown editor body belongs to. Non-PDF/score read the
          ;; static body → carry its fetched-for id, which lags the live topic-id
          ;; through a switch (stale-detectable). Real PDFs read per-page text
          ;; keyed on the page, not the topic; pin this to page-topic-id so the
          ;; editor's staleness check is a no-op there and PDF page-nav keeps its
          ;; in-place update unchanged.
          effective-content-topic-id (if (and is-pdf? (not is-score?))
                                       page-topic-id
                                       static-content-topic-id)
          ;; The identity of the body in the editor — see doc-context. Merged
          ;; mode holds the whole document, so it is the root; every other mode
          ;; holds one topic's body, so it is that topic.
          ;; Reconciled for the same reason as effective-content above: an
          ;; unreconciled `if` emits shrink+grow on every toggle, and that
          ;; ripples up the DAG until an `e/on-unmount` fires.
          editor-doc-id (e/Reconcile (if merged? pdf-root-id page-topic-id))
          ;; Score editor state — owned here (the stable client ancestor) so the
          ;; waveform strip, the PDF-toolbar rect button, the rect modal, and
          ;; the score card bar all share one pending-card selection.
          !score-region (atom nil)      ;; {:start-ms N :end-ms N} or nil
          !score-pages (atom {})        ;; {page {:width :height :rects [{:x :y :w :h}]}}
          !score-modal-open? (atom false)
          !score-edit (atom nil)        ;; {:group-id N} while editing a pair
          ;; Video editor state — owned here (the stable client ancestor) so the
          ;; player, the waveform strip, the transcript pane and the extract
          ;; button all address the SAME element and the SAME marked range.
          !video-el (atom nil)          ;; HTMLVideoElement — the single clock
          !video-region (atom nil)      ;; {:start-ms N :end-ms N} or nil
          ;; {:topic-id N :ms N|nil :seeks N} — see doc-context for the contract
          !video-playhead (atom {:topic-id nil :ms nil :seeks 0})
          !show-bib (atom false)
          show-bib? (e/watch !show-bib)
          ;; Mobile layout: reading-mode? = phone + learn origin (distraction-free
          ;; incremental-reading view). phone? alone drives PdfToolbar's compact
          ;; layout for every PDF on a phone.
          phone? (e/watch viewport/!phone?)
          reading-mode? (and phone? (= (:origin queue-ctx) :learn))]
      {:!current-page !current-page :current-page current-page
       :on-page-change! on-page-change!
       :last-page-save last-page-save :t-last-page t-last-page
       :!total !total :total total :!nav-target !nav-target :target-page target-page
       :page-info page-info :page-topic-id page-topic-id :effective-content effective-content
       :effective-content-topic-id effective-content-topic-id
       :merged? merged? :merged-page-numbers merged-page-numbers
       :editor-doc-id editor-doc-id
       :!show-bib !show-bib :show-bib? show-bib? :phone? phone? :reading-mode? reading-mode?
       :!score-region !score-region :!score-pages !score-pages
       :!score-modal-open? !score-modal-open? :!score-edit !score-edit
       :!video-el !video-el :!video-region !video-region
       :!video-playhead !video-playhead})))
