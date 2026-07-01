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
               pdf-root-id (cond
                             (= kind "pdf") topic-id
                             (and (= kind "page") (= root-kind "pdf")) root-id
                             :else nil)
               pdf-root-topic (cond (= kind "pdf") topic
                                    (and (= kind "page") (= root-kind "pdf")) (or root topic)
                                    :else nil)]
           ;; Scalars only — TopicPage destructures this map client-side, so
           ;; every key crosses the wire on each :refresh tick. Content bodies
           ;; are fetched separately: get-topic-content* (gated off for PDFs)
           ;; and content-toolbar-generate/get-parent-content* (server-side
           ;; at the point of use).
           {:kind kind
            :parent-id parent-id
            :page-number page-number
            :pdf-root-id pdf-root-id
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
  [user-id enc-key topic-id navigate! llm-enabled? queue-ctx]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          meta-refresh (e/server (e/watch (us/get-atom user-id :meta-refresh)))
          sync-mutations (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          card-mutations (e/server (e/watch (us/get-atom user-id :card-mutations)))
          card-refresh (+ refresh sync-mutations card-mutations)

          card-font-size (e/server (settings/get-card-font-size user-id))
          scan-dpi (e/server (settings/get-scan-dpi user-id))

          overview (e/server (get-topic-overview* refresh topic-id))
          kind (:kind overview)
          pdf-root-id (:pdf-root-id overview)
          is-pdf? (some? pdf-root-id)
          root-topic-id (:root-topic-id overview)
          ;; Content body fetched separately and gated: PDF panes get their
          ;; text from get-page-text*, so fetching the topic's stored content
          ;; for them was a dead transfer.
          static-content (e/server (when-not is-pdf?
                                     (get-topic-content* refresh topic-id)))
          initial-page (e/server
                         (when is-pdf?
                           (cond
                             (= kind "page") (:page-number overview)
                             (= kind "pdf") (or (settings/get-last-page user-id pdf-root-id) 1)
                             :else 1)))
          initial-layout (e/server
                           (when is-pdf?
                             (or (settings/get-pdf-layout user-id pdf-root-id) "left-right")))
          initial-top-pct (e/server (settings/get-card-split user-id))

          bib-topic-id (or pdf-root-id root-topic-id)
          pdf-root? (and is-pdf? (= kind "pdf"))
          citation (e/server (bibform/get-topic-citation* refresh user-id bib-topic-id))
          pdf-status (e/server (when pdf-root?
                                 (get-topic-status* meta-refresh pdf-root-id)))
          scanning-pages (e/server (e/watch (us/get-atom user-id :scanning-pages)))
          ocr-errors (e/server (e/watch (us/get-atom user-id :ocr-errors)))]
      {;; caller passthrough
       :user-id user-id :enc-key enc-key :navigate! navigate!
       :llm-enabled? llm-enabled? :queue-ctx queue-ctx
       ;; refresh clocks (also seed DocumentViewState)
       :refresh refresh :meta-refresh meta-refresh :card-refresh card-refresh
       ;; topic facts
       :overview overview :kind kind :pdf-root-id pdf-root-id :is-pdf? is-pdf?
       :is-live? (:is-live? overview) :pdf-has-file? (:pdf-has-file? overview)
       :pdf-page-count (:pdf-page-count overview) :root-topic-id root-topic-id
       :extract-status (:status overview) :static-content static-content
       :initial-page initial-page :initial-layout initial-layout :initial-top-pct initial-top-pct
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
    (let [{:keys [user-id initial-layout initial-top-pct]} resolved
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
          !left-pct (atom 50)
          left-pct (e/watch !left-pct)
          !top-split-pct (atom 50)
          top-split-pct (e/watch !top-split-pct)]
      ;; Persist the split on drag-commit / double-click (global per-user).
      (when t-top-pct
        (let [r (e/server (e/Offload #(settings/save-card-split user-id top-pct-save)))]
          (when (some? r)
            (if (:success r) (case r (t-top-pct)) (t-top-pct (:error r))))))
      {:top-pct top-pct :!top-pct !top-pct :!top-pct-save !top-pct-save :reset-split! reset-split!
       :layout layout :layout-save layout-save :t-layout t-layout :toggle-layout! toggle-layout!
       :top-bottom? (= layout "top-bottom")
       :left-pct left-pct :!left-pct !left-pct :top-split-pct top-split-pct :!top-split-pct !top-split-pct})))

(e/defn DocumentViewState
  "Current PDF page, same-doc page-jump, effective content, and biblio toggle —
   the client view-state that cycles through the current page (current-page
   feeds the server page-info / page-text fetches).

   Pre:  `resolved` carries :refresh :meta-refresh :is-pdf? :pdf-root-id :kind
         :initial-page :static-content :queue-ctx; `topic-id` is the viewed topic.
   Post: returns the page/content/biblio prop slice for DocumentBody."
  [resolved topic-id]
  (e/client
    (let [{:keys [refresh meta-refresh is-pdf? pdf-root-id initial-page
                  static-content queue-ctx]} resolved
          ;; Current PDF page (mutated by PdfPane via callback)
          !current-page (atom initial-page)
          current-page (e/watch !current-page)
          ;; PDF page count — surfaced by PdfViewerComponent via on-total!,
          ;; consumed by PdfToolbar's "of N" + nav-button disabled states.
          !total (atom 0)
          total (e/watch !total)
          ;; Same-doc page-jump channel from HierarchySidePanel. Hierarchy resets
          ;; {:topic-id pdf-root-id :page n} on sibling-page click; we derive
          ;; target-page for PdfPane and clear nav-target one tick later so the
          ;; viewer doesn't snap back if the user navigates manually.
          !nav-target (atom nil)
          nav-target (e/watch !nav-target)
          target-page (when (and is-pdf? pdf-root-id nav-target
                              (= (:topic-id nav-target) pdf-root-id))
                        (:page nav-target))
          ;; Live page-info — re-derives as user navigates pages
          page-info (e/server
                      (when is-pdf?
                        (get-page-info* refresh meta-refresh pdf-root-id current-page)))
          page-topic-id (if is-pdf?
                          (or (:topic-id page-info) topic-id)
                          topic-id)
          ;; Effective content: PDF -> fetched page text; non-PDF -> static
          text-result (e/server
                        (when is-pdf?
                          (e/Offload #(get-page-text* refresh pdf-root-id current-page))))
          effective-content (if is-pdf?
                              (when (:success text-result) (:text text-result))
                              static-content)
          !show-bib (atom false)
          show-bib? (e/watch !show-bib)
          ;; Mobile layout: reading-mode? = phone + learn origin (distraction-free
          ;; incremental-reading view). phone? alone drives PdfToolbar's compact
          ;; layout for every PDF on a phone.
          phone? (e/watch viewport/!phone?)
          reading-mode? (and phone? (= (:origin queue-ctx) :learn))]
      {:!current-page !current-page :current-page current-page
       :!total !total :total total :!nav-target !nav-target :target-page target-page
       :page-info page-info :page-topic-id page-topic-id :effective-content effective-content
       :!show-bib !show-bib :show-bib? show-bib? :phone? phone? :reading-mode? reading-mode?})))
