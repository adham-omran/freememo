(ns freememo.topic-page
  "Unified viewer for any topic — PDF root, PDF page, basic/wiki/web/epub.

   Resolves topic kind server-side; composes:
     - ToolbarBar (always) — ContentToolbar; hosts DocumentMetaGroup
       (Edit-Bibliography + citation + page-progress + Mark-PDF-Done)
     - HierarchySidePanel (always; owns its own open/collapsed state)
     - PdfPane (PDF only)
     - EditorPane (always)
     - BottomPanel (always) — ContentCardTable
     - PinSidePanel (always; owns its own open/collapsed state)

   Routing collapses /viewer/browse-pdf/<root>/<page> and /viewer/browse-topic/<id>
   into a single /viewer/topic/<id>. Page navigation inside a PDF does not
   update the URL — the URL is an entry hint."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.hierarchy-side-panel :refer [HierarchySidePanel]]
   [freememo.pin-side-panel :refer [PinSidePanel]]
   [freememo.pdf-pane :refer [PdfPane]]
   [freememo.pdf-toolbar :refer [PdfToolbar]]
   [freememo.editor-pane :refer [EditorPane]]
   [freememo.bottom-panel :refer [ToolbarBar BottomPanel]]
   [freememo.document-view :refer [DocumentColumns DocumentToolbars]]
   [freememo.document-body :refer [DocumentBody]]
   [freememo.bibliography-form :as bibform :refer [BibliographyForm]]
   [freememo.keyboard :as keyboard]
   [freememo.navigation :as nav]
   [freememo.viewport :as viewport]
   [freememo.util :as util]
   [clojure.string :as str]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.db :as db])))

;; ---------------------------------------------------------------------------
;; Server query helpers
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
(e/defn TopicPage [user-id enc-key topic-id navigate! llm-enabled? queue-ctx]
  (e/client
    (dom/div
      (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

      (if (nil? topic-id)
        (dom/div
          (dom/props {:style {:padding "32px" :text-align "center"
                              :color "var(--color-text-secondary)"}})
          (dom/text "No topic selected."))

        (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
              meta-refresh (e/server (e/watch (us/get-atom user-id :meta-refresh)))
              sync-mutations (e/server (e/watch (us/get-atom user-id :sync-mutations)))
              card-mutations (e/server (e/watch (us/get-atom user-id :card-mutations)))
              card-refresh (+ refresh sync-mutations card-mutations)

              ;; User settings — fetched once at mount
              card-font-size (e/server (settings/get-card-font-size user-id))
              scan-dpi (e/server (settings/get-scan-dpi user-id))

              ;; Topic resolution
              overview (e/server (get-topic-overview* refresh topic-id))
              kind (:kind overview)
              pdf-root-id (:pdf-root-id overview)
              is-pdf? (some? pdf-root-id)
              is-live? (:is-live? overview)
              pdf-has-file? (:pdf-has-file? overview)
              pdf-page-count (:pdf-page-count overview)
              root-topic-id (:root-topic-id overview)
              ;; Content body fetched separately and gated: PDF panes get
              ;; their text from get-page-text*, so fetching the topic's
              ;; stored content for them was a dead transfer.
              static-content (e/server (when-not is-pdf?
                                         (get-topic-content* refresh topic-id)))
              extract-status (:status overview)
              initial-page (e/server
                             (when is-pdf?
                               (cond
                                 (= kind "page") (:page-number overview)
                                 (= kind "pdf") (or (settings/get-last-page user-id pdf-root-id) 1)
                                 :else 1)))
              initial-layout (e/server
                               (when is-pdf?
                                 (or (settings/get-pdf-layout user-id pdf-root-id) "left-right")))

              ;; Current PDF page (mutated by PdfPane via callback)
              !current-page (atom initial-page)
              current-page (e/watch !current-page)
              ;; PDF page count — surfaced by PdfViewerComponent via on-total!,
              ;; consumed by PdfToolbar's "of N" + nav-button disabled states.
              !total (atom 0)
              total (e/watch !total)

              ;; Same-doc page-jump channel from HierarchySidePanel.
              ;; Hierarchy resets {:topic-id pdf-root-id :page n} on sibling-page
              ;; click; we derive target-page for PdfPane (which calls
              ;; viewer/go-to-page!) and clear nav-target one tick later so the
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

              ;; UI state
              !show-bib (atom false)
              show-bib? (e/watch !show-bib)
              bib-topic-id (or pdf-root-id root-topic-id)
              ;; Document-meta inputs for the toolbar's DocumentMetaGroup
              ;; (formerly the bibliography bar). Computed here so they reach the
              ;; toolbar, which mounts above the content.
              pdf-root? (and is-pdf? (= kind "pdf"))
              citation (e/server (bibform/get-topic-citation* refresh user-id bib-topic-id))
              pdf-status (e/server (when pdf-root?
                                     (get-topic-status* meta-refresh pdf-root-id)))
              ;; Content↕card-table split (global per-user, persisted). Seeded
              ;; from settings; nil → client default. Manual drag + double-click
              ;; persist via !top-pct-save; layout toggle resets live (see
              ;; toggle-layout!) but does not persist.
              initial-top-pct (e/server (settings/get-card-split user-id))
              !top-pct (atom (or initial-top-pct (default-split-pct)))
              top-pct (e/watch !top-pct)
              !top-pct-save (atom nil)
              top-pct-save (e/watch !top-pct-save)
              [t-top-pct _] (e/Token top-pct-save)
              reset-split! (fn []
                             (let [d (default-split-pct)]
                               (reset! !top-pct d)
                               (reset! !top-pct-save d)))

              ;; PDF split layout (owned by TopicPage, persisted per-doc)
              !layout (atom (or initial-layout "left-right"))
              layout (e/watch !layout)
              !layout-save (atom nil)
              layout-save (e/watch !layout-save)
              [t-layout _] (e/Token layout-save)
              ;; Layout toggle — hoisted to outer scope so both the PdfToolbar
              ;; (above the content) and the PdfPane region can reference it.
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
              top-split-pct (e/watch !top-split-pct)
              top-bottom? (= layout "top-bottom")

              ;; PDF state watched at outer scope
              scanning-pages (e/server (e/watch (us/get-atom user-id :scanning-pages)))
              ocr-errors (e/server (e/watch (us/get-atom user-id :ocr-errors)))

              ;; Mobile layout (spec §0). reading-mode? = phone + learn origin:
              ;; a distraction-free incremental-reading view that hides the
              ;; hierarchy, pins, and card table and reduces the command bar to
              ;; the IR verbs (Extract + Add-Card). phone? alone drives the
              ;; PdfToolbar's compact C3 layout for every PDF on a phone.
              phone? (e/watch viewport/!phone?)
              reading-mode? (and phone? (= (:origin queue-ctx) :learn))]

          ;; Persist the split on drag-commit / double-click (global per-user).
          (when t-top-pct
            (let [r (e/server (e/Offload #(settings/save-card-split user-id top-pct-save)))]
              (when (some? r)
                (if (:success r) (case r (t-top-pct)) (t-top-pct (:error r))))))

          (DocumentBody
            {:user-id user-id :enc-key enc-key :page-topic-id page-topic-id :kind kind :pdf-root-id pdf-root-id :root-topic-id root-topic-id
             :is-pdf? is-pdf? :current-page current-page :effective-content effective-content :llm-enabled? llm-enabled? :extract-status extract-status :navigate! navigate!
             :queue-ctx queue-ctx :citation citation :page-info page-info :pdf-root? pdf-root? :pdf-status pdf-status :reading-mode? reading-mode?
             :card-refresh card-refresh :!show-bib !show-bib :show-bib? show-bib? :total total :layout layout :is-live? is-live?
             :scan-dpi scan-dpi :scanning-pages scanning-pages :ocr-errors ocr-errors :phone? phone? :!current-page !current-page :toggle-layout! toggle-layout!
             :bib-topic-id bib-topic-id :initial-page initial-page :target-page target-page :pdf-has-file? pdf-has-file? :pdf-page-count pdf-page-count :top-pct top-pct
             :top-bottom? top-bottom? :top-split-pct top-split-pct :left-pct left-pct :!total !total :!nav-target !nav-target :!top-split-pct !top-split-pct
             :!left-pct !left-pct :card-font-size card-font-size :t-layout t-layout :layout-save layout-save
             :!top-pct !top-pct :!top-pct-save !top-pct-save :reset-split! reset-split!}))))))
