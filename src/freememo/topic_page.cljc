(ns freememo.topic-page
  "Unified viewer for any topic — PDF root, PDF page, basic/wiki/web/epub.

   Resolves topic kind server-side; composes:
     - TitleBar (PDF only) — doc title + rename + page-stats
     - HierarchySidePanel (always; owns its own open/collapsed state)
     - PdfPane (PDF only)
     - EditorPane (always)
     - BottomPanel (always) — ContentToolbar + ContentCardTable
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
   [freememo.editor-pane :refer [EditorPane]]
   [freememo.bottom-panel :refer [BottomPanel]]
   [freememo.bibliography-form :as bibform :refer [BibliographyForm]]
   [freememo.keyboard :as keyboard]
   [freememo.navigation :as nav]
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

(defn get-topic-title*
  "Latest title for a topic id (re-fetches on :refresh / :tree-mutations)."
  [_refresh topic-id]
  #?(:clj (when topic-id (:topics/title (db/get-topic topic-id)))
     :cljs nil))

(defn get-topic-status*
  "Latest status for a topic id. Re-fetches on :meta-refresh."
  [_meta-refresh topic-id]
  #?(:clj (when topic-id (or (:topics/status (db/get-topic topic-id)) "active"))
     :cljs nil))

(defn rename-and-bump!
  "Atomic rename + bump both :refresh (for doc lists) and :tree-mutations
   (for side panel + library tree)."
  [user-id id new-title]
  #?(:clj (do (db/rename-topic! id new-title)
            (swap! (us/get-atom user-id :refresh) inc)
            (swap! (us/get-atom user-id :tree-mutations) inc)
            :ok)
     :cljs nil))

(defn default-split-pct []
  #?(:cljs (if (< (.-innerHeight js/window) 900) 50 75)
     :clj 75))

;; ---------------------------------------------------------------------------
;; TitleBar — PDF-only header with doc title + rename + page-stats
;; (The hierarchy hamburger lives inside HierarchySidePanel itself.)
;; ---------------------------------------------------------------------------

(e/defn TitleBar [user-id pdf-root-id refresh page-info !show-bib citation pdf-root? pdf-status]
  (e/client
    (let [current-title (e/server (get-topic-title* refresh pdf-root-id))
          !editing-title (atom false)
          editing-title (e/watch !editing-title)
          done? (= pdf-status "done")]
      (dom/div
        (dom/props {:class "title-bar"
                    :style {:display "flex" :align-items "baseline" :gap "14px"
                            :padding "10px 16px" :flex-shrink "0"
                            :background "var(--color-bg-subtle)"
                            :border-bottom "1px solid var(--color-border)"}})

        ;; Bibliography — matches the app's standard btn-secondary used in
        ;; the bottom-panel toolbar, so the bar's affordance reads as part
        ;; of the same system rather than a one-off custom pill.
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :style {:flex-shrink "0"}
                      :data-tooltip "Edit bibliography"})
          (dom/text "Bibliography")
          (dom/On "click" (fn [_] (reset! !show-bib true)) nil))

        ;; PDF Done toggle — visible only at the PDF root (not on child pages).
        ;; Flips topics.status between "active" and "done"; bumps :meta-refresh
        ;; so the label updates without a page reload. Pages keep their own
        ;; status independently (no cascade).
        (when pdf-root?
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:flex-shrink "0"}
                        :data-tooltip (if done?
                                        "Restore this PDF to the active queue"
                                        "Mark this PDF as completed")})
            (dom/text (if done? "Restore PDF" "Mark PDF Done"))
            (let [event (dom/On "click" (fn [_] (str (random-uuid))) nil)
                  [t ?error] (e/Token event)]
              (when t
                (case (e/server (if done?
                                  (db/restore-topic! pdf-root-id)
                                  (db/done-topic! pdf-root-id)))
                  (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                    (t)))))))

        ;; Citation — italic caption framed by separator dots
        (when citation
          (dom/span
            (dom/props {:style {:font-size "12px"
                                :font-style "italic"
                                :color "var(--color-text-hint)"
                                :flex-shrink "0"
                                :max-width "32%"
                                :overflow "hidden"
                                :text-overflow "ellipsis"
                                :white-space "nowrap"}
                        :data-tooltip citation})
            (dom/text citation)))

        ;; Title — editable in place
        (if editing-title
          (e/for-by identity [_k [pdf-root-id]]
            (dom/input
              (dom/props {:type "text" :placeholder "Title" :maxlength "500"
                          :style {:flex "1" :padding "4px 8px" :font-size "13px"
                                  :color "var(--color-text-primary)"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "3px"
                                  :background "var(--color-bg-card)"}})
              (set! (.-value dom/node) (or current-title ""))
              (let [n dom/node]
                (js/setTimeout
                  (fn []
                    (.focus n)
                    (when (pos? (count (.-value n))) (.select n)))
                  0))
              (dom/On "keydown"
                (fn [ev]
                  (when (= (.-key ev) "Escape")
                    (.preventDefault ev)
                    (set! (.-value (.-target ev)) (or current-title ""))
                    (.blur (.-target ev))
                    (reset! !editing-title false)))
                nil)
              (let [event (dom/On "change" #(-> % .-target .-value) nil)
                    [t _] (e/Token event)]
                (when t
                  (e/on-unmount #(reset! !editing-title false))
                  (let [trimmed (str/trim event)]
                    (if (str/blank? trimmed)
                      (t)
                      (let [ok (e/server (e/Offload #(rename-and-bump! user-id pdf-root-id trimmed)))]
                        (when (some? ok)
                          (case ok (t))))))))))
          ;; Outer span owns the tooltip + click; inner span clips with
          ;; ellipsis. Splitting prevents overflow:hidden from clipping the
          ;; tooltip ::after popup (it would inherit the outer's clip).
          (dom/span
            (dom/props {:style {:flex "1" :min-width "0" :cursor "pointer"
                                :display "block"}
                        :data-tooltip "Click to edit"})
            (dom/On "click" (fn [_] (reset! !editing-title true)) nil)
            (dom/span
              (dom/props {:style {:display "block"
                                  :font-size "15px" :font-weight "700"
                                  :color "var(--color-text-primary)"
                                  :overflow "hidden"
                                  :text-overflow "ellipsis"
                                  :white-space "nowrap"}})
              (dom/text (or current-title "")))))

        ;; Page-stats badge (X / Y) — folio-style chip with tabular numerals
        (when (and page-info (pos? (:total page-info)))
          (dom/span
            (dom/props {:class "tooltip-right"
                        :style {:color "var(--color-text-primary)"
                                :font-size "11px"
                                :font-family "ui-monospace, 'SF Mono', Menlo, Consolas, monospace"
                                :font-variant-numeric "tabular-nums"
                                :letter-spacing "0.04em"
                                :white-space "nowrap" :flex-shrink "0"
                                :padding "3px 8px"
                                :background "var(--color-bg-card)"
                                :border "1px solid var(--color-border)"
                                :border-radius "10px"}
                        :data-tooltip (:remaining-tooltip page-info)})
            (dom/text (:done page-info) "/" (:total page-info))))))))

;; ---------------------------------------------------------------------------
;; TopicPage shell
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
              enable-ai? (e/server (settings/get-enable-ai-scan-button user-id))
              enable-pdfbox? (e/server (settings/get-enable-pdfbox-button user-id))
              enable-pdfjs? (e/server (settings/get-enable-pdfjs-button user-id))

              ;; Topic resolution
              overview (e/server (get-topic-overview* refresh topic-id))
              kind (:kind overview)
              pdf-root-id (:pdf-root-id overview)
              is-pdf? (some? pdf-root-id)
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
              !top-pct (atom (default-split-pct))
              top-pct (e/watch !top-pct)

              ;; PDF split layout (owned by TopicPage, persisted per-doc)
              !layout (atom (or initial-layout "left-right"))
              layout (e/watch !layout)
              !layout-save (atom nil)
              layout-save (e/watch !layout-save)
              [t-layout _] (e/Token layout-save)
              !left-pct (atom 50)
              left-pct (e/watch !left-pct)
              !top-split-pct (atom 50)
              top-split-pct (e/watch !top-split-pct)
              top-bottom? (= layout "top-bottom")

              ;; PDF state watched at outer scope
              scanning-pages (e/server (e/watch (us/get-atom user-id :scanning-pages)))
              ocr-errors (e/server (e/watch (us/get-atom user-id :ocr-errors)))]

          ;; Persist layout on toggle
          (when t-layout
            (if (and is-pdf? pdf-root-id)
              (let [r (e/server (e/Offload #(settings/save-pdf-layout user-id pdf-root-id layout-save)))]
                (when (some? r)
                  (if (:success r) (case r (t-layout)) (t-layout (:error r)))))
              (t-layout)))

          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                :min-height "0" :overflow "hidden"}})

            ;; Bibliography header — single line in both modes:
            ;; PDF:     [Bibliography] [Mark PDF Done?] citation? title [stats]
            ;; non-PDF: [Bibliography] citation
            (let [citation (e/server (bibform/get-topic-citation* refresh user-id bib-topic-id))
                  pdf-root? (and is-pdf? (= kind "pdf"))
                  pdf-status (e/server (when pdf-root?
                                         (get-topic-status* meta-refresh pdf-root-id)))]
              (if is-pdf?
                (TitleBar user-id pdf-root-id refresh page-info !show-bib citation pdf-root? pdf-status)
                (dom/div
                  (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                                      :padding "6px 12px" :flex-shrink "0"
                                      :border-bottom "1px solid var(--color-border)"
                                      :background "var(--color-bg-subtle)"}})
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:padding "2px 8px" :font-size "13px" :line-height "1"
                                        :flex-shrink "0"}
                                :data-tooltip "Edit bibliography"})
                    (dom/text "Bibliography")
                    (dom/On "click" (fn [_] (reset! !show-bib true)) nil))
                  (dom/span
                    (dom/props {:style {:flex "1" :min-width "0"
                                        :font-size "12px"
                                        :color "var(--color-text-secondary)"
                                        :overflow "hidden"
                                        :text-overflow "ellipsis"
                                        :white-space "nowrap"}
                                :data-tooltip (or citation "")})
                    (dom/text (or citation ""))))))

            ;; Auto-open biblio modal once on first mount after a fresh
            ;; import. claim-pending-biblio-show?* returns true (and clears the
            ;; mark) exactly once per (user-id, topic-id). The keyed e/for-by
            ;; frame mounts only when auto-show? is true, runs reset! once on
            ;; mount, and never remounts thereafter (key is the stable topic-id).
            (let [auto-show? (e/server
                               (e/Offload
                                 #(bibform/claim-pending-biblio-show?* user-id bib-topic-id)))]
              (e/for-by identity [_k (when (true? auto-show?) [bib-topic-id])]
                (let [opened (reset! !show-bib true)]
                  (when opened nil))))

            ;; Bibliography modal — overlays everything when shown
            (when show-bib?
              (BibliographyForm !show-bib user-id bib-topic-id))

            ;; Body row: side panel | content column
            (dom/div
              (dom/props {:style {:flex "1" :display "flex" :flex-direction "row"
                                  :min-height "0" :overflow "hidden"}})

              ;; Clear nav-target after deriving target-page, so the viewer can
              ;; navigate away manually without being snapped back.
              (when target-page (reset! !nav-target nil))

              ;; LEFT: hierarchy side panel (manages its own open/collapsed state).
              ;; Pass !nav-target only when this TopicPage hosts the PDF viewer —
              ;; only in that mode is the target-page derivation (which gates on
              ;; is-pdf?) able to consume an in-document page jump. Outside PDF
              ;; mode the side panel must fall through to (navigate! :viewer …).
              (HierarchySidePanel user-id page-topic-id root-topic-id navigate!
                (when is-pdf? !nav-target))

              ;; RIGHT: content column
              (dom/div
                (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                    :min-width "0" :min-height "0" :overflow "hidden"}})

                ;; TOP REGION:
                ;;   PDF mode left-right: [PdfPane | drag | EditorPane]
                ;;   PDF mode top-bottom: [PdfPane / drag / EditorPane]
                ;;   Non-PDF: just EditorPane
                (dom/div
                  (dom/props {:style {:height (str top-pct "%") :display "flex"
                                      :flex-direction (if (and is-pdf? top-bottom?)
                                                        "column"
                                                        "row")
                                      :min-height "0" :overflow "hidden"}})

                  (when is-pdf?
                    (let [pdf-style (if top-bottom?
                                      {:height (str top-split-pct "%")
                                       :min-height "0" :overflow "hidden"}
                                      {:width (str left-pct "%")
                                       :min-width "0" :overflow "hidden"})
                          toggle-layout! (fn []
                                           (let [new-l (if (= @!layout "left-right")
                                                         "top-bottom"
                                                         "left-right")
                                                 new-top-pct (if (= new-l "top-bottom")
                                                               80
                                                               (default-split-pct))]
                                             (reset! !layout new-l)
                                             (reset! !layout-save new-l)
                                             (reset! !top-pct new-top-pct)))]
                      (dom/div
                        (dom/props {:style pdf-style})
                        (reset! !current-page
                          (PdfPane {:user-id user-id
                                    :pdf-root-id pdf-root-id
                                    :initial-page initial-page
                                    :target-page target-page
                                    :layout layout
                                    :on-page-change! (fn [p] (reset! !current-page p))
                                    :on-layout-toggle! toggle-layout!})))

                      ;; Drag handle BETWEEN PdfPane and EditorPane
                      (dom/div
                        (dom/props {:class (if top-bottom? "split-divider-v" "split-divider-h")
                                    :title "Drag to resize panels"})
                        (dom/On "pointerdown"
                          (fn [e]
                            (if top-bottom?
                              (util/start-drag! e :y !top-split-pct)
                              (util/start-drag! e :x !left-pct)))
                          nil))))

                  (EditorPane
                    {:user-id user-id
                     :enc-key enc-key
                     :topic-id page-topic-id
                     :audio-topic-id (when (= kind "audio") page-topic-id)
                     :is-pdf-page? is-pdf?
                     :root-topic-id (or pdf-root-id root-topic-id)
                     :page-number (when is-pdf? current-page)
                     :scan-dpi scan-dpi
                     :llm-enabled? llm-enabled?
                     :enable-ai? enable-ai?
                     :enable-pdfbox? enable-pdfbox?
                     :enable-pdfjs? enable-pdfjs?
                     :static-content effective-content
                     :scanning-pages scanning-pages
                     :ocr-errors ocr-errors
                     :on-imported-navigate!
                     (fn [tid]
                       (when navigate!
                         (navigate! :viewer (nav/nav-topic tid nil))))}))

                ;; Vertical drag handle
                (dom/div
                  (dom/props {:class "split-divider-v" :title "Drag to resize panels"})
                  (dom/On "pointerdown" (fn [e] (util/start-drag! e :y !top-pct)) nil))

                ;; BOTTOM: shared toolbar + card table
                (BottomPanel
                  {:user-id user-id
                   :enc-key enc-key
                   :topic-id page-topic-id
                   :audio? (= kind "audio")
                   :root-topic-id (or pdf-root-id root-topic-id)
                   :page-number (when is-pdf? current-page)
                   :static-content effective-content
                   :context-mode (if is-pdf? :page :extract)
                   :context-tooltip (if is-pdf?
                                      "Include context for better cards. With a selection: current page + N previous pages. Without: N previous pages."
                                      "Include context for better cards. With a selection: extract text. Without: original page text.")
                   :llm-enabled? llm-enabled?
                   :extract-status (when-not is-pdf? extract-status)
                   :navigate! navigate!
                   :origin (:origin queue-ctx)
                   :on-done! (:on-done! queue-ctx)
                   :card-font-size card-font-size}
                  card-refresh))

              ;; PIN SIDE PANEL: collapsible right-side pins for current topic
              (PinSidePanel page-topic-id root-topic-id user-id))))))))
