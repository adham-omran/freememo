(ns freememo.pdf-toolbar
  "Second command bar, rendered under the main ContentToolbar and shown only
   when a PDF item is in view. Hosts every PDF-scoped control that previously
   lived split across PdfViewerComponent (page-nav, zoom, layout-toggle) and
   EditorPane's page-header (mark-page-done + Scan / Compare-OCR / Copy-text /
   Copy-all + the Live-Document add-photos affordance).

   Styling: every control is a ghost .btn-secondary inside a .toolbar-group, so
   the main toolbar's transparent-at-rest / hover-fill rule applies — the whole
   row matches the main ContentToolbar (S2).

   Collapse (C1): the action buttons (mark-page-done, Scan, Compare-OCR, Copy)
   carry .icon-label and collapse to icon-only under the same overflow mechanism
   as the main toolbar (install-overflow-detector!; tier-1 hides .icon-label).
   Page-nav and zoom glyphs carry no .icon-label, so they never collapse."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
   [freememo.copy-text :as copy]
   [freememo.pdf-action-dropdowns :refer [ScanDropdown CopyDropdown]]
   [freememo.pdf-viewer :as viewer]
   [freememo.pdf-viewer-component :refer [LiveDocAddPhotos]]
   [freememo.toolbar-overflow :refer [install-overflow-detector!]]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

;; ---------------------------------------------------------------------------
;; Page-done status read. Wraps the DB call with a _refresh-first arg so the
;; reactive call site re-queries when :meta-refresh bumps — the done button
;; reflects a toggle immediately (a native checkbox got this free from the
;; browser; a button must be driven by the reactive value).
;; ---------------------------------------------------------------------------

(defn page-done?*
  [_refresh pdf-root-id page-number]
  #?(:clj (boolean (when (and pdf-root-id page-number)
                     (db/get-page-done-status pdf-root-id page-number)))
     :cljs nil))

(e/defn PdfToolbar
  "props: {:user-id :enc-key :pdf-root-id :page-number :total :layout :is-live?
           :scan-dpi :llm-enabled? :scanning-pages :ocr-errors
           :on-page-change! :on-layout-toggle!}
   `pdf-root-id` is the document root — it serves as document-id (LiveDoc),
   done/scan target, and copy-extract scope (all the same value upstream)."
  [{:keys [user-id enc-key pdf-root-id page-number total layout is-live?
           scan-dpi llm-enabled? scanning-pages ocr-errors
           on-page-change! on-layout-toggle!]}]
  (e/client
    (let [!input-val   (atom (str page-number))
          input-val    (e/watch !input-val)
          !inp-focused (atom false)
          inp-focused  (e/watch !inp-focused)
          !tier        (atom 0)
          _tier        (e/watch !tier)
          !overflow-open (atom false)
          ;; Reactive per-PDF extraction style (drives Copy text / Copy all).
          settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          extract-style (e/server (copy/get-extract-style* settings-refresh user-id pdf-root-id))
          ;; Navigate: drive PDF.js and notify TopicPage's current-page. Scroll-
          ;; driven changes flow back independently via the viewer's own
          ;; page-change event → PdfPane → on-page-change! (idempotent overlap).
          nav! (fn [p]
                 (viewer/go-to-page! p)
                 (when on-page-change! (on-page-change! p)))]

      ;; Sync the page input to the live page when the user isn't typing.
      (when (and (not inp-focused) (not= input-val (str page-number)))
        (reset! !input-val (str page-number)))

      (dom/div
        ;; pdf-toolbar-container: ranks this bar's stacking context below the
        ;; main ContentToolbar (which hosts the History modal backdrop) and
        ;; above EditorToolbar, so dropped tooltips and the modal backdrop layer
        ;; correctly across the three stacked bars. See index.css.
        (dom/props {:class "toolbar-container pdf-toolbar-container"})
        (let [container-node dom/node]
          (dom/div
            (dom/props {:class "toolbar pdf-toolbar-bar"})
            (let [toolbar-node dom/node
                  cleanup (install-overflow-detector!
                            container-node toolbar-node !tier !overflow-open)]
              (e/on-unmount cleanup)

              ;; ── Page navigation (no collapse) ───────────────────────────
              (dom/div
                (dom/props {:class "toolbar-group"
                            :style {:display "flex" :align-items "center" :gap "4px"}})
                (let [prev-disabled? (or (= page-number 1) (= total 0))]
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :title "Previous Page" :disabled prev-disabled?})
                    (dom/text "◀")
                    (dom/On "click"
                      (fn [_] (when (and (> page-number 1) (> total 0))
                                (nav! (dec page-number)))) nil)))
                (dom/input
                  (dom/props {:type "text" :value input-val
                              :style {:width "40px" :text-align "center" :padding "4px"
                                      :border "1px solid var(--color-border)"
                                      :border-radius "3px" :font-size "14px"}})
                  (dom/On "focus" (fn [_] (reset! !inp-focused true)) nil)
                  (dom/On "blur"
                    (fn [e]
                      (let [raw (-> e .-target .-value)]
                        (reset! !inp-focused false)
                        (let [n (js/parseInt raw)]
                          (when (and (not (js/isNaN n)) (>= n 1) (<= n total))
                            (nav! (max 1 (min total n)))))))
                    nil)
                  (dom/On "keydown"
                    (fn [e] (when (= "Enter" (.-key e)) (.blur (.-target e)))) nil)
                  (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                    (when (some? v) (reset! !input-val v))))
                (dom/span
                  (dom/props {:style {:color "var(--color-text-primary)" :padding "0 4px"}})
                  (dom/text "of " total))
                (let [next-disabled? (or (>= page-number total) (= total 0))]
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :title "Next Page" :disabled next-disabled?})
                    (dom/text "▶")
                    (dom/On "click"
                      (fn [_] (when (and (< page-number total) (> total 0))
                                (nav! (inc page-number)))) nil))))

              ;; ── Zoom + layout toggle (no collapse) ──────────────────────
              (dom/div
                (dom/props {:class "toolbar-group"
                            :style {:display "flex" :align-items "center" :gap "4px"
                                    :padding-left "12px"
                                    :border-left "1px solid var(--color-border)"}})
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom Out"})
                  (dom/text "−")
                  (e/for [_ (dom/On-all "click")] (viewer/zoom! 0.9)))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom In"})
                  (dom/text "+")
                  (e/for [_ (dom/On-all "click")] (viewer/zoom! 1.1)))
                (dom/select
                  (dom/props {:style {:padding "6px 8px" :cursor "pointer"
                                      :background "transparent"
                                      :color "var(--color-text-label)"
                                      :border "1px solid transparent"
                                      :border-radius "3px"}})
                  (dom/option (dom/props {:value "page-width"}) (dom/text "Page Width"))
                  (dom/option (dom/props {:value "page-fit"}) (dom/text "Page Fit"))
                  (dom/option (dom/props {:value "0.5"}) (dom/text "50%"))
                  (dom/option (dom/props {:value "0.75"}) (dom/text "75%"))
                  (dom/option (dom/props {:value "1.0"}) (dom/text "100%"))
                  (dom/option (dom/props {:value "1.25"}) (dom/text "125%"))
                  (dom/option (dom/props {:value "1.5"}) (dom/text "150%"))
                  (dom/option (dom/props {:value "2.0"}) (dom/text "200%"))
                  (e/for [[t e] (dom/On-all "change")]
                    (when e
                      (let [v (-> e .-target .-value)]
                        (case v
                          "page-width" (viewer/set-zoom-fit!)
                          "page-fit" (viewer/set-zoom-page-fit!)
                          (let [scale (js/parseFloat v)]
                            (when-not (js/isNaN scale) (viewer/set-zoom! scale))))))
                    (t)))
                (when on-layout-toggle!
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :style {:font-size "14px"}
                                :data-tooltip (if (= layout "top-bottom")
                                                "Switch to side-by-side layout"
                                                "Switch to stacked layout")})
                    (dom/text (if (= layout "top-bottom") "⇅" "⇄"))
                    (dom/On "click" (fn [_] (on-layout-toggle!)) nil))))

              ;; ── Mark-page-done button (no collapse) — matches Mark PDF
              ;;    Done (bibliography_toolbar): plain ghost secondary button,
              ;;    check/rotate icon, Done/Restore label. Keyboard ref drives
              ;;    the Done shortcut via .click. ────────────────────────────
              (dom/div
                (dom/props {:class "toolbar-group"
                            :style {:padding-left "12px"
                                    :border-left "1px solid var(--color-border)"}})
                (let [refresh    (e/server (e/watch (us/get-atom user-id :meta-refresh)))
                      page-done? (e/server (page-done?* refresh pdf-root-id page-number))]
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :aria-label (if page-done?
                                              (str "Restore page " page-number)
                                              (str "Mark page " page-number " as done"))
                                :data-tooltip "Mark this page as completed to track your extraction progress"})
                    (icons/Icon (if page-done? :rotate-ccw :check) :size 16)
                    (dom/span (dom/props {:class "icon-label"})
                      (dom/text (if page-done?
                                  (str "Restore page " page-number)
                                  (str "Mark page " page-number " as done"))))
                    (reset! keyboard/!done-btn-ref dom/node)
                    (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                    (let [click-event (dom/On "click"
                                        (fn [_] {:id (str (random-uuid)) :page page-number})
                                        nil)
                          [t _] (e/Token click-event)]
                      (when t
                        (case (e/server (db/toggle-page-done! pdf-root-id (:page click-event)))
                          (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                            (t))))))))

              ;; ── Scan (+Compare OCR) / Copy (+Copy all) dropdowns (C1/C2) ─
              ;;    Wrapped in .toolbar-group so the ghost-button rule reaches
              ;;    the dropdown triggers — matches the main toolbar.
              (dom/div
                (dom/props {:class "toolbar-group"})
                (when llm-enabled?
                  (ScanDropdown {:user-id user-id :enc-key enc-key
                                 :pdf-root-id pdf-root-id :page-number page-number
                                 :scan-dpi scan-dpi
                                 :scanning? (contains? scanning-pages [pdf-root-id page-number])}))
                (CopyDropdown {:user-id user-id :pdf-root-id pdf-root-id
                               :page-number page-number :extract-style extract-style})

                ;; Live Document add-photos (only for live docs)
                (when is-live?
                  (LiveDocAddPhotos {:document-id pdf-root-id :compact? true})))

              ;; ── OCR error display — auto-dismiss after 3s ───────────────
              (when-let [ocr-err (get ocr-errors [pdf-root-id page-number])]
                (let [!show (atom true)
                      show (e/watch !show)]
                  (dom/div
                    (dom/props {:style {:padding "6px 10px"
                                        :background "var(--color-danger-bg)"
                                        :border "1px solid var(--color-danger-light)"
                                        :border-radius "var(--radius-sm)"
                                        :font-size "13px"
                                        :color "var(--color-danger-dark)"
                                        :opacity (if show "1" "0")
                                        :transition "opacity 0.5s ease-out"}})
                    (dom/text ocr-err)
                    (e/client
                      (js/setTimeout (fn [] (reset! !show false)) 3000))))))))))))
