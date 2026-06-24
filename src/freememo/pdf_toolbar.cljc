(ns freememo.pdf-toolbar
  "Second command bar, rendered under the main ContentToolbar and shown only
   when a PDF item is in view. Hosts every PDF-scoped control that previously
   lived split across PdfViewerComponent (page-nav, zoom, layout-toggle) and
   EditorPane's page-header (done-checkbox + Scan / Compare-OCR / Copy-text /
   Copy-all + the Live-Document add-photos affordance).

   Collapse (C1): the four AI/extraction action buttons collapse to icon-only
   under the same overflow mechanism as the main toolbar (.toolbar-container +
   install-overflow-detector!; tier-1 hides .icon-label). Page-nav, zoom, and
   the done-checkbox do NOT collapse — they carry no .icon-label."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
   [freememo.copy-text :as copy]
   [freememo.ocr-compare :as ocr-compare]
   [freememo.pdf-viewer :as viewer]
   [freememo.pdf-viewer-component :refer [LiveDocAddPhotos]]
   [freememo.toolbar-overflow :refer [install-overflow-detector!]]
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled]
                   [java.util.concurrent Executors])))

;; ---------------------------------------------------------------------------
;; OCR scan infrastructure (relocated from editor_pane.cljc — this is now the
;; sole caller). Submits an OCR scan as a Missionary task on a bounded executor.
;; ---------------------------------------------------------------------------

#?(:clj (defonce ocr-executor (Executors/newFixedThreadPool 3)))

(defn start-ocr-scan!
  "Submit an OCR scan as a Missionary task on the bounded executor.
   Timeout after 30s. Cancel function stored in per-user :scan-cancellers."
  [uid doc page ek dpi]
  #?(:clj
     (do
       (swap! (us/get-atom uid :scanning-pages) conj [doc page])
       (swap! (us/get-atom uid :ocr-errors) dissoc [doc page])
       (log/log-info (str "OCR scan started topic-id=" doc " page=" page))
       (let [cancel-fn
             ((m/timeout
                (m/via ocr-executor (page/extract-page-text uid doc page ek dpi))
                30000
                {:success false :error "Scan timed out after 30 seconds"})
              (fn [result]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (if (:success result)
                  (do (log/log-info (str "OCR scan complete topic-id=" doc " page=" page))
                    (swap! (us/get-atom uid :refresh) inc))
                  (do (log/log-info (str "OCR scan failed topic-id=" doc " page=" page " error=" (:error result)))
                    (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (:error result))
                    (toasts/push! uid
                      {:level :error
                       :message (:error result)
                       :actions (if (= :insufficient-credits (:error-type result))
                                  [{:label "Top up credits" :nav :settings}]
                                  [])})))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page]))
              (fn [e]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (when-not (instance? Cancelled e)
                  (log/log-info (str "OCR scan error topic-id=" doc " page=" page " ex=" (.getMessage e)))
                  (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (.getMessage e))
                  (toasts/push! uid {:level :error :message (.getMessage e)}))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page])))]
         (swap! (us/get-atom uid :scan-cancellers) assoc [doc page] cancel-fn))
       nil)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Shared inline styles for the non-collapsing controls (no .btn class — these
;; are compact viewer chrome, matching the prior PdfViewerComponent toolbar).
;; ---------------------------------------------------------------------------

(def ^:private btn-style
  {:padding "6px 12px"
   :cursor "pointer"
   :background "var(--color-bg-card)"
   :border "1px solid var(--color-border)"
   :border-radius "3px"})

(defn- disabled-btn-style [disabled?]
  (assoc btn-style
    :cursor (if disabled? "not-allowed" "pointer")
    :background (if disabled? "var(--color-border)" "var(--color-bg-card)")))

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
        (dom/props {:class "toolbar-container"})
        (let [container-node dom/node]
          (dom/div
            (dom/props {:class "toolbar pdf-toolbar-bar"})
            (let [toolbar-node dom/node
                  cleanup (install-overflow-detector!
                            container-node toolbar-node !tier !overflow-open)]
              (e/on-unmount cleanup)

              ;; ── Page navigation (no collapse) ───────────────────────────
              (dom/div
                (dom/props {:style {:display "flex" :align-items "center" :gap "4px"}})
                (let [prev-disabled? (or (= page-number 1) (= total 0))]
                  (dom/button
                    (dom/props {:title "Previous Page" :disabled prev-disabled?
                                :style (disabled-btn-style prev-disabled?)})
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
                    (dom/props {:title "Next Page" :disabled next-disabled?
                                :style (disabled-btn-style next-disabled?)})
                    (dom/text "▶")
                    (dom/On "click"
                      (fn [_] (when (and (< page-number total) (> total 0))
                                (nav! (inc page-number)))) nil))))

              ;; ── Zoom + layout toggle (no collapse) ──────────────────────
              (dom/div
                (dom/props {:style {:display "flex" :align-items "center" :gap "4px"
                                    :padding-left "12px"
                                    :border-left "1px solid var(--color-border)"}})
                (dom/button
                  (dom/props {:title "Zoom Out" :style btn-style})
                  (dom/text "−")
                  (e/for [_ (dom/On-all "click")] (viewer/zoom! 0.9)))
                (dom/button
                  (dom/props {:title "Zoom In" :style btn-style})
                  (dom/text "+")
                  (e/for [_ (dom/On-all "click")] (viewer/zoom! 1.1)))
                (dom/select
                  (dom/props {:style {:padding "6px 8px" :cursor "pointer"
                                      :background "var(--color-bg-card)"
                                      :border "1px solid var(--color-border)"
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
                    (dom/props {:style (assoc btn-style :font-size "14px")
                                :data-tooltip (if (= layout "top-bottom")
                                                "Switch to side-by-side layout"
                                                "Switch to stacked layout")})
                    (dom/text (if (= layout "top-bottom") "⇅" "⇄"))
                    (dom/On "click" (fn [_] (on-layout-toggle!)) nil))))

              ;; ── Mark-page-done checkbox (no collapse) ───────────────────
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "3px"
                                    :font-size "12px" :cursor "pointer"
                                    :padding-left "12px"
                                    :border-left "1px solid var(--color-border)"}
                            :title "Mark this page as completed to track your extraction progress"})
                (e/for-by identity [_page [page-number]]
                  (dom/input
                    (dom/props {:type "checkbox"})
                    (let [is-done (e/server (when (and pdf-root-id page-number)
                                              (db/get-page-done-status pdf-root-id page-number)))]
                      (set! (.-checked dom/node) (boolean is-done)))
                    (reset! keyboard/!done-btn-ref dom/node)
                    (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
                    (let [change-event (dom/On "change"
                                         (fn [ev] {:checked (-> ev .-target .-checked)
                                                   :page page-number})
                                         nil)
                          [t _] (e/Token change-event)]
                      (when t
                        (case (e/server (db/toggle-page-done! pdf-root-id (:page change-event)))
                          (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                            (t)))))))
                (dom/text (str "Mark page " page-number " as done")))

              ;; ── Scan Page (collapses; E2 sparkle, stays secondary) ──────
              (when llm-enabled?
                (let [scanning? (contains? scanning-pages [pdf-root-id page-number])]
                  (dom/button
                    (dom/props {:class "btn btn-sm btn-secondary"
                                :aria-label "Scan page"
                                :data-tooltip "Extract this page's text with AI OCR (uses credits)"
                                :disabled scanning?})
                    (icons/Icon :sparkles :size 16)
                    (dom/span (dom/props {:class "icon-label"})
                      (dom/text (if scanning? "Scanning…" "Scan Page")))
                    (reset! keyboard/!scan-btn-ref dom/node)
                    (e/on-unmount (fn [] (reset! keyboard/!scan-btn-ref nil)))
                    (let [click-event (dom/On "click"
                                        (fn [_] {:id (str (random-uuid)) :page page-number})
                                        nil)
                          [t _] (e/Token click-event)]
                      (when t
                        (case (e/server
                                (let [pg (:page click-event)
                                      doc pdf-root-id
                                      uid user-id
                                      ek enc-key]
                                  (if (contains? @(us/get-atom uid :scanning-pages) [doc pg])
                                    (log/log-info (str "OCR scan already in progress topic-id=" doc " page=" pg))
                                    (start-ocr-scan! uid doc pg ek scan-dpi))))
                          (t)))))))

              ;; ── Compare OCR / Copy text / Copy all (collapse) ───────────
              (when llm-enabled?
                (ocr-compare/OcrCompareButton user-id pdf-root-id page-number scan-dpi))
              (copy/CopyTextButton user-id pdf-root-id page-number extract-style)
              (copy/CopyAllTextButton user-id pdf-root-id extract-style)

              ;; ── Live Document add-photos (only for live docs) ───────────
              (when is-live?
                (LiveDocAddPhotos {:document-id pdf-root-id :compact? true}))

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
