(ns freememo.pdf-viewer-component
  "PDF viewer UI component using PDF.js."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.pdf-viewer :as viewer]))

(e/defn PdfViewerComponent
  "Renders a PDF viewer for the given document ID and exposes current page number.
   Props: {:document-id <int>, :initial-page <int>, :on-navigate! <fn>,
           :layout <str>, :on-layout-toggle! <fn>}
   Returns: The current page number (for OCR integration)."
  [{:keys [document-id initial-page on-navigate!
           layout on-layout-toggle! target-page]}]
  (e/client
    ;; e/snapshot seeds the atoms ONCE at first mount. Without it, Electric
    ;; re-evaluates (atom …) on subsequent reactive cycles when callers
    ;; rebuild prop closures, recreating !page and silently throwing away
    ;; scroll-induced page changes (observed: scroll to p14 → atom reset → p15).
    (let [seed-page (e/snapshot (or initial-page 1))
          !page (atom seed-page)
          !total (atom 0)
          !container (atom nil)
          !viewer-div (atom nil)
          !input-val (atom (str seed-page))
          !inp-focused (atom false)
          ;; Per-mount stable refs: !timer-id for unmount clearTimeout;
          ;; !requested-doc-id dedupes init/swap to fire once per document-id.
          !timer-id (atom nil)
          !requested-doc-id (atom nil)
          page (e/watch !page)
          total (e/watch !total)
          input-val (e/watch !input-val)
          inp-focused (e/watch !inp-focused)]

      ;; External page-jump request (e.g. hierarchy click). The viewer's own
      ;; on-page-change callback (registered at init time) is the single
      ;; source of truth for !page + on-navigate! — calling them directly here
      ;; rebuilds the on-navigate closure identity, which re-fires the
      ;; setTimeout below and destroys/reinits the viewer mid-jump.
      (when (and target-page (pos? total) (not= target-page page))
        (viewer/go-to-page! target-page))

      ;; Sync page → input-val when not typing
      (when (and (not inp-focused) (not= input-val (str page)))
        (reset! !input-val (str page)))

      (dom/div
        (dom/props {:style {:height "100%"
                            :display "flex"
                            :flex-direction "column"
                            :border "1px solid var(--color-border)"
                            :border-radius "4px"
                            :overflow "hidden"}})

        ;; Toolbar
        (dom/div
          (dom/props {:class "pdf-toolbar"
                      :style {:background "var(--color-bg-subtle)"
                              :border-bottom "1px solid var(--color-border)"
                              :padding "8px"
                              :display "flex"
                              :align-items "center"
                              :gap "12px"
                              :flex-wrap "wrap"}})

          ;; Page navigation section
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "4px"}})

            ;; Previous button
            (dom/button
              (dom/props {:title "Previous Page"
                          :disabled (or (= page 1) (= total 0))
                          :style {:padding "6px 12px"
                                  :cursor (if (or (= page 1) (= total 0)) "not-allowed" "pointer")
                                  :background (if (or (= page 1) (= total 0)) "var(--color-border)" "var(--color-bg-card)")
                                  :border "1px solid var(--color-border)"
                                  :border-radius "3px"}})
              (dom/text "◀")
              (dom/On "click"
                (fn [_]
                  (let [p @!page t @!total]
                    (when (and (> p 1) (> t 0))
                      (let [new-page (dec p)]
                        (reset! !page new-page)
                        (viewer/go-to-page! new-page)
                        (when on-navigate! (on-navigate! new-page))))))
                nil))

            ;; Typed page input
            (dom/input
              (dom/props {:type "text"
                          :value input-val
                          :style {:width "40px" :text-align "center" :padding "4px"
                                  :border "1px solid var(--color-border)" :border-radius "3px"
                                  :font-size "14px"}})
              (dom/On "focus" (fn [_] (reset! !inp-focused true)) nil)
              (dom/On "blur"
                (fn [e]
                  ;; Capture value before any state changes that might trigger re-render
                  (let [raw (-> e .-target .-value)]
                    (reset! !inp-focused false)
                    (let [n (js/parseInt raw)]
                      (when (and (not (js/isNaN n)) (>= n 1) (<= n @!total))
                        (let [clamped (max 1 (min @!total n))]
                          (reset! !page clamped)
                          (viewer/go-to-page! clamped)
                          (when on-navigate! (on-navigate! clamped)))))))
                nil)
              (dom/On "keydown"
                (fn [e]
                  (when (= "Enter" (.-key e))
                    (.blur (.-target e))))
                nil)
              (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
                (when (some? v) (reset! !input-val v))))

            (dom/span
              (dom/props {:class "pdf-collapse-md"
                          :style {:color "var(--color-text-primary)" :padding "0 4px"}})
              (dom/text "of " total))

            ;; Next button
            (dom/button
              (dom/props {:title "Next Page"
                          :disabled (or (>= page total) (= total 0))
                          :style {:padding "6px 12px"
                                  :cursor (if (or (>= page total) (= total 0)) "not-allowed" "pointer")
                                  :background (if (or (>= page total) (= total 0)) "var(--color-border)" "var(--color-bg-card)")
                                  :border "1px solid var(--color-border)"
                                  :border-radius "3px"}})
              (dom/text "▶")
              (dom/On "click"
                (fn [_]
                  (let [p @!page t @!total]
                    (when (and (< p t) (> t 0))
                      (let [new-page (inc p)]
                        (reset! !page new-page)
                        (viewer/go-to-page! new-page)
                        (when on-navigate! (on-navigate! new-page))))))
                nil)))

          ;; Zoom controls + layout toggle (merged into one group to reduce wrapping)
          (dom/div
            (dom/props {:style {:display "flex"
                                :align-items "center"
                                :gap "4px"
                                :margin-left "20px"
                                :padding-left "20px"
                                :border-left "1px solid var(--color-border)"}})

            (dom/button
              (dom/props {:class "pdf-collapse-md"
                          :title "Zoom Out"
                          :style {:padding "6px 12px"
                                  :cursor "pointer"
                                  :background "var(--color-bg-card)"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "3px"}})
              (dom/text "−")
              (e/for [_ (dom/On-all "click")] (viewer/zoom! 0.9)))

            (dom/button
              (dom/props {:class "pdf-collapse-md"
                          :title "Zoom In"
                          :style {:padding "6px 12px"
                                  :cursor "pointer"
                                  :background "var(--color-bg-card)"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "3px"}})
              (dom/text "+")
              (e/for [_ (dom/On-all "click")] (viewer/zoom! 1.1)))
            (dom/select
              (dom/props {:style {:padding "6px 8px"
                                  :cursor "pointer"
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
                        (when-not (js/isNaN scale)
                          (viewer/set-zoom! scale))))))
                (t)))
            (when on-layout-toggle!
              (dom/button
                (dom/props {:style {:padding "6px 10px" :cursor "pointer"
                                    :background "var(--color-bg-card)"
                                    :border "1px solid var(--color-border)"
                                    :border-radius "3px" :font-size "14px"}
                            :data-tooltip (if (= layout "top-bottom")
                                            "Switch to side-by-side layout"
                                            "Switch to stacked layout")})
                (dom/text (if (= layout "top-bottom") "\u21C5" "\u21C4"))
                (dom/On "click" (fn [_] (on-layout-toggle!)) nil)))))

        ;; Viewer wrapper (relative positioning for absolute container inside)
        (dom/div
          (dom/props {:style {:flex "1"
                              :position "relative"}})

          ;; Viewer container — created ONCE (init-viewer!) and the document is
          ;; swapped IN-PLACE (set-document!) when document-id changes; never
          ;; remounted per topic. Mirrors the persistent Quill editor
          ;; (rich_text_editor_component) and fixes the :diff-corruption WS crash
          ;; that the previous per-document e/for-by remount caused.
          (dom/div
            (dom/props {:class "pdf-viewer-container"
                        :style {:position "absolute"
                                :top "0"
                                :left "0"
                                :right "0"
                                :bottom "0"
                                :overflow "auto"
                                :background "var(--color-pdf-bg)"}})
            (reset! !container dom/node)
            (e/on-unmount
              (fn []
                (log/log-debug "PDF-COMP unmount")
                (when-let [t @!timer-id] (js/clearTimeout t))
                (viewer/destroy-viewer!)))

            (dom/div
              (dom/props {:class "pdfViewer"})
              (reset! !viewer-div dom/node)

              ;; document-id and initial-page appear ONLY inside fn closures
              ;; (Electric treats fn bodies as opaque). !requested-doc-id dedupes
              ;; so this fires exactly once per document-id change.
              (let [pdf-url (str "/api/pdf/" document-id)
                    on-ready (fn [^js pdf _]
                               (reset! !total (.-numPages pdf))
                               (viewer/on-page-change! (fn [page-num]
                                                         (reset! !page page-num)
                                                         (when on-navigate! (on-navigate! page-num))))
                               ;; A PDF-page extract's "Go to page" button primes
                               ;; nav/!pending-page-jump then navigates here; consume
                               ;; it post-load (go-to-page is reliable only after
                               ;; pagesloaded). Overrides the last-page resume.
                               (let [pending @nav/!pending-page-jump
                                     jump (when (and pending (= (:root pending) document-id))
                                            (:page pending))
                                     resume-page (or jump initial-page 1)]
                                 (when jump (reset! nav/!pending-page-jump nil))
                                 (when (> resume-page 1)
                                   (viewer/go-to-page-after-load! resume-page)))
                               (viewer/setup-pinch-zoom! @!container))]
                (when (not= document-id @!requested-doc-id)
                  (reset! !requested-doc-id document-id)
                  (if (nil? @viewer/!viewer-state)
                    ;; No viewer yet → create once, deferred so DOM nodes exist.
                    (reset! !timer-id
                      (js/setTimeout
                        (fn []
                          (reset! !timer-id nil)
                          (viewer/init-viewer! @!container @!viewer-div pdf-url on-ready))
                        100))
                    ;; Viewer exists, different doc → swap in place (no remount).
                    (viewer/set-document! pdf-url on-ready))))))))

      ;; Return current page number for OCR integration
      page)))
