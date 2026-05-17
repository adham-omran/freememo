(ns freememo.settings-page
  "Settings page UI component."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :refer [mac-platform?]]
   [freememo.storage-section :refer [StorageSection]]
   [freememo.ai-features-section :refer [AIFeaturesSection]]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))


;; Section anchor ids + sidebar labels, in display order.
;; Storage is folded into the Account tab.
(def section-defs
  [{:id "settings-account" :label "Account"}
   {:id "settings-extraction" :label "Page Extraction"}
   {:id "settings-ai" :label "AI Features"}
   {:id "settings-appearance" :label "Appearance"}
   {:id "settings-anki" :label "Anki Sync"}
   {:id "settings-keyboard" :label "Keyboard Shortcuts"}])

;; Stable atom shared across reactive cycles. Atoms created inside e/defn let
;; bodies are recreated on every cycle (CLAUDE.md anti-pattern) — the scroll
;; handler would write to a stale atom while SettingsNav watches a fresh one.
#?(:cljs (defonce !settings-active-section
           (atom (-> section-defs first :id))))

(defn valid-section-id? [id]
  (boolean (some #(= id (:id %)) section-defs)))

(defn init-active-from-hash!
  "On page mount, if `location.hash` names a section, set the active tab to it.
   Idempotent — safe to call from a reactive body."
  []
  #?(:cljs
     (let [hash (.-hash js/window.location)
           id (when (and hash (> (.-length hash) 1)) (subs hash 1))]
       (when (valid-section-id? id)
         (reset! !settings-active-section id)))
     :clj nil))

(defn set-active-tab!
  "Switch active tab and reflect the choice in the URL hash without scrolling."
  [id]
  #?(:cljs
     (do (reset! !settings-active-section id)
       (.replaceState js/history #js {} "" (str "#" id)))
     :clj nil))

(defn tab-style
  "display:block when this tab is active, display:none otherwise.
   Hides inactive sections without unmounting them — server fetches in each
   section's let bindings still happen once on mount."
  [active section-id]
  {:display (if (= active section-id) "block" "none")})

(e/defn SettingsNav []
  (e/client
    (let [active (e/watch !settings-active-section)]
      (dom/nav
        (dom/props {:class "settings-nav"})
        (e/for-by :id [{:keys [id label]} section-defs]
          (dom/a
            (dom/props {:href (str "#" id)
                        :class (if (= id active)
                                 "settings-nav__item settings-nav__item--active"
                                 "settings-nav__item")})
            (dom/text label)
            (dom/On "click"
              (fn [e] (.preventDefault e) (set-active-tab! id))
              nil)))))))

(e/defn SettingsPage [user-id username enc-key]
  (e/client
    (init-active-from-hash!)
    (let [active (e/watch !settings-active-section)]
      (dom/div
        (dom/props {:class "page-container settings-tabs-page"})

        ;; TOP: horizontal tab bar (Anki-style)
        (SettingsNav)

        ;; BELOW: content panel — only the active tab is visible (display:none others)
        (dom/div
          (dom/props {:class "settings-content"})

      ;; ── Account tab (includes Storage usage) ──
          (dom/div
            (dom/props {:class "card" :id "settings-account"
                        :style (tab-style active "settings-account")})
            (dom/h3 (dom/props {:class "section-title"}) (dom/text "Account"))
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"}})
              (dom/span
                (dom/props {:style {:font-size "14px" :color "var(--color-text-label)"}})
                (dom/text "Logged in as ")
                (dom/strong (dom/text username)))
              (dom/form
                (dom/props {:action "/api/logout" :method "post" :style {:margin "0"}})
                (dom/button
                  (dom/props {:type "submit"
                              :class "btn btn-danger"
                              :style {:padding "6px 14px" :font-size "13px"}})
                  (dom/text "Logout"))))

        ;; Email updates toggle
            (let [server-email-updates (e/server (settings/get-email-updates user-id))
                  !email-updates (atom server-email-updates)
                  email-updates (e/watch !email-updates)]
              (dom/div
                (dom/props {:class "field" :style {:margin-top "12px"}})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked email-updates
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t ?error] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !email-updates change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-email-updates user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/span
                    (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                    (dom/text "Subscribe to email updates")))))

        ;; Storage usage (formerly its own tab; now part of Account)
            (StorageSection user-id))

      ;; ── Page Extraction Buttons section ──
          (let [server-ai-btn (e/server (settings/get-enable-ai-scan-button user-id))
                !ai-btn (atom server-ai-btn)
                ai-btn (e/watch !ai-btn)
                server-pdfbox-btn (e/server (settings/get-enable-pdfbox-button user-id))
                !pdfbox-btn (atom server-pdfbox-btn)
                pdfbox-btn (e/watch !pdfbox-btn)
                server-pdfjs-btn (e/server (settings/get-enable-pdfjs-button user-id))
                !pdfjs-btn (atom server-pdfjs-btn)
                pdfjs-btn (e/watch !pdfjs-btn)]
            (dom/div
              (dom/props {:class "card" :id "settings-extraction"
                          :style (tab-style active "settings-extraction")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Page Extraction Buttons"))
              (dom/div
                (dom/props {:style {:padding "10px 12px" :background "var(--color-info-bg)" :border-radius "var(--radius-md)"
                                    :margin-bottom "var(--sp-3)" :font-size "13px" :line-height "1.5"
                                    :color "var(--color-text-secondary)"}})
                (dom/text "Choose which extraction buttons appear on the page toolbar. Native extractors (PDFBox, PDF.js) read text directly from the PDF — fast and free, but produce no result on scanned pages."))

          ;; AI Scan Page toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked ai-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !ai-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-ai-scan-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Scan Page\" button (AI / OpenAI Vision)"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Best for scanned PDFs and complex layouts. Requires LLM features and an API key.")))))

          ;; PDFBox toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked pdfbox-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !pdfbox-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-pdfbox-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Extract (PDFBox)\" button"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Server-side native text extraction. No AI, no API key.")))))

          ;; PDF.js toggle
              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px" :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked pdfjs-btn
                                :style {:width "18px" :height "18px" :accent-color "var(--color-primary)"}})
                    (let [change-event (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !pdfjs-btn change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-enable-pdfjs-button user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div
                    (dom/span
                      (dom/props {:style {:font-size "14px" :font-weight "500" :color "var(--color-text-primary)"}})
                      (dom/text "Show \"Extract (PDF.js)\" button"))
                    (dom/div
                      (dom/props {:class "hint"})
                      (dom/text "Client-side native text extraction using the loaded PDF.js viewer. No AI, no API key.")))))))

          (dom/div
            (dom/props {:id "settings-ai"
                        :style (tab-style active "settings-ai")})
            (AIFeaturesSection user-id enc-key))

      ;; ── Appearance section ──
          (let [server-font-size (e/server (settings/get-card-font-size user-id))
                !font-size (atom server-font-size)
                font-size (e/watch !font-size)
                server-theme (e/server (settings/get-theme user-id))
                !theme (atom server-theme)
                theme (e/watch !theme)]
            (dom/div
              (dom/props {:class "card" :id "settings-appearance"
                          :style (tab-style active "settings-appearance")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Appearance"))

              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Theme"))
                (dom/select
                  (dom/props {:value theme :class "select"})
                  (dom/option (dom/props {:value "auto"}) (dom/text "Auto"))
                  (dom/option (dom/props {:value "light"}) (dom/text "Light"))
                  (dom/option (dom/props {:value "dark"}) (dom/text "Dark"))
                  (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                        [t ?error] (e/Token change-event)]
                    (when (some? change-event)
                      (reset! !theme change-event))
                    (when t
                      (let [r (e/server (e/Offload #(settings/save-theme user-id change-event)))]
                        (case r
                          (if (:success r)
                            (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                              (t))
                            (t (:error r))))))))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Auto follows your system preference")))

              (dom/div
                (dom/props {:class "field"})
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "10px"}})
                  (dom/span (dom/props {:class "label" :style {:margin-bottom "0"}})
                    (dom/text "Card Text Size"))
                  (e/for-by identity [_k [:font-size-input]]
                    (dom/input
                      (dom/props {:type "number" :min "10" :max "20"
                                  :style {:width "56px" :font-size "13px" :padding "4px 6px"
                                          :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"}})
                      (set! (.-value dom/node) (str font-size))
                      (let [change-event (dom/On "change" #(-> % .-target .-value js/parseInt) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !font-size change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-card-font-size user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r)))))))))
                  (dom/span (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                    (dom/text "px")))
                (dom/div (dom/props {:class "hint"})
                  (dom/text "Adjusts text size for cards (10-20px)")))))

      ;; ── Anki Sync section ──
          (let [server-source-mode (e/server (settings/get-source-display-mode user-id))
                !source-mode (atom server-source-mode)
                source-mode (e/watch !source-mode)
                server-source-field (e/server (settings/get-anki-source-field user-id))
                !source-field (atom server-source-field)
                source-field (e/watch !source-field)
                server-bib-mode (e/server (settings/get-bibliography-display-mode user-id))
                !bib-mode (atom server-bib-mode)
                bib-mode (e/watch !bib-mode)
                server-bib-field (e/server (settings/get-bibliography-field-name user-id))
                !bib-field (atom server-bib-field)
                bib-field (e/watch !bib-field)
                server-auto-mode (e/server (settings/get-anki-auto-load-mode user-id))
                !auto-mode (atom server-auto-mode)
                auto-mode (e/watch !auto-mode)
                server-image-mode (e/server (settings/get-image-display-mode user-id))
                !image-mode (atom server-image-mode)
                image-mode (e/watch !image-mode)
                server-images-front (e/server (settings/get-anki-images-front-field user-id))
                !images-front (atom server-images-front)
                images-front (e/watch !images-front)
                server-images-back (e/server (settings/get-anki-images-back-field user-id))
                !images-back (atom server-images-back)
                images-back (e/watch !images-back)]
            (dom/div
              (dom/props {:class "card" :id "settings-anki"
                          :style (tab-style active "settings-anki")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Anki Sync"))

            ;; Source Display Mode
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Source Display Mode"))
                (dom/div
                  (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "source-display-mode" :value "append"
                                  :checked (= source-mode "append")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t ?error] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !source-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-source-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Append to card"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Source text appended to card content during Anki sync"))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "source-display-mode" :value "field"
                                  :checked (= source-mode "field")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t ?error] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !source-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-source-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Separate field"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Source sent as a separate Anki field"))))))

            ;; Source Field Name — only meaningful in "field" mode
              (when (= source-mode "field")
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Source Field Name"))
                  (dom/input
                    (dom/props {:type "text"
                                :value source-field
                                :placeholder "Source"
                                :class "input"
                                :style {:width "240px"}})
                    (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !source-field change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-anki-source-field user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div (dom/props {:class "hint"})
                    (dom/text "Anki field name that receives the source reference. Defaults to \"Source\"."))))

            ;; Bibliography Display Mode
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Bibliography Display Mode"))
                (dom/div
                  (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "bibliography-display-mode" :value "off"
                                  :checked (= bib-mode "off")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !bib-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Off"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "No bibliography in Anki output"))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "bibliography-display-mode" :value "append"
                                  :checked (= bib-mode "append")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !bib-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Append to card"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Citation appended below the card content during Anki sync"))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "bibliography-display-mode" :value "field"
                                  :checked (= bib-mode "field")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !bib-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-bibliography-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Separate field"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Citation sent as a separate Anki field"))))))

            ;; Bibliography Field Name — only meaningful in "field" mode
              (when (= bib-mode "field")
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Bibliography Field Name"))
                  (dom/input
                    (dom/props {:type "text"
                                :value bib-field
                                :placeholder "Bibliography"
                                :class "input"
                                :style {:width "240px"}})
                    (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !bib-field change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-bibliography-field-name user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div (dom/props {:class "hint"})
                    (dom/text "Anki field name that receives the citation. Defaults to \"Bibliography\". When set to the same field as Source, the two values are combined in append style."))))

            ;; Image Display Mode — how pinned images are routed to Anki on push
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Image Display Mode"))
                (dom/div
                  (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "image-display-mode" :value "inline"
                                  :checked (= image-mode "inline")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !image-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-image-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Inline"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Pinned images stay inside the card's front/back HTML"))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "image-display-mode" :value "field"
                                  :checked (= image-mode "field")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !image-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-image-display-mode user-id change-event)))]
                            (case r
                              (if (:success r)
                                (case (e/server (swap! (us/get-atom user-id :settings-refresh) inc))
                                  (t))
                                (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Separate fields"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Pinned images routed to dedicated Anki fields (named below)"))))))

            ;; Image Front/Back Field Names — only meaningful in field mode
              (when (= image-mode "field")
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Images Front Field"))
                  (dom/input
                    (dom/props {:type "text"
                                :value images-front
                                :placeholder "Images Front"
                                :class "input"
                                :style {:width "240px"}})
                    (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !images-front change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-anki-images-front-field user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div (dom/props {:class "hint"})
                    (dom/text "Anki field receiving front-pin images. Defaults to \"Images Front\"."))))

              (when (= image-mode "field")
                (dom/div
                  (dom/props {:class "field"})
                  (dom/label (dom/props {:class "label"}) (dom/text "Images Back Field"))
                  (dom/input
                    (dom/props {:type "text"
                                :value images-back
                                :placeholder "Images Back"
                                :class "input"
                                :style {:width "240px"}})
                    (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                          [t _] (e/Token change-event)]
                      (when (some? change-event)
                        (reset! !images-back change-event))
                      (when t
                        (let [r (e/server (e/Offload #(settings/save-anki-images-back-field user-id change-event)))]
                          (case r
                            (if (:success r) (t) (t (:error r))))))))
                  (dom/div (dom/props {:class "hint"})
                    (dom/text "Anki field receiving back-pin images. Defaults to \"Images Back\"."))))

            ;; Auto-load Settings — controls what the Anki Sync modal restores on open
              (dom/div
                (dom/props {:class "field"})
                (dom/label (dom/props {:class "label"}) (dom/text "Auto-load Settings"))
                (dom/div
                  (dom/props {:style {:display "flex" :flex-direction "column" :gap "10px" :margin-top "4px"}})
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "anki-auto-load-mode" :value "per-item"
                                  :checked (= auto-mode "per-item")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !auto-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Use Last Settings Per Item"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Restore the saved settings for this specific document; auto-saves on every successful push."))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "anki-auto-load-mode" :value "global"
                                  :checked (= auto-mode "global")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !auto-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "Use Last Settings Globally"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Restore the most-recent settings used anywhere; same defaults across documents."))))
                  (dom/label
                    (dom/props {:style {:display "flex" :align-items "flex-start" :gap "8px" :cursor "pointer"}})
                    (dom/input
                      (dom/props {:type "radio" :name "anki-auto-load-mode" :value "none"
                                  :checked (= auto-mode "none")
                                  :style {:margin-top "3px"}})
                      (let [change-event (dom/On "change" #(-> % .-target .-value) nil)
                            [t _] (e/Token change-event)]
                        (when (some? change-event)
                          (reset! !auto-mode change-event))
                        (when t
                          (let [r (e/server (e/Offload #(settings/save-anki-auto-load-mode user-id change-event)))]
                            (case r
                              (if (:success r) (t) (t (:error r))))))))
                    (dom/div
                      (dom/span (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"}})
                        (dom/text "None (Manual)"))
                      (dom/div (dom/props {:class "hint"})
                        (dom/text "Open the modal with default values; you set everything fresh each time."))))))))

      ;; ── Keyboard Shortcuts section (read-only) ──
          (let [mac? (e/client (mac-platform?))
                mod-key (if mac? "Cmd" "Ctrl")]
            (dom/div
              (dom/props {:class "card" :id "settings-keyboard"
                          :style (tab-style active "settings-keyboard")})
              (dom/h3 (dom/props {:class "section-title"}) (dom/text "Keyboard Shortcuts"))
              (let [shortcut-row (fn [key desc]
                                   {:key (str mod-key "+Shift+" key) :desc desc})
                    shortcuts [(shortcut-row "E" "Extract topic from selection")
                               (shortcut-row "G" "Generate cards")
                               (shortcut-row "S" "Scan Page (OCR)")
                               (shortcut-row "X" "Anki Sync")
                               (shortcut-row "D" "Mark Done")]]
                
                (dom/table
                  (dom/props {:style {:width "100%" :border-collapse "collapse" :font-size "14px"}})
                  (e/for-by :key [s shortcuts]
                    (dom/tr
                      (dom/td (dom/props {:style {:padding "6px 0" :color "var(--color-text-secondary)" :width "180px"}})
                        (dom/text (:key s)))
                      (dom/td (dom/props {:style {:padding "6px 0" :color "var(--color-text-primary)"}})
                        (dom/text (:desc s))))))))))))))
