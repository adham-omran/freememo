(ns electric-starter-app.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [electric-starter-app.settings-page :refer [SettingsPage]]
            [electric-starter-app.pdf-page :refer [PdfPage]]
            [electric-starter-app.ocr-page :refer [OcrPage]]))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body] ; DOM nodes will mount under this one
      (dom/div ; mandatory wrapper div to ensure node ordering - https://github.com/hyperfiddle/electric/issues/74
        (dom/h1 (dom/text "Card Maker"))

        (let [!active-tab (atom :settings)
              active-tab (e/watch !active-tab)
              tab-style (fn [key]
                          {:padding "10px 24px" :border "none" :background "none" :cursor "pointer"
                           :font-size "16px" :font-weight (if (= active-tab key) "600" "400")
                           :color (if (= active-tab key) "#2563eb" "#666")
                           :border-bottom (if (= active-tab key) "2px solid #2563eb" "2px solid transparent")
                           :margin-bottom "-2px"})]

          ;; Tab bar
          (dom/div
            (dom/props {:style {:display "flex" :border-bottom "2px solid #e0e0e0" :margin-bottom "16px"}})

            (dom/button
              (dom/props {:style (tab-style :settings)})
              (dom/text "Settings")
              (dom/On "click" (fn [_] (reset! !active-tab :settings)) nil))

            (dom/button
              (dom/props {:style (tab-style :pdf)})
              (dom/text "PDF Documents")
              (dom/On "click" (fn [_] (reset! !active-tab :pdf)) nil))

            (dom/button
              (dom/props {:style (tab-style :ocr)})
              (dom/text "OCR Text Extraction")
              (dom/On "click" (fn [_] (reset! !active-tab :ocr)) nil)))

          ;; Tab content
          (when (= active-tab :settings) (SettingsPage))
          (when (= active-tab :pdf) (PdfPage))
          (when (= active-tab :ocr) (OcrPage)))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request
