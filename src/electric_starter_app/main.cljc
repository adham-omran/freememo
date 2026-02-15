(ns electric-starter-app.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [electric-starter-app.settings-page :refer [SettingsPage]]
            [electric-starter-app.pdf-page :refer [PdfPage]]
            [electric-starter-app.ocr-page :refer [OcrPage]]
            [electric-starter-app.login-page :refer [LoginPage]]))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body] ; DOM nodes will mount under this one
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            username (e/server (get-in ring-request [:session :username]))
            auth-error (e/server (get-in ring-request [:session :auth-error]))]
        (if (e/server (some? user-id))
          ;; Authenticated: render app
          (dom/div
            (dom/props {:style {:height "100vh" :display "flex" :flex-direction "column" :overflow "hidden"}})
            (dom/h1 (dom/props {:style {:margin "8px 16px" :flex-shrink "0"}}) (dom/text "Card Maker"))

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
                (dom/props {:style {:display "flex" :border-bottom "2px solid #e0e0e0" :flex-shrink "0"}})

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
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow (if (= active-tab :ocr) "hidden" "auto")}})
                (when (= active-tab :settings) (SettingsPage user-id username))
                (when (= active-tab :pdf) (PdfPage user-id))
                (when (= active-tab :ocr) (OcrPage user-id)))))

          ;; Not authenticated: render login page
          (LoginPage auth-error))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))  ; inject server-only ring-request
     :cljs (e/boot-client {} Main (e/server (e/amb)))))     ; symmetric – same arity – no-value hole in place of server-only ring-request
