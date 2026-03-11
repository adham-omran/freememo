(ns electric-starter-app.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [electric-starter-app.home-page :refer [HomePage]]
            [electric-starter-app.settings-page :refer [SettingsPage]]
            [electric-starter-app.pdf-page :refer [PdfPage]]
            [electric-starter-app.ocr-page :refer [OcrPage]]
            [electric-starter-app.queue-page :refer [QueuePage]]
            [electric-starter-app.extract-page :refer [ExtractPage]]
            [electric-starter-app.login-page :refer [LoginPage]]
            #?(:clj [electric-starter-app.settings :as settings])))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            username (e/server (get-in ring-request [:session :username]))
            enc-key  (e/server (get-in ring-request [:session :enc-key]))
            auth-error (e/server (get-in ring-request [:session :auth-error]))]
        (if (e/server (some? user-id))
          ;; Authenticated: render app
          (dom/div
            (dom/props {:style {:height "100vh" :display "flex" :flex-direction "column" :overflow "hidden"}})
            (dom/h1 (dom/props {:style {:margin "8px 16px" :flex-shrink "0"}}) (dom/text "FreeMemo"))

            (let [saved-tab (e/server (settings/get-active-tab user-id))
                  !active-tab (atom saved-tab)
                  active-tab (e/watch !active-tab)
                  !tab-to-save (atom nil)
                  tab-to-save (e/watch !tab-to-save)
                  [?token _error] (e/Token tab-to-save)
                  tab-style (fn [key]
                              {:padding "10px 24px" :border "none" :background "none" :cursor "pointer"
                               :font-size "16px" :font-weight (if (= active-tab key) "600" "400")
                               :color (if (= active-tab key) "#2563eb" "#666")
                               :border-bottom (if (= active-tab key) "2px solid #2563eb" "2px solid transparent")
                               :margin-bottom "-2px"})
                  !nav-target (atom nil)
                  navigate! (fn [tab]
                              (reset! !active-tab tab)
                              (reset! !tab-to-save tab))]

              ;; Persist active tab on change
              (when-some [token ?token]
                (e/server (settings/save-active-tab user-id tab-to-save))
                (token))

              ;; Tab bar
              (dom/div
                (dom/props {:style {:display "flex" :border-bottom "2px solid #e0e0e0" :flex-shrink "0"}})

                (dom/button
                  (dom/props {:style (tab-style :home)})
                  (dom/text "Home")
                  (dom/On "click" (fn [_] (navigate! :home)) nil))

                (dom/button
                  (dom/props {:style (tab-style :settings)})
                  (dom/text "Settings")
                  (dom/On "click" (fn [_] (navigate! :settings)) nil))

                (dom/button
                  (dom/props {:style (tab-style :pdf)})
                  (dom/text "PDF Documents")
                  (dom/On "click" (fn [_] (navigate! :pdf)) nil))

                (dom/button
                  (dom/props {:style (tab-style :workspace)})
                  (dom/text "Workspace")
                  (dom/On "click" (fn [_] (navigate! :workspace)) nil))

                (dom/button
                  (dom/props {:style (tab-style :queue)})
                  (dom/text "Queue")
                  (dom/On "click" (fn [_] (navigate! :queue)) nil)))

              ;; Tab content
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow (if (#{:workspace :extract} active-tab) "hidden" "auto")}})
                (when (= active-tab :home) (HomePage navigate!))
                (when (= active-tab :settings) (SettingsPage user-id username enc-key))
                (when (= active-tab :pdf) (PdfPage user-id))
                (when (= active-tab :workspace) (OcrPage user-id enc-key !nav-target))
                (when (= active-tab :queue) (QueuePage user-id !nav-target #(navigate! :extract)))
                (when (= active-tab :extract) (ExtractPage user-id (:content-item-id (e/watch !nav-target)) navigate!)))))

          ;; Not authenticated: render login page
          (LoginPage auth-error))))))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
