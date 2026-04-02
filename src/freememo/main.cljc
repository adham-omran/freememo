(ns freememo.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.logging :as log]
            [freememo.home-page :refer [HomePage]]
            [freememo.settings-page :refer [SettingsPage]]
            [freememo.library-page :refer [LibraryPage]]
            [freememo.import-page :refer [ImportPage]]
            [freememo.learn-page :refer [LearnPage]]
            [freememo.extract-page :refer [ExtractPage]]
            [freememo.queue-page :refer [QueuePage]]
            [freememo.login-page :refer [LoginPage]]
            [freememo.keyboard :as keyboard]
            #?(:clj [freememo.settings :as settings])))

#?(:clj (defonce !settings-refresh (atom 0)))
#?(:clj (defonce !library-refresh (atom 0)))

(defn get-llm-enabled* [_refresh user-id]
  #?(:clj (settings/get-llm-enabled user-id)
     :cljs nil))

(defn get-active-tab* [_refresh user-id]
  #?(:clj (settings/get-active-tab user-id)
     :cljs nil))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            username (e/server (get-in ring-request [:session :username]))
            enc-key (e/server (get-in ring-request [:session :enc-key]))
            auth-error (e/server (get-in ring-request [:session :auth-error]))]
        (if (e/server (some? user-id))
          ;; Authenticated: render app
          (dom/div
            (dom/props {:style {:height "100vh" :display "flex" :flex-direction "column" :overflow "hidden"}})

            (let [settings-refresh (e/server (e/watch !settings-refresh))
                  llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))
                  saved-tab (e/server (get-active-tab* settings-refresh user-id))
                  !active-tab (atom saved-tab)
                  active-tab (e/watch !active-tab)
                  !tab-to-save (atom nil)
                  tab-to-save (e/watch !tab-to-save)
                  [?token _error] (e/Token tab-to-save)
                  tab-style (fn [key]
                              {:padding "6px 16px" :border "none" :background "none" :cursor "pointer"
                               :font-size "14px" :font-weight (if (= active-tab key) "600" "400")
                               :color (if (= active-tab key) "var(--color-primary)" "var(--color-text-secondary)")
                               :border-bottom (if (= active-tab key) "2px solid var(--color-primary)" "2px solid transparent")
                               :margin-bottom "-2px"})
                  !nav-target (atom nil)
                  navigate! (fn
                              ([tab]
                               (when (= @!active-tab tab)
                                 (reset! !nav-target :go-home))
                               (reset! !active-tab tab)
                               (reset! !tab-to-save tab))
                              ([tab nav]
                               (log/log-debug (str "Navigation tab=" tab " nav=" (pr-str nav)))
                               (reset! !nav-target nav)
                               (reset! !active-tab tab)
                               (reset! !tab-to-save tab)))]

              ;; Persist active tab on change
              (when-some [token ?token]
                (e/server (settings/save-active-tab user-id tab-to-save))
                (token))

              ;; Combined title + tab bar (single row)
              (dom/div
                (dom/props {:class "tab-bar"
                            :style {:display "flex" :align-items "center" :border-bottom "2px solid var(--color-border)" :flex-shrink "0"}})

                ;; Inline title
                (dom/span
                  (dom/props {:style {:font-size "16px" :font-weight "700" :padding "6px 16px"
                                      :color "var(--color-text-primary)" :cursor "pointer" :white-space "nowrap"}})
                  (dom/text "FreeMemo")
                  (dom/On "click" (fn [_] (navigate! :home)) nil))

                (dom/button
                  (dom/props {:style (tab-style :home)})
                  (dom/text "Home")
                  (dom/On "click" (fn [_] (navigate! :home)) nil))

                (dom/button
                  (dom/props {:style (tab-style :learn)})
                  (dom/text "Learn")
                  (dom/On "click" (fn [_] (navigate! :learn)) nil))

                (dom/button
                  (dom/props {:style (tab-style :library)})
                  (dom/text "Library")
                  (dom/On "click" (fn [_] (navigate! :library)) nil))

                (dom/button
                  (dom/props {:style (tab-style :import)})
                  (dom/text "Import")
                  (dom/On "click" (fn [_] (navigate! :import)) nil))

                (dom/button
                  (dom/props {:style (tab-style :queue)})
                  (dom/text "Queue")
                  (dom/On "click" (fn [_] (navigate! :queue)) nil))

                (dom/button
                  (dom/props {:style (tab-style :settings)})
                  (dom/text "Settings")
                  (dom/On "click" (fn [_] (navigate! :settings)) nil)))

              ;; Tab content
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow (if (#{:extract :learn :library :queue} active-tab) "hidden" "auto")}})
                (when (= active-tab :home) (HomePage navigate! user-id enc-key !nav-target))
                (when (= active-tab :library)
                  (let [lib-refresh (e/server (e/watch !library-refresh))
                        tree-signal (LibraryPage user-id !nav-target navigate! lib-refresh)
                        bumped (when (and tree-signal (pos? tree-signal))
                                 (e/server (swap! !library-refresh inc)))]
                    bumped))
                (when (= active-tab :import) (ImportPage user-id !nav-target navigate! enc-key llm-enabled?))
                (when (= active-tab :queue) (QueuePage user-id !nav-target navigate!))
                (when (= active-tab :settings) (SettingsPage user-id username enc-key !settings-refresh))
                (when (= active-tab :learn) (LearnPage user-id enc-key !nav-target #(navigate! :extract) navigate! llm-enabled?))
                (when (= active-tab :extract)
                  (let [nav (e/watch !nav-target)]
                    (ExtractPage user-id enc-key (:topic-id nav) navigate!
                      (fn [topic-id page & [kind]]
                        (reset! !nav-target {:topic-id topic-id :kind (or kind "pdf") :page page})
                        (navigate! :learn))
                      llm-enabled? (:origin nav)))))))

          ;; Not authenticated: render login page
          (LoginPage auth-error))))))

(defn electric-boot [ring-request]
  #?(:clj (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
