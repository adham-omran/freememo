(ns freememo.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router5 :as r]
            [freememo.logging :as log]
            [freememo.navigation :as nav]
            [freememo.home-page :refer [HomePage]]
            [freememo.settings-page :refer [SettingsPage]]
            [freememo.library-page :refer [LibraryPage]]
            [freememo.import-page :refer [ImportPage]]
            [freememo.learn-page :refer [LearnPage]]
            [freememo.learn-session :refer [LearnSession]]
            [freememo.topic-page :refer [TopicPage]]
            [freememo.search-page :refer [SearchPage]]
            [freememo.subset-review :refer [SubsetReviewSession]]
            [freememo.landing-page :refer [LandingPage]]
            [freememo.keyboard :as keyboard]
            #?(:clj [freememo.settings :as settings])
            #?(:clj [freememo.quota :as quota])
            #?(:clj [freememo.user-state :as us])
            #?(:clj [freememo.db :as db])
            #?(:clj [freememo.crypto :as crypto])))

;; Per-user refresh via user-state registry

(defn get-browse-page-stats* [_refresh parent-id]
  #?(:clj (let [pages (db/list-pages parent-id)
                remaining (sort (map :topics/page_number
                                  (remove #(= "done" (:topics/status %)) pages)))]
            {:done (- (count pages) (count remaining))
             :total (count pages)
             :remaining remaining})
     :cljs nil))

(defn get-learning-queue* [_refresh user-id]
  #?(:clj (vec (db/get-learning-queue user-id))
     :cljs nil))

(defn get-llm-enabled* [_refresh user-id]
  #?(:clj (settings/get-llm-enabled user-id)
     :cljs nil))

(defn get-theme* [_refresh user-id]
  #?(:clj (settings/get-theme user-id)
     :cljs nil))

(defn get-usage-pct* [_refresh user-id]
  #?(:clj (let [usage (quota/get-user-usage db/ds user-id)
                cap (quota/get-user-quota db/ds user-id)]
            (if (and cap (pos? cap))
              (* 100.0 (/ (double usage) cap))
              0.0))
     :cljs 0.0))

;; Server-only: `format` is CLJ-only; embedding it in an e/defn body captures
;; the var and breaks WebSocket serialization. Format the banner text here
;; and ship the result string instead.
(defn format-banner-text* [_refresh pct]
  #?(:clj (format "Storage %.0f%% full — delete documents from Library to free space."
            (double (or pct 0)))
     :cljs ""))

(defn reconstruct-session* [user-id]
  #?(:clj (when user-id
            (when-let [user (db/get-user-by-id user-id)]
              (db/insert-user-event! user-id "session_resume")
              (let [google-id (:users/google_id user)
                    enc-key (when google-id (crypto/derive-key-for-oauth-user google-id))]
                {:username (:users/username user)
                 :enc-key enc-key})))
     :cljs nil))

;; Convert navigate! API calls to route lists for router5.
;; Viewer navs use (nav/nav-topic id origin) — single /viewer/topic/<id> route.

(defn- nav->route
  ([tab] (nav->route tab nil))
  ([tab nav-map]
   (if (nil? nav-map)
     (case tab
       :home nil
       :viewer (list 'viewer) ; empty viewer
       (list (symbol (name tab))))
     (case (:type nav-map)
       :topic (list 'viewer 'topic (:topic-id nav-map))
       :learn-session (list 'viewer 'learn-session)
       :subset-review (list 'viewer 'subset-review (:root-id nav-map))
       (list (symbol (name tab)))))))

(e/defn NotFoundView []
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                          :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
      (dom/text "Not found"))))

(e/defn EmptyViewerView []
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                          :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
      (dom/text "Open something from the Library"))))

(e/defn ViewerContent [user-id enc-key navigate! llm-enabled?]
  (e/client
    (r/pop ; consume 'viewer from route
      (let [[vtype] r/route]
        (if (nil? vtype)
          (EmptyViewerView)
          (case vtype
            topic
            (r/pop
              (let [[topic-id] r/route
                    authorized? (e/server (some? (db/get-topic-for-user user-id topic-id)))]
                (if (not authorized?)
                  (NotFoundView)
                  (dom/div
                    (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                    (TopicPage user-id enc-key topic-id navigate! llm-enabled? nil)))))

            learn-session
            (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
                  queue-vec (e/server (get-learning-queue* refresh user-id))
                  !queue-idx (atom 0)]
              (LearnSession user-id enc-key queue-vec !queue-idx navigate! llm-enabled?))

            subset-review
            (r/pop
              (let [[root-id] r/route
                    authorized? (e/server (some? (db/get-topic-for-user user-id root-id)))]
                (if (not authorized?)
                  (NotFoundView)
                  (let [root-name (e/server (:topics/title (db/get-topic-for-user user-id root-id)))]
                    (SubsetReviewSession user-id enc-key root-id root-name
                      (fn [] (navigate! :library))
                      llm-enabled?)))))

            ;; Unknown viewer sub-type
            (EmptyViewerView)))))))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            session-data (e/server (reconstruct-session* user-id))
            username (e/server (:username session-data))
            enc-key (e/server (:enc-key session-data))
            auth-error (e/server (get-in ring-request [:session :auth-error]))
            theme (e/server
                    (when user-id
                      (get-theme* (e/watch (us/get-atom user-id :settings-refresh))
                        user-id)))]
        (dom/props {:data-theme (when (and theme (not= theme "auto")) theme)})
        (if (e/server (some? user-id))
          ;; Authenticated: render app with URL router
          (dom/div
            (dom/props {:style {:height "100vh" :display "flex" :flex-direction "column" :overflow "hidden"}})

            (r/router (r/HTML5-History)
              (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                    llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))

                    ;; Navigation bridge: children call navigate!, reactive graph calls Navigate!
                    !nav-cmd (atom nil)
                    nav-cmd (e/watch !nav-cmd)
                    [?nav-token _nav-err] (e/Token nav-cmd)

                    ;; Derive active tab from URL route
                    [page] r/route
                    active-tab (if page (keyword (str page)) :home)

                    tab-style (fn [key]
                                {:padding "6px 16px" :border "none" :background "none" :cursor "pointer"
                                 :font-size "14px" :font-weight (if (= active-tab key) "600" "400")
                                 :color (if (= active-tab key) "var(--color-primary)" "var(--color-text-secondary)")
                                 :border-bottom (if (= active-tab key) "2px solid var(--color-primary)" "2px solid transparent")
                                 :margin-bottom "-2px"})
                    navigate! (fn
                                ([tab]
                                 (reset! !nav-cmd {:route (nav->route tab) :id (random-uuid)}))
                                ([tab nav]
                                 (log/log-debug (str "Navigation tab=" tab " nav=" (pr-str nav)))
                                 (reset! !nav-cmd {:route (nav->route tab nav) :id (random-uuid)})))]

                ;; Execute pending navigation via router
                (when-some [token ?nav-token]
                  (r/Navigate! ['/ (:route nav-cmd)])
                  (token))

                ;; Combined title + tab bar
                (dom/div
                  (dom/props {:class "tab-bar"
                              :style {:display "flex" :align-items "center" :border-bottom "2px solid var(--color-border)" :flex-shrink "0"}})

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
                    (dom/props {:style (tab-style :viewer)})
                    (dom/text "Viewer")
                    (dom/On "click" (fn [_] (navigate! :viewer)) nil))

                  (dom/button
                    (dom/props {:style (tab-style :library)})
                    (dom/text "Library")
                    (dom/On "click" (fn [_] (navigate! :library)) nil))

                  (dom/button
                    (dom/props {:style (tab-style :search)})
                    (dom/text "Search")
                    (dom/On "click" (fn [_] (navigate! :search)) nil))

                  (dom/button
                    (dom/props {:style (tab-style :import)})
                    (dom/text "Import")
                    (dom/On "click" (fn [_] (navigate! :import)) nil))

                  (dom/button
                    (dom/props {:style (tab-style :settings)})
                    (dom/text "Settings")
                    (dom/On "click" (fn [_] (navigate! :settings)) nil)))

                ;; Storage warning banner — appears when usage exceeds 80%.
                (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
                      usage-pct (e/server (get-usage-pct* refresh user-id))
                      banner-text (e/server (format-banner-text* refresh usage-pct))]
                  (when (and usage-pct (> usage-pct 80))
                    (dom/div
                      (dom/props {:style {:padding "8px 16px"
                                          :background (if (>= usage-pct 100)
                                                        "var(--color-danger-bg, #fee)"
                                                        "var(--color-warning-bg, #fff3cd)")
                                          :color "var(--color-text-primary)"
                                          :border-bottom "1px solid var(--color-border)"
                                          :font-size "13px" :flex-shrink "0"}})
                      (dom/text banner-text)
                      (dom/button
                        (dom/props {:style {:margin-left "12px" :padding "2px 10px" :font-size "12px"
                                            :background "transparent" :border "1px solid var(--color-border)"
                                            :color "var(--color-text-primary)"
                                            :border-radius "3px" :cursor "pointer"}})
                        (dom/text "Library")
                        (dom/On "click" (fn [_] (navigate! :library)) nil)))))

                ;; Tab content
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow (if (#{:viewer :learn :library :search} active-tab) "hidden" "auto")}})
                  (when (= active-tab :home) (HomePage navigate! user-id enc-key))
                  (when (= active-tab :library)
                    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))]
                      (LibraryPage user-id navigate! refresh)))
                  (when (= active-tab :search)
                    (r/pop ; consume 'search from route; SearchPage reads remaining segments
                      (SearchPage user-id navigate!)))
                  (when (= active-tab :import) (ImportPage user-id navigate! enc-key llm-enabled?))
                  (when (= active-tab :settings) (SettingsPage user-id username enc-key))
                  (when (= active-tab :learn) (LearnPage user-id navigate! nil))
                  (when (= active-tab :viewer)
                    (ViewerContent user-id enc-key navigate! llm-enabled?))))))

          ;; Not authenticated: render landing page
          (LandingPage auth-error))))))

(defn electric-boot [ring-request]
  #?(:clj (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
