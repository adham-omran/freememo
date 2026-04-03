(ns freememo.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.logging :as log]
            [freememo.navigation :as nav]
            [freememo.home-page :refer [HomePage]]
            [freememo.settings-page :refer [SettingsPage]]
            [freememo.library-page :refer [LibraryPage]]
            [freememo.import-page :refer [ImportPage]]
            [freememo.learn-page :refer [LearnPage]]
            [freememo.learn-session :refer [LearnSession]]
            [freememo.extract-page :refer [ExtractPage]]
            [freememo.page-viewer :refer [OcrPage]]
            [freememo.subset-review :refer [SubsetReviewSession]]
            [freememo.login-page :refer [LoginPage]]
            [freememo.keyboard :as keyboard]
            #?(:clj [freememo.settings :as settings])
            #?(:clj [freememo.user-state :as us])
            #?(:clj [freememo.db :as db])))

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

            (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                  llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))
                  saved-tab (e/server (get-active-tab* settings-refresh user-id))
                  !active-tab (atom saved-tab)
                  active-tab (e/watch !active-tab)
                  !viewer-nav (atom nil)
                  viewer-nav (e/watch !viewer-nav)
                  !tab-to-save (atom nil)
                  tab-to-save (e/watch !tab-to-save)
                  [?token _error] (e/Token tab-to-save)
                  tab-style (fn [key]
                              {:padding "6px 16px" :border "none" :background "none" :cursor "pointer"
                               :font-size "14px" :font-weight (if (= active-tab key) "600" "400")
                               :color (if (= active-tab key) "var(--color-primary)" "var(--color-text-secondary)")
                               :border-bottom (if (= active-tab key) "2px solid var(--color-primary)" "2px solid transparent")
                               :margin-bottom "-2px"})
                  navigate! (fn
                              ([tab]
                               (reset! !active-tab tab)
                               (reset! !tab-to-save tab))
                              ([tab nav]
                               (log/log-debug (str "Navigation tab=" tab " nav=" (pr-str nav)))
                               (when (= tab :viewer)
                                 (reset! !viewer-nav nav))
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
                  (dom/props {:style (tab-style :viewer)})
                  (dom/text "Viewer")
                  (dom/On "click" (fn [_] (navigate! :viewer)) nil))

                (dom/button
                  (dom/props {:style (tab-style :library)})
                  (dom/text "Library")
                  (dom/On "click" (fn [_] (navigate! :library)) nil))

                (dom/button
                  (dom/props {:style (tab-style :import)})
                  (dom/text "Import")
                  (dom/On "click" (fn [_] (navigate! :import)) nil))

                (dom/button
                  (dom/props {:style (tab-style :settings)})
                  (dom/text "Settings")
                  (dom/On "click" (fn [_] (navigate! :settings)) nil)))

              ;; Tab content
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :overflow (if (#{:viewer :learn :library} active-tab) "hidden" "auto")}})
                (when (= active-tab :home) (HomePage navigate! user-id enc-key))
                (when (= active-tab :library)
                  (let [lib-refresh (e/server (e/watch (us/get-atom user-id :library-refresh)))
                        tree-signal (LibraryPage user-id navigate! lib-refresh)
                        bumped (when (and tree-signal (pos? tree-signal))
                                 (e/server (swap! (us/get-atom user-id :library-refresh) inc)))]
                    bumped))
                (when (= active-tab :import) (ImportPage user-id navigate! enc-key llm-enabled?))
                (when (= active-tab :settings) (SettingsPage user-id username enc-key))
                (when (= active-tab :learn) (LearnPage user-id navigate! viewer-nav))
                (when (= active-tab :viewer)
                  (if (nil? viewer-nav)
                    ;; Empty state
                    (dom/div
                      (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                          :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
                      (dom/text "Open something from the Library"))
                    ;; Content
                    (let [vtype (:type viewer-nav)]
                      (case vtype
                        :browse-topic
                        (let [topic-id (:topic-id viewer-nav)
                              exists? (e/server (some? (db/get-topic topic-id)))]
                          (if exists?
                            (dom/div
                              (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                              (ExtractPage user-id enc-key topic-id
                                (fn
                                  ([_tab] (navigate! (or (:origin viewer-nav) :library)))
                                  ([_tab _nav] (navigate! (or (:origin viewer-nav) :library))))
                                (fn [root-id page kind]
                                  (if (= kind "pdf")
                                    (navigate! :viewer (nav/nav-browse-pdf root-id page (:origin viewer-nav)))
                                    (navigate! :viewer (nav/nav-browse-topic root-id (:origin viewer-nav)))))
                                llm-enabled? (:origin viewer-nav)))
                            (do (reset! !viewer-nav nil) nil)))

                        :browse-pdf
                        (dom/div
                          (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                          (let [topic-id (:topic-id viewer-nav)
                                origin (:origin viewer-nav)
                                doc-title (e/server (:topics/title (db/get-topic topic-id)))
                                pv-refresh (e/server (e/watch (us/get-atom user-id :refresh)))
                                page-stats (e/server (get-browse-page-stats* pv-refresh topic-id))]
                            (dom/div
                              (dom/props {:class "header-bar" :style {:gap "12px"}})
                              (dom/button
                                (dom/props {:class "btn btn-sm btn-secondary"})
                                (dom/text (case origin :library "Back to Library" :learn "Back to Learn" "Back"))
                                (dom/On "click" (fn [_] (navigate! (or origin :library))) nil))
                              (dom/span
                                (dom/props {:style {:color "var(--color-text-secondary)" :font-size "14px"}})
                                (dom/text (str "Browsing: " (or doc-title "document"))))
                              (when (and page-stats (pos? (:total page-stats)))
                                (let [remaining (:remaining page-stats)]
                                  (dom/span
                                    (dom/props {:style {:color "var(--color-text-secondary)" :font-size "13px" :margin-left "auto" :cursor "default"}
                                                :data-tooltip (cond
                                                                (empty? remaining) "All pages done!"
                                                                (<= (count remaining) 20)
                                                                (str "Remaining: " (clojure.string/join ", " remaining))
                                                                :else
                                                                (str "Remaining: " (clojure.string/join ", " (take 20 remaining))
                                                                  " ... and " (- (count remaining) 20) " more"))})
                                    (dom/text (:done page-stats) " / " (:total page-stats) " pages done"))))))
                          (dom/div
                            (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                            (let [!vnav (atom viewer-nav)]
                              (OcrPage user-id enc-key !vnav llm-enabled?))))

                        :learn-session
                        (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
                              queue-vec (e/server (get-learning-queue* refresh user-id))
                              !queue-idx (atom 0)]
                          (LearnSession user-id enc-key queue-vec !queue-idx navigate! llm-enabled?))

                        :subset-review
                        (SubsetReviewSession user-id enc-key (:root-id viewer-nav) (:root-name viewer-nav)
                          (fn [] (navigate! :library))
                          llm-enabled?)

                        ;; Unknown type — show empty state
                        (dom/div
                          (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                                              :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
                          (dom/text "Open something from the Library")))))))))

          ;; Not authenticated: render login page
          (LoginPage auth-error))))))

(defn electric-boot [ring-request]
  #?(:clj (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
