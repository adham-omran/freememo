(ns freememo.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router5 :as r]
            [freememo.logging :as log]
            [freememo.navigation :as nav]
            [freememo.home-page :refer [HomePage]]
            [freememo.settings-page :refer [SettingsPage]]
            [freememo.library-page :refer [LibraryPage]]
            [freememo.credits-return-page :refer [CreditsReturnPage]]
            #?(:clj [freememo.credits :as credits])
            #?(:clj [freememo.geo :as geo])
            [freememo.import-page :refer [ImportPage]]
            [freememo.learn-page :refer [LearnPage]]
            [freememo.learn-session :refer [LearnSession]]
            [freememo.topic-page :refer [TopicPage]]
            [freememo.knowledge-page :refer [KnowledgePage]]
            [freememo.graph-page :refer [GraphPage]]
            [freememo.quiz-page :refer [QuizPage GlobalQuizInvokers]]
            [freememo.search-page :refer [SearchPage]]
            [freememo.help-page :refer [HelpPage]]
            [freememo.subset-review :refer [SubsetReviewSession]]
            [freememo.landing-page :refer [LandingPage]]
            [freememo.toast-stack :refer [ToastStack]]
            [freememo.optimistic :refer [CommandDispatcher]]
            [freememo.undo-history-modal :refer [ActionsNavButton UndoHistoryModal]]
            [freememo.icons :as icons]
            [freememo.tooltip :as tooltip]
            [freememo.toolbar-overflow :refer [install-overflow-detector!]]
            [freememo.command-bus :as bus]
            [freememo.command-palette :refer [CommandPalette]]
            [freememo.keyboard :as keyboard] ; loads the registry-driven shortcut handler

            #?(:clj [freememo.settings :as settings])
            #?(:clj [freememo.quota :as quota])
            #?(:clj [freememo.user-state :as us])
            #?(:clj [freememo.db :as db])
            #?(:clj [freememo.config :as config])
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
       :library (list 'library 'documents) ; documents tree is an explicit sub-route
       :knowledge (list 'knowledge 'documents) ; documents is the default sub-route
       (list (symbol (name tab))))
     (case (:type nav-map)
       :topic (if-let [p (:page nav-map)]
                (list 'viewer 'topic (:topic-id nav-map) p)
                (list 'viewer 'topic (:topic-id nav-map)))
       :learn-session (list 'viewer 'learn-session)
       :subset-review (list 'viewer 'subset-review (:root-id nav-map))
       :library-cards (list 'library 'cards)
       :knowledge-entities (list 'knowledge 'entities)
       :knowledge-questions (list 'knowledge 'questions)
       :knowledge-facts (list 'knowledge 'facts (:doc-id nav-map))
       ;; SearchPage seeds [mode kind query] from these segments.
       :search-query (list 'search "fuzzy" "all"
                       #?(:cljs (js/encodeURIComponent (:query nav-map))
                          :clj (:query nav-map)))
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
              ;; /viewer/topic/<id>[/<page>] — page is the URL anchor for PDF
              ;; restore (nil for a bare URL → last-page fallback downstream).
              (let [[topic-id page-seg] r/route
                    url-page (some-> page-seg str parse-long)
                    authorized? (e/server (some? (db/get-topic-for-user user-id topic-id)))]
                (if (not authorized?)
                  (NotFoundView)
                  (dom/div
                    (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})
                    (TopicPage user-id enc-key topic-id navigate! llm-enabled? nil url-page)))))

            learn-session
            ;; Freeze the queue for the session: fetch once on mount, do NOT watch
            ;; :refresh. Re-fetching mid-session reshuffled queue-vec, changed the
            ;; reactive topic-id, and latest-wins-cancelled the in-flight
            ;; done-topic!/advance Offload (Done appeared to advance but never
            ;; committed). The per-day-stable order means a snapshot is the
            ;; intended model. The overview (LearnOverview) re-queries separately.
            (let [queue-vec (e/server (get-learning-queue* 0 user-id))
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

;; A primary-nav destination — text-only in the bar (item 3: icons live only on
;; the right-cluster utilities). `.is-active` drives the filled active pill (CSS
;; owns the visual; item 4). `collapse-class` (or nil) is the tier that hides this
;; tab into the ⋮ dropdown; lower tiers hide first (nav-collapse-1 first). `tip`
;; is the descriptive hover tooltip.
(e/defn NavTab [active-tab navigate! tab-key label collapse-class tip]
  (e/client
    (dom/button
      (dom/props {:class (cond-> "nav-tab"
                           (= active-tab tab-key) (str " is-active")
                           collapse-class          (str " " collapse-class))})
      (tooltip/Tooltip! tip)
      (dom/span (dom/props {:class "nav-tab-label"}) (dom/text label))
      (dom/On "click" (fn [_] (navigate! tab-key)) nil))))

;; A right-cluster utility — icon-only, pinned, never collapses. `tip` names the
;; button for both hover and assistive tech (icon has no visible text).
(e/defn NavUtil [active-tab navigate! tab-key icon tip]
  (e/client
    (dom/button
      (dom/props {:class (cond-> "nav-tab nav-util" (= active-tab tab-key) (str " is-active"))})
      (tooltip/Tooltip! tip :aria? true)
      (icons/Icon icon :size 18)
      (dom/On "click" (fn [_] (navigate! tab-key)) nil))))

;; Dropdown proxy for a collapsed tab. `overflow-class` (nav-overflow-N) mirrors
;; the tab's collapse tier so the proxy appears exactly when its inline tab hid.
(e/defn NavProxy [navigate! !overflow-open tab-key icon label overflow-class]
  (e/client
    (dom/button
      (dom/props {:class (str "nav-overflow-item " overflow-class)})
      (icons/Icon icon :size 16)
      (dom/span (dom/props {:class "nav-overflow-item-label"}) (dom/text label))
      (dom/On "click" (fn [_] (navigate! tab-key) (reset! !overflow-open false)) nil))))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            session-data (e/server (reconstruct-session* user-id))
            username (e/server (:username session-data))
            enc-key (e/server (:enc-key session-data))
            auth-error (e/server (get-in ring-request [:session :auth-error]))
            base-url (e/server (credits/request-base-url ring-request))
            client-country (e/server (-> ring-request geo/client-ip geo/country-of))
            theme (e/server
                    (when user-id
                      (get-theme* (e/watch (us/get-atom user-id :settings-refresh))
                        user-id)))]
        (dom/props {:data-theme (when (and theme (not= theme "auto")) theme)})
        (if (e/server (some? user-id))
          ;; Authenticated: render app with URL router
          (dom/div
            (dom/props {:style {:height "100dvh" :display "flex" :flex-direction "column" :overflow "hidden"}})

            (r/router (r/HTML5-History)
              (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                    llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))

                    ;; Undo/Actions modal open-state (shared by nav button + modal)
                    !undo-modal-open? (atom false)

                    ;; Navigation bridge: children call navigate!, reactive graph calls Navigate!
                    !nav-cmd (atom nil)
                    nav-cmd (e/watch !nav-cmd)
                    [?nav-token _nav-err] (e/Token nav-cmd)

                    ;; Derive active tab from URL route
                    [page] r/route
                    active-tab (if page (keyword (str page)) :home)

                    ;; Content-aware nav overflow (mirrors ContentToolbar):
                    ;; install-overflow-detector! writes `.collapse-N` on the
                    ;; .tab-bar-container; !collapse-tier / !nav-overflow-open are
                    ;; e/watched so Electric stays reactive on tier + dropdown
                    ;; state (the detector owns collapse-N via classList).
                    !collapse-tier (atom 0)
                    !nav-overflow-open (atom false)
                    nav-overflow-open (e/watch !nav-overflow-open)
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

                ;; Shell command context: what app-level commands need.
                ;; Re-published whenever active-tab changes (merge overwrites).
                (bus/publish-ctx! {:active-tab active-tab
                                   :navigate! navigate!
                                   :undo-modal-open-atom !undo-modal-open?})

                ;; Skip link — first focusable element; visually hidden until
                ;; focused (.skip-link CSS). WCAG 2.4.1 bypass for the nav bar.
                (dom/a
                  (dom/props {:href "#main-content" :class "skip-link"})
                  (dom/text "Skip to content"))

                ;; Top nav. `.tab-bar-container` = brand + Import CTA (left,
                ;; pinned) · measured `.tab-bar` destinations (collapse into ⋮) ·
                ;; `.nav-utils` icon utilities (right, pinned). Brand, Import, and
                ;; utilities never collapse; only destinations feed the ⋮ menu.
                (dom/nav
                  (dom/props {:class (str "tab-bar-container" (when nav-overflow-open " overflow-open"))
                              :aria-label "Primary"})
                  (let [container-node dom/node]

                    ;; Brand is a real link (keyboard + SR reachable); click is
                    ;; intercepted so navigation stays in the SPA router.
                    (dom/a
                      (dom/props {:href "/"
                                  :style {:font-size "16px" :font-weight "700" :padding "6px 16px"
                                          :color "var(--color-text-primary)" :cursor "pointer" :white-space "nowrap"
                                          :text-decoration "none" :flex-shrink "0"}})
                      (dom/text "FreeMemo")
                      (dom/On "click" (fn [e] (.preventDefault e) (navigate! :home)) nil))

                    ;; Import — primary create CTA, accented, pinned left.
                    (dom/button
                      (dom/props {:class (cond-> "nav-import-cta" (= active-tab :import) (str " is-active"))})
                      (tooltip/Tooltip! "Add content — link, upload, paste, Zotero")
                      (icons/Icon :plus :size 18)
                      (dom/span (dom/props {:class "nav-import-label"}) (dom/text "Import"))
                      (dom/On "click" (fn [_] (navigate! :import)) nil))

                    ;; Measured destination strip — flex-nowrap + overflow clip so
                    ;; the detector reads overflow via scrollWidth. Two groups
                    ;; (Content · Discovery) split by a lockstep-collapsing divider.
                    (dom/div
                      (dom/props {:class "tab-bar"})
                      (let [toolbar-node dom/node
                            cleanup (install-overflow-detector!
                                      container-node toolbar-node
                                      !collapse-tier !nav-overflow-open)]
                        (e/on-unmount cleanup)

                        ;; Content group — Home never collapses; the rest carry
                        ;; descending priority (nav-collapse-N, higher N = leaves
                        ;; later, so Home>Viewer>Library>Learn>Quiz).
                        (NavTab active-tab navigate! :home "Home" nil "Dashboard & due items")
                        (NavTab active-tab navigate! :viewer "Viewer" "nav-collapse-7" "Read an open document")
                        (NavTab active-tab navigate! :library "Library" "nav-collapse-6" "Your documents & cards")
                        (NavTab active-tab navigate! :learn "Learn" "nav-collapse-5" "Spaced-repetition queue")
                        (NavTab active-tab navigate! :quiz "Quiz" "nav-collapse-4" "LLM-graded quiz on your facts")

                        ;; Content · Discovery divider (hides once Discovery empties).
                        (dom/div (dom/props {:class "nav-group-divider"}))

                        ;; Discovery group — collapses first (Graph→Facts→Search).
                        (NavTab active-tab navigate! :search "Search" "nav-collapse-3" "Search all content")
                        (NavTab active-tab navigate! :knowledge "Facts" "nav-collapse-2" "Distilled facts & entities")
                        (NavTab active-tab navigate! :graph "Graph" "nav-collapse-1" "Visual map of your facts")

                        ;; ⋮ overflow trigger — CSS reveals it from tier 1.
                        (dom/div
                          (dom/props {:class "nav-overflow-trigger"})
                          (dom/button
                            (dom/props {:class "nav-tab" :aria-label "More" :title "More"
                                        :style {:cursor "pointer" :border "none" :background "none"}})
                            (icons/Icon :more-vertical :size 18)
                            (dom/On "click" (fn [_] (swap! !nav-overflow-open not)) nil)))

                        ;; Overflow dropdown — proxies flow inline (display:contents)
                        ;; until .overflow-open turns the panel into a card; each
                        ;; proxy reveals at the tier its inline tab hid (drop order).
                        (dom/div
                          (dom/props {:class "nav-overflow-panel"})
                          (NavProxy navigate! !nav-overflow-open :graph :share-2 "Graph" "nav-overflow-1")
                          (NavProxy navigate! !nav-overflow-open :knowledge :link "Facts" "nav-overflow-2")
                          (NavProxy navigate! !nav-overflow-open :search :search "Search" "nav-overflow-3")
                          (NavProxy navigate! !nav-overflow-open :quiz :clipboard "Quiz" "nav-overflow-4")
                          (NavProxy navigate! !nav-overflow-open :learn :graduation-cap "Learn" "nav-overflow-5")
                          (NavProxy navigate! !nav-overflow-open :library :library "Library" "nav-overflow-6")
                          (NavProxy navigate! !nav-overflow-open :viewer :book-open "Viewer" "nav-overflow-7"))))

                    ;; Utilities — icon-only, pinned right, never collapse.
                    (dom/div (dom/props {:class "nav-utils"})
                      (NavUtil active-tab navigate! :help :circle-help "Help")
                      (NavUtil active-tab navigate! :settings :settings "Settings")
                      (ActionsNavButton !undo-modal-open?))

                    ;; Backdrop for outside-click close — outside .tab-bar so
                    ;; clipping can't hide it.
                    (when nav-overflow-open
                      (dom/div
                        (dom/props {:class "nav-overflow-backdrop"})
                        (dom/On "click" (fn [_] (reset! !nav-overflow-open false)) nil)))))

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
                (dom/main
                  (dom/props {:id "main-content"
                              :style {:flex "1" :min-height "0" :overflow (if (#{:viewer :learn :library :search :graph} active-tab) "hidden" "auto")}})
                  (when (= active-tab :home) (HomePage navigate! user-id enc-key))
                  (when (= active-tab :library)
                    (r/pop ; consume 'library from route; LibraryPage reads the sub-view segment
                      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))]
                        (LibraryPage user-id navigate! refresh))))
                  (when (= active-tab :search)
                    (r/pop ; consume 'search from route; SearchPage reads remaining segments
                      (SearchPage user-id navigate!)))
                  (when (= active-tab :import) (ImportPage user-id navigate! enc-key llm-enabled?))
                  (when (= active-tab :knowledge)
                    (r/pop ; consume 'knowledge from route; KnowledgePage reads the sub-view segment
                      (KnowledgePage user-id navigate!)))
                  (when (= active-tab :graph) (GraphPage user-id navigate!))
                  (when (= active-tab :quiz) (QuizPage user-id))
                  (when (= active-tab :help) (HelpPage))
                  (when (= active-tab :settings) (SettingsPage user-id username enc-key base-url client-country))
                  (when (= active-tab :credits) (CreditsReturnPage user-id navigate!))
                  (when (= active-tab :learn) (LearnPage user-id navigate! nil))
                  (when (= active-tab :viewer)
                    (ViewerContent user-id enc-key navigate! llm-enabled?)))

                ;; Project-wide toast overlay (fixed position; renders nothing when queue empty)
                (ToastStack user-id navigate!)

                ;; Optimistic-update command pump (headless; renders nothing)
                (CommandDispatcher user-id)

                ;; Command architecture: queue-invocation bridge + palette
                (bus/QueueInvoker user-id)
                (GlobalQuizInvokers navigate!)
                (CommandPalette active-tab)

                ;; Actions history modal (undo-newest is a :queue command now)
                (UndoHistoryModal user-id !undo-modal-open?))))

          ;; Not authenticated: render landing page
          (LandingPage auth-error (e/server (config/auth-mode))))))))

(defn electric-boot [ring-request]
  #?(:clj (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
