(ns freememo.main
  (:require [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.router5 :as r]
            [freememo.logging :as log]
            [freememo.navigation :as nav]
            [freememo.route-timing :as route-timing]
            [freememo.home-page :refer [HomePage]]
            [freememo.settings-page :refer [SettingsPage]]
            [freememo.library-page :refer [LibraryPage]]
            [freememo.credits-return-page :refer [CreditsReturnPage]]
            #?(:clj [freememo.credits :as credits])
            #?(:clj [freememo.geo :as geo])
            #?(:clj [freememo.electric-process-failure :as process-failure])
            [freememo.import-page :refer [ImportPage]]
            [freememo.learn-page :refer [LearnPage]]
            [freememo.schedule-page :refer [SchedulePage]]
            [freememo.curriculum-page :refer [CurriculumPage]]
            [freememo.learn-session :refer [LearnSession]]
            [freememo.topic-page :refer [TopicPage]]
            [freememo.knowledge-page :refer [KnowledgePage]]
            [freememo.graph-page :refer [GraphPage]]
            [freememo.quiz-page :refer [QuizPage GlobalQuizInvokers]]
            [freememo.search-page :refer [SearchPage]]
            [freememo.help-page :refer [HelpPage PageHelpModal]]
            [freememo.subset-review :refer [SubsetReviewSession]]
            [freememo.landing-page :refer [LandingPage]]
            [freememo.legal-copy :as legal]
            [freememo.legal-page :refer [LegalPage]]
            [freememo.toast-stack :refer [ToastStack]]
            [freememo.optimistic :refer [CommandDispatcher]]
            [freememo.client-errors :refer [ClientErrorForwarder]]
            [freememo.interaction-trail :as trail :refer [InteractionForwarder]]
            [freememo.undo-history-modal :refer [UndoHistoryModal]]
            [freememo.activity-panel :refer [ActivityPanel]]
            [freememo.nav-rail :as nav-rail :refer [NavRail]]
            [freememo.spotlight :as spotlight :refer [Spotlight]]
            [freememo.quick-sync :refer [QuickSyncButton]]
            [freememo.video-import-modal :refer [VideoUploadActivity]]
            [freememo.import-modal :refer [StagingUploadActivity]]
            [freememo.command-bus :as bus]
            [freememo.command-palette :refer [CommandPalette]]
            [freememo.page-title :refer [PageTitle]]
            [freememo.page-title-format :as page-title-format]
            [freememo.keyboard :as keyboard] ; loads the registry-driven shortcut handler

            #?(:clj [freememo.settings :as settings])
            #?(:clj [freememo.quota :as quota])
            #?(:clj [freememo.user-state :as us])
            #?(:clj [freememo.db :as db])
            #?(:clj [freememo.config :as config])
            #?(:clj [freememo.storage-meter :as meter])
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

;; Session queue. Drops the query's :error — the session cannot render one, and
;; LearnOverview is where a user meets a failed queue first (it is the screen
;; "Start Learning" is on). See db/get-learning-queue's postcondition.
(defn get-learning-queue* [_refresh user-id]
  #?(:clj (vec (:rows (db/get-learning-queue user-id)))
     :cljs nil))

(defn get-llm-enabled* [_refresh user-id]
  #?(:clj (settings/get-llm-enabled user-id)
     :cljs nil))

(defn get-theme* [_refresh user-id]
  #?(:clj (settings/get-theme user-id)
     :cljs nil))

(defn get-code-block-font-size* [_refresh user-id]
  #?(:clj (settings/get-code-block-font-size user-id)
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
              ;; §4.6 6.1 — lazy storage accrual on user access. Session boot is
              ;; the coarsest honest "access" point: once per connection, not
              ;; once per navigation. Internally throttled and total, so a
              ;; metering failure can never block sign-in.
              (meter/accrue! user-id)
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
       ;; /schedule/<id>. One extra segment, read back by `r/pop` in
       ;; SchedulePage, so a forecast survives a reload and is linkable.
       :schedule-detail (list 'schedule (:id nav-map))
       :curriculum-detail (list 'curriculum (:id nav-map))
       ;; /curriculum/<id>/exam/<form-id>[/sitting/<sitting-id>]. CurriculumPage
       ;; reads these segments after `r/pop`, so the exam page needs no
       ;; active-tab branch of its own and the Routes count stays 14.
       :curriculum-exam (list 'curriculum (:curriculum-id nav-map)
                          'exam (:form-id nav-map))
       :curriculum-sitting (list 'curriculum (:curriculum-id nav-map)
                             'exam (:form-id nav-map)
                             'sitting (:sitting-id nav-map))
       :subset-review (list 'viewer 'subset-review (:root-id nav-map))
       ;; /help/<slug>. A symbol, so the segment reads as a bare word in the
       ;; URL; HelpPage reads it back after `r/pop`. A sub-route, so the
       ;; Routes count stays 14 (plans/searchable-help.md 2.1).
       :help (list 'help (symbol (:slug nav-map)))
       :library-cards (list 'library 'cards)
       ;; Entities/Questions carry an OPTIONAL doc-id: arriving from a document
       ;; keeps the sub-view scoped to it. Branch rather than emit a trailing nil —
       ;; a nil segment's encoding is the encoder's business, and an absent segment
       ;; is unambiguously "unscoped" in every encoder.
       :knowledge-entities (if-let [doc-id (:doc-id nav-map)]
                             (list 'knowledge 'entities doc-id)
                             (list 'knowledge 'entities))
       :knowledge-questions (if-let [doc-id (:doc-id nav-map)]
                              (list 'knowledge 'questions doc-id)
                              (list 'knowledge 'questions))
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

(e/defn EmptyViewerView
  "A bare /viewer. The rail's Continue opens the last document instead, so this
   is reached only by URL, and it offers the way out (plans/nav-stage-rail.md N5)."
  [navigate!]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :flex-direction "column" :gap "12px"
                          :align-items "center" :justify-content "center"
                          :height "100%" :color "var(--color-text-secondary)" :font-size "15px"}})
      (dom/text "Open something from the Library")
      (dom/button
        (dom/props {:class "btn btn-secondary"})
        (dom/text "Open Library")
        (dom/On "click" (fn [_] (navigate! :library)) nil)))))

(e/defn ViewerContent [user-id enc-key navigate! llm-enabled?]
  (e/client
    (r/pop ; consume 'viewer from route
      (let [[vtype] r/route]
        (if (nil? vtype)
          (do (EmptyViewerView navigate!)
              (PageTitle user-id [:label :viewer]))
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
                    (TopicPage user-id enc-key topic-id navigate! llm-enabled? nil url-page)))
                ;; The route owns the title, never TopicPage: the Learn session and
                ;; subset review mount TopicPage too, and title themselves.
                (PageTitle user-id [:topic topic-id])))

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
                      llm-enabled?)))
                (PageTitle user-id [:topic root-id])))

            ;; Unknown viewer sub-type
            (do (EmptyViewerView navigate!)
                (PageTitle user-id [:label :viewer]))))))))

(e/defn Main [ring-request]
  (e/client
    (binding [dom/node js/document.body]
      (let [user-id (e/server (get-in ring-request [:session :user-id]))
            ;; Only the PATH crosses the wire. Resolving the document on the
            ;; client keeps the copy off the wire entirely: `freememo.legal-copy`
            ;; is already in the client bundle, because `freememo.legal-page`
            ;; reads it, so sending the text too would ship it twice.
            legal-uri (e/server (:uri ring-request))
            legal-doc (legal/document-at legal-uri)
            ;; A legal page renders no app, so resuming a session for it would
            ;; write a `session_resume` row and accrue storage for nothing. The
            ;; guard repeats the lookup server-side rather than reading
            ;; `legal-doc`: that binding is client-sited, and reading it here
            ;; would add a client→server round trip to every app boot.
            session-data (e/server
                           (when-not (legal/document-at (:uri ring-request))
                             (reconstruct-session* user-id)))
            username (e/server (:username session-data))
            enc-key (e/server (:enc-key session-data))
            auth-error (e/server (get-in ring-request [:session :auth-error]))
            base-url (e/server (credits/request-base-url ring-request))
            client-country (e/server (-> ring-request geo/client-ip geo/country-of))
            appearance-settings (e/server
                                  (when user-id
                                    (let [refresh (e/watch
                                                    (us/get-atom user-id :settings-refresh))]
                                      [(get-theme* refresh user-id)
                                       (get-code-block-font-size* refresh user-id)])))
            theme (first appearance-settings)
            code-block-font-size (second appearance-settings)
            code-block-font-size-css ({"extra-small" "0.75em"
                                       "small" "0.875em"
                                       "regular" "1em"
                                       "large" "1.25em"}
                                      code-block-font-size "1em")]
        (dom/props {:data-theme (when (and theme (not= theme "auto")) theme)
                    :style {:--code-block-font-size code-block-font-size-css}})
        (cond
          ;; Public legal document — same words for a visitor, a signed-in
          ;; reader, and (through freememo.crawler-page) a crawler.
          ;;
          ;; Decided before the sign-in branch because it does not depend on
          ;; `user-id`. That placement also keeps these paths out of the router,
          ;; which lives inside the authenticated branch: routed as a tab,
          ;; /privacy would yield an `active-tab` that matches no
          ;; `(when (= active-tab …))` branch and render an empty page.
          ;; The test is client-sited, so it costs no round trip.
          (some? legal-doc)
          (LegalPage legal-doc)

          (e/server (some? user-id))
          ;; Authenticated: render app with URL router
          (dom/div
            ;; `.app-shell` in index.css sets the height and also locks the
            ;; document scroll. Without that lock, Safari could scroll the page
            ;; itself and move the whole app out of view.
            (dom/props {:class "app-shell"})

            (r/router (r/HTML5-History)
              (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                    llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))

                    ;; Undo/Actions modal open-state (shared by nav button + modal)
                    !undo-modal-open? (atom false)

                    ;; Help for this page: the section slug the modal shows, or
                    ;; nil while closed (rail button + help-page/PageHelpModal)
                    !page-help-slug (atom nil)

                    ;; Activity monitor open-state (shared by the rail's Activity
                    ;; item + the popover). The popover is a sibling of the page
                    ;; column, anchored to the rail's docked edge in index.css.
                    !activity-open? (atom false)

                    ;; Navigation bridge: children call navigate!, reactive graph calls Navigate!
                    !nav-cmd (atom nil)
                    nav-cmd (e/watch !nav-cmd)
                    [?nav-token _nav-err] (e/Token nav-cmd)

                    ;; Derive active tab from URL route
                    [page] r/route
                    active-tab (if page (keyword (str page)) :home)
                    ;; Every tab change funnels through here — rail items,
                    ;; in-page links and bus/run-nav! all call it — so
                    ;; it is the one place a :nav trail entry belongs. Only the
                    ;; nav TYPE is recorded: a search nav carries the user's
                    ;; query text, which must not reach the log.
                    navigate! (fn
                                ([tab]
                                 (trail/record! :nav {:tab tab :from active-tab})
                                 (route-timing/start-navigation! tab)
                                 (reset! !nav-cmd {:route (nav->route tab) :id (random-uuid)}))
                                ([tab nav]
                                 (log/log-debug (str "Navigation tab=" tab " nav=" (pr-str nav)))
                                 (trail/record! :nav {:tab tab :from active-tab
                                                      :nav-type (:type nav)})
                                 (route-timing/start-navigation! tab)
                                 (reset! !nav-cmd {:route (nav->route tab nav) :id (random-uuid)})))]

                ;; Execute pending navigation via router
                (when-some [token ?nav-token]
                  (r/Navigate! ['/ (:route nav-cmd)])
                  (token))

                ;; Shell command context: what app-level commands need.
                ;; Re-published whenever active-tab changes (merge overwrites).
                (bus/publish-ctx! {:active-tab active-tab
                                   :navigate! navigate!
                                   :undo-modal-open-atom !undo-modal-open?
                                   :activity-open-atom !activity-open?})

                ;; Skip link — first focusable element; visually hidden until
                ;; focused (.skip-link CSS). WCAG 2.4.1 bypass for the nav rail.
                ;; Absolutely positioned, so it takes no place in the shell row.
                (dom/a
                  (dom/props {:href "#main-content" :class "skip-link"})
                  (dom/text "Skip to content"))

                ;; The nav rail, then the page column (plans/nav-stage-rail.md
                ;; 1.1.1). The rail sits outside every route branch, so it
                ;; mounts once and never remounts on navigation.
                (NavRail user-id r/route navigate! !undo-modal-open? !activity-open? !page-help-slug)

                (dom/div
                  (dom/props {:class "app-column"})

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
                  (case (route-timing/mark-shell! active-tab)
                    (dom/main
                      (dom/props {:id "main-content"
                                  :style {:flex "1" :min-height "0" :overflow (if (#{:viewer :learn :library :search :graph :schedule :curriculum} active-tab) "hidden" "auto")}})
                      (when (= active-tab :home) (HomePage navigate! user-id enc-key))
                      (when (= active-tab :library)
                        (r/pop ; consume 'library from route; LibraryPage reads the sub-view segment
                          (LibraryPage user-id navigate!)))
                      (when (= active-tab :search)
                        (r/pop ; consume 'search from route; SearchPage reads remaining segments
                          (SearchPage user-id navigate!)))
                      (when (= active-tab :import) (ImportPage user-id navigate! enc-key llm-enabled?))
                      (when (= active-tab :knowledge)
                        (r/pop ; consume 'knowledge from route; KnowledgePage reads the sub-view segment
                          (KnowledgePage user-id navigate!)))
                      (when (= active-tab :graph) (GraphPage user-id navigate!))
                      (when (= active-tab :quiz) (QuizPage user-id navigate!))
                      (when (= active-tab :help)
                        (r/pop ; consume 'help; HelpPage reads the slug segment
                          (HelpPage)))
                      (when (= active-tab :settings) (SettingsPage user-id username enc-key base-url client-country))
                      (when (= active-tab :credits) (CreditsReturnPage user-id navigate!))
                      (when (= active-tab :learn) (LearnPage user-id navigate! nil))
                      (when (= active-tab :schedule)
                        (r/pop ; consume 'schedule; SchedulePage reads the id segment
                          (SchedulePage user-id navigate!)))
                      (when (= active-tab :curriculum)
                        (r/pop
                          (CurriculumPage user-id navigate!)))
                      (when (= active-tab :viewer)
                        (ViewerContent user-id enc-key navigate! llm-enabled?))
                      ;; One title owner per route: Main for plain tabs, the page for
                      ;; tabs whose routes name an item. A tab with no label reads
                      ;; Not found (plans/reactive-page-title.md §4).
                      (when-not (page-title-format/self-titled-tabs active-tab)
                        (PageTitle user-id [:label active-tab])))))

                ;; Activity popover — a sibling of the page column, anchored to
                ;; the rail's docked edge (index.css), so an open popover never
                ;; depends on the rail staying open (plans/nav-stage-rail.md N3).
                (ActivityPanel user-id !activity-open? navigate!)

                ;; Project-wide toast overlay (fixed position; renders nothing when queue empty)
                (ToastStack user-id navigate!)

                ;; Optimistic-update command pump (headless; renders nothing)
                (CommandDispatcher user-id)

                ;; Anki reconcile driver (headless). At the root, not in the
                ;; toolbar it used to live in: a reconcile spans the whole
                ;; collection, so a driver mounted under one document died on
                ;; navigation and took the in-flight run with it.
                (QuickSyncButton user-id)

                ;; Activity mirrors for the pushed tier (headless). At the root
                ;; because the transfers they watch outlive the modals that
                ;; started them — that is the whole point of hoisting their state.
                (VideoUploadActivity user-id)
                (StagingUploadActivity user-id)

                ;; Client→server error trail pump (headless; renders nothing)
                (ClientErrorForwarder user-id)

                ;; Client→server interaction trail pump (headless; renders nothing)
                (InteractionForwarder user-id)

                ;; Command architecture: queue-invocation bridge + palette
                (bus/QueueInvoker user-id)
                (GlobalQuizInvokers navigate!)
                (CommandPalette active-tab)

                ;; Actions history modal (undo-newest is a :queue command now)
                (UndoHistoryModal user-id !undo-modal-open?)

                ;; Help for this page: the routed tab's Help section, over the page
                (PageHelpModal !page-help-slug navigate!)

                ;; The getting-started spotlight. Last, so it paints over the
                ;; page, and z-index keeps it under the toasts
                ;; (plans/getting-started-checklist.md 3.3).
                (Spotlight (spotlight/route-key active-tab (nav-rail/viewer-topic-id r/route))))))

          :else
          ;; Not authenticated: render landing page
          (LandingPage auth-error (e/server (config/auth-mode))))))))

(defn electric-boot
  "Boot this app's Electric process. Every entrypoint in src-dev and src-prod
   goes through here.

   The server side logs its process failure through
   `electric-process-failure/logged`, because the adapter's own failure line
   is lost when the teardown stalls."
  [ring-request]
  #?(:clj (process-failure/logged ring-request
            (e/boot-server {} Main (e/server ring-request)))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
