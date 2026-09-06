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
            [freememo.help-page :refer [HelpPage]]
            [freememo.subset-review :refer [SubsetReviewSession]]
            [freememo.landing-page :refer [LandingPage]]
            [freememo.legal-copy :as legal]
            [freememo.legal-page :refer [LegalPage]]
            [freememo.toast-stack :refer [ToastStack]]
            [freememo.optimistic :refer [CommandDispatcher]]
            [freememo.client-errors :refer [ClientErrorForwarder]]
            [freememo.interaction-trail :as trail :refer [InteractionForwarder]]
            [freememo.undo-history-modal :refer [ActionsNavButton UndoHistoryModal]]
            [freememo.activity-panel :refer [ActivityNavButton ActivityPanel]]
            [freememo.quick-sync :refer [QuickSyncButton]]
            [freememo.video-import-modal :refer [VideoUploadActivity]]
            [freememo.import-modal :refer [StagingUploadActivity]]
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

;; Dropdown proxy for a PINNED ACTION that left the bar, not a destination: it
;; toggles a modal, so it takes that modal's atom instead of a tab key.
;; Revealed by width (≤600px) rather than by collapse tier, because the button
;; it stands in for is hidden by the same media query — the two rules are a pair
;; and exactly one of the affordances is live at any width.
(e/defn NavActionProxy [!overflow-open !modal-open? icon label]
  (e/client
    (dom/button
      (dom/props {:class "nav-overflow-item nav-overflow-action"})
      (icons/Icon icon :size 16)
      (dom/span (dom/props {:class "nav-overflow-item-label"}) (dom/text label))
      (dom/On "click" (fn [_] (swap! !modal-open? not) (reset! !overflow-open false)) nil))))

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
            theme (e/server
                    (when user-id
                      (get-theme* (e/watch (us/get-atom user-id :settings-refresh))
                        user-id)))]
        (dom/props {:data-theme (when (and theme (not= theme "auto")) theme)})
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
            (dom/props {:style {:height "100dvh" :display "flex" :flex-direction "column" :overflow "hidden"}})

            (r/router (r/HTML5-History)
              (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
                    llm-enabled? (e/server (get-llm-enabled* settings-refresh user-id))

                    ;; Undo/Actions modal open-state (shared by nav button + modal)
                    !undo-modal-open? (atom false)

                    ;; Activity monitor open-state (shared by nav badge + popover).
                    ;; Same split as Actions above: the badge lives inside
                    ;; .nav-utils, the popover must sit outside .tab-bar.
                    !activity-open? (atom false)

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
                    _collapse-tier (e/watch !collapse-tier)
                    !nav-overflow-open (atom false)
                    nav-overflow-open (e/watch !nav-overflow-open)
                    ;; Every tab change funnels through here — nav tabs, the ⋮
                    ;; overflow, in-page links and bus/run-nav! all call it — so
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
                ;; focused (.skip-link CSS). WCAG 2.4.1 bypass for the nav bar.
                (dom/a
                  (dom/props {:href "#main-content" :class "skip-link"})
                  (dom/text "Skip to content"))

                ;; Top nav. `.tab-bar-container` = brand + Import CTA (left,
                ;; pinned) · measured `.tab-bar` destinations (collapse into ⋮) ·
                ;; the ⋮ trigger and its panel (pinned) · `.nav-utils` icon
                ;; utilities (right, pinned). Brand, Import, the trigger and the
                ;; utilities never collapse; only destinations feed the ⋮ menu.
                ;; The pinned chrome has a width budget — see index.css.
                (dom/nav
                  (dom/props {:class (str "tab-bar-container" (when nav-overflow-open " overflow-open"))
                              :aria-label "Primary"})
                  (let [container-node dom/node]

                    ;; Brand is a real link (keyboard + SR reachable); click is
                    ;; intercepted so navigation stays in the SPA router. It also
                    ;; carries Home at phone widths, where the Home tab is the
                    ;; first thing the strip clips. Both forms render; the ≤600px
                    ;; media query picks the 44px mark over the 112px wordmark, so
                    ;; the styling has to be a class, not inline props. The
                    ;; aria-label names the link in both forms.
                    (dom/a
                      (dom/props {:href "/" :class "nav-brand" :aria-label "FreeMemo — home"})
                      (dom/img (dom/props {:class "nav-brand-mark" :src "/freememo/favicon.svg" :alt ""}))
                      (dom/span (dom/props {:class "nav-brand-wordmark"}) (dom/text "FreeMemo"))
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
                        (NavTab active-tab navigate! :schedule "Schedule" "nav-collapse-8" "Deadline plan for a document")
                        (NavTab active-tab navigate! :curriculum "Curriculum" "nav-collapse-4" "Ordered study blocks and long-form exams")
                        (NavTab active-tab navigate! :quiz "Quiz" "nav-collapse-4" "LLM-graded quiz on your facts")

                        ;; Content · Discovery divider (hides once Discovery empties).
                        (dom/div (dom/props {:class "nav-group-divider"}))

                        ;; Discovery group — collapses first (Graph→Facts→Search).
                        (NavTab active-tab navigate! :search "Search" "nav-collapse-3" "Search all content")
                        (NavTab active-tab navigate! :knowledge "Facts" "nav-collapse-2" "Distilled facts & entities")
                        (NavTab active-tab navigate! :graph "Graph" "nav-collapse-1" "Visual map of your facts")))

                    ;; ⋮ trigger and its panel are SIBLINGS of the measured strip,
                    ;; never children of it. Inside `.tab-bar` they inherited its
                    ;; `overflow-x: clip` and, once the strip shrank below their
                    ;; own width, laid out past its right edge: unpainted, still
                    ;; focusable, and sitting on top of the Settings button, which
                    ;; then swallowed every click aimed at the ⋮. Out here the
                    ;; trigger is pinned like the utilities, so it cannot be
                    ;; clipped at any width, and the detector measures only tabs.
                    ;; CSS reveals it from tier 1, and unconditionally ≤600px
                    ;; where the panel also carries Actions.
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
                    ;; Actions is last and reveals by width, not by tier.
                    (dom/div
                      (dom/props {:class "nav-overflow-panel"})
                      (NavProxy navigate! !nav-overflow-open :graph :share-2 "Graph" "nav-overflow-1")
                      (NavProxy navigate! !nav-overflow-open :knowledge :link "Facts" "nav-overflow-2")
                      (NavProxy navigate! !nav-overflow-open :search :search "Search" "nav-overflow-3")
                      (NavProxy navigate! !nav-overflow-open :quiz :clipboard "Quiz" "nav-overflow-4")
                      (NavProxy navigate! !nav-overflow-open :curriculum :route "Curriculum" "nav-overflow-4")
                      (NavProxy navigate! !nav-overflow-open :schedule :clock "Schedule" "nav-overflow-8")
                      (NavProxy navigate! !nav-overflow-open :learn :graduation-cap "Learn" "nav-overflow-5")
                      (NavProxy navigate! !nav-overflow-open :library :library "Library" "nav-overflow-6")
                      (NavProxy navigate! !nav-overflow-open :viewer :book-open "Viewer" "nav-overflow-7")
                      (NavActionProxy !nav-overflow-open !undo-modal-open? :rotate-ccw "Actions"))

                    ;; Utilities — icon-only, pinned right, never collapse.
                    (dom/div (dom/props {:class "nav-utils"})
                      (NavUtil active-tab navigate! :help :circle-help "Help")
                      (NavUtil active-tab navigate! :settings :settings "Settings")
                      (ActionsNavButton !undo-modal-open?)
                      (ActivityNavButton user-id !activity-open?))

                    ;; Backdrop for outside-click close — outside .tab-bar so
                    ;; clipping can't hide it.
                    (when nav-overflow-open
                      (dom/div
                        (dom/props {:class "nav-overflow-backdrop"})
                        (dom/On "click" (fn [_] (reset! !nav-overflow-open false)) nil)))

                    ;; Activity popover — sibling of .tab-bar for the same reason
                    ;; the ⋮ panel is: inside it would inherit `overflow-x: clip`.
                    (ActivityPanel user-id !activity-open? navigate!)))

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
                    (when (= active-tab :help) (HelpPage))
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
                      (ViewerContent user-id enc-key navigate! llm-enabled?))))

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
                (UndoHistoryModal user-id !undo-modal-open?))))

          :else
          ;; Not authenticated: render landing page
          (LandingPage auth-error (e/server (config/auth-mode))))))))

(defn electric-boot [ring-request]
  #?(:clj (e/boot-server {} Main (e/server ring-request))
     :cljs (e/boot-client {} Main (e/server (e/amb)))))
