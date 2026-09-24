(ns freememo.home-page
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.getting-started :as getting-started]
   [freememo.getting-started-steps :as steps]
   [freememo.loading :as loading]
   [freememo.navigation :as nav]
   [freememo.schedule :as sched]
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

(defn get-api-key-status* [_refresh user-id _enc-key]
  #?(:clj (settings/get-openrouter-key-status user-id)
     :cljs nil))

(defn scheduled-page-nudges
  "Keep only schedules that can name pages the reader should start now."
  [summaries]
  (->> summaries
    (keep (fn [{:keys [pages-today next-page-number] :as schedule}]
            (when (and (pos? (long (or pages-today 0))) next-page-number)
              schedule)))
    vec))

(defn home-summary-result* [settings-refresh schedule-refresh user-id enc-key]
  #?(:clj
     (try
       (let [cal (db/get-review-calendar user-id 30)
             first-future (first (filter #(pos? (:count %)) cal))
             next-review
             (when first-future
               (let [review-date (:review_date first-future)
                     today (java.time.LocalDate/now)
                     days (.until today (if (instance? java.time.LocalDate review-date)
                                          review-date
                                          (.toLocalDate review-date))
                            java.time.temporal.ChronoUnit/DAYS)]
                 (cond
                   (<= days 0) "today"
                   (= days 1) "tomorrow"
                   :else (str "in " days " days"))))
             configured? (:configured? (get-api-key-status* settings-refresh user-id enc-key))]
         (loading/region-success
           {:configured? configured?
            :getting-started (getting-started/checklist-input user-id configured?)
            :queue-count (db/get-learning-queue-count user-id)
            :total-count (db/get-total-topic-count user-id)
            :next-review next-review
            :schedule-nudges
            (scheduled-page-nudges
              (sched/schedule-summaries schedule-refresh user-id))}))
       (catch Exception error
         (loading/region-failure (or (ex-message error) "Unknown error"))))
     :cljs (loading/region-success
             {:configured? false :getting-started {:dismissed? true}
              :queue-count 0 :total-count 0 :next-review nil
              :schedule-nudges []})))

(e/defn ScheduleNudges [navigate! schedule-nudges]
  (e/client
    (when (seq schedule-nudges)
      (dom/div
        (dom/props {:style {:margin "0 0 20px" :display "flex"
                            :flex-direction "column" :gap "8px"}})
        (e/for-by :id [schedule schedule-nudges]
          (let [root-topic-id (:root-topic-id schedule)
                page-number (:next-page-number schedule)
                title (:title schedule)
                href (str "/viewer/topic/" root-topic-id "/" page-number)]
            (dom/div
              (dom/props {:style {:padding "10px 12px"
                                  :border "1px solid var(--color-border)"
                                  :border-radius "var(--radius-md)"
                                  :font-size "14px"}})
              (dom/text (str "You have " (:pages-today schedule)
                          " pages scheduled in "))
              (dom/a
                (dom/props {:href href
                            :style {:font-weight "600"
                                    :color "var(--color-primary-text)"
                                    :text-decoration "underline"}})
                (dom/text title)
                (dom/On "click"
                  (fn [event]
                    (.preventDefault event)
                    (navigate! :viewer (nav/nav-topic root-topic-id :home page-number)))
                  nil)))))))))

(e/defn HomeSummaryLoading [navigate!]
  (e/client
    (dom/div
      (dom/p
        (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin "0 0 32px 0"}
                    :role "status"})
        (dom/span (dom/props {:class "spinner" :aria-hidden "true"}))
        (dom/text "Loading your study summary…"))
      (dom/button
        (dom/props {:class "btn btn-primary"
                    :disabled true
                    :aria-busy "true"
                    :style {:width "100%" :padding "14px 0" :font-size "16px" :font-weight "600"
                            :border-radius "var(--radius-md)" :margin-bottom "8px"}})
        (dom/span (dom/props {:class "spinner" :aria-hidden "true"}))
        (dom/text "Loading…"))
      (dom/div (dom/props {:style {:height "61px"}})))))

(e/defn HomeSummaryFailure [navigate! error]
  (e/client
    (dom/div
      (dom/p
        (dom/props {:style {:font-size "14px" :color "var(--color-danger-text)" :margin "0 0 32px 0"}
                    :role "status"})
        (dom/text (str "Could not load your study summary. " error)))
      (dom/button
        (dom/props {:class "btn btn-primary"
                    :style {:width "100%" :padding "14px 0" :font-size "16px" :font-weight "600"
                            :border-radius "var(--radius-md)" :margin-bottom "8px"}})
        (dom/text "Import Content")
        (dom/On "click" (fn [_] (navigate! :import)) nil))
      (dom/div (dom/props {:style {:height "61px"}})))))

(e/defn HomeSummary [navigate! user-id summary]
  (e/client
    (let [{:keys [configured? queue-count total-count next-review schedule-nudges
                  getting-started]} summary
          new-user? (zero? total-count)
          has-due? (pos? queue-count)
          checklist (steps/checklist getting-started)
          ;; The checklist's first step is the import call to action, so a
          ;; new user with the checklist open sees no second one (2.6).
          checklist-imports? (and new-user? (some? checklist))]
      (dom/div
        (when checklist
          (getting-started/GettingStarted navigate! user-id checklist getting-started))

        (when-not (or has-due? checklist-imports?)
          (dom/p
            (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin "0 0 32px 0"}})
            (dom/text
              (if new-user?
                "Import your first document to get started."
                (str "All caught up." (when next-review (str " Next review " next-review ".")))))))

        (ScheduleNudges navigate! schedule-nudges)

        (when (and (not new-user?) has-due?)
          (dom/p
            (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin "0 0 32px 0"}})
            (dom/a
              (dom/props {:href "/learn"
                          :style {:font-weight "600" :color "var(--color-primary-text)" :cursor "pointer"
                                  :text-decoration "none"}})
              (dom/text (str queue-count " topics due"))
              (dom/On "click" (fn [e] (.preventDefault e) (navigate! :learn)) nil))
            (dom/text (str " across " total-count " total"))))

        (when-not checklist-imports?
          (dom/button
            (dom/props {:class "btn btn-primary"
                        :style {:width "100%" :padding "14px 0" :font-size "16px" :font-weight "600"
                                :border-radius "var(--radius-md)" :margin-bottom "8px"}})
            (dom/text
              (cond
                (and new-user? (not configured?)) "Set Up API Key"
                new-user? "Import Your First Document"
                has-due? "Continue Learning"
                :else "Browse Library"))
            (dom/On "click"
              (fn [_]
                (cond
                  (and new-user? (not configured?)) (navigate! :settings)
                  new-user? (navigate! :import)
                  has-due? (navigate! :viewer (nav/nav-learn-session))
                  :else (navigate! :library)))
              nil)))

        (dom/div
          (dom/props {:style {:text-align "center" :margin-bottom "40px"}})
          (when-not new-user?
            (dom/a
              (dom/props {:href "/import"
                          :style {:font-size "13px" :color "var(--color-text-secondary)" :cursor "pointer"
                                  :text-decoration "underline"}})
              (dom/text "or import more content")
              (dom/On "click" (fn [e] (.preventDefault e) (navigate! :import)) nil))))

        (when-not configured?
          (dom/div
            (dom/props {:style {:background "var(--color-warning-bg)" :border "1px solid var(--color-warning-light)" :border-radius "var(--radius-md)"
                                :padding "12px 16px" :display "flex" :align-items "center" :gap "8px"}})
            (dom/span
              (dom/props {:style {:font-size "13px" :color "var(--color-warning-dark)" :flex "1"}})
              (dom/text "OpenRouter API key not configured."))
            (dom/a
              (dom/props {:href "/settings"
                          :style {:font-size "13px" :color "var(--color-warning-dark)" :font-weight "600" :cursor "pointer"
                                  :text-decoration "underline"}})
              (dom/text "Configure in Settings")
              (dom/On "click" (fn [e] (.preventDefault e) (navigate! :settings)) nil))))))))

(e/defn HomePage [navigate! user-id enc-key]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          schedule-refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          request-key [settings-refresh schedule-refresh user-id]]
      (dom/div
        (dom/props {:class "page-container" :style {:padding-top "48px"}})
        (dom/h1
          (dom/props {:style {:font-size "2rem" :margin "0 0 6px 0" :font-weight "700"}})
          (dom/text "FreeMemo"))
        (dom/p
          (dom/props {:style {:font-size "1rem" :color "var(--color-text-secondary)" :margin "0 0 16px 0"}})
          (dom/text "Incremental reading with AI flashcards."))
        (loading/WithLatestRegion request-key
          (e/fn []
            (e/server
              (e/Offload #(assoc (home-summary-result* settings-refresh schedule-refresh user-id enc-key)
                             :request-key request-key))))
          (e/fn [summary _phase _error] (HomeSummary navigate! user-id summary))
          (e/fn [_phase] (HomeSummaryLoading navigate!))
          (e/fn [error] (HomeSummaryFailure navigate! error)))
        (getting-started/ReopenRequests user-id)))))
