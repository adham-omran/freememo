(ns electric-starter-app.home-page
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.settings :as settings])
   #?(:clj [electric-starter-app.db :as db])))

(defn get-api-key-status* [user-id enc-key]
  #?(:clj (settings/get-openai-api-key-status user-id enc-key)
     :cljs nil))

(defn get-queue-count* [user-id]
  #?(:clj (db/get-learning-queue-count user-id)
     :cljs 0))

(defn get-total-count* [user-id]
  #?(:clj (db/get-total-topic-count user-id)
     :cljs 0))

(defn next-review-label* [user-id]
  #?(:clj
     (let [cal (db/get-review-calendar user-id 30)
           first-future (first (filter #(pos? (:count %)) cal))]
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
             :else (str "in " days " days")))))
     :cljs nil))

(e/defn HomePage [navigate! user-id enc-key !nav-target]
  (e/client
    (let [api-status (e/server (get-api-key-status* user-id enc-key))
          configured? (:configured? api-status)
          queue-count (e/server (get-queue-count* user-id))
          total-count (e/server (get-total-count* user-id))
          next-review (e/server (next-review-label* user-id))
          new-user? (zero? total-count)
          has-due? (pos? queue-count)]
      (dom/div
        (dom/props {:style {:padding "48px 24px" :max-width "640px" :margin "0 auto"}})

        ;; Section 1: Hero + Adaptive Status Line
        (dom/h1
          (dom/props {:style {:font-size "2rem" :margin "0 0 6px 0" :font-weight "700"}})
          (dom/text "FreeMemo"))

        (dom/p
          (dom/props {:style {:font-size "1rem" :color "var(--color-text-secondary)" :margin "0 0 16px 0"}})
          (dom/text "Incremental reading with spaced repetition."))

        (dom/p
          (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin "0 0 32px 0"}})
          (dom/text
            (cond
              new-user?
              "Import your first document to get started."

              has-due?
              ""

              :else
              (str "All caught up." (when next-review (str " Next review " next-review "."))))))

        ;; Inline clickable "N topics due" for active users
        (when (and (not new-user?) has-due?)
          (dom/p
            (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)" :margin "-28px 0 32px 0"}})
            (dom/a
              (dom/props {:style {:font-weight "600" :color "var(--color-primary)" :cursor "pointer"
                                  :text-decoration "none"}})
              (dom/text (str queue-count " topics due"))
              (dom/On "click" (fn [_] (navigate! :learn)) nil))
            (dom/text (str " across " total-count " total"))))

        ;; Section 2: Primary Action Button
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
                has-due? (do (reset! !nav-target :start-session)
                           (navigate! :learn))
                :else (navigate! :library)))
            nil))

        ;; Secondary action link
        (dom/div
          (dom/props {:style {:text-align "center" :margin-bottom "40px"}})
          (when-not new-user?
            (dom/a
              (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)" :cursor "pointer"
                                  :text-decoration "underline"}})
              (dom/text (if has-due? "or import more content" "or import more content"))
              (dom/On "click" (fn [_] (navigate! :import)) nil))))

        ;; Section 3: Quick links (compact)
        (dom/div
          (dom/props {:style {:display "flex" :gap "12px" :margin-bottom "40px"}})
          (e/for-by first [row [["Import" :import] ["Library" :library] ["Queue" :queue]]]
            (let [[label target] row]
              (dom/button
                (dom/props {:class "btn btn-secondary"
                            :style {:flex "1" :padding "10px 0" :font-size "13px"}})
                (dom/text label)
                (dom/On "click" (fn [_] (navigate! target)) nil)))))

        ;; Section 4: API Key Notice (only when not configured)
        (when-not configured?
          (dom/div
            (dom/props {:style {:background "#fffbeb" :border "1px solid #fde68a" :border-radius "var(--radius-md)"
                                :padding "12px 16px" :display "flex" :align-items "center" :gap "8px"}})
            (dom/span
              (dom/props {:style {:font-size "13px" :color "#92400e" :flex "1"}})
              (dom/text "OpenAI API key not configured."))
            (dom/a
              (dom/props {:style {:font-size "13px" :color "#92400e" :font-weight "600" :cursor "pointer"
                                  :text-decoration "underline"}})
              (dom/text "Configure in Settings")
              (dom/On "click" (fn [_] (navigate! :settings)) nil))))))))
