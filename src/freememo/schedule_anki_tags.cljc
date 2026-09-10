(ns freememo.schedule-anki-tags
  "One-time Anki tagging for the cards inside a selected study schedule range."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.schedule :as sched]))

(defn apply-range-tag!
  "Add one tag to the supplied distinct Anki note IDs.

   Pre: `tag` is nonblank and note IDs came from the selected schedule range.
   Post: `!result` reports the completed action or its local Anki error."
  [note-ids tag unpushed-card-count !result]
  #?(:cljs
     (do
       (reset! !result {:phase :saving})
       ((helpers/anki-call! "addTags" {:notes note-ids :tags tag})
        (fn [_]
          (reset! !result {:phase :done
                           :message (str "Tagged " (count note-ids) " note"
                                         (when (not= 1 (count note-ids)) "s")
                                         ". Skipped " unpushed-card-count " unpushed card"
                                         (when (not= 1 unpushed-card-count) "s") ".")}))
        (fn [error]
          (reset! !result {:phase :error :message (str error)})))
       nil)
     :clj nil))

(e/defn ScheduleRangeTagger
  "Render the selected schedule's one-time range-tag action."
  [user-id schedule-id]
  (e/client
    (let [range-notes (e/server
                        (e/Offload #(sched/schedule-range-anki-notes nil user-id schedule-id)))
          note-ids (e/server (:note-ids range-notes))
          unpushed-card-count (e/server (:unpushed-card-count range-notes))
          !tag (atom "")
          tag (e/watch !tag)
          !result (atom nil)
          result (e/watch !result)
          saving? (= :saving (:phase result))]
      (dom/section
        (dom/props {:class "sched-section"
                    :style {:margin-top "16px" :padding-top "16px"
                            :border-top "1px solid var(--color-border)"}})
        (dom/h3
          (dom/props {:style {:margin "0 0 6px" :font-size "14px"}})
          (dom/text "Tag range cards"))
        (dom/p
          (dom/props {:style {:margin "0 0 8px" :font-size "13px"
                              :color "var(--color-text-secondary)"}})
          (dom/text "Adds one tag to pushed Anki notes in this schedule range."))
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :align-items "center"}})
          (dom/input
            (dom/props {:class "input" :type "text" :value tag
                        :placeholder "study::week-1" :aria-label "Anki tag"})
            (let [value (dom/On "input" (fn [event] (.. event -target -value)) nil)]
              (when (some? value)
                (reset! !tag value))))
          (dom/button
            (dom/props {:class "btn btn-secondary"
                        :disabled (or saving? (str/blank? tag))})
            (dom/text (if saving? "Tagging…" "Tag range cards"))
            (dom/On "click"
              (fn [_]
                (let [tag (str/trim tag)]
                  (if (seq note-ids)
                    (apply-range-tag! note-ids tag unpushed-card-count !result)
                  (reset! !result {:phase :done
                                   :message (str "Tagged 0 notes. Skipped " unpushed-card-count
                                                 " unpushed card"
                                                 (when (not= 1 unpushed-card-count) "s") ".")}))))
              nil)))
        (when result
          (dom/p
            (dom/props {:style {:margin "8px 0 0" :font-size "13px"
                                :color (if (= :error (:phase result))
                                         "var(--color-danger-text)"
                                         "var(--color-text-secondary)")}})
            (dom/text (if (= :error (:phase result))
                        (str "Could not tag Anki notes. " (:message result))
                        (:message result)))))))))
