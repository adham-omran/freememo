(ns freememo.assistant-selection-cards
  "The token that turns a Generate cards click in the answer selection menu into
   one queued card job. Its own `e/defn`, so the panel's method stays small.
   See plans/assistant-answer-selection-actions.md §5.2."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.gen-settings :as gs]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.settings :as settings])))

(e/defn SelectionCardsEffect
  "Queue cards for each new value of `!job` = {:id :message-id :html :card-count},
   or nil. The menu writes `!job`, and this component watches it.
   Post: gs/!gen-settings holds the saved settings when nothing has set it
         yet, so the menu's count starts at the saved count. The job is queued
         with the job's count, or its error is in `!error`. Success reports
         itself with a toast when the cards are saved."
  [user-id page-topic-id !job !error]
  (e/client
    (let [saved (e/server (settings/get-generate-settings user-id))
          seeded (e/watch gs/!gen-settings)
          job (e/watch !job)
          [token _] (e/Token job)]
      ;; GenSettingsOwner seeds the same atom when the document view mounts it.
      ;; The guard keeps a value the user set from being replaced.
      (when (and (some? saved) (nil? seeded))
        (reset! gs/!gen-settings (gs/effective saved)))
      (when token
        (let [result (e/server
                       (e/Offload
                         #(assistant/selection->cards!
                            user-id page-topic-id (:message-id job) (:html job)
                            (:card-count job))))]
          (case result
            (case (when (:error result) (reset! !error (:error result)))
              (token))))))))
