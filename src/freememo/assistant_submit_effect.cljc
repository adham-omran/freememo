(ns freememo.assistant-submit-effect
  "Submission effect for one assistant message."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.interaction-trail :as trail]
   #?(:clj [freememo.assistant :as assistant])))

(e/defn SubmitEffect
  [{:keys [submit refs user-id root-topic-id page-topic-id]}
   token !draft !refs !echo !active !error]
  (e/client
    (when token
      (let [{:keys [id text chat mode] ref-ids :refs} submit
            refs-snapshot (e/snapshot refs)
            compact? (= "/compact" (str/trim text))]
        (if (str/blank? text)
          (token)
          (case
            (trail/record! :assistant
              {:action (if compact? :compact :submit)
               :client-id id :chat-id chat :topic-id root-topic-id
               :page-topic-id page-topic-id :reference-count (count ref-ids)})
            (do
              (reset! !draft "")
              (reset! !refs [])
              (reset! !echo (when-not compact? {:id id :text text}))
              (let [result
                    (e/server
                      (e/Offload
                        #(assistant/submit-chat-input!
                           user-id root-topic-id page-topic-id chat mode
                           text ref-ids id)))]
                (case result
                  (case
                    (diagnostics/replace-state!
                      {:topic-id root-topic-id
                       :page-topic-id page-topic-id
                       :chat-id (:chat-id result)
                       :run-uuid (:run-uuid result)
                       :run-kind (some-> (:command result) name)
                       :run-status (if (:ok result) "queued" "rejected")
                       :render-phase (if (:ok result) "streaming" "terminal")})
                    (do
                      (reset! !active (:chat-id result))
                      (if (:ok result)
                        (reset! !error nil)
                        (do
                          (reset! !error (:error result))
                          (reset! !draft text)
                          (reset! !refs refs-snapshot)
                          (reset! !echo nil)))
                      (token))))))))))))
