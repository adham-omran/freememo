(ns freememo.assistant-submit-effect
  "Submission effect for one assistant message."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   #?(:clj [freememo.assistant :as assistant])))

(e/defn SubmitEffect
  [{:keys [token submit refs !draft !refs !echo user-id root-topic-id
           page-topic-id !active !error]}]
  (e/client
    (when token
      (let [{:keys [id text chat mode] ref-ids :refs} submit
            refs-snapshot (e/snapshot refs)
            compact? (= "/compact" (str/trim text))]
        (if (str/blank? text)
          (token)
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
                (do
                  (reset! !active (:chat-id result))
                  (if (:ok result)
                    (reset! !error nil)
                    (do
                      (reset! !error (:error result))
                      (reset! !draft text)
                      (reset! !refs refs-snapshot)
                      (reset! !echo nil)))
                  (token))))))))))
