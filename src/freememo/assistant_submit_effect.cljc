(ns freememo.assistant-submit-effect
  "Submission effect for one assistant message."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   [freememo.assistant-annotations :as annotations]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.interaction-trail :as trail]
   #?(:clj [freememo.assistant :as assistant])))

(e/defn SubmitEffect
  [{:keys [submit refs annotation-state user-id root-topic-id page-topic-id]}
   token !draft !refs !echo !active !error !annotations]
  (e/client
    (when token
      (let [{:keys [id text chat mode web-search? question-pins?] ref-ids :refs
             wire-annotations :annotations} submit
            refs-snapshot (e/snapshot refs)
            annotation-snapshot (e/snapshot annotation-state)
            annotated? (boolean (seq wire-annotations))
            ;; An Ask over pins carries no text: the server builds the turn
            ;; from the open pins (plans/anchored-questions.md §6.2.5.1).
            question-pins? (boolean question-pins?)
            compact? (and (not annotated?) (not question-pins?)
                       (= "/compact" (str/trim text)))]
        (if (and (str/blank? text) (not annotated?) (not question-pins?))
          (token)
          (case
            (trail/record! :assistant
              {:action (if compact? :compact :submit)
               :client-id id :chat-id chat :topic-id root-topic-id
               :page-topic-id page-topic-id :reference-count (count ref-ids)
               :annotation-count (count wire-annotations)
               :question-pins? question-pins?})
            (do
              (when-not question-pins?
                (composer/replace-composer-draft! !draft "")
                (reset! !refs [])
                (reset! !annotations answer-selection/empty-annotations))
              ;; The echo is the text the server stores, so the row that
              ;; retires it shows the same words.
              (reset! !echo (when-not compact?
                              {:id id
                               :text (cond
                                       question-pins? "Asking about my open pins…"
                                       annotated? (annotations/user-message wire-annotations text)
                                       :else text)}))
              (let [result
                    (e/server
                      (e/Offload
                        #(if question-pins?
                           (assistant/submit-question-pins!
                             user-id root-topic-id page-topic-id chat mode web-search? id)
                           (assistant/submit-chat-input!
                             user-id root-topic-id page-topic-id chat mode web-search?
                             text ref-ids id wire-annotations))))]
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
                          (when-not question-pins?
                            (composer/replace-composer-draft! !draft text)
                            (reset! !refs refs-snapshot)
                            (reset! !annotations annotation-snapshot))
                          (reset! !echo nil)))
                      (token))))))))))))
