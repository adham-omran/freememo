(ns freememo.assistant-submit-effect
  "Submission effect for one assistant message."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   [freememo.assistant-annotations :as annotations]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-composer :as composer]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.assistant-slash :as slash]
   [freememo.interaction-trail :as trail]
   #?(:clj [freememo.assistant :as assistant])))

(defn- echo-text
  "What the optimistic echo of a send shows: the text the server will store.
   nil for `/compact`, which stores no turn. A `/fork` stores its question
   (plans/assistant-chat-forks.md §4.3.2)."
  [text wire-annotations question-pins?]
  (let [command (when-not (or (seq wire-annotations) question-pins?)
                  (slash/parse text))]
    (cond
      question-pins? "Asking about my open pins…"
      (seq wire-annotations) (annotations/user-message wire-annotations text)
      (= :compact (:command command)) nil
      (= :fork (:command command)) (:question command)
      :else text)))

(e/defn SubmitEffect
  "Send one composer submit and settle the panel.
   Post: on success the panel shows the chat the server names, which is the
   new fork for a fork, and a pending fork the send consumed is cleared. On an
   error the draft, references and annotations come back."
  [{:keys [submit refs annotation-state user-id root-topic-id page-topic-id]}
   token !draft !refs !echo !active !error !annotations !pending-fork]
  (e/client
    (when token
      (let [{:keys [id text chat mode web-search? question-pins? fork] ref-ids :refs
             wire-annotations :annotations} submit
            refs-snapshot (e/snapshot refs)
            annotation-snapshot (e/snapshot annotation-state)
            annotated? (boolean (seq wire-annotations))
            ;; An Ask over pins carries no text: the server builds the turn
            ;; from the open pins (plans/anchored-questions.md §6.2.5.1).
            question-pins? (boolean question-pins?)
            echo (echo-text text wire-annotations question-pins?)
            compact? (and (nil? echo) (not question-pins?))]
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
              (reset! !echo (when echo {:id id :text echo}))
              (let [result
                    (e/server
                      (e/Offload
                        #(if question-pins?
                           (assistant/submit-question-pins!
                             user-id root-topic-id page-topic-id chat mode web-search? id)
                           (assistant/submit-chat-input!
                             user-id root-topic-id page-topic-id chat mode web-search?
                             text ref-ids id wire-annotations fork))))]
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
                        (do (reset! !error nil)
                            (when (= :fork (:command result))
                              (reset! !pending-fork nil)))
                        (do
                          (reset! !error (:error result))
                          (when-not question-pins?
                            (composer/replace-composer-draft! !draft text)
                            (reset! !refs refs-snapshot)
                            (reset! !annotations annotation-snapshot))
                          (reset! !echo nil)))
                      (token))))))))))))
