(ns freememo.assistant-composer
  "Message editor and controls for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   #?(:clj [freememo.assistant :as assistant])))

(defn- submit-edit [draft-atom active-atom refs-atom mode-id]
  (let [text (str/trim @draft-atom)]
    (when-not (str/blank? text)
      {:id (str (random-uuid)) :text text :chat @active-atom
       :refs (mapv :id @refs-atom) :mode mode-id})))

(defn sync-composer-draft-value!
  "Set the visible composer value after a programmatic draft edit.

   Forms5 owns the textarea's value, so a `reset!` on the draft atom alone does
   not move what the learner sees. Picking a reference and removing one both
   rewrite the draft from outside the input, and both go through here."
  [value]
  #?(:cljs
     (when-let [input (.querySelector js/document ".assistant-panel__input")]
       (set! (.-value input) value))
     :clj nil))

(defn close-reference-menu-after-input!
  "Close the menu after Forms5 finishes the current input transaction."
  [value !at-open?]
  #?(:cljs
     (js/setTimeout
       #(when-not (panel-data/active-document-mention? value)
          (reset! !at-open? false))
       0)
     :clj nil))

(e/defn AssistantComposer
  [{:keys [at-open? mention-query active-reference-index reference-document-count refs
           sending? mode-entry draft user-id active root-topic-id]}
   reference-documents !picked !refs !mention-index !at-open? !draft !active !submit !pending-mode
   !error]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__composer-shell"})
      ;; Keep the virtual-scroll frame stable while the form processes an input
      ;; event. Conditional mounting in the same frame strips Forms5 input props.
      (rows/ReferenceMenu at-open? reference-documents reference-document-count
        mention-query active-reference-index !picked)
      (when (seq refs)
        (dom/div
          (dom/props {:class "assistant-panel__refs"})
          (e/for-by :id [reference refs]
            (dom/span
              (dom/props {:class "assistant-panel__ref-chip"})
              (dom/text (str "@" (:title reference)))
              (dom/button
                (dom/props {:class "assistant-panel__ref-remove" :type "button"
                            :title "Remove reference"
                            :aria-label "Remove reference"})
                (dom/text "×")
                (let [event (dom/On "click" identity nil)
                      [remove-token _] (e/Token event)]
                  (when remove-token
                    (let [next-draft (panel-data/draft-without-reference
                                       draft (:title reference))]
                      (case (swap! !refs
                              (fn [current]
                                (into []
                                  (remove #(= (:id reference) (:id %)))
                                  current)))
                        (case (sync-composer-draft-value! next-draft)
                          (case (reset! !draft next-draft)
                            (remove-token))))))))))))
      (dom/div
        (dom/props {:class "assistant-panel__composer"})
        (dom/On "input"
          (fn [event]
            (close-reference-menu-after-input!
              (.. event -target -value) !at-open?))
          nil)
        (dom/On "keydown"
          (fn [event]
            (cond
              (and at-open? (= (.-key event) "ArrowDown")
                (pos? reference-document-count))
              (do
                (.preventDefault event)
                (swap! !mention-index #(mod (inc %) reference-document-count)))

              (and at-open? (= (.-key event) "ArrowUp")
                (pos? reference-document-count))
              (do
                (.preventDefault event)
                (swap! !mention-index #(mod (dec %) reference-document-count)))

              (and at-open? (= (.-key event) "Enter")
                (pos? reference-document-count))
              (do
                (.preventDefault event)
                (reset! !picked active-reference-index))

              (and at-open? (= (.-key event) "Escape"))
              (do (.preventDefault event) (reset! !at-open? false))

              (and (= (.-key event) "Enter")
                (not (.-shiftKey event))
                (not sending?))
              (do
                (.preventDefault event)
                (when-let [edit
                           (submit-edit !draft !active !refs (:id mode-entry))]
                  (reset! !submit edit)))

              (= (.-key event) "@")
              (do (reset! !mention-index 0) (reset! !at-open? true))

              (and at-open? (= (.-key event) " "))
              (reset! !at-open? false)))
          nil)
        (e/for [[command-token command]
                (e/amb
                  (dom/div
                    (dom/props {:class "assistant-panel__input-row"})
                    (forms/Input! :assistant/draft draft :as :textarea
                      :class "assistant-panel__input" :dir "auto" :rows 2
                      :placeholder
                      "Ask about this page… (@ for another document)"
                      :aria-label "Assistant message" :disabled sending?))
                  (dom/div
                    (dom/props {:class "assistant-panel__composer-footer"})
                    (dom/div
                      (dom/props {:class "assistant-panel__composer-controls"})
                      (rows/ModeRow
                        user-id active mode-entry !pending-mode)
                      (rows/ModelRow user-id root-topic-id))
                    (if sending?
                      (forms/Button! [::stop]
                        :class "btn btn-secondary assistant-panel__send"
                        :label "Stop" :aria-label "Stop response")
                      (forms/Button! [::submit]
                        :class "btn btn-primary assistant-panel__send"
                        :label "↑" :aria-label "Send"
                        :disabled (str/blank? draft)))))]
          (cond
            (map? command)
            (case (reset! !draft (:assistant/draft command))
              (command-token))

            (= [::submit] command)
            (case
              (when-let [edit
                         (submit-edit !draft !active !refs (:id mode-entry))]
                (reset! !submit edit))
              (command-token))

            (= [::stop] command)
            (let [result
                  (e/server
                    (e/Offload #(assistant/stop-active-run! user-id active)))]
              (case result
                (case
                  (reset! !error
                    (when-not (:stopped result)
                      "No active response to stop."))
                  (command-token))))))))))
