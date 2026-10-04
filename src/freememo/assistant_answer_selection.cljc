(ns freememo.assistant-answer-selection
  "The assistant panel's side of the answer selection menu: the annotation
   state, and the calls Electric bodies make into
   freememo.answer-selection-menu, which is ClojureScript only.

   Annotation state is one value, {:chat-id id :items [annotation …]}. It is
   tagged with the chat its answers belong to, so a chat switch hides the items
   without an effect that would have to run at the right moment. Adding to
   another chat starts a new list.

   See plans/assistant-answer-selection-actions.md §5.1, §5.4."
  #?(:cljs (:require [freememo.answer-selection-menu :as menu])))

(def empty-annotations {:chat-id nil :items []})

(defn visible-annotations
  "The annotations of `chat-id`, in the order they were made."
  [state chat-id]
  (if (and chat-id (= chat-id (:chat-id state)))
    (:items state)
    []))

(defn add-annotation
  "`state` with `annotation` appended to the list of `chat-id`.
   Pre:  `chat-id` is the open chat."
  [state chat-id annotation]
  {:chat-id chat-id
   :items (conj (visible-annotations state chat-id) annotation)})

(defn remove-annotation
  "`state` without the annotation `id`."
  [state id]
  (update state :items (fn [items] (into [] (remove #(= id (:id %))) items))))

(defn annotation
  "An annotation from a menu capture and its note."
  [capture note]
  (assoc (select-keys capture [:message-id :quote :start :end])
    :id (str (random-uuid))
    :note note))

(defn install!
  "Install the selection menu on the transcript `node`. CLJ no-op.

   The handlers only write atoms: a menu action is browser state until an
   Electric token in the panel acts on it.
   - Generate cards resets `!selection-cards` to {:id :message-id :html}.
   - Ask aside resets `!side` to {:key :chat-id :message-id :passage}, which
     replaces any open side question.
   - Annotate adds to `!annotations` for the open chat.
   Post: a teardown fn on CLJS, nil on CLJ."
  [node !active !annotations !selection-cards !side]
  #?(:cljs
     (menu/install! node
       {:on-generate (fn [capture]
                       (reset! !selection-cards
                         {:id (str (random-uuid))
                          :message-id (:message-id capture)
                          :html (:html capture)}))
        :on-ask (fn [capture]
                  (reset! !side
                    {:key (str (random-uuid))
                     :chat-id @!active
                     :message-id (:message-id capture)
                     :passage (:quote capture)}))
        :on-annotate (fn [capture note]
                       (swap! !annotations add-annotation @!active
                         (annotation capture note)))})
     :clj nil))

(defn paint!
  "Highlight `items` in the transcript `node`. CLJ no-op. Post: nil."
  [node items]
  #?(:cljs (menu/paint! node items)
     :clj nil))
