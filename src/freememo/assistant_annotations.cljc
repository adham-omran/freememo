(ns freememo.assistant-annotations
  "Annotations: segments of assistant answers that the learner marks, each with
   an optional note, sent together as ONE user turn.

   An annotation is client state until the send. The server turns the list into
   the stored user message with `user-message`, and the browser calls the same
   function for the optimistic echo, so the echo and the stored row match.

   Shape of one annotation, as the composer holds it:
   {:id string :message-id assistant-message-id :quote text :note text-or-nil
    :start n :end n}. Only :quote and :note cross to the server. :start and :end
   are text offsets in the answer's rendered leaf, for the highlight.

   See plans/assistant-answer-selection-actions.md §5.4."
  (:require [clojure.string :as str]))

(def max-annotations
  "Most annotations one send carries."
  20)

(def max-quote-chars
  "Longest quote one annotation carries."
  4000)

(def max-note-chars
  "Longest note one annotation carries."
  2000)

(defn- squeeze
  "`s` with every whitespace run as one space, trimmed. nil reads as empty."
  [s]
  (-> (or s "") (str/replace #"\s+" " ") str/trim))

(defn wire-annotations
  "The part of `annotations` the server needs, in order.
   Post: a vector of {:quote :note}, with whitespace squeezed and a blank note
         as nil."
  [annotations]
  (mapv (fn [{quoted :quote note :note}]
          {:quote (squeeze quoted)
           :note (let [n (str/trim (or note ""))]
                   (when-not (str/blank? n) n))})
    annotations))

(defn problem
  "Why `annotations` cannot be sent, or nil when they can.
   Pre:  `annotations` is the output of `wire-annotations`, or nil."
  [annotations]
  (cond
    (> (count annotations) max-annotations)
    (str "At most " max-annotations " annotations can be sent at once.")

    (some #(str/blank? (:quote %)) annotations)
    "An annotation has no quoted text."

    (some #(> (count (:quote %)) max-quote-chars) annotations)
    (str "An annotation quotes more than " max-quote-chars " characters.")

    (some #(> (count (or (:note %) "")) max-note-chars) annotations)
    (str "An annotation note is longer than " max-note-chars " characters.")))

(defn user-message
  "The user turn that carries `annotations` and the typed `draft`.
   Pre:  `annotations` is non-empty output of `wire-annotations`.
   Post: numbered `[n] \"quote\"` blocks, each followed by its note when it has
         one, then the draft when it is not blank.

   The block form is the one measured on 2026-10-02: with the instructions in
   `freememo.assistant`, the model answered each block in its own section."
  [annotations draft]
  (let [blocks (map-indexed
                 (fn [i {quoted :quote note :note}]
                   (str "[" (inc i) "] \"" quoted "\""
                     (when note (str "\n" note))))
                 annotations)
        typed (str/trim (or draft ""))]
    (str "I have questions about these segments of your answers.\n\n"
      (str/join "\n\n" blocks)
      (when-not (str/blank? typed)
        (str "\n\n" typed)))))
