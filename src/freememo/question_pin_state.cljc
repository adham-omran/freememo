(ns freememo.question-pin-state
  "Client state the rendered pane and the assistant panel share
   (plans/anchored-questions.md §6.5). The two live in separate Electric
   subtrees with no common scope, so the state is a namespace-level atom, the
   `rich-text-editor/!navigate-topic` shape.

   `!finger` is the passage the reader is pointing at in the assistant panel:
   {:quote :anchor :source-topic-id :scroll? :id}, or nil. `:source-topic-id`
   is the file the quote came from, or nil for a map citation, which always
   belongs to the open file. The rendered pane locates the quote in the
   snapshot it shows and outlines it. `:id` makes a repeated click a new value.")

(defonce !finger (atom nil))

(defn point!
  "Point at `passage`, or stop pointing when it is nil."
  [passage]
  (reset! !finger (when passage (assoc passage :id (random-uuid))))
  nil)
