(ns freememo.question-pin-state
  "Client state the rendered pane and the assistant panel share
   (plans/anchored-questions.md §6.5). The two live in separate Electric
   subtrees with no common scope, so the state is a namespace-level atom, the
   `rich-text-editor/!navigate-topic` shape.

   `!finger` is the passage the reader is pointing at in the assistant panel:
   {:quote :anchor :source-doc :scroll? :id}, or nil. `:source-doc` is the
   document key [repo-id path] the quote came from, or nil for a map
   citation, which always belongs to the open file. The rendered pane locates the quote in the
   snapshot it shows and outlines it. `:id` makes a repeated click a new value.")

(defonce !finger (atom nil))

(defn point!
  "Point at `passage`, or stop pointing when it is nil."
  [passage]
  (reset! !finger (when passage (assoc passage :id (random-uuid))))
  nil)

(defonce !canvas-request
  ;; A citation the reader clicked whose file is in the open walkthrough step
  ;; but not in its canvas (plans/commit-range-walkthrough.md §6.6.4): the
  ;; canvas swaps to `:source-doc`'s file and then points at the passage.
  (atom nil))

(defn open-in-canvas!
  "Ask the walkthrough canvas to show `passage`'s file and point at it."
  [passage]
  (reset! !canvas-request (assoc passage :id (random-uuid)))
  nil)
