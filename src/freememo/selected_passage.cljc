(ns freememo.selected-passage
  "The passage a card action takes: what the reader selected, on whichever
   surface holds it (plans/editor-pane-toggle.md §5).

   Generate, Generate with Prompt, Compare models, Extract and Add new card
   all read it, so the order the surfaces are tried in is stated once, here:
   the code pane's rendered Markdown view, then its source view, then the
   document editor. A code-pane selection is the live DOM selection, so it
   wins over a Quill range the editor may still hold from earlier. With the
   editor pane hidden, the Quill fallback reads nothing, because hiding it
   calls `editor/release-selection!`."
  (:require
   [freememo.code-pane :as code-pane]
   [freememo.rich-text-editor :as editor]))

(defn current
  "The reader's selection, from the first surface that holds one.

   Pre : none. Client only; nil on the JVM.
   Post: nil when no surface holds a selection. Otherwise
         {:html :text :surface}, where :html is non-empty and :surface is one
         of :code-rendered, :code-source or :editor. :index and :length are
         present iff :surface is :editor. :html keeps the form each surface
         gave before this namespace existed — rendered HTML, an escaped code
         block, or the editor's stored form. :text can be blank (an
         image-only editor range), so each caller keeps its own rule for that."
  []
  #?(:clj nil
     :cljs
     (if-let [html (code-pane/rendered-selection-html)]
       {:html html :text (str (.getSelection js/window)) :surface :code-rendered}
       (if-let [text (code-pane/pane-selection-text)]
         {:html (code-pane/source-block-html text) :text text :surface :code-source}
         (when-let [{:keys [html text index length]} (editor/get-selection-html!)]
           (when (seq html)
             {:html html :text text :surface :editor :index index :length length}))))))

(defn mark!
  "Highlight `passage` where it was taken from.

   Pre : `passage` came from `current`, or is nil.
   Post: an :editor passage is highlighted with `opts`, as
         `editor/highlight-range!` takes them. Any other passage, or nil, is
         left alone: the code pane is read-only and has nothing to mark.
         Returns nil."
  [passage & opts]
  #?(:clj nil
     :cljs (when (= :editor (:surface passage))
             (apply editor/highlight-range! (:index passage) (:length passage) opts)))
  nil)
