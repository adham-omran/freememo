(ns freememo.gdoc-snapshot-pane
  "The read-only reader of one Google Doc snapshot (plans/google-docs-pull.md,
   row 6.1).

   Mounts in ContentColumn in place of EditorPane, which is what makes a
   snapshot uneditable on the client: EditorPane owns the Quill instance and
   the auto-save that writes `topics.content`. The server half of that guard is
   `db/update-topic-content!`, which refuses a write to a read-only kind.

   A snapshot is the Doc's Markdown export, rendered by
   `repo-markdown/render-document`, so the body reads like the repository's
   rendered Markdown pane: the same `.code-pane-rendered` styles and the same
   fence highlighting.

   The body carries `code-pane/passage-surface-class`, so `selected-passage`
   takes a selection from it as it does from the rendered Markdown pane, and
   Extract and the Generate actions work on the selected text. With nothing
   selected, Generate reads the topic's `content_text`, which is the whole
   snapshot."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.code-pane :as code-pane]
   [freememo.doc-context :as dctx]
   [freememo.math :as math]
   #?(:clj [freememo.db :as db])))

(defn snapshot-html*
  "Post: the stored HTML of `user-id`'s snapshot `topic-id`, or \"\" when it is
   not one. A snapshot never changes after its Pull, so no channel is read."
  [user-id topic-id]
  #?(:clj (let [t (db/get-topic-for-user user-id topic-id)]
            (if (= "gdoc-snapshot" (:topics/kind t)) (or (:topics/content t) "") ""))
     :cljs nil))

(defn show-snapshot!
  "Write `html` into `node`, typeset its math, and highlight its fences.
   CLJ no-op. Returns nil."
  [node html]
  (math/set-html! node html)
  (code-pane/highlight-fences! node)
  nil)

(e/defn GdocSnapshotPane
  "Pre : dctx/page-topic-id names a `gdoc-snapshot` topic.
   Post: its HTML, rendered read-only with math typeset and fences
         highlighted."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/page-topic-id
          html (e/server (e/Offload #(snapshot-html* user-id topic-id)))]
      (dom/div
        (dom/props {:class "gdoc-snapshot" :data-testid "gdoc-snapshot"})
        (dom/div
          (dom/props {:class (str "gdoc-snapshot__body code-pane-rendered " code-pane/passage-surface-class)
                      :tabindex "0"})
          (show-snapshot! dom/node html))))))
