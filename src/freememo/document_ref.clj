(ns freememo.document-ref
  "A document ref: one repository file at one commit
   (plans/commit-range-walkthrough.md §4, §6.3.1, §6.3.2).

   A ref is {:repo-id :path :sha}, plus :topic-id when that commit was
   imported and the file is a topic. Feature 1 (plans/anchored-questions.md)
   keys pins and maps by (repo-id, path) and reads content through `content`,
   so a file a walkthrough shows at a commit that was never imported works the
   same way as a snapshot topic.

   A DOCUMENT ref is the second variant (plans/document-question-pins.md
   §1.2): {:doc-topic-id root :topic-id open}, for a topic read in the Quill
   document editor. Its map is keyed by `doc-topic-id`, and it has no source
   text here: the browser locates its pins in the live editor (§3).

   This namespace is the ONE place that turns a topic into a ref, so the ONE
   place that decides which topic a document map is keyed by (§7.1.3), and the
   ONE place that turns a ref into source text."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [freememo.git-clone :as git]
            [freememo.repo-import :as repo-import]
            [freememo.repo-tree :as tree])
  (:import [java.nio.charset StandardCharsets]))

(def ^:private sha-re #"[0-9a-f]{40}|[0-9a-f]{64}")

(def document-kinds
  "Topic kinds read in the Quill document editor, so a document ref's kinds
   (plans/document-question-pins.md §2, §7.1). A `pdf` root and its `page`s
   open the editor on a page. An extract, an EPUB chapter and a topic the user
   made are `basic`. `code` is absent: a repository file has a repository ref,
   and any other code topic opens the code pane."
  #{"basic" "web" "wikipedia" "epub" "markdown" "supermemo" "pdf" "page" "audio"})

(defn- document-for-topic
  "The document ref of `user-id`'s topic `topic-id`, or nil when its view is
   not the document editor.
   The key rule (§7.1): a `page` keys its PDF root, so the merged view and
   every page view share one map (§7.1.1). Every other kind keys itself, so an
   extract keeps a map of its own (§7.1.2)."
  [user-id topic-id]
  (when-let [{:keys [id kind parent_id parent_kind]} (db/question-document-topic user-id topic-id)]
    (when (contains? document-kinds kind)
      {:doc-topic-id (if (and (= "page" kind) (= "pdf" parent_kind)) parent_id id)
       :topic-id id})))

(defn document?
  "True for a document ref, false for a repository ref."
  [ref]
  (contains? ref :doc-topic-id))

(defn for-topic
  "The ref of `user-id`'s topic `topic-id`: a repository ref for a repository
   file (§6.3.2), else a document ref for a topic read in the document editor,
   else nil."
  [user-id topic-id]
  (when topic-id
    (if-let [{:keys [repo_topic_id repo_path commit_sha]} (db/question-doc-ref user-id topic-id)]
      {:repo-id repo_topic_id :path repo_path :sha commit_sha :topic-id topic-id}
      (document-for-topic user-id topic-id))))

(defn resolve-spec
  "A ref from what a client sends: {:topic-id} or {:repo-id :path :sha}.
   Post: a ref `user-id` may read, or nil. A topic spec is checked by
         `for-topic`; a ref spec by the repository node's owner and the shape
         of its path and sha."
  [user-id {:keys [topic-id repo-id path sha]}]
  (cond
    topic-id (for-topic user-id topic-id)
    (and (integer? repo-id) (string? path) (seq path) (not (str/starts-with? path "/"))
      (not (str/includes? path "..")) (string? sha) (re-matches sha-re sha)
      (db/owned-repo-node? user-id repo-id))
    {:repo-id repo-id :path path :sha sha
     :topic-id (db/find-file-topic-at user-id repo-id path sha)}))

(defn doc-key
  "The document key a ref belongs to: pins and maps are keyed by it (§4 of
   plans/anchored-questions.md). A document ref's key is [:topic root]."
  [{:keys [repo-id path doc-topic-id] :as ref}]
  (if (document? ref) [:topic doc-topic-id] [repo-id path]))

(def ^:private max-cached 32)

(defonce ^:private !contents
  ;; {[repo-id path sha] {:source :lang}}. A file at a commit never changes.
  ;; Emptied, not evicted, when full.
  (atom {}))

(defn content
  "The source of `user-id`'s file at `ref` (§6.3.1).
   Post: {:source :lang}, or nil when the file is absent or not text, or the
         ref is a document ref. The
         source is the file topic's `topic_code` when the commit was
         imported, and `git cat-file` on the clone otherwise; one ref gives
         the same bytes through both."
  [user-id {:keys [repo-id path sha topic-id] :as ref}]
  (when (and ref (not (document? ref)))
    (or (when topic-id
          (when-let [{:keys [source lang]} (db/get-topic-code user-id topic-id)]
            {:source source :lang lang}))
      (let [k [repo-id path sha]]
        (or (get @!contents k)
          (when-let [ctx (repo-import/clone-context user-id repo-id)]
            (when-let [bytes (git/file-at (:dir ctx) sha path ctx)]
              (let [v {:source (String. ^bytes bytes StandardCharsets/UTF_8)
                       :lang (tree/language-for path)}]
                (swap! !contents (fn [m] (assoc (if (> (count m) max-cached) {} m) k v)))
                v))))))))

(defn markdown?
  "True when the ref's file renders as Markdown."
  [{:keys [path]}]
  (and (some? path) (= "markdown" (tree/language-for path))))
