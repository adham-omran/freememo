(ns freememo.document-ref
  "A document ref: one repository file at one commit
   (plans/commit-range-walkthrough.md §4, §6.3.1, §6.3.2).

   A ref is {:repo-id :path :sha}, plus :topic-id when that commit was
   imported and the file is a topic. Feature 1 (plans/anchored-questions.md)
   keys pins and maps by (repo-id, path) and reads content through `content`,
   so a file a walkthrough shows at a commit that was never imported works the
   same way as a snapshot topic.

   This namespace is the ONE place that turns a ref into source text."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [freememo.git-clone :as git]
            [freememo.repo-import :as repo-import]
            [freememo.repo-tree :as tree])
  (:import [java.nio.charset StandardCharsets]))

(def ^:private sha-re #"[0-9a-f]{40}|[0-9a-f]{64}")

(defn for-topic
  "The ref of `user-id`'s repository file `topic-id`, or nil when it is not
   one (§6.3.2)."
  [user-id topic-id]
  (when topic-id
    (when-let [{:keys [repo_topic_id repo_path commit_sha]} (db/question-doc-ref user-id topic-id)]
      {:repo-id repo_topic_id :path repo_path :sha commit_sha :topic-id topic-id})))

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
   plans/anchored-questions.md)."
  [{:keys [repo-id path]}]
  [repo-id path])

(def ^:private max-cached 32)

(defonce ^:private !contents
  ;; {[repo-id path sha] {:source :lang}}. A file at a commit never changes.
  ;; Emptied, not evicted, when full.
  (atom {}))

(defn content
  "The source of `user-id`'s file at `ref` (§6.3.1).
   Post: {:source :lang}, or nil when the file is absent or not text. The
         source is the file topic's `topic_code` when the commit was
         imported, and `git cat-file` on the clone otherwise; one ref gives
         the same bytes through both."
  [user-id {:keys [repo-id path sha topic-id] :as ref}]
  (when ref
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
  (= "markdown" (tree/language-for path)))
