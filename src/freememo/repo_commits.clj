(ns freememo.repo-commits
  "Commit details for the repository home and the commit card, read from the
   repository node's clone on each visit (plans/repository-home.md §4.2.3,
   §4.4). Nothing here is stored: the clone is the source.

   A read costs no network while the clone holds the commit. A missing clone
   is made again by `repo-import/clone-context`, and a commit the clone lacks
   is fetched once."
  (:require [taoensso.telemere :as tel]
            [freememo.db :as db]
            [freememo.git-clone :as git]
            [freememo.repo-import :as repo-import]
            [freememo.walkthrough-range :as wr]))

(defn- failure-message [^Throwable t]
  (if (= :freememo.forge/forge-failure (:type (ex-data t)))
    (.getMessage t)
    (do (tel/error! {:id ::commit-read-failed} t)
        "FreeMemo could not read this commit. The server log has the details.")))

(defn- clone-holding
  "The clone of `user-id`'s repository `repo-id` that holds every sha of
   `shas`, fetching once when one is missing.
   Post: the clone's {:dir …}, or nil when the node is not an imported
         repository. Throws a forge failure when a fetch fails."
  [user-id repo-id shas]
  (when-let [ctx (repo-import/clone-context user-id repo-id)]
    (if (every? #(git/commit-exists? (:dir ctx) %) shas)
      ctx
      (repo-import/clone-context user-id repo-id true))))

(defn snapshot-subjects
  "The subject line of each snapshot commit of `user-id`'s repository node
   `repo-id`. Post: {:ok {sha subject}}, or {:error S}. A sha the clone
   cannot name is absent from the map."
  [user-id repo-id]
  (try
    (let [shas (keep :commit_sha (db/repo-snapshots user-id repo-id))]
      (if (empty? shas)
        {:ok {}}
        (let [{:keys [dir]} (clone-holding user-id repo-id shas)
              present (filterv #(git/commit-exists? dir %) shas)]
          {:ok (if (seq present)
                 (into {} (map (juxt :sha :subject)) (git/log dir (into ["--no-walk"] present)))
                 {})})))
    (catch Throwable t {:error (failure-message t)})))

(defn commit-card
  "What the card of `user-id`'s commit snapshot `topic-id` shows (§4.4.1),
   and whether it can be walked through (§4.4.4).
   Post: nil when `topic-id` is not a commit snapshot; else {:repo-id :sha
         :ref :subject :body :author :date :changed-files :refusal}, where
         `:refusal` is why `walkthrough-range/taught-range` refuses the
         commit alone, or nil; or {:error S} when the clone cannot be read."
  [user-id topic-id]
  (when-let [{:keys [repo-id sha ref]} (db/commit-node user-id topic-id)]
    (try
      (let [{:keys [dir]} (clone-holding user-id repo-id [sha])
            c (first (git/log dir ["-1" sha]))
            refusal (try (wr/taught-range dir sha sha) nil
                      (catch clojure.lang.ExceptionInfo e
                        (if (= :freememo.forge/forge-failure (:type (ex-data e)))
                          (.getMessage e)
                          (throw e))))]
        {:repo-id repo-id :sha sha :ref ref
         :subject (:subject c) :body (:body c) :author (:author c) :date (:date c)
         :changed-files (count (git/changed-paths dir sha (:parents c)))
         :refusal refusal})
      (catch Throwable t {:error (failure-message t)}))))
