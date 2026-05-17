(ns freememo.biblio-import
  "Post-import orchestrator: load the just-imported artifact, run the right
   harvester, resolve any extracted identifier, merge, and persist to
   sources.csl. Also marks the topic so the viewer auto-opens the
   bibliography modal on its next mount."
  (:require [freememo.db :as db]
            [freememo.biblio-pdf :as bp]
            [freememo.biblio-merge :as bm]
            [freememo.biblio-resolver :as br]
            [freememo.epub :as epub]
            [taoensso.telemere :as tel]))

;; Per-user set of topic-ids whose viewer should auto-open the biblio modal.
;; Read-and-clear semantics in `claim-show?` makes this a one-shot signal.
(defonce ^:private !pending-shows (atom {}))

(defn mark-for-show!
  "Pre:  user-id and topic-id non-nil.
   Post: (claim-show? user-id topic-id) returns true exactly once."
  [user-id topic-id]
  (when (and user-id topic-id)
    (swap! !pending-shows update user-id (fnil conj #{}) topic-id)))

(defn claim-show?
  "Pre:  user-id and topic-id non-nil.
   Post: returns true iff topic-id was previously marked for this user; the
         mark is removed on hit. Subsequent calls return false."
  [user-id topic-id]
  (let [hit (volatile! false)]
    (swap! !pending-shows
      (fn [m]
        (if (contains? (get m user-id #{}) topic-id)
          (do (vreset! hit true)
              (update m user-id disj topic-id))
          m)))
    @hit))

(defn- topic-csl-type [kind]
  (case kind
    "pdf"       "document"
    "epub"      "book"
    "wikipedia" "webpage"
    "web"       "webpage"
    "markdown"  "document"
    "webpage"))

(defn- harvest-for-kind
  "Dispatch the right local harvester. Returns {:local <csl> :identifiers
   <map>} or {:local {} :identifiers {}}."
  [topic file-bytes web-biblio]
  (let [kind (:topics/kind topic)]
    (or
      (case kind
        "pdf"  (when file-bytes (bp/harvest-pdf file-bytes))
        "epub" (when file-bytes (epub/extract-biblio file-bytes))
        ("web" "wikipedia") web-biblio
        nil)
      {:local {} :identifiers {}})))

(defn- non-blank-ids [ids]
  (into {} (filter (fn [[_ v]] (and v (not (and (string? v) (empty? v))))) (or ids {}))))

(defn prepare-biblio!
  "Harvest + resolve + merge biblio for a just-imported topic; persist to
   sources.csl; mark the topic for modal auto-show.

   Pre:  user-id and topic-id non-nil; topic exists and is owned by user.
         web-biblio (optional) is the {:local :identifiers} captured at
         URL fetch time, since topics.content holds post-cleaning HTML.
   Post: {:ok true :source-id N} on success;
         {:ok false :error msg} on any failure.
   Invariant: idempotent — re-running overwrites the merged CSL and is safe.
   Blame: caller bug if user-id or topic-id is nil; implementation bug
   otherwise — resolver failures are absorbed (biblio just lacks resolved
   fields)."
  ([user-id topic-id]
   (prepare-biblio! user-id topic-id nil))
  ([user-id topic-id web-biblio]
   (try
     (let [topic (db/get-topic-for-user user-id topic-id)]
       (when-not topic
         (throw (ex-info "Topic not found" {:topic-id topic-id :user-id user-id})))
       (let [kind         (:topics/kind topic)
             file-bytes   (when (#{"pdf" "epub"} kind)
                            (some-> (db/get-topic-file topic-id) :topic_files/file_data))
             harvest      (harvest-for-kind topic file-bytes web-biblio)
             ids          (non-blank-ids (:identifiers harvest))
             resolved     (when (seq ids)
                            (try
                              (let [r (br/resolve! ids)]
                                (when (:csl r)
                                  (tel/log! {:level :info :id ::resolved
                                             :data {:resolver (:resolver r)
                                                    :topic-id topic-id}}
                                    "biblio resolved via resolver")
                                  (:csl r)))
                              (catch Exception e
                                (tel/log! {:level :warn :id ::resolver-error}
                                  (.getMessage e))
                                nil)))
             current-sid  (:topics/source_id topic)
             existing-csl (when current-sid
                            (:sources/csl (db/get-source current-sid)))
             merged       (bm/merge-biblio
                            {:local    (merge existing-csl (:local harvest))
                             :resolved resolved})
             csl-type     (or (:type merged) (topic-csl-type kind))
             url          (or (:URL merged) (:url existing-csl))
             title        (or (:title merged) (:topics/title topic))
             result       (if current-sid
                            (do (db/update-source! {:id current-sid
                                                    :csl-type csl-type
                                                    :csl merged
                                                    :url url
                                                    :title title})
                                {:ok true :source-id current-sid})
                            (let [new-src (db/create-source!
                                            {:user-id user-id
                                             :csl-type csl-type
                                             :csl merged
                                             :url url
                                             :title title})
                                  sid     (:sources/id new-src)]
                              (db/attach-source-to-topic! user-id topic-id sid)
                              {:ok true :source-id sid}))]
         (mark-for-show! user-id topic-id)
         result))
     (catch Exception e
       (tel/error! {:id ::prepare-biblio :data {:topic-id topic-id}} e)
       {:ok false :error (.getMessage e)}))))
