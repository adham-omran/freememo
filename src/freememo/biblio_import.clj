(ns freememo.biblio-import
  "Post-import orchestrator: load the just-imported artifact, run the right
   harvester, resolve any extracted identifier, merge, and persist to
   sources.csl. Also marks the topic so the viewer auto-opens the
   bibliography modal on its next mount."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [freememo.biblio-pdf :as bp]
            [freememo.biblio-merge :as bm]
            [freememo.biblio-resolver :as br]
            [freememo.epub :as epub]
            [freememo.user-state :as us]
            [freememo.commands :as commands]
            [freememo.wikipedia :as wikipedia]
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
    "audio"     "song"
    "webpage"))

(defn- non-empty-biblio?
  "True when web-biblio carries any local CSL fields or identifiers.
   An empty {:local {} :identifiers {}} is treated as 'nothing supplied',
   so the PDF/EPUB harvesters still run as a fallback."
  [web-biblio]
  (boolean
    (and (map? web-biblio)
         (or (seq (:local web-biblio))
             (seq (:identifiers web-biblio))))))

(defn- harvest-for-kind
  "Dispatch the right local harvester. Returns {:local <csl> :identifiers
   <map>} or {:local {} :identifiers {}}.

   For pdf/epub topics with a non-empty `web-biblio` supplied by the
   importer (e.g. Zotero CSL-JSON), the supplied biblio wins and the
   on-disk harvesters are skipped — preserves curated metadata."
  [topic file-bytes web-biblio]
  (let [kind (:topics/kind topic)]
    (or
      (case kind
        "pdf"  (if (non-empty-biblio? web-biblio)
                 web-biblio
                 (when file-bytes (bp/harvest-pdf file-bytes)))
        "epub" (if (non-empty-biblio? web-biblio)
                 web-biblio
                 (when file-bytes (epub/extract-biblio file-bytes)))
        ("web" "wikipedia") web-biblio
        nil)
      {:local {} :identifiers {}})))

(defn- non-blank-ids [ids]
  (into {} (filter (fn [[_ v]] (and v (not (and (string? v) (empty? v))))) (or ids {}))))

(defn- arxiv-from-csl
  "Pre:  csl is a map (possibly empty) representing a CSL-JSON record.
   Post: arXiv id string when extractable from :URL (arxiv.org/abs|pdf/<id>)
   or :DOI (10.48550/arXiv.<id>); nil otherwise."
  [csl]
  (or (when-let [u (:URL csl)]
        (some-> (re-find #"(?i)arxiv\.org/(?:abs|pdf)/([^?#]+)" (str u)) second))
      (when-let [d (:DOI csl)]
        (some-> (re-find #"(?i)10\.48550/arXiv\.(.+)" (str d)) second))))

(defn- isbn-buckets
  "Pre:  csl is a map.
   Post: {:isbn-13 s} when (:ISBN csl) is 13 alphanum chars,
         {:isbn-10 s} when 10 chars, nil otherwise."
  [csl]
  (when-let [raw (:ISBN csl)]
    (let [digits (str/replace (str raw) #"[^0-9Xx]" "")]
      (cond
        (= 13 (count digits)) {:isbn-13 digits}
        (= 10 (count digits)) {:isbn-10 digits}
        :else nil))))

(defn- ids-from-csl
  "Pre:  csl is a map.
   Post: resolver-shaped identifier map {:doi :arxiv :isbn-13 :isbn-10},
         keys present only when their value is non-blank."
  [csl]
  (let [csl (or csl {})]
    (cond-> {}
      (seq (str (:DOI csl)))   (assoc :doi (:DOI csl))
      (arxiv-from-csl csl)     (assoc :arxiv (arxiv-from-csl csl))
      :always                  (merge (isbn-buckets csl)))))

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

(defn refetch-biblio!
  "Re-resolve and merge bibliography for a topic that already has a source row.
   For PDF/EPUB topics, re-runs the on-disk harvester.
   For web/wikipedia topics, re-uses identifiers already stored in
   sources.csl — does NOT re-fetch the source URL.

   Pre:  user-id and topic-id non-nil; topic exists and is owned by user.
   Post: {:ok true :source-id N :resolver kw-or-nil} when the existing
         sources row was rewritten (no-op rewrite is still :ok; :resolver is
         nil when no identifier was present to dispatch on);
         {:ok false :error :no-source} when topic.source_id is nil;
         {:ok false :error msg} on any other failure.
   Invariant: never marks the topic for modal auto-show; bumps :refresh on
   success. Resolver failures are absorbed (merged CSL just lacks the
   resolved fields). Existing CSL fields are preserved on no-op (empty
   resolved + empty harvest local does not clobber)."
  [user-id topic-id]
  (tel/log! {:level :info :id ::refetch-start :data {:user-id user-id :topic-id topic-id}}
    "biblio refetch click")
  (try
    (let [topic (db/get-topic-for-user user-id topic-id)]
      (when-not topic
        (throw (ex-info "Topic not found" {:topic-id topic-id :user-id user-id})))
      (let [current-sid (:topics/source_id topic)]
        (if-not current-sid
          (do (tel/log! {:level :info :id ::refetch-no-source
                         :data {:topic-id topic-id}}
                "refetch: topic has no source row")
              {:ok false :error :no-source})
          (let [kind         (:topics/kind topic)
                source-row   (db/get-source current-sid)
                existing-csl (or (:sources/csl source-row) {})
                file-bytes   (when (#{"pdf" "epub"} kind)
                               (some-> (db/get-topic-file topic-id)
                                 :topic_files/file_data))
                url-for-fetch (or (:sources/url source-row)
                                  (:URL existing-csl)
                                  (:topics/source_url topic))
                harvest      (or (case kind
                                   "pdf"  (when file-bytes (bp/harvest-pdf file-bytes))
                                   "epub" (when file-bytes (epub/extract-biblio file-bytes))
                                   ("web" "wikipedia")
                                   (if url-for-fetch
                                     (try
                                       (let [r (wikipedia/fetch-url url-for-fetch)]
                                         (if (:success r)
                                           (do (tel/log! {:level :info :id ::refetch-web
                                                          :data {:url url-for-fetch
                                                                 :topic-id topic-id}}
                                                 "web/wiki refetch ok")
                                               (:biblio r))
                                           (do (tel/log! {:level :warn :id ::refetch-web-fail
                                                          :data {:url url-for-fetch
                                                                 :error (:error r)
                                                                 :topic-id topic-id}}
                                                 "web/wiki refetch failed")
                                               {:local {} :identifiers (ids-from-csl existing-csl)})))
                                       (catch Exception e
                                         (tel/log! {:level :warn :id ::refetch-web-error
                                                    :data {:topic-id topic-id}}
                                           (.getMessage e))
                                         {:local {} :identifiers (ids-from-csl existing-csl)}))
                                     {:local {} :identifiers (ids-from-csl existing-csl)})
                                   nil)
                               {:local {} :identifiers {}})
                ids          (non-blank-ids (:identifiers harvest))
                resolver-result (when (seq ids)
                                  (try (br/resolve! ids)
                                       (catch Exception e
                                         (tel/log! {:level :warn :id ::refetch-resolver-error}
                                           (.getMessage e))
                                         nil)))
                resolved     (:csl resolver-result)
                _            (tel/log! {:level :info :id ::refetch-resolved
                                        :data {:kind kind
                                               :ids ids
                                               :resolver (:resolver resolver-result)
                                               :resolver-status (when (and (seq ids) (nil? resolved))
                                                                  (or (:status resolver-result) :no-csl))
                                               :topic-id topic-id}}
                               "biblio refetch resolve")
                merged       (bm/merge-biblio
                               {:local    (merge existing-csl (:local harvest))
                                :resolved resolved})
                csl-type     (or (:type merged) (topic-csl-type kind))
                url          (or (:URL merged) (:url existing-csl))
                title        (or (:title merged) (:topics/title topic))]
            (db/update-source! {:id current-sid :csl-type csl-type
                                :csl merged :url url :title title})
            (commands/bump! user-id :refetch-biblio)
            {:ok true :source-id current-sid
             :resolver (:resolver resolver-result)}))))
    (catch Exception e
      (tel/error! {:id ::refetch-biblio :data {:topic-id topic-id}} e)
      {:ok false :error (.getMessage e)})))
