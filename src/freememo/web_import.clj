(ns freememo.web-import
  "Server-side URL import orchestration.

   `import-url!*` fetches a URL, dedupes against an existing topic via the
   sources table, and either creates a new web topic (HTML) or stages binary
   bytes for confirmation (PDF/EPUB).

   `confirm-staged-upload!*` consumes a staged upload (claiming it one-shot)
   and produces the final topic.

   Both fns return the canonical `{:ok true …}` / `{:ok false :error …}`
   shape expected by Forms5 services."
  (:require [freememo.archive :as archive]
            [freememo.audio :as audio]
            [freememo.biblio-import :as biblio-import]
            [freememo.db :as db]
            [freememo.epub :as epub]
            [freememo.image-rehost :as image-rehost]
            [freememo.import-staging :as staging]
            [freememo.logging :as log]
            [taoensso.telemere :as tel]
            [freememo.pdf :as pdf]
            [freememo.quota :as quota]
            [freememo.commands :as commands]
            [freememo.kg-code :as kg-code]
            [freememo.supermemo-import :as sm-import]
            [clojure.string :as str]
            [freememo.wikipedia :as wiki]))

(defn- attach-biblio-best-effort!
  "Best-effort biblio attach — log and swallow exceptions so a biblio
   failure doesn't roll back the topic creation."
  [user-id topic-id biblio]
  (try (biblio-import/prepare-biblio! user-id topic-id biblio)
       (catch Exception e
         (log/log-warn (str "biblio attach failed for topic " topic-id ": " (.getMessage e))))))

(defn import-url!*
  "Fetch a URL and either import as a web topic or stage as binary.
   Pre  : user-id is a logged-in user; url is non-blank.
   Post : returns one of:
            {:ok true :flow :imported       :topic-id N :title S}
            {:ok true :flow :already-exists :topic-id N :title S}
            {:ok true :flow :staged :dispatch :pdf|:epub
                       :upload-id S :filename S :flow-label S}
            {:ok false :error S}.
   Invariant: a topic with the canonical URL is created at most once
   (dedupe via sources.url → topics.source_id)."
  [user-id url]
  (try
    (let [r (wiki/fetch-url url)]
      (cond
        (false? (:success r))
        {:ok false :error (:error r)}

        (:dispatch r)
        (let [{:keys [^bytes bytes filename]} r
              flow (:dispatch r)
              upload-id (staging/stage! user-id bytes filename flow)]
          {:ok true :flow :staged :dispatch flow
           :upload-id upload-id :filename filename
           :flow-label (case flow :pdf "PDF" :epub "EPUB" (name flow))})

        :else
        (let [canonical-url (or (get-in r [:biblio :local :URL]) (:url r) url)
              existing-source (db/find-source-by-url user-id canonical-url)
              existing-topic  (when existing-source
                                (db/find-web-topic-by-source-id user-id (:sources/id existing-source)))]
          (if existing-topic
            {:ok true :flow :already-exists
             :topic-id (:topics/id existing-topic)
             :title (:topics/title existing-topic)}
            (let [html (:html (image-rehost/rehost-html-images!
                                 {:html (:html r) :base-url (:url r) :user-id user-id}))
                  topic-id (db/create-web-topic! user-id (:title r) html
                             {:url (:url r)
                              :source-type (:source-type r)
                              :issued-date-parts (:issued-date-parts r)})]
              (when topic-id
                (attach-biblio-best-effort! user-id topic-id (:biblio r))
                (commands/bump! user-id :import-document))
              {:ok true :flow :imported :topic-id topic-id :title (:title r)})))))
    (catch Exception e
      (tel/error! {:id ::import-url :data {:user-id user-id :url url}} e)
      {:ok false :error (.getMessage e)})))

(defn confirm-staged-upload!*
  "Finalize a staged binary upload into a topic.
   Pre  : user-id owns the staged upload; upload-id is unclaimed; image-mode is :reduce or :strip.
   Post : {:ok true :topic-id N} on success, {:ok false :error S} otherwise.
   Invariant: claim is one-shot — replays return {:ok false}."
  [user-id upload-id image-mode]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes filename flow extra]} entry
            web-biblio (:web-biblio extra)
            image-mode (or image-mode :reduce)]
        (case flow
          :pdf
          (let [result (pdf/save-pdf user-id filename bytes)]
            (if (:success result)
              (let [topic-id (:id result)]
                (attach-biblio-best-effort! user-id topic-id web-biblio)
                (commands/bump! user-id :import-document)
                {:ok true :topic-id topic-id})
              {:ok false :error (:error result)}))

          :epub
          (let [result (epub/process-epub bytes image-mode)]
            (if (:error result)
              {:ok false :error (:error result)}
              (let [{:keys [title chapters]} result
                    display-name (or title filename "Untitled EPUB")
                    file-size (alength bytes)
                    {:keys [topic-id]} (db/create-epub-topic!
                                         user-id display-name bytes file-size chapters)]
                (attach-biblio-best-effort! user-id topic-id web-biblio)
                (commands/bump! user-id :import-document)
                {:ok true :topic-id topic-id})))

          :audio
          (let [result (audio/save-audio! user-id filename bytes (audio/filename->mime filename))]
            (if (:success result)
              (do (commands/bump! user-id :import-document)
                  {:ok true :topic-id (:id result)})
              {:ok false :error (:error result)}))

          {:ok false :error (str "Unknown flow: " flow)}))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-staged-upload!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))

(defn- env-bytes
  "Read a byte count from the environment, falling back to `default`."
  [var-name default]
  (or (some-> (System/getenv var-name) parse-long) default))

(def supermemo-extract-limits
  "Extraction caps for a SuperMemo collection — the one place they are set.

   All three differ from `archive/default-limits`, which are sized for a code
   repo, and all three had to move for a multi-gigabyte collection:

   - `:max-total-bytes` was 1 GB, which a 7.5 GB archive exceeds before it is a
     tenth extracted. This is the cap that binds against DISK, so it is
     env-tunable rather than generous by default — see
     plans/supermemo-import-large-archives.md §6.8 8.3.
   - `:max-entries` was 50 000. A collection stores one file per element
     component, so a large one has hundreds of thousands.
   - `:max-entry-bytes` was 20 MB, which any embedded audio or video slot
     exceeds. Raising it is only safe because `archive/copy-entry!` now streams
     to disk — while it buffered each entry in heap, this cap was the only thing
     between a large member and an OutOfMemoryError.

   A collection is routinely past 150 MB uncompressed even when it is small."
  {:prefix "supermemo-import"
   :max-total-bytes (env-bytes "SUPERMEMO_MAX_EXTRACT_BYTES" (* 16 1024 1024 1024))
   :max-entries (env-bytes "SUPERMEMO_MAX_EXTRACT_ENTRIES" 1000000)
   :max-entry-bytes (env-bytes "SUPERMEMO_MAX_EXTRACT_ENTRY_BYTES" (* 2 1024 1024 1024))})

(defn- archive-too-large-message
  "Turn an extraction cap breach into something the user can act on."
  [e fallback]
  (if (= ::archive/archive-too-large (:type (ex-data e)))
    "Collection archive is too large to import"
    (or (.getMessage ^Exception e) fallback)))

(defn confirm-supermemo-session!*
  "Import a SuperMemo collection from a completed chunked upload session.

   The session-based sibling of `confirm-supermemo-upload!*`. That one reads
   bytes out of `import-staging`, which holds them in JVM heap and therefore
   cannot carry an archive past a few hundred megabytes. This one reads from the
   session's large object, so the archive never enters heap at all.

   Pre  : user-id owns session-id; the session is complete and has flow
          'archive'; the archive holds a SuperMemo collection.
   Post : {:ok true :topic-id N :report {...}} on success, {:ok false :error S}
          otherwise. On SUCCESS the session is released — large object unlinked,
          materialized archive deleted, and every reserved byte refunded,
          because the archive was transport and only the media the import stored
          should stay counted.
   Invariant: on failure the session SURVIVES, so the user can retry without
          re-uploading. The reap sweep collects it if they do not.
   Invariant: the extracted temp dir has exactly one owner — this fn, which
          deletes it on every path."
  [user-id session-id]
  (try
    (let [m (db/materialize-upload-archive! user-id session-id)]
      (if-not (:ok m)
        m
        (let [dir (archive/extract-to-temp-dir! (:file m) supermemo-extract-limits)]
          (try
            (let [r (sm-import/import-collection! user-id dir)]
              (when (:ok r)
                (commands/bump! user-id :import-supermemo)
                (db/release-upload-session! user-id session-id))
              r)
            (finally
              (archive/delete-dir! dir))))))
    (catch Exception e
      (log/log-error (str "confirm-supermemo-session!* failed: " (.getMessage e)))
      {:ok false :error (archive-too-large-message e "Import failed")})))

(defn confirm-repo-session!*
  "Import a code repository from a completed chunked upload session.

   Pre  : user-id owns session-id; the session is complete and has flow
          'archive'; the archive holds a code repository.
   Post : {:ok true :topic-id N} on success, {:ok false :error S} otherwise. On
          SUCCESS the session is released, as in `confirm-supermemo-session!*`.
   Invariant: the extracted dir has one owner — `start-repo-distill!` when
          extract-facts? is true, else this fn."
  [user-id session-id extract-facts?]
  (try
    (let [m (db/materialize-upload-archive! user-id session-id)]
      (if-not (:ok m)
        m
        (let [dir (kg-code/unzip-repo! (:file m))]
          (try
            (let [repo-name (str/replace (or (:filename m) "repo") #"(?i)\.(zip|7z)$" "")
                  {:keys [root-id]} (kg-code/create-repo-topics! user-id repo-name dir)]
              (if extract-facts?
                (kg-code/start-repo-distill! user-id root-id dir)
                (kg-code/delete-dir! dir))
              (commands/bump! user-id :import-document)
              (db/release-upload-session! user-id session-id)
              {:ok true :topic-id root-id})
            (catch Throwable t
              (kg-code/delete-dir! dir)
              (throw t))))))
    (catch Exception e
      (log/log-error (str "confirm-repo-session!* failed: " (.getMessage e)))
      {:ok false :error (archive-too-large-message e "Import failed")})))

(defn confirm-repo-upload!*
  "Finalize a staged code-repo (.zip) upload into a topic tree, optionally
   distilling knowledge-graph facts.
   Pre  : user-id owns the staged upload; upload-id is unclaimed; the entry's
          flow is :repo.
   Post : {:ok true :topic-id N} on success, {:ok false :error S} otherwise.
   Invariant: claim is one-shot — replays return {:ok false}. The unzip temp
          dir has exactly one owner: start-repo-distill! when extract-facts?,
          else this fn deletes it. Fact distillation is opt-in — when
          extract-facts? is false, topics are created with no kg_facts."
  [user-id upload-id extract-facts?]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes filename flow]} entry]
        (if (not= :repo flow)
          {:ok false :error (str "Not a code repository: " flow)}
          ;; Topics are created synchronously (fast — caller navigates to the
          ;; root immediately). When facts are requested, start-repo-distill!
          ;; takes ownership of the temp dir and deletes it when the async run
          ;; ends; otherwise we delete it here. On topic-creation failure the
          ;; dir has no owner yet, so delete it before re-throwing.
          (let [dir (kg-code/unzip-repo! bytes)]
            (try
              (let [repo-name (str/replace (or filename "repo") #"(?i)\.zip$" "")
                    {:keys [root-id]} (kg-code/create-repo-topics! user-id repo-name dir)]
                (if extract-facts?
                  (kg-code/start-repo-distill! user-id root-id dir)
                  (kg-code/delete-dir! dir))
                (commands/bump! user-id :import-document)
                {:ok true :topic-id root-id})
              (catch Throwable t
                (kg-code/delete-dir! dir)
                (throw t))))))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-repo-upload!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))

(defn confirm-supermemo-upload!*
  "Finalize a staged SuperMemo collection (.zip) into a topic tree.
   Pre  : user-id owns the staged upload; upload-id is unclaimed; the entry's
          flow is :supermemo.
   Post : {:ok true :topic-id N :report {...}} on success, {:ok false :error S}
          otherwise. The report accounts for everything the import did not
          carry over and is the caller's to surface.
   Invariant: claim is one-shot — replays return {:ok false}. The extracted
          temp dir has exactly one owner: this fn, which deletes it on every
          path. The import is synchronous because the caller navigates to the
          returned root as soon as it commits."
  [user-id upload-id]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes flow]} entry]
        (if (not= :supermemo flow)
          {:ok false :error (str "Not a SuperMemo collection: " flow)}
          (let [dir (archive/extract-to-temp-dir! bytes supermemo-extract-limits)]
            (try
              (let [r (sm-import/import-collection! user-id dir)]
                (when (:ok r) (commands/bump! user-id :import-supermemo))
                r)
              (finally
                (archive/delete-dir! dir))))))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-supermemo-upload!* failed: " (.getMessage e)))
      {:ok false :error (archive-too-large-message e "Import failed")})))

(defn confirm-score-upload!*
  "Finalize a Score import from TWO staged uploads (sheet-music PDF + recording).
   Pre  : user-id owns both staged uploads; the pdf entry has flow :pdf and the
          audio entry flow :audio.
   Post : {:ok true :topic-id N} or {:ok false :error S}. Claims are one-shot;
          a failed pair leaves neither entry reusable (restaging is a re-upload).
   The topic title comes from the PDF filename — the score names the piece."
  [user-id pdf-upload-id audio-upload-id]
  (try
    (let [pdf-entry (staging/claim! user-id pdf-upload-id)
          audio-entry (staging/claim! user-id audio-upload-id)]
      (cond
        (nil? pdf-entry) {:ok false :error "PDF upload not found or expired"}
        (nil? audio-entry) {:ok false :error "Audio upload not found or expired"}
        (not= :pdf (:flow pdf-entry)) {:ok false :error "Sheet-music file is not a PDF"}
        (not= :audio (:flow audio-entry)) {:ok false :error "Recording is not an audio file"}
        :else
        (let [topic (db/create-score-topic! user-id (:filename pdf-entry)
                      (:bytes pdf-entry) (:bytes audio-entry)
                      (audio/filename->mime (:filename audio-entry)))]
          (commands/bump! user-id :import-document)
          {:ok true :topic-id (:topics/id topic)})))
    (catch clojure.lang.ExceptionInfo e
      (log/log-error (str "confirm-score-upload!* failed: " (.getMessage e)))
      {:ok false :error (if (quota/quota-error? (ex-data e))
                          (case (:reason (ex-data e))
                            :file-too-large "File exceeds the per-file storage limit"
                            "Storage quota exceeded — delete documents to free space")
                          (.getMessage e))})
    (catch Exception e
      (log/log-error (str "confirm-score-upload!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))
