(ns freememo.web-import
  "Server-side URL import orchestration.

   `import-url!*` fetches a URL, dedupes against an existing topic via the
   sources table, and either creates a new web topic (HTML) or stages binary
   bytes for confirmation (PDF/EPUB).

   `confirm-staged-upload!*` consumes a staged upload (claiming it one-shot)
   and produces the final topic.

   Both fns return the canonical `{:ok true …}` / `{:ok false :error …}`
   shape expected by Forms5 services."
  (:require [freememo.audio :as audio]
            [freememo.biblio-import :as biblio-import]
            [freememo.db :as db]
            [freememo.epub :as epub]
            [freememo.image-rehost :as image-rehost]
            [freememo.import-staging :as staging]
            [freememo.logging :as log]
            [freememo.pdf :as pdf]
            [freememo.quota :as quota]
            [freememo.commands :as commands]
            [freememo.kg-code :as kg-code]
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
      (log/log-error (str "import-url!* failed: " (.getMessage e)))
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

          :repo
          ;; Topics are created synchronously (fast — caller navigates to the
          ;; root immediately); fact distillation runs async and owns/deletes
          ;; the temp dir. If topic creation fails before the async job starts,
          ;; delete the dir here — ownership hasn't transferred yet.
          (let [dir (kg-code/unzip-repo! bytes)]
            (try
              (let [repo-name (str/replace (or filename "repo") #"(?i)\.zip$" "")
                    {:keys [root-id]} (kg-code/create-repo-topics! user-id repo-name dir)]
                (kg-code/start-repo-distill! user-id root-id dir)
                (commands/bump! user-id :import-document)
                {:ok true :topic-id root-id})
              (catch Throwable t
                (kg-code/delete-dir! dir)
                (throw t))))

          {:ok false :error (str "Unknown flow: " flow)}))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-staged-upload!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))

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
