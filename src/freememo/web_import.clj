(ns freememo.web-import
  "Server-side URL import orchestration.

   `import-url!*` fetches a URL, dedupes against an existing topic via the
   sources table, and either creates a new web topic (HTML) or stages binary
   bytes for confirmation (PDF/EPUB).

   `confirm-staged-upload!*` consumes a staged upload (claiming it one-shot)
   and produces the final topic.

   Both fns return the canonical `{:ok true …}` / `{:ok false :error …}`
   shape expected by Forms5 services."
  (:require [freememo.biblio-import :as biblio-import]
            [freememo.db :as db]
            [freememo.epub :as epub]
            [freememo.import-staging :as staging]
            [freememo.logging :as log]
            [freememo.pdf :as pdf]
            [freememo.user-state :as us]
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
            (let [topic-id (db/create-web-topic! user-id (:title r) (:html r)
                             {:url (:url r)
                              :source-type (:source-type r)
                              :issued-date-parts (:issued-date-parts r)})]
              (when topic-id
                (attach-biblio-best-effort! user-id topic-id (:biblio r))
                (swap! (us/get-atom user-id :refresh) inc)
                (swap! (us/get-atom user-id :tree-mutations) inc))
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
                (swap! (us/get-atom user-id :refresh) inc)
                (swap! (us/get-atom user-id :tree-mutations) inc)
                (attach-biblio-best-effort! user-id topic-id web-biblio)
                {:ok true :topic-id topic-id})))

          {:ok false :error (str "Unknown flow: " flow)}))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-staged-upload!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))
