(ns freememo.web-import
  "Server-side URL import orchestration.

   `import-url!*` fetches a URL, dedupes against an existing topic via the
   sources table, and either creates a new web topic (HTML) or stages binary
   bytes for confirmation (PDF/EPUB).

   `confirm-staged-upload!*` consumes a staged upload (claiming it one-shot)
   and produces the final topic.

   Both fns return the canonical `{:ok true …}` / `{:ok false :error …}`
   shape expected by Forms5 services."
  (:require [freememo.account-import :as account-import]
            [freememo.archive :as archive]
            [freememo.audio :as audio]
            [freememo.biblio-import :as biblio-import]
            [freememo.csl :as csl]
            [freememo.db :as db]
            [freememo.epub :as epub]
            [freememo.image-rehost :as image-rehost]
            [freememo.page-pipeline :as pipeline]
            [freememo.text :as text]
            [freememo.toasts :as toasts]
            [freememo.user-state :as us]
            [freememo.import-staging :as staging]
            [freememo.logging :as log]
            [taoensso.telemere :as tel]
            [freememo.pdf :as pdf]
            [freememo.quota :as quota]
            [freememo.commands :as commands]
            [freememo.kg-code :as kg-code]
            [freememo.sphinx-import :as sphinx-import]
            [freememo.supermemo-import :as sm-import]
            [clojure.string :as str]
            [freememo.wikipedia :as wiki]))

(defn find-existing-topic-for-url*
  "The topic already holding `url`, or nil.

   Pre : `url` is absolute and fragment-stripped — `link-target` strips it, and
         it must, or `page.html#a` and `page.html#b` would be two topics.
   Post: {:topic-id N :title S} or nil.
   Inv : one indexed lookup on (user_id, url) plus one on source_id. Cheap
         enough to run on the click path, which is why the duplicate answer is
         immediate while a real import is not."
  [user-id url]
  (when-let [src (db/find-source-by-url user-id url)]
    (when-let [t (db/find-web-topic-by-source-id user-id (:sources/id src))]
      {:topic-id (:topics/id t) :title (:topics/title t)})))

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

(defn start-url-import!*
  "Answer a link Import without waiting for the network.

   Pre  : `user-id` is logged in; `url` is absolute and fragment-stripped.
   Post : returns immediately, one of
            {:flow :already-exists :topic-id N :title S}
            {:flow :queued}
            {:flow :busy}
          and NEVER {:flow :imported} — the fetch has not happened yet.
   Inv  : the duplicate answer is synchronous and the fetch is not. A reader
          clicking a link is answered at once when the page is already held,
          and handed off when it is not; nothing makes them wait on a network
          round trip inside their editor.
   Inv  : membership in `:importing-urls` is published for the whole extent of
          the fetch, so `activity-reporter` discovers the row, keeps it fresh
          and closes it. `us/with-in-flight` owns the `finally`.
   Inv  : the outcome reaches the user as a toast, because by the time it
          lands they may be reading something else. `import-url!*` owns the
          import itself; this owns only when it runs and who is told."
  [user-id url]
  (if-let [existing (find-existing-topic-for-url* user-id url)]
    (assoc existing :flow :already-exists)
    (let [in-flight (us/get-atom user-id :importing-urls)]
      (if (contains? @in-flight url)
        ;; A second click on the same link joins the first rather than racing
        ;; it — two fetches would produce two topics for one page.
        {:flow :busy}
        (do
          (future
            (us/with-in-flight user-id :importing-urls url
              (let [r (try (import-url!* user-id url)
                           (catch Throwable e
                             (tel/error! {:id ::start-url-import :data {:user-id user-id :url url}} e)
                             {:ok false :error (.getMessage e)}))
                    errors (us/get-atom user-id :url-import-errors)]
                ;; Record the outcome BEFORE `with-in-flight`'s finally drops
                ;; membership: that is when the reporter closes the row and
                ;; reads `:error-fn`. Recorded after, the row is already closed
                ;; as a success.
                (if (:ok r)
                  (swap! errors dissoc url)
                  (swap! errors assoc url (or (:error r) "Import failed")))
                (cond
                  (and (:ok r) (:topic-id r))
                  (toasts/push! user-id
                    {:level :success
                     :message (str "Imported \"" (:title r) "\"")
                     :actions [{:label "Open"
                                :nav [:viewer {:type :topic :topic-id (:topic-id r)}]}]})

                  ;; A PDF or EPUB needs a confirmation step this path cannot
                  ;; drive, so it is reported rather than silently staged.
                  (= :staged (:flow r))
                  (toasts/push! user-id
                    {:level :info
                     :message (str (:flow-label r) " downloaded — open Import to finish it.")
                     :actions [{:label "Import" :nav :import}]})

                  :else
                  (toasts/push! user-id
                    {:level :error
                     :message (str "Could not import " url
                               (when (:error r) (str " — " (:error r))))})))))
          {:flow :queued})))))

(defn capture-page!*
  "Store a page the browser serialized, which the server cannot fetch itself.

   Pre  : user-id is a logged-in user. `html` is a serialized browser DOM, not
          yet sanitized. `url` is the page's address. `csl` is caller-supplied
          CSL-JSON, or nil.
   Post : returns one of:
            {:ok true :flow :captured       :topic-id N :title S}
            {:ok true :flow :already-exists :topic-id N :title S}
            {:ok false :error S}.
   Inv  : within one source, a capture whose stored text repeats an existing
          capture's creates no second topic, unless `version?` is set.

   Three differences from `import-url!*`, all of them because the browser did
   the fetching.

   The pipeline runs with `:browser-dom?`, so the constructs Quill drops are
   rewritten before they can be stored and flattened by the first edit.

   De-duplication compares CONTENT, not the URL alone. A URL identifies a page,
   but a capture is a page at a moment, and the same URL legitimately yields a
   selection, a re-read, and a later revision. `find-topic-by-content-digest`
   answers the question the user actually asks — is this already here.

   Images are re-hosted from `data:` URIs, never fetched. The extension inlines
   them precisely because the server cannot reach them, so
   `rehost-html-images!` would 403 on exactly the pages this path exists for."
  [user-id {:keys [html url csl version?]}]
  (try
    (cond
      (str/blank? html)
      {:ok false :error "Nothing to capture" :blame :caller}

      ;; Without a URL there is no source row, and without a source row there
      ;; is nothing to deduplicate against — every capture would silently
      ;; become a new root. Refusing is honest; succeeding quietly is not.
      (str/blank? url)
      {:ok false :error "A capture needs the page address" :blame :caller}

      :else
      (let [page (pipeline/page->document html url {:browser-dom? true})
            canonical-url (or (get-in page [:biblio :local :URL]) url)
            source (db/ensure-source-for-url! user-id canonical-url
                     {:source-type (:source-type page)
                      :title (:title page)
                      :issued-date-parts (:issued-date-parts page)})
            source-id (:sources/id source)
            stored-html (:html (image-rehost/rehost-data-uris!
                                 {:html (:html page) :user-id user-id}))
            content-text (text/strip-html stored-html)
            existing (when-not version?
                       (db/find-topic-by-content-digest user-id source-id content-text))]
        (if existing
          {:ok true :flow :already-exists
           :topic-id (:topics/id existing)
           :title (:topics/title existing)}
          (let [topic-id (db/create-web-topic! user-id (:title page) stored-html
                           {:url canonical-url
                            :source-type (:source-type page)
                            :issued-date-parts (:issued-date-parts page)})]
            (when topic-id
              (attach-biblio-best-effort! user-id topic-id
                (if csl (csl/csljson->web-biblio csl) (:biblio page)))
              (commands/bump! user-id :import-document))
            {:ok true :flow :captured :topic-id topic-id :title (:title page)}))))
    (catch Exception e
      (tel/error! {:id ::capture-page :data {:user-id user-id :url url}} e)
      {:ok false :error (.getMessage e)})))

(defn existing-pdf-capture
  "The user's PDF topic for `url`, if a capture of that page already exists.

   Pre : user-id non-nil. `url` is a page address, or nil.
   Post: {:topics/id :topics/title} or nil.
   Inv : reads only. A caller uses this to avoid staging bytes it will discard.

   Content de-duplication cannot serve a screenshot. `content_text` is empty
   until the user runs OCR, so every screenshot would digest alike. The URL is
   the whole of a screenshot's identity, so the URL is what this matches."
  [user-id url]
  (when (and user-id (not (str/blank? url)))
    (when-let [source (db/find-source-by-url user-id url)]
      (db/find-pdf-topic-by-source-id user-id (:sources/id source)))))

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
                ;; `:biblio` rides along from the parse that just ran, so this
                ;; costs no second read of the blob we just wrote. A supplied
                ;; web-biblio still wins — it is curated, ours is derived.
                (attach-biblio-best-effort! user-id topic-id
                  (or web-biblio (:biblio result)))
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

(defn confirm-epub-session!*
  "Turn a completed chunked EPUB session into an `epub` topic and its chapters.

   The session-based sibling of `confirm-staged-upload!*`'s `:epub` branch. That
   one reads bytes out of `import-staging`, which holds them in JVM heap and so
   cannot carry a book past a few hundred megabytes. This one parses from the
   session's materialized file, so the book never enters heap as an array.

   Pre  : user-id owns session-id; the session is complete and has flow 'epub';
          `finalize` has already validated its magic. image-mode is :reduce or
          :strip.
   Post : {:ok true :topic-id N} on success, {:ok false :error S} otherwise. On
          SUCCESS the session row is deleted by `finalize-epub-session!` and its
          large object becomes the topic's — the bytes are NOT copied and NOT
          refunded, because the book is what the user now stores.
   Invariant: on failure the session SURVIVES, so the user can retry without
          re-uploading. The reap sweep collects it if they do not.
   Invariant: `:biblio` from the parse is handed straight to the biblio step, so
          the book is read once and never re-read from storage."
  [user-id session-id image-mode]
  (try
    (let [m (db/materialize-upload-session! user-id session-id "epub")]
      (if-not (:ok m)
        m
        (let [result (epub/process-epub (:file m) (or image-mode :reduce))]
          (if (:error result)
            {:ok false :error (:error result)}
            (let [{:keys [title chapters biblio]} result
                  display-name (or title (:filename m) "Untitled EPUB")
                  r (db/finalize-epub-session! user-id session-id display-name chapters)]
              (if-not (:ok r)
                r
                (do
                  (attach-biblio-best-effort! user-id (:topic-id r) biblio)
                  (commands/bump! user-id :import-document)
                  {:ok true :topic-id (:topic-id r)})))))))
    (catch Exception e
      (log/log-error (str "confirm-epub-session!* failed: " (.getMessage e)))
      {:ok false :error (.getMessage e)})))

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

(def account-extract-limits
  "Extraction caps for an account transfer archive
   (plans/user-data-export-import.md §5.6.3).

   `archive/default-limits` are sized for a code repo and every one of the three
   binds well below a real account: 300 MB total against a corpus that carries
   PDFs and video, 20 MB per entry against a single video large object, and
   50 000 entries against one file per blob. `:max-total-bytes` is the cap that
   binds against DISK, so it is env-tunable rather than generous by default —
   same reasoning as `supermemo-extract-limits`."
  {:prefix "account-import"
   :max-total-bytes (env-bytes "ACCOUNT_MAX_EXTRACT_BYTES" (* 32 1024 1024 1024))
   :max-entries (env-bytes "ACCOUNT_MAX_EXTRACT_ENTRIES" 200000)
   :max-entry-bytes (env-bytes "ACCOUNT_MAX_EXTRACT_ENTRY_BYTES" (* 8 1024 1024 1024))})

(defn confirm-account-session!*
  "Import an account export from a completed chunked upload session.

   The account sibling of `confirm-supermemo-session!*`, and it reads from the
   session's large object for the same reason: an export carrying video cannot
   enter heap.

   Pre  : user-id owns session-id; the session is complete and has flow
          'archive'; the archive holds a FreeMemo account export.
          `parent-topic-id` is a topic user-id owns, or nil for the tree root.
   Post : {:ok true :import-id N :resumed? bool :report {...}} on success,
          {:ok false :error S} otherwise. On SUCCESS the session is released —
          large object unlinked, materialized archive deleted, and every reserved
          byte refunded, because the archive was transport and only what the
          import stored should stay counted.
   Invariant: on failure the session SURVIVES, so a retry needs no re-upload.
          That is what makes a resume possible after a blob write fails.
   Invariant: the extracted temp dir has exactly one owner — this fn, which
          deletes it on every path."
  [user-id session-id parent-topic-id allow-second-copy?]
  (try
    (let [m (db/materialize-upload-archive! user-id session-id)]
      (if-not (:ok m)
        m
        (let [dir (archive/extract-to-temp-dir! (:file m) account-extract-limits)]
          (try
            (let [r (account-import/import-archive! db/ds user-id dir parent-topic-id
                      {:allow-second-copy? allow-second-copy?})]
              (when (:ok r)
                (commands/bump! user-id :import-account)
                (db/release-upload-session! user-id session-id))
              r)
            (finally
              (archive/delete-dir! dir))))))
    (catch Exception e
      (log/log-error (str "confirm-account-session!* failed: " (.getMessage e)))
      {:ok false :error (archive-too-large-message e "Import failed")})))

(def sphinx-extract-limits
  "Extraction caps for a Sphinx HTML documentation build.

   Two of the three differ from `archive/default-limits`, which are sized for a
   code repo:

   - `:max-total-bytes` was 300 MB. The reference corpus (the Blender manual)
     carries 254 MB of images alone before a byte of HTML, and it is one manual.
   - `:max-entries` was 50 000. A build writes one file per page plus one per
     image; the reference corpus is 2361 pages and 3138 images, and a larger
     project's API reference multiplies the page count.
   - `:max-entry-bytes` stays at the default. No single page or figure in a
     documentation build approaches 20 MB, and a file that does is not one this
     import can use.

   `:keep?` is what makes those numbers hold: `sphinx-import/extract-keep?`
   drops the theme's JavaScript and the search index, which is 1.7 MB of the
   reference corpus and unreachable from any topic."
  {:prefix "sphinx-import"
   :keep? sphinx-import/extract-keep?
   :max-total-bytes (env-bytes "SPHINX_MAX_EXTRACT_BYTES" (* 4 1024 1024 1024))
   :max-entries (env-bytes "SPHINX_MAX_EXTRACT_ENTRIES" 200000)
   :max-entry-bytes (env-bytes "SPHINX_MAX_EXTRACT_ENTRY_BYTES" (* 20 1024 1024))})

(defn confirm-sphinx-session!*
  "Import a Sphinx HTML documentation build from a completed chunked upload.

   Pre  : user-id owns session-id; the session is complete and has flow
          'archive'; the archive holds a Sphinx build.
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
        (let [dir (archive/extract-to-temp-dir! (:file m) sphinx-extract-limits)]
          (try
            (let [r (sphinx-import/import-tree! user-id dir)]
              (when (:ok r)
                (commands/bump! user-id :import-document)
                (db/release-upload-session! user-id session-id))
              r)
            (finally
              (archive/delete-dir! dir))))))
    (catch Exception e
      (log/log-error (str "confirm-sphinx-session!* failed: " (.getMessage e)))
      {:ok false :error (archive-too-large-message e "Import failed")})))

(defn confirm-sphinx-upload!*
  "Finalize a staged Sphinx documentation build (.zip) into a topic tree.

   The staging sibling of `confirm-sphinx-session!*`. Staging holds bytes in JVM
   heap, so this path serves only a build small enough to have gone through
   multipart — the reference corpus is far past that and arrives as a session.

   Pre  : user-id owns the staged upload; upload-id is unclaimed; the entry's
          flow is :sphinx.
   Post : {:ok true :topic-id N :report {...}} on success, {:ok false :error S}
          otherwise.
   Invariant: claim is one-shot — replays return {:ok false}. The extracted temp
          dir has exactly one owner: this fn, which deletes it on every path."
  [user-id upload-id]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes flow]} entry]
        (if (not= :sphinx flow)
          {:ok false :error (str "Not a documentation build: " flow)}
          (let [dir (archive/extract-to-temp-dir! bytes sphinx-extract-limits)]
            (try
              (let [r (sphinx-import/import-tree! user-id dir)]
                (when (:ok r) (commands/bump! user-id :import-document))
                r)
              (finally
                (archive/delete-dir! dir))))))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-sphinx-upload!* failed: " (.getMessage e)))
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

(defn confirm-account-upload!*
  "Finalize a staged account export into this account.

   The staging sibling of `confirm-account-session!*`. Staging holds bytes in JVM
   heap, so this path serves only an export small enough to have gone through
   multipart — anything larger arrives as an upload session.

   Pre  : user-id owns the staged upload; upload-id is unclaimed; the entry's
          flow is :account. `parent-topic-id` is a topic user-id owns, or nil.
   Post : {:ok true :import-id N :resumed? bool :report {...}} on success,
          {:ok false :error S} otherwise.
   Invariant: claim is one-shot — a replay returns {:ok false}, which also means
          a failed import here cannot be resumed without re-uploading. That is
          the cost of the heap path, and why the session path exists."
  [user-id upload-id parent-topic-id allow-second-copy?]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes flow]} entry]
        (if (not= :account flow)
          {:ok false :error (str "Not a FreeMemo account export: " flow)}
          (let [dir (archive/extract-to-temp-dir! bytes account-extract-limits)]
            (try
              (let [r (account-import/import-archive! db/ds user-id dir parent-topic-id
                        {:allow-second-copy? allow-second-copy?})]
                (when (:ok r) (commands/bump! user-id :import-account))
                r)
              (finally
                (archive/delete-dir! dir))))))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (log/log-error (str "confirm-account-upload!* failed: " (.getMessage e)))
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
