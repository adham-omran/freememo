(ns freememo.google-docs
  "Import a Google Doc as a `gdoc` root, and Pull it again later
   (plans/google-docs-pull.md).

   A Doc is read through a connected account (`freememo.google-accounts`)
   under `drive.file`, so only a Doc the user picked in the Picker is
   readable. The id of that pick is all a later Pull needs.

   One fetch is: `files.get` for the name and the kind, `files.export` as
   `text/html`, then `gdoc-html/export->html` → `doc-html/rewrite-for-quill!`
   → `clean-html`. The digest of that result decides whether a Pull changes
   anything. Images are re-hosted only for a snapshot that will be stored.

   A Pull publishes the root id in `:pulling-gdocs` while it runs, which is
   both the Activity monitor's membership and the guard that keeps a second
   Pull of the same root from starting."
  (:require
    [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [freememo.commands :as commands]
    [freememo.db :as db]
    [freememo.doc-html :as doc-html]
    [freememo.gdoc-html :as gdoc-html]
    [freememo.google-accounts :as accounts]
    [freememo.html-cleaner :as cleaner]
    [freememo.image-rehost :as rehost]
    [freememo.user-state :as us]
    [freememo.util :as util]))

(def doc-mime-type "application/vnd.google-apps.document")

(def ^:private max-export-bytes
  "Google stops an export at 10 MB. The read stops a little later, so a larger
   body is refused here rather than held in heap."
  (* 12 1024 1024))

(defn- url-encode [s] (java.net.URLEncoder/encode (str s) "UTF-8"))

(defn- error-reason
  "Post: the first `reason` of a Google API error body, or nil."
  [body]
  (let [parsed (if (string? body)
                 (try (json/parse-string body true) (catch Exception _ nil))
                 body)]
    (some-> parsed :error :errors first :reason)))

(defn- drive-failure
  "Post: {:ok false :error s} for a Drive response that was not a 200.
   `email` names the connected account in the message."
  [status body email]
  {:ok false
   :reconnect? (= 401 status)
   :error (cond
            (= 404 status) (str "Google Drive does not find this Doc for " email
                             ". It may be deleted, or no longer shared with that account.")
            (= 401 status) (str "Google no longer accepts the connection to " email
                             ". Connect the account again in Settings, Google Accounts.")
            (= "exportSizeLimitExceeded" (error-reason body))
            "This Doc is larger than the 10 MB Google allows for an export."
            (#{403 429} status) (str "Google Drive refused the request (" (or (error-reason body) status)
                                  "). Try again later.")
            :else (str "Google Drive answered " status ". Try again later."))})

(defn- file-meta
  "Pre : `token` is a live access token for the account that picked `file-id`.
   Post: {:ok true :name :mime-type :trashed?}, or a `drive-failure`."
  [token file-id email]
  (let [{:keys [status body]} (http/get (str "https://www.googleapis.com/drive/v3/files/" (url-encode file-id))
                                {:query-params {:fields "name,mimeType,trashed"
                                                :supportsAllDrives "true"}
                                 :headers {"Authorization" (str "Bearer " token)}
                                 :as :json :coerce :always
                                 :throw-exceptions false})]
    (if (= 200 status)
      {:ok true :name (:name body) :mime-type (:mimeType body)
       :trashed? (boolean (:trashed body))}
      (drive-failure status body email))))

(defn- export-html
  "Pre : as `file-meta`, and the file is a Google Doc.
   Post: {:ok true :html s}, or a `drive-failure`."
  [token file-id email]
  (let [{:keys [status body]} (http/get (str "https://www.googleapis.com/drive/v3/files/"
                                          (url-encode file-id) "/export")
                                {:query-params {:mimeType "text/html"}
                                 :headers {"Authorization" (str "Bearer " token)}
                                 :as :byte-array
                                 :throw-exceptions false})]
    (cond
      (not= 200 status) (drive-failure status (some-> ^bytes body (String. "UTF-8")) email)
      (> (alength ^bytes body) max-export-bytes)
      {:ok false :error "This Doc is larger than the 10 MB Google allows for an export."}
      :else {:ok true :html (String. ^bytes body "UTF-8")})))

(defn doc-url
  "Post: the address that opens Doc `file-id` in Google Docs."
  [file-id]
  (str "https://docs.google.com/document/d/" (url-encode file-id) "/edit"))

(defn stored-html
  "A Drive HTML export, as the sanitized HTML a snapshot stores before its
   images are re-hosted.
   Post: nil iff `export` is nil."
  [export]
  (some-> export
    gdoc-html/export->html
    doc-html/doc-html->quill-html
    cleaner/clean-html))

(defn- fetch-doc
  "Read the Doc `file-id` through connected account `sub`.
   Post: {:ok true :name :html :hash}, or {:ok false :error s}."
  [user-id sub file-id]
  (let [email (or (accounts/account-email user-id sub) "the connected account")
        token (accounts/access-token user-id sub)]
    (if-not (:ok token)
      token
      (let [meta (file-meta (:token token) file-id email)]
        (cond
          (not (:ok meta)) meta
          (not= doc-mime-type (:mime-type meta))
          {:ok false :error "Only a Google Doc can be imported. This file is another kind."}
          (:trashed? meta) {:ok false :error "This Doc is in the Google Drive trash."}
          :else
          (let [export (export-html (:token token) file-id email)]
            (if-not (:ok export)
              export
              (let [html (or (stored-html (:html export)) "")]
                {:ok true :name (:name meta)
                 :html html :hash (gdoc-html/content-hash html)}))))))))

(defn- rehosted
  "Post: `html` with every image downloaded into this user's media. An image
   that cannot be downloaded stays a link to Google, which is what
   `rehost-html-images!` does for every import."
  [user-id html]
  (:html (rehost/rehost-html-images! {:html html :base-url "https://docs.google.com/"
                                      :user-id user-id})))

(defn- pull-time
  "Post: now, in the form the repository home shows an import time."
  []
  (util/format-timestamp (java.sql.Timestamp/from (java.time.Instant/now))))

(defn import!
  "Import Doc `file-id`, picked through connected account `sub`.

   Pre : `user-id` is authenticated and picked `file-id` in the Picker with
         `sub`'s token.
   Post: {:ok true :topic-id root-id} with a root and its first snapshot, or
         {:ok false :error s}. A Doc this user already imported is refused,
         with its root as :topic-id, because a second root would split one
         Doc's history in two."
  [user-id sub file-id]
  (if-let [existing (db/find-gdoc-root user-id file-id)]
    {:ok false :topic-id existing
     :error "This Doc is already imported. Open it and press Pull to get its changes."}
    (let [doc (fetch-doc user-id sub file-id)]
      (if-not (:ok doc)
        doc
        (let [{:keys [root-id]} (db/create-gdoc-document!
                                  {:user-id user-id
                                   :title (or (not-empty (:name doc)) "Google Doc")
                                   :drive-file-id file-id :account-sub sub
                                   :snapshot-title (pull-time)
                                   :content (rehosted user-id (:html doc))
                                   :hash (:hash doc)})]
          (commands/bump! user-id :import-document)
          {:ok true :topic-id root-id})))))

(defn- pull-unguarded [user-id root-id]
  (if-let [{:keys [drive_file_id google_account_sub]} (db/gdoc-root user-id root-id)]
    (let [doc (fetch-doc user-id google_account_sub drive_file_id)]
      (cond
        (not (:ok doc)) doc

        (= (:hash doc) (db/latest-gdoc-snapshot-hash root-id))
        {:ok true :unchanged? true}

        :else
        (let [r (db/add-gdoc-snapshot! {:user-id user-id :root-id root-id
                                        :title (pull-time)
                                        :content (rehosted user-id (:html doc))
                                        :hash (:hash doc)})]
          (cond
            (nil? r) {:ok false :error "This Google Doc no longer exists in your library."}
            (:unchanged? r) {:ok true :unchanged? true}
            :else (do (commands/bump! user-id :pull-google-doc)
                      {:ok true :topic-id (:topic-id r)})))))
    {:ok false :error "This topic is not an imported Google Doc."}))

(defn pull!
  "Pull `user-id`'s `gdoc` root `root-id` once.

   Post: {:ok true :topic-id id} for a new snapshot, {:ok true :unchanged? true}
         when the Doc did not change, or {:ok false :error s}. Only one Pull of a
         root runs at a time: a second one returns an error at once.
   Inv : `root-id` is in `:pulling-gdocs` exactly while the work runs. An error
         is recorded in `:gdoc-pull-errors` BEFORE membership drops, so the
         Activity reporter closes the row as failed."
  [user-id root-id]
  (let [pulling (us/get-atom user-id :pulling-gdocs)
        errors (us/get-atom user-id :gdoc-pull-errors)
        [before _] (swap-vals! pulling conj root-id)]
    (if (contains? before root-id)
      {:ok false :error "A Pull of this Doc is already running."}
      (do
        (swap! errors dissoc root-id)
        (us/work-started! user-id)
        (try
          (let [r (pull-unguarded user-id root-id)]
            (when-not (:ok r) (swap! errors assoc root-id (:error r)))
            r)
          (catch Throwable t
            (tel/error! {:id ::pull-failed :data {:user-id user-id :root-id root-id}} t)
            (let [msg "The Pull failed. Try again."]
              (swap! errors assoc root-id msg)
              {:ok false :error msg}))
          (finally
            (swap! pulling disj root-id)))))))
