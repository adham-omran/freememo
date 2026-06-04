(ns freememo.api
  "REST API handlers for file upload, page text save, and authentication."
  (:require
   [freememo.pdf :as pdf]
   [freememo.epub :as epub]
   [freememo.page-ocr :as page]
   [freememo.db :as db]
   [freememo.auth :as auth]
   [freememo.google-oauth :as google-oauth]
   [freememo.extractor :as extractor]
   [freememo.html-cleaner :as cleaner]
   [freememo.quota :as quota]
   [freememo.user-state :as us]
   [freememo.biblio-import :as biblio-import]
   [freememo.content-type :as ct]
   [freememo.import-staging :as staging]
   [freememo.markdown :as md]
   [freememo.url-validate :as url]
   [freememo.csl :as csl]
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [freememo.wayl :as wayl]
   [freememo.geo :as geo]
   [freememo.config :as config]
   [taoensso.telemere :as tel]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [cheshire.core :as json])
  (:import [org.jsoup Jsoup]))

(defn- error-status
  "Map a result :code to an HTTP status. Defaults to 500."
  [code]
  (case code
    "file-too-large"    413
    "over-quota"        507
    "invalid-file-type" 400
    500))

(defn- json-response
  ([status body] {:status status
                  :headers {"Content-Type" "application/json"}
                  :body (json/generate-string body)}))

(defn- require-auth [request]
  (get-in request [:session :user-id]))

(defn logout-handler [_request]
  {:status 302
   :headers {"Location" "/"}
   :session nil})

(defn- upload-byte-cap
  "Per-request size cap from `quota/per-file-max-bytes`; nil when unlimited (env=0).
   Backstop in case the Ring middleware is bypassed."
  []
  (when-not (quota/unlimited? quota/per-file-max-bytes)
    quota/per-file-max-bytes))

(defn log-upload-failure!
  "Emit :warn so upload failures are diagnosable from app.log. Always
   includes :remote-ip + :user-agent; merges branch-specific data."
  [id request data]
  (tel/log! {:level :warn :id id}
    (merge {:remote-ip (geo/client-ip request)
            :user-agent (get-in request [:headers "user-agent"])}
           data)))

(defn- bytes->magic-hex
  "First 4 bytes as space-separated uppercase hex, e.g. \"50 4B 03 04\"."
  [^bytes b]
  (when (and b (>= (alength b) 4))
    (clojure.string/join " "
      (map #(format "%02X" (aget b %)) (range 4)))))

;; ── Helpers shared by new upload handlers ───────────────────────────

(defn- read-multipart-bytes [tempfile]
  (with-open [in (io/input-stream tempfile)]
    (let [baos (java.io.ByteArrayOutputStream.)]
      (io/copy in baos)
      (.toByteArray baos))))

(defn- quota-info->response [data]
  (case (:reason data)
    :file-too-large (json-response 413 {:success false
                                        :error (str "File exceeds per-file limit (" (:limit data) " bytes)")
                                        :code "file-too-large"
                                        :limit (:limit data) :incoming (:incoming data)})
    :over-quota (json-response 507 {:success false
                                    :error "Storage quota exceeded — delete documents to free space"
                                    :code "over-quota"
                                    :used (:used data) :limit (:limit data) :incoming (:incoming data)})))

(defn- html-default-title [html-str filename]
  (let [doc (Jsoup/parse (or html-str ""))]
    (or (some-> (.selectFirst doc "title") .text str/trim not-empty)
        (some-> (.selectFirst doc "h1") .text str/trim not-empty)
        (some-> (.selectFirst doc "h2") .text str/trim not-empty)
        (when filename (str/replace filename #"(?i)\.(html?|htm)$" ""))
        "Untitled")))

(defn- markdown-default-title [md-content filename]
  (or (md/extract-frontmatter-title md-content)
      (when filename (str/replace filename #"(?i)\.(md|markdown)$" ""))
      "Untitled"))

;; URL import previously lived at /api/upload-url. It now runs through the
;; Electric service in `freememo.import-modal` calling
;; `freememo.web-import/import-url!*` directly — no HTTP endpoint needed.

;; ── /api/upload-file ───────────────────────────────────────────────

(defn upload-file-handler
  "POST /api/upload-file — multipart file.
   Pre:  authenticated; size within cap.
   Post: one of:
         {:success true :upload_id ... :flow \"pdf|epub\" :filename :size} — staged
         {:success true :flow \"html|markdown\" :content :default_title :filename}
         {:success false :error <msg> :code <...>}"
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (if-let [file (get-in request [:params "file"])]
        (let [filename (:filename file)
              mime-type (or (:content-type file) "")
              tempfile (:tempfile file)
              ^bytes bytes (read-multipart-bytes tempfile)
              size (alength bytes)
              cap (upload-byte-cap)]
          (cond
            (and cap (> size cap))
            (do (log-upload-failure! ::upload-file-too-large request
                  {:user-id user-id :filename filename :file-size size :limit cap})
                (json-response 413 {:success false :error (str "File exceeds " cap " bytes")
                                    :code "file-too-large" :limit cap :incoming size}))

            :else
            (let [[kind reject-msg] (ct/classify-multipart filename mime-type bytes)]
              (case kind
                :pdf
                (let [upload-id (staging/stage! user-id bytes filename :pdf)]
                  (json-response 200 {:success true :upload_id upload-id :flow "pdf"
                                      :filename filename :size size}))

                :epub
                (if (epub/epub-magic-bytes? bytes)
                  (let [upload-id (staging/stage! user-id bytes filename :epub)]
                    (json-response 200 {:success true :upload_id upload-id :flow "epub"
                                        :filename filename :size size}))
                  (do (log-upload-failure! ::upload-file-bad-epub request
                        {:user-id user-id :filename filename :file-size size
                         :magic-hex (bytes->magic-hex bytes)})
                      (json-response 400 {:success false :error "Not a valid EPUB file"
                                          :code "invalid-file-type"})))

                :html
                (let [content (String. bytes "UTF-8")
                      default-title (html-default-title content filename)]
                  (json-response 200 {:success true :flow "html"
                                      :content content
                                      :default_title default-title
                                      :filename filename}))

                :markdown
                (let [content (String. bytes "UTF-8")
                      default-title (markdown-default-title content filename)]
                  (json-response 200 {:success true :flow "markdown"
                                      :content content
                                      :default_title default-title
                                      :filename filename}))

                :reject
                (json-response 400 {:success false :error reject-msg
                                    :code "invalid-file-type"})))))
        (json-response 400 {:success false :error "No file provided"}))
      (catch Exception e
        (tel/error! {:id ::upload-file-handler} e)
        (json-response 500 {:success false :error "Upload failed. Please try again with a different file."})))
    (json-response 401 {:success false :error "Not authenticated"})))

;; ── /api/upload-paste ──────────────────────────────────────────────

(defn upload-paste-handler
  "POST /api/upload-paste — {format, title, content, url?}.
   Pre:  authenticated; title and content non-blank; format is \"html\" or \"markdown\".
   Post: {:success true :doc_id N} or {:success false :error <msg>}."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [format (get-in request [:params "format"])
            title (get-in request [:params "title"])
            content (get-in request [:params "content"])
            url-val (get-in request [:params "url"])]
        (cond
          (str/blank? title)
          (json-response 400 {:success false :error "Title is required"})

          (str/blank? content)
          (json-response 400 {:success false :error "Content is required"})

          (= format "html")
          (let [bib (when (seq url-val) {:url url-val :source-type "web"})
                topic-id (db/create-web-topic! user-id title
                           (cleaner/clean-html content)
                           bib)]
            (when topic-id
              (try (biblio-import/prepare-biblio! user-id topic-id nil)
                   (catch Exception e
                     (tel/log! {:level :warn :id ::upload-paste-biblio} (.getMessage e)))))
            (swap! (us/get-atom user-id :refresh) inc)
            (swap! (us/get-atom user-id :tree-mutations) inc)
            (json-response 200 {:success true :doc_id topic-id}))

          (= format "markdown")
          (let [html (md/parse-markdown content)
                topic-id (db/create-markdown-topic! user-id title html)]
            (when topic-id
              (try (biblio-import/prepare-biblio! user-id topic-id nil)
                   (catch Exception e
                     (tel/log! {:level :warn :id ::upload-paste-biblio} (.getMessage e)))))
            (swap! (us/get-atom user-id :refresh) inc)
            (swap! (us/get-atom user-id :tree-mutations) inc)
            (json-response 200 {:success true :doc_id topic-id}))

          :else
          (json-response 400 {:success false :error "Unsupported format"})))
      (catch Exception e
        (tel/error! {:id ::upload-paste-handler} e)
        (json-response 500 {:success false :error "Import failed. Please try again."})))
    (json-response 401 {:success false :error "Not authenticated"})))

;; ── /api/upload-staged ─────────────────────────────────────────────

(defn upload-staged-handler
  "POST /api/upload-staged — {upload_id, image_mode?}.
   Pre:  authenticated; upload-id refers to an unclaimed entry staged by this user.
   Post: {:success true :doc_id N} or error JSON.
   Invariant: claim is one-shot — replays return 404."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [upload-id (get-in request [:params "upload_id"])
            image-mode (keyword (or (get-in request [:params "image_mode"]) "reduce"))]
        (cond
          (str/blank? upload-id)
          (json-response 400 {:success false :error "Missing upload_id"})

          :else
          (if-let [entry (staging/claim! user-id upload-id)]
            (let [{:keys [^bytes bytes filename flow extra]} entry
                  web-biblio (:web-biblio extra)]
              (case flow
                :pdf
                (let [result (pdf/save-pdf user-id filename bytes)]
                  (if (:success result)
                    (let [topic-id (:id result)]
                      (try (biblio-import/prepare-biblio! user-id topic-id web-biblio)
                           (catch Exception e
                             (tel/log! {:level :warn :id ::upload-staged-pdf-biblio}
                               (.getMessage e))))
                      (json-response 200 {:success true :doc_id topic-id}))
                    (do (log-upload-failure! ::upload-staged-pdf-error request
                          {:user-id user-id :filename filename
                           :reason (:error result) :code (:code result)})
                        (json-response (error-status (:code result))
                          (cond-> {:success false :error (:error result)}
                            (:code result) (assoc :code (:code result))
                            (:used result) (assoc :used (:used result))
                            (:limit result) (assoc :limit (:limit result)))))))

                :epub
                (let [result (epub/process-epub bytes image-mode)]
                  (if (:error result)
                    (do (log-upload-failure! ::upload-staged-epub-process-error request
                          {:user-id user-id :filename filename
                           :reason (:error result)})
                        (json-response 400 {:success false :error (:error result)}))
                    (let [{:keys [title chapters]} result
                          display-name (or title filename "Untitled EPUB")
                          file-size (alength bytes)
                          {:keys [topic-id]} (db/create-epub-topic!
                                               user-id display-name bytes file-size chapters)]
                      (swap! (us/get-atom user-id :refresh) inc)
                      (swap! (us/get-atom user-id :tree-mutations) inc)
                      (try (biblio-import/prepare-biblio! user-id topic-id)
                           (catch Exception e
                             (tel/log! {:level :warn :id ::upload-staged-epub-biblio}
                               (.getMessage e))))
                      (json-response 200 {:success true :doc_id topic-id}))))

                (json-response 400 {:success false :error (str "Unknown flow: " flow)})))
            (json-response 404 {:success false :error "Upload not found or expired"}))))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (if (quota/quota-error? data)
            (quota-info->response data)
            (do (tel/error! {:id ::upload-staged-handler} e)
                (json-response 500 {:success false :error "Import failed. Please try again."})))))
      (catch Exception e
        (tel/error! {:id ::upload-staged-handler} e)
        (json-response 500 {:success false :error "Import failed. Please try again."})))
    (json-response 401 {:success false :error "Not authenticated"})))

;; ── /api/zotero/stage — accept PDF bytes from the FreeMemo Zotero plugin ──
;;
;; All Zotero I/O now lives in the browser, mediated by the FreeMemo
;; for-Zotero plugin. The browser fetches bytes + CSL-JSON from the plugin's
;; loopback endpoints and POSTs them here for staging. Server stays the
;; trust boundary: it re-enforces the size cap and runs the CSL → web-biblio
;; conversion before handing off to staging/stage!.

(defn zotero-stage-handler
  "POST /api/zotero/stage — multipart {file, filename?, csljson?}.
   Pre:  authenticated; Zotero enabled in settings; file is PDF bytes from
         the FreeMemo Zotero plugin; csljson is optional raw CSL-JSON for
         the parent item (JSON-encoded string).
   Post: {:success true :upload_id ... :flow \"pdf\" :filename :size}.
   Blame: 413 when bytes exceed per-file cap (caller); 400 missing file;
          401 unauthenticated; 403 Zotero disabled."
  [request]
  (if-let [user-id (require-auth request)]
    (if-not (= "true" (db/get-setting user-id settings/ZOTERO_ENABLED))
      (json-response 403 {:success false :error "Zotero is not enabled"})
      (try
        (if-let [file (get-in request [:params "file"])]
          (let [filename    (or (not-empty (get-in request [:params "filename"]))
                                (:filename file)
                                "attachment.pdf")
                csljson-str (get-in request [:params "csljson"])
                csljson     (when (and csljson-str (not (str/blank? csljson-str)))
                              (try (json/parse-string csljson-str true)
                                   (catch Exception _ nil)))
                web-biblio  (csl/csljson->web-biblio csljson)
                ^bytes bytes (read-multipart-bytes (:tempfile file))
                size        (alength bytes)
                cap         (upload-byte-cap)]
            (cond
              (and cap (> size cap))
              (do (log-upload-failure! ::zotero-stage-too-large request
                    {:user-id user-id :filename filename :file-size size :limit cap})
                  (json-response 413 {:success false
                                      :error (str "File exceeds " cap " bytes")
                                      :code "file-too-large"
                                      :limit cap :incoming size}))

              :else
              (let [upload-id (staging/stage! user-id bytes filename :pdf
                                {:web-biblio web-biblio
                                 :source     :zotero})]
                (json-response 200 {:success true
                                    :upload_id upload-id
                                    :flow      "pdf"
                                    :filename  filename
                                    :size      size}))))
          (json-response 400 {:success false :error "Missing file"}))
        (catch clojure.lang.ExceptionInfo e
          (let [data (ex-data e)]
            (if (quota/quota-error? data)
              (quota-info->response data)
              (do (tel/error! {:id ::zotero-stage} e)
                  (json-response 500 {:success false :error "Stage failed"})))))
        (catch Exception e
          (tel/error! {:id ::zotero-stage} e)
          (json-response 500 {:success false :error "Stage failed"}))))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn create-topic-handler [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [title (or (get-in request [:params "title"]) "New Topic")
            {:keys [topic-id]} (db/create-standalone-topic! user-id title)]
        (if topic-id
          {:status 200
           :headers {"Content-Type" "application/json"}
           :body (json/generate-string {:success true :doc_id topic-id})}
          {:status 500
           :headers {"Content-Type" "application/json"}
           :body (json/generate-string {:success false :error "Failed to create topic"})}))
      (catch Exception e
        (tel/error! {:id ::create-topic-handler} e)
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:success false :error "Failed to create topic. Please try again."})}))
    {:status 401
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:success false :error "Not authenticated"})}))

(defn upload-media-handler
  "Accept a multipart image upload, dedup by (user, sha256), return media id."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (if-let [file (get-in request [:params "file"])]
        (let [filename (:filename file)
              mime-type (or (:content-type file) "application/octet-stream")
              tempfile (:tempfile file)
              bytes (with-open [in (io/input-stream tempfile)]
                      (let [baos (java.io.ByteArrayOutputStream.)]
                        (io/copy in baos)
                        (.toByteArray baos)))
              size (alength bytes)
              cap (upload-byte-cap)]
          (cond
            (not (re-find #"^image/" mime-type))
            (json-response 400 {:success false :error "Only image uploads are supported"})

            (and cap (> size cap))
            (do (log-upload-failure! ::upload-media-too-large request
                  {:user-id user-id :filename filename :file-size size :limit cap})
                (json-response 413 {:success false :error (str "File exceeds " cap " bytes")
                                    :code "file-too-large" :limit cap :incoming size}))

            :else
            (let [id (db/upsert-media! {:user-id user-id
                                        :kind "image"
                                        :bytes bytes
                                        :mime-type mime-type})]
              (json-response 200 {:success true :id id}))))
        (json-response 400 {:success false :error "No file provided"}))
      (catch Exception e
        (tel/error! {:id ::upload-media-handler} e)
        (json-response 500 {:success false :error "Upload failed. Please try again."})))
    (do (log-upload-failure! ::upload-media-not-authenticated request {})
        (json-response 401 {:success false :error "Not authenticated"}))))

(defn get-media-handler
  "Serve media bytes by id, auth-checked per user."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [uri (:uri request)
            media-id (-> uri (clojure.string/split #"/") last parse-long)
            row (db/get-media media-id)]
        (cond
          (nil? row)
          {:status 404 :body "Not found"}

          (not= (:media/user_id row) user-id)
          {:status 403 :body "Forbidden"}

          :else
          {:status 200
           :headers {"Content-Type" (:media/mime_type row)
                     "Cache-Control" "private, max-age=86400"}
           :body (io/input-stream (:media/bytes row))}))
      (catch Exception e
        (tel/error! {:id ::get-media-handler} e)
        {:status 500 :body "Internal server error"}))
    {:status 401 :body "Not authenticated"}))

(defn get-pdf-handler [request]
  "Serve PDF file by topic ID from database."
  (if-let [user-id (require-auth request)]
    (try
      (let [uri (:uri request)
            topic-id (-> uri (clojure.string/split #"/") last parse-long)
            topic (db/get-topic topic-id)]
        (if (and topic (= (:topics/user_id topic) user-id))
          (if-let [file-row (db/get-topic-file topic-id)]
            (let [title (:topics/title topic)
                  download-name (if (re-find #"(?i)\.pdf$" title)
                                  title
                                  (str title ".pdf"))]
              {:status 200
               :headers {"Content-Type" "application/pdf"
                         "Content-Disposition" (str "inline; filename=\"" download-name "\"")}
               :body (io/input-stream (:topic_files/file_data file-row))})
            {:status 404
             :body "PDF not found"})
          {:status 404
           :body "PDF not found"}))
      (catch Exception e
        (tel/error! {:id ::get-pdf-handler} e)
        {:status 500
         :body "Internal server error"}))
    {:status 302
     :headers {"Location" "/"}
     :body ""}))

(defn google-auth-handler [_request]
  (if-not (google-oauth/configured?)
    {:status 503
     :headers {"Content-Type" "text/plain"}
     :body "Google sign-in is not configured (missing credentials)"}
    (let [state (str (random-uuid))]
      {:status 302
       :headers {"Location" (google-oauth/auth-url state)}
       :session {:oauth-state state}})))

(defn google-callback-handler [request]
  (let [code (get-in request [:params "code"])
        state (get-in request [:params "state"])
        expected-state (get-in request [:session :oauth-state])]
    (cond
      (not= state expected-state)
      {:status 302
       :headers {"Location" "/"}
       :session {:auth-error "OAuth state mismatch — please try again"}}

      (clojure.string/blank? code)
      {:status 302
       :headers {"Location" "/"}
       :session {:auth-error "Google sign-in was cancelled"}}

      :else
      (try
        (let [claims (google-oauth/exchange-code code)
              google-sub (str (:sub claims))
              email (str (:email claims))
              display-name (str (:name claims))
              {:keys [user-id]} (google-oauth/find-or-create-user google-sub email display-name)]
          ;; One-time signup credit grant (idempotent; no-op in self-host).
          (try (db/grant-signup-credits! user-id)
               (catch Exception e (tel/error! {:id ::signup-grant} e)))
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id user-id}})
        (catch Exception e
          (tel/error! {:id ::google-callback-handler} e)
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error "Google sign-in failed — please try again"}})))))

(defn login-handler
  "GET /login?user=X&password=Y — dev-only username/password login.
   On success: 302 → / with :session {:user-id N}.
   On failure: 302 → / with :session {:auth-error ...}.
   Query-string credentials leak into server logs and browser history —
   acceptable only for local Playwright-driven testing."
  [request]
  (let [params (:params request)
        username (get params "user")
        password (get params "password")]
    (if (or (str/blank? username) (str/blank? password))
      {:status 302
       :headers {"Location" "/"}
       :session {:auth-error "Missing user or password"}}
      (let [result (auth/authenticate username password)]
        (if (:success result)
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id (:user-id result)}}
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error (:error result)}})))))

(defn save-page-text-handler [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [params (:params request)
            parent-id (some-> (get params "document_id") parse-long)
            page-number (some-> (get params "page_number") parse-long)
            html (get params "html")]
        (if (and parent-id page-number html)
          (if (db/get-topic-for-user user-id parent-id)
            (let [result (page/save-page-html-impl parent-id page-number html)]
              {:status (if (:success result) 200 500)
               :headers {"Content-Type" "text/plain"}
               :body (if (:success result) "ok" (str "error: " (:error result)))})
            {:status 404
             :headers {"Content-Type" "text/plain"}
             :body "Not found"})
          {:status 400
           :headers {"Content-Type" "text/plain"}
           :body "Missing required parameters: document_id, page_number, html"}))
      (catch Exception e
        (tel/error! {:id ::save-page-text-handler} e)
        {:status 500
         :headers {"Content-Type" "text/plain"}
         :body "error: Failed to save page text. Please try again."}))
    {:status 302
     :headers {"Location" "/"}
     :body ""}))

;; ── Credits: checkout + Wayl webhook (§5.5 / §5.6) ──────────────────

(defn credits-checkout-handler
  "POST /api/credits/checkout — {amount_iqd}. Creates a Wayl top-up link.
   Pre:  authenticated; credits enabled; amount is a configured preset.
   Post: {:success true :url u} or {:success false :error msg}. Wayl URL has
         `currency=usd` appended when the client IP resolves to a non-IQ country
         (or to unknown — fallback is USD).
   Blame: 401 unauth (caller); 400 bad amount / credits off / provider error."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (let [amount (some-> (get-in request [:params "amount_iqd"]) parse-long)
            ip (geo/client-ip request)
            country (geo/country-of ip)]
        (tel/log! {:level :debug :id ::credits-checkout-geo
                   :data {:remote-ip ip :country country :usd? (not= "IQ" country)}}
          "Geo-resolved checkout currency")
        (let [r (credits/start-checkout! user-id amount
                  (credits/request-base-url request) country)]
          (if (:ok r)
            (json-response 200 {:success true :url (:url r)})
            (json-response 400 {:success false :error (:error r)}))))
      (catch Exception e
        (tel/error! {:id ::credits-checkout} e)
        (json-response 500 {:success false :error "Checkout failed. Please try again."})))
    (json-response 401 {:success false :error "Not authenticated"})))

(defn- request-body-bytes
  "Raw request body bytes. Wayl posts application/json, which Ring's param
   middleware does not consume, so the stream is intact here — required to
   verify the HMAC over the exact bytes received."
  ^bytes [request]
  (when-let [^java.io.InputStream b (:body request)]
    (let [baos (java.io.ByteArrayOutputStream.)]
      (io/copy b baos)
      (.toByteArray baos))))

(defn wayl-webhook-handler
  "POST /api/wayl/webhook — Wayl payment callback. Verifies HMAC over the raw
   body, then credits idempotently on a paid status.
   Post: 401 forged signature; 200 once accepted (so Wayl stops retrying);
         500 on internal error (so Wayl retries a transient failure)."
  [request]
  (let [raw (request-body-bytes request)
        sig (get-in request [:headers "x-wayl-signature-256"])]
    (if-not (wayl/verify-signature? raw sig)
      (do (tel/log! {:level :warn :id ::wayl-webhook-rejected
                     :data {:sig-present? (some? sig)
                            :remote-addr (:remote-addr request)}}
            "Wayl webhook rejected: invalid signature")
          (json-response 401 {:success false :error "Invalid signature"}))
      (try
        (let [payload (json/parse-string (String. raw "UTF-8") true)
              reference-id (:referenceId payload)
              status (or (:paymentStatus payload) (:status payload))]
          (cond
            (str/blank? reference-id)
            (json-response 200 {:success true})

            (#{"Complete" "Delivered"} status)
            (let [r (db/complete-credit-order! reference-id)]
              (when (:credited r)
                (swap! (us/get-atom (:user-id r) :credits-refresh) inc)
                (tel/log! {:level :info :id ::wayl-credited
                           :data {:reference-id reference-id :amount (:amount r)
                                  :user-id (:user-id r)}}
                  "Wayl payment credited"))
              (json-response 200 {:success true}))

            (#{"Cancelled" "Rejected" "Returned"} status)
            (do (db/fail-credit-order! reference-id)
                (json-response 200 {:success true}))

            :else
            (json-response 200 {:success true})))
        (catch Exception e
          (tel/error! {:id ::wayl-webhook} e)
          (json-response 500 {:success false :error "Webhook processing failed"}))))))

(defn api-routes [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= uri "/api/logout") (= method :post))
      (logout-handler request)

(and (= uri "/api/upload-file") (= method :post))
      (upload-file-handler request)

      (and (= uri "/api/upload-paste") (= method :post))
      (upload-paste-handler request)

      (and (= uri "/api/upload-staged") (= method :post))
      (upload-staged-handler request)

      (and (= uri "/api/zotero/stage") (= method :post))
      (zotero-stage-handler request)

      (and (= uri "/api/create-topic") (= method :post))
      (create-topic-handler request)

      (and (re-matches #"/api/pdf/\d+" uri) (= method :get))
      (get-pdf-handler request)

      (and (= uri "/api/upload-media") (= method :post))
      (upload-media-handler request)

      (and (re-matches #"/api/media/\d+" uri) (= method :get))
      (get-media-handler request)

      (and (= uri "/api/save-page-text") (= method :post))
      (save-page-text-handler request)

      (and (= uri "/api/credits/checkout") (= method :post))
      (credits-checkout-handler request)

      (and (= uri "/api/wayl/webhook") (= method :post))
      (wayl-webhook-handler request)

      (and (= uri "/login") (= method :get))
      (login-handler request)

      (and (= uri "/auth/google") (= method :get))
      (google-auth-handler request)

      (and (= uri "/auth/google/callback") (= method :get))
      (google-callback-handler request)

      :else nil)))
