(ns freememo.api
  "REST API handlers for file upload, page text save, and authentication."
  (:require
   [freememo.pdf :as pdf]
   [freememo.epub :as epub]
   [freememo.page-ocr :as page]
   [freememo.db :as db]
   [freememo.google-oauth :as google-oauth]
   [freememo.extractor :as extractor]
   [freememo.html-cleaner :as cleaner]
   [freememo.quota :as quota]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]
   [clojure.java.io :as io]
   [clojure.string]
   [cheshire.core :as json]))

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

(def ^:private upload-byte-cap 104857600) ;; 100 MB; backstop in case the Ring middleware is bypassed.

(defn log-upload-failure!
  "Emit :warn so upload failures are diagnosable from app.log. Always
   includes :remote-ip + :user-agent; merges branch-specific data."
  [id request data]
  (tel/log! {:level :warn :id id}
    (merge {:remote-ip (or (get-in request [:headers "cf-connecting-ip"])
                           (:remote-addr request))
            :user-agent (get-in request [:headers "user-agent"])}
           data)))

(defn- bytes->magic-hex
  "First 4 bytes as space-separated uppercase hex, e.g. \"50 4B 03 04\"."
  [^bytes b]
  (when (and b (>= (alength b) 4))
    (clojure.string/join " "
      (map #(format "%02X" (aget b %)) (range 4)))))

(defn upload-pdf-handler [request]
  (if-let [user-id (require-auth request)]
    (try
      (if-let [file (get-in request [:params "file"])]
        (let [filename (:filename file)
              tempfile (:tempfile file)
              bytes (with-open [in (io/input-stream tempfile)]
                      (let [baos (java.io.ByteArrayOutputStream.)]
                        (io/copy in baos)
                        (.toByteArray baos)))
              size (alength bytes)]
          (if (> size upload-byte-cap)
            (do (log-upload-failure! ::upload-pdf-too-large request
                  {:user-id user-id :filename filename :file-size size :limit upload-byte-cap})
                (json-response 413 {:success false :error "File exceeds 100 MB limit"
                                    :code "file-too-large" :limit upload-byte-cap :incoming size}))
            (let [result (pdf/save-pdf user-id filename bytes)]
              (if (:success result)
                (json-response 200 {:success true :doc_id (:id result)})
                (do (log-upload-failure! ::upload-pdf-process-error request
                      {:user-id user-id :filename filename :file-size size
                       :reason (:error result) :code (:code result)})
                    (json-response (error-status (:code result))
                      (cond-> {:success false :error (:error result)}
                        (:code result) (assoc :code (:code result))
                        (:used result) (assoc :used (:used result))
                        (:limit result) (assoc :limit (:limit result)))))))))
        (json-response 400 {:success false :error "No file provided"}))
      (catch Exception e
        (tel/error! {:id ::upload-pdf-handler} e)
        (json-response 500 {:success false :error "Upload failed. Please try again with a different file."})))
    (do (log-upload-failure! ::upload-pdf-not-authenticated request {})
        (json-response 401 {:success false :error "Not authenticated"}))))

(defn upload-epub-handler [request]
  (if-let [user-id (require-auth request)]
    (try
      (if-let [file (get-in request [:params "file"])]
        (let [filename (:filename file)
              tempfile (:tempfile file)
              auto-extract? (= "true" (get-in request [:params "auto_extract"]))
              image-mode (keyword (or (get-in request [:params "image_mode"]) "reduce"))
              bytes (with-open [in (io/input-stream tempfile)]
                      (let [baos (java.io.ByteArrayOutputStream.)]
                        (io/copy in baos)
                        (.toByteArray baos)))
              file-size (alength bytes)]
          (cond
            (> file-size upload-byte-cap)
            (do (log-upload-failure! ::upload-epub-too-large request
                  {:user-id user-id :filename filename :file-size file-size :limit upload-byte-cap})
                (json-response 413 {:success false :error "File exceeds 100 MB limit"
                                    :code "file-too-large" :limit upload-byte-cap :incoming file-size}))

            (not (epub/epub-magic-bytes? bytes))
            (do (log-upload-failure! ::upload-epub-bad-magic request
                  {:user-id user-id :filename filename :file-size file-size
                   :magic-hex (bytes->magic-hex bytes)})
                (json-response 400 {:success false :error "Not a valid EPUB file"
                                    :code "invalid-file-type"}))

            :else
            (let [result (epub/process-epub bytes image-mode)]
              (if (:error result)
                (do (log-upload-failure! ::upload-epub-process-error request
                      {:user-id user-id :filename filename :file-size file-size
                       :reason (:error result)})
                    (json-response 400 {:success false :error (:error result)}))
                (let [{:keys [title chapters]} result
                      display-name (or title filename "Untitled EPUB")
                      {:keys [topic-id chapter-ids]} (db/create-epub-topic! user-id display-name bytes file-size chapters)]
                  (when (and auto-extract? topic-id (seq chapter-ids))
                    (try
                      (doseq [[ch-id ch] (map vector chapter-ids chapters)]
                        (when (and ch-id (seq (:html ch)))
                          (when-let [{:keys [sections]} (extractor/extract-and-annotate (:html ch))]
                            (let [clean-sections (mapv #(update % :content cleaner/clean-html) sections)]
                              (doseq [section clean-sections]
                                (db/create-topic! {:user-id user-id
                                                   :parent-id ch-id
                                                   :kind "basic"
                                                   :title (or (first (re-find #"[^<]+" (:content section))) "Extract")
                                                   :content (:content section)}))))))
                      (catch Exception e
                        (tel/log! {:level :warn :id ::upload-epub-auto-extract} (.getMessage e)))))
                  (swap! (us/get-atom user-id :refresh) inc)
                  (json-response 200 {:success true :doc_id topic-id}))))))
        (json-response 400 {:success false :error "No file provided"}))
      (catch clojure.lang.ExceptionInfo e
        (let [data (ex-data e)]
          (if (quota/quota-error? data)
            (case (:reason data)
              :file-too-large (json-response 413 {:success false
                                                  :error (str "File exceeds per-file limit (" (:limit data) " bytes)")
                                                  :code "file-too-large"
                                                  :limit (:limit data) :incoming (:incoming data)})
              :over-quota (json-response 507 {:success false
                                              :error "Storage quota exceeded — delete documents to free space"
                                              :code "over-quota"
                                              :used (:used data) :limit (:limit data) :incoming (:incoming data)}))
            (do (tel/error! {:id ::upload-epub-handler} e)
                (json-response 500 {:success false :error "Upload failed. Please try again with a different file."})))))
      (catch Exception e
        (tel/error! {:id ::upload-epub-handler} e)
        (json-response 500 {:success false :error "Upload failed. Please try again with a different file."})))
    (do (log-upload-failure! ::upload-epub-not-authenticated request {})
        (json-response 401 {:success false :error "Not authenticated"}))))

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

(defn get-pdf-handler [request]
  "Serve PDF file by topic ID from database."
  (if-let [user-id (require-auth request)]
    (try
      (let [uri (:uri request)
            topic-id (-> uri (clojure.string/split #"/") last parse-long)
            topic (db/get-topic topic-id)]
        (if (and topic (= (:topics/user_id topic) user-id))
          (if-let [file-row (db/get-topic-file topic-id)]
            {:status 200
             :headers {"Content-Type" "application/pdf"
                       "Content-Disposition" (str "inline; filename=\"" (:topics/title topic) "\"")}
             :body (io/input-stream (:topic_files/file_data file-row))}
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
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id user-id}})
        (catch Exception e
          (tel/error! {:id ::google-callback-handler} e)
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error "Google sign-in failed — please try again"}})))))

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

(defn api-routes [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= uri "/api/logout") (= method :post))
      (logout-handler request)

      (and (= uri "/api/upload-pdf") (= method :post))
      (upload-pdf-handler request)

      (and (= uri "/api/upload-epub") (= method :post))
      (upload-epub-handler request)

      (and (= uri "/api/create-topic") (= method :post))
      (create-topic-handler request)

      (and (re-matches #"/api/pdf/\d+" uri) (= method :get))
      (get-pdf-handler request)

      (and (= uri "/api/save-page-text") (= method :post))
      (save-page-text-handler request)

      (and (= uri "/auth/google") (= method :get))
      (google-auth-handler request)

      (and (= uri "/auth/google/callback") (= method :get))
      (google-callback-handler request)

      :else nil)))
