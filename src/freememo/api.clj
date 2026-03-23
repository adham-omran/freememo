(ns freememo.api
  "REST API handlers for file upload, page text save, and authentication."
  (:require
    [freememo.pdf :as pdf]
    [freememo.epub :as epub]
    [freememo.page-ocr :as page]
    [freememo.db :as db]
    [freememo.auth :as auth]
    [freememo.crypto :as crypto]
    [freememo.google-oauth :as google-oauth]
    [freememo.extractor :as extractor]
    [freememo.html-cleaner :as cleaner]
    [taoensso.telemere :as tel]
    [clojure.java.io :as io]
    [clojure.string]
    [cheshire.core :as json]))

(defn- require-auth [request]
  (get-in request [:session :user-id]))

(defn signup-handler [request]
  (let [username (get-in request [:params "username"])
        password (get-in request [:params "password"])]
    (if (or (clojure.string/blank? username) (clojure.string/blank? password))
      {:status 302
       :headers {"Location" "/"}
       :session {:auth-error "Username and password are required"}}
      (let [result (auth/create-user username password)]
        (if (:success result)
          (let [user-id (:user-id result)
                enc-key (crypto/derive-key password user-id)]
            {:status 302
             :headers {"Location" "/"}
             :session {:user-id user-id :username username :enc-key enc-key}})
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error (:error result)}})))))

(defn login-handler [request]
  (let [username (get-in request [:params "username"])
        password (get-in request [:params "password"])]
    (if (or (clojure.string/blank? username) (clojure.string/blank? password))
      {:status 302
       :headers {"Location" "/"}
       :session {:auth-error "Username and password are required"}}
      (let [result (auth/authenticate username password)]
        (if (:success result)
          (let [user-id (:user-id result)
                enc-key (crypto/derive-key password user-id)]
            {:status 302
             :headers {"Location" "/"}
             :session {:user-id user-id :username (:username result) :enc-key enc-key}})
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error (:error result)}})))))

(defn logout-handler [_request]
  {:status 302
   :headers {"Location" "/"}
   :session nil})

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
              result (pdf/save-pdf user-id filename bytes)]
          (if (:success result)
            {:status 200
             :headers {"Content-Type" "application/json"}
             :body (json/generate-string {:success true :doc_id (:id result)})}
            {:status 500
             :headers {"Content-Type" "application/json"}
             :body (json/generate-string {:success false :error (:error result)})}))
        {:status 400
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:success false :error "No file provided"})})
      (catch Exception e
        (tel/error! {:id ::upload-pdf-handler} e)
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:success false :error "Upload failed. Please try again with a different file."})}))
    {:status 401
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:success false :error "Not authenticated"})}))

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
              result (epub/process-epub bytes image-mode)]
          (if (:error result)
            {:status 400
             :headers {"Content-Type" "application/json"}
             :body (json/generate-string {:success false :error (:error result)})}
            (let [{:keys [title chapters]} result
                  display-name (or title filename "Untitled EPUB")
                  {:keys [topic-id chapter-ids]} (db/create-epub-topic! user-id display-name bytes (alength bytes) chapters)]
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
              {:status 200
               :headers {"Content-Type" "application/json"}
               :body (json/generate-string {:success true :doc_id topic-id})})))
        {:status 400
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:success false :error "No file provided"})})
      (catch Exception e
        (tel/error! {:id ::upload-epub-handler} e)
        {:status 500
         :headers {"Content-Type" "application/json"}
         :body (json/generate-string {:success false :error "Upload failed. Please try again with a different file."})}))
    {:status 401
     :headers {"Content-Type" "application/json"}
     :body (json/generate-string {:success false :error "Not authenticated"})}))

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
              {:keys [user-id username enc-key]} (google-oauth/find-or-create-user google-sub email display-name)]
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id user-id :username username :enc-key enc-key}})
        (catch Exception e
          (tel/error! {:id ::google-callback-handler} e)
          {:status 302
           :headers {"Location" "/"}
           :session {:auth-error "Google sign-in failed — please try again"}})))))

(defn save-page-text-handler [request]
  (try
    (let [params (:params request)
          parent-id (some-> (get params "document_id") parse-long)
          page-number (some-> (get params "page_number") parse-long)
          html (get params "html")]
      (if (and parent-id page-number html)
        (let [result (page/save-page-html-impl parent-id page-number html)]
          {:status (if (:success result) 200 500)
           :headers {"Content-Type" "text/plain"}
           :body (if (:success result) "ok" (str "error: " (:error result)))})
        {:status 400
         :headers {"Content-Type" "text/plain"}
         :body "Missing required parameters: document_id, page_number, html"}))
    (catch Exception e
      (tel/error! {:id ::save-page-text-handler} e)
      {:status 500
       :headers {"Content-Type" "text/plain"}
       :body "error: Failed to save page text. Please try again."})))

(defn api-routes [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= uri "/api/signup") (= method :post))
      (signup-handler request)

      (and (= uri "/api/login") (= method :post))
      (login-handler request)

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
