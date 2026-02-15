(ns electric-starter-app.api
  "REST API handlers for file upload, page text save, and authentication."
  (:require
    [electric-starter-app.pdf :as pdf]
    [electric-starter-app.page :as page]
    [electric-starter-app.db :as db]
    [electric-starter-app.auth :as auth]
    [clojure.java.io :as io]
    [clojure.string]))

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
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id (:user-id result) :username username}}
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
          {:status 302
           :headers {"Location" "/"}
           :session {:user-id (:user-id result) :username (:username result)}}
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
            {:status 302
             :headers {"Location" "/"}
             :body ""}
            {:status 500
             :headers {"Content-Type" "text/plain"}
             :body (str "error: " (:error result))}))
        {:status 400
         :headers {"Content-Type" "text/plain"}
         :body "error: No file provided"})
      (catch Exception e
        (println "ERROR [upload-pdf-handler]:" (.getMessage e))
        (.printStackTrace e)
        {:status 500
         :headers {"Content-Type" "text/plain"}
         :body (str "error: " (.getMessage e))}))
    {:status 302
     :headers {"Location" "/"}
     :body ""}))

(defn get-pdf-handler [request]
  "Serve PDF file by ID from database."
  (if-let [user-id (require-auth request)]
    (try
      (let [uri (:uri request)
            doc-id (-> uri (clojure.string/split #"/") last parse-long)]
        (if-let [doc (first (db/get-documents-by-id user-id doc-id))]
          {:status 200
           :headers {"Content-Type" "application/pdf"
                     "Content-Disposition" (str "inline; filename=\"" (:documents/filename doc) "\"")}
           :body (io/input-stream (:documents/file_data doc))}
          {:status 404
           :body "PDF not found"}))
      (catch Exception e
        (println "ERROR [get-pdf-handler]:" (.getMessage e))
        {:status 500
         :body "Internal server error"}))
    {:status 302
     :headers {"Location" "/"}
     :body ""}))

(defn save-page-text-handler [request]
  (try
    (let [params (:params request)
          document-id (some-> (get params "document_id") parse-long)
          page-number (some-> (get params "page_number") parse-long)
          html (get params "html")]
      (if (and document-id page-number html)
        (let [result (page/save-page-html-impl document-id page-number html)]
          {:status (if (:success result) 200 500)
           :headers {"Content-Type" "text/plain"}
           :body (if (:success result) "ok" (str "error: " (:error result)))})
        {:status 400
         :headers {"Content-Type" "text/plain"}
         :body "Missing required parameters: document_id, page_number, html"}))
    (catch Exception e
      (println "ERROR [save-page-text-handler]:" (.getMessage e))
      {:status 500
       :headers {"Content-Type" "text/plain"}
       :body (str "error: " (.getMessage e))})))

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

      (and (re-matches #"/api/pdf/\d+" uri) (= method :get))
      (get-pdf-handler request)

      (and (= uri "/api/save-page-text") (= method :post))
      (save-page-text-handler request)

      :else nil)))  ; return nil to pass through to next middleware
