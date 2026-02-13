(ns electric-starter-app.api
  "REST API handlers for file upload."
  (:require
    [electric-starter-app.pdf :as pdf]
    [electric-starter-app.db :as db]
    [clojure.java.io :as io]
    [clojure.string]))

(defn upload-pdf-handler [request]
  (try
    (if-let [file (get-in request [:params "file"])]
      (let [filename (:filename file)
            tempfile (:tempfile file)
            bytes (with-open [in (io/input-stream tempfile)]
                    (let [baos (java.io.ByteArrayOutputStream.)]
                      (io/copy in baos)
                      (.toByteArray baos)))
            result (pdf/save-pdf filename bytes)]
        (if (:success result)
          {:status 302
           :headers {"Location" "/"}
           :body ""}
          {:status 500
           :headers {"Content-Type" "text/html"}
           :body (str "<html><body><h1>Upload Failed</h1><p>" (:error result) "</p><a href='/'>Go back</a></body></html>")}))
      {:status 400
       :headers {"Content-Type" "text/html"}
       :body "<html><body><h1>Upload Failed</h1><p>No file provided</p><a href='/'>Go back</a></body></html>"})
    (catch Exception e
      (println "ERROR [upload-pdf-handler]:" (.getMessage e))
      (.printStackTrace e)
      {:status 500
       :headers {"Content-Type" "text/html"}
       :body (str "<html><body><h1>Upload Failed</h1><p>" (.getMessage e) "</p><a href='/'>Go back</a></body></html>")})))

(defn get-pdf-handler [request]
  "Serve PDF file by ID from database."
  (try
    (let [uri (:uri request)
          doc-id (-> uri (clojure.string/split #"/") last parse-long)]
      (if-let [doc (first (db/get-documents-by-id doc-id))]
        {:status 200
         :headers {"Content-Type" "application/pdf"
                   "Content-Disposition" (str "inline; filename=\"" (:documents/filename doc) "\"")}
         :body (io/input-stream (:documents/file_data doc))}
        {:status 404
         :body "PDF not found"}))
    (catch Exception e
      (println "ERROR [get-pdf-handler]:" (.getMessage e))
      {:status 500
       :body "Internal server error"})))

(defn api-routes [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= uri "/api/upload-pdf") (= method :post))
      (upload-pdf-handler request)

      (and (re-matches #"/api/pdf/\d+" uri) (= method :get))
      (get-pdf-handler request)

      :else nil)))  ; return nil to pass through to next middleware
