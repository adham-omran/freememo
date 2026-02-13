(ns electric-starter-app.api
  "REST API handlers for file upload."
  (:require
    [electric-starter-app.pdf :as pdf]
    [clojure.java.io :as io]))

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

(defn api-routes [request]
  (let [uri (:uri request)
        method (:request-method request)]
    (cond
      (and (= uri "/api/upload-pdf") (= method :post))
      (upload-pdf-handler request)

      :else nil)))  ; return nil to pass through to next middleware
