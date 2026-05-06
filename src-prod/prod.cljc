(ns prod ; jetty 10+ – the default
  #?(:cljs (:require-macros [prod :refer [comptime-resource]]))
  (:require
   freememo.main

   #?(:clj [ring.adapter.jetty :as ring])
   #?(:clj [ring.util.response :as ring-response])
   #?(:clj [ring.middleware.not-modified :refer [wrap-not-modified]])
   #?(:clj [ring.middleware.params :refer [wrap-params]])
   #?(:clj [ring.middleware.multipart-params :refer [wrap-multipart-params]])
   #?(:clj [ring.middleware.resource :refer [wrap-resource]])
   #?(:clj [ring.middleware.content-type :refer [wrap-content-type]])
   #?(:clj [ring.middleware.session :refer [wrap-session]])
   #?(:clj [ring.middleware.session.cookie :refer [cookie-store]])
   #?(:clj [hyperfiddle.electric-ring-adapter3 :as electric-ring])
   #?(:cljs [hyperfiddle.electric-client3 :as electric-client])

   #?(:clj clojure.edn)
   #?(:clj clojure.java.io)
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [freememo.logging :as logging])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.api :as api])))

(defmacro comptime-resource [filename] (some-> filename clojure.java.io/resource slurp clojure.edn/read-string))

(declare wrap-prod-index-page wrap-ensure-cache-bust-on-server-deployment)

#?(:clj
   (defn wrap-api-routes [handler]
     (fn [request]
       (or (api/api-routes request)
           (handler request)))))

#?(:clj
   (def upload-routes #{"/api/upload-pdf" "/api/upload-epub"}))

#?(:clj
   (defn wrap-route-body-size
     "Reject requests whose Content-Length exceeds the per-route cap, before
      any body parsing runs."
     [handler]
     (fn [request]
       (let [uri (:uri request)
             method (:request-method request)
             max-bytes (cond
                         (and (= method :post) (upload-routes uri)) 104857600
                         (and (= method :post) (.startsWith ^String uri "/api/")) 10485760
                         :else nil)
             content-length (some-> (get-in request [:headers "content-length"]) Long/parseLong)]
         (if (and max-bytes content-length (> content-length max-bytes))
           {:status 413
            :headers {"Content-Type" "application/json"}
            :body (str "{\"success\":false,\"error\":\"Request too large (limit "
                    max-bytes " bytes)\",\"code\":\"request-too-large\"}")}
           (handler request))))))

#?(:clj ; server entrypoint
   (defn -main [& {:strs [] :as args}] ; clojure.main entrypoint, args are strings
     (logging/init!)
     (let [config
           ;; Client and server versions must match in prod (dev is not concerned)
           ;; `src-build/build.clj` will compute the common version and store it in `resources/electric-manifest.edn`
           ;; On prod boot, `electric-manifest.edn`'s content is injected here.
           ;; Server is therefore aware of the program version.
           ;; The client's version is injected in the compiled .js file.
           (merge
             (comptime-resource "electric-manifest.edn")
             {:host "0.0.0.0", :port (Integer/parseInt (or (System/getenv "PORT") "8080")),
              :resources-path "public"
              ;; shadow-cljs build manifest path, to get the fingerprinted main.sha1.js file to ensure cache invalidation
              :manifest-path "public/freememo/js/manifest.edn"})]
       (log/info (pr-str config))
       (assert (string? (:hyperfiddle/electric-user-version config)))

       ;; Initialize database
       (db/setup-schema)

       (ring/run-jetty
         (-> (fn [ring-request] (-> (ring-response/not-found "Page not found") (ring-response/content-type "text/plain")))
           (wrap-prod-index-page config) ; defined below
           (wrap-resource (:resources-path config))
           (wrap-content-type)
           (wrap-not-modified)
           (wrap-ensure-cache-bust-on-server-deployment)
           (electric-ring/wrap-electric-websocket (fn [ring-request] (freememo.main/electric-boot ring-request)))
           (electric-ring/wrap-reject-stale-client config) ; ensures electric client and servers stays in sync.
           (wrap-api-routes)
           (wrap-multipart-params {:max-file-size 104857600}) ; 100 MB
           (wrap-route-body-size)
           (wrap-params)
           (wrap-session {:store (cookie-store {:key (let [secret (or (System/getenv "ENC_KEY_SECRET") "dev-enc-key-secret-change-in-prod")
                                                                      hash (-> (java.security.MessageDigest/getInstance "SHA-256")
                                                                             (.digest (.getBytes secret "UTF-8")))]
                                                                  (java.util.Arrays/copyOf hash 16))})
                          :cookie-attrs {:max-age 2592000 :same-site :lax :secure true :http-only true}}))
         {:host (:host config), :port (:port config), :join? false
          :ws-idle-timeout (* 60 1000)          ; 60 seconds in milliseconds
          :ws-max-binary-size (* 100 1024 1024) ; 100MB - for demo
          :ws-max-text-size (* 100 1024 1024)   ; 100MB - for demo
          :configurator (fn [server]
                          ;; Gzip served assets
                          (.setHandler server (doto (new org.eclipse.jetty.server.handler.gzip.GzipHandler)
                                                (.setMinGzipSize 1024)
                                                (.setHandler (.getHandler server)))))}))))

#?(:cljs ; client entrypoint
   (defn ^:export -main []
     ;; client-side electric process boot happens here
     ((electric-client/reload-when-stale ; hard-reload the page to fetch new assets when a new server version is deployed
        (freememo.main/electric-boot nil)))))  ; boot client-side Electric process


#?(:clj
   (defn template
     "In string template `\"<div>$:foo/bar$</div>\"`, replace all instances of $key$
  with target specified by map `m`. Target values are coerced to string with `str`.
  E.g. (template \"<div>$:foo$</div>\" {:foo 1}) => \"<div>1</div>\" - 1 is coerced to string."
     [t m] (reduce-kv (fn [acc k v] (clojure.string/replace acc (str "$" k "$") (str v))) t m)))

#?(:clj
   (defn get-compiled-javascript-modules [manifest-path]
     (when-let [manifest (clojure.java.io/resource manifest-path)]
       (let [manifest-folder (when-let [folder-name (second (rseq (clojure.string/split manifest-path #"\/")))]
                               (str folder-name "/"))]
         (->> (slurp manifest)
           (clojure.edn/read-string)
           (reduce (fn [r module] (assoc r (keyword "hyperfiddle.client.module" (name (:name module)))
                                    (str manifest-folder (:output-name module)))) {}))))))

#?(:clj
   (defn wrap-ensure-cache-bust-on-server-deployment [next-handler]
     (fn [ring-req]
       (-> (next-handler ring-req)
         (ring-response/update-header "Cache-Control" (fn [cache-control] (or cache-control "public, max-age=0, must-revalidate")))))))

#?(:clj
   (defn wrap-prod-index-page
     "Serves `index.prod.html` with injected javascript modules from `manifest.edn`.
  `manifest.edn` is generated at client build time and contains javascript modules
  information (e.g. file location and file hash)."
     [next-handler config]
     (fn [ring-req]
       (assert (string? (:resources-path config)))
       (assert (string? (:manifest-path config)))
       (if-let [response (ring-response/resource-response (str (:resources-path config) "/freememo/index.prod.html"))]
         (if-let [module (get-compiled-javascript-modules (:manifest-path config))]
           (-> (ring-response/response (template (slurp (:body response)) (merge config module)))
             (ring-response/content-type "text/html")
             (ring-response/header "Cache-Control" "no-store")) ; never cache – this is dynamically generated content.
           (-> (ring-response/not-found (pr-str ::missing-shadow-build-manifest)) ; can't inject js modules
             (ring-response/content-type "text/plain")))
         ;; else – index.prod.html wasn't not found on classpath
         (next-handler ring-req)))))
