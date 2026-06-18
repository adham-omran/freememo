(ns dev ; jetty 10+ – the default
  (:require
   freememo.main
   #?(:clj dev-metadata)

   #?(:clj [shadow.cljs.devtools.api :as shadow-cljs-compiler])
   #?(:clj [shadow.cljs.devtools.server :as shadow-cljs-compiler-server])
   #?(:clj [clojure.string])
   #?(:clj [clojure.tools.logging :as log])
   #?(:clj [freememo.logging :as logging])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.api :as api])
   #?(:clj [freememo.config :as config])
   #?(:clj [freememo.quota :as quota])

   #?(:clj [nrepl.server :as nrepl])
   #?(:clj [ring.adapter.jetty :as ring])
   #?(:clj [ring.util.response :as ring-response])
   #?(:clj [ring.middleware.params :refer [wrap-params]])
   #?(:clj [ring.middleware.multipart-params :refer [wrap-multipart-params]])
   #?(:clj [ring.middleware.resource :refer [wrap-resource]])
   #?(:clj [ring.middleware.content-type :refer [wrap-content-type]])
   #?(:clj [ring.middleware.session :refer [wrap-session]])
   #?(:clj [ring.middleware.session.cookie :refer [cookie-store]])
   #?(:clj [hyperfiddle.electric-ring-adapter3 :as electric-ring])))

;; Mirror src-prod/prod.cljc so dev with `:credits-enabled? true` doesn't throw
;; ::prod-model-missing on first LLM call. Bump together with src-prod.
#?(:clj (reset! config/!prod-model "gpt-5.1"))

(comment (-main)) ; repl entrypoint

#?(:clj
   (defn wrap-api-routes [handler]
     (fn [request]
       (or (api/api-routes request)
           (handler request)))))

#?(:clj
   (defn wrap-no-store
     "Dev only: mark static-asset responses uncacheable so CSS/JS/HTML edits
      show on a normal reload (no hard-refresh needed). Wrap ONLY the static
      stack — prod serves hashed assets and never uses this."
     [handler]
     (fn [request]
       (some-> (handler request)
         (update :headers assoc "Cache-Control" "no-store")))))

#?(:clj
   (def upload-routes #{"/api/upload-pdf" "/api/upload-epub"}))

#?(:clj
   (defn- parse-content-length
     "Parse a Content-Length header. Returns Long for digit-only values, nil otherwise.
      Rejects negative, signed, and non-numeric strings without throwing."
     [header]
     (when (and header (re-matches #"\d+" header))
       (Long/parseLong header))))

#?(:clj
   (defn wrap-route-body-size
     "Cap request body size on /api/*. Upload routes use `quota/per-file-max-bytes`
      (nil when env=0 → unlimited); 10 MB on other /api/* (request-shape guard).
      Returns 411 for chunked uploads (no body length to enforce upfront);
      413 when Content-Length exceeds the cap."
     [handler]
     (fn [request]
       (let [uri (:uri request)
             method (:request-method request)
             max-bytes (cond
                         (and (= method :post) (upload-routes uri))
                         (when-not (quota/unlimited? quota/per-file-max-bytes)
                           quota/per-file-max-bytes)

                         (and (= method :post) (.startsWith ^String uri "/api/")) 10485760
                         :else nil)
             transfer-encoding (some-> (get-in request [:headers "transfer-encoding"])
                                 clojure.string/lower-case)
             content-length (parse-content-length (get-in request [:headers "content-length"]))]
         (cond
           (and max-bytes transfer-encoding (clojure.string/includes? transfer-encoding "chunked"))
           (do (api/log-upload-failure! :freememo.api/upload-chunked-rejected request
                 {:user-id (or (get-in request [:session :user-id]) :anonymous)
                  :uri uri :transfer-encoding transfer-encoding})
               {:status 411
                :headers {"Content-Type" "application/json"}
                :body "{\"success\":false,\"error\":\"Length Required (chunked uploads not accepted)\",\"code\":\"length-required\"}"})

           (and max-bytes content-length (> content-length max-bytes))
           (do (api/log-upload-failure! :freememo.api/upload-too-large request
                 {:user-id (or (get-in request [:session :user-id]) :anonymous)
                  :uri uri :content-length content-length :limit max-bytes})
               {:status 413
                :headers {"Content-Type" "application/json"}
                :body (str "{\"success\":false,\"error\":\"Request too large (limit "
                        max-bytes " bytes)\",\"code\":\"request-too-large\"}")})

           :else
           (handler request))))))

#?(:clj
   (defn start-nrepl-on-free-port!
     "Bind an nREPL server on the first free port in [base-port, base-port + attempts).
      Concurrent worktree dev servers each claim the next free port."
     [base-port attempts]
     (loop [port base-port]
       (when (>= port (+ base-port attempts))
         (throw (ex-info "No free nREPL port" {:base-port base-port :attempts attempts})))
       (if-let [server (try (nrepl/start-server :port port)
                            (catch java.net.BindException _ nil))]
         (do (log/info (str "nREPL server started on port " port))
             server)
         (recur (inc port))))))

#?(:clj ; server entrypoint
   (defn -main [& args]
     (logging/init!)
     (log/info "Starting Electric compiler and server...")

     ;; Initialize database
     (db/setup-schema)

     (def nrepl-server (start-nrepl-on-free-port! 7888 20))

     (def http-port (Integer/parseInt (or (System/getenv "PORT") "8080")))

     (shadow-cljs-compiler-server/start!)
     (shadow-cljs-compiler/watch :dev)

     (def server (ring/run-jetty
                   (-> ; ring middlewares – applied bottom up:
                     (fn [ring-request] ; 6. index page fallback
                         (-> (ring-response/resource-response "index.dev.html" {:root "public/freememo"})
                           (ring-response/content-type "text/html")))
                     (wrap-resource "public") ; 5. serve assets from disk.
                     (wrap-content-type) ; 4. boilerplate – to server assets with correct mime/type.
                     (wrap-no-store) ; 4b. dev: never cache static assets (so CSS/HTML edits show on reload)
                     (electric-ring/wrap-electric-websocket ; 3. install Electric server.
                       (fn [ring-request] (freememo.main/electric-boot ring-request))) ; boot server-side Electric process
                     (wrap-api-routes) ; 2. API routes
                     (wrap-multipart-params) ; 1c. parse multipart form data
                     (wrap-route-body-size) ; 1b. Reject chunked + cap Content-Length (100 MB uploads, 10 MB other /api/*)
                     (wrap-params) ; 1a. boilerplate – parse request URL parameters.
                     (wrap-session {:store (cookie-store {:key (let [secret (or (System/getenv "ENC_KEY_SECRET") "dev-enc-key-secret-change-in-prod")
                                                                              hash (-> (java.security.MessageDigest/getInstance "SHA-256")
                                                                                     (.digest (.getBytes secret "UTF-8")))]
                                                                          (java.util.Arrays/copyOf hash 16))})
                                    :cookie-attrs {:max-age 2592000 :same-site :lax :http-only true}})) ; 0. session middleware (outermost – runs first)
                   {:host "0.0.0.0", :port http-port, :join? false
                    :ws-idle-timeout (* 60 1000) ; 60 seconds in milliseconds
                    :ws-max-binary-size (if (quota/unlimited? quota/per-file-max-bytes)
                                          Long/MAX_VALUE quota/per-file-max-bytes)
                    :ws-max-text-size   (if (quota/unlimited? quota/per-file-max-bytes)
                                          Long/MAX_VALUE quota/per-file-max-bytes)}))
     (log/info (str "👉 http://0.0.0.0:" http-port))

     (dev-metadata/start!)))

(declare browser-process)
#?(:cljs ; client entrypoint
   (defn ^:dev/after-load ^:export -main []
     (set! browser-process
       ((freememo.main/electric-boot nil)))))  ; boot client-side Electric process

#?(:cljs
   (defn ^:dev/before-load stop! [] ; for hot code reload at dev time
     (when browser-process (browser-process)) ; tear down electric browser process
     (set! browser-process nil)))

(comment
  (shadow-cljs-compiler-server/stop!)
  (.stop server) ; stop jetty server
  (.close nrepl-server) ; stop nrepl server
  (dev-metadata/stop!) ; stop metadata jetty server
  )
