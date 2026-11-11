(ns freememo.local-install-stub
  "A stub that plays freememo.net inside the test JVM, for Seam 1 tests that
   need to control what the server says (the update feed, a refused Sync Protocol).
   It answers only what a Local Install asks at start:

     GET  /api/desktop/feed   the feed in `:feed` (a map), or HTTP 500 when nil
     POST /api/desktop/id-block  a fixed id block
     POST /api/sync/meta      `:meta-status` and `:meta-body` (a map)
     GET  <path>              the bytes in `:files` ({\"/dl/app.jar\" byte-array}), for an App Update

   Change the answers with `(swap! (:state stub) assoc …)`."
  (:require [cheshire.core :as json])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]))

(defn start!
  "Post: `{:origin :state :server}`. `initial` sets `:feed`, `:meta-status`, `:meta-body`."
  [initial]
  (let [state (atom (merge {:feed nil :meta-status 200 :meta-body {}} initial))
        server (HttpServer/create (InetSocketAddress. (java.net.InetAddress/getByName "127.0.0.1") 0) 0)
        respond! (fn [^HttpExchange ex status body]
                   (let [bytes (.getBytes (if (string? body) body (json/generate-string body)) "UTF-8")]
                     (.add (.getResponseHeaders ex) "Content-Type" "application/json")
                     (.sendResponseHeaders ex status (if (zero? (alength bytes)) -1 (alength bytes)))
                     (when (pos? (alength bytes)) (with-open [o (.getResponseBody ex)] (.write o bytes)))))]
    (.createContext server "/"
      (reify HttpHandler
        (handle [_ ex]
          (try
            (let [path (.getPath (.getRequestURI ex))
                  {:keys [feed meta-status meta-body]} @state]
              (slurp (.getRequestBody ex))
              (case path
                "/api/desktop/feed" (if (do (swap! state update :feed-hits (fnil inc 0)) feed) (respond! ex 200 feed) (respond! ex 500 {"error" "down"}))
                "/api/desktop/id-block" (respond! ex 200 {"start" 10000000 "end" 20000000})
                "/api/sync/meta" (respond! ex meta-status meta-body)
                (if-let [^bytes bytes (get-in @state [:files path])]
                  (do (.sendResponseHeaders ex 200 (alength bytes))
                      (with-open [o (.getResponseBody ex)] (.write o bytes)))
                  (respond! ex 404 {"error" "not-found"}))))
            (catch Throwable _ (.close ex))))))
    (.start server)
    {:server server :state state
     :origin (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(defn stop! [{:keys [^HttpServer server]}] (.stop server 0))
