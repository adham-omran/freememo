(ns dev-metadata
  "Project metadata navigator. Boots AgentMain on a separate port to serve an
   HFQL navigator over deps.edn, classpath JARs, and JVM stats. Dev-only —
   lives in src-dev and runs orthogonal to the user app. AgentMain is reused
   standalone (no cloud-proxy, no agent tunnel)."
  (:require [hyperfiddle.electric3 :as e]
            #?(:clj [hyperfiddle.hfql2 :as hfql :refer [hfql]])
            [hyperfiddle.navigator-agent-entrypoint :refer [AgentMain]]
            #?(:clj [clojure.string :as str])
            #?(:clj [clojure.tools.deps :as deps])
            #?(:clj [clojure.tools.logging :as log])
            #?(:clj [ring.adapter.jetty :as ring])
            #?(:clj [ring.util.response :as ring-response])
            #?(:clj [ring.middleware.params :refer [wrap-params]])
            #?(:clj [ring.middleware.resource :refer [wrap-resource]])
            #?(:clj [ring.middleware.content-type :refer [wrap-content-type]])
            #?(:clj [hyperfiddle.electric-ring-adapter3 :as electric-ring]))
  #?(:clj (:import [java.lang.management ManagementFactory]
                   [java.util.jar JarFile])))

#?(:clj
   (defn list-project-jars []
     (let [cp (System/getProperty "java.class.path")]
       (->> (str/split cp #"[;:]")
            (filter #(.endsWith ^String % ".jar"))
            (map #(JarFile. (clojure.java.io/file %)))))))

#?(:clj
   (defn jar-version [^JarFile jar]
     (some-> (.getManifest jar) (.getMainAttributes) (.getValue "Implementation-Version"))))

#?(:clj
   (defn electric-and-missionary-jars []
     (->> (list-project-jars)
          (filter (fn [^JarFile j]
                    (let [n (.getName j)]
                      (or (.contains n "/com/hyperfiddle/electric")
                          (.contains n "/missionary/")
                          (.contains n "missionary-")
                          (.contains n "electric-"))))))))

#?(:clj
   (defn metadata-sitemap []
     {`project-deps
      (hfql {(deps/slurp-deps (clojure.java.io/file "deps.edn"))
             [{:deps [count keys]}]})

      `electric-deps
      (hfql {(electric-and-missionary-jars) {* [.getName jar-version]}})

      `project-classpath
      (hfql {(list-project-jars) {* [.getName jar-version .getManifest]}})

      `jvm
      (hfql {(ManagementFactory/getOperatingSystemMXBean)
             [.getArch .getAvailableProcessors .getCpuLoad .getSystemCpuLoad type]})}))

(def setup-fn (constantly {}))

(defn electric-boot [ring-request]
  #?(:clj  (e/boot-server {} AgentMain
             (e/server ring-request)
             (e/server (metadata-sitemap))
             (e/server setup-fn))
     :cljs (e/boot-client {} AgentMain
             (e/server (e/amb))
             (e/server (e/amb))
             (e/server (e/amb)))))

(declare browser-process)
#?(:cljs
   (defn ^:dev/after-load ^:export -main []
     (set! browser-process
       ((electric-boot nil)))))

#?(:cljs
   (defn ^:dev/before-load stop! []
     (when browser-process (browser-process))
     (set! browser-process nil)))

#?(:clj (defonce ^:private !server (atom nil)))

#?(:clj
   (defn start!
     "Boot the metadata navigator on `port` (caller passes PORT+1 so it sits one
      above the app). Binds and logs the same port — no drift."
     [port]
     (when-not @!server
       (reset! !server
         (ring/run-jetty
           (-> (fn [_ring-request]
                 (-> (ring-response/resource-response "index.metadata.html" {:root "public/freememo"})
                     (ring-response/content-type "text/html")))
               (wrap-resource "public")
               (wrap-content-type)
               (electric-ring/wrap-electric-websocket
                 (fn [ring-request] (electric-boot ring-request)))
               (wrap-params))
           {:host "0.0.0.0", :port port, :join? false
            :ws-idle-timeout (* 60 1000)
            :ws-max-binary-size (* 100 1024 1024)
            :ws-max-text-size (* 100 1024 1024)}))
       (log/info (str "👉 http://0.0.0.0:" port " (project metadata)")))))

#?(:clj
   (defn stop! []
     (when-let [s @!server]
       (.stop s)
       (reset! !server nil))))
