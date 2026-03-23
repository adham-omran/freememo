(ns freememo.logging
  (:require [taoensso.telemere :as tel]
            #?(:clj [clojure.java.io :as io])))

;; Plain defn wrappers safe for use inside e/defn bodies in .cljc files.
;; Telemere macros (tel/log!, tel/error!) expand to platform-specific code
;; that Electric's dual-compiler can't handle inside e/defn. These wrappers
;; are plain fns visible on both CLJ and CLJS.

(defn log-info [msg]
  #?(:clj (tel/log! :info msg)
     :cljs (js/console.log "[INFO]" msg)))

(defn log-debug [msg]
  #?(:clj (tel/log! :debug msg)
     :cljs (when js/goog.DEBUG (js/console.log "[DEBUG]" msg))))

(defn log-warn [msg]
  #?(:clj (tel/log! :warn msg)
     :cljs (js/console.warn "[WARN]" msg)))

(defn log-error [msg]
  #?(:clj (tel/log! :error msg)
     :cljs (js/console.error "[ERROR]" msg)))

(defn log-trace [msg]
  #?(:clj (tel/log! :trace msg)
     :cljs nil))

#?(:clj
   (defn init! []
     ;; SLF4J bridge is automatic via telemere-slf4j dep on classpath
     (let [level (keyword (or (System/getenv "LOG_LEVEL")
                              (if (System/getenv "PROD") "info" "debug")))]
       (tel/set-min-level! level))
     ;; Quiet noisy Java libraries — filter ALL signal kinds (including :slf4j)
     (tel/set-min-level! nil "com.zaxxer.hikari.*" :warn)
     (tel/set-min-level! nil "org.eclipse.jetty.*" :warn)
     (tel/set-min-level! nil "org.apache.pdfbox.*" :warn)
     (tel/set-min-level! nil "org.xnio.*" :warn)
     (tel/set-min-level! nil "org.jboss.*" :warn)
     (tel/set-min-level! nil "io.undertow.*" :warn)
     (tel/set-min-level! nil "org.apache.http.*" :warn)

     ;; File logging — writes to ./logs/app.log
     (let [log-dir (io/file "logs")]
       (.mkdirs log-dir)
       (tel/add-handler! :log-file
         (tel/handler:file
           {:path "logs/app.log"
            :output-fn (tel/format-signal-fn {})})))))
