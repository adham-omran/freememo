(ns freememo.logging
  (:require [taoensso.telemere :as tel]
            [clojure.string :as str]
            #?@(:clj [[clojure.java.io :as io]
                      [freememo.config :as config]
                      [taoensso.telemere.postal :as tel-postal]])))

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
   (def ^:private format-alert-signal (tel/format-signal-fn {})))

#?(:clj
   (defn- alert-email-body
     "Email-safe rendering of a Telemere signal. Replaces the '<<< error <<<'
      marker line with 'Error' and drops the closing '>>> error >>>' line;
      any remaining line-leading '>' run becomes '›' — mail clients
      quote-collapse '>'-led lines, and space-indenting doesn't survive
      RFC 3676 space-unstuffing."
     [signal]
     (-> (format-alert-signal signal)
       (str/replace #"(?m)^<<< error <<<$" "Error")
       (str/replace #"(?m)\n?>>> error >>>$" "")
       (str/replace #"(?m)^[ \t]*>+"
         (fn [lead] (str/replace lead ">" "›"))))))

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

     ;; File logging — writes to ./logs/app.log. handler:file rotates by
     ;; default: 4MB × 8 gzipped parts, :monthly interval × 6 retained.
     (let [log-dir (io/file "logs")]
       (.mkdirs log-dir)
       (tel/add-handler! :log-file
         (tel/handler:file
           {:path "logs/app.log"
            :output-fn (tel/format-signal-fn {})})))

     ;; Billing-failure alert email — fires ONLY for
     ;; :freememo.credits/credit-charge-failed (a swallowed debit failure after
     ;; a successful OpenAI call; see plans/credit-charge-retry-alert.md).
     ;; Telemere's handler harness contains send failures (an alert path that
     ;; can throw would re-create the swallowed-error problem one level up) and
     ;; rate-limits via handler:postal's default :limit caps (max 5/min … 30/6h).
     (if-let [{:keys [host port user pass from to]} (config/smtp-config)]
       (do (tel/add-handler! :alert/postal
             (tel-postal/handler:postal
               {:conn-opts {:host host :port port :user user :pass pass :ssl true}
                :msg-opts  {:from from :to to}
                :subject-fn (fn [signal]
                              (str "[freememo] credit charge failed user="
                                (get-in signal [:data :user-id])))
                :body-fn alert-email-body})
             {:async {:mode :dropping :buffer-size 64 :n-threads 1}
              :min-level :error
              :id-filter :freememo.credits/credit-charge-failed})
           (tel/log! {:level :info :id ::alert-email
                      :data {:host host :to to}}
             "Billing-failure alert email enabled"))
       (tel/log! {:level :info :id ::alert-email}
         "Billing-failure alert email disabled (no SMTP config)"))

     ;; Catch uncaught exceptions on any thread (e.g. Electric reactor, futures)
     (Thread/setDefaultUncaughtExceptionHandler
       (reify Thread$UncaughtExceptionHandler
         (uncaughtException [_ thread ex]
           (tel/error! {:id ::uncaught-exception
                        :data {:thread (.getName thread)}}
             ex))))))
