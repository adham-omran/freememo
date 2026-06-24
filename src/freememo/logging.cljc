(ns freememo.logging
  (:require [taoensso.telemere :as tel]
            [clojure.string :as str]
            #?@(:clj [[clojure.java.io :as io]
                      [freememo.config :as config]
                      [taoensso.telemere.postal :as tel-postal]])))

;; Plain defn wrappers safe for use inside e/defn bodies in .cljc files.
;; Telemere macros (tel/log!, tel/error!) expand to platform-specific code
;; that Electric's dual-compiler can't handle inside e/defn. These wrappers
;; are plain fns visible on both CLJ and CLJS, and route through Telemere on
;; BOTH peers — so CLJS log calls obey Telemere's min-levels instead of
;; printing unconditionally to the browser console.

(defn log-info  [msg] (tel/log! :info  msg))
(defn log-debug [msg] (tel/log! :debug msg))
(defn log-warn  [msg] (tel/log! :warn  msg))
(defn log-error [msg] (tel/log! :error msg))
(defn log-trace [msg] (tel/log! :trace msg))

#?(:cljs
   (defn init-client!
     "Client log init — call once from the client entrypoint.
      Lowers the runtime min-level to :debug in dev only (`goog.DEBUG`); prod
      keeps Telemere's :info default so :debug stays silent. Prod *builds*
      additionally elide :debug calls at compile time via the
      `taoensso.telemere.ct-min-level` JVM property set on the build alias."
     []
     (when ^boolean js/goog.DEBUG
       (tel/set-min-level! :debug))))

#?(:clj
   (def ^:private format-alert-signal (tel/format-signal-fn {})))

#?(:clj
   (defn- alert-subject-fn
     "Subject line for one alert class: \"[freememo] <tag>: <signal-id> user=N\"
      (user suffix only when the signal carries :user-id)."
     [subject-tag]
     (fn [signal]
       (let [user-id (get-in signal [:data :user-id])]
         (str "[freememo] " subject-tag ": " (some-> (:id signal) name)
           (when user-id (str " user=" user-id)))))))

#?(:clj
   (def ^:private alert-routes
     "Per-class email alert routing — each row registers one postal handler.
      Separate handlers = separate rate-cap pools, so a pipeline-failure storm
      can never starve a payment alert. :limit absent = handler:postal defaults
      (max 5/min, 10/15min, 15/h, 30/6h).
      See plans/credit-charge-retry-alert.md for the original billing alert."
     [{:handler-id :alert/money-failures
       :subject-tag "MONEY"
       :min-level :warn
       :ids #{:freememo.credits/credit-charge-failed
              :freememo.api/wayl-webhook
              :freememo.api/wayl-webhook-rejected
              :freememo.api/credits-checkout
              :freememo.api/signup-grant
              :freememo.wayl/create-link
              :freememo.wayl/create-link-failed
              :freememo.wayl/get-link-status}}
      {:handler-id :alert/business-events
       :subject-tag "event"
       :min-level :info
       :ids #{:freememo.api/wayl-credited
              :freememo.google-oauth/user-signup
              :freememo.db/signup-grant
              :freememo.db/grandfather-grant}}
      {:handler-id :alert/pipeline-failures
       :subject-tag "pipeline"
       :min-level :error
       ;; Pipeline storms summarize, they don't narrate: max 2/10min, 6/6h.
       :limit [[2 (* 10 60 1000)] [6 (* 6 60 60 1000)]]
       :ids #{:freememo.ocr/extract-text
              :freememo.ocr/extract-text-pdfbox
              :freememo.cards/generate-basic-cards
              :freememo.cards/generate-cloze-cards
              :freememo.cards/save-cards
              :freememo.cards/generate-cards-count-mismatch
              :freememo.epub/process-epub
              :freememo.pdf/save-pdf}}]))

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
   (defn- register-alert-route!
     "Register one postal handler for an alert-routes row.
      Pre: smtp is a complete config/smtp-config map; route is an alert-routes row.
      Post: handler `(:handler-id route)` delivers exactly the signals matching
      (:ids route) at >= (:min-level route), rate-capped per route."
     [{:keys [host port user pass from to]}
      {:keys [handler-id subject-tag min-level ids limit]}]
     (tel/add-handler! handler-id
       (tel-postal/handler:postal
         {:conn-opts {:host host :port port :user user :pass pass :ssl true}
          :msg-opts  {:from from :to to}
          :subject-fn (alert-subject-fn subject-tag)
          :body-fn alert-email-body})
       (cond-> {:async {:mode :dropping :buffer-size 64 :n-threads 1}
                :min-level min-level
                :id-filter ids}
         limit (assoc :limit limit)))))

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

     ;; Alert emails — only when SMTP is configured (config.edn :secrets or
     ;; env). One postal handler per alert class (see alert-routes above).
     ;; Telemere's handler harness contains send failures (an alert path that
     ;; can throw would re-create the swallowed-error problem one level up).
     (if-let [smtp (config/smtp-config)]
       (do (run! #(register-alert-route! smtp %) alert-routes)
           (tel/log! {:level :info :id ::alert-email
                      :data {:host (:host smtp) :to (:to smtp)
                             :routes (mapv :handler-id alert-routes)}}
             "Alert email routes enabled"))
       (tel/log! {:level :info :id ::alert-email}
         "Alert email disabled (no SMTP config)"))

     ;; Catch uncaught exceptions on any thread (e.g. Electric reactor, futures)
     (Thread/setDefaultUncaughtExceptionHandler
       (reify Thread$UncaughtExceptionHandler
         (uncaughtException [_ thread ex]
           (tel/error! {:id ::uncaught-exception
                        :data {:thread (.getName thread)}}
             ex))))))
