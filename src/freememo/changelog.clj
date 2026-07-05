(ns freememo.changelog
  "Release-notes parsing and email broadcast (server-only, REPL-driven).

   Single source of truth is the git-tracked CHANGELOG.md at the repo root,
   structured as a sequence of `## <version>` release blocks, newest first,
   each with up to three subsections: `### For users`, `### Known issues`,
   `### Technical`. Only `For users` + `Known issues` reach subscribers;
   `### Technical` never leaves the repo.

   Operator flow (run from the REPL):
     (preview-broadcast)   ; build + inspect the exact message, sends NOTHING
     (broadcast-latest!)   ; send the newest release to opted-in users (BCC)

   Discord/GitHub delivery is manual: copy the `For users`/`Known issues`
   markdown into the channel; committing CHANGELOG.md is the GitHub delivery."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.config :as config]
   [freememo.db :as db]
   [freememo.markdown :as md]
   [freememo.settings :as settings]
   [postal.core :as postal]
   [taoensso.telemere :as tel]))

;; CWD-relative — the operator runs broadcasts from the repo root in dev,
;; where CHANGELOG.md lives. Not on the classpath (it's neither src nor
;; resources), so io/resource can't find it; read the file directly.
(def changelog-path "CHANGELOG.md")

(def ^:private broadcast-headings
  "Subsections that reach subscribers, in order. `Technical` is excluded by
   omission — this vector is the single enforcement point for that contract."
  ["For users" "Known issues"])

(defn- read-changelog
  "The changelog markdown string, or nil when the file is absent."
  []
  (let [f (io/file changelog-path)]
    (when (.exists f) (slurp f))))

(defn parse-releases
  "Parse changelog markdown into release maps, newest-first.
   Post: vector of {:version <str> :body <str>}; `body` is the markdown
   between this `## ` heading and the next. Empty vector for nil/blank input."
  [changelog-md]
  (if (str/blank? changelog-md)
    []
    (->> (str/split changelog-md #"(?m)^## +")
      (drop 1) ; discard any preamble before the first `## ` heading
      (mapv (fn [chunk]
              (let [nl (str/index-of chunk "\n")
                    version (str/trim (if nl (subs chunk 0 nl) chunk))
                    body (if nl (subs chunk (inc nl)) "")]
                {:version version :body body}))))))

(defn- subsection
  "Markdown body under a `### <heading>` within a release body, trimmed, or
   nil when the heading is absent. Body runs to the next `### ` or end."
  [release-body heading]
  (let [pat (re-pattern (str "(?m)^### +"
                          (java.util.regex.Pattern/quote heading) "\\s*$"))]
    (when-let [m (re-find pat release-body)]
      (let [after (subs release-body (+ (str/index-of release-body m) (count m)))
            next-heading (re-find #"(?m)^### +" after)
            block (if next-heading
                    (subs after 0 (str/index-of after next-heading))
                    after)]
        (str/trim block)))))

(defn release-broadcast-markdown
  "Subscriber-facing markdown for a release: `For users` + `Known issues`
   each under its own H3, concatenated; `Technical` excluded.
   Post: markdown string, or nil when neither subsection has content."
  [release]
  (let [parts (for [h broadcast-headings
                    :let [body (subsection (:body release) h)]
                    :when (seq body)]
                (str "### " h "\n\n" body))]
    (when (seq parts)
      (str/join "\n\n" parts))))

(defn latest-release
  "The newest release map from CHANGELOG.md, or nil."
  []
  (first (parse-releases (read-changelog))))

(defn broadcast-recipients
  "Email addresses of users opted into email updates. Post: vector (maybe empty)."
  []
  (db/list-email-update-recipients))

(defn- subject-for [version]
  (str "FreeMemo — " version))

(defn- settings-url []
  (str settings/app-base-url "/settings"))

(defn- footer-html []
  (str "<hr style=\"margin-top:24px;border:none;border-top:1px solid #ddd\">"
    "<p style=\"font-size:12px;color:#888\">"
    "You're receiving this because you subscribed to FreeMemo updates. "
    "Manage your email preferences in "
    "<a href=\"" (settings-url) "\">Settings</a>.</p>"))

(defn- footer-text []
  (str "\n\n---\n"
    "You're receiving this because you subscribed to FreeMemo updates.\n"
    "Manage your email preferences in Settings: " (settings-url)))

(defn render-broadcast
  "Render a release into an email payload.
   Post: {:version :subject :html :text :recipient-count}, or nil when there
   is no changelog / no broadcastable content."
  ([] (render-broadcast (latest-release)))
  ([release]
   (when-let [body-md (some-> release release-broadcast-markdown)]
     {:version (:version release)
      :subject (subject-for (:version release))
      :html (str (md/parse-markdown body-md) (footer-html))
      :text (str body-md (footer-text))
      :recipient-count (count (broadcast-recipients))})))

(defn- build-message
  "postal message map for a rendered payload + recipients. BCC-only: recipients
   never see each other, and there is no `:to`.
   Pre: smtp has a :from address."
  [smtp payload recipients]
  {:from (:from smtp)
   :bcc (vec recipients)
   :subject (:subject payload)
   :body [:alternative
          {:type "text/plain; charset=utf-8" :content (:text payload)}
          {:type "text/html; charset=utf-8" :content (:html payload)}]})

(defn preview-broadcast
  "DRY RUN: build the exact message broadcast-latest! would send, print a
   summary, and return it WITHOUT sending. Works with SMTP unconfigured
   (renders with the configured :from, else a placeholder).
   Post: {:message <postal-map> :recipients <vec> :payload <render-map>}, or nil."
  []
  (if-let [payload (render-broadcast)]
    (let [recipients (broadcast-recipients)
          smtp (config/smtp-config)
          from (or (:from smtp) "updates@freememo.net")
          message (build-message {:from from} payload recipients)]
      (println "── FreeMemo changelog broadcast — DRY RUN (nothing sent) ──")
      (println "Version:    " (:version payload))
      (println "Subject:    " (:subject payload))
      (println "From:       " from)
      (println "Recipients: " (count recipients)
        (if (seq recipients) (str "→ " (str/join ", " recipients)) "(none opted in)"))
      (println "SMTP:       " (if smtp "configured"
                                  "NOT configured — broadcast-latest! would send nothing"))
      (println "── text body ──")
      (println (:text payload))
      (println "── end ──")
      {:message message :recipients recipients :payload payload})
    (do (println "No broadcastable changelog entry (missing CHANGELOG.md, or the "
              "newest release has no `For users`/`Known issues` content).")
        nil)))

(defn broadcast-latest!
  "Send the newest changelog release to opted-in subscribers via SMTP (BCC).
   Pre:  config/smtp-config non-nil, CHANGELOG.md has a broadcastable release,
         and ≥1 opted-in recipient; else no-op with a reason.
   Post: {:sent <n> :version <v> :result <postal>} on send, else
         {:sent 0 :reason <:no-smtp|:no-content|:no-recipients>}.
   NOTE: no send-log — re-invoking re-sends (accepted risk, v1)."
  []
  (let [smtp (config/smtp-config)
        payload (render-broadcast)
        recipients (broadcast-recipients)]
    (cond
      (nil? smtp)
      (do (tel/log! {:level :warn :id ::broadcast} "No SMTP config — nothing sent")
          {:sent 0 :reason :no-smtp})

      (nil? payload)
      (do (tel/log! {:level :warn :id ::broadcast} "No broadcastable changelog entry")
          {:sent 0 :reason :no-content})

      (empty? recipients)
      (do (tel/log! {:level :info :id ::broadcast} "No opted-in recipients")
          {:sent 0 :reason :no-recipients})

      :else
      (let [result (postal/send-message
                     {:host (:host smtp) :port (:port smtp)
                      :user (:user smtp) :pass (:pass smtp) :ssl true}
                     (build-message smtp payload recipients))]
        (tel/log! {:level :info :id ::broadcast
                   :data {:version (:version payload)
                          :recipients (count recipients)
                          :code (:code result)}}
          "Changelog broadcast sent")
        {:sent (count recipients) :version (:version payload) :result result}))))
