(ns freememo.config
  "Deployment configuration — three buckets (see plans/credits-wayl-payment-system.md §5.3):

   1. Secrets — env vars only, never committed (Wayl token, webhook secret,
      platform OpenRouter key).
   2. Deployment flags — env vars (CREDITS_ENABLED, WAYL_HOST, WAYL_LINK_ENV).
   3. Economic tunables — config.edn (gitignored); config.example.edn committed.

   config.edn is read once at load from the working directory (or classpath
   fallback). Editing it requires a restart — pricing must not drift mid-run."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [taoensso.telemere :as tel]))

;; Defined further down (defonce config); forward-declared so the env-fallback
;; helpers below can reference it.
(declare config)

;; ── Deployment flags + secrets — env first, config.edn fallback ──
;;
;; Env vars take precedence (matches 12-factor / CI patterns). config.edn is
;; gitignored, so storing secrets there is no less safe than env — it's just
;; less ergonomic for multi-stage deploys. Either source works.

(defn- env-or-config
  "Resolve `env-var` from env, falling back to `config-path` in config.edn.
   Returns nil if neither is set."
  [env-var config-path]
  (or (some-> (System/getenv env-var) str/trim not-empty)
      (get-in config config-path)))

(defn credits-enabled?
  "True in official deployment (credits on, platform key). False = self-host.
   Accepts boolean true or string \"true\" (case-insensitive) from either source."
  []
  (let [v (env-or-config "CREDITS_ENABLED" [:deployment :credits-enabled?])]
    (or (true? v)
        (= "true" (some-> v str str/lower-case str/trim)))))

(defn wayl-host
  "Which Wayl API host to call: \"live\" → api.thewayl.com, \"test\" →
   api.thewayl-staging.com. Defaults to \"live\" because Wayl's staging host
   currently serves a placeholder cert. Sandbox vs real charging is controlled
   independently by `wayl-link-env`, NOT by which host you hit."
  []
  (or (env-or-config "WAYL_HOST" [:deployment :wayl-host]) "live"))

(defn wayl-link-env
  "Per-link environment sent to Wayl as the `env` field: \"test\" = sandbox
   (no real charge), \"live\" = real payment. Defaults to \"test\" so a
   misconfigured deployment cannot charge real money."
  []
  (or (env-or-config "WAYL_LINK_ENV" [:deployment :wayl-link-env]) "test"))

(defn auth-mode
  "Which login controls the landing page surfaces: :password | :google | :both.
   Source: AUTH_MODE env → [:deployment :auth-mode] config.edn → default
   :google in official (credits) deployments, :password in self-host.
   Post: one of :password :google :both (default applied on blank/unknown)."
  []
  (let [raw (some-> (env-or-config "AUTH_MODE" [:deployment :auth-mode])
              str str/lower-case str/trim)]
    (case raw
      "password" :password
      "google"   :google
      "both"     :both
      (if (credits-enabled?) :google :password))))

(defn cookie-secure?
  "Whether the session cookie carries the Secure attribute (HTTPS-only).
   COOKIE_SECURE env, default true. Set false for plain-HTTP localhost/LAN.
   Post: boolean — false only when explicitly set to \"false\"."
  []
  (let [v (env-or-config "COOKIE_SECURE" [:deployment :cookie-secure?])]
    (not (or (false? v)
             (= "false" (some-> v str str/lower-case str/trim))))))

(defn platform-openrouter-api-key
  "OpenRouter key for the OCR model picker (topology A1: all OCR lanes route
   through one OpenRouter key). nil when unconfigured."
  []
  (env-or-config "PLATFORM_OPENROUTER_API_KEY" [:secrets :platform-openrouter-api-key]))

(defn wayl-merchant-token []
  (env-or-config "WAYL_MERCHANT_TOKEN" [:secrets :wayl-merchant-token]))

(defn wayl-webhook-secret []
  (env-or-config "WAYL_WEBHOOK_SECRET" [:secrets :wayl-webhook-secret]))

(defn smtp-config
  "SMTP settings for the billing-failure alert email, or nil when not configured.
   Pre:  config.edn :secrets and/or SMTP_* / ALERT_* env vars (env wins per key).
   Post: nil (alerting disabled — the self-host default), or a complete map
         {:host :port :user :pass :from :to} with no nil values.
   Requires host, user, pass and alert-to; :port defaults to 465 (implicit TLS),
   :from defaults to :user."
  []
  (let [host (env-or-config "SMTP_HOST" [:secrets :smtp-host])
        user (env-or-config "SMTP_USER" [:secrets :smtp-user])
        pass (env-or-config "SMTP_PASS" [:secrets :smtp-pass])
        to   (env-or-config "ALERT_TO"  [:secrets :alert-to])]
    (when (and host user pass to)
      {:host host
       :port (or (let [p (env-or-config "SMTP_PORT" [:secrets :smtp-port])]
                   (if (string? p) (parse-long p) p))
                 465)
       :user user
       :pass pass
       :from (or (env-or-config "ALERT_FROM" [:secrets :alert-from]) user)
       :to   to})))

;; ── Prod default model (set at boot by src-prod/prod.cljc) ──
;;
;; Credits-enabled deployments set a default card-generation model :id. The
;; constant lives in src-prod (only on the prod classpath); src-prod/prod.cljc
;; reset!s this atom at namespace-load time, before any traffic. settings/get-model
;; uses it as the default when the user has no allowed saved selection; nil in
;; dev/self-host, where get-model falls back to card-models/default-id.

(defonce ^{:doc "Prod default card model :id, set by src-prod/prod.cljc at boot. nil = self-host / dev."}
  !prod-model (atom nil))

;; ── Economic tunables (config.edn) ──

(defn- load-edn [source]
  (when source
    (try
      (edn/read-string (slurp source))
      (catch Exception e
        (tel/error! {:id ::config-read :data {:source (str source)}} e)
        nil))))

(defn- load-config
  "Read config.edn from the working directory, else the classpath, else {}."
  []
  (or (load-edn (let [f (io/file "config.edn")] (when (.exists f) f)))
      (load-edn (io/resource "config.edn"))
      {}))

(defonce ^{:doc "Parsed config.edn. Stable for the process lifetime (restart to reload)."}
  config (load-config))

;; ── Build metadata (electric-manifest.edn, written by src-build/build.clj) ──

(defonce ^{:private true
           :doc "Deployed short git SHA baked into electric-manifest.edn at build
   time (build.clj :git-commit). \"dev\" when no manifest is on the classpath
   (dev/REPL — build-client hasn't run)."}
  git-commit-sha
  (or (:git-commit (load-edn (io/resource "electric-manifest.edn"))) "dev"))

(defn git-commit
  "Short git SHA of the running build, for the Settings → About readout.
   Returns \"dev\" outside a built artifact."
  []
  git-commit-sha)

(defn- credits-config [] (:credits config))

(defn markup
  "Markup multiplier on OpenAI cost (debit side). nil when unconfigured."
  []
  (:markup (credits-config)))

(defn fx-iqd-per-usd
  "IQD per 1 USD. nil when unconfigured."
  []
  (:fx-iqd-per-usd (credits-config)))

(defn ocr-model-allowlist
  "OCR-model :ids (from freememo.ocr-models/registry) a credits-mode user may
   pick — each carries its own OpenRouter cost (decision 4.2.1: allow-list, the
   user chooses the OCR cost). Empty vector when unconfigured.
   Post: a vector of id strings; empty ⇒ no picker for paying users (the pinned
   default stands). Ignored in self-host (credits disabled), where all registry
   models are offered."
  []
  (:ocr-model-allowlist (credits-config) []))

(defn card-model-allowlist
  "Card-generation model :ids (from freememo.card-models/registry) a credits-mode
   user may pick — each carries its own OpenRouter cost. Empty vector when
   unconfigured (⇒ all registry models offered). Ignored in self-host, where all
   registry models are offered."
  []
  (:card-model-allowlist (credits-config) []))

(defn presets
  "Top-up amounts in IQD shown as buttons. Empty vector when unconfigured."
  []
  (:presets (credits-config) []))

(defn signup-grant
  "One-time IQD grant for new users. 0 when unconfigured."
  []
  (:signup-grant (credits-config) 0))

(defn grandfather-grant
  "One-time IQD grant for pre-existing users at migration. 0 when unconfigured."
  []
  (:grandfather-grant (credits-config) 0))
