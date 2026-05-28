(ns freememo.config
  "Deployment configuration — three buckets (see plans/credits-wayl-payment-system.md §5.3):

   1. Secrets — env vars only, never committed (Wayl token, webhook secret,
      platform OpenAI key).
   2. Deployment flags — env vars (CREDITS_ENABLED, WAYL_ENV).
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

(defn platform-openai-api-key []
  (env-or-config "PLATFORM_OPENAI_API_KEY" [:secrets :platform-openai-api-key]))

(defn wayl-merchant-token []
  (env-or-config "WAYL_MERCHANT_TOKEN" [:secrets :wayl-merchant-token]))

(defn wayl-webhook-secret []
  (env-or-config "WAYL_WEBHOOK_SECRET" [:secrets :wayl-webhook-secret]))

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

(defn- credits-config [] (:credits config))

(defn markup
  "Markup multiplier on OpenAI cost (debit side). nil when unconfigured."
  []
  (:markup (credits-config)))

(defn fx-iqd-per-usd
  "IQD per 1 USD. nil when unconfigured."
  []
  (:fx-iqd-per-usd (credits-config)))

(defn model-rates
  "USD-per-1M-token rates {:input :cached-input :output} for a model, or nil."
  [model]
  (get-in (credits-config) [:models model]))

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
