(ns freememo.wayl
  "Wayl.io payment integration — link creation + webhook signature verification.

   Secrets/flags come from env via freememo.config (§5.3). All amounts in IQD.
   See plans/credits-wayl-payment-system.md §5.5/§5.6."
  (:require
   [freememo.config :as config]
   [clj-http.client :as http]
   [cheshire.core :as json]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   [javax.crypto Mac]
   [javax.crypto.spec SecretKeySpec]
   [java.security MessageDigest]))

(defn- base-url []
  (if (= "test" (config/wayl-host))
    "https://api.thewayl-staging.com"
    "https://api.thewayl.com"))

(defn- append-currency-usd
  "Append `currency=usd` to a URL using `?` if no query string, `&` otherwise.
   Wayl checkout pages render the amount in USD when this param is present;
   the contractual currency in the API body is unaffected."
  [^String url]
  (str url (if (str/includes? url "?") "&" "?") "currency=usd"))

(defn create-link!
  "Create a Wayl payment link for an order.
   Pre:  amount-iqd is an integer (Wayl minimum 1000); reference-id unique;
         webhook-url/redirection-url absolute HTTPS; merchant token + webhook
         secret configured.
   Post: returns {:ok true :url u :code c :link-id id} or {:ok false :error msg};
         no order state is mutated here (caller owns the credit_orders row).
         When `:currency-suffix?` is true, the returned :url has `currency=usd`
         appended for non-IQ checkout-page display."
  [{:keys [reference-id amount-iqd webhook-url redirection-url currency-suffix?]}]
  (try
    (let [token (config/wayl-merchant-token)
          secret (config/wayl-webhook-secret)]
      (when-not token (throw (ex-info "WAYL_MERCHANT_TOKEN not set" {})))
      (when-not secret (throw (ex-info "WAYL_WEBHOOK_SECRET not set" {})))
      (let [body {:env (config/wayl-link-env)
                  :referenceId reference-id
                  :total amount-iqd
                  :currency "IQD"
                  :customParameter ""
                  :lineItem [{:label "FreeMemo credits" :amount amount-iqd :type "increase"}]
                  :webhookUrl webhook-url
                  :webhookSecret secret
                  :redirectionUrl redirection-url}
            resp (http/post (str (base-url) "/api/v1/links")
                   {:headers {"X-WAYL-AUTHENTICATION" token
                              "Content-Type" "application/json"}
                    :body (json/generate-string body)
                    :as :json
                    :throw-exceptions false
                    :socket-timeout 15000
                    :connection-timeout 15000})
            data (get-in resp [:body :data])]
        (if (and (<= 200 (:status resp) 299) data)
          {:ok true
           :url (cond-> (:url data) currency-suffix? append-currency-usd)
           :code (:code data)
           :link-id (:id data)}
          (do (tel/log! {:level :warn :id ::create-link-failed
                         :data {:status (:status resp) :reference-id reference-id}}
                "Wayl link creation failed")
              {:ok false :error (or (get-in resp [:body :message]) "Payment provider error")}))))
    (catch Exception e
      (tel/error! {:id ::create-link :data {:reference-id reference-id}} e)
      {:ok false :error "Could not reach payment provider"})))

(defn get-link-status
  "Query Wayl for the current state of an existing link.
   Used by the post-redirect reconciliation path (5.10.2) when a webhook is
   missed or unreachable (e.g. in dev when Wayl can't reach localhost).
   Returns {:ok true :status \"...\"} or {:ok false :error msg}."
  [reference-id]
  (try
    (let [token (config/wayl-merchant-token)]
      (when-not token (throw (ex-info "WAYL_MERCHANT_TOKEN not set" {})))
      (let [resp (http/get (str (base-url) "/api/v1/links/" reference-id)
                   {:headers {"X-WAYL-AUTHENTICATION" token}
                    :as :json
                    :throw-exceptions false
                    :socket-timeout 15000
                    :connection-timeout 15000})
            data (get-in resp [:body :data])]
        (if (and (<= 200 (:status resp) 299) data)
          {:ok true :status (:status data)}
          {:ok false :error (or (get-in resp [:body :message]) "Wayl lookup failed")})))
    (catch Exception e
      (tel/error! {:id ::get-link-status :data {:reference-id reference-id}} e)
      {:ok false :error "Could not reach payment provider"})))

(defn- hmac-sha256-hex
  "Lowercase hex HMAC-SHA256 of `body-bytes` keyed by `secret`."
  [^String secret ^bytes body-bytes]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256"))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.doFinal mac body-bytes)))))

(defn verify-signature?
  "True when `signature-hex` matches HMAC-SHA256(raw-body, webhook-secret),
   compared in constant time. False when the secret/header/body is missing.
   Pre: raw-body is the exact bytes Wayl posted (not a re-serialized copy)."
  [^bytes raw-body ^String signature-hex]
  (let [secret (config/wayl-webhook-secret)]
    (boolean
      (and secret signature-hex raw-body
        (let [expected (hmac-sha256-hex secret raw-body)]
          (MessageDigest/isEqual (.getBytes expected "UTF-8")
                                 (.getBytes signature-hex "UTF-8")))))))
