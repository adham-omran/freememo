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

(def link-expires-in
  "How long a new link stays payable. It is Wayl's own default, sent
   explicitly so the lifetime is stated here and not assumed. An expired link
   keeps reporting `Created` (plans/wayl-link-lifecycle.md, 1.4), so nothing
   but `invalidate-link!` closes one."
  "1h")

(defn- request-opts
  "clj-http options shared by every Wayl call: the merchant header, a JSON
   body, no throw on a non-2xx status, and 15 s timeouts."
  [token]
  {:headers {"X-WAYL-AUTHENTICATION" token
             "Content-Type" "application/json"}
   :as :json
   :throw-exceptions false
   :socket-timeout 15000
   :connection-timeout 15000})

(defn- success? [resp]
  (<= 200 (:status resp) 299))

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
  [{:keys [reference-id amount-iqd webhook-url redirection-url currency-suffix? label]
    :or {label "FreeMemo credits"}}]
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
                  :lineItem [{:label label :amount amount-iqd :type "increase"}]
                  :webhookUrl webhook-url
                  :webhookSecret secret
                  :redirectionUrl redirection-url
                  :linkExpiresIn link-expires-in}
            resp (http/post (str (base-url) "/api/v1/links")
                   (assoc (request-opts token) :body (json/generate-string body)))
            data (get-in resp [:body :data])]
        (if (and (success? resp) data)
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
   Returns {:ok true :status \"...\" :total t} or {:ok false :error msg}.
   `:total` is as Wayl sent it: a string on this endpoint."
  [reference-id]
  (try
    (let [token (config/wayl-merchant-token)]
      (when-not token (throw (ex-info "WAYL_MERCHANT_TOKEN not set" {})))
      (let [resp (http/get (str (base-url) "/api/v1/links/" reference-id)
                   (request-opts token))
            data (get-in resp [:body :data])]
        (if (and (success? resp) data)
          {:ok true :status (:status data) :total (:total data)}
          {:ok false :error (or (get-in resp [:body :message]) "Wayl lookup failed")})))
    (catch Exception e
      (tel/error! {:id ::get-link-status :data {:reference-id reference-id}} e)
      {:ok false :error "Could not reach payment provider"})))

(defn invalidate-link!
  "Close an unpaid link at Wayl, so no one can pay it.
   Pre:  merchant token configured.
   Post: {:ok true :status s :total t} with the link's status after the call,
         or {:ok false :http-status n :error msg} when Wayl refused (measured:
         400 \"We can't invalidate link that is already [Cancelled]\" and 404
         for an unknown reference), or {:ok false :error msg} when the call
         failed. A refusal is expected for a link already closed or paid, so it
         logs at :info and mails nothing (plans/wayl-link-lifecycle.md 1.1)."
  [reference-id]
  (try
    (let [token (config/wayl-merchant-token)]
      (when-not token (throw (ex-info "WAYL_MERCHANT_TOKEN not set" {})))
      (let [resp (http/post (str (base-url) "/api/v1/links/" reference-id "/invalidate")
                   (request-opts token))
            data (get-in resp [:body :data])]
        (if (and (success? resp) (:status data))
          {:ok true :status (:status data) :total (:total data)}
          (let [error (or (get-in resp [:body :message]) "Wayl refused to invalidate the link")]
            (tel/log! {:level :info :id ::invalidate-link-refused
                       :data {:reference-id reference-id :status (:status resp) :error error}}
              "Wayl refused to invalidate a link")
            {:ok false :http-status (:status resp) :error error}))))
    (catch Exception e
      (tel/error! {:id ::invalidate-link :data {:reference-id reference-id}} e)
      {:ok false :error "Could not reach payment provider"})))

(defn verify-auth-key
  "Ask Wayl whether the merchant token is valid.
   Post: {:ok true} on a 2xx answer. {:ok false :error msg} on any other status,
         on an exception, or when no token is configured. Logs nothing: the
         caller logs a change of state, so a repeated failure stays quiet."
  []
  (try
    (if-let [token (config/wayl-merchant-token)]
      (let [resp (http/get (str (base-url) "/api/v1/verify-auth-key") (request-opts token))]
        (if (success? resp)
          {:ok true}
          {:ok false :error (or (get-in resp [:body :message]) (str "HTTP " (:status resp)))}))
      {:ok false :error "WAYL_MERCHANT_TOKEN not set"})
    (catch Exception e
      {:ok false :error (or (ex-message e) (str (class e)))})))

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
