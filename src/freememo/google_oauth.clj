(ns freememo.google-oauth
  "Google OAuth 2.0 sign-in flow."
  (:require
    [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.java.io :as io]
    [clojure.string]
    [taoensso.telemere :as tel]
    [freememo.db :as db]
    [freememo.crypto :as crypto])
  (:import [java.util Base64]))

;; Load credentials from resources/google_client.json (Google Cloud Console download format).
;; Falls back to environment variables if the file is absent.
(defonce ^:private credentials
  (delay
    (if-let [resource (io/resource "google_client.json")]
      (let [raw (json/parse-string (slurp resource) true)
            web (:web raw)]
        (tel/log! :info "Google OAuth: loaded credentials from google_client.json")
        web)
      (do
        (tel/log! :info "Google OAuth: google_client.json not found, using env vars")
        nil))))

;; Public because `freememo.google-drive` requests a SECOND authorization with
;; the same client. Duplicating the credential lookup there would let the two
;; flows drift onto different clients, which Google would reject at the token
;; exchange with nothing in the app to explain it.
(defn- blank->nil
  "Read a blank credential as an absent credential.

   Compose writes `GOOGLE_CLIENT_ID: \"${GOOGLE_CLIENT_ID:-}\"`, so an unset
   variable reaches the JVM as \"\" and not as no variable at all. \"\" satisfied
   every `or` below and every `boolean` above it, so the deployment reported
   itself configured, put an empty client_id into the authorization URL, and
   Google answered each sign-in with `Error 400: invalid_request`."
  [s]
  (when-not (clojure.string/blank? s) s))

(defn client-id []
  (or (blank->nil (:client_id @credentials))
      (blank->nil (System/getenv "GOOGLE_CLIENT_ID"))))

(defn client-secret []
  (or (blank->nil (:client_secret @credentials))
      (blank->nil (System/getenv "GOOGLE_CLIENT_SECRET"))))

(defn redirect-uri []
  ;; Env var takes precedence; otherwise pick the localhost callback from the file,
  ;; falling back to the first entry (production), then a hardcoded default.
  (or (blank->nil (System/getenv "GOOGLE_REDIRECT_URI"))
      (some #(when (clojure.string/includes? % "localhost") %) (:redirect_uris @credentials))
      (first (:redirect_uris @credentials))
      "http://localhost:8080/auth/google/callback"))

(defn- url-encode [s]
  (java.net.URLEncoder/encode s "UTF-8"))

(defn missing-credentials
  "Which Google credentials this deployment lacks, named as their env vars.
   Post: a vector, empty exactly when both are present and non-blank. Named
   rather than boolean, so the operator alert can say which value is blank.

   Blank is re-tested here rather than trusted from the readers above. This
   predicate answers `can anyone sign in`, and an answer that holds only while
   an invariant one layer down holds is the shape that produced the outage it
   exists to catch."
  []
  (cond-> []
    (clojure.string/blank? (client-id))     (conj "GOOGLE_CLIENT_ID")
    (clojure.string/blank? (client-secret)) (conj "GOOGLE_CLIENT_SECRET")))

(defn configured?
  "Whether this deployment can complete a Google sign-in.
   Post: true only when BOTH credentials are present and non-blank. The id alone
   is not enough — `auth-url` needs the id and `exchange-code` needs the secret,
   so an id-only deployment sends the user to Google and then fails the callback."
  []
  (empty? (missing-credentials)))

(defn report-unusable-sign-in!
  "Signal that this deployment offers Google sign-in and cannot perform it.

   Pre : the caller has established that AUTH_MODE includes google.
   Post: one :error signal, id ::sign-in-unconfigured, when a credential is
         blank, and no signal at all when none is. Returns `missing-credentials`
         either way.

   `freememo.logging/alert-routes` mails ::sign-in-unconfigured, because a blank
   credential locks every user out of the deployment and raises nothing else. No
   request fails, no exception is thrown, and the landing page still offers the
   button. The only other symptom is Google's own error page, which the operator
   never sees."
  []
  (let [missing (missing-credentials)]
    (when (seq missing)
      (tel/log! {:level :error :id ::sign-in-unconfigured
                 :data {:missing missing}}
        (str "Google sign-in is unusable: " (clojure.string/join " and " missing)
          " blank. No user can sign in.")))
    missing))

(defn auth-url
  "Build the Google OAuth authorization URL with a CSRF state parameter.
   Pre : `configured?` is true. A caller that skips that check gets an assertion
         naming the fault, rather than a URL that Google rejects with a 400 the
         caller cannot read."
  [state]
  {:pre [(configured?)]}
  (str "https://accounts.google.com/o/oauth2/v2/auth"
       "?client_id=" (url-encode (client-id))
       "&redirect_uri=" (url-encode (redirect-uri))
       "&response_type=code"
       "&scope=openid%20email%20profile"
       "&state=" (url-encode state)))

(defn- parse-jwt-payload
  "Base64-decode the payload section (index 1) of a JWT and parse as JSON.
   Does not verify the signature — we trust Google's token endpoint directly."
  [id-token]
  (let [parts (clojure.string/split id-token #"\.")
        payload (nth parts 1)
        ;; JWT uses URL-safe base64 without padding
        padded (let [r (mod (count payload) 4)]
                 (if (zero? r) payload (str payload (apply str (repeat (- 4 r) "=")))))
        decoded (.decode (Base64/getUrlDecoder) padded)]
    (json/parse-string (String. decoded "UTF-8") true)))

(defn exchange-code
  "Exchange an authorization code for Google user info.
   Returns a map with :sub, :email, :name on success, or throws on failure."
  [code]
  (let [response (http/post "https://oauth2.googleapis.com/token"
                   {:form-params {:code          code
                                  :client_id     (client-id)
                                  :client_secret (client-secret)
                                  :redirect_uri  (redirect-uri)
                                  :grant_type    "authorization_code"}
                    :as :json
                    :throw-exceptions true})
        id-token (get-in response [:body :id_token])]
    (parse-jwt-payload id-token)))

(defn find-or-create-user
  "Look up or create a user row for the given Google identity.
   Returns {:user-id ... :username ... :enc-key ... :new? ...}.
   `:new?` is true only on the sign-up path — the caller passes it to
   `freememo.deployment-hook/on-signup` and must not infer it any other way."
  [google-sub email display-name]
  (let [username (or (first (clojure.string/split (or email "") #"@"))
                     (str "user-" (subs google-sub 0 8)))
        existing (db/get-user-by-google-id google-sub)
        row      (if existing
                   existing
                   (db/upsert-google-user google-sub email username))
        user-id  (:users/id row)
        enc-key  (crypto/derive-key-for-oauth-user google-sub)]
    (when-not existing
      (tel/log! {:level :info :id ::user-signup
                 :data {:user-id user-id :email email :username username}}
        "New user signup"))
    ;; Google's `name` claim is the only source of a human-readable name; store
    ;; it on every login but never over an existing value, so a hand-curated
    ;; display_name survives.
    (db/set-display-name-if-absent! user-id display-name)
    (db/insert-user-event! user-id "login_google")
    {:user-id  user-id
     :username (or (:users/username row) username)
     :enc-key  enc-key
     :new?     (nil? existing)}))
