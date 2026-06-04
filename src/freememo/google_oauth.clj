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

(defn- client-id []
  (or (:client_id @credentials) (System/getenv "GOOGLE_CLIENT_ID")))

(defn- client-secret []
  (or (:client_secret @credentials) (System/getenv "GOOGLE_CLIENT_SECRET")))

(defn- redirect-uri []
  ;; Env var takes precedence; otherwise pick the localhost callback from the file,
  ;; falling back to the first entry (production), then a hardcoded default.
  (or (System/getenv "GOOGLE_REDIRECT_URI")
      (some #(when (clojure.string/includes? % "localhost") %) (:redirect_uris @credentials))
      (first (:redirect_uris @credentials))
      "http://localhost:8080/auth/google/callback"))

(defn- url-encode [s]
  (java.net.URLEncoder/encode s "UTF-8"))

(defn configured?
  "Returns true if Google OAuth credentials are available."
  []
  (boolean (client-id)))

(defn auth-url
  "Build the Google OAuth authorization URL with a CSRF state parameter."
  [state]
  (str "https://accounts.google.com/o/oauth2/v2/auth"
       "?client_id=" (url-encode (or (client-id) ""))
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
   Returns {:user-id ... :username ... :enc-key ...}."
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
    (db/insert-user-event! user-id "login_google")
    {:user-id  user-id
     :username (or (:users/username row) username)
     :enc-key  enc-key}))
