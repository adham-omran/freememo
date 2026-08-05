(ns freememo.google-drive
  "Google Drive read access under the `drive.file` scope, for NeoReader syncs.

   Separate from `freememo.google-oauth` on purpose. That namespace owns sign-in
   and its scope string is `openid email profile`; adding Drive there would make
   every new user grant Drive access at signup. Drive is a SECOND authorization
   round trip with its own callback, requested only when a user asks to sync.

   `drive.file` is a non-sensitive scope (Google's scope reference), so no
   restricted-scope verification and no security assessment apply. It grants
   access to a file the user picked with the Google Picker and to nothing else.
   Google's Picker guide states that an app reaches a file granted earlier by
   obtaining a fresh token and passing the stored file id, so a bound file needs
   no second Picker interaction.

   A folder pick grants NOTHING about the folder's contents. There is therefore
   no way to list `onyx/<DEVICE>/Read/`, and the first bind is always a manual
   Picker navigation by the user."
  (:require
    [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [freememo.crypto :as crypto]
    [freememo.db :as db]
    [freememo.google-oauth :as oauth]))

(def scope
  "The only Drive scope this app ever requests."
  "https://www.googleapis.com/auth/drive.file")

(def ^:private refresh-token-setting
  "Settings key holding the user's encrypted Drive refresh token.

   A global key, not a per-topic one, so it needs no `settings-key-prefixes`
   entry in `freememo.account-import`. It IS carried by an account export, and
   arrives useless: the key that encrypted it derives from the source user's
   Google subject id, so the target cannot decrypt it and the user simply
   re-authorizes."
  "google_drive_refresh_token")

(defn- url-encode [s] (java.net.URLEncoder/encode (str s) "UTF-8"))

;; ── The shared callback and its state ──────────────────────────────
;;
;; Drive authorization returns to the SAME redirect URI as sign-in, so a
;; deployment registers one URI in Cloud Console and never registers another.
;; The earlier shape derived a second URI by string-replacing the tail of the
;; sign-in one, which needed a second Console entry per environment AND failed
;; silently when the sign-in URI did not match the pattern it replaced: the
;; replace became a no-op, and Google then handed a Drive code to the sign-in
;; handler, which read it as an id_token and failed somewhere unrelated.
;;
;; `state` is the only channel Google round-trips verbatim, so the flow marker
;; lives there. This namespace builds the state and reads it back, because a
;; format split across two namespaces is one that drifts.

(def ^:private state-marker
  "Prefix marking a `state` as belonging to a Drive authorization. A sign-in
   state is a bare nonce and never carries it."
  "drive:")

(def ^:private state-separator
  "Separates the nonce from the return path. A `|` cannot appear in either — the
   nonce is a UUID and the path is URL-encoded in the query."
  "|")

(defn authorization-state
  "Build the opaque `state` for a Drive authorization.

   Pre : `nonce` is a CSRF nonce. `return-to` is an app-relative path.
   Post: a string carrying both, marked as a Drive flow. The caller MUST put the
         same string in the session and compare it on the way back."
  [nonce return-to]
  (str state-marker nonce state-separator return-to))

(defn authorization-state?
  "Pre : `state` is a callback's `state` param, possibly nil.
   Post: true iff it belongs to a Drive authorization rather than a sign-in."
  [state]
  (str/starts-with? (or state "") state-marker))

(defn parse-state
  "Pre : `authorization-state?` is true for `state`.
   Post: {:nonce s :return-to s}. `:return-to` is whatever was passed in and is
         NOT validated here — the caller decides what paths it will honor."
  [state]
  (let [[nonce return-to] (-> state
                            (subs (count state-marker))
                            (str/split (re-pattern (java.util.regex.Pattern/quote state-separator)) 2))]
    {:nonce nonce :return-to return-to}))

(defn authorize-url
  "The consent URL that yields a Drive refresh token.

   Pre : `state` is `authorization-state` output, also stored in the session.
   Post: a Google consent URL requesting only `scope`, returning to the SAME
         redirect URI as sign-in, with `access_type=offline` and
         `prompt=consent` so a refresh token is issued. Without both, Google
         returns an access token only and every later sync would need the user
         present. `include_granted_scopes` keeps the sign-in scopes the user
         already granted."
  [state]
  (str "https://accounts.google.com/o/oauth2/v2/auth"
    "?client_id=" (url-encode (oauth/client-id))
    "&redirect_uri=" (url-encode (oauth/redirect-uri))
    "&response_type=code"
    "&scope=" (url-encode scope)
    "&access_type=offline"
    "&prompt=consent"
    "&include_granted_scopes=true"
    "&state=" (url-encode state)))

(defn- user-enc-key
  "Pre : `user-id` names a Google-authenticated user.
   Post: that user's derived encryption key, or nil when the row carries no
         `google_id` — a password user has no Drive credential and never will."
  [user-id]
  (when-let [g (:users/google_id (db/get-user-by-id user-id))]
    (crypto/derive-key-for-oauth-user g)))

(defn store-refresh-token!
  "Persist a Drive refresh token for `user-id`, encrypted.

   Pre : `code` is the authorization code from the Drive callback.
   Post: true when Google returned a refresh token and it was stored; false
         otherwise. A false return is a caller-visible failure, not an
         exception — the caller redirects with a message.
   Inv : the plaintext token never reaches the database."
  [user-id code]
  ;; `redirect_uri` MUST equal the one the authorization used, or Google rejects
  ;; the exchange. Both read `oauth/redirect-uri`, so they cannot disagree.
  (let [resp (http/post "https://oauth2.googleapis.com/token"
               {:form-params {:code code
                              :client_id (oauth/client-id)
                              :client_secret (oauth/client-secret)
                              :redirect_uri (oauth/redirect-uri)
                              :grant_type "authorization_code"}
                :as :json
                :throw-exceptions true})
        refresh (get-in resp [:body :refresh_token])]
    (if (str/blank? refresh)
      (do (tel/log! {:level :warn :id ::no-refresh-token :data {:user-id user-id}}
            "Google returned no refresh token for Drive")
          false)
      (do (db/set-setting user-id refresh-token-setting
            (crypto/encrypt refresh (user-enc-key user-id)))
          true))))

(defn authorized?
  "Pre : `user-id` is authenticated.
   Post: true iff a Drive refresh token is stored for this user. Does not prove
         the grant is still live — only a token exchange can."
  [user-id]
  (not (str/blank? (db/get-setting user-id refresh-token-setting))))

(defn forget-refresh-token!
  "Pre : `user-id` is authenticated.
   Post: no Drive refresh token remains. Called when Google rejects the stored
         one, so the next sync asks the user to re-authorize instead of failing
         the same way forever."
  [user-id]
  (db/set-setting user-id refresh-token-setting ""))

(defn access-token
  "Mint a short-lived Drive access token for `user-id`.

   Pre : `authorized?` is true for this user.
   Post: an access-token string, or nil when no token is stored or Google
         rejected the refresh token. On rejection the stored token is forgotten,
         so the caller's next attempt reports \"re-authorize\" rather than
         retrying a dead credential.
   Note: the browser needs this same token for the Picker's `setOAuthToken`, so
         one credential serves both the server fetch and the Picker. That is why
         the server vends it rather than the browser running a second flow."
  [user-id]
  (let [stored (db/get-setting user-id refresh-token-setting)]
    (when-not (str/blank? stored)
      (let [refresh (crypto/decrypt stored (user-enc-key user-id))]
        (when-not (str/blank? refresh)
          (try
            (-> (http/post "https://oauth2.googleapis.com/token"
                  {:form-params {:refresh_token refresh
                                 :client_id (oauth/client-id)
                                 :client_secret (oauth/client-secret)
                                 :grant_type "refresh_token"}
                   :as :json
                   :throw-exceptions true})
              :body
              :access_token)
            (catch Exception e
              (tel/log! {:level :warn :id ::refresh-rejected :data {:user-id user-id}}
                (.getMessage e))
              (forget-refresh-token! user-id)
              nil)))))))

(defn file-name
  "The name of a Drive file the app was granted.

   Pre : `token` is a live `drive.file` access token. `file-id` was picked by
         this user.
   Post: the file's name, or nil on any failure. Used only to show the user what
         a document is bound to."
  [token file-id]
  (try
    (-> (http/get (str "https://www.googleapis.com/drive/v3/files/" (url-encode file-id))
          {:query-params {:fields "name"}
           :headers {"Authorization" (str "Bearer " token)}
           :as :json
           :throw-exceptions true})
      :body
      :name)
    (catch Exception e
      (tel/log! {:level :warn :id ::file-name-failed} (.getMessage e))
      nil)))

(def ^:private max-export-bytes
  "A NeoReader export is HTML holding a few hundred quotes. The reference file is
   under 200 KB, so 8 MB is far above any real export and still bounds the read."
  (* 8 1024 1024))

(defn export-html
  "Download a NeoReader export's body.

   Pre : `token` is a live `drive.file` access token. `file-id` names an HTML
         file this user picked.
   Post: {:ok true :html s} on success, or {:ok false :error msg}. A body over
         `max-export-bytes` is refused rather than read, so a mis-picked video
         cannot exhaust heap.
   Inv : the download happens server-side, so no export bytes pass through the
         browser."
  [token file-id]
  (try
    (let [resp (http/get (str "https://www.googleapis.com/drive/v3/files/" (url-encode file-id))
                 {:query-params {:alt "media"}
                  :headers {"Authorization" (str "Bearer " token)}
                  :as :byte-array
                  :throw-exceptions true})
          body (:body resp)]
      (cond
        (nil? body) {:ok false :error "Drive returned an empty file"}
        (> (alength ^bytes body) max-export-bytes)
        {:ok false :error "That file is too large to be a NeoReader export"}
        :else {:ok true :html (String. ^bytes body "UTF-8")}))
    (catch Exception e
      (tel/log! {:level :warn :id ::export-download-failed} (.getMessage e))
      {:ok false :error "Could not read that file from Google Drive"})))
