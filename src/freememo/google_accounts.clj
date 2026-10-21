(ns freememo.google-accounts
  "Google accounts a user connects to READ Google Docs from
   (plans/google-docs-pull.md, A2). The typical one is a work account, while
   the user signs in with a personal one.

   A connected account is not a login. It is a `drive.file` grant plus the
   account's identity, so FreeMemo can say which account a Doc comes from and
   reach it again for a pull. One settings row holds each account:

     google_account:<sub>  →  {\"email\" …, \"refresh\" <ciphertext>, \"reconnect\" bool}

   The NeoReader sync keeps its own single row (`freememo.google-drive`), so
   connecting a work account here never changes which Drive the NeoReader sync
   reads.

   The grant returns to the ONE registered redirect URI. `state` carries this
   flow's marker, and `api/google-callback-handler` dispatches on it, as it does
   for the Drive flow.

   The key prefix holds no topic id, so the rows need no
   `account-import/settings-key-prefixes` entry. An account export carries them,
   and they arrive undecryptable (`freememo.user-secrets`)."
  (:require
    [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.string :as str]
    [taoensso.telemere :as tel]
    [freememo.db :as db]
    [freememo.google-oauth :as oauth]
    [freememo.user-secrets :as secrets]))

(def scope
  "`drive.file` reads only the Docs the user picks. `openid email` names the
   account that granted it."
  "openid email https://www.googleapis.com/auth/drive.file")

(def ^:private key-prefix "google_account:")

(defn- setting-key [sub] (str key-prefix sub))

(defn- url-encode [s] (java.net.URLEncoder/encode (str s) "UTF-8"))

;; ── The flow's state ─────────────────────────────────────────────────

(def ^:private state-marker
  "Prefix marking a `state` as a Google-account connection. A sign-in state is
   a bare nonce, and a Drive state starts with `drive:`."
  "account:")

(def ^:private state-separator "|")

(defn authorization-state
  "Pre : `nonce` is a CSRF nonce. `return-to` is an app-relative path.
   Post: a string carrying both, marked as this flow. The caller MUST put the
         same string in the session and compare it on the way back."
  [nonce return-to]
  (str state-marker nonce state-separator return-to))

(defn authorization-state?
  "Post: true iff `state` belongs to a Google-account connection."
  [state]
  (str/starts-with? (or state "") state-marker))

(defn parse-state
  "Pre : `authorization-state?` is true for `state`.
   Post: {:nonce s :return-to s}. `:return-to` is NOT validated here."
  [state]
  (let [[nonce return-to] (-> state
                            (subs (count state-marker))
                            (str/split (re-pattern (java.util.regex.Pattern/quote state-separator)) 2))]
    {:nonce nonce :return-to return-to}))

(defn authorize-url
  "The consent URL that connects one Google account.

   Pre : `state` is `authorization-state` output, also stored in the session.
         `oauth/configured?` is true.
   Post: a Google URL with `access_type=offline` and `prompt=consent`, so a
         refresh token is issued, and `select_account`, so Google asks WHICH
         account even while the browser is signed in to one. Without the
         chooser, a browser signed in to the personal account would grant the
         personal account every time."
  [state]
  {:pre [(oauth/configured?)]}
  (str "https://accounts.google.com/o/oauth2/v2/auth"
    "?client_id=" (url-encode (oauth/client-id))
    "&redirect_uri=" (url-encode (oauth/redirect-uri))
    "&response_type=code"
    "&scope=" (url-encode scope)
    "&access_type=offline"
    "&prompt=" (url-encode "consent select_account")
    "&state=" (url-encode state)))

;; ── Stored accounts ──────────────────────────────────────────────────

(defn- read-row
  "Post: the decoded row of `sub`, or nil when none is stored."
  [user-id sub]
  (when-let [v (not-empty (db/get-setting user-id (setting-key sub)))]
    (try (json/parse-string v true)
         (catch Exception _ nil))))

(defn- write-row! [user-id sub row]
  (db/set-setting user-id (setting-key sub) (json/generate-string row)))

(defn connect!
  "Exchange an authorization code and store the account it names.

   Pre : `code` came back from `authorize-url`'s flow for this user.
   Post: {:ok true :email s} and a stored row, or {:ok false :error s} and no
         change. Connecting an account again replaces its row, which is how a
         \"reconnect\" account becomes usable again.
   Inv : the plaintext refresh token never reaches the database."
  [user-id code]
  (let [resp (http/post "https://oauth2.googleapis.com/token"
               {:form-params {:code code
                              :client_id (oauth/client-id)
                              :client_secret (oauth/client-secret)
                              :redirect_uri (oauth/redirect-uri)
                              :grant_type "authorization_code"}
                :as :json
                :throw-exceptions true})
        {:keys [refresh_token id_token scope]} (:body resp)
        claims (some-> id_token oauth/parse-jwt-payload)
        sub (some-> (:sub claims) str not-empty)
        email (str (:email claims))]
    (cond
      (nil? sub)
      {:ok false :error "Google did not say which account this is. Try again."}

      ;; Granular consent lets a user untick Drive and still finish the flow.
      (not (str/includes? (str scope) "auth/drive.file"))
      {:ok false :error "Google Drive access was not granted. Try again and allow it."}

      (str/blank? refresh_token)
      (do (tel/log! {:level :warn :id ::no-refresh-token :data {:user-id user-id}}
            "Google returned no refresh token for a connected account")
          {:ok false :error "Google granted no offline access. Try again and accept the prompt."})

      :else
      (do (write-row! user-id sub {:email email
                                   :refresh (secrets/encrypt user-id refresh_token)
                                   :reconnect false})
          {:ok true :email email}))))

(defn accounts
  "Post: [{:sub :email :reconnect?} …] for every connected account, sorted by
   email. Carries no token."
  [user-id]
  (->> (db/setting-keys-with-prefix user-id key-prefix)
    (keep (fn [k]
            (let [sub (subs k (count key-prefix))]
              (when-let [row (read-row user-id sub)]
                {:sub sub :email (:email row) :reconnect? (boolean (:reconnect row))}))))
    (sort-by :email)
    vec))

(defn account-email
  "Post: the email of connected account `sub`, or nil when it is not connected."
  [user-id sub]
  (:email (read-row user-id sub)))

(defn disconnect!
  "Post: no row remains for `sub`. Every Doc imported through it keeps its
   snapshots, and a later Pull reports the account as not connected."
  [user-id sub]
  (db/delete-setting user-id (setting-key sub)))

(defn- mark-reconnect! [user-id sub row]
  (write-row! user-id sub (assoc row :reconnect true)))

(defn- refusal-message
  "Post: one line for the user about a token-endpoint refusal."
  [body email]
  (let [error (:error body)]
    (if (= "admin_policy_enforced" error)
      (str "The administrator of " email " blocks FreeMemo. Ask them to allow it, then connect the account again.")
      (str "Google no longer accepts the connection to " email ". Connect the account again in Settings, Google Accounts."))))

(defn access-token
  "Mint a short-lived access token for connected account `sub`.

   Pre : `user-id` is authenticated.
   Post: {:ok true :token s}, or {:ok false :error s}. :reconnect? is true when
         the user must connect the account again — no row, an undecryptable
         token, or a refresh Google refused. A refusal marks the row
         \"reconnect\" and KEEPS it, so the Docs that name the account still
         say which account they need.
   Note: a network failure is not a refusal. It reports an error and leaves the
         row as it was."
  [user-id sub]
  (let [row (read-row user-id sub)
        refresh (some->> (:refresh row) (secrets/decrypt user-id))]
    (cond
      (nil? row)
      {:ok false :reconnect? true :error "This Doc's Google account is not connected. Connect it in Settings, Google Accounts."}

      (nil? refresh)
      (do (mark-reconnect! user-id sub row)
          {:ok false :reconnect? true :error (refusal-message nil (:email row))})

      :else
      (try
        (let [resp (http/post "https://oauth2.googleapis.com/token"
                     {:form-params {:refresh_token refresh
                                    :client_id (oauth/client-id)
                                    :client_secret (oauth/client-secret)
                                    :grant_type "refresh_token"}
                      :as :json
                      :coerce :always
                      :throw-exceptions false})
              {:keys [status body]} resp]
          (cond
            (and (= 200 status) (not (str/blank? (:access_token body))))
            (do (when (:reconnect row) (write-row! user-id sub (assoc row :reconnect false)))
                {:ok true :token (:access_token body)})

            (#{400 401 403} status)
            (do (tel/log! {:level :warn :id ::refresh-refused
                           :data {:user-id user-id :status status :error (:error body)}}
                  "Google refused a connected account's refresh token")
                (mark-reconnect! user-id sub row)
                {:ok false :reconnect? true :error (refusal-message body (:email row))})

            :else
            {:ok false :error "Google did not answer. Try again in a minute."}))
        (catch Exception e
          (tel/log! {:level :warn :id ::refresh-failed :data {:user-id user-id}} (.getMessage e))
          {:ok false :error "Could not reach Google. Try again in a minute."})))))
