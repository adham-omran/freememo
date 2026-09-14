(ns freememo.forge-tokens
  "Personal access tokens for git forges: one per forge and host, stored
   encrypted in `settings` (plans/git-forge-integration.md §4.2 2.1, D4).

   The setting key is `forge_token:<forge>:<host>`. One account can therefore
   hold a GitLab token for gitlab.com and another for a company instance, and a
   Forgejo token is never sent to a GitLab host.

   The encryption key derives from the USER ROW, never from the session.
   `crypto/encrypt` returns its input unchanged when the key is nil, and a
   password user's session carries no key (`main/reconstruct-session*`), so a
   session key would have stored every password user's token as plaintext. A
   Google user's key is the one `google-drive` uses; a password user's derives
   from `ENC_KEY_SECRET` and the user id.

   Account export copies settings rows verbatim. A token reaches the target
   instance undecryptable, because the user id or the server secret differs
   there, and `token` reads it as absent — the outcome the Drive token has."
  (:require [clojure.string :as str]
            [freememo.crypto :as crypto]
            [freememo.db :as db]
            [freememo.forge-kinds :as kinds]))

(def ^:private key-prefix "forge_token:")

(defn- setting-key [forge host]
  (str key-prefix forge ":" host))

(defn- encryption-key
  "Post: the AES key for `user-id`'s forge tokens. Never nil."
  [user-id]
  (if-let [google-sub (:users/google_id (db/get-user-by-id user-id))]
    (crypto/derive-key-for-oauth-user google-sub)
    (crypto/derive-key-for-local-user user-id)))

(defn save-token!
  "Pre : `forge` is in `forge-kinds/forge-ids`; `host` came from
         `forge/parse-origin`; `token` is not blank (violation = caller bug).
   Post: the setting holds ciphertext of the trimmed token.
   Invariant: the stored value is never the token. A key that failed to
         encrypt throws instead of writing plaintext."
  [user-id forge host token]
  (when-not (and (contains? kinds/forge-ids forge) (not (str/blank? host)) (not (str/blank? token)))
    (throw (ex-info "save-token! needs a forge id, a host and a token"
             {:type ::invalid-token-request :forge forge :host host})))
  (let [plain (str/trim token)
        cipher (crypto/encrypt plain (encryption-key user-id))]
    (when (= cipher plain)
      (throw (ex-info "Token encryption returned the plaintext" {:type ::not-encrypted})))
    (db/set-setting user-id (setting-key forge host) cipher)))

(defn token
  "Post: the plaintext token for (`forge`, `host`), or nil when none is stored
   or the stored value cannot be decrypted with this user's key."
  [user-id forge host]
  (when-let [stored (not-empty (db/get-setting user-id (setting-key forge host)))]
    (not-empty (crypto/decrypt stored (encryption-key user-id)))))

(defn delete-token!
  "Post: no token is stored for (`forge`, `host`)."
  [user-id forge host]
  (db/delete-setting user-id (setting-key forge host)))

(defn saved-origins
  "Post: [{:forge :host} …] for every stored token, sorted by forge then host.
   Carries no token."
  [user-id]
  (->> (db/setting-keys-with-prefix user-id key-prefix)
    (keep (fn [k]
            (let [[forge host] (str/split (subs k (count key-prefix)) #":" 2)]
              (when (and (contains? kinds/forge-ids forge) (not (str/blank? host)))
                {:forge forge :host host}))))
    (sort-by (juxt :forge :host))
    vec))
