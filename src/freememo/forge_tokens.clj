(ns freememo.forge-tokens
  "Personal access tokens for git forges: one per forge and host, stored
   encrypted in `settings` (plans/git-forge-integration.md §4.2 2.1, D4).

   The setting key is `forge_token:<forge>:<host>`. One account can therefore
   hold a GitLab token for gitlab.com and another for a company instance, and a
   Forgejo token is never sent to a GitLab host.

   `freememo.user-secrets` owns the encryption key and states why it derives
   from the user row."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [freememo.forge-kinds :as kinds]
            [freememo.user-secrets :as secrets]))

(def ^:private key-prefix "forge_token:")

(defn- setting-key [forge host]
  (str key-prefix forge ":" host))

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
  (db/set-setting user-id (setting-key forge host)
    (secrets/encrypt user-id (str/trim token))))

(defn token
  "Post: the plaintext token for (`forge`, `host`), or nil when none is stored
   or the stored value cannot be decrypted with this user's key."
  [user-id forge host]
  (secrets/decrypt user-id (db/get-setting user-id (setting-key forge host))))

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
