(ns freememo.user-secrets
  "The AES key that encrypts a user's stored third-party credentials: forge
   tokens (`freememo.forge-tokens`) and connected Google accounts
   (`freememo.google-accounts`).

   The key derives from the USER ROW, never from the session.
   `crypto/encrypt` returns its input unchanged when the key is nil, and a
   password user's session carries no key (`main/reconstruct-session*`), so a
   session key would store every password user's secret as plaintext. A Google
   user's key is the one `google-drive` uses. A password user's derives from
   `ENC_KEY_SECRET` and the user id.

   Account export copies settings rows verbatim. A secret reaches the target
   instance undecryptable, because the user id or the server secret differs
   there, and readers treat it as absent."
  (:require [freememo.crypto :as crypto]
            [freememo.db :as db]))

(defn encryption-key
  "Post: the AES key for `user-id`'s stored secrets. Never nil."
  [user-id]
  (if-let [google-sub (:users/google_id (db/get-user-by-id user-id))]
    (crypto/derive-key-for-oauth-user google-sub)
    (crypto/derive-key-for-local-user user-id)))

(defn encrypt
  "Pre : `plain` is a non-blank string.
   Post: ciphertext of `plain` under `user-id`'s key.
   Invariant: the result is never `plain`. A key that failed to encrypt throws
         instead of handing plaintext to a writer."
  [user-id plain]
  (let [cipher (crypto/encrypt plain (encryption-key user-id))]
    (when (= cipher plain)
      (throw (ex-info "Secret encryption returned the plaintext" {:type ::not-encrypted})))
    cipher))

(defn decrypt
  "Post: the plaintext of `cipher`, or nil when it is blank or does not decrypt
   under `user-id`'s key."
  [user-id cipher]
  (when-not (empty? cipher)
    (not-empty (crypto/decrypt cipher (encryption-key user-id)))))
