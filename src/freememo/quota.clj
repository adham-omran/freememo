(ns freememo.quota
  "Per-user file storage quota.

   Counter `users.usage_bytes` tracks the sum of `topic_files.file_size`.
   Optional override `users.quota_bytes` (NULL = use env default).

   Sentinel: 0 means unlimited — applies to env defaults and per-user overrides.

   Override is absolute: a later raise of STORAGE_QUOTA_BYTES does not lift
   per-user overrides automatically."
  (:require [next.jdbc :as jdbc]))

(def default-total-bytes
  (try
    (Long/parseLong (or (System/getenv "STORAGE_QUOTA_BYTES") "1073741824"))
    (catch Exception _ 1073741824)))

(def per-file-max-bytes
  (try
    (Long/parseLong (or (System/getenv "STORAGE_PER_FILE_MAX_BYTES") "104857600"))
    (catch Exception _ 104857600)))

(defn unlimited?
  "Sentinel check — `0` means no cap."
  [n]
  (or (nil? n) (zero? n)))

(defn get-user-quota
  "Effective per-user cap: override if set, else env default.
   `connectable` is a JDBC datasource or transaction."
  [connectable user-id]
  (or (:users/quota_bytes
        (jdbc/execute-one! connectable
          ["SELECT quota_bytes FROM users WHERE id = ?" user-id]))
    default-total-bytes))

(defn get-user-usage
  "Read the denormalized counter. Returns 0 for unknown users.
   `connectable` is a JDBC datasource or transaction."
  [connectable user-id]
  (or (:users/usage_bytes
        (jdbc/execute-one! connectable
          ["SELECT usage_bytes FROM users WHERE id = ?" user-id]))
    0))

(defn quota-error
  "Build an ex-info for ::quota-error with reason + data attached."
  [reason data]
  (ex-info (case reason
             :file-too-large "File exceeds per-file size limit"
             :over-quota "Storage quota exceeded"
             "Quota error")
    (merge {:type ::quota-error :reason reason} data)))

(defn quota-error?
  "Predicate for catching ::quota-error ex-data."
  [data]
  (= ::quota-error (:type data)))

(defn check-and-bump!
  "Atomic quota gate. Must be called inside an active `with-transaction`.
   Locks the users row, validates per-file and total caps, then increments
   `usage_bytes` by `incoming-bytes`.

   Pre: tx is a live connectable; user-id refers to an existing users row;
        incoming-bytes is non-negative.
   Post: on return, usage_bytes is incremented by incoming-bytes.
   Throws: `quota-error :file-too-large` or `quota-error :over-quota`
           on cap violation — caller's tx must abort to undo the lock.

   Invariant: between SELECT FOR UPDATE and UPDATE, no other tx can read
   or write this user's usage_bytes."
  [tx user-id incoming-bytes]
  (when (and (not (unlimited? per-file-max-bytes))
             (> incoming-bytes per-file-max-bytes))
    (throw (quota-error :file-too-large
             {:limit per-file-max-bytes :incoming incoming-bytes})))
  (let [row (jdbc/execute-one! tx
              ["SELECT usage_bytes, COALESCE(quota_bytes, ?) AS effective_quota
                FROM users WHERE id = ? FOR UPDATE"
               default-total-bytes user-id])
        used (:users/usage_bytes row)
        limit (:effective_quota row)]
    (when (or (nil? used) (nil? limit))
      (throw (ex-info "User not found for quota check"
               {:type ::user-not-found :user-id user-id})))
    (when (and (not (unlimited? limit))
               (> (+ used incoming-bytes) limit))
      (throw (quota-error :over-quota
               {:used used :limit limit :incoming incoming-bytes})))
    (jdbc/execute! tx
      ["UPDATE users SET usage_bytes = usage_bytes + ? WHERE id = ?"
       (long incoming-bytes) user-id])
    {:ok true :used (+ used incoming-bytes) :limit limit}))
