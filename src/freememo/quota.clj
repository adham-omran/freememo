(ns freememo.quota
  "Per-user file storage quota.

   Counter `users.usage_bytes` tracks the sum of `topic_files.file_size`.
   Optional override `users.quota_bytes` (NULL = use env default).

   Override is absolute: a later raise of STORAGE_QUOTA_BYTES does not lift
   per-user overrides automatically."
  (:require [freememo.db :as db]
            [next.jdbc :as jdbc]
            [taoensso.telemere :as tel]))

(def default-total-bytes
  (try
    (Long/parseLong (or (System/getenv "STORAGE_QUOTA_BYTES") "1073741824"))
    (catch Exception _ 1073741824)))

(def per-file-max-bytes
  (try
    (Long/parseLong (or (System/getenv "STORAGE_PER_FILE_MAX_BYTES") "104857600"))
    (catch Exception _ 104857600)))

(defn get-user-quota
  "Effective per-user cap: override if set, else env default."
  [user-id]
  (or (:users/quota_bytes
        (jdbc/execute-one! db/ds
          ["SELECT quota_bytes FROM users WHERE id = ?" user-id]))
    default-total-bytes))

(defn get-user-usage
  "Read the denormalized counter. Returns 0 for unknown users."
  [user-id]
  (or (:users/usage_bytes
        (jdbc/execute-one! db/ds
          ["SELECT usage_bytes FROM users WHERE id = ?" user-id]))
    0))

(defn check-quota
  "Returns {:ok true} when an upload of incoming-bytes by user-id would fit,
   {:ok false :reason :file-too-large | :over-quota :used :limit :incoming} otherwise."
  [user-id incoming-bytes]
  (cond
    (> incoming-bytes per-file-max-bytes)
    {:ok false :reason :file-too-large
     :limit per-file-max-bytes :incoming incoming-bytes}

    :else
    (let [used (get-user-usage user-id)
          quota (get-user-quota user-id)]
      (if (> (+ used incoming-bytes) quota)
        {:ok false :reason :over-quota
         :used used :limit quota :incoming incoming-bytes}
        {:ok true}))))

(defn record-usage!
  "Adjust users.usage_bytes by delta-bytes (signed). Clamped at 0.
   Pass a transaction `tx` when called inside `with-transaction`,
   otherwise the default datasource is used."
  ([user-id delta-bytes]
   (record-usage! db/ds user-id delta-bytes))
  ([connectable user-id delta-bytes]
   (jdbc/execute! connectable
     ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes + ?) WHERE id = ?"
      (long delta-bytes) user-id])))

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
