(ns freememo.quota
  "Per-user file storage quota.

   Counter `users.usage_bytes` tracks all stored bytes:
   `SUM(topic_files.file_size) + SUM(topic_videos.byte_size)`. The video term
   is a Postgres large object rather than a BYTEA column, but it costs the same
   disk and is capped by the same number.

   Optional per-user overrides (NULL = use env default):
     - `users.quota_bytes`      — total-storage cap
     - `users.upload_max_bytes` — per-upload-increment cap

   Sentinel: 0 means unlimited — applies to env defaults and per-user overrides.

   Override is absolute: a later raise of an env default does not lift
   per-user overrides automatically.

   Two tiers, one column: `quota_bytes` NULL means the free default below; the
   first completed credit order writes `paid-quota-bytes` into it
   (db/promote-to-paid-storage-tier!). The tier caps how MUCH may be stored;
   `freememo.storage-meter` prices how LONG it is stored. They are separate
   concerns and must not be collapsed — a purchase buys rent, not a bigger cap
   per purchase.

   HTTP-layer note: `wrap-route-body-size` is per-user-blind and caps upload
   requests at `request-max-bytes` (STORAGE_REQUEST_MAX_BYTES). A per-user
   `upload_max_bytes` above that ceiling — or 0/unlimited — is still bounded by
   it at the HTTP layer; lifting it requires raising STORAGE_REQUEST_MAX_BYTES."
  (:require [next.jdbc :as jdbc]
            [taoensso.telemere :as tel]))

(def default-total-bytes
  (try
    (Long/parseLong (or (System/getenv "STORAGE_QUOTA_BYTES") "1073741824"))
    (catch Exception _
      (tel/log! {:level :warn :id ::bad-env :data {:var "STORAGE_QUOTA_BYTES"}}
        "Invalid STORAGE_QUOTA_BYTES; using default")
      1073741824)))

(def default-upload-max-bytes
  "Default per-upload-increment cap; NULL fallback for users.upload_max_bytes."
  (try
    (Long/parseLong (or (System/getenv "STORAGE_PER_FILE_MAX_BYTES") "104857600"))
    (catch Exception _
      (tel/log! {:level :warn :id ::bad-env :data {:var "STORAGE_PER_FILE_MAX_BYTES"}}
        "Invalid STORAGE_PER_FILE_MAX_BYTES; using default")
      104857600)))

(def paid-quota-bytes
  "Total-storage cap written into `users.quota_bytes` on a user's first
   completed credit order. Default 100 GB — video-sized, where the 1 GB free
   default is not. 0 means unlimited, as everywhere else."
  (try
    (Long/parseLong (or (System/getenv "STORAGE_PAID_QUOTA_BYTES") "107374182400"))
    (catch Exception _
      (tel/log! {:level :warn :id ::bad-env :data {:var "STORAGE_PAID_QUOTA_BYTES"}}
        "Invalid STORAGE_PAID_QUOTA_BYTES; using default")
      107374182400)))

(def request-max-bytes
  "Absolute HTTP request-body ceiling for upload routes (per-user-blind).
   Invariant: every effective upload_max_bytes should be <= this."
  (try
    (Long/parseLong (or (System/getenv "STORAGE_REQUEST_MAX_BYTES") "104857600"))
    (catch Exception _
      (tel/log! {:level :warn :id ::bad-env :data {:var "STORAGE_REQUEST_MAX_BYTES"}}
        "Invalid STORAGE_REQUEST_MAX_BYTES; using default")
      104857600)))

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

(defn get-user-upload-max
  "Effective per-upload byte cap: override if set, else env default.
   `connectable` is a JDBC datasource or transaction."
  [connectable user-id]
  (or (:users/upload_max_bytes
        (jdbc/execute-one! connectable
          ["SELECT upload_max_bytes FROM users WHERE id = ?" user-id]))
    default-upload-max-bytes))

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
   Locks the users row, validates the per-user upload and total caps, then
   increments `usage_bytes` by `incoming-bytes`.

   Pre: tx is a live connectable; user-id refers to an existing users row;
        incoming-bytes is non-negative.
   Post: on return, usage_bytes is incremented by incoming-bytes.
   Throws: `quota-error :file-too-large` or `quota-error :over-quota`
           on cap violation — caller's tx must abort to undo the lock.

   Invariant: between SELECT FOR UPDATE and UPDATE, no other tx can read
   or write this user's usage_bytes."
  [tx user-id incoming-bytes]
  (let [row (jdbc/execute-one! tx
              ["SELECT usage_bytes,
                       COALESCE(quota_bytes, ?) AS effective_quota,
                       COALESCE(upload_max_bytes, ?) AS effective_upload_max
                FROM users WHERE id = ? FOR UPDATE"
               default-total-bytes default-upload-max-bytes user-id])
        used (:users/usage_bytes row)
        quota-limit (:effective_quota row)
        upload-limit (:effective_upload_max row)]
    (when (or (nil? used) (nil? quota-limit))
      (throw (ex-info "User not found for quota check"
               {:type ::user-not-found :user-id user-id})))
    (when (and (not (unlimited? upload-limit))
               (> incoming-bytes upload-limit))
      (throw (quota-error :file-too-large
               {:limit upload-limit :incoming incoming-bytes})))
    (when (and (not (unlimited? quota-limit))
               (> (+ used incoming-bytes) quota-limit))
      (throw (quota-error :over-quota
               {:used used :limit quota-limit :incoming incoming-bytes})))
    (jdbc/execute! tx
      ["UPDATE users SET usage_bytes = usage_bytes + ? WHERE id = ?"
       (long incoming-bytes) user-id])
    {:ok true :used (+ used incoming-bytes) :limit quota-limit}))

(defn reserve-bytes!
  "Atomic TOTAL-quota gate, without the per-upload cap. Must be called inside an
   active `with-transaction`.

   Sibling of `check-and-bump!`, not a refactor of it: that function's source is
   pinned (plans/incremental-video.md §4.2 2.5) because every existing upload
   path depends on it exactly as written. The duplicated SELECT is the price.

   Why the per-upload cap must NOT apply here: `upload_max_bytes` exists because
   a single HTTP request cannot exceed `request-max-bytes` — it is a transport
   bound wearing a storage name. A chunked video upload is many requests, each
   already capped by `wrap-route-body-size`, so applying the single-request
   ceiling to the whole file would reject a 700 MB video for exceeding a limit
   its transport never touches. That is precisely the ceiling chunking was built
   to get past. Video's own per-upload bound is `video-http/max-video-bytes`,
   enforced by the caller before this runs.

   Pre:  tx is live; user-id exists; incoming-bytes non-negative; the caller has
         already applied its own per-upload ceiling.
   Post: usage_bytes incremented by incoming-bytes.
   Throws: `quota-error :over-quota` — the caller's tx must abort to release the
           lock and undo the reservation.
   Invariant: between SELECT FOR UPDATE and UPDATE no other tx reads or writes
   this user's usage_bytes."
  [tx user-id incoming-bytes]
  (let [row (jdbc/execute-one! tx
              ["SELECT usage_bytes, COALESCE(quota_bytes, ?) AS effective_quota
                FROM users WHERE id = ? FOR UPDATE"
               default-total-bytes user-id])
        used (:users/usage_bytes row)
        quota-limit (:effective_quota row)]
    (when (or (nil? used) (nil? quota-limit))
      (throw (ex-info "User not found for quota check"
               {:type ::user-not-found :user-id user-id})))
    (when (and (not (unlimited? quota-limit))
               (> (+ used incoming-bytes) quota-limit))
      (throw (quota-error :over-quota
               {:used used :limit quota-limit :incoming incoming-bytes})))
    (jdbc/execute! tx
      ["UPDATE users SET usage_bytes = usage_bytes + ? WHERE id = ?"
       (long incoming-bytes) user-id])
    {:ok true :used (+ used incoming-bytes) :limit quota-limit}))

(defn remaining-bytes
  "Bytes the user may still store: effective cap minus current usage, or nil
   when the cap is unlimited. Clamped at 0 — an over-quota user has zero
   headroom, not negative headroom.
   Read-only; for showing a limit in the UI, not for gating."
  [connectable user-id]
  (let [cap (get-user-quota connectable user-id)]
    (when-not (unlimited? cap)
      (max 0 (- cap (get-user-usage connectable user-id))))))
