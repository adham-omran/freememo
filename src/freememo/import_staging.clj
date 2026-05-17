(ns freememo.import-staging
  "In-memory staging area for upload bytes held between detect-time and
   commit-time. Per-user FIFO eviction past a small cap; no TTL. Server
   restart clears all entries.")

(def ^:private per-user-cap 10)

(defonce ^:private !staged (atom {}))
;; Shape: {user-id [entry ...]} where entry = {:upload-id :bytes :filename :flow :size :created-at}

(defn- evict-overflow
  "Trim entries vector to per-user-cap, dropping oldest (FIFO)."
  [entries]
  (if (> (count entries) per-user-cap)
    (vec (drop (- (count entries) per-user-cap) entries))
    entries))

(defn stage!
  "Stage bytes under a fresh upload-id for the user.
   Pre:  user-id non-nil; bytes is byte[]; filename non-blank string;
         flow is :pdf|:epub|:html|:markdown.
   Post: returns the new upload-id (string); subsequent (claim! user-id id)
         returns the entry exactly once.
   Invariant: per-user FIFO eviction once outstanding count exceeds 10."
  [user-id ^bytes bytes filename flow]
  (let [upload-id (str (random-uuid))
        entry {:upload-id upload-id
               :bytes bytes
               :filename filename
               :flow flow
               :size (alength bytes)
               :created-at (System/currentTimeMillis)}]
    (swap! !staged update user-id
      (fn [entries] (evict-overflow (conj (or entries []) entry))))
    upload-id))

(defn claim!
  "Atomically retrieve and remove a staged entry.
   Pre:  user-id and upload-id non-nil.
   Post: returns the entry on first call; nil if not found, wrong user,
         or already claimed (replay-safe).
   Invariant: one-shot — second call for the same id returns nil."
  [user-id upload-id]
  (let [found (volatile! nil)]
    (swap! !staged update user-id
      (fn [entries]
        (when entries
          (let [matched (first (filter #(= (:upload-id %) upload-id) entries))]
            (when matched (vreset! found matched))
            (vec (remove #(= (:upload-id %) upload-id) entries))))))
    @found))
