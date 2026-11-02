(ns freememo.import-staging
  "In-memory staging area for upload bytes held between detect-time and
   commit-time. Per-user FIFO eviction past a small cap; no TTL. Server
   restart clears all entries.

   An entry a link click staged in the background is marked WAITING. The click
   left the Import dialog out of the path, so the entry is the only place the
   reader can finish that import from: the Import nav item shows how many wait,
   and the Import page lists them (plan item 2.12).
   `rev-atom` is the reactive handle for that count."
  (:import [java.util.concurrent ConcurrentHashMap]))

(def ^:private per-user-cap 10)

(defonce ^:private ^ConcurrentHashMap revs (ConcurrentHashMap.))

(defn rev-atom
  "The per-user counter atom that moves when the user's waiting entries change.
   An Electric form watches it, then reads `waiting`, so the nav badge and the
   Import page follow the staging area. Thread-safe."
  [user-id]
  (.computeIfAbsent revs (long user-id)
    (reify java.util.function.Function
      (apply [_ _] (atom 0)))))

(defn- bump! [user-id]
  (swap! (rev-atom user-id) inc)
  nil)

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
         flow is :pdf|:epub|:html|:markdown|:audio|:repo|:supermemo|:sphinx|
         :account. `extra`
         (5-arity) is an arbitrary map carried alongside the bytes through
         claim!.
   Post: returns the new upload-id (string); subsequent (claim! user-id id)
         returns the entry exactly once.
   Invariant: per-user FIFO eviction once outstanding count exceeds 10."
  ([user-id ^bytes bytes filename flow]
   (stage! user-id bytes filename flow nil))
  ([user-id ^bytes bytes filename flow extra]
   (let [upload-id (str (random-uuid))
         entry (cond-> {:upload-id upload-id
                        :bytes bytes
                        :filename filename
                        :flow flow
                        :size (alength bytes)
                        :created-at (System/currentTimeMillis)}
                 extra (assoc :extra extra))]
     (swap! !staged update user-id
       (fn [entries] (evict-overflow (conj (or entries []) entry))))
     ;; An overflow can evict a waiting entry.
     (bump! user-id)
     upload-id)))

(defn mark-waiting!
  "Mark a staged entry as waiting for the reader on the Import page.
   Pre:  `upload-id` came from `stage!` for `user-id`.
   Post: the entry is listed by `waiting`. Nil, and no change, for an entry
         that is gone."
  [user-id upload-id]
  (swap! !staged update user-id
    (fn [entries]
      (mapv #(if (= upload-id (:upload-id %)) (assoc % :waiting? true) %) entries)))
  (bump! user-id))

(defn waiting
  "The user's waiting entries, oldest first, without their bytes.
   Post: a vector of {:upload-id :filename :flow :size :created-at}."
  [user-id]
  (->> (get @!staged user-id)
    (filter :waiting?)
    (mapv #(select-keys % [:upload-id :filename :flow :size :created-at]))))

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
    (when (:waiting? @found) (bump! user-id))
    @found))
