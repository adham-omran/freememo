(ns freememo.user-state
  "Per-user server-side state registry.
   Returns the same atom for a given (user-id, key) pair across all sessions.
   Thread-safe via ConcurrentHashMap.computeIfAbsent."
  (:import [java.util.concurrent ConcurrentHashMap]))

(defonce ^ConcurrentHashMap registry (ConcurrentHashMap.))

(defn get-atom
  "Get or create a per-user atom. Returns the same atom for repeated calls
   with the same user-id and key. Thread-safe."
  [user-id k]
  (.computeIfAbsent registry [(long user-id) k]
    (reify java.util.function.Function
      (apply [_ _]
        (case k
          :refresh          (atom 0)
          :credits-refresh  (atom 0)
          :meta-refresh     (atom 0)
          :settings-refresh (atom 0)
          :card-mutations   (atom 0)
          :sync-mutations   (atom 0)
          :tree-mutations   (atom 0)
          :pin-mutations    (atom 0)
          :undo-mutations   (atom 0)
          :card-gen-status  (atom {})
          :scanning-pages   (atom #{})
          :transcribing-topics (atom #{})
          :ocr-errors       (atom {})
          :scan-cancellers  (atom {})
          :toasts           (atom []))))))
