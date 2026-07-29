(ns freememo.client-state
  "Per-session, client-only state registry — the browser-local counterpart to
   freememo.user-state.

   user-state atoms are keyed by user-id and SHARED across every session of that
   user. That sharing is what made the optimistic command queue double-execute
   when a user had two tabs open: both tabs' CommandDispatchers drained the one
   shared :pending-commands queue, so every command ran once per open tab.

   These atoms instead live in the browser. `get-atom` returns the same atom per
   key within one page load, and a fresh atom on the next page load, so two tabs
   never share a queue or an overlay — a command enqueued in one tab is executed
   by that tab only.

   Keys:
     :pending-commands  vector of {:id :type :payload} — this tab's dispatch queue
     :pending-cards     map of tempid -> overlay entry — this tab's add-* rows

   Consequence (intended): a command still pending when the tab reloads is lost,
   not resumed — an optimistic add is cheap to reissue, and per-session isolation
   is what removes the cross-tab doubling.

   Plain cljc (no reader conditional) so the vars resolve on both peers for
   Electric's dual compile; every call site is e/client-sited, so the server-peer
   registry is inert.")

(defonce ^:private registry (atom {}))

(defn get-atom
  "Return this session's atom for `k`, creating it on first use. Same atom for
   repeated calls with the same `k` within one page load. Pre: k ∈
   #{:pending-commands :pending-cards}."
  [k]
  (-> (swap! registry
        (fn [m]
          (if (contains? m k)
            m
            (assoc m k (atom (case k
                               :pending-commands []
                               :pending-cards    {}))))))
    (get k)))
