(ns freememo.toasts
  "Project-wide toast queue. Per-user atom of vector<toast>.
   Pre: caller knows user-id. Post: queue mutated atomically.
   Invariant: at most one entry per [level message], UNLESS pushed with
   :dedup? false (undo toasts opt out so each action stacks separately).
   :created-at is epoch millis (long) — serializable across the Electric wire."
  (:require [clojure.string :as str]
            [freememo.desktop-link :as link]
            [freememo.user-state :as us])
  (:import [java.util UUID]))

(defn- new-id [] (str (UUID/randomUUID)))

(defn- dedup-merge
  "If queue contains a toast with the same [level message], replace it in place
   (preserve :id, refresh :created-at, take new :actions/:sticky?). Otherwise append."
  [queue toast]
  (let [k [(:level toast) (:message toast)]
        idx (->> (map-indexed vector queue)
              (some (fn [[i t]] (when (= k [(:level t) (:message t)]) i))))]
    (if idx
      (let [existing (nth queue idx)
            merged   (-> existing
                       (assoc :created-at (:created-at toast)
                              :actions    (:actions toast)
                              :sticky?    (:sticky? toast)))]
        (assoc queue idx merged))
      (conj queue toast))))

(def sign-in-path
  "The path that starts a desktop sign-in. Electron stops the navigation and
   opens the browser sign-in (plans/electron-local-db-sync.md 15.5.1)."
  "/desktop/sign-in")

(defn error-actions
  "15.4.2 — the actions an error toast gets from its message, so no call site
   builds them. A feature that needs an account throws
   `link/sign-in-message`, and whatever error shape carries that text to the
   toast, the toast offers the sign-in."
  [level message]
  (if (and (= :error level) (str/includes? (str message) link/sign-in-message))
    [{:label "Sign in" :href sign-in-path}]
    []))

(defn push!
  "Push a toast for user-id. Returns the toast :id (preserves an existing
   id if dedup hits — see dedup-merge).
   Required keys in `toast`: :level :message. Optional: :actions :sticky? :dedup?.
   :sticky? defaults to (= :level :error). :dedup? defaults to true; pass false
   to always append a fresh entry (e.g. undo toasts that must stack)."
  [user-id {:keys [level message actions sticky? dedup?]
            :or {actions [] dedup? true}}]
  (let [sticky? (if (nil? sticky?) (= level :error) sticky?)
        record  {:id         (new-id)
                 :level      level
                 :message    (str message)
                 :actions    (into (vec actions) (error-actions level message))
                 :created-at (System/currentTimeMillis)
                 :sticky?    (boolean sticky?)}]
    (if dedup?
      (let [new-queue (swap! (us/get-atom user-id :toasts) dedup-merge record)
            stored    (some (fn [t] (when (= [level (str message)]
                                            [(:level t) (:message t)]) t))
                        new-queue)]
        (:id stored))
      (do (swap! (us/get-atom user-id :toasts) conj record)
          (:id record)))))

(defn dismiss!
  "Remove the toast with `toast-id` from user's queue. Idempotent."
  [user-id toast-id]
  (swap! (us/get-atom user-id :toasts)
    (fn [q] (filterv #(not= toast-id (:id %)) q)))
  nil)
