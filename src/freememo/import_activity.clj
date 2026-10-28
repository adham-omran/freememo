(ns freememo.import-activity
  "Import lifecycle state shared by URL, HTML upload, and HTML paste imports."
  (:require [freememo.user-state :as us]))

(defn- publish! [user-id state-key run-id reading]
  (swap! (us/get-atom user-id state-key) assoc run-id reading)
  (us/work-started! user-id)
  run-id)

(defn- finish!
  "Pre: the run exists. Post: its first outcome remains until reporter acknowledgment.
   Invariant: a late callback cannot create or change a finished run."
  [user-id state-key run-id outcome error]
  (swap! (us/get-atom user-id state-key)
    (fn [runs]
      (if-let [run (get runs run-id)]
        (if (:outcome run)
          runs
          (assoc runs run-id (assoc run :outcome outcome :error error)))
        runs)))
  (us/work-started! user-id)
  nil)

(defn run-url!
  "Pre: import! returns {:ok boolean}. Post: the result and terminal outcome agree.
   Invariant: reporting state remains discoverable after a short import finishes."
  [user-id url import!]
  (let [run-id (str (random-uuid))
        membership (us/get-atom user-id :importing-urls)]
    (swap! membership conj url)
    (publish! user-id :url-import-runs run-id {:phase "Fetching"})
    (try
      (let [result (import!)]
        (finish! user-id :url-import-runs run-id
          (if (:ok result) "success" "failure")
          (when-not (:ok result) (or (:error result) "Import failed")))
        result)
      (catch Throwable t
        (finish! user-id :url-import-runs run-id "failure"
          (or (.getMessage t) "Import failed"))
        (throw t))
      (finally (swap! membership disj url)))))

(defn start-html!
  "Pre: run-id identifies one browser import. Post: it has an upload phase.
   Invariant: cancellation before the start request prevents a late start."
  [user-id run-id]
  (let [runs (us/get-atom user-id :html-import-runs)]
    (locking runs
      (when-not (contains? @(us/get-atom user-id :html-import-cancelled) run-id)
        (when-not (contains? @runs run-id)
          (publish! user-id :html-import-runs run-id {:phase "Uploading"}))
        (not (:outcome (get @runs run-id)))))))

(defn html-upload-ready!
  "Pre: upload completed. Post: the same active run waits for confirmation.
   Invariant: a late upload cannot revive an abandoned run."
  [user-id run-id]
  (let [runs (us/get-atom user-id :html-import-runs)]
    (locking runs
      (when (= "Uploading" (get-in @runs [run-id :phase]))
        (when-not (get-in @runs [run-id :outcome])
          (swap! runs assoc-in [run-id :phase] "Awaiting confirmation")
          (us/work-started! user-id)))))
  nil)

(defn abandon-html!
  "Pre: outcome is cancelled or failure. Post: upload or review ends with that outcome.
   Invariant: browser abandonment cannot end server processing."
  [user-id run-id outcome error]
  {:pre [(#{"cancelled" "failure"} outcome)]}
  (let [runs (us/get-atom user-id :html-import-runs)]
    (locking runs
      (when (or (nil? (get @runs run-id))
                (#{"Uploading" "Awaiting confirmation"}
                  (get-in @runs [run-id :phase])))
        (swap! (us/get-atom user-id :html-import-cancelled) conj run-id)
        (finish! user-id :html-import-runs run-id outcome error))))
  nil)

(defn run-html!
  "Pre: a supplied run-id owns an upload or review. A paste can omit it.
   Post: the same run records processing and its result.
   Invariant: an abandoned review cannot create a topic. Invalid ownership is a caller error."
  [user-id run-id import!]
  (let [supplied? (some? run-id)
        run-id (or run-id (str (random-uuid)))
        runs (us/get-atom user-id :html-import-runs)
        claimed? (locking runs
                   (if supplied?
                     (when (and (#{"Uploading" "Awaiting confirmation"}
                                  (get-in @runs [run-id :phase]))
                                (nil? (get-in @runs [run-id :outcome])))
                       (swap! runs assoc-in [run-id :phase] "Importing")
                       true)
                     (do (publish! user-id :html-import-runs run-id {:phase "Importing"})
                         true)))]
    (if-not claimed?
      {:ok false :error "HTML import is no longer awaiting confirmation"}
      (do
        (us/work-started! user-id)
        (try
          (let [result (import!)]
            (finish! user-id :html-import-runs run-id
              (if (:ok result) "success" "failure")
              (when-not (:ok result) (or (:error result) "Import failed")))
            result)
          (catch Throwable t
            (finish! user-id :html-import-runs run-id "failure"
              (or (.getMessage t) "Import failed"))
            (throw t)))))))
