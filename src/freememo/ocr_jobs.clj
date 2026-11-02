(ns freememo.ocr-jobs
  "Page scan attempts with independent cancellation and deadline guards."
  (:require [freememo.commands :as commands]
            [freememo.logging :as log]
            [freememo.page-ocr :as page]
            [freememo.user-state :as us]
            [freememo.work-run :as run]
            [missionary.core :as m])
  (:import [java.util.concurrent Executors]))

(defonce executor
  (Executors/newFixedThreadPool 3
    (reify java.util.concurrent.ThreadFactory
      (newThread [_ runnable]
        (doto (Thread. runnable "freememo-ocr") (.setDaemon true))))))
(def ^:dynamic *timeout-ms* 30000)

(defn- settle! [uid key attempt result]
  (let [owners (us/get-atom uid :scan-runs)
        result (case (run/state attempt)
                 :cancelled {:success false :cancelled? true}
                 :timed-out {:success false :timed-out? true :error "Scan timed out after 30 seconds"}
                 result)]
    (locking owners
      (when (identical? attempt (get @owners key))
        (cond
          (:success result)
          (do (log/log-info (str "OCR scan complete topic-id=" (first key) " page=" (second key)))
              (commands/bump! uid :scan))
          (:cancelled? result)
          (log/log-info (str "OCR scan cancelled topic-id=" (first key) " page=" (second key)))
          :else
          (do (log/log-info (str "OCR scan failed topic-id=" (first key) " page=" (second key)
                                " error=" (:error result)))
              ;; The error is the :scan activity's :error-fn, so the closed
              ;; row reaches the banner strip; the PDF toolbar reads it too.
              (swap! (us/get-atom uid :ocr-errors) assoc key (:error result))))
        (swap! owners dissoc key)
        (swap! (us/get-atom uid :scan-cancellers) dissoc key)
        (swap! (us/get-atom uid :scan-cancelled) disj key)
        (swap! (us/get-atom uid :scanning-pages) disj key)))))

(defn start! [uid doc page-number enc-key dpi]
  (let [key [doc page-number]
        owners (us/get-atom uid :scan-runs)
        attempt (run/start *timeout-ms*)]
    (locking owners
      (when-let [old (get @owners key)] (run/cancel! old))
      (swap! owners assoc key attempt)
      (swap! (us/get-atom uid :scan-cancelled) disj key)
      (swap! (us/get-atom uid :scanning-pages) conj key)
      (swap! (us/get-atom uid :ocr-errors) dissoc key)
      (swap! (us/get-atom uid :scan-cancellers) assoc key
        (fn []
          (locking owners
            (when (identical? attempt (get @owners key))
              (run/cancel! attempt)
              (swap! (us/get-atom uid :scan-cancelled) conj key)))))
      (log/log-info (str "OCR scan started topic-id=" doc " page=" page-number
                        " run=" (:id attempt))))
    ((m/via executor
       (case (run/state attempt)
         :active (page/extract-page-text uid doc page-number enc-key dpi
                   {:cancelled? #(= :cancelled (run/state attempt))
                    :commit! #(run/commit! attempt %)})
         :timed-out {:success false :timed-out? true :error "Scan timed out after 30 seconds"}
         {:success false :cancelled? true}))
     #(settle! uid key attempt %)
     #(settle! uid key attempt {:success false :error (.getMessage %)}))
    (us/work-started! uid)
    nil))
