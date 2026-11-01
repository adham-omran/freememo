(ns freememo.anki-note-deletion-owner-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric.impl.lang3 :as lang]
            [hyperfiddle.electric-local-def3 :as local]
            [missionary.core :as m]
            [freememo.anki-note-deletion :as deletion]
            [freememo.anki-note-deletion-client :as client]
            [freememo.client-state :as cs]))

(use-fixtures :once
  (fn [run]
    (let [original client/Owner
          owner (try
                  (with-redefs [e/web-config {::lang/peers {:client :clj :server :clj}}]
                    (require 'freememo.anki-note-deletion-client :reload)
                    client/Owner)
                  (finally (alter-var-root #'client/Owner (constantly original))))]
      (with-redefs [client/Owner owner] (run)))))

(defn- capture-errors [tap flow]
  (fn [step done]
    (let [it (flow step done)]
      (reify
        clojure.lang.IFn (invoke [_] (it))
        clojure.lang.IDeref
        (deref [_]
          (try @it
               (catch Throwable error
                 (tap [:root-error error])
                 (throw error))))))))

(deftest route-changes-preserve-work-and-user-root-changes-stop-it
  (let [user (atom 1) route (atom :library)
        current (cs/get-atom :manual-note-deletion-owner)
        engine (local/->engine {:seed 241})
        tap #(local/tap engine %)
        next-value #(local/step engine (constantly true))
        until (fn [pred]
                (loop [n 0]
                  (let [value (next-value)]
                    (when (= :root-error (first value))
                      (throw (ex-info "The Electric user root failed." {} (second value))))
                    (if (pred value) value
                      (if (< n 20) (recur (inc n))
                        (throw (ex-info "The user-root change did not settle." {})))))))
        calls (atom []) pending (atom []) canceled (atom 0)
        call! (fn [_ {:keys [notes]}]
                (fn [success _failure]
                  (swap! calls conj notes)
                  (swap! pending conj success)
                  (fn [] (swap! canceled inc))))]
    (reset! current nil)
    (local/spawn engine
      (capture-errors tap
        (local/local-ngn {} engine
          (let [uid (e/watch user)]
            (client/Owner uid)
            (tap [uid (e/watch route) (:id (e/watch current))])))))
    (try
      (until #(= 1 (first %)))
      (let [first-owner @current]
        (client/start! call! (range 1001))
        (is (= 1 (count @calls)))
        (is (= 1 (count @(:cancellers first-owner))))
        (reset! route :home)
        (until #(= :home (second %)))
        (is (identical? first-owner @current))
        (is (zero? @canceled))
        ((first @pending) nil)
        (is (= [500 500] (mapv count @calls)))
        (reset! user 2)
        (until #(and (= 2 (first %)) (not= (:id first-owner) (nth % 2))))
        (is (false? @(:active? first-owner)))
        (is (= 1 @canceled))
        ((second @pending) nil)
        (is (= 2 (count @calls)))
        (let [second-owner @current]
          (is (= 2 (:user-id second-owner)))
          (client/start! call! (range 1001))
          (is (= 3 (count @calls)))
          (deletion/close-owner! current first-owner)
          (is (identical? second-owner @current))
          (is (= 1 @canceled))
          (reset! user nil)
          (until #(nil? (first %)))
          (is (nil? @current))
          (is (= 2 @canceled))
          ((nth @pending 2) nil)
          (is (= 3 (count @calls)))
          (client/start! call! [9999])
          (is (= 3 (count @calls)))))
      (finally
        (local/cancel engine)
        (when-let [owner @current] (deletion/close-owner! current owner))))))

(deftest synchronous-settlement-does-not-retain-a-canceller
  (let [current (atom nil)
        owner (deletion/mount-owner! current 1)]
    (deletion/start-owned! current (fn [_ _] (m/sp nil)) (range 1001))
    (is (empty? @(:cancellers owner)))
    (deletion/close-owner! current owner)
    (is (nil? @current))))
