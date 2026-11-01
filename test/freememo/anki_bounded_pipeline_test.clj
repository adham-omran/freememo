(ns freememo.anki-bounded-pipeline-test
  (:require [clojure.test :refer [deftest is]]
            [missionary.core :as m]
            [freememo.anki-reads :as reads]
            [freememo.anki-overlay-reads :as overlay]
            [freememo.anki-reconcile-client :as client]
            [freememo.anki-finish-io :as finish]
            [freememo.anki-digest :as digest]
            [freememo.task-request :as request]))

(defn await! [task]
  (let [result (promise)
        cancel (task #(deliver result [:ok %]) #(deliver result [:error %]))
        [status value] (deref result 30000 [:timeout])]
    (case status :ok value :error (throw value)
      (do (cancel) (throw (ex-info "The bounded pipeline test timed out." {}))))))

(defn note [nid]
  {:noteId nid :cards [(* nid 2) (inc (* nid 2))]
   :tags ["marked" "unrelated"]
   :fields {:Front {:order 0 :value (str "<p>Question " nid "</p>")}
            :Back {:order 1 :value "<p>Answer</p>"}
            :Remarks {:order 2 :value "<p>Private remark body</p>"}}})

(deftest overlay-releases-raw-batches-and-bounds-suspension
  (let [calls (atom [])
        result (await!
                 (overlay/read-overlay!
                   (fn [action {:keys [notes cards]}]
                     (m/sp
                       (swap! calls conj [action (count (or notes cards))])
                       (case action
                         "notesInfo" (mapv note notes)
                         "areSuspended" (mapv even? cards))))
                   (range 1 1002)))]
    (is (= 1001 (count (:present result))))
    (is (every? #(<= (second %) 500) @calls))
    (is (= ["notesInfo" "areSuspended" "areSuspended" "notesInfo"] (mapv first (take 4 @calls))))
    (is (not (.contains (pr-str result) "Private remark body")))
    (is (not (.contains (pr-str result) "Question ")))
    (is (= 2 (count (:field-fingerprints (first (:present result))))))
    (is (true? (:remark (first (:present result)))))))

(deftest scan-retains-only-digests-mods-and-absence
  (let [result (await! (client/scan-notes!
                         (fn [_ {:keys [notes]}]
                           (m/sp (mapv #(if (= % 123) {} (note %)) notes)))
                         (range 1 1002) (zipmap (range 1 1002) (repeat "basic"))))]
    (is (= #{:digests :mods :absent} (set (keys result))))
    (is (= [123] (:absent result)))
    (is (= 1000 (count (:digests result))))))

(deftest typed-overlay-sends-one-content-fingerprint-and-only-active-flags
  (let [result (await! (overlay/read-overlay!
                         (fn [action {:keys [notes cards]}]
                           (m/sp
                             (case action
                               "notesInfo" (mapv #(assoc (note %) :tags [] :fields
                                                   {:Front {:order 0 :value "Question"}
                                                    :Back {:order 1 :value "Answer"}}) notes)
                               "areSuspended" (mapv (constantly false) cards))))
                         [1] {1 "basic"}))
        state (first (:present result))]
    (is (= #{:note-id :fingerprint} (set (keys state))))
    (is (= (digest/text-tuple-fingerprint ["Question" "Answer"]) (:fingerprint state)))
    (is (< (count (pr-str state)) 120))
    (is (= (digest/text-tuple-fingerprint ["Question" ""])
          (overlay/note-fingerprint {:fields {:Front {:order 0 :value "Question"}}} "basic")))
    (is (not= (digest/text-tuple-fingerprint ["ab" "c"])
          (digest/text-tuple-fingerprint ["a" "bc"])))
    (is (= (digest/text-tuple-fingerprint ["😀" ""])
          (digest/text-tuple-fingerprint ["😀" nil])))))

(deftest cancellation-stops-the-next-delete-and-keeps-the-first-record
  (let [calls (atom []) checks (atom 0)
        result (await! (finish/finish!
                         (fn [action data] (m/sp (swap! calls conj [action data]) nil))
                         (fn [data]
                           (m/sp
                             (swap! calls conj [:server data])
                             (case (:op data)
                               :checkpoint {:verdict (if (= 1 (swap! checks inc)) :continue :cancel)}
                               :deleted {:success true})))
                         {:note-ids-to-delete (range 30000)} {}))]
    (is (= :cancel (:verdict result)))
    (is (= 1 (count (filter #(= "deleteNotes" (first %)) @calls))))
    (is (= 500 (count (get-in (second @calls) [1 :notes]))))
    (is (= :deleted (get-in (nth @calls 2) [1 :op])))))

(deftest pull-reread-drift-becomes-a-conflict
  (let [calls (atom [])
        scanned (note 99)
        result (await! (finish/finish!
                         (fn [_ _] (m/sp [(assoc-in scanned [:fields :Back :value] "Changed again")]))
                         (fn [data]
                           (m/sp
                             (swap! calls conj data)
                             (case (:op data)
                               :checkpoint {:verdict :continue}
                               :pull {:success true :count 0 :conflict-card-ids []})))
                         {:pull-card-ids [7] :pull-kind-by-card {7 "basic"}
                          :pull-versions {7 {:note-id 99 :local-digest "planned"}}}
                         {:digests {99 (digest/anki-digest (:fields scanned) "basic")}}))]
    (is (= [7] (get-in result [:pulls :conflict-card-ids])))
    (is (empty? (:card-ids (second @calls))))
    (is (empty? (:notes (second @calls))))))

(deftest canceled-request-cannot-settle-a-replacement
  (let [pending (atom nil) result (atom [])
        cancel ((request/request! pending {:op :old}) #(swap! result conj %) (fn [_]))
        old @pending]
    (cancel)
    ((request/request! pending {:op :new}) #(swap! result conj %) (fn [_]))
    (request/resolve! old :late)
    (request/resolve! @pending :current)
    (is (= [:current] @result))))
