(ns freememo.import-waiting-test
  "Imports that wait for the reader (plans/remove-toasts.md 2.12).

   A link click that fetched a PDF or an EPUB stages the bytes and marks them
   waiting. The Import nav item counts them and the Import page lists them.
   Nothing here needs PostgreSQL."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.import-staging :as staging]
            [freememo.import-waiting :as iw]
            [freememo.user-state :as us]
            [freememo.web-import :as web]))

(defn- user-id [] (+ 900000000 (rand-int 90000000)))

(defn- stage! [uid filename flow]
  (staging/stage! uid (.getBytes "bytes" "UTF-8") filename flow))

(deftest a-staged-entry-waits-only-when-marked
  (let [uid (user-id)
        plain (stage! uid "modal.pdf" :pdf)
        linked (stage! uid "linked.pdf" :pdf)]
    (is (= [] (staging/waiting uid)) "a modal upload is not waiting")
    (staging/mark-waiting! uid linked)
    (is (= [linked] (mapv :upload-id (staging/waiting uid))))
    (testing "the listing holds metadata and never the bytes"
      (is (= #{:upload-id :filename :flow :size :created-at}
            (set (keys (first (staging/waiting uid)))))))
    (staging/claim! uid plain)
    (staging/claim! uid linked)))

(deftest claiming-or-discarding-a-waiting-entry-moves-the-counter
  (let [uid (user-id)
        id (stage! uid "a.epub" :epub)
        !rev (staging/rev-atom uid)
        marked (do (staging/mark-waiting! uid id) @!rev)]
    (is (= {:ok true} (iw/discard!* uid id)))
    (is (> @!rev marked) "a discard moves the counter, so the badge re-reads")
    (is (= [] (iw/waiting* @!rev uid)))
    (testing "a second discard finds nothing and changes nothing"
      (let [after @!rev]
        (iw/discard!* uid id)
        (is (= after @!rev))))))

(deftest another-users-entry-cannot-be-discarded
  (let [owner (user-id)
        other (+ 1 owner)
        id (stage! owner "mine.pdf" :pdf)]
    (staging/mark-waiting! owner id)
    (iw/discard!* other id)
    (is (= [id] (mapv :upload-id (staging/waiting owner))))
    (staging/claim! owner id)))

(deftest the-badge-caps-at-99
  (is (= "1" (iw/badge-text 1)))
  (is (= "99" (iw/badge-text 99)))
  (is (= "99+" (iw/badge-text 100))))

(deftest kind-label-names-the-file-kind
  (is (= "PDF" (iw/kind-label :pdf)))
  (is (= "EPUB" (iw/kind-label :epub))))

(defn- await-until [pred]
  (loop [n 0]
    (cond (pred) true
          (> n 100) false
          :else (do (Thread/sleep 50) (recur (inc n))))))

(deftest a-link-click-that-stages-a-file-leaves-it-waiting
  (let [uid (user-id)
        id (stage! uid "paper.pdf" :pdf)]
    (with-redefs [web/find-existing-topic-for-url* (fn [_ _] nil)
                  web/import-url!* (fn [_ _] {:ok true :flow :staged :dispatch :pdf
                                              :upload-id id :filename "paper.pdf"})
                  us/work-started! (constantly nil)]
      (is (= {:flow :queued} (web/start-url-import!* uid "https://example.test/paper.pdf")))
      (is (await-until #(seq (staging/waiting uid))))
      (is (= [id] (mapv :upload-id (staging/waiting uid)))))
    (staging/claim! uid id)))

(deftest a-link-click-that-fails-leaves-nothing-waiting-and-does-not-throw
  (let [uid (user-id)]
    (with-redefs [web/find-existing-topic-for-url* (fn [_ _] nil)
                  web/import-url!* (fn [_ _] (throw (ex-info "Fetch failed" {})))
                  us/work-started! (constantly nil)]
      (is (= {:flow :queued} (web/start-url-import!* uid "https://example.test/bad")))
      ;; The in-flight set is cleared by import-url!*'s own lifecycle in
      ;; production; here the stub bypasses it, so the failure only has to
      ;; leave the staging area alone.
      (Thread/sleep 200)
      (is (= [] (staging/waiting uid))))
    (swap! (us/get-atom uid :importing-urls) empty)))
