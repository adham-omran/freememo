(ns freememo.pdf-fit-setting-test
  "Checks the per-PDF fit that the viewer opens a document at."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.settings :as settings]))

(deftest a-document-with-no-stored-fit-opens-at-page-width
  (with-redefs [db/get-setting (fn [_ _] nil)]
    (is (= "page-width" (settings/get-pdf-fit 1 10)))))

(deftest each-document-reads-its-own-fit
  (with-redefs [db/get-setting (fn [_ key] (when (= key "pdf_fit_10") "page-fit"))]
    (is (= "page-fit" (settings/get-pdf-fit 1 10)))
    (is (= "page-width" (settings/get-pdf-fit 1 11)))))

(deftest an-unknown-stored-fit-reads-as-the-default
  ;; A numeric zoom is not a fit. If one is ever stored, the viewer must not
  ;; receive it as a currentScaleValue it cannot name.
  (with-redefs [db/get-setting (fn [_ _] "1.5")]
    (is (= "page-width" (settings/get-pdf-fit 1 10)))))

(deftest a-failed-read-opens-at-the-default
  (with-redefs [db/get-setting (fn [_ _] (throw (ex-info "connection refused" {})))]
    (is (= "page-width" (settings/get-pdf-fit 1 10)))))

(deftest save-writes-only-a-known-fit
  (let [!writes (atom [])]
    (with-redefs [db/set-setting (fn [_ key value] (swap! !writes conj [key value]))]
      (testing "a known fit is stored under the document's key"
        (is (:success (settings/save-pdf-fit 1 10 "page-fit")))
        (is (= [["pdf_fit_10" "page-fit"]] @!writes)))
      (testing "anything else is refused and not written"
        (reset! !writes [])
        (is (false? (:success (settings/save-pdf-fit 1 10 "1.5"))))
        (is (false? (:success (settings/save-pdf-fit 1 10 nil))))
        (is (= [] @!writes))))))
