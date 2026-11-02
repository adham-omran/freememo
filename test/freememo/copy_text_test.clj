(ns freememo.copy-text-test
  "The count line and the error that a batch copy shows beside its button
   (plans/remove-toasts.md 2.4.1)."
  (:require
   [clojure.test :refer [deftest is]]
   [freememo.copy-text :as copy-text]))

(deftest the-count-line-names-the-pages-copied
  (is (= "Copied text for 12 pages" (copy-text/copied-line 12)))
  (is (= "Copied text for 1 page" (copy-text/copied-line 1))))

(deftest the-failed-part-is-an-error-and-absent-when-none-failed
  (is (nil? (copy-text/failed-line 0)))
  (is (nil? (copy-text/failed-line nil)))
  (is (= "1 page failed" (copy-text/failed-line 1)))
  (is (= "3 pages failed" (copy-text/failed-line 3))))
