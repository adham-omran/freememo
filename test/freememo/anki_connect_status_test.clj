(ns freememo.anki-connect-status-test
  "The five Connect outcomes (plans/getting-started-checklist.md 8.6.1)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.anki-connect-status :as status]))

(deftest granted
  (is (= :granted (status/classify {:result {:permission "granted"
                                             :requireApiKey false :version 6}}))))

(deftest denied
  (testing "No in Anki, or an origin on ignoreOriginList"
    (is (= :denied (status/classify {:result {:permission "denied"}})))))

(deftest needs-key-under-both-spellings
  (is (= :needs-key (status/classify {:result {:permission "granted"
                                               :requireApiKey true :version 6}})))
  (is (= :needs-key (status/classify {:result {:permission "granted"
                                               :requireApikey true :version 6}}))))

(deftest failures
  (is (= :timeout (status/classify {:failure :timeout})))
  (is (= :unreachable (status/classify {:failure :unreachable}))))

(deftest outdated
  (is (status/outdated? {:version 5}))
  (is (not (status/outdated? {:version 6})))
  (is (not (status/outdated? {}))))
