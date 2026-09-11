(ns freememo.card-generation-fact-setting-test
  "Checks the global and per-document controls for fact-aware card generation."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.content-toolbar-helpers :as helpers]
            [freememo.db :as db]
            [freememo.settings :as settings]))

(deftest the-account-setting-defaults-to-on
  (with-redefs [db/get-setting (fn [_ _] nil)]
    (is (true? (settings/get-use-facts-for-card-generation 1)))))

(deftest a-document-can-override-the-account-default
  (testing "the account default applies when the document has no override"
    (with-redefs [db/get-setting
                  (fn [_ key]
                    (when (= key settings/USE_FACTS_FOR_CARD_GENERATION) "false"))]
      (is (false? (settings/effective-use-facts-for-card-generation 1 10)))))
  (testing "a document can turn facts on when the account default is off"
    (with-redefs [db/get-setting
                  (fn [_ key]
                    (case key
                      "use_facts_for_card_generation" "false"
                      "use_facts_for_card_generation_10" "true"
                      nil))]
      (is (true? (settings/effective-use-facts-for-card-generation 1 10))))))

(deftest disabling-facts-does-not-query-the-knowledge-graph
  (with-redefs [settings/effective-use-facts-for-card-generation (fn [_ _] false)
                db/get-kg-facts (fn [& _]
                                  (throw (ex-info "Facts must not be queried" {})))]
    (is (= {:content "Passage" :from-facts? false}
          (helpers/resolve-gen-content "Passage" 1 10)))))
