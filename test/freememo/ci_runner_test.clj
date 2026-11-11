(ns freememo.ci-runner-test
  "The CI selection rule: behavior with the database refused."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.ci-runner :as r]))

(def refused-block
  "ERROR in (a-test) (PSQLException.java:1)\nexpected: x\n  actual: org.postgresql.util.PSQLException: Connection to localhost:1 refused.\n")

(def wrong-block "FAIL in (b-test) (b.clj:3)\nexpected: (= 1 2)\n  actual: (not (= 1 2))\n")

(def pass-text "\nTesting x\n\nRan 1 tests containing 1 assertions.\n0 failures, 0 errors.\n")

(deftest classify-by-behavior
  (testing "no failure is a pass"
    (is (= :pass (r/classify pass-text nil))))
  (testing "only refused connections mean the suite needs the database"
    (is (= :skipped (r/classify (str pass-text refused-block) nil)))
    (is (= :skipped (r/classify pass-text (java.net.ConnectException. "Connection refused"))))
    (is (= :skipped (r/classify "FAIL in (c) (c.clj:1)\n  actual: :freememo.shell/missing-shadow-build-manifest\n" nil))))
  (testing "a SQL error is red, though it is a PSQLException"
    (is (= :fail (r/classify "ERROR in (d) (d.clj:1)\n  actual: org.postgresql.util.PSQLException: ERROR: column \"x\" does not exist\n" nil)))
    (is (= :fail (r/classify pass-text (java.sql.SQLException. "duplicate key value violates unique constraint")))))
  (testing "a Hikari line without a refusal is red"
    (is (= :fail (r/classify "ERROR in (e) (e.clj:1)\n  actual: HikariPool-1 - Pool misconfigured\nsome Exception text\n" nil))))
  (testing "a missing build beside another failure is red"
    (is (= :fail (r/classify (str "FAIL in (c) (c.clj:1)\n  actual: :freememo.shell/missing-shadow-build-manifest\n" wrong-block) nil))))
  (testing "any other failure is red, even beside a refusal"
    (is (= :fail (r/classify wrong-block nil)))
    (is (= :fail (r/classify (str refused-block wrong-block) nil)))
    (is (= :fail (r/classify pass-text (IllegalStateException. "boom"))))))

(deftest seam-1-suites-are-left-out
  (is (r/excluded-ns? 'freememo.local-install-test))
  (is (r/excluded-ns? 'freememo.local-postgres-test))
  (is (not (r/excluded-ns? 'freememo.local-login-test)))
  (is (not (some r/excluded-ns? (r/test-namespaces "test")))))

(deftest finds-this-suite
  (is (some #{'freememo.ci-runner-test} (r/test-namespaces "test"))))
