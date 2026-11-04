(ns freememo.contact-support-test
  "Contact support (plans/paying-customer-commitments/issues/02-contact-support.md).
   The pure text seam: (paying?, account) -> mailto subject, body and reply line."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [freememo.contact-support :as cs])
  (:import (java.net URLDecoder)))

(defn- query-param [href k]
  (let [q (second (str/split href #"\?" 2))]
    (some (fn [kv]
            (let [[kk v] (str/split kv #"=" 2)]
              (when (= kk k) (URLDecoder/decode (str/replace v "+" "%2B") "UTF-8"))))
          (str/split q #"&"))))

(deftest a-paying-customer-gets-the-paid-subject-and-one-day
  (let [href (cs/mailto-href true "ann@example.test")]
    (is (str/starts-with? href "mailto:contact@adham-omran.com?"))
    (is (= "[Paid] FreeMemo support" (query-param href "subject")))
    (is (= "Account: ann@example.test\n\n\n" (query-param href "body")))
    (is (= "You get a reply within 1 working day. Working days are Saturday to Wednesday."
           (cs/reply-line true)))))

(deftest every-other-account-gets-the-plain-subject-and-three-days
  (let [href (cs/mailto-href false "bob")]
    (is (= "FreeMemo support" (query-param href "subject")))
    (is (= "Account: bob\n\n\n" (query-param href "body")))
    (is (= "You get a reply within 3 working days. Working days are Saturday to Wednesday."
           (cs/reply-line false)))))

(deftest the-mailto-encodes-so-a-mail-client-shows-the-text-as-written
  (let [href (cs/mailto-href false "a&b+c d@x.test")]
    (is (not (str/includes? href " ")))
    (is (not (str/includes? href "\n")))
    (is (= "Account: a&b+c d@x.test\n\n\n" (query-param href "body")))
    (is (not (str/includes? (second (str/split href #"\?" 2)) "+"))
        "a plus sign is %2B or the client reads it as a space")))
