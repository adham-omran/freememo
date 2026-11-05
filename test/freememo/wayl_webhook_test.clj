(ns freememo.wayl-webhook-test
  "The Wayl webhook handler with a signed body (plans/wayl-link-lifecycle.md §5).
   Needs no PostgreSQL: the routing fn is stubbed."
  (:require
   [cheshire.core :as json]
   [clojure.test :refer [deftest is testing]]
   [freememo.api :as api]
   [freememo.config :as config]
   [freememo.credits :as credits]
   [taoensso.telemere :as tel])
  (:import
   [javax.crypto Mac]
   [javax.crypto.spec SecretKeySpec]))

(def ^:private secret "webhook-test-secret")

(defn- sign [^bytes body]
  (let [mac (Mac/getInstance "HmacSHA256")]
    (.init mac (SecretKeySpec. (.getBytes secret "UTF-8") "HmacSHA256"))
    (apply str (map #(format "%02x" (bit-and % 0xff)) (.doFinal mac body)))))

(defn- request [payload signature-fn]
  (let [body (.getBytes (json/generate-string payload) "UTF-8")]
    {:request-method :post :uri "/api/wayl/webhook" :remote-addr "203.0.113.9"
     :headers {"x-wayl-signature-256" (signature-fn body)}
     :body (java.io.ByteArrayInputStream. body)}))

(def ^:private paid-payload
  {:event "order.paid" :referenceId "fm-a" :paymentStatus "Complete" :status "Delivered"
   :total 5000
   :customer {:name "Buyer" :phone "+9647000000000" :address "Baghdad"}})

(defn- received [signals]
  (filter #(= :freememo.api/wayl-webhook-received (:id %)) signals))

(deftest a-verified-webhook-logs-one-line-with-no-customer
  (let [calls (atom [])]
    (with-redefs [config/wayl-webhook-secret (constantly secret)
                  credits/apply-wayl-status! (fn [ref status total]
                                               (swap! calls conj [ref status total])
                                               {:outcome :credited :user-id 50 :amount 5000})]
      (let [{:keys [value signals]} (tel/with-signals
                                      (api/wayl-webhook-handler (request paid-payload sign)))
            [line & more] (received signals)]
        (testing "5.2: 200, and the payment status wins over the delivery status"
          (is (= 200 (:status value)))
          (is (= [["fm-a" "Complete" 5000]] @calls)))
        (testing "5.1: one :info line with the outcome and no customer field"
          (is (some? line))
          (is (empty? more))
          (is (= :info (:level line)))
          (is (= {:reference-id "fm-a" :event "order.paid" :payment-status "Complete"
                  :status "Delivered" :outcome :credited}
                 (:data line)))
          (is (not (re-find #"(?i)customer|Buyer|9647|Baghdad" (pr-str (:data line))))))
        (testing "the credit still logs ::wayl-credited, which is mailed"
          (is (some #(= :freememo.api/wayl-credited (:id %)) signals)))))))

(deftest a-forged-webhook-is-refused
  (with-redefs [config/wayl-webhook-secret (constantly secret)
                credits/apply-wayl-status! (fn [& _] (throw (ex-info "must not run" {})))]
    (let [{:keys [value signals]} (tel/with-signals
                                    (api/wayl-webhook-handler
                                      (request paid-payload (constantly "00"))))]
      (is (= 401 (:status value)))
      (is (empty? (received signals))))))

(deftest an-internal-error-asks-wayl-to-retry
  (with-redefs [config/wayl-webhook-secret (constantly secret)
                credits/apply-wayl-status! (fn [& _] (throw (ex-info "db down" {})))]
    (is (= 500 (:status (api/wayl-webhook-handler (request paid-payload sign)))))))

(deftest a-blank-reference-is-accepted-and-changes-nothing
  (with-redefs [config/wayl-webhook-secret (constantly secret)
                credits/apply-wayl-status! (fn [& _] (throw (ex-info "must not run" {})))]
    (is (= 200 (:status (api/wayl-webhook-handler
                          (request (dissoc paid-payload :referenceId) sign)))))))
