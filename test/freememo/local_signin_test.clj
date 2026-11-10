(ns freememo.local-signin-test
  "Unit tests for `freememo.local-signin`: the PKCE state machine, with no JVM,
   no PostgreSQL and no network. The Seam 1 tests are in
   `freememo.local-install-signin-test`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-signin :as signin]))

(deftest the-challenge-is-the-rfc-7636-s256-value
  (is (= "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM"
        (signin/challenge-of "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"))))

(defn- flow [& {:keys [online? saved]}]
  (signin/make-flow {:origin "https://example.test" :local-origin "http://127.0.0.1:5000"
                     :save-account! #(swap! saved conj %)
                     :online-fn (constantly online?)}))

(defn- start-sign-in! [{:keys [handler]}]
  (let [location (get-in (handler {:request-method :post :uri "/first-launch/sign-in"}) [:headers "Location"])]
    (is (str/starts-with? location "https://example.test/desktop/authorize?"))
    (is (str/includes? location "code_challenge_method=S256"))
    (is (str/includes? location "redirect_uri=http%3A%2F%2F127.0.0.1%3A5000%2Fcallback"))
    (second (re-find #"state=([^&]+)" location))))

(defn- callback [{:keys [handler]} state]
  (handler {:request-method :get :uri "/callback" :query-string (str "code=abc&state=" state)}))

(deftest offline-the-page-says-so-and-offers-a-retry
  (let [body (:body ((:handler (flow :online? false :saved (atom []))) {:request-method :get :uri "/"}))]
    (is (str/includes? body "Connect to the internet for the first start"))
    (is (str/includes? body "Try again"))
    (is (not (str/includes? body "Sign in")))))

(deftest online-the-page-offers-sign-in
  (let [body (:body ((:handler (flow :online? true :saved (atom []))) {:request-method :get :uri "/"}))]
    (is (str/includes? body "Sign in"))
    (is (not (str/includes? body "Connect to the internet")))))

(deftest a-wrong-state-is-refused-and-saves-nothing
  (let [saved (atom [])
        f (flow :online? true :saved saved)
        state (start-sign-in! f)]
    (is (= 400 (:status (callback f "wrong"))))
    (is (empty? @saved))
    (is (not (realized? (:account f))))
    (testing "a stray request does not cancel the pending sign-in"
      (with-redefs [signin/exchange-code! (fn [& _] {"access_token" "t" "user_id" 3 "username" "u"})]
        (is (= 200 (:status (callback f state))))
        (is (= 1 (count @saved)))
        (is (= "t" (get (first @saved) "token")))
        (is (true? (get (first @saved) "first_download")))))))

(deftest a-state-works-once
  (let [saved (atom [])
        f (flow :online? true :saved saved)
        state (start-sign-in! f)]
    (with-redefs [signin/exchange-code! (fn [& _] {"access_token" "t" "user_id" 3 "username" "u"})]
      (callback f state)
      (callback f state)
      (is (= 1 (count @saved)) "the second callback saves nothing"))))

(deftest a-callback-with-no-pending-sign-in-is-refused
  (let [saved (atom [])
        f (flow :online? true :saved saved)]
    (is (= 400 (:status (callback f "b"))))
    (is (empty? @saved))))

(deftest a-refused-code-saves-nothing
  (let [saved (atom [])
        f (flow :online? true :saved saved)
        state (start-sign-in! f)]
    (with-redefs [signin/exchange-code! (fn [& _] (throw (ex-info "Sign-in failed: refused" {:type ::signin/refused})))]
      (let [response (callback f state)]
        (is (str/includes? (:body response) "Sign-in failed"))
        (is (empty? @saved))))))
