(ns freememo.local-account-test
  "Unit tests for `freememo.local-account`: one library, one account. No JVM,
   no PostgreSQL, no network. The Seam 1 test is
   `freememo.local-install-account-test`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-account :as acct]))

(def ^:private account-a {"origin" "https://example.test" "token" "ta" "user_id" 1 "username" "ann"})

(defn- guard
  "A guard over fakes. Post: `{:handle (fn [request]) :calls atom :state atom}`."
  [{:keys [account revoked unsynced] :or {unsynced 0}}]
  (let [calls (atom [])
        state (atom {:revoked revoked :ready true})
        note (fn [k] (fn [& args] (swap! calls conj (into [k] args))))
        {:keys [wrap user-id]} (acct/make-guard
                                 {:origin "https://example.test" :local-origin "http://127.0.0.1:5000"
                                  :account account :online-fn (constantly true)
                                  :save-file! (note :save-file) :delete-file! (note :delete-file)
                                  :adopt! (note :adopt) :renew! (note :renew) :wipe! (note :wipe)
                                  :unsynced-count (constantly unsynced)
                                  :revoked? #(:revoked @state) :library-ready? #(:ready @state)})
        handle (wrap (fn [_] {:status 200 :body "the app"}))]
    {:handle handle :calls calls :state state :user-id user-id}))

(defn- sign-in!
  "Post: the callback answer of a sign-in whose token belongs to `token-account`."
  [{:keys [handle]} token-account]
  (let [location (get-in (handle {:request-method :post :uri "/first-launch/sign-in"}) [:headers "Location"])
        state (second (re-find #"state=([^&]+)" location))]
    (with-redefs [freememo.local-signin/exchange-code!
                  (fn [& _] {"access_token" (get token-account "token") "user_id" (get token-account "user_id")
                             "username" (get token-account "username")})]
      (handle {:request-method :get :uri "/callback" :query-string (str "code=c&state=" state)}))))

(deftest a-sign-in-with-another-account-is-refused-and-writes-nothing
  (let [g (guard {:account account-a})
        answer (sign-in! g {"token" "tb" "user_id" 2 "username" "bob"})]
    (is (= 200 (:status answer)))
    (is (str/includes? (:body answer) "/remove-library"))
    (is (str/includes? (:body answer) "bob"))
    (is (= [] @(:calls g)))
    (is (= 1 ((:user-id g))))))

(deftest a-sign-in-with-the-same-account-renews-the-token-and-keeps-the-library
  (let [g (guard {:account account-a})]
    (is (= 200 (:status (sign-in! g {"token" "ta2" "user_id" 1 "username" "ann"}))))
    (is (= [:save-file :renew] (mapv first @(:calls g))))
    (is (= "ta2" (get-in @(:calls g) [0 1 "token"])))
    (is (not (contains? (get-in @(:calls g) [0 1]) "first_download")))))

(deftest a-revoked-token-shows-a-sign-in-page-for-the-same-account
  (let [g (guard {:account account-a :revoked true})
        page (:body ((:handle g) {:request-method :get :uri "/"}))]
    (is (str/includes? page "ann"))
    (is (str/includes? page "/first-launch/sign-in"))
    (testing "other paths still reach the app"
      (is (= "the app" (:body ((:handle g) {:request-method :get :uri "/x"})))))
    (testing "a live token shows the app"
      (swap! (:state g) assoc :revoked false)
      (is (= "the app" (:body ((:handle g) {:request-method :get :uri "/"})))))))

(deftest the-remove-page-shows-the-count-and-removes-only-after-confirm
  (let [g (guard {:account account-a :unsynced 2})
        page (:body ((:handle g) {:request-method :get :uri "/remove-library"}))]
    (is (str/includes? page "<strong>2</strong> changes"))
    (testing "no confirmation, no removal"
      (is (= 400 (:status ((:handle g) {:request-method :post :uri "/remove-library"
                                        :body (java.io.ByteArrayInputStream. (.getBytes ""))}))))
      (is (= [] @(:calls g))))
    (testing "confirmed"
      (is (= 303 (:status ((:handle g) {:request-method :post :uri "/remove-library"
                                        :body (java.io.ByteArrayInputStream. (.getBytes "confirm=yes"))}))))
      (is (= [:wipe :delete-file] (mapv first @(:calls g))))
      (is (nil? ((:user-id g)))))))

(deftest after-a-removal-any-account-signs-in-and-the-first-download-shows
  (let [g (guard {:account account-a})]
    ((:handle g) {:request-method :post :uri "/remove-library"
                  :body (java.io.ByteArrayInputStream. (.getBytes "confirm=yes"))})
    (testing "GET / is the first-launch page, not the app"
      (is (str/includes? (:body ((:handle g) {:request-method :get :uri "/"})) "Sign in")))
    (reset! (:calls g) [])
    (is (= 200 (:status (sign-in! g {"token" "tb" "user_id" 2 "username" "bob"}))))
    (is (= [:save-file :adopt] (mapv first @(:calls g))))
    (is (= 2 ((:user-id g))))
    (testing "the progress page shows until the download commits"
      (swap! (:state g) assoc :ready false)
      (is (str/includes? (:body ((:handle g) {:request-method :get :uri "/"})) "Getting your library"))
      (swap! (:state g) assoc :ready true)
      (is (= "the app" (:body ((:handle g) {:request-method :get :uri "/"})))))))
