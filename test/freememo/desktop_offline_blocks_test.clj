(ns freememo.desktop-offline-blocks-test
  "plans/electron-local-db-sync.md 15.3.1, 15.3.5, 15.4.2, 15.5.8, 15.7.1,
   15.7.2: the server half of a local account's sign-in. Server peer. The
   database tests need PostgreSQL with the server schema, and skip when it is
   down."
  (:require [clojure.test :refer [deftest is testing]]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.desktop-auth :as auth]
            [freememo.desktop-license :as license]
            [freememo.desktop-link :as link]
            [freememo.desktop-version :as desktop-version]
            [freememo.sync-schema :as schema]
            [freememo.activity :as activity])
  (:import [java.time Duration Instant]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT merge_opened_at FROM desktop_tokens LIMIT 1"]) true
       (catch Exception _ false)))

(defn- one [sql & params] (jdbc/execute-one! db/ds (into [sql] params) opts))

(defn- install!
  "A user with one desktop token. Returns {:user :token-id :token}, where
   `token` is the bearer secret."
  []
  (let [u (:id (one "INSERT INTO users (username) VALUES (?) RETURNING id" (str "offline-" (System/nanoTime))))
        secret (str "secret-" (System/nanoTime))
        t (:id (one "INSERT INTO desktop_tokens (user_id, token_hash) VALUES (?, ?) RETURNING id"
                 u (auth/sha256-hex secret)))]
    {:user u :token-id t :token secret}))

(defn- wire [{:keys [start end]}] {"start" start "end" end})

(defn- window [token-id]
  (one "SELECT merge_opened_at, merge_closed_at FROM desktop_tokens WHERE id = ?" token-id))

(deftest an-offline-block-is-aligned-and-in-the-offline-range
  (dotimes [_ 1000]
    (let [{:keys [start end]} (schema/random-offline-block)]
      (is (<= schema/offline-id-floor start))
      (is (<= end schema/offline-id-end))
      (is (= schema/block-size (- end start)))
      (is (zero? (mod (- start schema/offline-id-floor) schema/block-size))))))

(deftest registration-accepts-new-blocks-and-refuses-overlaps
  (when (db-up?)
    (let [a (install!)
          b (install!)
          block (wire (schema/random-offline-block))]
      (testing "15.5.8: a new block is accepted and opens the merge window"
        (is (= {:accepted [block] :refused []} (auth/register-offline-blocks! (:user a) (:token-id a) [block])))
        (is (some? (:merge_opened_at (window (:token-id a))))))
      (testing "a retry gives the same answer and does not move the window"
        (let [opened (:merge_opened_at (window (:token-id a)))]
          (is (= {:accepted [block] :refused []} (auth/register-offline-blocks! (:user a) (:token-id a) [block])))
          (is (= opened (:merge_opened_at (window (:token-id a)))))))
      (testing "another account's block is refused, and no window opens"
        (is (= {:accepted [] :refused [block]} (auth/register-offline-blocks! (:user b) (:token-id b) [block])))
        (is (nil? (:merge_opened_at (window (:token-id b))))))
      (testing "a block outside the offline range or the alignment is refused"
        (let [low {"start" schema/client-id-floor "end" (+ schema/client-id-floor schema/block-size)}
              shifted {"start" (+ (get block "start") 1) "end" (+ (get block "end") 1)}]
          (is (= {:accepted [] :refused [low shifted]}
                (auth/register-offline-blocks! (:user b) (:token-id b) [low shifted]))))))))

(deftest the-merge-window-lets-one-token-upload-for-a-limited-time
  (when (db-up?)
    (let [a (install!)
          _ (auth/register-offline-blocks! (:user a) (:token-id a) [(wire (schema/random-offline-block))])
          access #(license/token-access db/ds (:user a) (:token-id a) desktop-version/desktop-major %)]
      (testing "15.7.2: open after a new block"
        (is (true? (:merge-upload? (access (Instant/now))))))
      (testing "closed after 7 days"
        (is (nil? (:merge-upload? (access (.plus (Instant/now) (Duration/ofDays 8)))))))
      (testing "closed by merge-done"
        (is (= 200 (:status (auth/merge-done-handler {:headers {"authorization" (str "Bearer " (:token a))}}))))
        (is (some? (:merge_closed_at (window (:token-id a)))))
        (is (nil? (:merge-upload? (access (Instant/now))))))
      (testing "a token with no window has none"
        (let [b (install!)]
          (is (nil? (:merge-upload? (license/token-access db/ds (:user b) (:token-id b)
                                      desktop-version/desktop-major)))))))))

(deftest an-open-window-allows-the-upload-only
  (testing "15.7.1: with no sync, the window allows :upload and nothing else"
    (let [refused {:state :unlicensed :sync :none}
          merging (assoc refused :merge-upload? true)]
      (is (not (license/allows? refused :upload)))
      (is (license/allows? merging :upload))
      (is (not (license/allows? merging :download)))
      (is (not (license/allows? merging :graves)))
      (is (not (license/allows? merging :ai))))))

(deftest server-blocks-stop-below-the-offline-range
  (when (db-up?)
    (let [a (install!)]
      (testing "15.3.5: a block that would end above the floor is refused before it is written"
        (with-redefs [schema/offline-id-floor schema/client-id-floor]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"offline id range"
                (auth/allocate-block! db/ds (:user a) (:token-id a)))))
        (is (nil? (one "SELECT 1 FROM id_blocks WHERE token_id = ?" (:token-id a))))))))

(deftest a-failure-for-a-local-account-offers-the-sign-in
  (testing "15.4.2: the failure banner reads the kind and shows Sign in"
    (is (= "needs-account" (activity/error-kind link/sign-in-message)))
    (is (= "needs-account"
          (activity/error-kind (str "Card generation failed: " link/sign-in-message))))
    (is (= "needs-account"
          (activity/error-kind (ex-info link/sign-in-message {:type ::link/sign-in-required}))))
    (is (nil? (activity/error-kind "Rate limited.")))))

(deftest the-server-peer-never-reports-offline
  (testing "15.2.3: the offline guard is a desktop guard"
    (try
      (link/set-online! false)
      (is (nil? (link/offline-message "Wikipedia import")))
      (is (not (link/local-account?)))
      (finally (link/set-online! true)))))
