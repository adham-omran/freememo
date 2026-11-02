(ns freememo.import-activity-integration-test
  "Import activity checks with PostgreSQL and the real reporter."
  (:require [clojure.test :refer [deftest is]]
            [cheshire.core :as json]
            [freememo.activity :as activity]
            [freememo.activity-reporter :as reporter]
            [freememo.api :as api]
            [freememo.db :as db]
            [freememo.import-activity :as imports]
            [freememo.user-state :as us]
            [freememo.web-import :as web]
            [freememo.wikipedia :as wiki]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- with-user! [test!]
  (let [user-id (:id (jdbc/execute-one! db/ds
                       ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                        (str "import-activity-" (random-uuid))
                        (str (random-uuid) "@example.test")]
                       {:builder-fn rs/as-unqualified-maps}))]
    (try
      (with-redefs [us/work-started! (constantly nil)]
        (test! user-id))
      (finally
        (doseq [k [:url-import-runs :html-import-runs :importing-urls :html-import-cancelled]]
          (swap! (us/get-atom user-id k) empty))
        (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user-id])))))

(deftest html-upload-review-and-import-share-one-database-row
  (with-user!
    (fn [uid]
      (let [run-id (str (random-uuid))
            request! (fn [params] {:session {:user-id uid} :params params})
            content (.getBytes "<title>Local article</title><p>Example</p>" "UTF-8")]
        (is (= 200 (:status (api/import-activity-handler
                             (request! {"run-id" run-id "action" "start"})))))
        (reporter/open-rows! uid)
        (let [id (:id (first (activity/user-activities uid)))]
          (is (= "Uploading" (:phase (first (activity/user-activities uid)))))
          (let [response (api/upload-file-handler
                           (request! {"file" {:filename "article.html"
                                              :content-type "text/html"
                                              :tempfile content}
                                      "import-run-id" run-id}))]
            (is (= 200 (:status response)))
            (is (= "html" (:flow (json/parse-string (:body response) true)))))
          (reporter/open-rows! uid)
          (is (= "Awaiting confirmation" (:phase (first (activity/user-activities uid)))))
          (let [response (api/upload-paste-handler
                           (request! {"format" "html" "title" "Local article"
                                      "content" "<p>Example</p>" "import-run-id" run-id}))
                result (json/parse-string (:body response) true)]
            (is (:success result))
            (is (= "Local article" (:topics/title (db/get-topic (:doc_id result))))))
          (reporter/open-rows! uid)
          (let [rows (activity/user-activities uid)]
            (is (= 1 (count rows)))
            (is (= id (:id (first rows))))
            (is (= "success" (:outcome (first rows))))
            (is (zero? (activity/live-count uid)))))))))

(deftest url-fetch-failure-remains-in-database-history-before-first-tick
  (with-user!
    (fn [uid]
      (with-redefs [wiki/fetch-url (fn [_] {:success false :error "Fetch failed"})]
        (is (false? (:ok (web/import-url!* uid "https://example.test/missing")))))
      (reporter/open-rows! uid)
      (reporter/open-rows! uid)
      (let [rows (activity/user-activities uid)]
        (is (= 1 (count rows)))
        (is (= "failure" (:outcome (first rows))))
        (is (= "Fetch failed" (:error (first rows))))))))

(deftest review-cancellation-remains-cancelled-after-a-late-upload
  (with-user!
    (fn [uid]
      (let [run-id (str (random-uuid))]
        (imports/start-html! uid run-id)
        (reporter/open-rows! uid)
        (imports/html-upload-ready! uid run-id)
        (imports/abandon-html! uid run-id "cancelled" nil)
        (reporter/open-rows! uid)
        (imports/html-upload-ready! uid run-id)
        (reporter/open-rows! uid)
        (let [rows (activity/user-activities uid)]
          (is (= 1 (count rows)))
          (is (= "cancelled" (:outcome (first rows))))
          (is (zero? (activity/live-count uid))))))))

(defn- await-outcome
  "The first retained URL-import run that has an outcome, or :timeout."
  [uid]
  (loop [n 0]
    (if-let [run (->> @(us/get-atom uid :url-import-runs) vals (filter :outcome) first)]
      run
      (if (> n 100) :timeout (do (Thread/sleep 50) (recur (inc n)))))))

(deftest editor-link-import-does-not-open-a-second-activity
  (with-user!
    (fn [uid]
      (with-redefs [wiki/fetch-url (fn [_] {:success false :error "Fetch failed"})]
        (is (= {:flow :queued}
              (web/start-url-import!* uid "https://example.test/editor-link")))
        ;; The retained run announces the end. Its
        ;; outcome and its error text are what the banner strip reads.
        (let [run (await-outcome uid)]
          (is (not= :timeout run))
          (is (= "failure" (:outcome run)))
          (is (= "Fetch failed" (:error run)))))
      (reporter/open-rows! uid)
      (reporter/open-rows! uid)
      (let [rows (activity/user-activities uid)]
        (is (= 1 (count rows)))
        (is (= "url-import" (:type (first rows))))
        (is (= "failure" (:outcome (first rows))))
        (is (= "Fetch failed" (:error (first rows))))))))
