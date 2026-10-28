(ns freememo.import-activity-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [cheshire.core :as json]
            [freememo.activity :as activity]
            [freememo.activity-reporter :as reporter]
            [freememo.api :as api]
            [freememo.biblio-import :as biblio]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.import-activity :as imports]
            [freememo.user-state :as us]
            [freememo.web-import :as web]
            [freememo.wikipedia :as wiki]))

(def uid 891274)
(def run-id "7dcc5adf-0061-4f2b-927a-9de925647e60")
(def state-keys [:url-import-runs :html-import-runs :importing-urls :html-import-cancelled])
(def rows (atom {}))

(defn- reset-state! []
  (doseq [k state-keys]
    (swap! (us/get-atom uid k) empty))
  (reset! rows {}))

(use-fixtures :each
  (fn [test!]
    (reset-state!)
    (try
      ;; Discovery is deliberately delayed until each test asks for it.
      (with-redefs [us/work-started! (constantly nil)
                    activity/start!
                    (fn [user-id type work-key reading]
                      (let [key [user-id (keyword type) work-key]
                            live (some (fn [[id row]]
                                         (when (and (= key (:key row)) (nil? (:outcome row))) id))
                                   @rows)
                            id (or live (inc (count @rows)))]
                        (when-not live
                          (swap! rows assoc id (assoc reading :key key)))
                        id))
                    activity/report!
                    (fn [_ id reading] (swap! rows update id merge reading))
                    activity/terminate!
                    (fn [_ id outcome opts]
                      (swap! rows update id merge opts {:outcome outcome}))]
        (test!))
      (finally (reset-state!)))))

(defn- discover! [] (reporter/open-rows! uid))
(defn- only-row [] (is (= 1 (count @rows))) (first (vals @rows)))
(defn- request [params] {:session {:user-id uid} :params params})
(defn- body [response] (json/parse-string (:body response) true))

(deftest fast-url-import-retains-history-until-discovery
  (let [result {:ok true :flow :already-exists :topic-id 17}]
    (is (= result (imports/run-url! uid "https://example.org/" (constantly result))))
    (is (empty? @(us/get-atom uid :importing-urls)))
    (is (empty? @rows))
    (discover!)
    (is (= "success" (:outcome (only-row))))
    (is (empty? @(us/get-atom uid :url-import-runs)))
    (discover!)
    (is (= 1 (count @rows)))))

(deftest failed-url-import-records-the-error
  (imports/run-url! uid "https://example.org/" #(hash-map :ok false :error "Fetch failed"))
  (discover!)
  (is (= {:outcome "failure" :error "Fetch failed"}
        (select-keys (only-row) [:outcome :error]))))

(deftest throwing-import-ends-membership-and-records-failure
  (is (thrown-with-msg? Exception #"Socket closed"
        (imports/run-url! uid "https://example.org/"
          #(throw (ex-info "Socket closed" {})))))
  (is (empty? @(us/get-atom uid :importing-urls)))
  (discover!)
  (is (= "failure" (:outcome (only-row)))))

(deftest shared-url-entry-point-publishes-before-fetch
  (let [seen (atom nil)]
    (with-redefs [wiki/fetch-url
                  (fn [_]
                    (discover!)
                    (reset! seen (only-row))
                    {:success false :error "Not reachable"})]
      (is (= {:ok false :error "Not reachable"}
            (web/import-url!* uid "https://example.org/"))))
    (is (= "Fetching" (:phase @seen)))
    (is (nil? (:outcome @seen)))
    (discover!)
    (is (= "failure" (:outcome (only-row))))))

(deftest html-uses-one-row-for-upload-review-and-processing
  (is (imports/start-html! uid run-id))
  (discover!)
  (is (= "Uploading" (:phase (only-row))))
  (imports/html-upload-ready! uid run-id)
  (discover!)
  (is (= "Awaiting confirmation" (:phase (only-row))))
  (is (= {:ok true :topic-id 17}
        (imports/run-html! uid run-id
          (fn []
            (discover!)
            (is (= "Importing" (:phase (only-row))))
            {:ok true :topic-id 17}))))
  (discover!)
  (is (= "success" (:outcome (only-row)))))

(deftest closing-review-cancels-and-prevents-late-import
  (imports/start-html! uid run-id)
  (imports/html-upload-ready! uid run-id)
  (imports/abandon-html! uid run-id "cancelled" nil)
  (discover!)
  (is (= "cancelled" (:outcome (only-row))))
  (imports/html-upload-ready! uid run-id)
  (is (not (imports/start-html! uid run-id)))
  (is (false? (:ok (imports/run-html! uid run-id
                     #(throw (ex-info "Must not create a topic" {}))))))
  (discover!)
  (is (= 1 (count @rows))))

(deftest cancellation-before-start-prevents-late-upload
  (imports/abandon-html! uid run-id "cancelled" nil)
  (is (not (imports/start-html! uid run-id)))
  (discover!)
  (is (empty? @rows)))

(deftest browser-failure-cannot-overwrite-server-processing
  (imports/start-html! uid run-id)
  (imports/run-html! uid run-id
    (fn []
      (imports/abandon-html! uid run-id "failure" "Connection lost")
      (imports/abandon-html! uid run-id "cancelled" nil)
      (discover!)
      (is (nil? (:outcome (only-row))))
      {:ok true :topic-id 17}))
  (discover!)
  (is (= "success" (:outcome (only-row)))))

(deftest paste-without-browser-tracking-still-has-an-activity
  (imports/run-html! uid nil #(hash-map :ok false :error "Cannot save HTML"))
  (discover!)
  (is (= {:outcome "failure" :error "Cannot save HTML"}
        (select-keys (only-row) [:outcome :error]))))

(deftest concurrent-discovery-does-not-reopen-a-finished-run
  (imports/run-url! uid "https://example.org/" #(hash-map :ok true))
  (let [passes (doall (repeatedly 8 #(future (discover!))))]
    (run! deref passes))
  (is (= "success" (:outcome (only-row)))))

(deftest html-http-lifecycle-keeps-the-import-id
  (let [saves (atom [])]
    (with-redefs [db/create-web-topic! (fn [& args] (swap! saves conj args) 17)
                  biblio/prepare-biblio! (fn [& _] nil)
                  commands/bump! (fn [& _] nil)]
      (is (:success (body (api/import-activity-handler
                           (request {"run-id" run-id "action" "start"})))))
      (imports/html-upload-ready! uid run-id)
      (discover!)
      (is (= {:success true :doc_id 17}
            (body (api/upload-paste-handler
                    (request {"format" "html" "title" "Example"
                              "content" "<p>Hello</p>" "import-run-id" run-id})))))
      (discover!)
      (is (= "success" (:outcome (only-row))))
      (is (= 1 (count @saves))))))

(deftest html-http-failure-records-the-server-error
  (with-redefs [db/create-web-topic! (fn [& _] (throw (ex-info "Save failed" {})))]
    (is (= 500 (:status (api/upload-paste-handler
                         (request {"format" "html" "title" "Example" "content" "<p>Hello</p>"})))))
    (discover!)
    (is (= {:outcome "failure" :error "Save failed"}
          (select-keys (only-row) [:outcome :error])))))

(deftest html-lifecycle-endpoint-requires-authentication-and-valid-id
  (is (= 401 (:status (api/import-activity-handler {:params {"run-id" run-id "action" "start"}}))))
  (is (= 400 (:status (api/import-activity-handler (request {"run-id" "bad" "action" "start"}))))))
