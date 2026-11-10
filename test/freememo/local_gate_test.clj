(ns freememo.local-gate-test
  "The Local Install gate (docs/adr/0001-jvm-launcher-replaces-electron.md).
   Needs no database and no JVM start."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.local-gate :as gate]))

(def origin "http://127.0.0.1:47077")
(def secret "a-cookie-secret")

(defn- make [& [{:keys [now-ms]}]]
  (let [calls (atom [])
        {:keys [wrap issue-token!]}
        (gate/make-gate (cond-> {:origin origin :secret secret :user-id 7
                                 :local-route (fn [r] (when (= "/api/desktop/status" (:uri r))
                                                        {:status 200 :body "status"}))}
                          now-ms (assoc :now-ms now-ms)))
        handler (wrap (fn [request]
                        (swap! calls conj request)
                        {:status 200 :headers {"Content-Type" "text/html"}
                         :body "<title>FreeMemo — x</title>"}))]
    {:handler handler :issue-token! issue-token! :calls calls}))

(defn- request [overrides]
  (merge {:request-method :get :uri "/" :headers {"host" "127.0.0.1:47077"
                                                  "cookie" (str "fm_local=" secret)}}
    overrides))

(deftest a-request-with-the-cookie-is-signed-in-and-reaches-the-app
  (let [{:keys [handler calls]} (make)
        response (handler (request {}))]
    (is (= 200 (:status response)))
    (is (= 7 (get-in (first @calls) [:session :user-id])))
    (is (str/includes? (:body response) "<title>FreeMemo Local") "the title carries the mark")))

(deftest static-assets-need-the-cookie-too
  (let [{:keys [handler calls]} (make)]
    (doseq [uri ["/freememo/index.css" "/freememo/js/main.ABC.js" "/freememo/favicon.svg"]]
      (is (= 401 (:status (handler (request {:uri uri :headers {"host" "127.0.0.1:47077"}})))) uri))
    (is (empty? @calls) "nothing reached the app")))

(deftest a-wrong-cookie-is-refused
  (let [{:keys [handler]} (make)]
    (is (= 401 (:status (handler (request {:headers {"host" "127.0.0.1:47077" "cookie" "fm_local=nope"}})))))))

(deftest a-wrong-host-is-refused-even-with-the-cookie
  (let [{:keys [handler]} (make)]
    (doseq [host ["evil.example" "127.0.0.1:1" "localhost:1" "localhost" nil]]
      (is (= 403 (:status (handler (request {:headers {"host" host "cookie" (str "fm_local=" secret)}}))))
        (pr-str host)))))

(deftest a-state-changing-request-needs-the-local-origin
  (let [{:keys [handler]} (make)
        post (fn [origin-header]
               (handler (request {:request-method :post :uri "/api/x"
                                  :headers (cond-> {"host" "127.0.0.1:47077" "cookie" (str "fm_local=" secret)}
                                             origin-header (assoc "origin" origin-header))})))]
    (is (= 403 (:status (post "https://evil.example"))))
    (is (= 403 (:status (post nil))))
    (is (= 200 (:status (post origin))))
    (testing "a WebSocket upgrade from a foreign page"
      (is (= 403 (:status (handler (request {:headers {"host" "127.0.0.1:47077" "upgrade" "websocket"
                                                       "origin" "https://evil.example"
                                                       "cookie" (str "fm_local=" secret)}}))))))))

(deftest a-token-is-exchanged-for-a-cookie-once
  (let [{:keys [handler issue-token!]} (make)
        token (issue-token!)
        exchange #(handler {:request-method :get :uri "/learn" :query-string (str "a=1&fm_token=" token)
                            :headers {"host" "127.0.0.1:47077"}})
        first-try (exchange)
        set-cookie (get-in first-try [:headers "Set-Cookie"])]
    (is (= 302 (:status first-try)))
    (is (= "/learn?a=1" (get-in first-try [:headers "Location"])) "the token leaves the URL")
    (is (str/includes? set-cookie (str "fm_local=" secret)))
    (is (str/includes? set-cookie "HttpOnly"))
    (is (str/includes? set-cookie "SameSite=Strict"))
    (is (= 401 (:status (exchange))) "a second use is refused")))

(deftest a-token-expires
  (let [t (atom 0)
        {:keys [handler issue-token!]} (make {:now-ms #(deref t)})
        token (issue-token!)]
    (reset! t (inc gate/token-ttl-ms))
    (is (= 401 (:status (handler {:request-method :get :uri "/" :query-string (str "fm_token=" token)
                                  :headers {"host" "127.0.0.1:47077"}}))))))

(deftest the-shell-endpoints-answer-behind-the-gate
  (let [{:keys [handler calls]} (make)]
    (is (= "status" (:body (handler (request {:uri "/api/desktop/status"})))))
    (is (empty? @calls))))

(deftest localhost-is-an-accepted-host-and-origin
  (let [{:keys [handler]} (make)]
    (is (= 200 (:status (handler (request {:headers {"host" "localhost:47077" "cookie" (str "fm_local=" secret)}})))))
    (is (= 200 (:status (handler (request {:request-method :post
                                           :headers {"host" "localhost:47077" "cookie" (str "fm_local=" secret)
                                                     "origin" "http://localhost:47077"}})))))
    (is (= 403 (:status (handler (request {:request-method :post
                                           :headers {"host" "localhost:47077" "cookie" (str "fm_local=" secret)
                                                     "origin" "http://localhost:1"}})))))))
