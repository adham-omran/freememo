(ns freememo.crawler-shell-test
  "Pins the branch that serves the crawler document through the app shell.
   Serving the document at every path would put identical text on unbounded
   URLs, and serving markup at a machine-read path reads as a malformed file.

   These tests read the client build manifest (`public/freememo/js/manifest.edn`).
   Without a build, `wrap-prod-index-page` answers every request with
   `:freememo.shell/missing-shadow-build-manifest`. So a fixture fails the suite
   once, names that cause, and runs no test. CI skips a suite whose every
   failure names it (test/CLAUDE.md, \"CI\"). Run `build-client` to run these tests."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :as t :refer [deftest is testing]]
            [freememo.api :as api]
            [freememo.availability :as availability]
            [freememo.landing-copy :as lc]
            [freememo.legal-copy :as legal]
            [freememo.shell :as shell]))

(def ^:private manifest-path "public/freememo/js/manifest.edn")

(defn- require-client-build
  "Run the tests only when the manifest exists. Else fail once, with the cause."
  [run-tests]
  (if (io/resource manifest-path)
    (run-tests)
    (t/do-report {:type :fail
                  :message "The client build is missing, so no shell test can run. Run build-client."
                  :expected :a-client-build-manifest
                  :actual ::shell/missing-shadow-build-manifest})))

(t/use-fixtures :once require-client-build)

(defn- executable-scripts
  "HTML → the opening tag of every `<script>` a browser would run.
   A JSON-LD block is data, so it is not counted."
  [html]
  (remove #(str/includes? % "type=\"application/ld+json\"")
          (re-seq #"<script\b[^>]*>" html)))

(def ^:private handler
  (shell/wrap-prod-index-page
    (fn [_] {:status 599 :body "fell through to next-handler"})
    {:resources-path "public" :manifest-path manifest-path}))

(defn- get-as [uri ua]
  (handler {:uri uri :request-method :get :headers (cond-> {} ua (assoc "user-agent" ua))}))

(def ^:private bot "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
(def ^:private browser "Mozilla/5.0 (Macintosh) AppleWebKit/537.36 Chrome/151.0.0.0 Safari/537.36")

(deftest shell-serves-documents-to-crawlers-at-published-paths-only
  (testing "a crawler at / gets the landing document"
    (let [r (get-as "/" bot)]
      (is (= 200 (:status r)))
      (is (str/includes? (get-in r [:headers "Content-Type"]) "text/html"))
      (is (empty? (executable-scripts (:body r))))
      (is (str/includes? (:body r) (:hero/headline lc/copy)))))

  (testing "a crawler at a legal path gets that document, not the landing one"
    ;; Without an entry in `crawler-response` this path 404s for a crawler while
    ;; rendering for a visitor — a privacy policy no regulator's fetcher can read.
    (let [r (get-as legal/privacy-path bot)]
      (is (= 200 (:status r)))
      (is (str/includes? (get-in r [:headers "Content-Type"]) "text/html"))
      (is (not (str/includes? (:body r) "<script")))
      (is (str/includes? (:body r) (:title (legal/document-at legal/privacy-path))))
      (is (not (str/includes? (:body r) (:hero/headline lc/copy)))
        "each path must carry its own text, or the two are duplicate content")))

  (testing "a crawler anywhere else gets 404 — bounded URLs, no duplicate content"
    (doseq [uri ["/library" "/viewer/topic/5" "/settings" "/anything"]]
      (let [r (get-as uri bot)]
        (is (= 404 (:status r)) uri)
        (is (not (str/includes? (str (:body r)) "<!DOCTYPE")) uri))))

  (testing "a browser always gets the app shell, at every path"
    ;; A legal path included: the Electric renderer serves it to a person, so
    ;; the shell must arrive there like anywhere else.
    (doseq [uri ["/" "/library" "/viewer/topic/5" legal/privacy-path]]
      (let [r (get-as uri browser)]
        (is (= 200 (:status r)) uri)
        (is (str/includes? (:body r) "<!DOCTYPE html>") uri)
        (is (= "no-store" (get-in r [:headers "Cache-Control"])) uri)
        (is (zero? (count (re-seq #"\$:freememo/deployment-head\$" (:body r))))
          (str uri " left a placeholder unsubstituted")))))

  (testing "a request with no user-agent gets the app, not the document"
    (is (str/includes? (:body (get-as "/" nil)) "<!DOCTYPE html>")))

  (testing "the crawler document is cacheable, unlike the no-store shell"
    (is (str/includes? (get-in (get-as "/" bot) [:headers "Cache-Control"]) "max-age"))))

(deftest a-crawler-at-the-availability-path-gets-the-page
  ;; `wrap-api-routes` runs before the shell in `prod`, so the API route owns this
  ;; path. A crawler must get that page, not the 404 the shell gives other paths.
  (let [handler (fn [req] (or (api/api-routes req) (handler req)))]
    (with-redefs [availability/cached-current-month (constantly nil)
                  availability/stored-months (constantly [])
                  availability/stored-outages (constantly [])]
      (let [r (handler {:uri "/availability" :request-method :get
                        :headers {"user-agent" "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"}})]
        (is (= 200 (:status r)))
        (is (str/includes? (get-in r [:headers "Content-Type"]) "text/html"))
        (is (str/includes? (:body r) "99.0%"))))))

(deftest machine-read-paths-never-receive-markup
  (testing "404 text/plain, whoever asks"
    (doseq [uri ["/ads.txt" "/security.txt" "/sitemap.xml" "/.well-known/anything"]
            ua [bot browser nil]]
      (let [r (get-as uri ua)]
        (is (= 404 (:status r)) (str uri " " ua))
        (is (str/includes? (get-in r [:headers "Content-Type"]) "text/plain") uri)
        (is (not (str/includes? (str (:body r)) "<!DOCTYPE")) uri))))

  (testing "ads.txt is in this list, and robots.txt is NOT"
    ;; A route in `freememo.api` owned `/ads.txt` while a seller body was
    ;; configured, and that route went with the ad path on 2026-08-06 — so this
    ;; handler owns the path now, or the shell would answer it with markup. A
    ;; static resource still owns robots.txt and is matched before this handler,
    ;; and listing it here too would give one path two owners.
    (is (contains? shell/text-only-paths "/ads.txt"))
    (is (not (contains? shell/text-only-paths "/robots.txt")))))
