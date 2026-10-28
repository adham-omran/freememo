(ns freememo.docs-http-test
  "Public documentation contracts. This namespace needs no database."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.docs-http :as docs]
            [freememo.help-content :as help])
  (:import [org.jsoup Jsoup]))

(defn- document [uri]
  (Jsoup/parse (:body (docs/response {:uri uri :request-method :get}))))

(deftest public-documents-work-without-a-session-or-javascript
  (doseq [uri ["/docs" "/docs/comprehension-loop" "/docs/how-to" "/docs/faq"]]
    (let [response (docs/response {:uri uri :request-method :get})
          doc (Jsoup/parse (:body response))]
      (is (= 200 (:status response)) uri)
      (is (= "text/html; charset=utf-8" (get-in response [:headers "Content-Type"])))
      (is (seq (.text (.selectFirst doc "main h1"))))
      (is (empty? (.select doc "script")))
      (is (= (str "https://freememo.net" uri) (.attr (.selectFirst doc "link[rel=canonical]") "href"))))))

(deftest guide-pages-share-canonical-help-instructions
  (doseq [{:keys [slug title intro steps]} help/sections]
    (let [response (docs/response {:uri (str "/docs/how-to/" slug) :request-method :get})
          text (.text (Jsoup/parse (:body response)))]
      (is (= 200 (:status response)) slug)
      (is (str/includes? text title))
      (is (str/includes? text (help/resolve-chords intro)))
      (doseq [{:keys [detail]} steps]
        (is (str/includes? text (help/resolve-chords detail)) slug)))))

(deftest public-search-uses-the-existing-help-matching-rule
  (let [response (docs/response {:uri "/docs/how-to" :request-method :get :query-string "q=OCR"})
        doc (Jsoup/parse (:body response))
        shown (map #(.attr % "id") (.select doc "section.docs-guide"))]
    (is (= (map #(str "help-" (:slug %)) (help/filter-sections "OCR")) shown)))
  (testing "a query cannot add HTML or an executable element"
    (let [response (docs/response {:uri "/docs/how-to" :request-method :get
                                  :query-string "q=%22%3E%3Cscript%3Ebad%3C%2Fscript%3E"})
          doc (Jsoup/parse (:body response))]
      (is (empty? (.select doc "script")))
      (is (= "\"><script>bad</script>" (.val (.selectFirst doc "input[name=q]"))))))
  (is (= 200 (:status (docs/response {:uri "/docs/how-to" :query-string "q=%XX"})))))

(deftest documentation-links-and-article-anchors-resolve
  (doseq [uri ["/docs" "/docs/comprehension-loop" "/docs/how-to" "/docs/faq"]
          :let [doc (document uri)]
          anchor (.select doc "a[href]")
          :let [href (.attr anchor "href")]]
    (cond
      (str/starts-with? href "/docs")
      (is (= 200 (:status (docs/response {:uri href :request-method :get})))
          (str uri " links to " href))
      (str/starts-with? href "#")
      (is (some? (.getElementById doc (subs href 1))) (str uri " links to " href)))))

(deftest docs-routing-does-not-capture-app-or-machine-paths
  (doseq [uri ["/" "/help" "/help/comprehension-loop" "/comprehension" "/robots.txt" "/docs-nope"]]
    (is (nil? (docs/response {:uri uri :request-method :get})) uri))
  (doseq [uri ["/docs/missing" "/docs/how-to/missing" "/docs/comprehension-loop/extra"]]
    (is (= 404 (:status (docs/response {:uri uri :request-method :get}))) uri))
  (let [response (docs/response {:uri "/docs" :request-method :post})]
    (is (= 405 (:status response)))
    (is (= "GET, HEAD" (get-in response [:headers "Allow"]))))
  (is (= "" (:body (docs/response {:uri "/docs" :request-method :head})))))
