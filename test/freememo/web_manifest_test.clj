(ns freememo.web-manifest-test
  "Pins the web app manifest that makes the site installable.
   There is no service worker. See
   plans/low-end-tablet-performance/issues/11-web-app-manifest.md."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.crawler-page :as crawler-page]
            [freememo.shell :as shell]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as ring-response]))

(def ^:private manifest-path "/freememo/manifest.webmanifest")

(defn- manifest []
  (json/parse-string (slurp (io/resource (str "public" manifest-path))) true))

(deftest manifest-has-the-installable-fields
  (let [m (manifest)]
    (is (= "FreeMemo" (:name m)))
    (is (= "FreeMemo" (:short_name m)))
    (is (= "/" (:start_url m)))
    (is (= "/" (:scope m)))
    (is (= "standalone" (:display m)))
    (is (re-matches #"#[0-9a-f]{6}" (:theme_color m)))
    (is (re-matches #"#[0-9a-f]{6}" (:background_color m)))
    (testing "icons: 192, 512 and a maskable 512, each a real file"
      (let [icons (:icons m)
            by-size (group-by :sizes icons)]
        (is (some #(= "any" (:purpose %)) (get by-size "192x192")))
        (is (some #(= "any" (:purpose %)) (get by-size "512x512")))
        (is (some #(= "maskable" (:purpose %)) (get by-size "512x512")))
        (doseq [{:keys [src type]} icons]
          (is (= "image/png" type))
          (is (some? (io/resource (str "public" src))) src))))))

(deftest app-pages-link-the-manifest
  (doseq [f ["index.prod.html" "index.dev.html"]]
    (let [html (slurp (io/resource (str "public/freememo/" f)))]
      (testing f
        (is (str/includes? html (str "<link rel=\"manifest\" href=\"" manifest-path "\">")))
        (is (re-find #"<meta name=\"theme-color\" content=\"#[0-9a-f]{6}\"" html))))))

(deftest crawler-document-is-untouched
  (is (not (str/includes? (crawler-page/landing-document) "manifest")))
  (is (not (str/includes? (crawler-page/landing-document) "theme-color"))))

(deftest manifest-is-served-with-the-manifest-media-type
  (let [h (-> (fn [_] (ring-response/not-found "no"))
            (wrap-resource "public")
            (wrap-content-type {:mime-types shell/extra-mime-types}))
        resp (h {:uri manifest-path :request-method :get})]
    (is (= 200 (:status resp)))
    (is (str/starts-with? (get-in resp [:headers "Content-Type"]) "application/manifest+json"))))
