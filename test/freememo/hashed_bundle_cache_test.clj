(ns freememo.hashed-bundle-cache-test
  "Pins which responses get the one-year immutable header.

   Shadow-cljs names a prod module `<name>.<32 hex digits>.js` under
   `/freememo/js/` (`:module-hash-names true`). The hash changes with the bytes,
   so that URL can be immutable. The HTML page, `manifest.edn` and any unhashed
   file keep the default header, because their bytes change under a fixed URL."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.shell :as shell]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.resource :refer [wrap-resource]]
            [ring.util.response :as ring-response]))

(def ^:private immutable "public, max-age=31536000, immutable")
(def ^:private hash32 "20AB455ADB44E63E884EC130FB017162")

(defn- handler-for
  "Handler that answers `status` for every path, with no Cache-Control."
  [status]
  (shell/wrap-hashed-bundle-cache-headers
    (fn [_] {:status status :headers {} :body "x"})))

(defn- cache-control [h uri]
  (get-in (h {:uri uri :request-method :get}) [:headers "Cache-Control"]))

(deftest hashed-bundle-is-immutable
  (let [h (handler-for 200)]
    (is (= immutable (cache-control h (str "/freememo/js/main." hash32 ".js"))))
    (is (= immutable (cache-control h (str "/freememo/js/main." (.toLowerCase hash32) ".js"))))))

(deftest other-paths-are-untouched
  (let [h (handler-for 200)]
    (doseq [uri ["/" "/index.html"
                 "/freememo/js/manifest.edn"
                 "/freememo/js/main.js"
                 "/freememo/js/main.abc.js"
                 (str "/freememo/js/main." hash32 ".js.map")
                 (str "/freememo/js/main." hash32 ".css")
                 (str "/other/main." hash32 ".js")
                 (str "/freememo/js/sub/main." hash32 ".js")
                 "/freememo/index.css"]]
      (testing uri
        (is (nil? (cache-control h uri)))))))

(deftest a-miss-is-not-immutable
  (is (nil? (cache-control (handler-for 404) (str "/freememo/js/main." hash32 ".js")))))

(deftest real-resource-chain
  ;; The same order as `prod/-main`: this middleware sits before the default.
  (let [h (-> (fn [_] (ring-response/not-found "no"))
            (wrap-resource "public")
            shell/wrap-hashed-bundle-cache-headers
            wrap-content-type)]
    (is (nil? (cache-control h "/freememo/index.css")))))
