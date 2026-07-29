(ns freememo.image-rehost-test
  "Covers `rehost-data-uris!`'s gate and pass-through paths — the branches that
   reject a src before it can reach the media table, so no DB fixture is needed.
   The happy path (a real PNG stored, src rewritten to /api/media/<id>) needs a
   user row and is verified manually; see plans/paste-any-image-rehost.md §4.3."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.image-rehost :as image-rehost]))

(deftest blank-html-is-a-no-op
  (is (= {:html nil :uploaded 0 :remaining 0 :error nil}
        (image-rehost/rehost-data-uris! {:html nil :user-id 1})))
  (is (= {:html "" :uploaded 0 :remaining 0 :error nil}
        (image-rehost/rehost-data-uris! {:html "" :user-id 1}))))

(deftest non-data-srcs-are-left-alone
  (testing "only data: srcs are touched — no absolutization, no HTTP"
    (let [html "<p><img src=\"/api/media/7\"><img src=\"https://example.com/a.png\"></p>"
          r (image-rehost/rehost-data-uris! {:html html :user-id 1})]
      (is (= 0 (:uploaded r)))
      (is (= 0 (:remaining r)))
      (is (nil? (:error r)))
      (is (re-find #"/api/media/7" (:html r)))
      (is (re-find #"https://example\.com/a\.png" (:html r))))))

(deftest non-image-mime-is-rejected
  (testing "data:text/html would be stored then echoed back as a same-origin script"
    (let [html (str "<p><img src=\"data:text/html;base64,"
                 "PHNjcmlwdD5hbGVydCgxKTwvc2NyaXB0Pg==\"></p>")
          r (image-rehost/rehost-data-uris! {:html html :user-id 1})]
      (is (= 0 (:uploaded r)))
      (is (= 1 (:remaining r)))
      (is (re-find #"Unsupported image format: text/html" (:error r)))
      (is (re-find #"data:text/html" (:html r))
        "the src is left in place, never silently dropped"))))

(deftest svg-is-rejected
  (testing "/api/media echoes the stored mime_type with no nosniff"
    (let [html "<p><img src=\"data:image/svg+xml;base64,PHN2Zy8+\"></p>"
          r (image-rehost/rehost-data-uris! {:html html :user-id 1})]
      (is (= 0 (:uploaded r)))
      (is (= 1 (:remaining r)))
      (is (re-find #"Unsupported image format: image/svg\+xml" (:error r))))))

(deftest undecodable-data-uri-is-reported
  (let [r (image-rehost/rehost-data-uris! {:html "<p><img src=\"data:\"></p>"
                                           :user-id 1})]
    (is (= 0 (:uploaded r)))
    (is (= 1 (:remaining r)))
    (is (= "Image data could not be decoded" (:error r)))))

(deftest first-error-is-the-reported-one
  (testing ":remaining counts every failure, :error names only the first"
    (let [html (str "<p><img src=\"data:image/svg+xml;base64,PHN2Zy8+\">"
                 "<img src=\"data:\"></p>")
          r (image-rehost/rehost-data-uris! {:html html :user-id 1})]
      (is (= 2 (:remaining r)))
      (is (re-find #"Unsupported image format" (:error r))))))
