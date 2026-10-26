(ns freememo.card-edit-rehost-test
  "The card EDIT path re-hosts data: images the way the ADD path does, so no
   base64 is persisted whichever way a card is saved. Both paths share
   `cards/rehost-row-data-uris!`, which sits on `image-rehost/rehost-data-uris!`.

   These tests need no PostgreSQL: the two db fns the path reaches are replaced
   with recorders, so they assert what `update-card` would WRITE."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.cards :as cards]
            [freememo.db :as db]))

;; 1x1 transparent PNG.
(def ^:private png-data-uri
  (str "data:image/png;base64,"
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="))

(defn- run-update-card
  "Call cards/update-card with db stubbed. Returns {:result r :written fields-or-nil
   :media [stored-bytes-count ...]}."
  [updated-fields]
  (let [written (atom nil)
        media (atom [])]
    (with-redefs [db/upsert-media! (fn [{:keys [bytes]}]
                                     (swap! media conj (alength ^bytes bytes))
                                     4242)
                  db/update-flashcard-versioned! (fn [_user _card fields]
                                                   (reset! written fields)
                                                   {:updated? true :versioned? true})]
      (let [r (cards/update-card 1 99 updated-fields)]
        {:result r :written @written :media @media}))))

(deftest edit-with-a-data-image-ends-with-an-api-media-src
  (testing "answer"
    (let [{:keys [result written media]}
          (run-update-card {:question "<p>Q</p>"
                            :answer (str "<p>A <img src=\"" png-data-uri "\"></p>")})]
      (is (:success result))
      (is (= 1 (count media)) "the bytes were stored once")
      (is (str/includes? (:answer written) "src=\"/api/media/4242\""))
      (is (not (str/includes? (:answer written) "data:")))
      (is (= "<p>Q</p>" (:question written)) "a field without an image is untouched")))
  (testing "cloze"
    (let [{:keys [result written]}
          (run-update-card {:cloze (str "<p>{{c1::x}} <img src=\"" png-data-uri "\"></p>")})]
      (is (:success result))
      (is (str/includes? (:cloze written) "/api/media/4242"))
      (is (not (str/includes? (:cloze written) "data:"))))))

(deftest edit-without-a-data-image-writes-the-html-byte-for-byte
  (testing "no Jsoup round trip, so an edit that touches no image is not
            re-serialized (that would version and re-digest an unchanged card)"
    (let [html "<p>a<br>b &nbsp; <img src=\"/api/media/7\"></p><hr>"
          {:keys [written media]} (run-update-card {:answer html})]
      (is (= html (:answer written)))
      (is (empty? media)))))

(deftest edit-with-an-unstorable-image-is-rejected-and-writes-nothing
  (let [{:keys [result written]}
        (run-update-card {:answer "<p><img src=\"data:image/svg+xml;base64,PHN2Zy8+\"></p>"})]
    (is (false? (:success result)))
    (is (re-find #"Image could not be stored: Unsupported image format" (:error result)))
    (is (nil? written) "the card is not written with the image dropped or inlined")))
