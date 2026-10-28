(ns freememo.comprehension-http-test
  "HTTP checks for answer privacy, multipart uploads, and printable question content."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.answer-sheet :as answer-sheet]
            [freememo.api :as api]
            [freememo.comprehension :as cycle]
            [freememo.comprehension-ocr :as scan])
  (:import [java.io File RandomAccessFile]
           [org.apache.pdfbox Loader]
           [org.apache.pdfbox.text PDFTextStripper]
           [org.jsoup Jsoup]))

(defn- request [method uri user-id]
  (cond-> {:request-method method :uri uri}
    user-id (assoc :session {:user-id user-id})))

(defn- decoded [response]
  (json/parse-string (:body response) true))

(deftest public-documentation-dispatches-before-the-app-shell
  (doseq [uri ["/docs" "/docs/comprehension-loop" "/docs/how-to" "/docs/how-to/comprehension-loop" "/docs/faq"]
          user-agent ["Mozilla/5.0" "Googlebot"]]
    (let [response (api/api-routes (assoc (request :get uri nil) :headers {"user-agent" user-agent}))
          doc (Jsoup/parse (:body response))]
      (is (= 200 (:status response)) (str uri " for " user-agent))
      (is (seq (.text (.selectFirst doc "main"))))
      (is (not (contains? response :session)))
      (is (empty? (.select doc "script")))))
  (let [response (api/api-routes (request :head "/docs/comprehension-loop" nil))]
    (is (= 200 (:status response)))
    (is (= "" (:body response))))
  (is (= 404 (:status (api/api-routes (request :get "/docs/not-a-page" nil))))))

(deftest unauthorized-answer-requests-never-reach-storage
  (let [calls (atom [])]
    (with-redefs [scan/stage-scan! (fn [& arguments] (swap! calls conj arguments))
                  scan/scan-original (fn [& arguments] (swap! calls conj arguments))
                  cycle/cycle-detail (fn [& arguments] (swap! calls conj arguments))]
      (doseq [[method uri] [[:post "/api/comprehension/scan"]
                            [:get "/api/comprehension/original/91"]
                            [:get "/api/comprehension/print/42"]]]
        (let [response (api/api-routes (request method uri nil))]
          (is (= 401 (:status response)) uri)
          (is (false? (:ok (decoded response))))))
      (is (empty? @calls)))))

(deftest multipart-upload-passes-owned-original-to-storage
  (let [file (File/createTempFile "freememo-answer-http-" ".png")
        bytes (byte-array [1 9 3 7])
        calls (atom [])
        multipart {:cycle_id "42" :file {:tempfile file :filename "answers.png" :content-type "image/png"}}]
    (try
      (with-open [out (io/output-stream file)] (.write out bytes))
      (with-redefs [scan/stage-scan!
                    (fn [uid cid original content-type filename]
                      (swap! calls conj {:uid uid :cid cid :bytes (vec original)
                                         :content-type content-type :filename filename})
                      (if (and (= 7 uid) (= 42 cid))
                        {:ok true :scan-id 91}
                        {:ok false :error "This study cycle is not available."}))]
        (let [params (into {} (map (fn [[k v]] [(name k) v])) multipart)
              response (api/api-routes (assoc (request :post "/api/comprehension/scan" 7) :params params))]
          (is (= 200 (:status response)))
          (is (= {:ok true :scan-id 91} (decoded response)))
          (is (= [{:uid 7 :cid 42 :bytes (vec bytes) :content-type "image/png" :filename "answers.png"}] @calls))
          (is (= "no-store" (get-in response [:headers "Cache-Control"])))
          (doseq [[uid cid] [[8 "42"] [7 "9999"]]]
            (let [rejected (api/api-routes (assoc (request :post "/api/comprehension/scan" uid)
                                                :params (assoc params "cycle_id" cid)))]
              (is (= 400 (:status rejected)))
              (is (false? (:ok (decoded rejected))))))))
      (finally (.delete file)))))

(deftest invalid-or-oversized-multipart-upload-stops-before-storage
  (let [file (File/createTempFile "freememo-answer-limit-" ".png")
        called (atom false)]
    (try
      (with-redefs [scan/stage-scan! (fn [& _] (reset! called true))]
        (doseq [params [{} {"cycle_id" "wrong" "file" {:tempfile file}} {"cycle_id" "42"}]]
          (is (= 400 (:status (api/api-routes (assoc (request :post "/api/comprehension/scan" 7) :params params))))))
        (with-open [sparse (RandomAccessFile. file "rw")]
          (.setLength sparse (inc (* 20 1024 1024))))
        (is (= 413 (:status (api/api-routes
                             (assoc (request :post "/api/comprehension/scan" 7)
                               :params {"cycle_id" "42" "file" {:tempfile file :content-type "image/png" :filename "large.png"}})))))
        (is (false? @called)))
      (finally (.delete file)))))

(deftest original-scans-require-ownership-and-private-caching
  (let [original (byte-array [1 9 3 7])]
    (with-redefs [scan/scan-original
                  (fn [uid sid]
                    (when (and (= 7 uid) (= 91 sid))
                      {:bytes original :content_type "image/png" :filename "answers.png"}))]
      (let [response (api/api-routes (request :get "/api/comprehension/original/91" 7))]
        (is (= 200 (:status response)))
        (is (= "image/png" (get-in response [:headers "Content-Type"])))
        (is (= "private, no-store" (get-in response [:headers "Cache-Control"])))
        (is (= "nosniff" (get-in response [:headers "X-Content-Type-Options"])))
        (with-open [body (:body response)]
          (is (= (vec original) (vec (.readAllBytes body))))))
      (doseq [[uid sid] [[8 91] [7 9999]]]
        (let [response (api/api-routes (request :get (str "/api/comprehension/original/" sid) uid))]
          (is (= 404 (:status response)))
          (is (= {:ok false :error "Scan not found."} (decoded response))))))))

(def ^:private printable-cycle
  {:id 42 :title "Source <script>secret</script> & notes"
   :form {:questions [{:id "q1" :format "multiple-choice" :points 3
                      :prompt "Which value is less than 4?"
                      :options [{:id "option-one" :text "5 > 4"}
                                {:id "option-two" :text "3 < 4"}]
                      :correct_option_id "option-two" :grading_guide "PRIVATE-GRADING-GUIDE"}
                     {:id "q2" :format "fill-in-the-blank" :points 2 :prompt "The force changes ____."
                      :accepted_answers ["PRIVATE-ACCEPTED-ANSWER"]}
                     {:id "q3" :format "long-essay" :points 10 :prompt "Explain the connection."
                      :rubric {:reasoning "PRIVATE-RUBRIC"}}]}})

(def ^:private private-texts
  ["PRIVATE-GRADING-GUIDE" "PRIVATE-ACCEPTED-ANSWER" "PRIVATE-RUBRIC" "correct_option_id"])

(defn- typst-ready?
  "Typst on PATH and the vendored Arabic font on the classpath. A clone without
   either skips the PDF check rather than failing it."
  []
  (and (io/resource "public/freememo/vendor/noto-naskh-arabic@2.021/NotoNaskhArabic-wght.ttf")
       (try (zero? (.waitFor (.start (ProcessBuilder. ["typst" "--version"]))))
            (catch java.io.IOException _ false))))

(deftest printable-sheet-is-a-pdf-of-prompts-without-answer-keys
  (if-not (typst-ready?)
    (println "SKIP printable-sheet-is-a-pdf: typst or the vendored font is missing")
    (with-redefs [cycle/cycle-detail (fn [uid cid] (when (and (= 7 uid) (= 42 cid)) printable-cycle))]
      (let [response (api/api-routes (request :get "/api/comprehension/print/42" 7))
            pdf (with-open [in (:body response)] (.readAllBytes in))
            text (with-open [doc (Loader/loadPDF ^bytes pdf)] (.getText (PDFTextStripper.) doc))]
        (is (= 200 (:status response)))
        (is (= "application/pdf" (get-in response [:headers "Content-Type"])))
        (is (= "private, no-store" (get-in response [:headers "Cache-Control"])))
        (is (= "inline; filename=\"answer-sheet-42.pdf\"" (get-in response [:headers "Content-Disposition"])))
        (doseq [[i question] (map-indexed vector (get-in printable-cycle [:form :questions]))]
          (is (str/includes? text (str "Question " (inc i))))
          (is (str/includes? text (:prompt question))))
        (testing "a title that looks like markup is printed, not run"
          (is (str/includes? text "<script>secret</script>")))
        (is (str/includes? text "option-two."))
        (doseq [private-text private-texts]
          (is (not (str/includes? text private-text))))))))

(deftest printable-sheet-falls-back-to-html-without-answer-keys
  (with-redefs [cycle/cycle-detail (fn [uid cid] (when (and (= 7 uid) (= 42 cid)) printable-cycle))
                answer-sheet/render (fn [_] {:ok false :reason :missing-binary :error "no typst"})]
    (let [response (api/api-routes (request :get "/api/comprehension/print/42" 7))
          doc (Jsoup/parse (:body response))
          articles (.select doc "article")
          text (.text doc)]
      (is (= 200 (:status response)))
      (is (= "private, no-store" (get-in response [:headers "Cache-Control"])))
      (is (= 3 (count articles)))
      (is (empty? (.select doc "script")))
      (is (= (:title printable-cycle) (.text (.selectFirst doc "h1"))))
      (is (= ["option-one. 5 > 4" "option-two. 3 < 4"]
             (mapv #(.text %) (.select doc "article.multiple-choice li"))))
      (doseq [[i question] (map-indexed vector (get-in printable-cycle [:form :questions]))]
        (is (str/includes? (.text (.get articles i)) (:prompt question)))
        (is (str/includes? (.text (.selectFirst (.get articles i) "h2")) (str "Question " (inc i))))
        (is (some? (.selectFirst (.get articles i) ".space"))))
      (doseq [private-text private-texts]
        (is (not (str/includes? text private-text))))
      (is (str/includes? text "Ordinary numbered paper also works.")))
    (doseq [[uid cid] [[8 42] [7 9999]]]
      (let [response (api/api-routes (request :get (str "/api/comprehension/print/" cid) uid))]
        (is (= 404 (:status response)))
        (is (= {:ok false :error "Cycle not found."} (decoded response)))))))
