(ns freememo.comprehension-http
  "Authenticated answer uploads, original scans, and printable answer sheets."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.answer-sheet :as answer-sheet]
            [freememo.comprehension :as cycle]
            [freememo.comprehension-ocr :as scan]))

(defn- reply [status body]
  {:status status :headers {"Content-Type" "application/json" "Cache-Control" "no-store"}
   :body (json/generate-string body)})

(defn upload-handler
  "Pre: authenticated multipart request with a cycle ID and one scan.
   Post: saves an owned original scan without grading it.
   Invariant: invalid uploads never alter source material."
  [request]
  (if-let [uid (get-in request [:session :user-id])]
    (let [cycle-id (some-> (get-in request [:params "cycle_id"]) str parse-long)
          {:keys [tempfile filename content-type]} (get-in request [:params "file"])]
      (cond
        (or (nil? cycle-id) (nil? tempfile))
        (reply 400 {:ok false :error "Choose a cycle and an answer scan."})
        (> (.length ^java.io.File tempfile) (* 20 1024 1024))
        (reply 413 {:ok false :error "The scan must be at most 20 MB."})
        :else
        (try
          (let [result (scan/stage-scan! uid cycle-id
                         (java.nio.file.Files/readAllBytes (.toPath ^java.io.File tempfile))
                         content-type filename)]
            (reply (if (:ok result) 200 400) result))
          (catch Exception _ (reply 400 {:ok false :error "The scan could not be saved. Check the file and try again."})))))
    (reply 401 {:ok false :error "Sign in to upload answers."})))

(defn original-handler
  "Post: only the scan owner receives original bytes. Unowned IDs return 404."
  [request]
  (if-let [uid (get-in request [:session :user-id])]
    (if-let [original (when-let [id (some-> (:uri request) (str/split #"/") last parse-long)]
                       (scan/scan-original uid id))]
      {:status 200
       :headers {"Content-Type" (:content_type original)
                 "Content-Disposition" "inline"
                 "Cache-Control" "private, no-store"
                 "X-Content-Type-Options" "nosniff"}
       :body (io/input-stream (:bytes original))}
      (reply 404 {:ok false :error "Scan not found."}))
    (reply 401 {:ok false :error "Sign in to view answers."})))

(defn- escape-html [value]
  (str/escape (str value) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&#39;"}))

(defn- html-sheet
  "The browser-printed sheet, served when the Typst PDF cannot be made."
  [detail]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8" "Cache-Control" "private, no-store"}
   :body (str "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">"
           "<title>Answer sheet | FreeMemo</title><style>body{font:16px system-ui;max-width:48rem;margin:2rem auto;padding:0 1rem}"
           "article{break-inside:avoid;margin:2rem 0}.space{min-height:8rem;border-bottom:1px solid #999}"
           ".long-essay .space{min-height:18rem}.multiple-choice .space,.fill-in-the-blank .space{min-height:3rem}"
           "@media print{.instructions{display:none}body{margin:0}}</style><body><h1>"
           (escape-html (:title detail)) "</h1><p class=\"instructions\">Use your browser’s Print command. Ordinary numbered paper also works.</p>"
           "<p>Cycle " (:id detail) ", Number each answer. Keep the question number on continuation pages.</p>"
           (str/join
             (map-indexed
               (fn [i question]
                 (str "<article class=\"" (escape-html (:format question)) "\"><h2>Question " (inc i)
                   ", " (escape-html (:points question)) " points</h2><p>" (escape-html (:prompt question)) "</p>"
                   (when (seq (:options question))
                     (str "<ul>" (str/join (map #(str "<li>" (escape-html (if (map? %) (str (:id %) ". " (:text %)) %)) "</li>") (:options question))) "</ul>"))
                   "<div class=\"space\"></div></article>"))
               (get-in detail [:form :questions]))) "</body></html>")})

(defn print-handler
  "Post: an owned cycle prints numbered prompts and answer space without answer
   keys, as a Typst PDF, or as an HTML page when Typst fails."
  [request]
  (if-let [uid (get-in request [:session :user-id])]
    (if-let [detail (when-let [id (some-> (:uri request) (str/split #"/") last parse-long)]
                     (cycle/cycle-detail uid id))]
      (let [{:keys [ok pdf]} (answer-sheet/render detail)]
        (if ok
          {:status 200
           :headers {"Content-Type" "application/pdf"
                     "Content-Disposition" (str "inline; filename=\"answer-sheet-" (:id detail) ".pdf\"")
                     "Cache-Control" "private, no-store"
                     "X-Content-Type-Options" "nosniff"}
           :body (io/input-stream pdf)}
          (html-sheet detail)))
      (reply 404 {:ok false :error "Cycle not found."}))
    (reply 401 {:ok false :error "Sign in to print an answer sheet."})))
