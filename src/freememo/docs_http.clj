(ns freememo.docs-http
  "Public, server-rendered documentation. It needs no session or Electric runtime."
  (:require [clojure.string :as str]
            [freememo.docs-content :as copy]
            [freememo.help-content :as help])
  (:import [java.net URLDecoder]))

(defn- esc [value]
  (str/escape (str value) {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;" \' "&#39;"}))

(defn- el [tag body]
  (str "<" tag ">" body "</" tag ">"))

(defn- link [href label]
  (str "<a href=\"" (esc href) "\">" (esc label) "</a>"))

(defn- paragraphs [xs]
  (str/join (map #(el "p" (esc %)) xs)))

(defn- links [xs]
  (when (seq xs)
    (str "<ul class=\"docs-links\">"
         (str/join (map (fn [[href label]] (el "li" (link href label))) xs)) "</ul>")))

(defn- page [path title description body]
  (str "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       "<title>" (esc title) ", FreeMemo Docs</title>"
       "<meta name=\"description\" content=\"" (esc description) "\">"
       "<link rel=\"canonical\" href=\"https://freememo.net" (esc path) "\">"
       "<link rel=\"icon\" href=\"/freememo/favicon-192.png\">"
       "<link rel=\"stylesheet\" href=\"/freememo/docs.css\"></head><body>"
       "<a class=\"docs-skip\" href=\"#content\">Skip to content</a>"
       "<header class=\"docs-header\"><a class=\"docs-brand\" href=\"/docs\">FreeMemo <span>Docs</span></a>"
       "<nav aria-label=\"Documentation\">"
       (str/join (for [[href label] [["/docs/comprehension-loop" "Comprehension loop"]
                                     ["/docs/how-to" "How-to guides"] ["/docs/faq" "FAQ"]]]
                   (str "<a href=\"" href "\"" (when (= path href) " aria-current=\"page\"") ">" label "</a>")))
       "<a class=\"docs-app\" href=\"/comprehension\">Open app ↗</a></nav></header>"
       "<main id=\"content\" class=\"docs-main\"><p class=\"docs-eyebrow\">FreeMemo documentation</p>"
       (el "h1" (esc title)) "<p class=\"docs-lead\">" (esc description) "</p>" body "</main>"
       "<footer class=\"docs-footer\">"
       (link "/docs" "Documentation") ", " (link "/help" "In-app Help") ", " (link "/privacy" "Privacy")
       "</footer></body></html>"))

(defn- step-rail []
  (str "<ol class=\"docs-cycle\">"
       (str/join (for [{:keys [title detail]} copy/cycle-steps]
                   (el "li" (str (el "strong" (esc title)) (el "p" (esc detail)))))) "</ol>"))

(defn- hub []
  (page "/docs" "Read, check, study again"
        "Understand the comprehension loop and find instructions for each study task."
        (str (step-rail)
             "<div class=\"docs-directory\">"
             (str/join (for [[href title description]
                            [["/docs/comprehension-loop" "Understand the comprehension loop" "Follow an assessment from source material to a new check of your understanding."]
                             ["/docs/how-to" "Complete a task" "Search the same instructions used by in-app Help."]
                             ["/docs/faq" "Find a short answer" "Check common questions about exams, handwriting, and study evidence."]]]
                         (str "<section>" (el "h2" (link href title)) (el "p" (esc description)) "</section>")))
             "</div>")))

(defn- article []
  (page "/docs/comprehension-loop" "The comprehension loop"
        "Use an exam to find gaps, choose further study, and check your understanding again."
        (str (step-rail)
             "<div class=\"docs-article-layout\"><nav class=\"docs-contents\" aria-label=\"On this page\"><h2>On this page</h2>"
             (links (map (juxt #(str "#" (:id %)) :title) copy/article)) "</nav><article>"
             (str/join (for [{:keys [id title paragraphs items links]} copy/article]
                         (str "<section id=\"" id "\">" (el "h2" (esc title))
                              (freememo.docs-http/paragraphs paragraphs)
                              (when (seq items) (el "ul" (str/join (map #(el "li" (esc %)) items))))
                              (freememo.docs-http/links links) "</section>")))
             "<p class=\"docs-next\">" (link "/docs/how-to/comprehension-loop" "Follow the step-by-step guide →")
             "</p></article></div>")))

(defn- help-section [{:keys [slug title intro steps]}]
  (str "<section id=\"help-" slug "\" class=\"docs-guide\">"
       (el "h2" (link (str "/docs/how-to/" slug) title))
       (el "p" (esc (help/resolve-chords intro)))
       (el "ol" (str/join (for [{:keys [label detail]} steps]
                            (el "li" (str (el "strong" (esc (help/resolve-chords label)))
                                          (el "p" (esc (help/resolve-chords detail)))))))) "</section>"))

(defn- query-value [query-string]
  (try
    (some (fn [pair]
            (let [[k v] (str/split pair #"=" 2)]
              (when (= "q" (URLDecoder/decode k "UTF-8"))
                (URLDecoder/decode (or v "") "UTF-8"))))
          (str/split (or query-string "") #"&"))
    (catch IllegalArgumentException _ "")))

(defn- guides [slug query]
  (let [section (when slug (help/section-by-slug slug))
        sections (if section [section] (help/filter-sections query))
        path (if section (str "/docs/how-to/" slug) "/docs/how-to")]
    (page path (if section (:title section) "How-to guides")
          "Find instructions for importing material, reading, assessment, review, and planning."
          (str "<form class=\"docs-search\" action=\"/docs/how-to\" method=\"get\">"
               "<label for=\"docs-query\">Search instructions</label>"
               "<div><input id=\"docs-query\" name=\"q\" type=\"search\" value=\"" (esc query) "\">"
               "<button type=\"submit\">Search</button></div></form>"
               (when (and (nil? section) (str/blank? query))
                 (str "<nav class=\"docs-guide-index\" aria-label=\"All guides\">"
                      (links (map (juxt #(str "/docs/how-to/" (:slug %)) :title) help/sections)) "</nav>"))
               (when (empty? sections) (el "p" "No instructions match. Try another search or clear the search field."))
               (str/join (map help-section sections))))))

(defn- faq []
  (page "/docs/faq" "Frequently asked questions"
        "Short answers about diagnostic exams, handwritten answers, and study evidence."
        (str (str/join (for [{:keys [question answer]} copy/faq]
                        (str "<section class=\"docs-faq\">" (el "h2" (esc question)) (el "p" (esc answer)) "</section>")))
             (el "p" (link "/docs/comprehension-loop" "Read the full comprehension-loop guide →")))))

(defn response
  "Return public documentation HTML, or nil for another route.
   Pre: request has a URI and optional query string.
   Post: GET and HEAD docs routes need no account. Unknown docs routes return 404."
  [{:keys [uri request-method query-string]}]
  (when (or (= "/docs" uri) (str/starts-with? (or uri "") "/docs/"))
    (let [slug (second (re-matches #"/docs/how-to/([a-z0-9-]+)" uri))
          body (case uri
                 "/docs" (hub)
                 "/docs/comprehension-loop" (article)
                 "/docs/how-to" (guides nil (or (query-value query-string) ""))
                 "/docs/faq" (faq)
                 (when (help/section-by-slug slug) (guides slug "")))
          allowed? (contains? #{nil :get :head} request-method)]
      {:status (cond (not allowed?) 405 body 200 :else 404)
       :headers (cond-> {"Content-Type" "text/html; charset=utf-8"
                         "Cache-Control" "public, max-age=300"}
                  (not allowed?) (assoc "Allow" "GET, HEAD"))
       :body (if (= :head request-method) ""
                 (if allowed?
                   (or body (page "/docs" "Page not found" "This documentation page does not exist." (el "p" (link "/docs" "Open documentation"))))
                   "Method not allowed"))})))
