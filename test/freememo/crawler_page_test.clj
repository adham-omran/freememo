(ns freememo.crawler-page-test
  "Pins the static document crawlers receive, and the branch that serves it.

   Why these assertions and not others: every failure here is silent. A crawler
   that stops receiving the document reads an empty body and reports nothing. A
   document that gains a script asks a machine with no JavaScript to fetch it. A
   copy key the crawler renderer never reads is text a visitor sees and a crawler
   never does, which is the drift the flat-copy-map design accepted and this file
   is the only guard against. A user-agent test that matches a browser would serve
   the static page to a person.

   This namespace is public. It requires no database: `freememo.shell` reaches
   only `freememo.crawler-page` and `freememo.deployment-hook`, and the route test
   that does need one lives with the excluded implementation's suite."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.crawler-page :as crawler-page]
            [freememo.landing-copy :as lc]
            [freememo.shell :as shell]))

(def ^:private document (crawler-page/landing-document))

(defn- visible-text
  "HTML → the words a crawler reads, with tags and runs of space removed."
  [html]
  (-> html (str/replace #"<[^>]+>" " ") (str/replace #"\s+" " ") str/trim))

;; ---------------------------------------------------------------------------
;; The document. It exists because Googlebot's renderer has no WebSocket support
;; and this app ships an empty body, so before it every crawler read 0 words.
;; ---------------------------------------------------------------------------

(deftest document-carries-the-landing-words-and-no-assets
  (testing "a real amount of text, not a stub"
    (let [words (count (str/split (visible-text document) #" "))]
      (is (> words 500) (str "only " words " words"))))

  (testing "no asset a crawler cannot use"
    ;; A crawler runs no JavaScript here, so the app bundle would do nothing and
    ;; the eager vendor tags would cost four round-trips for nothing. Omitting the
    ;; bundle also keeps Google's infrastructure from opening a WebSocket to us.
    (is (not (str/includes? document "<script")))
    (is (not (str/includes? document "rel=\"stylesheet\"")))
    (is (not (str/includes? document "open-sans")))
    (is (not (str/includes? document "/freememo/js/"))))

  (testing "one document, well formed enough to parse"
    (is (str/starts-with? document "<!DOCTYPE html>"))
    (is (= 1 (count (re-seq #"<html" document))))
    (is (= 1 (count (re-seq #"<head>" document))))
    (is (= 1 (count (re-seq #"<body>" document)))))

  (testing "the Open Graph set is complete — the unfurlers in the agent list read
            nothing else, so a missing tag regresses every shared link"
    (doseq [prop ["og:type" "og:site_name" "og:title" "og:description" "og:url"
                  "og:image" "og:image:alt"]]
      (is (str/includes? document (str "property=\"" prop "\"")) prop))
    (is (str/includes? document "name=\"twitter:card\""))
    (is (str/includes? document "rel=\"canonical\"")))

  (testing "every section of the landing page contributes"
    (let [text (visible-text document)]
      (doseq [[label s] [["hero headline" (:hero/headline lc/copy)]
                         ["loops title" (:loops/title lc/copy)]
                         ["objection title" (:objection/title lc/copy)]
                         ["features title" (:features/title lc/copy)]
                         ["faq title" (:faq/title lc/copy)]
                         ["pact title" (:pact/title lc/copy)]]]
        (is (str/includes? text s) label))
      (testing "and the data-driven blocks, not only the headings"
        (is (str/includes? text (first (first lc/features))))
        (is (str/includes? text (first (first lc/faq-items))))
        (is (str/includes? text (second (first lc/loops))))))))

(deftest faq-answers-that-carry-links-survive
  ;; One answer is a vector of parts rather than a string. A renderer that
  ;; assumed a string would drop it, and that is the only answer explaining
  ;; self-hosting.
  (is (str/includes? document "href=\"https://github.com/adham-omran/freememo\""))
  (is (str/includes? document ">codebase</a>"))
  (testing "and the surrounding fragments are still there"
    (is (str/includes? (visible-text document) "Self-host, yes"))))

(deftest document-escapes-every-value
  (testing "no stray angle bracket from a copy string"
    ;; Every value passes through `esc`. Counting tags is the cheap proxy: an
    ;; unescaped `<` in copy would open a tag that never closes.
    (is (= (count (re-seq #"<" document)) (count (re-seq #">" document)))))

  (testing "the quoted phrase in the FAQ question does not break the markup"
    ;; faq-items holds `What "pre-production" actually means`, whose quotes would
    ;; end an attribute if a value ever landed in one unescaped.
    (is (str/includes? document "&quot;pre-production&quot;"))))

;; ---------------------------------------------------------------------------
;; Head drift. The title, description and Open Graph values are duplicated from
;; index.prod.html by design, because deriving them would need a second
;; placeholder in that shell. Duplication is only safe while something checks it.
;; ---------------------------------------------------------------------------

(deftest crawler-head-matches-the-shell
  (let [shell-html (slurp (io/resource "public/freememo/index.prod.html"))
        content-of (fn [html attr]
                     (second (re-find (re-pattern (str attr "[^>]*content=\"([^\"]*)\"")) html)))]
    (doseq [attr ["name=\"description\"" "property=\"og:title\""
                  "property=\"og:description\"" "property=\"og:url\""
                  "property=\"og:image\"" "property=\"og:image:alt\""]]
      (is (= (content-of shell-html attr) (content-of document attr))
        (str attr " differs between index.prod.html and the crawler document")))))

;; ---------------------------------------------------------------------------
;; Copy parity. The words live in one namespace; the SECTION ORDER lives in both
;; renderers. A key only one renderer reads is text one audience never sees.
;; ---------------------------------------------------------------------------

(deftest copy-keys-are-read-by-both-renderers
  (let [electric (slurp (io/file "src/freememo/landing_page.cljc"))
        crawler  (slurp (io/file "src/freememo/crawler_page.clj"))]
    (doseq [k (keys lc/copy)]
      (let [needle (str k)]
        (is (str/includes? electric needle)
          (str needle " is never read by freememo.landing-page"))
        (is (str/includes? crawler needle)
          (str needle " is never read by freememo.crawler-page — a crawler cannot see it"))))))

;; ---------------------------------------------------------------------------
;; Who gets the document. A fixed allowlist, never an inverted browser list: an
;; inverted list hands the static page to any browser it fails to recognize.
;; ---------------------------------------------------------------------------

(deftest crawler-user-agent-matches-crawlers-and-no-browser
  (testing "the two agents AdSense uses — the fetch and the render — both match"
    (is (crawler-page/crawler-user-agent? "Mediapartners-Google"))
    (is (crawler-page/crawler-user-agent?
          "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")))

  (testing "unfurlers match, because they read the Open Graph tags"
    (doseq [ua ["Slackbot-LinkExpanding 1.0" "Twitterbot/1.0"
                "facebookexternalhit/1.1" "LinkedInBot/1.0" "Discordbot/2.0"]]
      (is (crawler-page/crawler-user-agent? ua) ua)))

  (testing "no real browser matches"
    (doseq [ua ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Safari/537.36"
                "Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 Version/18.0 Mobile/15E148 Safari/604.1"
                "Mozilla/5.0 (X11; Linux x86_64; rv:130.0) Gecko/20100101 Firefox/130.0"
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 Edg/131.0.0.0"]]
      (is (not (crawler-page/crawler-user-agent? ua)) ua)))

  (testing "absent or blank yields false, so a client that sends no agent gets the app"
    (is (not (crawler-page/crawler-user-agent? nil)))
    (is (not (crawler-page/crawler-user-agent? "")))
    (is (not (crawler-page/crawler-user-agent? "   ")))))

;; ---------------------------------------------------------------------------
;; The branch. Serving the document at every path would put identical text on
;; unbounded URLs, and serving markup at a machine-read path reads as a
;; malformed file.
;; ---------------------------------------------------------------------------

(def ^:private handler
  (shell/wrap-prod-index-page
    (fn [_] {:status 599 :body "fell through to next-handler"})
    {:resources-path "public" :manifest-path "public/freememo/js/manifest.edn"}))

(defn- get-as [uri ua]
  (handler {:uri uri :request-method :get :headers (cond-> {} ua (assoc "user-agent" ua))}))

(def ^:private bot "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
(def ^:private browser "Mozilla/5.0 (Macintosh) AppleWebKit/537.36 Chrome/151.0.0.0 Safari/537.36")

(deftest shell-serves-the-document-to-crawlers-at-the-root-only
  (testing "a crawler at / gets the document"
    (let [r (get-as "/" bot)]
      (is (= 200 (:status r)))
      (is (str/includes? (get-in r [:headers "Content-Type"]) "text/html"))
      (is (not (str/includes? (:body r) "<script")))
      (is (str/includes? (:body r) (:hero/headline lc/copy)))))

  (testing "a crawler anywhere else gets 404 — one crawlable URL, no duplicate content"
    (doseq [uri ["/library" "/viewer/topic/5" "/settings" "/anything"]]
      (let [r (get-as uri bot)]
        (is (= 404 (:status r)) uri)
        (is (not (str/includes? (str (:body r)) "<!DOCTYPE")) uri))))

  (testing "a browser always gets the app shell, at every path"
    (doseq [uri ["/" "/library" "/viewer/topic/5"]]
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

(deftest machine-read-paths-never-receive-markup
  (testing "404 text/plain, whoever asks"
    (doseq [uri ["/security.txt" "/sitemap.xml" "/.well-known/anything"]
            ua [bot browser nil]]
      (let [r (get-as uri ua)]
        (is (= 404 (:status r)) (str uri " " ua))
        (is (str/includes? (get-in r [:headers "Content-Type"]) "text/plain") uri)
        (is (not (str/includes? (str (:body r)) "<!DOCTYPE")) uri))))

  (testing "ads.txt and robots.txt are NOT in this list"
    ;; A route owns the first and a static resource owns the second, and both are
    ;; matched before this handler. Listing them here would give one path two
    ;; owners, which is the defect this work removes.
    (is (not (contains? shell/text-only-paths "/ads.txt")))
    (is (not (contains? shell/text-only-paths "/robots.txt")))))

(deftest robots-txt-is-a-real-file-and-names-no-sitemap
  (let [body (slurp (io/resource "public/robots.txt"))]
    (is (not (str/includes? body "<!DOCTYPE")))
    (is (re-find #"(?m)^User-agent: \*" body))
    (is (not (re-find #"(?i)sitemap:" body))
      "/sitemap.xml is a deliberate 404, so advertising a sitemap wastes a fetch")))
