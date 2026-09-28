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
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.crawler-page :as crawler-page]
            [freememo.landing-copy :as lc]
            [freememo.legal-copy :as legal]
            [freememo.shell :as shell]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.resource :refer [wrap-resource]]))

(def ^:private document (crawler-page/landing-document))

(defn- executable-scripts
  "HTML → the opening tag of every `<script>` a browser would run.
   A JSON-LD block is data, so it is not counted."
  [html]
  (remove #(str/includes? % "type=\"application/ld+json\"")
          (re-seq #"<script\b[^>]*>" html)))

(defn- json-ld
  "HTML → the parsed body of its one JSON-LD block, or nil."
  [html]
  (some-> (re-find #"(?s)<script type=\"application/ld\+json\">(.*?)</script>" html)
          second
          json/parse-string))

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
    (is (empty? (executable-scripts document)))
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
;; Head drift. The title, description, Open Graph values and JSON-LD are
;; duplicated from index.prod.html by design, because deriving them would need a second
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
        (str attr " differs between index.prod.html and the crawler document")))

    (testing "the title, which Google copies into the search result's title link"
      (let [title-of #(second (re-find #"<title>([^<]*)</title>" %))]
        (is (some? (title-of document)))
        (is (= (title-of shell-html) (title-of document)))))

    (testing "the JSON-LD, compared as data so layout does not matter"
      (is (some? (json-ld shell-html)) "index.prod.html carries no JSON-LD")
      (is (= (json-ld shell-html) (json-ld document) crawler-page/structured-data)))

    (testing "both link the PNG icon, the format Google Search accepts"
      (doseq [html [shell-html document]]
        (is (str/includes? html "rel=\"icon\" type=\"image/png\" sizes=\"192x192\" href=\"/freememo/favicon-192.png\""))))))

(deftest crawler-document-offers-google-no-svg-icon
  ;; Google Search takes no SVG favicon, and which link it picks from several is
  ;; undocumented. So the document Googlebot reads links raster icons only.
  (let [icon-links (re-seq #"<link rel=\"(?:icon|apple-touch-icon)\"[^>]*>" document)]
    (is (seq icon-links))
    (doseq [l icon-links]
      (is (not (str/includes? l ".svg")) l))))

(deftest json-ld-cannot-close-its-element
  ;; `esc` does not reach the JSON-LD block, so a value holding `</script>` would
  ;; end the element early and spill the rest of the JSON into the page.
  (let [html (#'crawler-page/json-ld-html {"description" "a</script><p>b"})]
    (is (= 1 (count (re-seq #"</script>" html))))
    (is (= {"description" "a</script><p>b"} (json-ld html)))))

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
;; Legal documents. Same two-renderer split as the landing page, and the same
;; silent failure: a part shape one renderer implements and the other does not
;; is a paragraph a crawler never reads, with nothing in any log.
;; ---------------------------------------------------------------------------

(def ^:private legal-parts
  "Every body part in every legal document, flattened."
  (for [doc (vals legal/documents)
        section (:sections doc)
        part (:body section)]
    part))

(deftest every-legal-part-classifies
  ;; A part `part-kind` cannot classify still renders — as
  ;; `unrenderable-part-marker`, so it is visible rather than missing. This test
  ;; is what keeps that marker out of a deploy.
  (doseq [part legal-parts]
    (is (contains? legal/part-kinds (legal/part-kind part))
      (str "no part kind matches " (pr-str part)))))

(def ^:private part-of-kind
  "One minimal part per kind, for exercising a renderer over the whole closed
   set rather than over the kinds today's copy happens to use."
  {:text {:p "PARTKINDPROBE"}
   ;; The plain fragment carries the whole marker: a run puts markup BETWEEN
   ;; fragments, so a marker split across two of them would never appear intact.
   :run  {:p [{:t "PARTKINDPROBE "} {:t "LINKED" :href "https://example.invalid/"}]}
   :list {:ul ["PARTKINDPROBE"]}})

(deftest legal-renderers-implement-every-part-kind
  (testing "the probe set covers the closed set"
    ;; Without this, adding a kind to `part-kinds` and forgetting a probe would
    ;; leave the renderer assertion below vacuously true for that kind.
    (is (= legal/part-kinds (set (keys part-of-kind)))))

  (testing "the crawler renderer emits real markup for each kind"
    ;; Executed, not grepped. An earlier version of this file searched the
    ;; renderer's SOURCE for `:t` and `:href`, and passed because those keywords
    ;; appear in a docstring — so deleting the fragment branch outright would
    ;; not have failed anything.
    (let [part-html #'crawler-page/part-html]
      (doseq [[kind part] part-of-kind]
        (let [html (part-html part)]
          (is (str/includes? html "PARTKINDPROBE") (str kind " dropped its text"))
          (is (not (str/includes? html legal/unrenderable-part-marker))
            (str kind " is not handled by freememo.crawler-page"))))

      (testing "the run kind keeps its link, which is the branch a grep missed"
        (is (str/includes? (part-html (:run part-of-kind))
              "<a href=\"https://example.invalid/\">LINKED</a>")))

      (testing "an unclassifiable part is visible, not silent"
        (is (str/includes? (part-html {:heading "not a part"})
              legal/unrenderable-part-marker)))))

  (testing "the Electric renderer loads and dispatches on the same classifier"
    ;; `freememo.legal-page` cannot be executed here — rendering it needs a
    ;; running Electric peer. Two things ARE checkable from the JVM: that the
    ;; namespace compiles at all, and that it names every kind it must handle.
    ;; Playwright row 15.6 is what compares its OUTPUT against this renderer's.
    (require 'freememo.legal-page)
    (let [src (slurp (io/file "src/freememo/legal_page.cljc"))]
      (doseq [kind legal/part-kinds]
        ;; `boolean` keeps a failure from dumping the whole namespace source
        ;; into the report.
        (is (boolean (str/includes? src (str "(= " kind " kind)")))
          (str "freememo.legal-page/Part has no branch for " kind))))))

(deftest no-document-holds-an-unrenderable-part
  (doseq [[path _] legal/documents]
    (is (not (str/includes? (crawler-page/legal-document path)
               legal/unrenderable-part-marker))
      (str path " renders a part neither renderer understands"))))

(deftest every-legal-part-reaches-the-crawler-document
  (let [document (crawler-page/legal-document legal/privacy-path)]
    (is (some? document) "the privacy path must produce a document")

    (testing "no asset a crawler cannot use, as with the landing document"
      (is (not (str/includes? document "<script")))
      (is (not (str/includes? document "rel=\"stylesheet\""))))

    (testing "the canonical link names this path, not the site root"
      ;; Sharing the root's canonical would tell a crawler the policy and the
      ;; landing page are one URL, and one of them would be dropped.
      (is (str/includes? document
            (str "<link rel=\"canonical\" href=\"" crawler-page/canonical-url "privacy\">"))))

    (testing "every sentence of the copy is in the document"
      (let [text (visible-text document)]
        (doseq [part legal-parts]
          (let [p (:p part)]
            (cond
              (string? p) (is (str/includes? text p) p)
              (some? p) (doseq [{:keys [t]} p] (is (str/includes? text t) t))
              :else (doseq [item (:ul part)] (is (str/includes? text item) item)))))))))

(deftest the-privacy-policy-answers-what-the-gdpr-asks
  ;; Article 13 names what a notice must carry. These are word checks, not legal
  ;; advice: they fail when a rewrite drops a required subject, which is the one
  ;; failure a reader of the page would not notice.
  (let [text (str/lower-case (visible-text (crawler-page/legal-document legal/privacy-path)))]
    (doseq [subject ["controller" "article 6(1)(b)" "openrouter" "wayl"
                     "standard contractual clauses" "one month"
                     "data protection authority" "cookie"]]
      (is (str/includes? text (str/lower-case subject))
        (str "the policy no longer mentions " subject)))

    (testing "the contact address a request must go to is on the page"
      (is (str/includes? text (str/lower-case legal/contact-email))))))

;; ---------------------------------------------------------------------------
;; Who gets the document. A fixed allowlist, never an inverted browser list: an
;; inverted list hands the static page to any browser it fails to recognize.
;; ---------------------------------------------------------------------------

(deftest crawler-user-agent-matches-crawlers-and-no-browser
  (testing "the three Google agents that read this page — the render, the
            Search Console and Rich Results tools, and the ad landing-page
            check — all match"
    (is (crawler-page/crawler-user-agent?
          "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)"))
    (is (crawler-page/crawler-user-agent?
          "Mozilla/5.0 (Linux; Android 6.0.1; Nexus 5X Build/MMB29P) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/151.0.0.0 Mobile Safari/537.36 (compatible; Google-InspectionTool/1.0)"))
    (is (crawler-page/crawler-user-agent?
          "AdsBot-Google (+http://www.google.com/adsbot.html)")))

  (testing "the ad-network crawler no longer matches"
    ;; `mediapartners-google` left the allowlist with the ad path on 2026-08-06.
    ;; It gets the app shell now, like any unrecognized client.
    (is (not (crawler-page/crawler-user-agent? "Mediapartners-Google"))))

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

(deftest robots-txt-is-a-real-file-and-names-no-sitemap
  (let [body (slurp (io/resource "public/robots.txt"))]
    (is (not (str/includes? body "<!DOCTYPE")))
    (is (re-find #"(?m)^User-agent: \*" body))
    (is (not (re-find #"(?i)sitemap:" body))
      "/sitemap.xml is a deliberate 404, so advertising a sitemap wastes a fetch")))

(deftest icons-are-static-files-ahead-of-the-shell
  ;; The shell is the catch-all, so an icon path with no file behind it gets the
  ;; app's HTML with a 200. That was /favicon.ico for any agent the allowlist does
  ;; not name, "Google Favicon" included. The chain below is prod.cljc's order:
  ;; the resource handler answers before the shell is reached.
  (let [chain (-> (fn [_] {:status 599 :body "fell through to next-handler"})
                  (shell/wrap-prod-index-page
                    {:resources-path "public" :manifest-path "public/freememo/js/manifest.edn"})
                  (wrap-resource "public")
                  wrap-content-type)]
    (doseq [ua [bot "Googlebot-Image/1.0"
                "Mozilla/5.0 (Windows NT 6.1; rv:6.0) Gecko/20110814 Firefox/6.0 Google Favicon"]
            [uri content-type] [["/favicon.ico" "image/x-icon"]
                                ["/freememo/favicon-192.png" "image/png"]
                                ["/freememo/apple-touch-icon.png" "image/png"]]]
      (let [r (chain {:uri uri :request-method :get :headers {"user-agent" ua}})]
        (is (= 200 (:status r)) (str uri " for " ua))
        (is (= content-type (get-in r [:headers "Content-Type"])) (str uri " for " ua))))))
