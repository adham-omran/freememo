(ns freememo.crawler-page
  "The static HTML document a crawler receives instead of the app shell.

   ## Why this exists

   No crawler can render this app. Google's documentation is explicit:
   \"Googlebot uses HTTP requests to retrieve content from your server and does
   not support other types of connections, such as WebSockets or WebRTC.\"
   Electric delivers every word over a WebSocket, and `index.prod.html` ships an
   empty `<body>`, so a crawler saw 0 words of content at every URL. Measured on
   the deployed shell before this namespace existed: 0 characters in the body, and
   46 crawlable words in total, all of them meta description.

   That is Google's documented dynamic-rendering workaround, not cloaking: the
   crawler reads the same words a visitor reads, from
   `freememo.landing-copy`, which is why the copy lives in data.

   ## What the document deliberately omits

   No executable script, no stylesheet, no font. A crawler runs no JavaScript
   here, so the app bundle would do nothing and the eager vendor tags would cost
   it four round-trips for nothing. Omitting the bundle also means Google's
   infrastructure never opens a WebSocket against this server.

   The one `<script>` element is `application/ld+json`, which is data a crawler
   parses and no browser runs.

   ## What it duplicates, and why

   The head's title, description, Open Graph values and JSON-LD are duplicated
   from `index.prod.html`. Deriving them from one source would need a second
   placeholder in that shell, which is the mechanism this design chose T3 to
   avoid. The shell already duplicates its own description into `og:description`,
   so the file tolerates the pattern. A change to either must be mirrored, and
   `crawler-head-matches-the-shell` is the test that catches a drift.

   Three copy blocks are not in `freememo.landing-copy` and this namespace carries
   its own equivalent prose for them: the source-paragraph demo, the four
   label-and-sentence pairs, and the contact line. Each composes one sentence from
   fragments around inline markup.

   See plans/crawler-rendering-and-well-known-files.md."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [freememo.landing-copy :as lc]
   [freememo.legal-copy :as legal]))

;; ── Who gets this document ──

(def crawler-user-agents
  "User-agent substrings that receive the static document, lower-cased.

   `googlebot` is the agent Google's Web Rendering Service uses, and
   `adsbot-google` scores the landing page of a Google Ads campaign — both read a
   page this app cannot render for them.

   `google-inspectiontool` is the agent of the Rich Results Test and of URL
   Inspection in Search Console. Its string does not contain `googlebot`, so
   without this member both tools read the app shell and cannot show what
   Googlebot reads. The unfurlers read only Open Graph tags,
   which `head-html` emits, so one document covers them too.

   `mediapartners-google` was a member until 2026-08-06. It is the ad-network
   crawler, so it stopped arriving when the ad path was removed, and a member that
   can never match is a claim about traffic that no longer exists.

   `adsbot-google` is NOT the same thing: it belongs to the advertiser side, and
   it arrives only while this deployment buys ad clicks. Removing the Google Ads
   conversion tag on 2026-08-16 did NOT settle that — a tag reports conversions,
   and campaigns can run without one. **Drop this member when the campaigns stop,
   on the same reasoning that removed `mediapartners-google`.** See
   plans/ad-path-removal.md and plans/legal-pages-and-ads-tag-removal.md.

   A fixed allowlist, never an inverted browser list: inverting it would hand the
   static document to any browser this list failed to recognize."
  ["googlebot" "google-inspectiontool" "adsbot-google"
   "bingbot" "duckduckbot" "yandexbot" "applebot"
   "slackbot" "twitterbot" "facebookexternalhit" "linkedinbot" "discordbot"
   "telegrambot" "whatsapp"])

(defn crawler-user-agent?
  "Whether `user-agent` names a crawler that cannot render this app.
   Pre: `user-agent` is a header value or nil.
   Post: true only when a substring of `crawler-user-agents` matches, without
   case. nil and blank yield false, so a client that sends no agent gets the app
   rather than the document."
  [user-agent]
  (boolean
    (when-some [ua (some-> user-agent str/lower-case not-empty)]
      (some #(str/includes? ua %) crawler-user-agents))))

;; ── Markup helpers ──

(def ^:private escapes
  {\< "&lt;" \> "&gt;" \& "&amp;" \" "&quot;"})

(defn- esc
  "Text → HTML-escaped text.
   Pre: `s` is a string. Post: contains no unescaped `<`, `>`, `&` or `\"`, so no
   copy value can close a tag or an attribute. Every value below passes through
   here, including the ones from `freememo.landing-copy`."
  [s]
  (str/escape (str s) escapes))

(defn- tag
  "Wrap `body` in `<name>` with an optional class. `body` is already escaped."
  ([name body] (str "<" name ">" body "</" name ">"))
  ([name class body]
   (str "<" name " class=\"" (esc class) "\">" body "</" name ">")))

(defn- para [s] (tag "p" (esc s)))
(defn- items [strs] (tag "ul" (str/join (map #(tag "li" (esc %)) strs))))

(defn- link
  "An anchor. External hrefs are escaped like any other value."
  [href text]
  (str "<a href=\"" (esc href) "\">" (esc text) "</a>"))

;; ── Head ──

(def canonical-url
  "The one URL this document represents.
   `wrap-prod-index-page` serves it for `/` only, so every crawl of the site
   resolves to this address and no two URLs carry the same text."
  "https://freememo.net/")

(def ^:private site-name
  "The site's name, alone. `og:site_name`, the JSON-LD `name` and the legal
   documents' titles read it."
  "FreeMemo")

(def ^:private home-title
  "The landing page's `<title>`, which Google copies into the search result's
   title link. It says what FreeMemo does, because the bare name gave Google
   nothing to tell this site apart from other products called FreeMemo.
   Duplicated from index.prod.html. Mirror any change."
  "FreeMemo — Incremental reading, AI quizzes, and Anki cards")

(def ^:private og-title "FreeMemo — read it once, remember it")

(def ^:private description
  "Duplicated from index.prod.html, per the T3 decision. Mirror any change."
  "Turn PDFs, EPUBs, articles and video into flashcards for your own Anki, paced reading, and AI-graded quizzes. Open source, free during early access.")

(def ^:private og-image (str canonical-url "freememo/landing-hero.png"))

(def ^:private favicon-png
  "The icon Google Search can use. Google takes no SVG favicon, so this document
   links only the PNG. It is also the Organization logo, which Google requires
   to be at least 112x112px."
  "/freememo/favicon-192.png")

(def ^:private apple-touch-icon "/freememo/apple-touch-icon.png")

(def structured-data
  "The JSON-LD graph in the landing document's head, as data.

   `WebSite` is what Google reads for the site name. `Organization` carries the
   `sameAs` link that ties this site to its repository, which Google uses to tell
   this FreeMemo apart from others. Every value is also on the page or in a meta
   tag that `crawler-head-matches-the-shell` checks.
   Duplicated from index.prod.html. That test compares the two as parsed data."
  (let [website-id (str canonical-url "#website")
        org-id (str canonical-url "#organization")]
    {"@context" "https://schema.org"
     "@graph" [{"@type" "WebSite"
                "@id" website-id
                "name" site-name
                "url" canonical-url
                "publisher" {"@id" org-id}}
               {"@type" "Organization"
                "@id" org-id
                "name" site-name
                "url" canonical-url
                "logo" (str (str/replace canonical-url #"/$" "") favicon-png)
                "description" description
                "sameAs" ["https://github.com/adham-omran/freememo"]}]}))

(defn- json-ld-html
  "A JSON-LD script element holding `data`.

   Pre: `data` is a map cheshire can encode.
   Post: the element's body holds no `</`, so no value can close the element.
   `esc` does not apply here, because entity escapes inside a script element are
   not decoded and would corrupt the JSON. `<\\/` is the JSON escape for `</`
   and parses back to the same string."
  [data]
  (str "<script type=\"application/ld+json\">"
       (str/replace (json/generate-string data) "</" "<\\/")
       "</script>"))

(def ^:private og-image-alt
  "FreeMemo showing a PDF page beside its extracted text, with generated basic and cloze flashcards below.")

(defn- meta-tag [kind k v]
  (str "<meta " kind "=\"" k "\" content=\"" (esc v) "\">"))

(defn- head-html
  "The document head.

   Post: carries the title, the description, a canonical link, the PNG icon
   links, the full Open Graph set and the `structured-data` JSON-LD. Google reads
   the favicon from this head only, because Googlebot never receives the shell.
   The Open Graph tags are load-bearing beyond search: the unfurlers in
   `crawler-user-agents` read nothing else, so dropping them would regress every
   shared FreeMemo link to a bare title.
   It carried one deployment contribution — a site-verification meta — until the
   ad path was removed on 2026-08-06. A later ownership proof would come back
   through a `freememo.deployment-hook` seam, not from this namespace."
  []
  (str "<head>"
       "<meta charset=\"utf-8\">"
       "<title>" (esc home-title) "</title>"
       "<link rel=\"canonical\" href=\"" (esc canonical-url) "\">"
       "<link rel=\"icon\" type=\"image/png\" sizes=\"192x192\" href=\"" favicon-png "\">"
       "<link rel=\"apple-touch-icon\" sizes=\"180x180\" href=\"" apple-touch-icon "\">"
       (meta-tag "name" "description" description)
       (meta-tag "name" "viewport" "width=device-width, initial-scale=1")
       (meta-tag "property" "og:type" "website")
       (meta-tag "property" "og:site_name" site-name)
       (meta-tag "property" "og:title" og-title)
       (meta-tag "property" "og:description" description)
       (meta-tag "property" "og:url" canonical-url)
       (meta-tag "property" "og:image" og-image)
       (meta-tag "property" "og:image:width" "3840")
       (meta-tag "property" "og:image:height" "2400")
       (meta-tag "property" "og:image:alt" og-image-alt)
       (meta-tag "name" "twitter:card" "summary_large_image")
       (json-ld-html structured-data)
       "</head>"))

;; ── Body sections ──
;;
;; Section order mirrors `freememo.landing-page/LandingPage`. The order lives in
;; both renderers, so a section added there is invisible here until it is added
;; below too — the cost the flat-copy-map decision accepted.

(defn- hero-html []
  (str (tag "h1" (esc (:hero/headline lc/copy)))
       (para (:hero/sub lc/copy))))

(defn- loops-html []
  (str (tag "h2" (esc (:loops/title lc/copy)))
       (para (:loops/lead lc/copy))
       (str/join
         (for [[n nm sched steps] lc/loops]
           (str (tag "h3" (esc (str n ". " nm)))
                (para sched)
                (items steps))))))

(defn- objection-html
  "The rebuttal section.

   The demo and the four practice bullets are the prose this namespace owns: on
   the Electric page each is assembled from fragments around inline markup, so
   they are not in `freememo.landing-copy`. Keep them equivalent in meaning to
   that page, or the two renderers say different things."
  []
  (str (tag "h2" (esc (:objection/title lc/copy)))
       (para (:objection/lead lc/copy))
       (tag "h3" "From a source paragraph to three review items")
       (para "Mitochondria are membrane-bound organelles found in most eukaryotic cells. They generate most of the cell's supply of ATP through oxidative phosphorylation, a process that takes place across the inner mitochondrial membrane.")
       (items
         ["A basic card for Anki asks what process mitochondria use to generate ATP, and answers: oxidative phosphorylation, across the inner mitochondrial membrane."
          "A cloze card for Anki deletes oxidative phosphorylation and inner mitochondrial membrane from the same sentence, one concept per deletion."
          "A quiz question stays in FreeMemo and asks where ATP synthesis takes place and by what process. The model grades your prose answer instead of asking you to rate yourself."])
       (items
         ["Page-level context. Generation looks at one page at a time, not the whole book, so cards stay tied to a specific paragraph."
          "Human review before sync. Cards land in an editable table. Edit, reject, or rewrite before anything reaches Anki."
          "Cloze precision. The model emits valid Anki cloze syntax, with each deletion a single concept."
          "Source traceability. Every card links back to its source page, so a wrong card is two clicks from the paragraph it came from."])))

(defn- features-html []
  (str (tag "h2" (esc (:features/title lc/copy)))
       (str/join (for [[title desc] lc/features]
                   (str (tag "h3" (esc title)) (para desc))))))

(defn- fragments-html
  "A text run, which is a string or a vector of `{:t :href}` fragments.
   Post: link fragments become anchors and plain fragments become escaped text.
   A renderer that assumed a string would drop every run that carries a link.
   Both the FAQ answers and the legal body paragraphs are this shape, which is
   why the name says what it renders rather than where it is used."
  [run]
  (if (string? run)
    (esc run)
    (str/join (for [{:keys [t href]} run]
                (if href (link href t) (esc t))))))

(defn- faq-html []
  (str (tag "h2" (esc (:faq/title lc/copy)))
       (str/join (for [[q a] lc/faq-items]
                   (str (tag "h3" (esc q)) (tag "p" (fragments-html a)))))))

(defn- pact-html []
  (str (tag "h2" (esc (:pact/title lc/copy)))
       (tag "h3" (esc (:pact/benefits-heading lc/copy)))
       (items (:pact/benefits lc/copy))
       (tag "h3" (esc (:pact/limits-heading lc/copy)))
       (items (:pact/limits lc/copy))
       ;; The Electric page composes this line around a mailto link, so the
       ;; sentence is this namespace's own.
       (tag "p" (str (esc "Questions, bug reports, or feature ideas: ")
                     (link "mailto:contact@adham-omran.com" "contact@adham-omran.com")))))

;; ── Entry point ──

(defn landing-document
  "The whole static document for a crawler, as a string.

   Post: a complete HTML document holding the landing page's words, NO
   executable script, no stylesheet link and no font link. Its one `<script>` is
   the `application/ld+json` block.
   Pure: reads only `freememo.landing-copy`."
  []
  (str "<!DOCTYPE html>"
       "<html lang=\"en\">"
       (head-html)
       "<body>"
       (tag "main"
         (str (hero-html) (loops-html) (objection-html)
              (features-html) (faq-html) (pact-html)))
       (tag "footer"
         (str (link "https://github.com/adham-omran/freememo" "Source")
              " "
              (link "mailto:contact@adham-omran.com" "Contact")
              " "
              (link legal/privacy-path "Privacy")))
       "</body></html>"))

;; ── Legal documents ──
;;
;; Section order does NOT live here. A legal document is a flat list of whole
;; sections, so this renderer walks `freememo.legal-copy/documents` rather than
;; naming a section at a time. That is why these pages carry no renderer-owned
;; prose and need no equivalent-meaning rule.

(defn- legal-head-html
  "The head of one legal document.

   Post: a title, a description, and a canonical link for THIS path — not the
   site root. Sharing `canonical-url` across both documents would tell a crawler
   the privacy policy and the landing page are the same URL.
   No Open Graph set: the unfurlers in `crawler-user-agents` are here for shared
   marketing links, and a legal document is not one.
   Pre: `path` is a key of `legal/documents`. `canonical-url` carries a trailing
   slash and a path carries a leading one, so the join drops one rather than
   slicing `path` — a slice would depend on a leading slash this fn cannot check,
   and would build a wrong canonical in silence if a key ever lacked one."
  [path doc]
  (str "<head>"
       "<meta charset=\"utf-8\">"
       "<title>" (esc (str (:title doc) " — " site-name)) "</title>"
       "<link rel=\"canonical\" href=\""
       (esc (str (str/replace canonical-url #"/$" "") path)) "\">"
       (meta-tag "name" "description"
         (str "The " (str/lower-case (:title doc)) " for " site-name "."))
       (meta-tag "name" "viewport" "width=device-width, initial-scale=1")
       "</head>"))

(defn- part-html
  "One body part, dispatched on `freememo.legal-copy/part-kind`.

   Pre: `part` is a map from a section body.
   Post: markup for every kind in `legal/part-kinds`, and
   `legal/unrenderable-part-marker` for a part that classifies to nil.
   The shape is NOT re-derived from the part's keys here: `part-kind` is the one
   classifier, and `freememo.legal-page/Part` dispatches on the same value, so
   the two renderers cannot disagree about what a part is."
  [part]
  (case (legal/part-kind part)
    :text (para (:p part))
    :run  (tag "p" (fragments-html (:p part)))
    :list (items (:ul part))
    (para legal/unrenderable-part-marker)))

(defn- legal-section-html [section]
  (str (tag "h2" (esc (:heading section)))
       (str/join (map part-html (:body section)))))

(defn legal-document
  "The static document for the legal page at `path`, or nil.

   Pre: `path` is a request path or nil.
   Post: nil when `path` names no legal document, so a caller branches on this
   rather than repeating the path set. Otherwise a complete HTML document with
   NO `<script>`, no stylesheet link and no font link, for the reason
   `landing-document` states.
   Pure: reads only `freememo.legal-copy`."
  [path]
  (when-some [doc (legal/document-at path)]
    (str "<!DOCTYPE html>"
         "<html lang=\"en\">"
         (legal-head-html path doc)
         "<body>"
         (tag "main"
           (str (tag "h1" (esc (:title doc)))
                (para (str "Last updated " (:updated doc)))
                (str/join (map legal-section-html (:sections doc)))))
         (tag "footer" (link "/" (str "Back to " site-name)))
         "</body></html>")))
