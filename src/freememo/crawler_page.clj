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

   No script, no stylesheet, no font. A crawler runs no JavaScript here, so the
   app bundle would do nothing and the eager vendor tags would cost it four
   round-trips for nothing. Omitting the bundle also means Google's infrastructure
   never opens a WebSocket against this server.

   ## What it duplicates, and why

   The head's title, description and Open Graph values are duplicated from
   `index.prod.html`. Deriving them from one source would need a second
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
   [clojure.string :as str]
   [freememo.deployment-hook :as hook]
   [freememo.landing-copy :as lc]))

;; ── Who gets this document ──

(def crawler-user-agents
  "User-agent substrings that receive the static document, lower-cased.

   `mediapartners-google` fetches the HTML and `googlebot` is the agent Google's
   Web Rendering Service uses for the render itself, so BOTH are required —
   either one alone leaves the render half-blind. The unfurlers read only Open
   Graph tags, which `head-html` emits, so one document covers them too.

   A fixed allowlist, never an inverted browser list: inverting it would hand the
   static document to any browser this list failed to recognize."
  ["mediapartners-google" "googlebot" "adsbot-google"
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

(def ^:private page-title "FreeMemo")

(def ^:private og-title "FreeMemo — read it once, remember it")

(def ^:private description
  "Duplicated from index.prod.html, per the T3 decision. Mirror any change."
  "Turn PDFs, EPUBs, articles and video into flashcards for your own Anki, paced reading, and AI-graded quizzes. Open source, free during early access.")

(def ^:private og-image (str canonical-url "freememo/landing-hero.png"))

(def ^:private og-image-alt
  "FreeMemo showing a PDF page beside its extracted text, with generated basic and cloze flashcards below.")

(defn- meta-tag [kind k v]
  (str "<meta " kind "=\"" k "\" content=\"" (esc v) "\">"))

(defn- head-html
  "The document head.

   Pre: `verification-html` is markup a deployment contributes to prove site
   ownership, or \"\" — `freememo.deployment-hook` guarantees a string.
   Post: carries the title, the description, a canonical link and the full Open
   Graph set. The Open Graph tags are load-bearing beyond search: the unfurlers in
   `crawler-user-agents` read nothing else, so dropping them would regress every
   shared FreeMemo link to a bare title."
  [verification-html]
  (str "<head>"
       "<meta charset=\"utf-8\">"
       "<title>" (esc page-title) "</title>"
       "<link rel=\"canonical\" href=\"" (esc canonical-url) "\">"
       (meta-tag "name" "description" description)
       (meta-tag "name" "viewport" "width=device-width, initial-scale=1")
       (meta-tag "property" "og:type" "website")
       (meta-tag "property" "og:site_name" page-title)
       (meta-tag "property" "og:title" og-title)
       (meta-tag "property" "og:description" description)
       (meta-tag "property" "og:url" canonical-url)
       (meta-tag "property" "og:image" og-image)
       (meta-tag "property" "og:image:width" "3840")
       (meta-tag "property" "og:image:height" "2400")
       (meta-tag "property" "og:image:alt" og-image-alt)
       (meta-tag "name" "twitter:card" "summary_large_image")
       verification-html
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

(defn- answer-html
  "An FAQ answer, which is a string or a vector of `{:t :href}` parts.
   Post: link parts become anchors and plain parts become escaped text. A
   renderer that assumed a string would drop the one answer that carries a link."
  [answer]
  (if (string? answer)
    (esc answer)
    (str/join (for [{:keys [t href]} answer]
                (if href (link href t) (esc t))))))

(defn- faq-html []
  (str (tag "h2" (esc (:faq/title lc/copy)))
       (str/join (for [[q a] lc/faq-items]
                   (str (tag "h3" (esc q)) (tag "p" (answer-html a)))))))

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

   Post: a complete HTML document holding the landing page's words and NO
   `<script>`, no stylesheet link and no font link. The site-verification markup
   comes from `freememo.deployment-hook`, which returns \"\" when no deployment
   contributes any — so a self-hosted build emits none.
   Pure: reads only `freememo.landing-copy` and the hook's cached configuration."
  []
  (str "<!DOCTYPE html>"
       "<html lang=\"en\">"
       (head-html (hook/site-verification-html))
       "<body>"
       (tag "main"
         (str (hero-html) (loops-html) (objection-html)
              (features-html) (faq-html) (pact-html)))
       (tag "footer"
         (str (link "https://github.com/adham-omran/freememo" "Source")
              " "
              (link "mailto:contact@adham-omran.com" "Contact")))
       "</body></html>"))
