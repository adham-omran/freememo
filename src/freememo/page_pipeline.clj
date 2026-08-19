(ns freememo.page-pipeline
  "Turn a web page's HTML into a stored document.

   One pipeline, two sources. `freememo.wikipedia` hands it bytes it fetched
   itself. The Chrome capture extension hands it a DOM the browser serialized,
   which a server fetch cannot reach — a page behind a login, a single-page
   application, an intranet host.

   The step order is load-bearing at two points.

   The title is read FIRST. `<title>` lives in `<head>`, which the main-region
   step discards, and it is absent from `clean-html`'s allow-list. Any caller
   that derives a title after the transform gets nothing.

   `doc-html` runs BEFORE `clean-html`, for the reason
   `sphinx-import/strip-headerlinks!` records: the sanitizer strips the class
   that identifies a construct, and afterwards nothing can recognize it.

   Image re-hosting is NOT here. It needs a user id and writes rows, so it
   belongs to the caller that owns both. `web-import` calls
   `image-rehost/rehost-html-images!` on the fetch path. The capture path calls
   `rehost-data-uris!`, because the extension inlines what the server cannot
   reach.

   See `plans/chrome-capture-extension.md`."
  (:require [clojure.string :as str]
            [freememo.biblio-web :as bw]
            [freememo.doc-html :as dh]
            [freememo.html-cleaner :as cleaner])
  (:import [org.jsoup Jsoup]))

;; ---------------------------------------------------------------------------
;; Publication date
;; ---------------------------------------------------------------------------

(defn- parse-iso-date-parts
  "Parse common publication-date string shapes into a CSL :date-parts vec:
   [[Y]] [[Y M]] or [[Y M D]]. Returns nil on unparseable input.
   Handles: 2023-01-19 · 2023/01/19 · 2023-01 · 2023 · 2023-01-19T18:59:59Z."
  [s]
  (when (and s (string? s) (not (str/blank? s)))
    (let [stripped (-> s str/trim (str/replace #"T.*$" "") (str/replace "/" "-"))
          parts (str/split stripped #"-")
          ints (->> parts
                 (keep #(try (Integer/parseInt %) (catch Exception _ nil)))
                 (take 3)
                 vec)]
      (when (and (seq ints) (<= 1900 (first ints) 2100))
        [ints]))))

(defn- extract-issued-date-parts
  "Mine a CSL :issued date-parts vec from a parsed HTML document by checking
   common publication-date meta tags. Priority order: citation_date (Google
   Scholar / ArXiv abstract pages) → DC.date → DC.date.issued → article:published_time
   (Open Graph) → <time pubdate datetime=…>. Returns nil if none present or
   none parse."
  [^org.jsoup.nodes.Document doc]
  (let [meta-attr (fn [selector]
                    (some-> (.selectFirst doc selector) (.attr "content") not-empty))
        time-attr (some-> (.selectFirst doc "time[pubdate][datetime]")
                    (.attr "datetime") not-empty)
        candidate (or (meta-attr "meta[name=citation_date]")
                      (meta-attr "meta[name=citation_publication_date]")
                      (meta-attr "meta[name=citation_online_date]")
                      (meta-attr "meta[name=DC.date.issued]")
                      (meta-attr "meta[name=DC.date]")
                      (meta-attr "meta[property=article:published_time]")
                      (meta-attr "meta[name=pubdate]")
                      time-attr)]
    (parse-iso-date-parts candidate)))

;; ---------------------------------------------------------------------------
;; Steps
;; ---------------------------------------------------------------------------

(def ^:private chrome-selectors
  "Page furniture that is never the document. Removed whole, contents included.

   `clean-html` would drop most of these tags anyway, but it keeps their TEXT —
   measured on one Wikipedia article, the sanitizer took 275278 bytes to 96344
   while leaving the character count unchanged to within one. Navigation menus
   and footer links become body text unless they are removed here."
  ["nav" "footer" "header" "aside" ".sidebar" ".nav"
   ".footer" ".header" ".advertisement" ".ad" "#comments"
   "script" "style" "noscript" "iframe"])

(def ^:private main-region-selectors
  "Where the document is, in descending confidence. The body is the fallback."
  ["article" "main" "[role=main]" ".content" "#content"])

(defn derive-title
  "The document's title, read before any step that can discard it.

   Pre : `html-str` is an HTML string or nil. `fallback` is what to use when
         the page names itself nothing — normally the URL.
   Post: a non-blank string.
   Inv : reads `<head>`, so a caller MUST call this on the RAW html, never on
         pipeline output. `page->document` already does.

   `<title>` is in no `clean-html` allow-list and `<head>` does not survive the
   main-region step, so a title derived afterwards is always the fallback."
  [html-str fallback]
  (let [doc (when-not (str/blank? html-str) (Jsoup/parse html-str))
        from-tag (some-> doc (.select "title") (.first) (.text) not-empty)
        from-heading (some-> doc (.select "h1") (.first) (.text) not-empty)]
    (or from-tag from-heading fallback)))

(defn page->document
  "Extract title, content and bibliography from a page's HTML.

   Pre : `html-str` is a non-blank HTML string. `url` is where it came from.
         `clean-html` has NOT run on it.
   Post: {:title :html :url :source-type \"web\" :biblio {:local :identifiers}},
         plus :issued-date-parts when the page names a publication date.
         `:html` is sanitized and safe to store.
   Inv : `:biblio`'s `:local` always carries a `:URL`, so a caller can attach a
         source without knowing where the page came from.

   `opts`:
     `:browser-dom?` — run the rewrites a serialized browser DOM needs. The
       fetch path leaves this off, which keeps its output byte-identical to
       what it produced before this namespace existed."
  ([html-str url] (page->document html-str url {}))
  ([html-str url {:keys [browser-dom?]}]
   (let [biblio (try (bw/harvest-html html-str)
                     (catch Exception _ {:local {} :identifiers {}}))
         title (derive-title html-str url)
         doc (Jsoup/parse html-str)
         issued-date-parts (extract-issued-date-parts doc)
         _ (doseq [sel chrome-selectors]
             (.remove (.select doc sel)))
         main-content (or (->> main-region-selectors
                            (keep (fn [sel] (some-> (.select doc sel) (.first) (.html))))
                            first)
                          (.html (.body doc)))
         quill-safe (if browser-dom?
                      (dh/browser-dom->quill-html main-content)
                      main-content)
         sanitized (cleaner/clean-html quill-safe)
         ;; `drop-bare-spans` runs AFTER the sanitizer by its own contract —
         ;; before it, a span still carries the class that decides whether it
         ;; survives. A browser DOM is full of styling spans whose classes the
         ;; allow-list drops, and Quill discards what is left on load.
         html (if browser-dom? (dh/drop-bare-spans sanitized) sanitized)
         biblio+url (update biblio :local
                      (fn [m] (cond-> m (not (:URL m)) (assoc :URL url))))]
     (cond-> {:title title :html html :url url :source-type "web" :biblio biblio+url}
       issued-date-parts (assoc :issued-date-parts issued-date-parts)))))
