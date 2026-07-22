(ns freememo.navigation
  "Navigation state constructors and schema.
   All navigation mutations should use these constructors to ensure
   consistent data shapes. No ad-hoc map construction.")

;; Viewer nav types (stored in !viewer-nav atom in Main):
;; {:type :topic           :topic-id _ :origin _}
;; {:type :learn-session}
;; {:type :subset-review   :root-id _ :root-name _}
;; Search nav type (tab :search):
;; {:type :search-query    :query "text"}

;; Cross-topic page handoff: a PDF-page extract's "Go to page" button sets this
;; before navigating to the PDF root; the destination TopicPage consumes it once
;; (→ same-doc target-page channel) and clears it. defonce on BOTH peers (no
;; reader conditional) — it is e/watch'd in an e/defn body, so the var must
;; resolve on CLJS to avoid a frame-signal mismatch. Shape: {:root id :page n}.
(defonce !pending-page-jump (atom nil))

(defn nav-topic
  ([topic-id origin] (nav-topic topic-id origin nil))
  ([topic-id origin page]
   ;; page (optional): a PDF page number for the /viewer/topic/<id>/<page> URL
   ;; anchor. nil for every non-PDF nav — nav->route omits the segment then.
   {:type :topic :topic-id topic-id :origin origin :page page}))

(defn nav-learn-session []
  {:type :learn-session})

(defn nav-subset-review [root-id root-name]
  {:type :subset-review :root-id root-id :root-name root-name})

(defn nav-search-query [query]
  {:type :search-query :query query})

(defn set-url-page!
  "Reflect the current PDF page in the address bar as /viewer/topic/<root>/<page>.
   replaceState — no history entry (page scrolls don't spam back/forward) and no
   popstate, so the router does not re-resolve or remount the viewer. The URL is
   the solid anchor: on reload the segment is parsed back into initial-page.
   Pre:  root-id and page are non-nil; caller is on the PDF root doc (kind pdf).
   Post: window pathname == /viewer/topic/<root>/<page>; no-op if already equal."
  [root-id page]
  #?(:cljs
     (when (and root-id page)
       (let [path (str "/viewer/topic/" root-id "/" page)
             cur  (.. js/window -location -pathname)]
         (when (not= path cur)
           (.replaceState js/window.history nil "" path))))
     :clj nil))
