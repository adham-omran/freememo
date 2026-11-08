(ns freememo.navigation
  "Navigation state constructors and schema.
   All navigation mutations should use these constructors to ensure
   consistent data shapes. No ad-hoc map construction."
  (:require [clojure.string :as str]))

(defn path->tab
  "The tab that a URL path names: its first segment as a keyword, or :home for
   the root. The same rule as Main's `active-tab`, which reads router5's route.
   Pre : path is a URL path such as \"/library/documents\", or nil.
   Post: a keyword."
  [path]
  (let [seg (first (remove empty? (str/split (str path) #"/")))]
    (if seg (keyword seg) :home)))

(defn current-tab
  "The tab the browser shows now, read from the URL when it is called. Main's
   navigate! uses it, so that navigate! does not close over `active-tab` and
   stays one value. Client only; nil on the JVM."
  []
  #?(:cljs (path->tab (.-pathname js/location))
     :clj nil))

(def kept-tabs
  "The tabs whose page Main keeps mounted after the first visit, hidden while
   another tab shows. On the target tablet a Learn unmount cost 8.5 s and its
   mount 5 s on every switch
   (plans/low-end-tablet-performance/issues/14-electric-mount-cost.md)."
  #{:learn})

(defn keep-tab!
  "Remember `tab` in `!kept` when it is a kept tab, and return the kept set.
   Pre : !kept is an atom holding a set of tab keywords.
   Post: the set never shrinks, and holds only members of `kept-tabs`."
  [!kept tab]
  (if (contains? kept-tabs tab)
    (swap! !kept conj tab)
    @!kept))

;; Viewer nav types (stored in !viewer-nav atom in Main):
;; {:type :topic           :topic-id _ :origin _}
;; {:type :learn-session}
;; {:type :subset-review   :root-id _ :root-name _}
;; Search nav type (tab :search):
;; {:type :search-query    :query "text"}
;; Help nav type (tab :help):
;; {:type :help            :slug "section-slug"}

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

(defn nav-walkthrough
  "/viewer/topic/<repo-id>/walkthrough/<id>: a commit-range walkthrough, a
   sub-route of its repository node (plans/commit-range-walkthrough.md
   §6.6.7.1)."
  [repo-id walkthrough-id]
  {:type :walkthrough :topic-id repo-id :walkthrough-id walkthrough-id})

(defn nav-learn-session []
  {:type :learn-session})

(defn nav-subset-review [root-id root-name]
  {:type :subset-review :root-id root-id :root-name root-name})

(defn nav-search-query [query]
  {:type :search-query :query query})

(defn nav-help
  "/help/<slug>: one Help section. Pre: `slug` is a section's `:slug`
   (freememo.help-content); an unknown one shows the whole page."
  [slug]
  {:type :help :slug slug})

(defn nav-comprehension [cycle-id]
  {:type :comprehension-cycle :cycle-id cycle-id})

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
