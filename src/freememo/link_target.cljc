(ns freememo.link-target
  "What a click on an anchor in the editor can offer.

   Pure string work, no DOM. A `.cljc` that requires nothing but
   `clojure.string`, so `test/` can reach it without a database and without
   Electric — the project has a Clojure test harness only, and the affordance
   table is the part of this feature a test can decide without a browser.

   URL resolution is implemented here rather than through `java.net.URI` or
   `js/URL`, because those two disagree on the inputs real documents carry:
   `URI` throws on an unencoded space or brace, `URL` accepts it. One portable
   implementation means one behaviour on both peers and one set of tests.

   `resolve-href` is the only entry point. Its `:kind` decides the affordance:

     :external        Import + View
     :in-app          navigate inside FreeMemo, then scroll to :fragment
     :fragment        scroll to a heading in this document; toast when absent
     :no-base         a relative href with no document URL to resolve against
     :handler         mailto:/tel: — hand to the OS, show no popover
     :inert-citation  reference plumbing whose target was never imported
     :inert           nothing this app can do

   Inv : never throws. A malformed href degrades to :inert, never to an
         exception in a click handler."
  (:require [clojure.string :as str]))

;; ── Reference plumbing ─────────────────────────────────────────────
;; 93.6% of the fragment links in the corpus point into a reference list that
;; the importer never carried into the document — 97371 `#cite_note`, 42421
;; `#cite_ref`, 3976 `#CITEREF`. No heading can ever satisfy them, so they get
;; no affordance at all: a toast on each would be noise, not information.
;;
;; If an importer ever carries reference lists in, this set must shrink.
(def ^:private citation-fragment-re
  #"(?x)
    ^(?:
       cite_note.*  | cite_ref.*  | CITEREF.*
     | fn[-_.\d].*  | fn\d*       | endnote.*  | footnote.*
     | ref-\d+      | n_.*        | g:\d+
     )$")

(defn- citation-fragment?
  "True when `frag` (no leading #) is reference plumbing rather than a heading."
  [frag]
  (boolean (and frag (re-matches citation-fragment-re frag))))

;; ── URL pieces ─────────────────────────────────────────────────────

(def ^:private absolute-re #"(?i)^([a-z][a-z0-9+.-]*):(.*)$")
(def ^:private http-scheme-re #"(?i)^https?$")

(defn- split-fragment
  "Post: [before-hash fragment-or-nil]. The fragment excludes the `#`.
   An empty fragment (`x#`) yields nil — there is nothing to scroll to."
  [s]
  (let [i (.indexOf ^String s "#")]
    (if (neg? i)
      [s nil]
      [(subs s 0 i) (not-empty (subs s (inc i)))])))

(defn- remove-dot-segments
  "RFC 3986 §5.2.4 on a path string.
   Pre : `path` is the path component, with or without a leading slash.
   Post: no `.` or `..` segment remains; a trailing slash is preserved."
  [path]
  (let [absolute? (and (seq path) (= \/ (first path)))
        segs (remove #{""} (str/split path #"/"))
        ;; §5.2.4 step 2D: a terminal "." or ".." is replaced by "/", so the
        ;; result keeps a trailing slash even though the segment itself goes.
        ;; Without this, base + "." yields /b/c instead of /b/c/.
        trailing? (or (and (seq path) (= \/ (last path)))
                    (contains? #{"." ".."} (last segs)))
        out (reduce (fn [acc seg]
                      (cond
                        (= "." seg)  acc
                        (= ".." seg) (if (seq acc) (pop acc) acc)
                        :else        (conj acc seg)))
              [] segs)
        joined (str/join "/" out)]
    (str (when absolute? "/")
      joined
      (when (and trailing? (seq joined)) "/"))))

(defn- base-parts
  "Post: {:scheme :authority :path} for an absolute http(s) base, else nil."
  [base]
  (when-let [[_ scheme rest] (and base (re-matches absolute-re base))]
    (when (re-matches http-scheme-re scheme)
      (let [after (if (str/starts-with? rest "//") (subs rest 2) rest)
            slash (.indexOf ^String after "/")
            authority (if (neg? slash) after (subs after 0 slash))
            path (if (neg? slash) "/" (subs after slash))
            ;; Drop the base's own query and fragment before merging.
            path (first (str/split path #"[?#]"))]
        {:scheme scheme :authority authority :path path}))))

(defn- resolve-against
  "RFC 3986 §5.3 merge, restricted to what an editor meets.
   Pre : `ref` has no scheme and is not empty; `base` is an absolute http(s) URL.
   Post: an absolute URL string, or nil when `base` is not usable."
  [ref base]
  (when-let [{:keys [scheme authority path]} (base-parts base)]
    (cond
      ;; //host/path — protocol-relative
      (str/starts-with? ref "//")
      (str scheme ":" ref)

      ;; /path — root-relative
      (str/starts-with? ref "/")
      (str scheme "://" authority (remove-dot-segments ref))

      ;; ?query — replace the base's query, keep its path
      (str/starts-with? ref "?")
      (str scheme "://" authority path ref)

      ;; path — relative to the base's directory
      :else
      (let [dir (subs path 0 (inc (.lastIndexOf ^String path "/")))]
        (str scheme "://" authority (remove-dot-segments (str dir ref)))))))

;; ── Entry point ────────────────────────────────────────────────────

(def ^:private topic-href-re #"^/viewer/topic/(\d+)$")
(def ^:private handler-scheme-re #"(?i)^(?:mailto|tel)$")

;; topics.id is a Postgres `integer`, so no topic is numbered above this.
(def ^:private max-topic-id 2147483647)

(defn- topic-id-of
  "The topic a `/viewer/topic/<id>` path names, or nil when it names none.

   Pre : `path` is an href with any fragment already removed.
   Post: an integer in [1, max-topic-id], or nil.
   Inv : never throws. `\\d+` is unbounded, so a long digit run would overflow
         `Long/parseLong` — and the two peers would not even fail alike, since
         `js/parseInt` returns 1e20 where the JVM raises. A run too long for
         the column is simply not a topic link, and the caller falls through to
         the ordinary relative-URL path."
  [path]
  (when-let [[_ digits] (re-matches topic-href-re path)]
    (when (<= (count digits) 10)          ; fits a Long and a JS safe integer
      (let [n #?(:clj (Long/parseLong digits) :cljs (js/parseInt digits 10))]
        (when (<= 1 n max-topic-id) n)))))

(defn- strip-fragment
  "The Import target. `page.html#a` and `page.html#b` are one page, so the
   fragment never reaches `:url` — and `:url` is the only dupe key, so there is
   no second way to derive one that could drift from this."
  [url]
  (when url (first (split-fragment url))))

(defn resolve-href
  "What the editor can offer for `href` in a document whose own address is
   `base-url`.

   Pre : `href` is the anchor's raw attribute value, or nil. `base-url` is the
         document's absolute http(s) address, or nil when it has none.
   Post: {:kind k} plus, by kind:
           :external  :url (absolute, fragment stripped — Import)
                      :view-url (absolute, fragment kept — View)
           :in-app    :topic-id, :fragment (or nil)
           :fragment  :fragment
         Other kinds carry no extra keys.
   Inv : `:fragment` means one thing only — an id to scroll to in the document
         the reader ends up in. An external link's fragment is on someone
         else's page and cannot be scrolled to here, so it is NOT reported;
         it lives in `:view-url`, the only place anything uses it.
   Inv : never throws."
  [href base-url]
  (let [href (some-> href str/trim)]
    (cond
      (or (nil? href) (= "" href)) {:kind :inert}

      ;; Fragment-only — this document, or plumbing pointing nowhere.
      (str/starts-with? href "#")
      (let [frag (not-empty (subs href 1))]
        (cond
          (nil? frag)              {:kind :inert-citation}   ; bare "#"
          (citation-fragment? frag) {:kind :inert-citation}
          :else                    {:kind :fragment :fragment frag}))

      ;; An in-app topic link, with an optional anchor inside it.
      :else
      (let [[before frag] (split-fragment href)]
        (if-let [topic-id (topic-id-of before)]
          {:kind :in-app
           :topic-id topic-id
           :fragment (when-not (citation-fragment? frag) frag)}

          (if-let [[_ scheme _] (re-matches absolute-re href)]
            (cond
              (re-matches handler-scheme-re scheme) {:kind :handler :url href}
              (re-matches http-scheme-re scheme)
              {:kind :external :url (strip-fragment href) :view-url href}
              ;; ftp:, javascript:, anything else this app cannot serve.
              :else {:kind :inert})

            ;; Relative. Needs the document's own address.
            (if-let [abs (resolve-against href base-url)]
              {:kind :external :url (strip-fragment abs) :view-url abs}
              {:kind :no-base})))))))
