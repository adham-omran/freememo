(ns freememo.sphinx-import
  "Map a Sphinx HTML documentation build onto a FreeMemo topic tree.

   Shape of the mapping (plans/sphinx-html-import.md):
     directory tree            -> topic tree; a directory's index.html IS that
                                  directory's topic
     directory with no index   -> synthetic container topic, nil content
     page                      -> topics row, kind 'web'
     <link rel=canonical>      -> one sources row for the BUILD, on the root
     <img src>                 -> media row; src rewritten to /api/media/<id>
     internal <a href>         -> /viewer/topic/<id>, fragment carried
     a.headerlink (the ¶)      -> removed before sanitizing
     fragment-only <a href>    -> kept, lands on a heading id
     external <a href>         -> unchanged

   **Ids are drawn before anything is written.** A documentation build's pages
   link to each other, so rewriting a page's links needs the topic ids of pages
   not yet inserted. `db/reserve-topic-ids!` supplies them up front, which makes
   the whole import one pass; the alternative is inserting every row and then
   UPDATEing its content, doubling the write for a 2000-page build. A failed
   import burns the reserved ids, which is what a sequence is for.

   **Transaction boundary: media is written first, outside the main
   transaction**, for the reason `freememo.supermemo-import` records —
   `upsert-media-row!` meters quota in a transaction of its own and the blobs
   are too large to hold one open across. The rollback therefore cannot reach
   them, so `import-tree!`'s failure path deletes the rows THIS import created
   and refunds their bytes. Topics and the source row commit or roll back as one
   unit.

   **Every page and link the mapping drops, synthesizes or rewrites is counted
   into the returned report.** Nothing is discarded silently.

   NEVER evaluates or fetches anything. The build is read from an extracted
   directory; no request leaves the server."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.db :as db]
            [freememo.doc-html :as doc-html]
            [freememo.html-cleaner :as cleaner]
            [freememo.input-check :as input]
            [freememo.priority :as priority]
            [freememo.quota :as quota]
            [freememo.text :as text]
            [next.jdbc :as jdbc]
            [taoensso.telemere :as tel])
  (:import [java.io File]
           [java.net URLConnection URLDecoder]
           [java.nio.file Paths]
           [org.jsoup Jsoup]
           [org.jsoup.nodes Document Element]))

;; ── What comes out of the archive ──────────────────────────────────

(def ^:private keep-entry-re
  "Entry names worth extracting. A build's bulk is `_static` JavaScript and the
   search index — 1.7 MB of it in the reference corpus — none of which any topic
   can reach. `.buildinfo` is kept although nothing reads its contents: it is
   how `find-doc-root` locates the build inside a wrapper directory."
  #"(?i)(\.buildinfo|\.html?|\.png|\.jpe?g|\.gif|\.webp|\.svg)$")

(defn extract-keep?
  "Whether an archive entry is worth writing to disk.
   Pre : `entry-name` is an archive entry name.
   Post: true for HTML, raster/vector images and `.buildinfo`; false otherwise.
   Invariant: pure; never throws."
  [entry-name]
  (boolean (re-find keep-entry-re (str entry-name))))

(def ^:private excluded-dirs
  "Directories that never hold reading content. `_sources` is the reStructured
   text the HTML was built from, `_downloads` is attachments, and the other two
   are assets — all four are addressed by the pages rather than read directly."
  #{"_static" "_sources" "_images" "_downloads"})

(def ^:private excluded-root-pages
  "Generated pages at the build root with nothing to read. `genindex` is a link
   table over every page, `search` is an empty shell driven by the search index
   this import does not extract, and the other three are navigation or
   boilerplate. Matched at the root only — a page with one of these names
   deeper in the tree is a real page about that subject."
  #{"genindex.html" "search.html" "404.html" "versions.html" "copyright.html"})

(defn find-doc-root
  "The directory holding the Sphinx build, which may sit inside a wrapper.
   Pre : `dir` is a readable directory.
   Post: the shallowest directory containing a `.buildinfo`, or nil."
  [^File dir]
  (->> (file-seq dir)
    (filter #(and (.isFile ^File %) (= ".buildinfo" (.getName ^File %))))
    (map #(.getParentFile ^File %))
    (sort-by #(count (.getPath ^File %)))
    first))

;; ── Paths ──────────────────────────────────────────────────────────
;; Keys in this namespace are POSIX-separated paths relative to the build root.
;; They are the tree's identity: a page's key, its parent's key and a link
;; target's key are the same kind of string, so resolution is one lookup.

(defn- rel-key
  "`f`'s path relative to `root`, POSIX-separated."
  [^File root ^File f]
  (-> (.relativize (.toPath root) (.toPath f)) str (str/replace "\\" "/")))

(defn- segments [k] (str/split k #"/"))

(defn- dir-of
  "The directory part of a key, or nil when the key sits at the root."
  [k]
  (when-let [i (str/last-index-of k "/")] (subs k 0 i)))

(defn- index-key
  "The key of `dir`'s own topic — its index.html, real or synthetic."
  [dir]
  (if dir (str dir "/index.html") "index.html"))

(defn- parent-key
  "The key of the topic `k` hangs off, or nil when `k` is the tree root.
   A directory index is a child of its PARENT directory's index; every other
   page is a child of its own directory's index."
  [k]
  (let [d (dir-of k)]
    (if (= "index.html" (last (segments k)))
      (when d (index-key (dir-of d)))
      (index-key d))))

(defn- importable-page?
  "Whether a key names a page this import reads."
  [k]
  (let [lower (str/lower-case k)
        segs (segments k)]
    (and (str/ends-with? lower ".html")
      (not-any? excluded-dirs (butlast segs))
      (not (and (= 1 (count segs)) (excluded-root-pages lower))))))

(defn- url-decode
  "Percent-decode a link target. `+` is escaped first — it is a literal plus in
   a path, and URLDecoder would otherwise read it as a space."
  [s]
  (try (URLDecoder/decode (str/replace s "+" "%2B") "UTF-8")
       (catch Exception _ s)))

(def ^:private absolute-target-re
  "A target this import must not touch: it carries a scheme, or is
   protocol-relative."
  #"(?i)^([a-z][a-z0-9+.-]*:|//)")

(defn- resolve-target
  "Resolve a relative link target against the page that carries it.
   Pre : `from-key` is a page key; `target` is a raw href/src attribute value.
   Post: [key fragment] where `key` is a build-root-relative POSIX path and
         `fragment` is the text after `#` or nil, or nil when the target is
         absolute, empty, fragment-only, or escapes the build root.
   Invariant: pure; never throws."
  [from-key target]
  (let [t (str/trim (str target))]
    (when-not (or (str/blank? t)
                (str/starts-with? t "#")
                (re-find absolute-target-re t))
      (let [[path+q fragment] (str/split t #"#" 2)
            path (first (str/split path+q #"\?" 2))]
        (when-not (str/blank? path)
          (let [base (.getParent (Paths/get from-key (into-array String [])))
                decoded (url-decode path)
                resolved (if base
                           (.resolve base decoded)
                           (Paths/get decoded (into-array String [])))
                norm (str/replace (str (.normalize resolved)) "\\" "/")]
            (when-not (str/starts-with? norm "..")
              [norm (not-empty (str fragment))])))))))

;; ── Titles ─────────────────────────────────────────────────────────

(defn- clamp-title
  "A title `insert-topic-rows!` will accept. Over-length is truncated rather
   than rejected: a documentation heading is the page's only name, and losing
   the page over its length is the worse trade."
  [s]
  (let [t (input/sanitize-title s)]
    (if (> (count t) input/title-max) (subs t 0 input/title-max) t)))

(defn- humanize
  "A readable title from one path segment: extension dropped, separators to
   spaces, each word capitalized. `tool_settings` reads \"Tool Settings\"."
  [seg]
  (->> (-> (str seg)
         (str/replace #"(?i)\.html?$" "")
         (str/replace #"[-_]+" " ")
         input/prettify-title
         (str/split #"\s+"))
    (map #(if (str/blank? %) % (str (str/upper-case (subs % 0 1)) (subs % 1))))
    (str/join " ")
    clamp-title))

(defn- page-title-fallback
  "A page's title when it carries no `h1`. Its own filename is the only name it
   has."
  [k]
  (humanize (last (segments k))))

(defn- container-title
  "A synthesized container's title, taken from the DIRECTORY it stands for.
   Its key ends in `index.html` like every other directory topic, so naming it
   from the key's tail would call all seven of them \"index\"."
  [k]
  (if-let [d (dir-of k)]
    (humanize (last (segments d)))
    "Documentation"))

;; ── Page reading ───────────────────────────────────────────────────

(def ^:private body-selectors
  "Where a Sphinx theme puts the page body, most specific first. furo and the
   Read the Docs theme use the first two; alabaster uses `div.body`. The `body`
   fallback imports theme chrome along with the content, which is worse than a
   clean extraction and better than an empty topic."
  ["article[role=main]" "div[role=main]" "div[itemprop=articleBody]"
   "div.body" "main" "body"])

(defn- article-element
  "The element holding the page's content, by the first selector that matches."
  ^Element [^Document doc]
  (some (fn [sel] (first (.select doc sel))) body-selectors))

(defn- canonical-url
  "The page's canonical URL, or nil."
  [^Document doc]
  (some-> ^Element (first (.select doc "link[rel=canonical]"))
    (.attr "href") str/trim not-empty))

(defn- strip-headerlinks!
  "Remove Sphinx's ¶ heading anchors. Returns how many were removed.

   This runs BEFORE `clean-html`, and it has to: the sanitizer strips the
   `headerlink` class that identifies them, so afterwards they are
   indistinguishable from a real same-page reference. In the reference corpus
   they are 12 738 of the 13 300 fragment-only links — the great majority of
   the fragment problem, removed at the boundary that can still see them."
  [^Element el]
  (let [anchors (.select el "a.headerlink")]
    (doseq [^Element a anchors] (.remove a))
    (.size anchors)))

;; ── Media ──────────────────────────────────────────────────────────

(defn- store-image!
  "Store one image file, once per import, and return its media id.

   Pre : `!media` holds {:by-file {File id} :created-ids #{id}}; `user-id` owns
         the write.
   Post: the media id, or nil when the file is unreadable. `:created-ids` gains
         the id only when this call inserted the row — a file whose bytes the
         user already held must not be deleted by the failure path.
   Invariant: a quota breach propagates. Swallowing it would silently drop
         every remaining image while reporting a successful import, and the
         user would have paid for the ones that fit."
  [user-id !media ^File f]
  (or (get-in @!media [:by-file f])
    (try
      (let [bytes (with-open [in (io/input-stream f)] (.readAllBytes in))
            row (db/upsert-media-row!
                  {:user-id user-id :kind "image" :bytes bytes
                   :mime-type (or (URLConnection/guessContentTypeFromName (.getName f))
                                "application/octet-stream")})]
        (swap! !media (fn [m]
                        (cond-> (assoc-in m [:by-file f] (:media/id row))
                          (:media/inserted? row)
                          (update :created-ids conj (:media/id row)))))
        (:media/id row))
      (catch Exception e
        (when (quota/quota-error? (ex-data e)) (throw e))
        (tel/log! {:level :warn :id ::image-skipped
                   :data {:file (.getName f) :error (.getMessage e)}}
          "Sphinx image skipped")
        nil))))

;; ── Rewriting ──────────────────────────────────────────────────────

(defn- rewrite-images!
  "Point every `<img>` at stored media. Mutates `article` in place.
   Post: {:stored n :unresolved n}. An image whose file is missing or unreadable
         is REMOVED rather than left pointing at a path this server does not
         serve — a broken image is a worse artefact than an absent one."
  [user-id !media ^File root page-key ^Element article]
  (let [!stored (atom 0) !unresolved (atom 0)]
    (doseq [^Element img (vec (.select article "img[src]"))]
      (let [src (.attr img "src")
            [target _] (resolve-target page-key src)
            f (when target (io/file root target))
            media-id (when (and f (.isFile f)) (store-image! user-id !media f))]
        (if media-id
          (do (.attr img "src" (str "/api/media/" media-id))
              (swap! !stored inc))
          (do (.remove img) (swap! !unresolved inc)))))
    {:stored @!stored :unresolved @!unresolved}))

(defn- rewrite-links!
  "Rewrite every `<a>` to something this app can serve. Mutates in place.

   Post: a counter map. An internal page link becomes
         `/viewer/topic/<id>[#fragment]`. A fragment-only link is KEPT as it is.
         A link to a page this import excluded, a dead link and a link to a
         non-image asset are UNWRAPPED: the text stays, the anchor goes. An
         absolute link is left exactly as it is.

   **The fragment used to be dropped and fragment-only links unwrapped**, both
   justified by `clean-html` stripping every `id` — there was nothing for an
   in-page anchor to land on. That stopped being true when the sanitizer
   allow-listed `id` on `h1`-`h6` and `p` and `freememo.quill-field` registered
   a Parchment attributor to keep it through an edit.

   Dropping the fragment collapsed distinct destinations onto one topic: 3531
   provable duplicates in the corpus, where three links differing only by
   anchor all became `/viewer/topic/309`.

   Neither change reaches a build already imported — those topics hold no ids,
   so a carried fragment would land nowhere. Re-import is what pays it off.
   See plans/uniform-anchor-links.md §3."
  [user-id !media ^File root page-key id-by-key ^Element article]
  (let [c (atom {:internal 0 :fragment-only 0 :asset 0 :external 0 :dead 0})
        bump! #(swap! c update % inc)]
    (doseq [^Element a (vec (.select article "a[href]"))]
      (let [href (str/trim (.attr a "href"))]
        (cond
          (str/blank? href) (.unwrap a)

          ;; An anchor into this same page. It lands on a heading `id` the
          ;; sanitizer now keeps, so it is left exactly as written.
          (str/starts-with? href "#")
          (bump! :fragment-only)

          (re-find absolute-target-re href)
          (bump! :external)

          :else
          (let [[target fragment] (resolve-target page-key href)
                anchor (if fragment (str "#" fragment) "")
                topic-id (when target (get id-by-key target))]
            (cond
              topic-id
              (do (.attr a "href" (str "/viewer/topic/" topic-id anchor))
                  (bump! :internal))

              ;; A `dirhtml` build addresses a page by its directory, so
              ;; `../meshes/` names `../meshes/index.html`. Normalizing drops
              ;; the trailing slash, which is why this is a second lookup
              ;; rather than a branch in `resolve-target`.
              (and target (get id-by-key (index-key target)))
              (do (.attr a "href" (str "/viewer/topic/" (get id-by-key (index-key target)) anchor))
                  (bump! :internal))

              :else
              (let [f (when target (io/file root target))
                    media-id (when (and f (.isFile f)
                                     (re-find #"(?i)\.(png|jpe?g|gif|webp|svg)$" target))
                               (store-image! user-id !media f))]
                (if media-id
                  (do (.attr a "href" (str "/api/media/" media-id))
                      (bump! :asset))
                  (do (.unwrap a) (bump! :dead)))))))))
    @c))

;; ── Tree ───────────────────────────────────────────────────────────

(defn- build-nodes
  "Every topic the tree needs, real pages and synthetic containers alike.

   Pre : `page-keys` is the set of importable page keys.
   Post: {key {:key :page-key :parent-key}}. A `:page-key` of nil marks a
         synthesized container — a directory that holds pages but no
         `index.html` of its own (seven of them in the reference corpus).
         Synthesis is transitive: a container's own parent is synthesized too
         when it is missing, up to the root.
   Invariant: terminates. Each round adds only keys strictly shorter in
         segment count than the ones that named them, and `parent-key` returns
         nil at the root."
  [page-keys]
  (loop [nodes (into {} (map (fn [k] [k {:key k :page-key k :parent-key (parent-key k)}]))
                 page-keys)]
    (let [missing (into #{} (comp (keep :parent-key) (remove #(contains? nodes %)))
                    (vals nodes))]
      (if (empty? missing)
        nodes
        (recur (into nodes
                 (map (fn [k] [k {:key k :page-key nil :parent-key (parent-key k)}]))
                 missing))))))

(defn- tree-depth
  "Distance from `k` to the tree root, following `:parent-key`.

   Path segment count is NOT this number and cannot stand in for it: a
   root-level page and the root index both have one segment, yet one is the
   other's child. Terminates because `parent-key` strictly shortens its
   argument and returns nil at the root."
  [nodes k]
  (loop [k k n 0]
    (if-let [p (:parent-key (get nodes k))]
      (recur p (inc n))
      n)))

(defn- ordered-nodes
  "Nodes breadth-first: every parent precedes its children.

   `insert-topic-rows!` splits a wide list across statements, and `parent_id`
   is a foreign key, so a child in an earlier batch than its parent would fail.
   Ordering here is what makes the split safe, and it puts the root first —
   which is what lets the caller address it as row 0."
  [nodes]
  (vec (sort-by (juxt #(tree-depth nodes (:key %)) :key) (vals nodes))))

;; ── Import ─────────────────────────────────────────────────────────

(defn- read-page
  "Parse one page into the row material the insert needs.
   Post: {:title :html :canonical :counts} or nil when the page has no body.
   Invariant: mutation is confined to the parsed document, which is discarded
         here — nothing on disk is written."
  [user-id !media ^File root page-key id-by-key ^File f]
  (let [doc (Jsoup/parse f "UTF-8" "")
        ;; Jsoup pretty-prints by default, so `(.html article)` below would
        ;; re-indent the markup and `clean-html` — which disables pretty-print
        ;; precisely so it never reformats — would then keep that indentation
        ;; as text nodes. `.ql-editor` is `white-space: pre-wrap`, so those
        ;; nodes are real blank space in any view that does not go through
        ;; Quill: one page measured 30180px tall against 9292px collapsed.
        ;; Serializing compactly stops them being created, which is cheaper and
        ;; more reliable than deleting them afterwards.
        _ (.prettyPrint (.outputSettings doc) false)
        article (article-element doc)]
    (when article
      (let [headerlinks (strip-headerlinks! article)
            title (or (some-> ^Element (first (.select article "h1")) .text str/trim not-empty)
                    (page-title-fallback page-key))
            imgs (rewrite-images! user-id !media root page-key article)
            links (rewrite-links! user-id !media root page-key id-by-key article)
            ;; Definition lists, keyboard keys, admonitions and figures have no
            ;; blot in Quill's Delta, so a stored page renders flat and the
            ;; first edit writes that flattening back. Rewrite them into forms
            ;; Quill holds, BEFORE clean-html strips the classes that identify
            ;; them — the same ordering `strip-headerlinks!` needs, and for the
            ;; same reason. See freememo.doc-html.
            rewrites (doc-html/rewrite-for-quill! article)
            ;; drop-bare-spans runs AFTER the sanitizer: a Sphinx
            ;; `span.std-ref` only becomes attribute-less once clean-html has
            ;; taken its class away.
            html (-> (.html article)
                   cleaner/clean-html
                   doc-html/drop-bare-spans)]
        {:title (clamp-title title)
         :html (not-empty (str/trim (str html)))
         :canonical-url (canonical-url doc)
         :counts (merge links rewrites
                   {:headerlinks headerlinks
                    :images-stored (:stored imgs)
                    :images-unresolved (:unresolved imgs)})}))))

(defn- merge-counts [a b] (merge-with + a b))

(defn import-tree!
  "Import an extracted Sphinx HTML build for `user-id`.

   Pre : `dir` is a readable directory holding a Sphinx build (this fn locates
         the build root within it); the caller owns `dir` and deletes it
         afterwards, on both the success and failure paths.
   Post: {:ok true :topic-id N :report {...}} when the tree committed, or
         {:ok false :error S} when it did not. On failure NOTHING persists:
         topics and the source row roll back with the transaction, and the media
         rows written before it opened are deleted and their bytes refunded, so
         a failed import costs the user no storage. Media whose bytes the user
         already held is untouched.
   Invariant: every page skipped, container synthesized and link rewritten,
         unwrapped or left alone is counted in `:report`."
  [user-id ^File dir]
  (let [root (find-doc-root dir)
        html-files (when root
                     (->> (file-seq root)
                       (filter #(.isFile ^File %))
                       (map (fn [f] [(rel-key root f) f]))
                       (filter #(str/ends-with? (str/lower-case (first %)) ".html"))
                       (sort-by first)))
        pages (filterv #(importable-page? (first %)) html-files)]
    (cond
      (nil? root)
      {:ok false :error "No Sphinx build found in the archive (expected a .buildinfo file)."}

      (empty? pages)
      {:ok false :error "This build has no importable pages."}

      :else
      (let [file-by-key (into {} pages)
            nodes (build-nodes (set (keys file-by-key)))
            ordered (ordered-nodes nodes)
            ids (db/reserve-topic-ids! (count ordered))
            id-by-key (zipmap (map :key ordered) ids)
            root-topic-id (first ids)
            !media (atom {:by-file {} :created-ids #{}})
            !counts (atom {})
            !empty (atom 0)
            ;; One sources row for the BUILD, on the root. A row per page would
            ;; put one bibliography entry per page — 2361 of them for the
            ;; reference corpus — when the citation belongs to the manual, not
            ;; to each of its pages. Captured from whichever page is read
            ;; first; every page of a build carries the same site.
            !build-url (atom nil)]
        (try
          (let [rows (mapv (fn [{:keys [key page-key parent-key]}]
                             (let [page (when page-key
                                          (read-page user-id !media root key
                                            id-by-key (file-by-key page-key)))
                                   html (:html page)]
                               (when page
                                 (swap! !counts merge-counts (:counts page))
                                 (swap! !build-url #(or % (:canonical-url page))))
                               (when (and page-key (nil? html)) (swap! !empty inc))
                               (cond-> {:id (get id-by-key key)
                                        :user_id user-id
                                        :kind "web"
                                        :title (or (:title page) (container-title key))
                                        :status "active"
                                        :priority priority/priority-default}
                                 parent-key (assoc :parent_id (get id-by-key parent-key))
                                 html (assoc :content html
                                        :content_text (text/strip-html html)))))
                       ordered)
                build-url @!build-url
                root-title (:title (first rows))]
            (jdbc/with-transaction [tx db/ds]
              (let [source-id (when build-url
                                (first (db/insert-source-rows! tx
                                         [{:user_id user-id
                                           :csl_type "webpage"
                                           :csl {:title root-title :URL build-url}
                                           :url build-url
                                           :title root-title}])))
                    rows (if source-id
                           (assoc-in rows [0 :source_id] source-id)
                           rows)]
                (db/insert-topic-rows! tx rows)
                {:ok true
                 :topic-id root-topic-id
                 :report
                 {:build (.getName root)
                  :url build-url
                  :counts
                  (let [c @!counts]
                    {:html-files-found (count html-files)
                     :pages-excluded (- (count html-files) (count pages))
                     :pages-imported (- (count pages) @!empty)
                     :pages-without-body @!empty
                     :containers-synthesized (count (remove :page-key ordered))
                     :topics-created (count rows)
                     :sources-created (if source-id 1 0)
                     :images-stored (count (:by-file @!media))
                     :images-unresolved (or (:images-unresolved c) 0)
                     :headerlinks-removed (or (:headerlinks c) 0)
                     :links-to-topics (or (:internal c) 0)
                     :links-to-assets (or (:asset c) 0)
                     :links-external-kept (or (:external c) 0)
                     :links-fragment-kept (or (:fragment-only c) 0)
                     :links-dead-unwrapped (or (:dead c) 0)
                     ;; freememo.doc-html — what it rewrote so Quill can hold it.
                     :terms-flattened (or (:terms-flattened c) 0)
                     :definition-bodies-flattened (or (:definition-bodies-flattened c) 0)
                     :keys-to-code (or (:keys-to-code c) 0)
                     :admonitions-to-blockquotes (or (:admonitions-to-blockquotes c) 0)
                     :figures-flattened (or (:figures-flattened c) 0)
                     :whitespace-nodes-dropped (or (:whitespace-nodes-dropped c) 0)})}})))
          (catch Exception e
            (tel/error! {:id ::import-failed :data {:user-id user-id}} e)
            ;; Media is written before the transaction opens, so the rollback
            ;; cannot reach it. Give back only the rows this import created, or
            ;; the user is billed for content nothing references. Guarded
            ;; separately so a failed compensation cannot mask the real error.
            (let [created (:created-ids @!media)]
              (try
                (let [r (db/delete-media-rows! user-id created)]
                  (when (pos? (long (:deleted r)))
                    (tel/log! {:level :info :id ::import-media-reverted
                               :data {:user-id user-id :deleted (:deleted r)
                                      :refunded (:refunded r)}}
                      "Reverted media stored by a failed Sphinx import")))
                (catch Exception ce
                  (tel/error! {:id ::import-media-revert-failed
                               :data {:user-id user-id :media-ids (count created)}} ce))))
            (if (quota/quota-error? (ex-data e))
              {:ok false :error "Not enough storage left to import this documentation build."}
              {:ok false :error (or (.getMessage e) "Import failed")})))))))
