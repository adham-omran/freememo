(ns freememo.kg-code
  "Static Clojure repo → knowledge-graph facts + topic tree.

   This is the AST→Facts front half; everything from kg_facts rightward
   (linking store, question generation, FSRS, review UI, Ontop RDF) is the
   existing prose pipeline, reused unchanged.

   SAFETY INVARIANT: clj-kondo's :analysis is a purely STATIC read — no
   namespace of the uploaded (untrusted) repo is loaded or evaluated. Nothing
   here may eval or require ingested code.

   Production flow (web-import/confirm-staged-upload!* :repo):
     unzip-repo! (bomb-guarded → temp dir)
       → create-repo-topics! (SYNC: 'code' root + one 'code' child per file;
         caller navigates immediately)
       → start-repo-distill! (ASYNC, mirrors kg-extract/start-distill!:
         analyze-repo → analysis->triples → persist-facts!, then delete dir).
   ingest-repo! runs both phases synchronously (test/REPL entry).

   The predicate vocabulary is FIXED (code relations are known at design time),
   unlike prose extraction's LLM-proposed, review-gated vocabulary."
  (:require
   [clj-kondo.core :as clj-kondo]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.user-state :as us]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]
   [missionary.core :as m])
  (:import
   [java.io File ByteArrayInputStream ByteArrayOutputStream]
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]
   [java.util.zip ZipInputStream ZipEntry]
   [java.util.concurrent Executors]
   [missionary Cancelled]))

;; ---------------------------------------------------------------------------
;; Ontology
;; ---------------------------------------------------------------------------

(def predicates
  "Fixed code-relation vocabulary; slugs are IRI-safe by construction (no
   slugify needed). Seeded status='approved' on first use."
  [{:slug "defined-in"    :label "defined in"}
   {:slug "calls"         :label "calls"}
   {:slug "requires"      :label "requires"}
   {:slug "has-arglist"   :label "has arglist"}
   {:slug "has-docstring" :label "has docstring"}
   {:slug "is-private"    :label "is private"}])

(def ^:private analyzer-tag
  "Recorded as kg_facts.source_model — provenance, mirroring the LLM pipeline's
   model id."
  "clj-kondo")

;; ---------------------------------------------------------------------------
;; Analysis → triples (pure)
;; ---------------------------------------------------------------------------

(defn- var-label
  "Fully-qualified var entity label, e.g. \"freememo.db/create-topic!\"."
  [ns name]
  (str ns "/" name))

(defn project-namespaces
  "Set of namespace-name strings DEFINED in `analysis` — the internal surface.
   calls/requires edges to namespaces outside this set (clojure.core, libraries)
   are dropped as noise, keeping the graph about THIS codebase.
   Pre: analysis is a clj-kondo :analysis map."
  [analysis]
  (into #{} (map (comp str :name)) (:namespace-definitions analysis)))

(defn analysis->triples
  "clj-kondo :analysis → distinct vector of {:s :p (:o | :lit)} triples. Pure.
   Every triple satisfies the existing valid-triple? shape (:s/:p non-blank
   strings, exactly one of :o/:lit).
   Edges:
     var -defined-in->    namespace
     var -has-arglist->   literal (space-joined arglists)  [when present]
     var -has-docstring-> literal (docstring)              [when present]
     var -is-private->    literal \"true\"                 [when private]
     var -calls->         var        [internal callees only]
     ns  -requires->      ns         [internal targets only]
   Self-loops (s = o) are dropped, matching kg-extract."
  [analysis]
  (let [internal (project-namespaces analysis)
        def-triples
        (mapcat
          (fn [{:keys [ns name arglist-strs doc private]}]
            (let [v (var-label ns name)]
              (cond-> [{:s v :p "defined-in" :o (str ns)}]
                (seq arglist-strs)     (conj {:s v :p "has-arglist" :lit (str/join " " arglist-strs)})
                (not (str/blank? doc)) (conj {:s v :p "has-docstring" :lit doc})
                private                (conj {:s v :p "is-private" :lit "true"}))))
          (:var-definitions analysis))
        call-triples
        (keep
          (fn [{:keys [from from-var to name]}]
            (when (and from-var (contains? internal (str to)))
              {:s (var-label from from-var) :p "calls" :o (var-label to name)}))
          (:var-usages analysis))
        require-triples
        (keep
          (fn [{:keys [from to]}]
            (when (contains? internal (str to))
              {:s (str from) :p "requires" :o (str to)}))
          (:namespace-usages analysis))]
    (into []
      (comp cat
        (remove (fn [{:keys [s o]}] (and o (= s o))))
        (distinct))
      [def-triples call-triples require-triples])))

(defn analyze-repo
  "clj-kondo :analysis over `root-dir`, the WHOLE repo in one run so var-usage
   call edges resolve across files. STATIC — loads/evaluates nothing.
   Pre: root-dir is a readable directory path or File. Post: a clj-kondo
   :analysis map (empty buckets when no Clojure sources present)."
  [root-dir]
  (:analysis
    (clj-kondo/run! {:lint [(str root-dir)]
                     :config {:output {:analysis {:arglists true}}}})))

;; ---------------------------------------------------------------------------
;; Persistence (deterministic, no LLM)
;; ---------------------------------------------------------------------------

(defn persist-facts!
  "Insert `triples` as kg_facts under graph `graph-topic-id`, linking entities
   by EXACT label (no trigram/LLM). Predicates seeded from the fixed vocabulary.
   Pre:  triples pass analysis->triples' shape; graph-topic-id owned by user-id
         (caller passes ids from its own ingest — violation = caller bug).
   Post: count of facts actually inserted (kg_facts dedup drops repeats)."
  [user-id graph-topic-id triples]
  (let [pred-id (into {}
                  (map (fn [{:keys [slug label]}]
                         [slug (:id (db/get-or-create-kg-predicate! user-id slug label))]))
                  predicates)
        entity-id (memoize (fn [label] (db/get-or-create-kg-entity! user-id label)))
        rows (into []
               (keep (fn [{:keys [s p o lit]}]
                       (when-let [pid (pred-id p)]
                         {:user_id user-id
                          :subject_entity_id (entity-id s)
                          :predicate_id pid
                          :object_entity_id (when o (entity-id o))
                          :object_literal lit
                          :object_datatype nil
                          :graph_topic_id graph-topic-id
                          :page_number nil
                          :status "approved"
                          :source_model analyzer-tag})))
               triples)]
    (count (db/insert-kg-facts! rows))))

;; ---------------------------------------------------------------------------
;; Topic tree
;; ---------------------------------------------------------------------------

(defn- html-escape [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn source->code-block-html
  "Source string → Quill code-block HTML: one <div class=\"ql-code-block\"
   data-language=lang> per source line, wrapped in a .ql-code-block-container.
   Matches quill_field's div.ql-code-block clipboard matcher, so stored source
   round-trips in the editor; html-cleaner's container carve-out preserves the
   data-language attribute.
   Pre:  lang matches ^[A-Za-z0-9_-]+$. Post: HTML string; blank lines
         preserved (split limit -1)."
  [source lang]
  (str "<div class=\"ql-code-block-container\" spellcheck=\"false\">"
    (str/join
      (map (fn [line]
             (str "<div class=\"ql-code-block\" data-language=\"" lang "\">"
               (html-escape line) "</div>"))
        (str/split (str source) #"\n" -1)))
    "</div>"))

(defn- path->title
  "Repo-relative path → a title that survives create-topic!'s sanitize-filename
   (which strips / and \\). Separators become \" > \" so the file's location
   stays legible in a flat list (directory nesting is out of scope for v1)."
  [rel-path]
  (str/replace rel-path #"[\\/]+" " > "))

(def ^:private clojure-ext-re #"(?i)\.clj[cs]?$")

(defn- quill-language
  "Quill syntax key for `filename`, or nil when unsupported (v1: Clojure only)."
  [filename]
  (when (re-find clojure-ext-re filename) "clojure"))

(defn- clojure-source-files
  "Seq of {:rel :source :lang} for every Clojure source file under `root-dir`.
   Pre:  root-dir readable. Post: :rel is root-relative, :source the file text,
   :lang a Quill syntax key."
  [root-dir]
  (let [root (io/file root-dir)
        root-path (.toPath root)]
    (->> (file-seq root)
      (filter #(.isFile ^File %))
      (keep (fn [^File f]
              (when-let [lang (quill-language (.getName f))]
                {:rel (str (.relativize root-path (.toPath f)))
                 :source (slurp f)
                 :lang lang}))))))

;; ---------------------------------------------------------------------------
;; Zip unpack (bomb-guarded, static — never evaluates extracted code)
;; ---------------------------------------------------------------------------

(defn delete-dir!
  "Recursively delete `dir`. Best-effort; never throws."
  [^File dir]
  (when (and dir (.exists dir))
    (doseq [^File f (reverse (file-seq dir))]
      (try (.delete f) (catch Exception _ nil)))))

(def ^:private max-zip-entries 50000)
(def ^:private max-entry-bytes (* 20 1024 1024))
(def ^:private max-total-uncompressed-bytes (* 300 1024 1024))

(defn- safe-target
  "File for `entry-name` under `dir`, or nil if it escapes `dir` (zip-slip /
   absolute path). Canonical-path containment is the real guard.
   Pre: dir a File. Post: a File strictly under dir, or nil."
  [^File dir ^String entry-name]
  (let [f (File. dir entry-name)
        base (str (.getCanonicalPath dir) File/separator)]
    (when (.startsWith (.getCanonicalPath f) base) f)))

(defn unzip-repo!
  "Extract a repo zip's Clojure sources to a fresh temp directory for static
   analysis + topic storage. Bomb-guarded: caps entry count, per-entry bytes,
   and total uncompressed bytes → ex-info {:type ::zip-too-large}. Zip-slip
   entries and non-Clojure files are skipped. NEVER evaluates extracted code.
   Pre:  zip-bytes is a byte[] ZIP archive.
   Post: a temp dir File holding the extracted .clj/.cljc/.cljs tree; on any
         failure the temp dir is removed and the error rethrown. Caller (or
         start-repo-distill!) MUST delete the returned dir."
  [^bytes zip-bytes]
  (let [dir (.toFile (Files/createTempDirectory "repo-ingest" (make-array FileAttribute 0)))]
    (try
      (with-open [zis (ZipInputStream. (ByteArrayInputStream. zip-bytes))]
        (loop [n-entries 0 total 0]
          (when (> n-entries max-zip-entries)
            (throw (ex-info "Zip has too many entries" {:type ::zip-too-large})))
          (if-let [^ZipEntry e (.getNextEntry zis)]
            (let [nm (.getName e)
                  target (when-not (.isDirectory e) (safe-target dir nm))]
              (if (or (nil? target) (not (re-find clojure-ext-re nm)))
                (recur (inc n-entries) total)
                (let [baos (ByteArrayOutputStream.)
                      buf (byte-array 8192)
                      written (loop [w 0]
                                (let [r (.read zis buf)]
                                  (if (pos? r)
                                    (let [w2 (+ w r)]
                                      (when (> w2 max-entry-bytes)
                                        (throw (ex-info "Zip entry too large" {:type ::zip-too-large})))
                                      (.write baos buf 0 r)
                                      (recur w2))
                                    w)))
                      total2 (+ total written)]
                  (when (> total2 max-total-uncompressed-bytes)
                    (throw (ex-info "Zip too large uncompressed" {:type ::zip-too-large})))
                  (io/make-parents target)
                  (io/copy (.toByteArray baos) target)
                  (recur (inc n-entries) total2))))
            dir)))
      (catch Throwable t
        (delete-dir! dir)
        (throw t)))))

;; ---------------------------------------------------------------------------
;; Orchestrator
;; ---------------------------------------------------------------------------

(defn create-repo-topics!
  "Create the repo-root 'code' topic + one 'code' child per Clojure source file
   (source stored in code-block style). Fast, synchronous — returns before fact
   distillation so the caller can navigate immediately.
   Pre:  root-dir readable; repo-name non-blank; user-id valid.
   Post: {:root-id int :file-topics int}. Root has nil content (a container);
         each child's content round-trips through the Quill editor."
  [user-id repo-name root-dir]
  (let [root-id (:topics/id (db/create-topic! {:user-id user-id :kind "code" :title repo-name}))
        files (clojure-source-files root-dir)]
    (doseq [{:keys [rel source lang]} files]
      (db/create-topic! {:user-id user-id :kind "code" :parent-id root-id
                        :title (path->title rel)
                        :content (source->code-block-html source lang)}))
    {:root-id root-id :file-topics (count files)}))

(defn distill-repo!
  "Distill static facts from `root-dir` into kg_facts under `root-id` (the named
   graph). Synchronous — run via start-repo-distill!. NO LLM, NO credit charge.
   Pre:  root-id owned by user-id; root-dir readable; clj-kondo on classpath.
   Post: {:success true :facts n} or {:success false :error msg}. Purely
         static — ingested code is never evaluated. Cancellation surfaces as
         InterruptedException for m/via."
  [user-id root-id root-dir]
  (try
    (let [facts (persist-facts! user-id root-id (analysis->triples (analyze-repo root-dir)))]
      (tel/log! {:level :info :id ::distill-repo-complete
                 :data {:user-id user-id :root root-id :facts facts}}
        "Repo fact distillation complete")
      {:success true :facts facts})
    (catch InterruptedException e (throw e))
    (catch Exception e
      (tel/error! {:id ::distill-repo} e)
      {:success false :error (.getMessage e)})))

(defonce ^:private distill-executor (Executors/newFixedThreadPool 2))

(def ^:private distill-timeout-ms
  "20 min — a large repo's analysis + per-entity linking round-trips."
  (* 20 60 1000))

(defn start-repo-distill!
  "Submit an async fact-distillation run for (user-id, root-id) over root-dir,
   then delete root-dir. Mirrors kg-extract/start-distill!: bounded executor,
   20-min timeout, completion toast, cancel fn. No-op when one is already in
   flight for that root. TAKES OWNERSHIP of root-dir — deletes it when the run
   ends (success, failure, or cancel).
   Pre:  root-id owned by user-id; root-dir is unzip-repo!'s temp tree.
   Post: nil. Progress in :distilling-repos; cancel fn in
         :repo-distill-cancellers; success toasts + bumps :distill."
  [user-id root-id root-dir]
  (when-not (contains? @(us/get-atom user-id :distilling-repos) root-id)
    (swap! (us/get-atom user-id :distilling-repos) conj root-id)
    (tel/log! {:level :info :id ::repo-distill-started
               :data {:user-id user-id :root root-id}}
      "Repo fact distillation started")
    (let [finish (fn [] (delete-dir! root-dir)
                   (swap! (us/get-atom user-id :repo-distill-cancellers) dissoc root-id)
                   (swap! (us/get-atom user-id :distilling-repos) disj root-id))
          cancel-fn
          ((m/timeout
             (m/via distill-executor (distill-repo! user-id root-id root-dir))
             distill-timeout-ms
             {:success false :error "Repo distillation timed out after 20 minutes"})
           (fn [result]
             (finish)
             (if (:success result)
               (do (commands/bump! user-id :distill)
                 (toasts/push! user-id
                   {:level :success
                    :message (str (:facts result) " code fact"
                               (when (not= 1 (:facts result)) "s") " extracted")}))
               (toasts/push! user-id {:level :error :message (:error result)})))
           (fn [e]
             (finish)
             (when-not (instance? Cancelled e)
               (tel/error! {:id ::repo-distill-task} e)
               (toasts/push! user-id {:level :error :message (ex-message e)}))))]
      (swap! (us/get-atom user-id :repo-distill-cancellers) assoc root-id cancel-fn))
    nil))

(defn ingest-repo!
  "Synchronous full ingest (topics + facts) from an unpacked repo dir — a
   convenience/testing entry; the production path splits these into
   create-repo-topics! (sync) + start-repo-distill! (async). Pre/Post = those
   two combined; returns {:root-id :file-topics :facts}."
  [user-id repo-name root-dir]
  (let [{:keys [root-id file-topics]} (create-repo-topics! user-id repo-name root-dir)]
    {:root-id root-id
     :file-topics file-topics
     :facts (:facts (distill-repo! user-id root-id root-dir))}))
