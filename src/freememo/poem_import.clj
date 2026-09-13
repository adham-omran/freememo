(ns freememo.poem-import
  "Read, validate and import a `freememo-poem/1` package.

   `plans/poem-package-format.md` is the contract and `plans/poem-reader-spec.md`
   rows 2.1 to 2.3 are the work. A package is one zip holding four JSON files at
   its root; an import writes one `topics` row of kind `poem` and one
   `poem_packages` row, in one transaction.

   **Validation runs at STAGE time, and again at confirm.** A staged package is
   summarized for the user before they commit to it, so the rules have to hold
   before the summary can be shown. Confirm re-reads and re-validates rather than
   trusting the staged summary: `import-staging` hands back bytes, and a rule that
   only ran on the path that produced the summary is a rule a second caller can
   miss.

   **A rejection names its rule.** V1 to V8 are numbered in the format doc, so
   \"V5: note n0042 …\" points at a statement the package's author can read,
   rather than at a symptom. That is the whole reason the numbers exist.

   **The four documents are stored verbatim.** Nothing here reshapes a verse or a
   note. `topics.content` gets a rendered fallback so search and
   `content_preview` work (format doc §7); the reader ignores it and renders from
   `poem_packages`.

   NEVER evaluates anything it reads, and makes no network request. The zip is
   read entry by entry with a per-entry byte cap; nothing is written to disk."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.import-staging :as staging]
            [freememo.input-check :as input]
            [freememo.priority :as priority]
            [freememo.text :as text]
            [next.jdbc :as jdbc]
            [taoensso.telemere :as tel])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream File FileInputStream
            InputStream]
           [java.util.zip ZipEntry ZipInputStream]))

;; ── Reading the zip ────────────────────────────────────────────────

(def package-entries
  "The four root entries a package carries, in the order a report names them.
   `freememo.content-type/poem-archive?` routes on this same set of names; the
   two are one fact, and a package that routed here without one of them is a
   routing bug rather than bad input."
  ["manifest.json" "verses.json" "notes.json" "sources.json"])

(def ^:private max-entry-bytes
  "Cap on one entry, so a zip bomb fails at this many bytes rather than filling
   the heap. 32 MB is far past any real package — معلقة امرئ القيس is 82 verses
   and its notes are a few hundred KB — and far below the JVM heap a rejection
   has to survive on."
  (* 32 1024 1024))

(defn- read-capped
  "Drain `zin`'s current entry into a String, or nil past `max-entry-bytes`.
   Reading into memory is deliberate: the whole package is small by construction,
   and writing it to disk would give this namespace a temp directory to own."
  [^ZipInputStream zin]
  (let [out (ByteArrayOutputStream.)
        buf (byte-array 65536)]
    (loop [total 0]
      (let [n (.read zin buf 0 (alength buf))]
        (cond
          (neg? n) (String. (.toByteArray out) "UTF-8")
          (> (+ total n) max-entry-bytes) nil
          :else (do (.write out buf 0 n) (recur (+ total n))))))))

(defn- read-entries
  "The four package entries of `src`, as raw strings.

   Pre : `src` is a byte[] or a File holding a ZIP.
   Post: a map of entry name to its UTF-8 content, holding only names in
         `package-entries` and only those the zip carried. Other entries are
         skipped without being read, per format doc §1.
   Invariant: never throws. An unreadable zip yields `{}`, which V1 reports."
  [src]
  (let [wanted (set package-entries)]
    (try
      (with-open [in ^InputStream (if (bytes? src)
                                    (ByteArrayInputStream. ^bytes src)
                                    (FileInputStream. ^File src))
                  zin (ZipInputStream. in)]
        (loop [acc {}]
          (if-let [^ZipEntry e (.getNextEntry zin)]
            (let [nm (.getName e)]
              (if (and (contains? wanted nm) (not (.isDirectory e))
                    (not (contains? acc nm)))
                (recur (if-let [s (read-capped zin)] (assoc acc nm s) acc))
                (recur acc)))
            acc)))
      (catch Exception _ {}))))

;; ── Validation (format doc §6) ─────────────────────────────────────

(def note-types
  "The five note types (format doc §4). A type outside this set makes `word` and
   `verse` unanswerable, so V5 is where an unknown one is reported."
  #{"gloss" "iraab" "meaning" "commentary" "meter"})

(def ^:private word-keyed-types
  "Types whose note addresses one word. The other three address the verse, or —
   for `meter` — the poem."
  #{"gloss" "iraab"})

(def min-diacritic-ratio
  "V6's floor. Measured inputs: التبريزي's 82 verses run 63.8% to 100%
   (`plans/poem-reader-spec.md` §5.2), so 0.60 admits every verse of the chosen
   list and refuses the 0.4% text الدرة prints."
  0.60)

(defn diacritic-ratio
  "V6's ratio for one verse's text.

   Pre : `s` is a string, possibly nil.
   Post: marks (U+064B to U+0652) divided by letters (U+0621 to U+064A), as a
         double. 0.0 when the text carries no letter at all — a verse with
         nothing to vocalize fails V6 rather than dividing by zero."
  [s]
  (let [cs (or s "")
        letters (count (filter #(<= 0x0621 (int %) 0x064A) cs))
        marks (count (filter #(<= 0x064B (int %) 0x0652) cs))]
    (if (zero? letters) 0.0 (/ (double marks) (double letters)))))

(defn- half-word-counts
  "[sadr-count ajuz-count] over one verse's `:words`."
  [verse]
  (let [ws (:words verse)]
    [(count (filter #(= "sadr" (:half %)) ws))
     (count (filter #(= "ajuz" (:half %)) ws))]))

(defn- check-v1
  "V1 — the four entries exist at the zip root and parse as JSON.
   Post: {:ok true :package {…}} or {:ok false :error S}. Parsing is part of V1,
   so a truncated file is reported as V1 rather than as a later rule's confusion."
  [entries]
  (let [missing (remove #(contains? entries %) package-entries)]
    (if (seq missing)
      {:ok false :error (str "V1: the package is missing " (str/join ", " missing)
                          " at the zip root.")}
      (let [parsed (reduce (fn [acc nm]
                             (if (:error acc)
                               acc
                               (try
                                 (assoc acc (keyword (subs nm 0 (- (count nm) 5)))
                                   (json/parse-string (get entries nm) true))
                                 (catch Exception e
                                   (assoc acc :error
                                     (str "V1: " nm " is not valid JSON ("
                                       (.getMessage e) ")."))))))
                     {} package-entries)]
        (cond
          (:error parsed) {:ok false :error (:error parsed)}
          (not (map? (:manifest parsed)))
          {:ok false :error "V1: manifest.json must hold one JSON object."}
          (not-every? sequential? [(:verses parsed) (:notes parsed) (:sources parsed)])
          {:ok false :error (str "V1: verses.json, notes.json and sources.json"
                              " must each hold a JSON array.")}
          :else {:ok true :package (select-keys parsed [:manifest :verses :notes :sources])})))))

(defn- check-v2 [{:keys [manifest]}]
  (let [f (:format manifest)]
    (when (not= "freememo-poem/1" f)
      (str "V2: manifest format is " (pr-str f) ", expected \"freememo-poem/1\"."))))

(defn- check-v3 [{:keys [manifest verses]}]
  (let [declared (:verse_count manifest)
        ns* (mapv :n verses)]
    (or
      (when-not (and (integer? declared) (pos? declared))
        (str "V3: manifest verse_count is " (pr-str declared)
          ", expected a positive integer."))
      (when (not= ns* (vec (range 1 (inc declared))))
        (str "V3: verse numbers must run 1 to " declared
          " with no gap; verses.json holds " (count verses) " entries"
          (when-let [bad (first (remove nil? (map (fn [n i] (when (not= n i) n))
                                               ns* (range 1 (inc (count ns*))))))]
            (str " and reaches " bad " out of order"))
          "."))
      (when-let [bad (first (filter (fn [v]
                                      (let [[s a] (half-word-counts v)]
                                        (or (zero? s) (zero? a))))
                              verses))]
        (let [[s a] (half-word-counts bad)]
          (str "V3: verse " (:n bad) " has " s " sadr word(s) and " a
            " ajuz word(s); each half needs at least one."))))))

(defn- check-v4 [{:keys [notes sources]}]
  (let [ids (set (map :id sources))]
    (or
      (when-let [bad (first (remove #(contains? ids (:source_id %)) notes))]
        (str "V4: note " (:id bad) " names source_id " (pr-str (:source_id bad))
          ", which is not in sources.json."))
      (when-let [bad (first (filter #(or (nil? (:page_id %)) (nil? (:printed_page %)))
                             notes))]
        (str "V4: note " (:id bad) " has a null page_id or printed_page;"
          " both must be present.")))))

(defn- check-v5 [{:keys [verses notes]}]
  (let [by-n (into {} (map (juxt :n identity)) verses)]
    (or
      (when-let [bad (first (remove #(contains? note-types (:type %)) notes))]
        (str "V5: note " (:id bad) " has type " (pr-str (:type bad))
          ", which is not one of " (str/join ", " (sort note-types)) "."))
      (when-let [bad (first (filter (fn [{:keys [verse type]}]
                                      (if (nil? verse)
                                        (not= "meter" type)
                                        (not (contains? by-n verse))))
                              notes))]
        (str "V5: note " (:id bad) " names verse " (pr-str (:verse bad))
          (if (nil? (:verse bad))
            " — only a meter note may have a null verse."
            ", which is not a verse in verses.json.")))
      (when-let [bad (first (filter (fn [{:keys [verse word type]}]
                                      (if (contains? word-keyed-types type)
                                        (not (contains?
                                               (set (map :i (:words (get by-n verse))))
                                               word))
                                        (some? word)))
                              notes))]
        (str "V5: note " (:id bad) " of type " (:type bad) " has word "
          (pr-str (:word bad))
          (if (contains? word-keyed-types (:type bad))
            (str ", which is not a word index of verse " (:verse bad) ".")
            " — only a gloss or iraab note carries a word."))))))

(defn- check-v6 [{:keys [verses]}]
  (when-let [bad (first (filter #(< (diacritic-ratio (str (:sadr %) (:ajuz %)))
                                   min-diacritic-ratio)
                          verses))]
    (str "V6: verse " (:n bad) " has a diacritic ratio of "
      (format "%.3f" (diacritic-ratio (str (:sadr bad) (:ajuz bad))))
      ", below the " min-diacritic-ratio " floor.")))

(defn- check-v7 [{:keys [notes]}]
  (when-let [bad (first (filter (fn [{:keys [html]}]
                                  (let [h (str html)]
                                    (or (str/includes? h "<img")
                                      (str/includes? h "/api/media/"))))
                          notes))]
    (str "V7: note " (:id bad) " carries an <img tag or an /api/media/ URL;"
      " a quoted note holds neither.")))

(defn- check-v8 [{:keys [notes]}]
  (let [ids (map :id notes)
        dup (first (for [[id n] (frequencies ids) :when (> n 1)] id))]
    (or
      (when-let [bad (first (filter #(or (nil? (:id %)) (str/blank? (str (:id %)))) notes))]
        (str "V8: a note has no id (" (pr-str (select-keys bad [:verse :type])) ")."))
      (when dup (str "V8: note id " (pr-str dup) " appears more than once.")))))

(defn validate
  "Apply V1 to V8 to a parsed package.

   Pre : `package` is `check-v1`'s `:package`.
   Post: nil when every rule holds, otherwise the message of the FIRST rule that
         fails, in V2-to-V8 order. One message rather than a list: the rules build
         on each other, so a V3 failure makes every later report noise.
   Invariant: pure, and never throws on package content."
  [package]
  (some #(% package) [check-v2 check-v3 check-v4 check-v5 check-v6 check-v7 check-v8]))

(defn read-package
  "Read and validate the package in `src`.

   Pre : `src` is a byte[] or a File holding a ZIP that `content-type/poem-archive?`
         accepted.
   Post: `{:ok true :package {:manifest :verses :notes :sources}}`, or
         `{:ok false :error S}` where S names the rule that refused it.
   Invariant: never throws — a bad package is routine input, not a caller bug."
  [src]
  (try
    (let [r (check-v1 (read-entries src))]
      (if-not (:ok r)
        r
        (if-let [err (validate (:package r))]
          {:ok false :error err}
          r)))
    (catch Exception e
      (tel/error! {:id ::read-package-failed} e)
      {:ok false :error "This poem package could not be read."})))

;; ── What the user is shown before they confirm ─────────────────────

(defn summary
  "The staged summary the Import modal shows (spec row 2.5).

   Pre : `package` has passed `validate`.
   Post: `{:title :poet :meter :verse-count :note-count :sources [S …]}` — plain
         JSON-safe values, because this crosses to the client as the upload
         response body. `:sources` holds each source's chip text, which is what a
         reader will see beside a note."
  [{:keys [manifest verses notes sources]}]
  {:title (str (:title manifest))
   :poet (str (:poet manifest))
   :meter (str (:meter manifest))
   :verse-count (count verses)
   :note-count (count notes)
   :sources (mapv #(str (or (:short %) (:title %))) sources)})

;; ── The rendered fallback (format doc §7) ──────────────────────────

(defn- escape-html
  "The four characters that can break out of text content. `\"` is included
   because the rendered paragraph carries an attribute."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")))

(defn render-content
  "The poem as HTML, one `<p dir=\"rtl\">` per verse.

   Pre : `verses` is the validated verse vector.
   Post: a string of `<p dir=\"rtl\">sadr   ajuz</p>` paragraphs, three spaces
         between the halves (format doc §7).

   This exists so `topics.content_text` and the generated `content_preview` hold
   the poem's words — search reads those columns, and a poem whose text lives
   only in a JSONB column would be unfindable. `dir=\"rtl\"` survives
   `clean-html`, which keeps `dir` on every i18n tag (spec §6)."
  [verses]
  (str/join "\n"
    (map (fn [{:keys [sadr ajuz]}]
           (str "<p dir=\"rtl\">" (escape-html sadr) "   " (escape-html ajuz) "</p>"))
      verses)))

(defn- package-title
  "The topic title, clamped to `input/title-max`.
   A manifest with no title would otherwise insert a blank row; the fallback names
   the kind so the row is still findable in the Library."
  [manifest]
  (let [t (str/trim (str (:title manifest)))
        t (if (str/blank? t) "Poem" t)]
    (if (> (count t) input/title-max) (subs t 0 input/title-max) t)))

;; ── Import ─────────────────────────────────────────────────────────

(defn import-package!
  "Write a validated package as one poem topic.

   Pre : `package` has passed `read-package`. `user-id` owns the import.
   Post: `{:ok true :topic-id N :summary {…}}`, or `{:ok false :error S}`. On
         failure NOTHING persists: the topic row and the package row are written
         inside one transaction, so a package-row failure takes the topic with it
         (spec row 2.2).
   Invariant: writes no media and no source row, so there is nothing outside the
         transaction to compensate — unlike the Sphinx import, whose images are
         written before it opens."
  [user-id package]
  (try
    (let [manifest (:manifest package)
          content (render-content (:verses package))]
      (jdbc/with-transaction [tx db/ds]
        (let [topic-id (first (db/insert-topic-rows! tx
                                [{:user_id user-id
                                  :kind "poem"
                                  :title (package-title manifest)
                                  :status "active"
                                  :priority priority/priority-default
                                  :content content
                                  :content_text (text/strip-html content)}]))]
          (db/insert-poem-package! tx user-id topic-id package)
          {:ok true :topic-id topic-id :summary (summary package)})))
    (catch Exception e
      (tel/error! {:id ::import-failed :data {:user-id user-id}} e)
      {:ok false :error (or (.getMessage e) "Import failed")})))

(defn confirm-poem-upload!*
  "Finalize a staged poem package into a topic.

   Pre  : user-id owns the staged upload; upload-id is unclaimed; the entry's
          flow is :poem.
   Post : `{:ok true :topic-id N :summary {…}}` or `{:ok false :error S}`.
   Invariant: claim is one-shot — a replay returns {:ok false}. The package is
          read and validated AGAIN here rather than trusted from stage time, so
          this path holds V1 to V8 on its own."
  [user-id upload-id]
  (try
    (if-let [entry (staging/claim! user-id upload-id)]
      (let [{:keys [^bytes bytes flow]} entry]
        (if (not= :poem flow)
          {:ok false :error (str "Not a poem package: " flow)}
          (let [r (read-package bytes)]
            (if-not (:ok r)
              r
              (let [res (import-package! user-id (:package r))]
                (when (:ok res) (commands/bump! user-id :import-document))
                res)))))
      {:ok false :error "Upload not found or expired"})
    (catch Exception e
      (tel/error! {:id ::confirm-poem-upload-failed :data {:user-id user-id}} e)
      {:ok false :error "Import failed"})))

(defn confirm-poem-session!*
  "Import a poem package from a completed chunked upload session.

   The session sibling of `confirm-poem-upload!*`. A poem package is small enough
   that the Poem card uploads it multipart, so this path serves only a package
   that reached the archive route another way — a `.zip` dropped on the intake
   area or on the Code card, which `content-type/classify-archive` resolves to
   `:poem` at finalize.

   Pre  : user-id owns session-id; the session is complete and holds a poem
          package.
   Post : `{:ok true :topic-id N :summary {…}}` or `{:ok false :error S}`. On
          SUCCESS the session is released — the archive was transport, and the
          package it carried now lives in `poem_packages`.
   Invariant: on failure the session SURVIVES, so a retry needs no re-upload."
  [user-id session-id]
  (try
    (let [m (db/materialize-upload-archive! user-id session-id)]
      (if-not (:ok m)
        m
        (let [r (read-package (:file m))]
          (if-not (:ok r)
            r
            (let [res (import-package! user-id (:package r))]
              (when (:ok res)
                (commands/bump! user-id :import-document)
                (db/release-upload-session! user-id session-id))
              res)))))
    (catch Exception e
      (tel/error! {:id ::confirm-poem-session-failed :data {:user-id user-id}} e)
      {:ok false :error "Import failed"})))
