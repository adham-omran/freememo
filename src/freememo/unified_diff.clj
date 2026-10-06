(ns freememo.unified-diff
  "Read `git diff-tree -p -M` output, and replay it to say which change wrote
   each line of a file (plans/commit-range-walkthrough.md §6.4.3, §6.6.3).

   Pure: no git and no database, so a test reaches every rule.

   A file's text is a vector of LINES, each with its terminator: \"a\\n\", and
   a last line without one when the file has no final newline. Concatenating
   the lines gives the text back exactly, so an offset into the text a pin
   anchors to is an offset into the lines.

   A file diff with no text hunks — a binary file, a pure rename, a mode
   change — still gets one hunk, index 0, with `:pseudo? true`, so grouping
   assigns every change of the range to a concept."
  (:require [clojure.string :as str]))

;; ── Lines ──────────────────────────────────────────────────────────

(defn lines
  "Post: the lines of `text`, each ending in \"\\n\" except perhaps the last.
   (apply str (lines t)) = t."
  [text]
  (let [text (or text "")]
    (loop [i 0 start 0 out (transient [])]
      (let [nl (str/index-of text "\n" i)]
        (if (nil? nl)
          (persistent! (if (< start (count text)) (conj! out (subs text start)) out))
          (recur (inc nl) (inc nl) (conj! out (subs text start (inc nl)))))))))

;; ── Parsing ────────────────────────────────────────────────────────

(defn- unquote-path
  "A path as git prints it: bare, or C-quoted when it holds a special byte."
  [s]
  (if (and (str/starts-with? s "\"") (str/ends-with? s "\"") (> (count s) 1))
    (let [body (subs s 1 (dec (count s)))
          out (java.io.ByteArrayOutputStream.)]
      (loop [i 0]
        (when (< i (count body))
          (let [c (.charAt body i)]
            (if (and (= \\ c) (< (inc i) (count body)))
              (let [n (.charAt body (inc i))]
                (cond
                  (Character/isDigit n)
                  (do (.write out (Integer/parseInt (subs body (inc i) (+ i 4)) 8))
                      (recur (+ i 4)))
                  :else
                  (do (.write out (int (case n \n 10 \t 9 \" 34 \\ 92 \a 7 \b 8 \f 12 \r 13 \v 11 (int n))))
                      (recur (+ i 2)))))
              (do (.write out (.getBytes (str c) "UTF-8"))
                  (recur (inc i)))))))
      (String. (.toByteArray out) "UTF-8"))
    s))

(defn- strip-prefix [path prefix]
  (when-not (= "/dev/null" path)
    (let [p (unquote-path path)]
      (if (str/starts-with? p prefix) (subs p (count prefix)) p))))

(def ^:private hunk-header-re #"^@@ -(\d+)(?:,(\d+))? \+(\d+)(?:,(\d+))? @@(.*)$")

(defn- header-path
  "The path of a `--- ` or `+++ ` line, which may end in a tab and a date."
  [line prefix]
  (strip-prefix (first (str/split (subs line 4) #"\t")) prefix))

(defn- finish-hunk [h]
  (let [ls (:lines h)]
    (assoc h
      :added (count (filter #(= \+ (first %)) ls))
      :removed (count (filter #(= \- (first %)) ls)))))

(defn- start-file
  "A file diff from its `diff --git a/x b/y` line. The two paths are a guess
   until `---`/`+++` or `rename` lines confirm them: a space in a path makes
   the header ambiguous."
  [line]
  (let [rest (subs line (count "diff --git "))
        [a b] (if (str/starts-with? rest "\"")
                (let [end (inc (str/index-of rest "\" " 1))]
                  [(subs rest 0 end) (subs rest (inc end))])
                (let [i (str/index-of rest " b/")]
                  (if i [(subs rest 0 i) (subs rest (inc i))] [rest rest])))]
    {:old-path (strip-prefix a "a/") :new-path (strip-prefix b "b/")
     :status :modified :binary? false :hunks []}))

(defn parse
  "The file diffs of patch `text`, in its order.
   Post: [{:old-path :new-path :status :binary? :hunks [...]}].
     `:status` is :added, :deleted, :renamed or :modified. A path is nil on
     the side where the file does not exist.
     A hunk is {:index :header :old-start :old-count :new-start :new-count
     :lines [\" ctx\\n\" \"-old\\n\" \"+new\\n\"] :added :removed}. A line keeps
     its terminator; a line the diff marks `\\ No newline at end of file` has
     none. A file diff with no hunks gets one `:pseudo? true` hunk."
  [text]
  (let [out (transient [])
        flush-file (fn [out file hunk]
                     (if file
                       (let [hunks (cond-> (:hunks file) hunk (conj (finish-hunk hunk)))
                             hunks (if (seq hunks)
                                     (vec (map-indexed #(assoc %2 :index %1) hunks))
                                     [{:index 0 :pseudo? true :lines [] :added 0 :removed 0
                                       :header (cond (:binary? file) "Binary file changed"
                                                     (= :renamed (:status file)) "File renamed"
                                                     (= :added (:status file)) "Empty file added"
                                                     (= :deleted (:status file)) "Empty file deleted"
                                                     :else "File mode changed")}])]
                         (conj! out (assoc file :hunks hunks)))
                       out))]
    (loop [[line & more :as ls] (str/split (or text "") #"\n" -1)
           out out file nil hunk nil]
      (cond
        (empty? ls) (persistent! (flush-file out file hunk))

        (str/starts-with? line "diff --git ")
        (recur more (flush-file out file hunk) (start-file line) nil)

        (nil? file) (recur more out file hunk)

        (and hunk (re-matches #"[ +\-].*" line))
        (recur more out file (update hunk :lines conj (str line "\n")))

        (and hunk (str/starts-with? line "\\"))
        ;; "\ No newline at end of file" ends the line before it.
        (recur more out file
          (update hunk :lines (fn [v] (let [l (peek v)]
                                        (conj (pop v) (subs l 0 (dec (count l))))))))

        (str/starts-with? line "@@ ")
        (let [[_ os oc ns nc tail] (re-matches hunk-header-re line)]
          (recur more out (cond-> file hunk (update :hunks conj (finish-hunk hunk)))
            {:header line
             :old-start (parse-long os) :old-count (if oc (parse-long oc) 1)
             :new-start (parse-long ns) :new-count (if nc (parse-long nc) 1)
             :context (str/trim (str tail))
             :lines []}))

        hunk (recur more out file hunk)

        (str/starts-with? line "new file mode") (recur more out (assoc file :status :added :old-path nil) nil)
        (str/starts-with? line "deleted file mode") (recur more out (assoc file :status :deleted :new-path nil) nil)
        (str/starts-with? line "rename from ")
        (recur more out (assoc file :status :renamed :old-path (unquote-path (subs line 12))) nil)
        (str/starts-with? line "rename to ")
        (recur more out (assoc file :status :renamed :new-path (unquote-path (subs line 10))) nil)
        (str/starts-with? line "--- ")
        (recur more out (assoc file :old-path (header-path line "a/")) nil)
        (str/starts-with? line "+++ ")
        (recur more out (assoc file :new-path (header-path line "b/")) nil)
        (str/starts-with? line "Binary files ") (recur more out (assoc file :binary? true) nil)
        :else (recur more out file hunk)))))

(defn hunk-text
  "The hunk as a reader or a model reads it: its header, then its lines,
   clipped to `max-chars`."
  [{:keys [header lines]} max-chars]
  (let [body (str header "\n" (apply str lines))]
    (if (> (count body) max-chars)
      (str (subs body 0 max-chars) "\n… (" (- (count body) max-chars) " more characters)")
      body)))

;; ── Replay ─────────────────────────────────────────────────────────

(defn apply-hunks
  "Replay the hunks of one file diff on `old`, a vector of {:text :origin}.
   Each added line gets `(origin-of hunk)`; context lines keep theirs.
   Post: the new vector. Throws ex-info ::mismatch when a context or removed
   line differs from `old`, which means the diff is not of this text."
  [old hunks origin-of]
  (loop [[h & more] hunks pos 0 out (transient [])]
    (if (nil? h)
      (persistent! (reduce conj! out (subvec old (min pos (count old)))))
      (let [start (if (zero? (:old-count h)) (:old-start h) (dec (:old-start h)))
            _ (when (or (< start pos) (> start (count old)))
                (throw (ex-info "Hunk out of order" {:type ::mismatch})))
            out (reduce conj! out (subvec old pos start))
            [pos out] (reduce (fn [[pos out] line]
                                (let [kind (first line)
                                      text (subs line 1)]
                                  (case kind
                                    \+ [pos (conj! out {:text text :origin (origin-of h)})]
                                    (\space \-)
                                    (let [o (get old pos)]
                                      (when-not (= text (:text o))
                                        (throw (ex-info "Line differs" {:type ::mismatch :at pos})))
                                      [(inc pos) (if (= \space kind) (conj! out o) out)]))))
                        [start out] (:lines h))]
        (recur more pos out)))))
