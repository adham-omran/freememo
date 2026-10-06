(ns freememo.walkthrough-range
  "The commits, hunks and file states of one walkthrough range, read from the
   repository's clone (plans/commit-range-walkthrough.md §6.4, §6.6.3).

   Intermediate commits are never topics (§3): everything here is read from
   git and cached in memory, keyed by the clone and the sha. A commit's diff
   never changes, so an entry never goes stale."
  (:require [clojure.string :as str]
            [freememo.git-clone :as git]
            [freememo.unified-diff :as diff])
  (:import [java.io File]
           [java.nio.charset StandardCharsets]))

(def max-commits
  "Longest range a walkthrough takes (§6.4.2)."
  50)

(def noise-hunk-lines
  "A hunk that adds and removes more lines than this together is noise
   (§6.4.4): nobody studies 36,000 lines of vendored code line by line."
  1500)

(defn- failure [msg]
  (ex-info msg {:type :freememo.forge/forge-failure}))

;; ── The range ──────────────────────────────────────────────────────

(defn range-commits
  "The commits from `sha-a` (excluded) to `sha-b` on `sha-b`'s first-parent
   line, oldest first (§6.4.2).
   Pre : `dir` is a clone holding both commits.
   Post: [{:sha :parents :author :date :subject :body}], 1 to `max-commits`
         long. Throws ::forge-failure when A is not an ancestor of B, when
         they are equal, or when the range is longer than `max-commits`."
  [^File dir sha-a sha-b]
  (cond
    (not (and (git/commit-exists? dir sha-a) (git/commit-exists? dir sha-b)))
    (throw (failure "One of the two commits is not in this repository's history."))

    (= sha-a sha-b)
    (throw (failure "Choose two different commits."))

    (not (git/ancestor? dir sha-a sha-b))
    (throw (failure "The first commit must come before the second on the same branch."))

    :else
    (let [commits (git/log dir ["--reverse" "--first-parent" (str sha-a ".." sha-b)])]
      (cond
        (empty? commits)
        (throw (failure "No commit lies between these two commits on the first-parent line."))

        (> (count commits) max-commits)
        (throw (failure (str "This range holds " (count commits) " commits. A walkthrough takes at most "
                          max-commits ". Choose a later first commit.")))

        :else commits))))

(defn recent-commits
  "The newest `n` commits of `ref` in the clone, newest first, for the
   picker (§6.6.1). Post: as `git-clone/log`."
  [^File dir ref n]
  (git/log dir ["--first-parent" (str "--max-count=" n) ref]))

;; ── Diffs ──────────────────────────────────────────────────────────

(def ^:private max-cached-diffs 400)

(defonce ^:private !diffs
  ;; {[clone-path sha] parsed-file-diffs}. Emptied, not evicted, when full.
  (atom {}))

(defn commit-diff
  "The parsed diff of `commit` against its first parent (§6.4.3).
   Post: `unified-diff/parse`'s value. Cached per clone and sha."
  [^File dir {:keys [sha parents]} opts]
  (let [k [(.getAbsolutePath dir) sha]]
    (or (get @!diffs k)
      (let [parsed (diff/parse (git/diff dir (first parents) sha opts))]
        (swap! !diffs (fn [m] (assoc (if (> (count m) max-cached-diffs) {} m) k parsed)))
        parsed))))

(defn hunk-key
  "The identity of one hunk across tables and requests (§6.4.3)."
  [commit-sha old-path new-path index]
  [commit-sha old-path new-path index])

(defn range-hunks
  "Every hunk of the range, in commit order then file order.
   Post: [{:commit-sha :old-path :new-path :index :header :added :removed
           :pseudo? :lines :status}]."
  [^File dir commits opts]
  (into []
    (mapcat (fn [c]
              (mapcat (fn [{:keys [old-path new-path status hunks]}]
                        (map #(assoc (select-keys % [:index :header :added :removed :pseudo? :lines])
                                :commit-sha (:sha c) :old-path old-path :new-path new-path
                                :status status)
                          hunks))
                (commit-diff dir c opts))))
    commits))

(defn noise-keys
  "The keys of the hunks that are noise (§6.4.4): a file that its commit's
   `.gitattributes` marks vendored or generated, or a hunk past
   `noise-hunk-lines`. Post: a set of `hunk-key`s."
  [^File dir hunks opts]
  (let [flagged (into {}
                  (map (fn [[sha hs]]
                         [sha (git/attributes dir sha (distinct (keep :new-path hs)) opts)]))
                  (group-by :commit-sha hunks))]
    (into #{}
      (comp (filter (fn [{:keys [commit-sha new-path added removed]}]
                      (or (> (+ added removed) noise-hunk-lines)
                        (some #{:vendored :generated} (get-in flagged [commit-sha new-path])))))
        (map (fn [{:keys [commit-sha old-path new-path index]}]
               (hunk-key commit-sha old-path new-path index))))
      hunks)))

;; ── File states ────────────────────────────────────────────────────

(defn- text-at [^File dir sha path opts]
  (some-> (git/file-at dir sha path opts) (String. StandardCharsets/UTF_8)))

(defn file-history
  "The file diffs that made `path` as it is at commit index `upto` of
   `commits`, oldest first, following renames back.
   Post: {:base-path p-at-A-or-nil :steps [[commit file-diff] …]}."
  [^File dir commits upto path opts]
  (loop [i upto cur path steps ()]
    (if (neg? i)
      {:base-path cur :steps (vec steps)}
      (let [c (nth commits i)
            fd (some #(when (= cur (:new-path %)) %) (commit-diff dir c opts))]
        (cond
          (nil? fd) (recur (dec i) cur steps)
          (= :added (:status fd)) {:base-path nil :steps (vec (cons [c fd] steps))}
          :else (recur (dec i) (or (:old-path fd) cur) (cons [c fd] steps)))))))

(defn file-state
  "`path` as it is at commit `commit-sha` of the range, each line with the
   hunk that last wrote it (§6.6.3).

   `sha-a` is the range's start. A line unchanged since A has origin nil.
   Post: {:text :lines [{:text :origin}] :attributed? bool}. `:origin` is a
         `hunk-key`. When the replay does not reproduce the file — a diff git
         printed differently from the bytes — `:attributed?` is false and
         every line has origin nil, so the screen shows the file plainly
         rather than wrongly. nil when the file does not exist there."
  [^File dir sha-a commits commit-sha path opts]
  (let [upto (first (keep-indexed #(when (= commit-sha (:sha %2)) %1) commits))]
    (when-let [text (when upto (text-at dir commit-sha path opts))]
      (let [{:keys [base-path steps]} (file-history dir commits upto path opts)
            base (if base-path (or (text-at dir sha-a base-path opts) "") "")
            plain (mapv (fn [l] {:text l :origin nil}) (diff/lines text))]
        (try
          (let [replayed (reduce (fn [acc [c fd]]
                                   (diff/apply-hunks acc (remove :pseudo? (:hunks fd))
                                     (fn [h] (hunk-key (:sha c) (:old-path fd) (:new-path fd) (:index h)))))
                           (mapv (fn [l] {:text l :origin nil}) (diff/lines base))
                           steps)]
            (if (= text (apply str (map :text replayed)))
              {:text text :lines replayed :attributed? true}
              {:text text :lines plain :attributed? false}))
          (catch clojure.lang.ExceptionInfo e
            (if (= ::diff/mismatch (:type (ex-data e)))
              {:text text :lines plain :attributed? false}
              (throw e))))))))

(defn plan-path?
  "True for a plan file: Markdown under a `plans/` directory (§6.5.1)."
  [path]
  (boolean (and path (str/ends-with? path ".md")
             (or (str/starts-with? path "plans/") (str/includes? path "/plans/")))))

(defn work-package-labels
  "The work-package labels a commit message names, such as `WP2.2`."
  [message]
  (vec (distinct (re-seq #"\bWP\d+(?:\.\d+)*\b" (str message)))))
