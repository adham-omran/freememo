(ns freememo.walkthrough
  "Commit-range walkthroughs: preparing one, reading its steps and canvas,
   fixing its grouping, and what the tutor is told inside one
   (plans/commit-range-walkthrough.md §6.4–§6.7).

   Preparation is long work and joins the Activity monitor (§6.5.4):
   `:preparing-walkthroughs` maps a walkthrough id to its phase while it runs,
   `:walkthrough-errors` keeps a failure for the reporter, and cancelling is
   cooperative (§6.5.5). The canceller marks the walkthrough `failed` in the
   database at once, and `db/save-walkthrough-grouping!` writes only into a
   walkthrough that is still `preparing`, so a grouping that finishes after a
   cancel lands nowhere.

   The tutor sees a walkthrough through FOCUS: the walkthrough screen tells
   the server which step and file it shows (`set-focus!`), and a run about
   that repository node reads it (`request-tail`, `focused-ref`). The focus
   lives in this process only; the last screen to report wins.

   Every write ends in `commands/bump!`, so `:walkthrough-mutations` is bumped
   only through the command registry."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [freememo.activity :as activity]
            [freememo.assistant-evidence-excerpt :as evidence-excerpt]
            [freememo.commands :as commands]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.document-ref :as docref]
            [freememo.git-clone :as git]
            [freememo.kg-llm :as llm]
            [freememo.question-map :as question-map]
            [freememo.question-map-block :as block]
            [freememo.repo-import :as repo-import]
            [freememo.repo-markdown :as repo-md]
            [freememo.repo-tree :as tree]
            [freememo.text :as text]
            [freememo.unified-diff :as diff]
            [freememo.user-state :as us]
            [freememo.walkthrough-grouping :as grouping]
            [freememo.walkthrough-name :as wname]
            [freememo.walkthrough-range :as wr]
            [taoensso.telemere :as tel]))

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

(defn- failure-message [^Throwable t]
  (if (= :freememo.forge/forge-failure (:type (ex-data t)))
    (.getMessage t)
    (do (tel/error! {:id ::walkthrough-failed} t)
        (or (some-> t ex-message not-empty) "The walkthrough failed. The server log has the details."))))

;; ── History and start ──────────────────────────────────────────────

(def ^:private picker-commits
  "Commits the picker lists. Its picklists search and scroll, so a long
   history costs one read."
  500)

(defn history
  "The newest commits of `user-id`'s repository node `repo-id`, for picking
   A and B (§6.6.1). The clone is fetched first, so the list is current.
   Post: {:ok true :commits [{:sha :subject :author :date}]} newest first, or
         {:error S}."
  [user-id repo-id]
  (try
    (if-let [{:keys [ref]} (db/get-repo-import user-id repo-id)]
      (let [{:keys [dir]} (repo-import/clone-context user-id repo-id true)]
        {:ok true :ref ref
         :commits (mapv #(select-keys % [:sha :subject :author :date])
                    (wr/recent-commits dir (:sha (git/resolve-ref dir ref "the repository"))
                      picker-commits))})
      {:error "This topic is not an imported repository."})
    (catch Throwable t {:error (failure-message t)})))

(declare prepare!)

(defn start!
  "Start a walkthrough of `user-id`'s repository node that teaches the commits
   from `from-sha` to `to-sha`, both included, and prepare it on a server
   thread (§6.4.2, §6.5). The walkthrough's A is `from-sha`'s first parent.
   Post: {:ok true :walkthrough-id N} once the row exists, or {:error S} when
         `walkthrough-range/taught-range` refuses the range, and then nothing
         is written."
  [user-id repo-id from-sha to-sha]
  (try
    (if-let [{:keys [dir]} (repo-import/clone-context user-id repo-id)]
      (let [{:keys [sha-a commits]} (wr/taught-range dir from-sha to-sha)]
        (if-let [wid (db/create-walkthrough! user-id repo-id sha-a to-sha commits)]
          (do (future (prepare! user-id wid repo-id))
              (commands/bump! user-id :start-walkthrough)
              {:ok true :walkthrough-id wid})
          {:error "This topic is not an imported repository."}))
      {:error "This topic is not an imported repository."})
    (catch Throwable t {:error (failure-message t)})))

;; ── Preparation ────────────────────────────────────────────────────

(defn- cancelled? [user-id wid]
  (contains? @(us/get-atom user-id :walkthrough-cancelled) wid))

(defn- check-cancelled! [user-id wid]
  (when (cancelled? user-id wid)
    (throw (ex-info "Walkthrough preparation cancelled" {:type ::cancelled}))))

(defn- set-phase! [user-id wid phase]
  (swap! (us/get-atom user-id :preparing-walkthroughs) assoc wid phase))

(defn- bill-pass!
  "One ledger row per pass (§6.5.6)."
  [user-id model-id pass cost tokens]
  (when (and cost (pos? cost))
    (credits/record-cost-charge! user-id
      (case pass :concepts :walkthrough.concepts :classify :walkthrough.classify)
      model-id cost tokens)))

(defn- group-range!
  "Read the range's hunks and group them. Post: `grouping/group!`'s value."
  [user-id wid repo-id]
  (let [ctx (assoc (repo-import/clone-context user-id repo-id) :cancelled? #(cancelled? user-id wid))
        {:keys [commits]} (db/get-walkthrough user-id wid)
        _ (set-phase! user-id wid "reading diffs")
        hunks (wr/range-hunks (:dir ctx) commits ctx)
        noise (wr/noise-keys (:dir ctx) hunks ctx)
        _ (check-cancelled! user-id wid)
        ;; The extraction step's model: grouping reads commits as extraction
        ;; reads documents, and a step of its own would be a Settings row
        ;; for one feature.
        {:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :extract)]
    (grouping/group!
      {:chat (fn [system user root-key root-type]
               (llm/chat! api-key model-fields system user root-key root-type
                 {:feature :walkthrough :user-id user-id}))
       :commits commits :hunks hunks :noise noise
       :check-cancelled! #(check-cancelled! user-id wid)
       :set-phase! #(set-phase! user-id wid %)
       :on-pass (fn [pass cost tokens] (bill-pass! user-id (:id entry) pass cost tokens))})))

(defn prepare!
  "Prepare walkthrough `wid` while it is published in
   `:preparing-walkthroughs` (§6.5.4).
   Post: the walkthrough is `ready` with its grouping, or `failed` with a
         message; a cancelled one reads \"Cancelled.\" and holds no hunk.
   Invariant: membership, the canceller and the mark are removed on every
         path, and a failure is recorded in `:walkthrough-errors` before
         membership drops, so the reporter closes the row as failed."
  [user-id wid repo-id]
  (let [errors (us/get-atom user-id :walkthrough-errors)
        cancellers (us/get-atom user-id :walkthrough-cancellers)
        marks (us/get-atom user-id :walkthrough-cancelled)]
    (swap! marks disj wid)
    (set-phase! user-id wid "starting")
    (swap! cancellers assoc wid (fn []
                                  (swap! marks conj wid)
                                  (db/set-walkthrough-failed! user-id wid "Cancelled.")
                                  (commands/bump! user-id :cancel-walkthrough)))
    (us/work-started! user-id)
    (try
      (let [{:keys [concepts hunks]} (group-range! user-id wid repo-id)]
        (check-cancelled! user-id wid)
        (set-phase! user-id wid "saving")
        (when (db/save-walkthrough-grouping! user-id wid concepts hunks)
          (commands/bump! user-id :prepare-walkthrough))
        (swap! errors dissoc wid))
      (catch Throwable t
        (if (cancelled? user-id wid)
          (do (swap! errors dissoc wid)
              (db/set-walkthrough-failed! user-id wid "Cancelled."))
          (let [msg (failure-message t)]
            (swap! errors assoc wid msg)
            (db/set-walkthrough-failed! user-id wid msg)))
        (commands/bump! user-id :prepare-walkthrough))
      (finally
        (swap! (us/get-atom user-id :preparing-walkthroughs) dissoc wid)
        (swap! cancellers dissoc wid)
        (swap! marks disj wid)))))

(defn restart!
  "Prepare a failed walkthrough again, from the same range. A failure costs
   the reader nothing to retry: no grouping was stored.
   Post: {:ok true} once the preparation runs, or {:error S} when the
         walkthrough is not the user's or is not failed."
  [user-id wid]
  (if-let [repo-id (db/restart-walkthrough! user-id wid)]
    (do (future (prepare! user-id wid repo-id))
        (commands/bump! user-id :restart-walkthrough)
        {:ok true})
    {:error "Only a walkthrough that failed can be prepared again."}))

(defn rename!
  "Give `user-id`'s walkthrough `wid` the name `typed`, or clear it when
   `typed` is blank (plans/repository-home.md §4.3.3). The one writer for the
   home row and the screen title.
   Post: {:ok true}, or {:error S} for a name that is too long or a
         walkthrough that is not the user's."
  [user-id wid typed]
  (let [{:keys [name error]} (wname/normalize-name typed)]
    (cond
      error {:error error}
      (db/rename-walkthrough! user-id wid name)
      (do (commands/bump! user-id :rename-walkthrough) {:ok true})
      :else {:error "That walkthrough could not be renamed."})))

(defn delete!
  "Delete `user-id`'s walkthrough `wid` (plans/repository-home.md §4.2.2.7).
   A preparation still running would write into the deleted walkthrough, so
   it must be cancelled first.
   Post: {:ok true}, or {:error S}."
  [user-id wid]
  (let [w (db/get-walkthrough user-id wid)]
    (cond
      (nil? w) {:error "That walkthrough could not be deleted."}
      (= "preparing" (:status w)) {:error "Cancel the preparation before deleting the walkthrough."}
      (pos? (db/delete-walkthrough! user-id wid))
      (do (commands/bump! user-id :delete-walkthrough) {:ok true})
      :else {:error "That walkthrough could not be deleted."})))

(defn cancel!
  "Abandon the preparation of `wid` (§6.5.5). Post: `activity/cancel-work!`'s
   result."
  [user-id wid]
  (activity/cancel-work! user-id "walkthrough-prepare" (str wid)))

(defn phase-of
  "The phase of `wid`'s preparation in `phases`, the value of
   `:preparing-walkthroughs`, or nil."
  [phases wid]
  (get phases wid))

;; ── Reading ────────────────────────────────────────────────────────

(defn overview
  "The step bar's data for `user-id`'s walkthrough (§6.6.2).
   Post: nil, or {:id :name :display-name :repo-id :repo-title :sha-a :sha-b
         :sha-from :status :error :step :commit-count :concepts [{:id :position
         :label :summary :noise :understood? :hunk-count}]}. `:name` is the
         reader's own, or nil."
  [user-id wid]
  (when-let [w (db/get-walkthrough user-id wid)]
    (let [concepts (mapv (fn [c] {:id (:id c) :position (:position c) :label (:label c)
                                  :summary (:summary c) :noise (:noise c)
                                  :understood? (some? (:understood_at c))
                                  :hunk-count (:hunk_count c)})
                     (db/walkthrough-concepts user-id wid))
          sha-from (:sha (first (:commits w)))]
      {:id wid :name (:name w)
       :display-name (wname/display-name (:name w)
                       (map (fn [c] {:label (:label c) :noise? (:noise c)}) concepts)
                       sha-from (:sha_b w))
       :repo-id (:repo_topic_id w) :repo-title (:repo_title w)
       :sha-a (:sha_a w) :sha-b (:sha_b w) :sha-from sha-from
       :status (:status w) :error (:error w)
       :step (:step w) :commit-count (count (:commits w))
       :concepts concepts})))

(defn- progress-text [{:keys [status steps]}]
  (case status
    "preparing" "Preparing…"
    "failed" "Failed"
    (str (count (filter :understood? steps)) " / " (count steps))))

(defn- progressed-text
  "`Oct 6, 14:23`, in the server's zone, as `util/format-timestamp` reads time."
  [ts]
  (when ts
    (.format (java.time.LocalDateTime/ofInstant (.toInstant ts) (java.time.ZoneId/systemDefault))
      (java.time.format.DateTimeFormatter/ofPattern "MMM d, HH:mm" java.util.Locale/ENGLISH))))

(defn list-for-repo
  "`user-id`'s walkthroughs of repository node `repo-id`, as the repository
   home's table shows them, the most recently progressed first
   (plans/repository-home.md §4.2.2.2).
   Post: [{:id :name :display-name :range :progress :progressed :status}].
         `:name` is the reader's own, or nil."
  [user-id repo-id]
  (mapv (fn [w]
          {:id (:id w) :name (:name w) :status (:status w)
           :display-name (wname/display-name (:name w) (:steps w) (:sha_from w) (:sha_b w))
           :range (str (short-sha (:sha_from w)) " → " (short-sha (:sha_b w)) ", "
                    (:commit_count w) (if (= 1 (:commit_count w)) " commit" " commits"))
           :progress (progress-text w)
           :progressed (progressed-text (:updated_at w))})
    (db/list-walkthroughs user-id repo-id)))

(defn- hunk-key-of [{:keys [commit_sha old_path new_path hunk_index]}]
  (wr/hunk-key commit_sha old_path new_path hunk_index))

(defn- step-context
  "Everything one step's reads need. Post: nil, or {:w :commits :concepts
   :hunks :concept :position-of {hunk-key position}}."
  [user-id wid position]
  (when-let [w (db/get-walkthrough user-id wid)]
    (let [concepts (db/walkthrough-concepts user-id wid)
          position-of-concept (into {} (map (juxt :id :position)) concepts)
          hunks (db/walkthrough-hunks user-id wid)]
      {:w w :commits (:commits w) :concepts concepts :hunks hunks
       :concept (some #(when (= position (:position %)) %) concepts)
       :position-of (into {} (map (juxt hunk-key-of (comp position-of-concept :concept_id))) hunks)})))

(defn- file-of
  "The path a hunk row's file has after its commit, or before it when the
   commit deletes it."
  [{:keys [new_path old_path]}]
  (or new_path old_path))

(defn step-files
  "The files the concept at `position` touches, the main document first
   (§6.6.3, §6.6.4). The main document is a Markdown file when the concept
   touches one, else the file with the most changed lines.
   Post: [{:path :sha :deleted? :markdown? :added :removed :hunks [{:id :header
           :added :removed :commit-sha :subject}]}]. `:sha` is the commit the
         canvas shows the file at: the last commit of the range in which this
         concept changes it, or that commit's parent when it deletes it."
  [user-id wid position]
  (when-let [{:keys [commits concept hunks]} (step-context user-id wid position)]
    (when concept
      (let [index-of (into {} (map-indexed (fn [i c] [(:sha c) i])) commits)
            subject-of (into {} (map (juxt :sha :subject)) commits)
            mine (filter #(= (:id concept) (:concept_id %)) hunks)
            files (for [[path hs] (group-by file-of mine)]
                    (let [last-hunk (apply max-key #(index-of (:commit_sha %) -1) hs)
                          deleted? (nil? (:new_path last-hunk))
                          commit (some #(when (= (:commit_sha last-hunk) (:sha %)) %) commits)]
                      {:path path
                       :sha (if deleted? (first (:parents commit)) (:commit_sha last-hunk))
                       :deleted? deleted?
                       :markdown? (= "markdown" (tree/language-for path))
                       :added (reduce + (map :added hs)) :removed (reduce + (map :removed hs))
                       :hunks (mapv (fn [h] {:id (:id h) :header (:header h)
                                             :added (:added h) :removed (:removed h)
                                             :commit-sha (:commit_sha h)
                                             :subject (subject-of (:commit_sha h))})
                                (sort-by (juxt #(index-of (:commit_sha %) -1) :hunk_index) hs))}))]
        (vec (sort-by (juxt #(if (:markdown? %) 0 1) #(- (+ (:added %) (:removed %))) :path) files))))))

;; ── The canvas ─────────────────────────────────────────────────────

(def fold-threshold-lines
  "A code file longer than this folds its unchanged runs (§6.6.3)."
  150)

(def ^:private fold-min-run 12)
(def ^:private fold-context 3)

(defn line-class
  "The canvas class of a line written by the concept at `origin-position`,
   seen from step `position` (§6.6.2, §6.6.3).
   \"Since previous step\" (:previous) colors this step's changes and
   outlines every other concept's. \"Since A\" (:start) also colors the
   changes of earlier steps. A line unchanged since A has no class: gray."
  [origin-position position mode]
  (cond
    (nil? origin-position) nil
    (= origin-position position) "wt-c"
    (and (= :start mode) (< origin-position position)) "wt-e"
    :else "wt-o"))

(defn- strongest [classes]
  (some (set classes) ["wt-c" "wt-e" "wt-o"]))

(defn fold-runs
  "The runs of `classes`, a vector of line classes, that fold: unchanged runs
   of at least `fold-min-run` lines, less `fold-context` lines on each side
   that touches a change. Post: [[start end-exclusive] …]."
  [classes]
  (let [n (count classes)]
    (if (<= n fold-threshold-lines)
      []
      (loop [i 0 out []]
        (if (>= i n)
          out
          (if (some? (nth classes i))
            (recur (inc i) out)
            (let [end (or (first (filter #(some? (nth classes %)) (range i n))) n)
                  s (if (zero? i) i (+ i fold-context))
                  e (if (= end n) end (- end fold-context))]
              (recur end (if (>= (- e s) fold-min-run) (conj out [s e]) out)))))))))

(defn- code-html
  "Code canvas HTML: one span per line, its newline outside the span, so the
   text nodes concatenate to the source exactly (§6.3.7). A folded run keeps
   its lines inside a hidden body, so folding moves no offset."
  [lines classes]
  (let [folds (into {} (map (fn [[s e]] [s e])) (fold-runs classes))
        line-html (fn [i]
                    (let [t (:text (nth lines i))
                          nl? (str/ends-with? t "\n")
                          body (if nl? (subs t 0 (dec (count t))) t)]
                      (str "<span class=\"wt-line" (when-let [c (nth classes i)] (str " " c)) "\">"
                        (text/escape-html body) "</span>" (when nl? "\n"))))]
    (str "<pre class=\"wt-code\">"
      (loop [i 0 out (StringBuilder.)]
        (if (>= i (count lines))
          (str out)
          (if-let [e (folds i)]
            (recur e (doto out
                       (.append (str "<span class=\"wt-fold\" data-label=\"⋯ " (- e i)
                                  " unchanged lines\"><span class=\"wt-fold__body\">"))
                       (.append (apply str (map line-html (range i e))))
                       (.append "</span></span>")))
            (recur (inc i) (.append out (line-html i))))))
      "</pre>")))

(defn- markdown-html
  "Rendered Markdown with each top-level block marked by the strongest class
   among its source lines (§6.6.3). When the source lines cannot be matched
   to the rendered blocks, nothing is marked and the document still shows."
  [source path classes]
  (let [html (repo-md/render source path (constantly {}) nil)
        ranges (repo-md/top-level-block-lines source)
        [body elements] (repo-md/top-level-elements html)]
    (if (= (count ranges) (count elements))
      (do (doseq [[el [s e]] (map vector elements ranges)]
            (when-let [c (strongest (subvec classes (min s (count classes)) (min (inc e) (count classes))))]
              (.attr ^org.jsoup.nodes.Element el "data-wt" ^String c)))
          (.html ^org.jsoup.nodes.Element body))
      html)))

(defn canvas
  "The canvas of the file at `path` in the step at `position`, colored for
   `mode` (:previous or :start) (§6.6.3).
   Post: nil, or {:path :sha :markdown? :html :attributed? :ref-spec}.
         `:ref-spec` names the file at that commit for pins (§6.6.6)."
  [user-id wid position path mode]
  (when-let [{:keys [w commits position-of]} (step-context user-id wid position)]
    (when-let [{:keys [sha deleted?]} (some #(when (= path (:path %)) %) (step-files user-id wid position))]
      (let [repo-id (:repo_topic_id w)
            {:keys [dir] :as ctx} (repo-import/clone-context user-id repo-id)
            in-range? (and (not deleted?) (some #(= sha (:sha %)) commits))
            state (if in-range?
                    (wr/file-state dir (:sha_a w) commits sha path ctx)
                    (when-let [{:keys [source]} (docref/content user-id {:repo-id repo-id :path path :sha sha})]
                      {:text source :attributed? false
                       :lines (mapv (fn [l] {:text l :origin nil}) (diff/lines source))}))]
        (when state
          (let [classes (mapv #(line-class (some-> (:origin %) position-of) position mode) (:lines state))
                markdown? (= "markdown" (tree/language-for path))]
            {:path path :sha sha :markdown? markdown? :attributed? (:attributed? state)
             :ref-spec {:repo-id repo-id :path path :sha sha}
             :html (if markdown?
                     (markdown-html (:text state) path classes)
                     (code-html (:lines state) classes))}))))))

;; ── Writes ─────────────────────────────────────────────────────────

(defn understood!
  "\"I understand\" on the current step (§6.6.5). Post: {:ok step} or {:error}."
  [user-id wid]
  (if-let [step (db/mark-concept-understood! user-id wid)]
    (do (commands/bump! user-id :walkthrough-step) {:ok step})
    {:error "That walkthrough could not be moved on."}))

(defn go-to-step!
  "Open step `step` (§6.6.2). Post: {:ok step} or {:error}."
  [user-id wid step]
  (if-let [s (db/set-walkthrough-step! user-id wid step)]
    (do (commands/bump! user-id :walkthrough-step) {:ok s})
    {:error "That step could not be opened."}))

(defn move-hunk!
  "Move a hunk to another concept (§6.5.7.1). Post: {:ok true} or {:error}."
  [user-id hunk-id concept-id]
  (if (pos? (or (db/move-walkthrough-hunk! user-id hunk-id concept-id) 0))
    (do (commands/bump! user-id :move-walkthrough-hunk) {:ok true})
    {:error "That change could not be moved."}))

(defn rename-concept!
  "Rename a concept (§6.5.7.2). Post: {:ok true} or {:error}."
  [user-id concept-id label]
  (let [label (str/trim (str label))]
    (cond
      (str/blank? label) {:error "A concept needs a name."}
      (> (count label) grouping/max-label-chars) {:error "That name is too long."}
      (pos? (or (db/rename-walkthrough-concept! user-id concept-id label) 0))
      (do (commands/bump! user-id :rename-walkthrough-concept) {:ok true})
      :else {:error "Another step already has that name."})))

(defn merge-concepts!
  "Merge one concept into another (§6.5.7.3). Post: {:ok true} or {:error}."
  [user-id from-id into-id]
  (if (db/merge-walkthrough-concepts! user-id from-id into-id)
    (do (commands/bump! user-id :merge-walkthrough-concepts) {:ok true})
    {:error "Those steps could not be merged."}))

;; ── Focus ──────────────────────────────────────────────────────────

(defonce ^:private !focus
  ;; {[user-id repo-id] {:walkthrough-id :position :path :sha}}
  (atom {}))

(defn set-focus!
  "The walkthrough screen of `user-id` on repository node `repo-id` shows
   step `position` and the file at `path` and `sha`. Post: nil."
  [user-id repo-id focus]
  (swap! !focus assoc [user-id repo-id] focus)
  nil)

(defn clear-focus!
  "The view of step `position` of walkthrough `wid` closed. A focus another
   view set since stays: the next view can report before the last one's
   unmount runs. Post: nil."
  [user-id repo-id wid position]
  (swap! !focus (fn [m] (let [f (get m [user-id repo-id])]
                          (if (and (= wid (:walkthrough-id f)) (= position (:position f)))
                            (dissoc m [user-id repo-id])
                            m))))
  nil)

(defn focus [user-id page-topic-id]
  (get @!focus [user-id page-topic-id]))

(defn focused-ref
  "The document ref of the file the walkthrough screen on `page-topic-id`
   shows, or nil when no walkthrough screen is open there."
  [user-id page-topic-id]
  (when-let [{:keys [path sha]} (focus user-id page-topic-id)]
    (when (and path sha)
      (docref/resolve-spec user-id {:repo-id page-topic-id :path path :sha sha}))))

;; ── The tutor ──────────────────────────────────────────────────────

(def ^:private hunk-budget 40000)
(def ^:private document-budget 60000)
(def ^:private why-budget 12000)
(def ^:private code-chunk-lines 40)

(defn- evidence-key [i] (str "E" i))

(defn- hunk-evidence
  "The concept's hunks as evidence, within `hunk-budget` (§6.7.2). A hunk's
   quote is its added text, which sits in the file at that commit exactly,
   so pointing at the citation can outline it in the canvas."
  [ctx {:keys [w commits]} concept-hunks first-index]
  (let [subject-of (into {} (map (juxt :sha :subject)) commits)
        by-commit (group-by :commit_sha concept-hunks)
        parsed (into {}
                 (mapcat (fn [c]
                           (when (by-commit (:sha c))
                             (for [fd (wr/commit-diff (:dir ctx) c ctx)
                                   h (:hunks fd)]
                               [(wr/hunk-key (:sha c) (:old-path fd) (:new-path fd) (:index h)) h]))))
                 commits)]
    (loop [[h & more] concept-hunks i first-index used 0 out []]
      (if (or (nil? h) (> used hunk-budget))
        out
        (let [d (parsed (hunk-key-of h))
              body (when d (diff/hunk-text d 4000))
              added (when d (not-empty (apply str (keep #(when (str/starts-with? % "+") (subs % 1))
                                                    (:lines d)))))
              path (file-of h)]
          (if-not body
            (recur more i used out)
            (recur more (inc i) (+ used (count body))
              (conj out {:citation-key (evidence-key i)
                         :document-topic-id (:repo_topic_id w) :page-topic-id nil :page-number nil
                         :anchor nil :document-title path
                         :display-label (str "Change in " (short-sha (:commit_sha h)) ": "
                                          (subject-of (:commit_sha h)))
                         :source-revision (:commit_sha h)
                         :quoted-text (or added (apply str (map #(subs % 1) (:lines d))))
                         :quoted-html nil
                         :snapshot-content (str "Commit " (short-sha (:commit_sha h)) " \""
                                             (subject-of (:commit_sha h)) "\", file " path "\n" body)
                         :walkthrough-id (:id w)}))))))))

(defn- document-evidence
  "The canvas file's passages as evidence, within `document-budget`. A
   Markdown file gives its rendered blocks, with heading anchors; code gives
   chunks of `code-chunk-lines` lines, each its exact text."
  [user-id ref wid first-index]
  (when-let [{:keys [source]} (docref/content user-id ref)]
    (let [blocks (if (docref/markdown? ref)
                   (mapv (fn [b] {:text (:text b) :html (:html b) :anchor (:anchor b)
                                  :label (str (or (:section-title b) "Passage") ", excerpt "
                                           (:block-index b))})
                     (evidence-excerpt/evidence-blocks
                       (repo-md/render source (:path ref) (constantly {}) nil)))
                   (mapv (fn [i chunk] {:text (apply str chunk) :html nil :anchor nil
                                        :label (str "Lines " (inc (* i code-chunk-lines)) "–"
                                                 (+ (* i code-chunk-lines) (count chunk)))})
                     (range) (partition-all code-chunk-lines (diff/lines source))))]
      (loop [[b & more] blocks i first-index used 0 out []]
        (if (or (nil? b) (> used document-budget))
          out
          (recur more (inc i) (+ used (count (:text b)))
            (conj out {:citation-key (evidence-key i)
                       :document-topic-id (:repo-id ref) :page-topic-id nil :page-number nil
                       :anchor (:anchor b) :document-title (:path ref)
                       :display-label (str (:label b) ", at " (short-sha (:sha ref)))
                       :source-revision (:sha ref)
                       :quoted-text (:text b) :quoted-html (:html b)
                       :snapshot-content (:text b)
                       :walkthrough-id wid})))))))

(defn- why-text
  "The commit messages and plan diffs behind the concept (§6.7.2)."
  [ctx commits concept]
  (let [shas (set (:commits concept))
        mine (filter #(shas (:sha %)) commits)
        text (str/join "\n\n"
               (for [c mine]
                 (str "Commit " (short-sha (:sha c)) ": " (:subject c)
                   (when (seq (:body c)) (str "\n" (:body c)))
                   (let [plans (filter #(wr/plan-path? (:new-path %)) (wr/commit-diff (:dir ctx) c ctx))]
                     (when (seq plans)
                       (str "\nPlan diff:\n"
                         (str/join "\n" (for [fd plans h (:hunks fd)]
                                          (str (:new-path fd) "\n" (diff/hunk-text h 3000))))))))))]
    (if (> (count text) why-budget) (str (subs text 0 why-budget) "\n… (clipped)") text)))

(defn- marks-text
  "The reader's known marks on this repository, each with when they learned
   it relative to the range (§6.7.3)."
  [user-id ctx w]
  (let [marks (db/known-marks-on-repo user-id (:repo_topic_id w))]
    (if (empty? marks)
      "(none)"
      (str/join "\n"
        (for [{:keys [label source as_of_sha]} marks]
          (str "- " label " (" (if (= "self" source) "the reader says so" "shown to the tutor")
            (when as_of_sha
              (str ", learned at " (short-sha as_of_sha)
                (if (and (git/commit-exists? (:dir ctx) as_of_sha)
                      (git/ancestor? (:dir ctx) as_of_sha (:sha_a w)))
                  ", BEFORE this range"
                  ", inside this range")))
            ")"))))))

(def ^:private known-block-instructions
  (str "## The known block\n\n"
    "When the reader's own answer shows they understand an idea, start your reply with:\n\n"
    "```" block/fence-tag "\n{\"known\": [\"<idea in a few words>\"]}\n```\n\n"
    "The block is JSON and the reader never sees it. Leave it out when nothing new is known. "
    "Never list an idea only because you explained it.\n"))

(defn request-tail
  "What a run about repository node `page-topic-id` is told while a
   walkthrough screen is open on it (§6.7), or nil.
   `first-index` is the first free evidence number.
   Post: {:content system-text :evidence [...] :run {:walkthrough-id :repo-id
         :as-of-sha}}. Step 0 is the opener: the tutor tests what the reader
         believes about the state at A (§6.7.1). A later step teaches its
         concept from its hunks, the canvas file, and why it changed (§6.7.2).
         `:as-of-sha` is the version a known mark from this reply records."
  [user-id page-topic-id first-index]
  (when-let [{:keys [walkthrough-id position]} (focus user-id page-topic-id)]
    (when-let [{:keys [w commits concepts concept hunks] :as sctx}
               (step-context user-id walkthrough-id (or position 0))]
      (when (= "ready" (:status w))
        (let [ctx (repo-import/clone-context user-id (:repo_topic_id w))
              ref (focused-ref user-id page-topic-id)
              concept-list (str/join "\n" (map #(str (:position %) ". " (:label %) " — " (:summary %))
                                            concepts))
              head (str "# Commit-range walkthrough\n\n"
                     "The reader is learning what changed in this repository from commit "
                     (short-sha (:sha_a w)) " (A) to " (short-sha (:sha_b w)) " (B), "
                     (count commits) " commits, one concept per step.\n\n"
                     "## Steps\n\n" concept-list "\n\n"
                     "## What the reader already knows\n\n" (marks-text user-id ctx w) "\n\n")]
          (if (or (nil? concept) (zero? position))
            {:content (str head
                        "## This is the opener\n\n"
                        "Before step 1, test what the reader believes about the code AT A: the "
                        "parts these concepts change. Ask one question at a time, and wait for the "
                        "answer. Correct a wrong belief briefly. Do not teach the changes yet.\n\n"
                        known-block-instructions)
             :evidence []
             :run {:walkthrough-id walkthrough-id :repo-id (:repo_topic_id w) :as-of-sha (:sha_a w)}}
            (let [mine (filterv #(= (:id concept) (:concept_id %)) hunks)
                  hunk-ev (hunk-evidence ctx sctx mine first-index)
                  doc-ev (when ref (document-evidence user-id ref walkthrough-id
                                     (+ first-index (count hunk-ev))))
                  evidence (into hunk-ev doc-ev)
                  index-of (into {} (map-indexed (fn [i c] [(:sha c) i])) commits)
                  last-sha (:sha (nth commits (apply max 0 (map #(index-of (:commit_sha %) 0) mine))))]
              {:content (str head
                          "## This step: " (:position concept) ". " (:label concept) "\n\n"
                          (:summary concept) "\n\n"
                          "Teach this concept. Each claim cites a change or a passage below with "
                          "its evidence key. Ask the reader a question to check understanding. "
                          "When this concept changes an idea the reader learned BEFORE this range, "
                          "say that the idea changed after they learned it.\n\n"
                          "## Why it changed\n\n" (why-text ctx commits concept) "\n\n"
                          "## Sources\n\n"
                          (str/join "\n\n" (map #(str "Source " (:citation-key %) " — "
                                                   (:document-title %) " — " (:display-label %) "\n"
                                                   (:snapshot-content %))
                                             evidence))
                          "\n\n" known-block-instructions)
               :evidence evidence
               :run {:walkthrough-id walkthrough-id :repo-id (:repo_topic_id w)
                     :as-of-sha last-sha}})))))))

(def ^:private max-known 12)

(defn known-problem
  "Why the `known` list of a parsed block cannot be applied, or nil."
  [data]
  (let [known (:known data)]
    (cond
      (nil? known) nil
      (not (and (sequential? known) (every? string? known))) "\"known\" is not a list of ideas."
      (> (count known) max-known) (str "\"known\" lists more than " max-known " ideas.")
      (some #(or (str/blank? %) (> (count %) block/max-label-chars)) known)
      "An idea in \"known\" is empty or too long.")))

(defn apply-reply!
  "Write the `known` ideas of the block that `text` starts with as tutor
   marks with the run's version (§6.7.1).
   Post: {:prose text-without-block :problem nil-or-message}. Nothing is
         written for a block that cannot be parsed or checked."
  [user-id {:keys [repo-id as-of-sha]} text]
  (let [{:keys [block prose]} (block/split-reply text)]
    (if-not block
      {:prose text :problem nil}
      (let [data (try (json/parse-string block true) (catch Exception _ ::unparsed))
            problem (if (= ::unparsed data) "The block is not valid JSON." (known-problem data))]
        (if (or problem (empty? (:known data)))
          {:prose prose :problem problem}
          (let [labels (vec (distinct (map str/trim (:known data))))
                ;; An entity of the same name is the same idea; the linker
                ;; finds near names, and only the rest are new.
                by-lower (merge (question-map/link-labels! user-id labels :walkthrough.link)
                           (db/kg-entity-ids-by-labels user-id labels))
                missing (remove #(by-lower (str/lower-case %)) labels)
                created (zipmap (map str/lower-case missing)
                          (db/insert-kg-entities! user-id missing {:from-diagnosis? true}))
                ids (keep #(or (by-lower (str/lower-case %)) (created (str/lower-case %))) labels)]
            (doseq [id ids]
              (db/set-known-mark! user-id id "tutor" {:repo-id repo-id :sha as-of-sha}))
            (commands/bump! user-id :apply-walkthrough-known)
            {:prose prose :problem nil}))))))
