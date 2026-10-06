(ns freememo.walkthrough-grouping
  "Group the hunks of a commit range into concepts (plans/commit-range-walkthrough.md
   §6.5, grouping (d)).

   Pass 1 reads each commit's message, plan-file diffs, work-package labels
   and file list, and proposes concepts in teaching order. Pass 2 puts every
   hunk in exactly one concept. A hunk the model does not place, or places in
   a concept that does not exist, goes to \"Other changes\"; a noise hunk
   never reaches the model and goes to \"Noise\".

   The prompts, the checks and the final assembly are pure. `group!` runs the
   two passes through a `chat` fn with `kg-llm/chat!`'s contract, so a test
   or a probe can stand in for the model."
  (:require [clojure.string :as str]
            [freememo.unified-diff :as diff]
            [freememo.walkthrough-range :as wr]))

(def max-concepts 30)
(def max-label-chars 120)

(def other-label "Other changes")
(def noise-label "Noise")

(def ^:private pass-1-budget
  "Characters of commit input one pass-1 call takes. Past it, each commit is
   summarized first (§6.5.1)."
  120000)

(def ^:private plan-chars-per-commit 6000)
(def ^:private hunk-chars 1500)
(def ^:private pass-2-budget 40000)

(defn- short-sha [sha] (subs sha 0 (min 7 (count sha))))

(defn- fold [s] (some-> s str str/trim str/lower-case))

;; ── Pass 1 input ───────────────────────────────────────────────────

(defn commit-input
  "One commit as pass 1 reads it: sha, subject, body, work-package labels,
   file list with line stats, and its plan-file diffs clipped to a budget."
  [{:keys [sha subject body]} hunks]
  (let [by-file (group-by (juxt :old-path :new-path) hunks)
        plan-text (->> hunks
                    (filter #(wr/plan-path? (or (:new-path %) (:old-path %))))
                    (map #(str "--- " (or (:new-path %) (:old-path %)) "\n"
                            (diff/hunk-text % plan-chars-per-commit)))
                    (str/join "\n"))
        plan-text (if (> (count plan-text) plan-chars-per-commit)
                    (str (subs plan-text 0 plan-chars-per-commit) "\n… (plan diff clipped)")
                    plan-text)
        wps (wr/work-package-labels (str subject "\n" body))]
    (str "## Commit " (short-sha sha) "\n"
      subject "\n"
      (when (seq body) (str "\n" body "\n"))
      (when (seq wps) (str "\nWork packages: " (str/join ", " wps) "\n"))
      "\nFiles:\n"
      (str/join "\n"
        (map (fn [[[old new] hs]]
               (str "- " (if (and old new (not= old new)) (str old " → " new) (or new old))
                 " (+" (reduce + (map :added hs)) " -" (reduce + (map :removed hs)) ")"))
          by-file))
      (when (seq plan-text) (str "\n\nPlan diff:\n" plan-text))
      "\n")))

(def pass-1-system
  (str "You read the commits of a range of a software repository and name the "
       "concepts a reader must learn to understand the range. A concept is one "
       "idea that changed: a feature, a rename, a refactor, a fix. One commit can "
       "carry several concepts, and one concept can span several commits.\n"
       "Use the commit messages, the plan diffs and the work-package labels. "
       "Do not make one concept per file.\n"
       "Return JSON: {\"concepts\": [{\"label\": \"…\", \"summary\": \"…\", "
       "\"commits\": [\"<7-character sha>\"], \"depends_on\": [\"<label>\"]}]}.\n"
       "- label: a few words naming the idea.\n"
       "- summary: two or three sentences on what changed and why.\n"
       "- commits: the commits that carry this concept. Every commit must appear "
       "in at least one concept.\n"
       "- depends_on: concepts a reader must learn first.\n"
       "List the concepts in teaching order: a concept after the concepts it "
       "depends on. Name at most " max-concepts " concepts."))

(def summary-system
  (str "Summarize each commit of a software repository for a reader who will "
       "group the commits by concept. Return JSON: {\"summaries\": "
       "[{\"commit\": \"<7-character sha>\", \"summary\": \"…\"}]}. One summary "
       "per commit, in at most 80 words, naming what changed and why."))

;; ── Pass 1 checks ──────────────────────────────────────────────────

(defn- sha-of
  "The full sha in `shas` that `s` names, a prefix of at least 7 characters."
  [shas s]
  (let [s (str/lower-case (str/trim (str s)))]
    (when (>= (count s) 7)
      (some #(when (str/starts-with? % s) %) shas))))

(defn concept-problem
  "Why pass-1 output `data` cannot be used for commits `shas`, or nil."
  [data shas]
  (let [concepts (:concepts data)
        labels (map (comp fold :label) concepts)]
    (cond
      (not (map? data)) "The reply is not an object."
      (not (sequential? concepts)) "\"concepts\" is not a list."
      (empty? concepts) "No concept was named."
      (> (count concepts) max-concepts) (str "More than " max-concepts " concepts were named.")
      (not-every? #(and (map? %) (string? (:label %)) (seq (str/trim (:label %)))
                     (<= (count (:label %)) max-label-chars))
        concepts)
      "A concept has no label, or its label is too long."
      (not= (count labels) (count (distinct labels))) "Two concepts share a label."
      (some #(#{(fold other-label) (fold noise-label)} %) labels)
      (str "\"" other-label "\" and \"" noise-label "\" are reserved labels.")
      (not-every? #(and (sequential? (:commits %)) (every? string? (:commits %))) concepts)
      "A concept's commits are not a list of shas."
      (some #(nil? (sha-of shas %)) (mapcat :commits concepts))
      "A concept names a commit that is not in the range."
      (not= (set shas) (set (keep #(sha-of shas %) (mapcat :commits concepts))))
      (str "These commits are in no concept: "
        (str/join ", " (map short-sha (remove (set (keep #(sha-of shas %) (mapcat :commits concepts)))
                                        shas)))
        ".")
      (not-every? #(let [d (:depends_on %)] (or (nil? d) (and (sequential? d) (every? string? d))))
        concepts)
      "A concept's depends_on is not a list of labels.")))

(defn ordered
  "`concepts` in an order where each follows those it depends on, keeping the
   model's order otherwise (§6.5.3). A dependency cycle keeps the model's
   order for the concepts in it."
  [concepts]
  (let [by-label (into {} (map (juxt (comp fold :label) identity)) concepts)
        deps (fn [c] (filter by-label (map fold (:depends_on c))))]
    (loop [remaining (vec concepts) out [] placed #{}]
      (if (empty? remaining)
        out
        (let [ready (first (filter #(every? placed (remove #{(fold (:label %))} (deps %))) remaining))
              pick (or ready (first remaining))]
          (recur (vec (remove #(identical? % pick) remaining))
            (conj out pick) (conj placed (fold (:label pick)))))))))

(defn normalize-concepts
  "Pre: (concept-problem data shas) is nil.
   Post: [{:label :summary :commits [full sha] :depends-on}] in teaching order."
  [data shas]
  (mapv (fn [c] {:label (str/trim (:label c))
                 :summary (str/trim (str (:summary c)))
                 :commits (vec (distinct (keep #(sha-of shas %) (:commits c))))
                 :depends_on (:depends_on c)})
    (ordered (:concepts data))))

;; ── Pass 2 ─────────────────────────────────────────────────────────

(def pass-2-system
  (str "You place each hunk of a diff in the concept it belongs to. A test, a "
       "helper or a doc edit belongs to the concept it serves, not to the nearest "
       "code. Return JSON: {\"assignments\": {\"<hunk id>\": \"<concept label>\"}}. "
       "Give every hunk id exactly one label from the list, or \"other\" when none "
       "fits."))

(defn concepts-text [concepts]
  (str/join "\n" (map #(str "- " (:label %) ": " (:summary %)) concepts)))

(defn hunk-batches
  "`hunks` with ids, cut into batches whose clipped text fits `pass-2-budget`.
   Post: [[[id hunk] …] …]."
  [hunks]
  (let [ided (map-indexed (fn [i h] [(str "H" (inc i)) h]) hunks)]
    (loop [[x & more :as xs] ided batch [] size 0 out []]
      (if (empty? xs)
        (cond-> out (seq batch) (conj batch))
        (let [n (count (diff/hunk-text (second x) hunk-chars))]
          (if (and (seq batch) (> (+ size n) pass-2-budget))
            (recur xs [] 0 (conj out batch))
            (recur more (conj batch x) (+ size n) out)))))))

(defn batch-input [commit-subject batch]
  (str/join "\n\n"
    (map (fn [[id h]]
           (str "### " id " · commit " (short-sha (:commit-sha h)) " \""
             (commit-subject (:commit-sha h)) "\" · "
             (let [{:keys [old-path new-path]} h]
               (if (and old-path new-path (not= old-path new-path))
                 (str old-path " → " new-path)
                 (or new-path old-path)))
             "\n" (diff/hunk-text h hunk-chars)))
      batch)))

(defn read-assignments
  "The label each hunk of `batch` got in pass-2 output `data`.
   Post: {id label}; a label is one of `labels`' spellings, or nil when the
         reply gave none, \"other\", or a label that does not exist."
  [data batch labels]
  (let [by-fold (into {} (map (juxt fold identity)) labels)
        given (:assignments data)]
    (into {}
      (map (fn [[id _]]
             [id (when (map? given)
                   (by-fold (fold (or (get given (keyword id)) (get given id)))))]))
      batch)))

(defn assemble
  "The final grouping.
   Pre : `assignment` is {hunk-key label-or-nil} for every non-noise hunk.
   Post: {:concepts [{:label :summary :commits :noise?}] :hunks [... :concept]}.
         Every hunk has exactly one concept. A concept with no hunk is dropped.
         \"Other changes\" comes before \"Noise\", and both are last."
  [concepts hunks noise assignment]
  (let [key-of (fn [{:keys [commit-sha old-path new-path index]}]
                 (wr/hunk-key commit-sha old-path new-path index))
        labeled (mapv (fn [h]
                        (let [k (key-of h)]
                          (assoc h :concept (cond (noise k) noise-label
                                                  :else (or (get assignment k) other-label)))))
                  hunks)
        used (set (map :concept labeled))
        commits-of (fn [label] (vec (distinct (map :commit-sha (filter #(= label (:concept %)) labeled)))))]
    {:concepts (cond-> (filterv #(used (:label %)) concepts)
                 (used other-label) (conj {:label other-label
                                           :summary "Changes that fit no other concept."
                                           :commits (commits-of other-label)})
                 (used noise-label) (conj {:label noise-label :noise? true
                                           :summary "Vendored, generated or very large changes. You can skip this step."
                                           :commits (commits-of noise-label)}))
     :hunks labeled}))

;; ── Running the passes ─────────────────────────────────────────────

(defn- chat-checked
  "Call `chat` and check its reply with `problem`; on a problem, call once
   more with the problem stated. Post: {:parsed :problem :cost :tokens}."
  [chat system user root-key problem]
  (let [first-r (chat system user root-key :map)
        p1 (problem (:parsed first-r))]
    (if (nil? p1)
      (assoc first-r :problem nil)
      (let [second-r (chat system (str user "\n\nYour previous reply could not be used: " p1
                                    " Reply again.")
                       root-key :map)]
        {:parsed (:parsed second-r) :problem (problem (:parsed second-r))
         :cost (+ (or (:cost first-r) 0.0) (or (:cost second-r) 0.0))
         :tokens (into (vec (:tokens first-r)) (:tokens second-r))}))))

(defn group!
  "Run both passes over a range.

   `chat` is (fn [system user root-key root-type] → {:parsed :cost :tokens}),
   `kg-llm/chat!` with its key and model bound. `check-cancelled!` throws when
   the reader abandoned the preparation; it runs before every call, so a
   cancel stops the spending at the next call. `on-pass` receives (pass cost
   tokens) once per finished pass, which is where the caller bills (§6.5.6).

   Post: `assemble`'s value. Throws ex-info ::no-concepts when pass 1 gives
         no usable concept list after its retry."
  [{:keys [chat commits hunks noise check-cancelled! on-pass set-phase!]}]
  (let [shas (mapv :sha commits)
        hunks-of (group-by :commit-sha hunks)
        inputs (mapv #(commit-input % (hunks-of (:sha %))) commits)
        !cost (atom {:cost 0.0 :tokens []})
        add-cost! (fn [{:keys [cost tokens]}]
                    (swap! !cost #(-> % (update :cost + (or cost 0.0)) (update :tokens into tokens))))
        ;; Pass 1, with commit summaries first when the input is too long.
        _ (set-phase! "finding concepts")
        pass-1-user (if (<= (reduce + (map count inputs)) pass-1-budget)
                      (str/join "\n" inputs)
                      (str/join "\n\n"
                        (mapcat (fn [batch]
                                  (check-cancelled!)
                                  (let [r (chat summary-system (str/join "\n" batch) :summaries :map)]
                                    (add-cost! r)
                                    (map (fn [{:keys [commit summary]}]
                                           (str "## Commit " commit "\n" summary))
                                      (:summaries (:parsed r)))))
                          (loop [xs inputs batch [] size 0 out []]
                            (if (empty? xs)
                              (cond-> out (seq batch) (conj batch))
                              (let [n (count (first xs))]
                                (if (and (seq batch) (> (+ size n) pass-1-budget))
                                  (recur xs [] 0 (conj out batch))
                                  (recur (rest xs) (conj batch (first xs)) (+ size n) out))))))))
        _ (check-cancelled!)
        r1 (chat-checked chat pass-1-system pass-1-user :concepts #(concept-problem % shas))
        _ (add-cost! r1)
        _ (on-pass :concepts (:cost @!cost) (:tokens @!cost))
        _ (when (:problem r1)
            (throw (ex-info (str "The concept list could not be used. " (:problem r1))
                     {:type ::no-concepts})))
        concepts (normalize-concepts (:parsed r1) shas)
        labels (map :label concepts)
        subject-of (into {} (map (juxt :sha :subject)) commits)
        key-of (fn [{:keys [commit-sha old-path new-path index]}]
                 (wr/hunk-key commit-sha old-path new-path index))
        to-place (remove #(noise (key-of %)) hunks)
        _ (set-phase! "grouping changes")
        !pass-2 (atom {:cost 0.0 :tokens []})
        assignment (into {}
                     (mapcat (fn [batch]
                               (check-cancelled!)
                               (let [user (str "Concepts:\n" (concepts-text concepts)
                                            "\n\nHunks:\n\n" (batch-input subject-of batch))
                                     r (chat-checked chat pass-2-system user :assignments
                                         (fn [data]
                                           (let [given (when (map? (:assignments data))
                                                         (set (map name (keys (:assignments data)))))
                                                 missing (remove #(contains? given %) (map first batch))]
                                             (when (seq missing)
                                               (str "Some hunk ids have no label: "
                                                 (str/join ", " (take 20 missing)) ".")))))
                                     got (read-assignments (:parsed r) batch labels)]
                                 (swap! !pass-2 #(-> % (update :cost + (or (:cost r) 0.0))
                                                   (update :tokens into (:tokens r))))
                                 (map (fn [[id h]] [(key-of h) (get got id)]) batch))))
                     (hunk-batches to-place))]
    (on-pass :classify (:cost @!pass-2) (:tokens @!pass-2))
    (assemble concepts hunks noise assignment)))
