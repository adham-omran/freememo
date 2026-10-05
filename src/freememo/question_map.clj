(ns freememo.question-map
  "Anchored questions on rendered Markdown: pins, the question map, and the
   tutor reply that builds and updates it (plans/anchored-questions.md).

   This namespace owns three things and nothing else touches them:
     - where a pin or a citation sits in a snapshot (`locate-in-topic`),
     - what a run about a document with a map is told (`request-tail`), and
     - what a finished reply writes into the map (`apply-reply!`).

   Every write ends in `commands/bump!`, so `:question-mutations` is bumped
   only through the command registry."
  (:require
   [cheshire.core :as json]
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.credits :as credits]
   [freememo.db :as db]
   [freememo.kg-extract :as kg-extract]
   [freememo.kg-llm :as llm]
   [freememo.question-map-block :as block]
   [freememo.quote-anchor :as quote-anchor]
   [freememo.repo-markdown :as repo-md]
   [taoensso.telemere :as tel]))

;; ── Locating ───────────────────────────────────────────────────────

(defn- rendered-text-html
  "The rendered Markdown of `user-id`'s file `topic-id`, for locating text.
   Links are left unresolved: an unresolved link is unwrapped to its text, so
   the text a reader sees, and every offset in it, is the same either way.
   Post: HTML, or nil when the topic has no Markdown source the user owns."
  [user-id topic-id]
  (when-let [{:keys [source lang repo_path]} (db/get-topic-code user-id topic-id)]
    (when (and (= "markdown" lang) (seq source))
      (repo-md/render source repo_path (constantly {}) nil))))

(def ^:private max-cached-locations 4096)

(defonce ^:private !locations
  ;; {[topic-id quote anchor] location-or-::none}. A snapshot's source never
  ;; changes, so an entry never goes stale (§6.1.3.1). The map is emptied when
  ;; it outgrows its bound rather than evicted entry by entry: a miss costs one
  ;; render, and the next paint refills what is in view.
  (atom {}))

(defn locate-in-topic
  "Where each of `passages` sits in `user-id`'s file `topic-id`.
   `passages` is [{:quote :anchor}]. Post: a vector in the same order, each a
   `quote-anchor/locate` result or nil. The file is rendered at most once, and
   not at all when every passage is cached."
  [user-id topic-id passages]
  (let [k (fn [{:keys [quote anchor]}] [topic-id quote anchor])
        cached @!locations
        misses (remove #(contains? cached (k %)) passages)
        found (when (seq misses)
                (when-let [html (rendered-text-html user-id topic-id)]
                  (into {}
                    (map (fn [p] [(k p) (or (quote-anchor/locate html (:quote p) (:anchor p))
                                          ::none)]))
                    misses)))]
    (when (seq found)
      (swap! !locations (fn [m] (merge (if (> (count m) max-cached-locations) {} m) found))))
    (mapv (fn [p] (let [v (get (merge cached found) (k p))]
                    (when-not (= ::none v) v)))
      passages)))

;; ── Document view ──────────────────────────────────────────────────

(defn doc-key
  "The document key of `user-id`'s file `topic-id`, or nil (§4)."
  [user-id topic-id]
  (when topic-id (db/question-doc-key user-id topic-id)))

(defn map-id-for-topic
  "The question map of the file's document key, or nil when it has none."
  [user-id topic-id]
  (when-let [{:keys [repo_topic_id repo_path]} (doc-key user-id topic-id)]
    (db/question-map-id user-id repo_topic_id repo_path)))

(defn snapshot-view
  "Everything the rendered pane and the map panel show for `user-id`'s file
   `topic-id`.
   Post: nil when the file has no document key. Otherwise
         {:map-id (or nil) :pins [...] :roots [...] :open-count n}, where each
         pin carries `:range`, its location in THIS snapshot, or nil when its
         quote no longer matches here (§6.1.3). Nothing is written."
  [user-id topic-id]
  (when (doc-key user-id topic-id)
    (if-let [map-id (map-id-for-topic user-id topic-id)]
      (let [pins (db/question-pins user-id map-id)
            ranges (locate-in-topic user-id topic-id
                     (map (fn [p] {:quote (:quote p) :anchor (:heading_anchor p)}) pins))
            roots (db/question-roots user-id map-id)
            root-label (into {} (map (juxt :id :label)) roots)]
        {:map-id map-id
         :pins (mapv (fn [p r] (assoc p :range r :root_label (root-label (:root_id p))))
                 pins ranges)
         :roots roots
         :open-count (count (filter #(= "open" (:status %)) pins))})
      {:map-id nil :pins [] :roots [] :open-count 0})))

(defn pin-counts
  "{:open n :total m} for the pins of `user-id`'s file `topic-id`. No match is
   computed, so the code pane's header can read it cheaply."
  [user-id topic-id]
  (let [pins (some->> (map-id-for-topic user-id topic-id) (db/question-pins user-id))]
    {:open (count (filter #(= "open" (:status %)) pins)) :total (count pins)}))

(defn locate-for-finger
  "Where the reader's pointed-at passage sits in `user-id`'s file `topic-id`,
   or nil (§6.5.1). `source-topic-id` is the file the quote came from, nil for
   a map citation. A quote from another file is located only when that file is
   a snapshot of the same document, because a passage of another document
   would light up text that merely matches it."
  [user-id topic-id source-topic-id quote anchor]
  (when (and topic-id (seq quote)
          (or (nil? source-topic-id) (= source-topic-id topic-id)
            (= (doc-key user-id source-topic-id) (doc-key user-id topic-id))))
    (first (locate-in-topic user-id topic-id [{:quote quote :anchor anchor}]))))

(defn map-card
  "The roots of `user-id`'s file `topic-id`, for the map card (§6.3.7).
   Post: [{:id :label :status :in_document :citations :known_sources
           :pin-labels}], `:pin-labels` being the labels of the root's open
         pins. The `next` root comes first, learned roots last. Empty when
         the file has no map. No match is computed."
  [user-id topic-id]
  (if-let [map-id (map-id-for-topic user-id topic-id)]
    (let [pins (db/question-pins user-id map-id)
          open-by-root (group-by :root_id (filter #(= "open" (:status %)) pins))]
      (->> (db/question-roots user-id map-id)
        (mapv #(assoc % :pin-labels (mapv :label (open-by-root (:id %)))))
        (sort-by #({"next" 0 "open" 1 "learned" 2} (:status %)))
        vec))
    []))

;; ── Pin writes ─────────────────────────────────────────────────────

(def max-quote-chars 4000)
(def max-question-chars 2000)

(defn create-pin!
  "Pin `quote` on `user-id`'s file `topic-id`, under heading `anchor`.
   Pre:  `quote` is non-blank. Post: {:ok pin} or {:error message}."
  [user-id topic-id {:keys [quote anchor question]}]
  (let [quote (str/trim (str quote))
        question (str/trim (str question))]
    (cond
      (str/blank? quote) {:error "Select some text first."}
      (> (count quote) max-quote-chars) {:error "That selection is too long to pin."}
      (> (count question) max-question-chars) {:error "That question is too long."}
      :else
      (if-let [{:keys [repo_topic_id repo_path]} (doc-key user-id topic-id)]
        (let [map-id (db/ensure-question-map! user-id repo_topic_id repo_path)
              pin (db/create-question-pin! user-id map-id
                    {:snapshot-topic-id topic-id :quote quote
                     :heading-anchor anchor :question question})]
          (commands/bump! user-id :save-question-pin)
          {:ok pin})
        {:error "Questions can be pinned only on a file from a repository."}))))

(defn edit-pin!
  "Apply one edit to `user-id`'s pin. `edit` is {:op :question :text},
   {:op :status :status}, or {:op :delete}. Post: {:ok true} or {:error}."
  [user-id pin-id {:keys [op text status]}]
  (let [n (case op
            :question (when (<= (count (str text)) max-question-chars)
                        (db/update-question-pin-question! user-id pin-id (str/trim (str text))))
            :status (db/set-question-pin-status! user-id pin-id status)
            :delete (db/delete-question-pin! user-id pin-id))]
    (if (and n (pos? n))
      (do (commands/bump! user-id (if (= :delete op) :delete-question-pin :save-question-pin))
          {:ok true})
      {:error "That pin could not be changed."})))

(defn teach-root!
  "\"Teach me\" on `user-id`'s root (§6.3.7). Post: {:ok true} or {:error}."
  [user-id root-id]
  (if (db/set-question-root-next! user-id root-id)
    (do (commands/bump! user-id :teach-question-root) {:ok true})
    {:error "That idea could not be selected."}))

(defn set-root-known!
  "\"I know it\" (`known?` true) or its undo (false) on `user-id`'s root.
   A mark is written on the root's entity, which is created when the root has
   none (§6.4.2.4). Undo removes the marks of both sources (§6.4.4)."
  [user-id root-id known?]
  (if-let [entity-id (db/ensure-question-root-entity! user-id root-id)]
    (do (if known?
          (db/set-known-mark! user-id entity-id "self")
          (db/delete-known-marks! user-id entity-id))
        (commands/bump! user-id :mark-question-root-known)
        {:ok true})
    {:error "That idea could not be marked."}))

;; ── The run ────────────────────────────────────────────────────────

(defn- pin-line [{:keys [label quote question]}]
  (str "[pin " label "] \"" (str/replace quote #"\s+" " ") "\""
    (when-not (str/blank? question) (str "\n" question))))

(defn diagnosis-user-message
  "The user turn of an Ask over `open-pins` (§6.2.5)."
  [open-pins]
  (str "I pinned these passages and questions:\n\n"
    (str/join "\n\n" (map pin-line open-pins))
    "\n\nFind what these questions have in common, then teach me."))

(defn open-pins-message
  "The Ask turn for `user-id`'s file `topic-id`: {:map-id :text} built from
   every open pin of its map, or {:error} when it has none. The one place a pin
   set is built for an Ask, whichever control started it (§6.2.5.1)."
  [user-id topic-id]
  (let [map-id (map-id-for-topic user-id topic-id)
        open (when map-id (filterv #(= "open" (:status %)) (db/question-pins user-id map-id)))]
    (if (seq open)
      {:map-id map-id :text (diagnosis-user-message open)}
      {:error "There are no open pins to ask about."})))

(def ^:private block-instructions
  (str "## The map block\n\n"
    "Start your reply with this block when the map changes, and leave it out when it does not:\n\n"
    "```" block/fence-tag "\n"
    "{\"roots\": [{\"label\": \"…\", \"in_document\": true, \"citations\": [\"E3\"]}],\n"
    " \"pins\": {\"1\": \"<root label>\"},\n"
    " \"next\": \"<root label>\",\n"
    " \"learned\": [\"<root label>\"]}\n"
    "```\n\n"
    "Every key is optional. The block is JSON. The reader never sees it.\n"
    "- \"roots\" adds missing ideas. A label names one idea in a few words. "
    "\"in_document\" says whether this document explains it. "
    "\"citations\" are evidence keys of the passages that explain it.\n"
    "- \"pins\" links a pin, by its number, to the root it depends on.\n"
    "- \"next\" is the root you will teach now.\n"
    "- \"learned\" lists roots the reader has now shown they understand, from "
    "their own answers. Do not list a root only because you explained it.\n"))

(def ^:private diagnosis-instructions
  (str "## This reply is a diagnosis\n\n"
    "The reader pinned several passages with questions. Read them together. "
    "Several questions usually rest on one missing idea, or on one wrong assumption. "
    "Find those ideas. They are the roots.\n"
    "Start with the map block. Link every open pin to exactly one root. "
    "Prefer a root that several pins share over one root per pin. "
    "Set \"next\" to the root to teach first: the one the others depend on, "
    "and never a root the reader already knows.\n"
    "After the block, name the roots in one or two sentences, saying which pins "
    "each one explains. Then ask your first question about the next root. "
    "Do not answer the pins directly.\n"))

(defn- root-line [{:keys [label status in_document known_sources]}]
  (str "- " label " — " status
    (when-not in_document ", outside this document")
    (when (seq known_sources)
      (str ", KNOWN (" (str/join " and " (map {"self" "the reader says so"
                                              "tutor" "shown in an earlier lesson"}
                                          known_sources))
        ")"))))

(defn request-tail
  "The system message a run about `user-id`'s file `page-topic-id` carries, or
   nil when the file has no map and this is not an Ask.

   It goes in the request's tail, never the cached head (see
   `assistant/annotation-instructions`). On an Ask (`diagnosis?`), it also
   carries `tutor-persona`: the run teaches as the Tutor whatever the chat's
   mode is, and the chat's mode is not changed (§6.3.1.2).
   Post: {:map-id :content}."
  [user-id page-topic-id diagnosis? tutor-persona]
  (when-let [map-id (map-id-for-topic user-id page-topic-id)]
    (let [roots (db/question-roots user-id map-id)
          open (filterv #(= "open" (:status %)) (db/question-pins user-id map-id))
          root-of (into {} (map (juxt :id :label)) roots)]
      (when (or diagnosis? (seq roots))
        {:map-id map-id
         :content
         (str "# Question map\n\n"
           "The reader keeps a map of the ideas they are missing in this document.\n\n"
           (when diagnosis?
             (str "## Persona for this reply\n\n" tutor-persona "\n\n"))
           "## Roots\n\n"
           (if (seq roots) (str/join "\n" (map root-line roots)) "(none yet)")
           "\n\nDo not teach a KNOWN root unless the reader asks for it.\n\n"
           "## Open pins\n\n"
           (if (seq open)
             (str/join "\n" (map #(str (pin-line %)
                                    (when-let [r (root-of (:root_id %))]
                                      (str "\n(root: " r ")")))
                              open))
             "(none)")
           "\n\n" block-instructions
           (when diagnosis? (str "\n" diagnosis-instructions)))}))))

(defn- link-roots!
  "Entity ids for root `labels`, through the extraction linker (§6.4.2.1).
   Post: {lower-label entity-id}. A missing key or credit, or a failed call,
         yields {} and the roots stay unlinked: `ensure-question-root-entity!`
         gives each one an entity when the reader marks it."
  [user-id labels]
  (if (empty? labels)
    {}
    (try
      (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :link)
            {:keys [by-label cost tokens]} (kg-extract/link-entities! api-key model-fields
                                             user-id labels {:from-diagnosis? true})]
        (when (and cost (pos? cost))
          (credits/record-cost-charge! user-id :question-map.link (:id entry) cost tokens))
        by-label)
      (catch Exception e
        (tel/log! {:level :warn :id ::link-failed
                   :data {:user-id user-id :labels (count labels) :error (ex-message e)}}
          "Question-map root linking failed; roots stay unlinked")
        {}))))

(defn- citations
  "The {:quote :anchor} passages behind evidence `keys` of one run.
   A key the run did not retain is dropped. So is a citation of another file:
   the map belongs to this file, and only its passages can light up in it. A
   repo file's evidence names the repository node as its document and the file
   as its page (`assistant/topic-parts`), so the page is what is compared."
  [evidence keys file-topic-id]
  (into []
    (keep (fn [k]
            (let [{:keys [quoted_text anchor page_topic_id]} (get evidence k)]
              (when (and (seq quoted_text) (= page_topic_id file-topic-id))
                {:quote quoted_text :anchor anchor}))))
    keys))

(defn apply-reply!
  "Write the map block at the start of `text`, if any, into map `map-id`.

   `run` is {:map-id :diagnosis? :run-uuid :page-topic-id}.
   Post: {:prose text-without-block :problem nil-or-message}. The prose is
         what the transcript stores. When the block cannot be parsed or
         checked, nothing is written and :problem says why (§6.3.2). Writes
         go through `db/apply-question-map-plan!` in one transaction."
  [user-id {:keys [map-id diagnosis? run-uuid page-topic-id]} text]
  (let [{:keys [block prose]} (block/split-reply text)]
    (if-not block
      {:prose text :problem (when diagnosis? "The tutor did not send a map.")}
      (let [data (try (json/parse-string block true) (catch Exception _ ::unparsed))
            roots (db/question-roots user-id map-id)
            pins (db/question-pins user-id map-id)
            problem (if (= ::unparsed data)
                      "The map block is not valid JSON."
                      (block/problem data (map :label roots) (map :label pins)))]
        (if problem
          {:prose prose :problem problem}
          (let [root-label (into {} (map (juxt :id :label)) roots)
                p (block/plan data
                    {:diagnosis? diagnosis?
                     :roots (mapv (fn [r] {:label (:label r) :status (:status r)
                                           :known? (boolean (seq (:known_sources r)))})
                              roots)
                     :open-pins (into []
                                  (comp (filter #(= "open" (:status %)))
                                    (map (fn [pin] {:label (:label pin) :question (:question pin)
                                                    :quote (:quote pin)
                                                    :root-label (root-label (:root_id pin))})))
                                  pins)})
                unlinked (into [] (comp (filter #(nil? (:entity_id %))) (map :label)) roots)
                by-lower (link-roots! user-id (into (mapv :label (:new-roots p)) unlinked))
                entity-of #(get by-lower (str/lower-case (str/trim %)))
                evidence (into {} (map (juxt :citation_key identity))
                           (db/get-assistant-run-evidence user-id run-uuid))]
            (db/apply-question-map-plan! user-id map-id
              (assoc p
                :new-roots (mapv (fn [{:keys [label citation-keys] :as r}]
                                   (assoc r :entity-id (entity-of label)
                                     :citations (citations evidence citation-keys
                                                  page-topic-id)))
                             (:new-roots p))
                :relinks (into {} (keep (fn [l] (when-let [e (entity-of l)] [l e]))) unlinked)))
            (commands/bump! user-id :apply-question-map)
            {:prose prose :problem nil}))))))
