(ns freememo.question-map
  "Anchored questions on repository files: pins, the question map, and the
   tutor reply that builds and updates it (plans/anchored-questions.md).

   Every function takes a document ref (`freememo.document-ref`), never a
   topic id: a walkthrough shows files at commits that are not topics
   (plans/commit-range-walkthrough.md §6.3). A caller holding a topic converts
   it with `document-ref/for-topic` at its boundary.

   This namespace owns three things and nothing else touches them:
     - where a pin or a citation sits in a file at a commit (`locate`),
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
   [freememo.document-ref :as docref]
   [freememo.kg-extract :as kg-extract]
   [freememo.kg-llm :as llm]
   [freememo.question-map-block :as block]
   [freememo.question-map-view :as view]
   [freememo.quote-anchor :as quote-anchor]
   [freememo.repo-markdown :as repo-md]
   [freememo.util :as util]
   [taoensso.telemere :as tel]))

;; ── Locating ───────────────────────────────────────────────────────

(defn rendered-text-html
  "The rendered Markdown of `user-id`'s file at `ref`, for locating text.
   Links are left unresolved: an unresolved link is unwrapped to its text, so
   the text a reader sees, and every offset in it, is the same either way.
   Post: HTML, or nil when the file is not Markdown or has no source."
  [user-id ref]
  (when (docref/markdown? ref)
    (when-let [{:keys [source]} (docref/content user-id ref)]
      (when (seq source)
        (repo-md/render source (:path ref) (constantly {}) nil)))))

(defn locate-in-source
  "Where `quote` sits in code `source` (§6.3.7 of
   plans/commit-range-walkthrough.md): an exact match, whitespace and all.
   A repeated quote resolves to the occurrence whose start is nearest
   `offset`, the first one when `offset` is nil.
   Post: {:offsets [start end]}, character offsets into `source`, or nil."
  [^String source ^String quote offset]
  (when (and (seq source) (seq quote))
    (let [starts (loop [from 0 acc []]
                   (let [i (.indexOf source quote (int from))]
                     (if (neg? i) acc (recur (inc i) (conj acc i)))))]
      (when-let [best (if (and offset (seq starts))
                        (apply min-key #(Math/abs (long (- % offset))) starts)
                        (first starts))]
        {:offsets [best (+ best (count quote))]}))))

(def ^:private max-cached-locations 4096)

(defonce ^:private !locations
  ;; {[repo-id path sha quote anchor offset] location-or-::none}. A file at a
  ;; commit never changes, so an entry never goes stale, and two snapshots of
  ;; one file share no entry (§6.3.3). The map is emptied when it outgrows its
  ;; bound rather than evicted entry by entry: a miss costs one render, and
  ;; the next paint refills what is in view.
  (atom {}))

(defn locate
  "Where each of `passages` sits in `user-id`'s file at `ref`.
   `passages` is [{:quote :anchor :offset}]. Post: a vector in the same order,
   each a `quote-anchor/locate` result for Markdown, a `locate-in-source`
   result for code, or nil. The file is read and rendered at most once, and
   not at all when every passage is cached."
  [user-id {:keys [repo-id path sha topic-id] :as ref} passages]
  (let [;; A commit node always has its sha; one seeded without it falls back
        ;; to its file topic, so two such snapshots never share an entry.
        version (or sha [:topic topic-id])
        k (fn [{:keys [quote anchor offset]}] [repo-id path version quote anchor offset])
        cached @!locations
        misses (remove #(contains? cached (k %)) passages)
        found (when (seq misses)
                (if (docref/markdown? ref)
                  (when-let [html (rendered-text-html user-id ref)]
                    (into {}
                      (map (fn [p] [(k p) (or (quote-anchor/locate html (:quote p) (:anchor p))
                                            ::none)]))
                      misses))
                  (when-let [source (:source (docref/content user-id ref))]
                    (into {}
                      (map (fn [p] [(k p) (or (locate-in-source source (:quote p) (:offset p))
                                            ::none)]))
                      misses))))]
    (when (seq found)
      (swap! !locations (fn [m] (merge (if (> (count m) max-cached-locations) {} m) found))))
    (mapv (fn [p] (let [v (get (merge cached found) (k p))]
                    (when-not (= ::none v) v)))
      passages)))

;; ── Document view ──────────────────────────────────────────────────

(defn map-id-for-ref
  "The question map of the ref's document key, or nil when it has none."
  [user-id {:keys [repo-id path] :as ref}]
  (when ref (db/question-map-id user-id repo-id path)))

(defn- pin-passage [p]
  {:quote (:quote p) :anchor (:heading_anchor p) :offset (:offset_hint p)})

(defn snapshot-view
  "Everything a pane and the map panel show for `user-id`'s file at `ref`.
   Post: nil when there is no ref. Otherwise
         {:map-id (or nil) :pins [...] :tombstones [...] :roots [...]
          :open-count n}, where each pin carries `:range`, its location in
         THIS file at this commit, or nil when its quote no longer matches
         here (§6.1.3). `:tombstones` are the map's deleted seen pins, for the
         pin list only (plans/pin-numbering-reuse-and-tombstones.md §9).
         Nothing is written."
  [user-id ref]
  (when ref
    (if-let [map-id (map-id-for-ref user-id ref)]
      (let [pins (db/question-pins user-id map-id)
            ranges (locate user-id ref (map pin-passage pins))
            roots (db/question-roots user-id map-id)
            root-label (into {} (map (juxt :id :label)) roots)]
        {:map-id map-id
         :pins (mapv (fn [p r] (assoc p :range r :root_label (root-label (:root_id p))))
                 pins ranges)
         :tombstones (db/question-pin-tombstones user-id map-id)
         :roots roots
         :open-count (count (filter #(= "open" (:status %)) pins))})
      {:map-id nil :pins [] :tombstones [] :roots [] :open-count 0})))

(defn pin-counts
  "{:open n :total m} for the pins of `user-id`'s file at `ref`. No match is
   computed, so a pane header can read it cheaply."
  [user-id ref]
  (let [pins (some->> (map-id-for-ref user-id ref) (db/question-pins user-id))]
    {:open (count (filter #(= "open" (:status %)) pins)) :total (count pins)}))

(defn locate-for-finger
  "Where the reader's pointed-at passage sits in `user-id`'s file at `ref`,
   or nil (§6.5.1). `source-doc` is the document key [repo-id path] the quote
   came from, nil for a map citation. A quote from another document is never
   located, because a passage of another document would light up text that
   merely matches it."
  [user-id ref source-doc quote anchor]
  (when (and ref (seq quote)
          (or (nil? source-doc) (= source-doc (docref/doc-key ref))))
    (first (locate user-id ref [{:quote quote :anchor anchor}]))))

(defn- map-root
  "One root of `map-card`: its state, its pin labels, and its done time as a
   short date. The raw times stay on the server."
  [root open-by-root closed-by-root]
  (let [done-at (view/done-time root)]
    (-> root
      (assoc :state (view/root-state root)
             :pin-labels (mapv :label (open-by-root (:id root)))
             :closed-pin-labels (mapv :label (closed-by-root (:id root)))
             :done-label (util/format-date-short done-at))
      (dissoc :learned_at :known_at))))

(defn map-card
  "The map of `user-id`'s file at `ref`, for the map tab
   (plans/question-map-tab.md §4.5.1). No match is computed.
   Post: {:roots [...] :unplaced n}. Each root is a `db/question-roots` row
         with `:state` (`question-map-view/root-state`), `:pin-labels` and
         `:closed-pin-labels` (the labels of its open and closed pins), and
         `:done-label`, the date it became done, or nil. Roots are in trail
         order (`question-map-view/trail`). `:unplaced` counts the open pins
         with no root. {:roots [] :unplaced 0} when the file has no map."
  [user-id ref]
  (if-let [map-id (map-id-for-ref user-id ref)]
    (let [pins (db/question-pins user-id map-id)
          {open-pins "open" closed-pins "closed"} (group-by :status pins)
          open-by-root (group-by :root_id open-pins)
          closed-by-root (group-by :root_id closed-pins)]
      {:roots (mapv #(map-root % open-by-root closed-by-root)
                (view/trail (db/question-roots user-id map-id)))
       :unplaced (count (open-by-root nil))})
    {:roots [] :unplaced 0}))

;; ── Pin writes ─────────────────────────────────────────────────────

(def max-quote-chars 4000)
(def max-question-chars 2000)

(defn create-pin!
  "Pin `quote` on `user-id`'s file at `ref`, under heading `anchor` on
   Markdown, or at character `offset` on code (§6.3.7 of
   plans/commit-range-walkthrough.md). A code quote is kept exactly, because
   it anchors by an exact match.
   Pre:  `quote` is non-blank. Post: {:ok pin} or {:error message}."
  [user-id ref {:keys [quote anchor offset question]}]
  (let [quote (if (and ref (not (docref/markdown? ref))) (str quote) (str/trim (str quote)))
        question (str/trim (str question))]
    (cond
      (str/blank? quote) {:error "Select some text first."}
      (> (count quote) max-quote-chars) {:error "That selection is too long to pin."}
      (> (count question) max-question-chars) {:error "That question is too long."}
      :else
      (if ref
        (let [map-id (db/ensure-question-map! user-id (:repo-id ref) (:path ref))
              pin (db/create-question-pin! user-id map-id
                    {:snapshot-topic-id (:topic-id ref) :snapshot-sha (:sha ref) :quote quote
                     :heading-anchor anchor :offset-hint (when (integer? offset) offset)
                     :question question})]
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
  "\"Teach me\" on `user-id`'s root (§6.3.7). A `learned` root is refused
   (plans/question-map-tab.md §4.4.7). Post: {:ok true} or {:error}."
  [user-id root-id]
  (if (db/set-question-root-next! user-id root-id)
    (do (commands/bump! user-id :teach-question-root) {:ok true})
    {:error "That idea could not be selected."}))

(defn set-root-known!
  "\"I know this\" (`known?` true) or its undo (false) on `user-id`'s root.
   A `self` mark is written on the root's entity, which is created when the
   root has none (§6.4.2.4), with the commit of `ref`, the file the reader is
   reading, as its version (K2, plans/commit-range-walkthrough.md §6.8.2).
   On the `next` root it also moves `next` on (plans/question-map-tab.md
   §4.4.5). Undo removes the `self` mark only, so a tutor's confirmation
   stays (§4.4.6)."
  [user-id root-id known? ref]
  (if-let [entity-id (db/ensure-question-root-entity! user-id root-id)]
    (do (if known?
          (db/mark-root-known! user-id root-id entity-id
            (when ref {:repo-id (:repo-id ref) :sha (:sha ref)}))
          (db/delete-known-mark! user-id entity-id "self"))
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
  "The Ask turn for `user-id`'s file at `ref`: {:map-id :text} built from
   every open pin of its map, or {:error} when it has none. The one place a pin
   set is built for an Ask, whichever control started it (§6.2.5.1)."
  [user-id ref]
  (let [map-id (map-id-for-ref user-id ref)
        ;; The text becomes a chat message, so its pins are seen
        ;; (plans/pin-numbering-reuse-and-tombstones.md §5.2).
        open (when map-id (db/open-question-pins-for-tutor! user-id map-id))]
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
  "The system message a run about `user-id`'s file at `ref` carries, or nil
   when the file has no map and this is not an Ask.

   It goes in the request's tail, never the cached head (see
   `assistant/annotation-instructions`). On an Ask (`diagnosis?`), it also
   carries `tutor-persona`: the run teaches as the Tutor whatever the chat's
   mode is, and the chat's mode is not changed (§6.3.1.2).
   Post: {:map-id :content}."
  [user-id ref diagnosis? tutor-persona]
  (when-let [map-id (map-id-for-ref user-id ref)]
    (let [roots (db/question-roots user-id map-id)
          tail? (or diagnosis? (seq roots))
          ;; Read, and so marked seen, only when the tail names them
          ;; (plans/pin-numbering-reuse-and-tombstones.md §5.2).
          open (when tail? (db/open-question-pins-for-tutor! user-id map-id))
          root-of (into {} (map (juxt :id :label)) roots)]
      (when tail?
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

(defn link-labels!
  "Entity ids for idea `labels`, through the extraction linker (§6.4.2.1),
   billed under `endpoint`.
   Post: {lower-label entity-id}. A missing key or credit, or a failed call,
         yields {} and the labels stay unlinked: `ensure-question-root-entity!`
         gives a root an entity when the reader marks it."
  [user-id labels endpoint]
  (if (empty? labels)
    {}
    (try
      (let [{:keys [api-key entry model-fields]} (llm/resolve-model+gate! user-id :link)
            {:keys [by-label cost tokens]} (kg-extract/link-entities! api-key model-fields
                                             user-id labels {:from-diagnosis? true})]
        (when (and cost (pos? cost))
          (credits/record-cost-charge! user-id endpoint (:id entry) cost tokens))
        by-label)
      (catch Exception e
        (tel/log! {:level :warn :id ::link-failed
                   :data {:user-id user-id :labels (count labels) :error (ex-message e)}}
          "Question-map root linking failed; roots stay unlinked")
        {}))))

(defn- citations
  "The {:quote :anchor} passages behind evidence `keys` of one run.
   A key the run did not retain is dropped. So is a citation of another
   document: the map belongs to this document key, and only its passages can
   light up in it. Evidence of a repository file, a topic or not, names the
   repository node as its document and the path as its title
   (`assistant/topic-parts`, plans/commit-range-walkthrough.md §6.3.5), so
   that pair is what is compared."
  [evidence keys {:keys [repo-id path]}]
  (into []
    (keep (fn [k]
            (let [{:keys [quoted_text anchor document_topic_id document_title]} (get evidence k)]
              (when (and (seq quoted_text) (= document_topic_id repo-id) (= document_title path))
                {:quote quoted_text :anchor anchor}))))
    keys))

(defn apply-reply!
  "Write the map block at the start of `text`, if any, into map `map-id`.

   `run` is {:map-id :diagnosis? :run-uuid :ref}.
   Post: {:prose text-without-block :problem nil-or-message}. The prose is
         what the transcript stores. When the block cannot be parsed or
         checked, nothing is written and :problem says why (§6.3.2). Writes
         go through `db/apply-question-map-plan!` in one transaction."
  [user-id {:keys [map-id diagnosis? run-uuid ref]} text]
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
                by-lower (link-labels! user-id (into (mapv :label (:new-roots p)) unlinked)
                           :question-map.link)
                entity-of #(get by-lower (str/lower-case (str/trim %)))
                evidence (into {} (map (juxt :citation_key identity))
                           (db/get-assistant-run-evidence user-id run-uuid))]
            (db/apply-question-map-plan! user-id map-id
              (assoc p
                :new-roots (mapv (fn [{:keys [label citation-keys] :as r}]
                                   (assoc r :entity-id (entity-of label)
                                     :citations (citations evidence citation-keys ref)))
                             (:new-roots p))
                :relinks (into {} (keep (fn [l] (when-let [e (entity-of l)] [l e]))) unlinked)
                :as-of-sha (:sha ref)
                :run-uuid run-uuid))
            (commands/bump! user-id :apply-question-map)
            {:prose prose :problem nil}))))))
