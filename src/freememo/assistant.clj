(ns freememo.assistant
  "Server-side AI assistant for the reading view.

   One chat is a conversation about one document and uses one persona and one
   opaque provider session. Each submitted turn creates a durable run before it
   starts a provider stream. The run owns its exact request, retained evidence,
   partial text, provider facts, and terminal state. Stop closes the local text
   boundary while the provider stream drains for final usage data."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.openrouter :as openrouter]
   [freememo.settings :as settings]
   [freememo.assistant-annotations :as annotations]
   [freememo.assistant-budget :as budget]
   [freememo.assistant-chat-tree :as tree]
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-evidence-excerpt :as evidence-excerpt]
   [freememo.assistant-modes :as modes]
   [freememo.assistant-slash :as slash]
   [freememo.card-models :as card-models]
   [freememo.cards :as cards]
   [freememo.cards-from-facts :as cff]
   [freememo.config :as config]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.content-toolbar-helpers :as card-gen]
   [freememo.html-cleaner :as cleaner]
   [freememo.markdown :as markdown]
   [freememo.model-context :as model-context]
   [freememo.document-ref :as docref]
   [freememo.question-map :as question-map]
   [freememo.question-map-block :as map-block]
   [freememo.repo-markdown :as repo-md]
   [freememo.text :as text]
   [freememo.toasts :as toasts]
   [freememo.topic-kinds :as topic-kinds]
   [freememo.db :as db]
   [freememo.walkthrough :as walkthrough]
   [missionary.core :as m]
   [taoensso.telemere :as tel])
  (:import
   [java.util.concurrent Executors]))

(def ^:private mode-system-prompts
  "mode :id → delay over that persona's prompt file. One delay per entry, so a
   file is read once per JVM on first use of its mode. An unreadable file yields
   nil and logs: the send still goes out (personaless) rather than throwing."
  (into {}
    (map (fn [{:keys [id prompt]}]
           [id (delay (try (slurp (io/resource prompt))
                        (catch Exception e
                          (tel/error! {:id ::persona-load
                                       :data {:mode id :resource prompt}} e)
                          nil)))]))
    modes/registry))

(defn- mode-system-prompt
  "Persona text for `mode-id`, resolved through the registry so an unrecognized
   id falls back to the default persona rather than to no persona at all.
   Pre:  `mode-id` is a string or nil (a NULL column is normal input).
   Post: the persona text, or nil when that file failed to load."
  [mode-id]
  (some-> (get mode-system-prompts (:id (modes/resolve-mode mode-id))) deref))

(defn- bump! [user-id]
  (commands/bump-channels! user-id #{:assistant-mutations}))

(defn- bump-stream! [user-id]
  (commands/bump-channels! user-id #{:assistant-stream}))

(declare start-compact-run! transcript-message-view answer-context-text)

;; ── Context resolution ─────────────────────────────────────────────────────

(def ^:private max-context-facts
  "Upper bound on KG facts injected into one send's grounding — keeps a large
   document (or code repo) from overflowing the prompt. Nearest facts win."
  50)

(def ^:private max-window-chars
  "Upper bound on the combined characters of one window of PDF pages or EPUB
   chapters (`window-packet`). Farthest units are dropped first. The center
   unit is never dropped, but it is cut when it alone passes the bound."
  40000)

(defn- resolve-context
  "Resolve the learner's live reading context for one run. The client-provided
   page must belong to the user and descend from the chat's root. An invalid
   page falls back to the owned root.
   Pre: root-topic-id is the root of an owned chat.
   Post: {:root-id :root-kind :pdf? :page-topic-id :center-page :window};
   :center-page is the 1-based page number for a PDF page, else nil."
  [user-id root-topic-id page-topic-id]
  (let [root (db/get-topic-for-user user-id root-topic-id)
        candidate (when page-topic-id (db/get-topic-for-user user-id page-topic-id))
        valid-page? (and candidate
                      (some #{root-topic-id} (db/get-ancestor-ids page-topic-id)))
        resolved-page-id (if valid-page? page-topic-id root-topic-id)
        resolved-page (if valid-page? candidate root)
        pdf? (= "pdf" (:topics/kind root))
        center-page (when pdf?
                      (:topics/page_number resolved-page))]
    {:root-id root-topic-id
     :root-kind (:topics/kind root)
     :pdf? pdf?
     :page-topic-id resolved-page-id
     :center-page center-page
     :window (settings/get-assistant-pdf-window user-id)}))

(defn- title-from
  "A short chat title derived from the learner's first message."
  [text]
  (let [one-line (-> text (str/replace #"\s+" " ") str/trim)]
    (if (> (count one-line) 48)
      (str (subs one-line 0 48) "…")
      one-line)))

(defn- grounding-system-suffix
  "System-prompt suffix grounding the persona in the document's approved KG facts.
   For a PDF the facts are limited to the current ±window pages (nearest first);
   otherwise the whole document's facts are used. Either way the count is capped
   at max-context-facts so a large document or code repo cannot overflow the
   prompt.

   The instruction wrapped around the facts is mode-conditional, because the two
   halves of the catalog want opposite things from them: a persona that withholds
   answers (Tutor, Socratic) must steer with the facts and never recite them, but
   General exists to state them. Emitting the ban to General would put two
   contradictory instructions in one system message.

   Pre:  ctx from resolve-context (root owned by user-id); `mode` is a registry
         entry (from modes/resolve-mode, so never nil).
   Post: a non-blank string iff ≥1 fact selected, else nil (persona unchanged)."
  [user-id ctx mode]
  (let [facts (if (and (:pdf? ctx) (:center-page ctx))
                (let [c (:center-page ctx) w (:window ctx)]
                  (->> (db/get-kg-facts-context user-id (:root-id ctx)
                         {:start-page (max 1 (- c w)) :end-page (+ c w)})
                    (sort-by #(Math/abs (- (int (or (:page_number %) c)) (int c))))
                    (take max-context-facts)))
                (db/get-kg-facts-context user-id (:root-id ctx)
                  {:limit max-context-facts}))]
    (when (seq facts)
      (let [mastery (db/get-fact-mastery user-id (mapv :id facts))
            weak (filterv #(cff/weak-fact? (get mastery (:id %))) facts)
            direct? (:answers-directly? mode)]
        (str "\n\n# Facts established in this document\n\n"
          (if direct?
            (str "Cite these as established by this document when they answer the "
              "learner's question. They carry no page number — say the document "
              "establishes them, without inventing a locator.\n\n")
            (str "Use these only to ground and steer your questions — do not recite them "
              "as answers.\n\n"))
          (cff/render-facts facts)
          (when (seq weak)
            (str "\n\n## Focus: facts the learner has not yet mastered\n\n"
              (if direct?
                (str "These most need practice — prefer them when they serve the "
                  "question.\n\n")
                (str "These most need practice — steer your questions toward them. Still "
                  "never recite them as answers.\n\n"))
              (cff/render-facts weak))))))))

(defn- mission-suffix
  "System-prompt segment naming the learner's goal for this document, so the tutor
   grounds its questions in that goal.
   Pre:  root-topic-id owned by user-id.
   Post: \"\" when no goal is set (caller composes suffixes with str), else a
         non-blank segment."
  [user-id root-topic-id]
  (let [goal (str/trim (or (settings/get-learning-goal user-id root-topic-id) ""))]
    (if (str/blank? goal)
      ""
      (str "\n\n# Learning goal\n\n"
        "The learner is studying this document to: " goal "\n"
        "Ground your questions in this goal; prioritize what serves it."))))

;; ── Message assembly ───────────────────────────────────────────────────────

;; ── Durable run assembly ───────────────────────────────────────────────────

;; One token can name several keys. The prompt asks for one key per token, but
;; models write `[evidence:E274, E361]` anyway. A pattern for one key only left
;; that text raw in the answer, with no link.
(def ^:private evidence-token-pattern
  #"\[evidence:\s*([A-Za-z0-9_-]+(?:\s*[,;]\s*(?:evidence:\s*)?[A-Za-z0-9_-]+)*)\s*\]")

(defn- token-keys
  "The citation keys in the key list of one evidence token, in order."
  [key-list]
  (map #(str/replace (str/trim %) #"^evidence:\s*" "")
    (str/split key-list #"[,;]")))

(def ^:private evidence-system-instructions
  ;; The last three sentences exist because the sources come before the
  ;; transcript (`answer-request`). An answer in the transcript keeps its raw
  ;; tokens. After a page change, its E3 named a block of the old page, but the
  ;; E3 in the sources above it is a block of the new page.
  (str "\n\n# Evidence citations\n\n"
    "Cite supplied sources only with their exact token, such as [evidence:E1]. "
    "Give each key its own token: write [evidence:E1] [evidence:E2], not [evidence:E1, E2]. "
    "Do not invent evidence keys. Use the narrowest source block that contains the claim. "
    "Place each citation after the claim it supports. "
    "Sources are supplied again for each request. "
    "An earlier answer can discuss text that is not supplied now. "
    "Its evidence keys then named other text, so never copy a key from an earlier answer."))

(def ^:private annotation-instructions
  ;; A system message in the request's tail, never part of the first system
  ;; message: that one is inside the cached prefix, and changing it for one
  ;; turn would miss the cache. Measured 2026-10-02 on Gemini 3.8 Flash: with
  ;; this text the reply had one section per segment in 9 of 9 runs over the
  ;; three modes, and in 0 of 6 without it. In the tail, 8 of 9 requests still
  ;; read the cache. plans/assistant-answer-selection-actions.md §3.
  (str "# Annotated segments\n\n"
    "The user's latest message quotes numbered segments of your earlier answers. "
    "A segment can carry a question.\n"
    "Reply with one section for each segment, in the order given.\n"
    "Start each section with a line that holds only its number and the quote, "
    "for example **[1] \"…\"**.\n"
    "In that section, answer the segment's question. "
    "When a segment has no question, explain that segment.\n"
    "This structure replaces the reply structure of your mode for this reply only. "
    "Keep your mode's teaching approach inside each section."))

(defn- side-instructions
  "The system message that turns a request into a side question about `passage`.
   It sits in the request's tail, after the shared head, for the reason given
   on `annotation-instructions`."
  [passage]
  (str "# Side question\n\n"
    "This is a side conversation, separate from the main chat. "
    "The user selected this passage of your previous answer:\n"
    "\"" passage "\"\n"
    "Answer only the side question about that passage. Keep the answer short. "
    "Do not continue the main lesson."))

(defn- source-revision
  "Stable revision text for one retained source snapshot."
  [topic]
  (some-> (or (:topics/updated_at topic) (:topics/created_at topic)) str))

(defn- evidence-key [index]
  (str "E" index))

(defn- evidence-excerpt [text]
  (let [text (or text "")]
    (subs text 0 (min 1000 (count text)))))

(defn- evidence-display-label [{:keys [section-title block-index]} page-number]
  (let [excerpt (str "excerpt " block-index)]
    (cond
      (and page-number section-title) (str "Page " page-number " · " section-title " · " excerpt)
      page-number (str "Page " page-number " · " excerpt)
      section-title (str section-title " · " excerpt)
      :else (str "Source " excerpt))))

(defn- blocks-evidence
  "Build evidence for `blocks` from `evidence-excerpt/evidence-blocks`.
   `:snapshot-content` is the text the model reads, and it is never HTML. A model
   copies an entity such as `&lt;` into its answer verbatim, and the answer's
   Markdown renderer then shows the entity instead of `<`. `:quoted-html` keeps
   the markup for the citation display."
  [blocks {:keys [document-topic-id page-topic-id page-number document-title
                  source-revision]} first-index]
  (mapv
    (fn [offset block]
      {:citation-key (evidence-key (+ first-index offset))
       :document-topic-id document-topic-id
       :page-topic-id page-topic-id
       :page-number page-number
       :anchor (:anchor block)
       :document-title document-title
       :display-label (evidence-display-label block page-number)
       :source-revision source-revision
       :quoted-text (:text block)
       :quoted-html (:html block)
       :snapshot-content (:text block)})
    (range)
    blocks))

(defn- content-evidence
  "Build block-level evidence for one retained HTML source."
  [content location first-index]
  (blocks-evidence (evidence-excerpt/evidence-blocks content) location first-index))

(defn- evidence-message-content [sources]
  (->> sources
    (map (fn [source]
           (str "Source " (:citation-key source) " — " (:document-title source)
             (when-let [page (:page-number source)] (str ", page " page))
             " — " (:display-label source) "\n"
             (:snapshot-content source))))
    (str/join "\n\n")))

(defn- repo-file?
  "True for a file imported from a git forge. Same test as topic-state's
   `:is-code?`: `kg-code` also writes kind code, with its source in `content`."
  [topic]
  (and (= "code" (:topics/kind topic)) (some? (:topics/repo_path topic))))

(defn- repo-file-html
  "The source of repo file `topic` as HTML that `content-evidence` can split, or
   nil for an empty leaf.
   A Markdown file renders as the code pane's rendered view does, so its blocks
   carry section titles and heading anchors. Its links stay unresolved, because
   the model reads only the text. Any other file is one escaped `pre`."
  [user-id topic]
  (when-let [{:keys [source lang]} (db/get-topic-code user-id (:topics/id topic))]
    (if (= "markdown" lang)
      (repo-md/render source (:topics/repo_path topic) (constantly {}) nil)
      (str "<pre>" (text/escape-html source) "</pre>"))))

(def ^:private max-repo-file-chars
  "Upper bound on the characters of one repo file's text in one run. A file is
   stored up to 1 MB (`repo-import/max-source-bytes`), and the smallest context
   in the model catalog is about 131K tokens. Source code runs near three
   characters per token, so this bound keeps the file under about 80K tokens and
   leaves the rest for the persona, the transcript and the answer. It counts the
   text the model reads, not the per-block source labels."
  240000)

(defn- within-budget
  "Split `blocks` where their `:text` passes `budget` characters.
   Post: [kept omitted], in order. `kept` is the longest prefix that fits, and
         it holds the first block, so a source is never sent as nothing."
  [blocks budget]
  (loop [kept [] total 0 [block & more :as remaining] blocks]
    (let [total' (+ total (count (:text block)))]
      (if (and block (or (empty? kept) (<= total' budget)))
        (recur (conj kept block) total' more)
        [kept (vec remaining)]))))

(def ^:private max-noted-sections
  "Most section names that a cut note lists."
  40)

(defn- cut-note
  "What the model reads after a source that `within-budget` cut, or nil when
   nothing was cut. `title` names the source: a repo file's path, a PDF page or
   an EPUB chapter. The note names the sections that were left out, wholly or
   in part, so the model says that a section is past what it read. Without the
   note, the model says that the source has no such section."
  [title kept omitted]
  (when (seq omitted)
    (let [chars #(reduce + (map (comp count :text) %))
          sent (chars kept)
          sections (distinct (keep :section-title omitted))
          listed (take max-noted-sections sections)
          unlisted (- (count sections) (count listed))]
      (str "Note: " title " is too long to read in one turn. The sources above are "
        "its first " sent " of " (+ sent (chars omitted)) " characters. "
        "The rest of it was not sent."
        (when (seq listed)
          (str " Sections not sent, wholly or in part: " (str/join "; " listed)
            (when (pos? unlisted) (str "; and " unlisted " more"))
            "."))
        " If the learner asks about a part that was not sent, say that it is past "
        "what you read. Do not say that the source does not contain it."))))

(defn- window-packet
  "Build block-level evidence and one rendered source turn for a window of
   numbered units: the pages of a PDF, or the chapters of an EPUB.
   Pre: each unit is {:number :html :label :location}. :location is the unit's
        `blocks-evidence` location, and :label names the unit in a cut note.
        A unit with blank html adds nothing.
   Post: {:messages :evidence :next-index}. Every source block has one evidence
        key. Units go nearest `center` first while their html fits in
        `max-window-chars`, and the turn shows them in number order. The
        nearest unit always goes. When it alone passes the budget, only its
        leading blocks that fit go (`within-budget`), and the turn ends with a
        `cut-note`, so the model does not take the cut for the end of the
        text."
  [units center first-index]
  (let [candidates (->> units
                     (keep (fn [unit]
                             (when-let [html (some-> (:html unit) str/trim not-empty)]
                               (assoc unit :html html))))
                     (sort-by #(Math/abs (- (int (:number %)) (int center)))))
        kept (->> candidates
               (reduce (fn [{:keys [units total]} unit]
                         (let [length (count (:html unit))]
                           (if (or (empty? units) (<= (+ total length) max-window-chars))
                             {:units (conj units unit) :total (+ total length)}
                             {:units units :total total})))
                 {:units [] :total 0})
               :units
               (sort-by :number))
        ;; Only the nearest unit can pass the budget, and then it is the only
        ;; unit kept, so cutting each unit on its own is exact.
        {:keys [evidence notes]}
        (reduce (fn [{:keys [evidence] :as acc} {:keys [html label location]}]
                  (let [[blocks omitted] (within-budget
                                           (evidence-excerpt/evidence-blocks html)
                                           max-window-chars)]
                    (-> acc
                      (update :evidence into
                        (blocks-evidence blocks location (+ first-index (count evidence))))
                      (update :notes conj (cut-note label blocks omitted)))))
          {:evidence [] :notes []}
          kept)
        content (evidence-message-content evidence)]
    {:messages (if (str/blank? content)
                 []
                 [{:role "user"
                   :content (str/join "\n\n" (cons content (remove nil? notes)))}])
     :evidence evidence
     :next-index (+ first-index (count evidence))}))

(defn- pdf-context-packet
  "Build the sources for a window of PDF pages (`window-packet`): the current
   PDF, or a PDF the learner @-references.
   Pre: ctx carries the PDF's :root-id, a :center-page and a :window.
   Post: {:messages :evidence :next-index}. The page budget matches the legacy
   reading window. A page with no text, such as one nobody scanned, adds
   nothing."
  [ctx root-title first-index]
  (let [center (:center-page ctx)
        window (:window ctx)]
    (window-packet
      (when center
        (map (fn [page]
               (let [n (:topics/page_number page)]
                 {:number n
                  :html (:topics/content page)
                  :label (str root-title ", page " n)
                  :location {:document-topic-id (:root-id ctx)
                             :page-topic-id (:topics/id page)
                             :page-number n
                             :document-title root-title
                             :source-revision (source-revision page)}}))
          (db/get-context-pages (:root-id ctx) (max 1 (- center window)) (+ center window))))
      center first-index)))

(defn- epub-reference-packet
  "Build the sources for an @-referenced EPUB `book` (`window-packet`).
   The root holds no text. Each chapter is a `basic` child, and its
   `page_number` is the chapter index (`db/insert-epub-topic!`).
   No reading position is stored for a book, so the window centers on the first
   chapter the learner has not marked done. When every chapter is done, it
   centers on chapter 1. The width is the learner's assistant window setting,
   counted in chapters.
   Each chapter is its own document, so a citation opens the chapter. A chapter
   index is not a page, so no label shows it."
  [user-id book first-index]
  (let [book-id (:topics/id book)
        book-title (or (:topics/title book) "Untitled")
        center (or (db/get-epub-reading-chapter book-id) 1)
        window (settings/get-assistant-pdf-window user-id)]
    (window-packet
      (map (fn [chapter]
             (let [title (str book-title " — "
                           (or (:topics/title chapter)
                             (str "Chapter " (:topics/page_number chapter))))]
               {:number (:topics/page_number chapter)
                :html (:topics/content chapter)
                :label title
                :location {:document-topic-id (:topics/id chapter)
                           :page-topic-id (:topics/id chapter)
                           :page-number nil
                           :document-title title
                           :source-revision (source-revision chapter)}}))
        (db/get-epub-chapters book-id (max 1 (- center window)) (+ center window)))
      center first-index)))

(def ^:private max-reference-file-chars
  "Upper bound on the characters of one @-referenced repo file. A run can carry
   several references beside the current topic, so a referenced file gets the
   budget of one PDF window (`max-window-chars`), not the budget of the file the
   learner reads (`max-repo-file-chars`)."
  max-window-chars)

(defn- topic-parts
  "The sources `topic` gives, in order, as {:blocks :location :note} parts.
   A repo file keeps its source in `topic_code` and the learner's notes about it
   in `topics.content`, so it gives two sources: the file, then the notes. Each
   has its own title, so the model can tell them apart. A repo file's
   `page_number` is its place in the tree, not a page, so no label shows it.
   Only the file has a budget, `file-budget` characters. When it is cut, its
   part carries a note that says so. Any other topic gives its content under
   `title`.
   The current topic and an @ reference both read through this fn, so a repo
   file reads the same way in both."
  [user-id topic document-topic-id title file-budget]
  (let [file? (repo-file? topic)
        path (:topics/repo_path topic)
        location {:document-topic-id document-topic-id
                  :page-topic-id (:topics/id topic)
                  :page-number (when-not file? (:topics/page_number topic))
                  :source-revision (source-revision topic)}
        notes (some-> (:topics/content topic) str/trim not-empty
                evidence-excerpt/evidence-blocks)]
    (if file?
      (let [[kept omitted] (within-budget
                             (some-> (repo-file-html user-id topic)
                               evidence-excerpt/evidence-blocks)
                             file-budget)]
        [{:blocks kept
          :location (assoc location :document-title path)
          :note (cut-note path kept omitted)}
         {:blocks notes
          :location (assoc location :document-title (str path " — learner's notes"))}])
      [{:blocks notes :location (assoc location :document-title title)}])))

(defn- add-parts
  "Append each part of `parts` to `packet` as its evidence and one rendered
   source turn, which ends with the part's note when it has one. A part whose
   blocks render blank adds nothing."
  [packet parts]
  (reduce
    (fn [{:keys [messages evidence next-index] :as packet} {:keys [blocks note] :as part}]
      (let [sources (blocks-evidence blocks (:location part) next-index)
            rendered (evidence-message-content sources)]
        (if (str/blank? rendered)
          packet
          {:messages (conj messages {:role "user"
                                     :content (cond-> rendered
                                                note (str "\n\n" note))})
           :evidence (into evidence sources)
           :next-index (+ next-index (count sources))})))
    packet
    parts))

(defn- topic-context-packet
  "Build block-level sources for the current non-PDF topic (`topic-parts`).
   A repo file here has the budget `max-repo-file-chars`."
  [user-id ctx root-title first-index]
  (add-parts {:messages [] :evidence [] :next-index first-index}
    (when-let [topic (some-> (:page-topic-id ctx) db/get-topic)]
      (topic-parts user-id topic (:root-id ctx) root-title max-repo-file-chars))))

(defn- one-reference-packet
  "The sources that one owned @-referenced `topic` gives, keyed from
   `first-index`. A referenced topic is its own document, so a citation opens it.
   A PDF root holds no text. Its text is in its pages, so it reads as the open
   PDF does: the learner's reading window, centered on the page they last read.
   A PDF they never opened centers on page 1. An EPUB root holds no text
   either, and reads as a window of its chapters (`epub-reference-packet`).
   Any other topic reads through `topic-parts`, and a repo file there has the
   budget `max-reference-file-chars`."
  [user-id topic first-index]
  (let [topic-id (:topics/id topic)
        title (or (:topics/title topic) "Untitled")]
    (case (:topics/kind topic)
      "pdf" (pdf-context-packet {:root-id topic-id
                                 :center-page (settings/get-last-page user-id topic-id)
                                 :window (settings/get-assistant-pdf-window user-id)}
              title first-index)
      "epub" (epub-reference-packet user-id topic first-index)
      (add-parts {:messages [] :evidence [] :next-index first-index}
        (topic-parts user-id topic topic-id title max-reference-file-chars)))))

(defn- reference-context-packet
  "Build retained block-level sources for owned @ references, in their order.
   A reference that gives no text is logged. The model then reads nothing for
   it and the learner sees no error, so the log is the only trace."
  [user-id ref-topic-ids first-index]
  (reduce
    (fn [{:keys [messages evidence next-index]} topic-id]
      (let [topic (db/get-topic-for-user user-id topic-id)
            one (if topic
                  (one-reference-packet user-id topic next-index)
                  {:messages [] :evidence [] :next-index next-index})]
        (when (empty? (:evidence one))
          (tel/log! {:level :warn :id ::reference-without-text
                     :data {:user-id user-id :topic-id topic-id
                            :kind (:topics/kind topic)}}
            "An @ reference gave the assistant no text"))
        {:messages (into messages (:messages one))
         :evidence (into evidence (:evidence one))
         :next-index (:next-index one)}))
    {:messages [] :evidence [] :next-index first-index}
    (or ref-topic-ids [])))

(defn- run-source-packet
  "Resolve grounded fact evidence, current reading sources, and @ references.
   Keys follow the order of the request: the facts, then the current topic,
   then the references. The facts message names its own key, so a key numbered
   after any source would change that message whenever the source did.
   Post: every source shown to the model has one retained evidence record.
   :current-messages are the current topic's source turns, and
   :reference-messages are the @ references' source turns."
  [user-id ctx mode ref-topic-ids]
  (let [root (db/get-topic (:root-id ctx))
        root-title (or (:topics/title root) "Untitled")
        grounding (grounding-system-suffix user-id ctx mode)
        facts-source (when-not (str/blank? grounding)
                       {:citation-key (evidence-key 1)
                        :document-topic-id (:root-id ctx)
                        :page-topic-id (:page-topic-id ctx)
                        :page-number (:center-page ctx)
                        :document-title root-title
                        :display-label "document-level evidence"
                        :source-revision (source-revision root)
                        :quoted-text (evidence-excerpt grounding)
                        :snapshot-content grounding})
        first-source-index (if facts-source 2 1)
        current (if (:pdf? ctx)
                  (pdf-context-packet ctx root-title first-source-index)
                  (topic-context-packet user-id ctx root-title first-source-index))
        refs (reference-context-packet user-id ref-topic-ids (:next-index current))]
    {:current-messages (:messages current)
     :reference-messages (:messages refs)
     :evidence (-> (if facts-source [facts-source] [])
                 (into (:evidence current))
                 (into (:evidence refs)))
     :grounding-suffix (when grounding
                         (str grounding "\n\nEvidence key for these established facts: "
                           "[evidence:" (:citation-key facts-source) "]."))}))

(defn- sendable-rows
  "Message `rows` after message id `after-id` that a request carries. Blank
   assistant placeholders never enter a request."
  [rows after-id]
  (filterv #(and (> (:assistant_messages/id %) (or after-id 0))
              (or (= "user" (:assistant_messages/role %))
                (not (str/blank? (:assistant_messages/content %)))))
    rows))

(defn- provider-turns
  "The provider turns of message `rows` after message id `after-id`."
  [rows after-id]
  (mapv (fn [message]
          {:role (:assistant_messages/role message)
           :content (:assistant_messages/content message)})
    (sendable-rows rows after-id)))

(defn- chat-node
  "A chat row as freememo.assistant-chat-tree reads it."
  [{:keys [id parent_chat_id fork_message_id title excluded activity_ms]}]
  {:id id :parent-id parent_chat_id :fork-message-id fork_message_id
   :title (or title "Untitled") :excluded? (boolean excluded) :activity-ms activity_ms})

(defn- chat-lineage
  "id → chat node for `chat` and its ancestors, or {} for a root chat, which
   reads no lineage."
  [user-id chat]
  (if (:assistant_chats/parent_chat_id chat)
    (into {} (map (juxt :id chat-node))
      (db/get-assistant-chat-lineage user-id (:assistant_chats/id chat)))
    {}))

(defn- inherited-path
  "`chat`'s inherited path (plans/assistant-chat-forks.md §4.2.1)."
  [user-id chat]
  (tree/inherited-path (chat-lineage user-id chat) (:assistant_chats/id chat)))

(defn- inherited-rows
  "The message rows that `path` names, oldest first."
  [path]
  (into [] (mapcat (fn [[chat-id through]] (db/get-assistant-messages chat-id through))) path))

(defn- checkpoint-message [heading checkpoint]
  {:role "system" :content (str "# " heading "\n\n" (:summary checkpoint))})

(defn- request-transcript
  "The conversation part of a request about `chat` (§4.2.2).

   The order is the inherited checkpoint, the inherited rows after it, the own
   checkpoint, and the own rows after that. `:head-messages` is the first
   checkpoint, which goes in the head's checkpoint slot before the sources, as
   a root chat's checkpoint always has. `:messages` is the rest, which follows
   the sources.
   Post: for a root chat, {:head-messages [own checkpoint?] :messages own
         rows}, the same request as before forks existed. For a child,
         {:head-messages [inherited checkpoint?] :messages inherited rows, own
         checkpoint?, own rows}."
  [user-id chat]
  (let [chat-id (:assistant_chats/id chat)
        path (inherited-path user-id chat)
        own-checkpoint (db/latest-assistant-checkpoint user-id chat-id "own")
        own (provider-turns (db/get-assistant-messages chat-id)
              (:covered_through_message_id own-checkpoint))
        own-message (some->> own-checkpoint (checkpoint-message "Conversation checkpoint"))]
    (if (empty? path)
      {:head-messages (cond-> [] own-message (conj own-message))
       :messages own}
      (let [inherited-checkpoint (db/latest-assistant-checkpoint user-id chat-id "inherited")]
        {:head-messages (cond-> []
                          inherited-checkpoint
                          (conj (checkpoint-message
                                  "Checkpoint of the conversation this chat forked from"
                                  inherited-checkpoint)))
         :messages (-> (provider-turns (inherited-rows path)
                         (:covered_through_message_id inherited-checkpoint))
                     (cond-> own-message (conj own-message))
                     (into own))}))))

(defn- with-cache-breakpoint
  "`messages` with a `cache_control` breakpoint on the last one. Its text
   becomes one text part, so the text the model reads does not change.
   OpenRouter uses only the last breakpoint for Gemini, and caches every
   message up to it. Empty `messages` stay empty."
  [messages]
  (if (seq messages)
    (update messages (dec (count messages))
      (fn [{:keys [content] :as message}]
        (assoc message :content [{:type "text" :text content
                                  :cache_control {:type "ephemeral"}}])))
    messages))

(defn- request-head
  "The messages every request about `chat` starts with, and the sources they show.

   A provider caches the start of a request that repeats the previous one, so
   the stable parts come first: instructions, facts, checkpoint, and the current
   topic's sources. The transcript after the checkpoint follows. A main answer
   and a side question both start with this head, so a side question reads the
   cache the main chat wrote.
   Gemini caches only up to an explicit breakpoint. When `cache-breakpoint?`,
   the last current-topic source turn carries one. Every system message here
   comes before it, because Gemini fixes its system instruction with the cache.
   The one exception is a fork's own checkpoint, which `request-transcript`
   puts after the inherited rows it follows in time. It is a system message in
   the tail, as the per-turn instructions of `answer-request` are.
   See plans/assistant-request-prefix-cache.md.

   Post: {:messages head :reference-messages refs :evidence sources}. The
         reference turns are separate: they belong to one turn, not the head."
  [user-id chat page-topic-id ref-topic-ids cache-breakpoint?]
  (let [root-topic-id (:assistant_chats/root_topic_id chat)
        mode (modes/resolve-mode (:assistant_chats/mode chat))
        ctx (resolve-context user-id root-topic-id page-topic-id)
        sources (run-source-packet user-id ctx mode ref-topic-ids)
        transcript (request-transcript user-id chat)
        system-content (str (mode-system-prompt (:id mode))
                         (mission-suffix user-id root-topic-id)
                         evidence-system-instructions)
        facts-message (when-let [grounding (:grounding-suffix sources)]
                        {:role "system" :content grounding})]
    {:messages (-> [{:role "system" :content system-content}]
                 (cond-> facts-message (conj facts-message))
                 (into (:head-messages transcript))
                 (into (cond-> (:current-messages sources)
                         cache-breakpoint? with-cache-breakpoint))
                 (into (:messages transcript)))
     :reference-messages (:reference-messages sources)
     :evidence (:evidence sources)}))

(def turn-kinds
  "What kind of turn a user message is. nil is an ordinary turn. `:annotated`
   quotes numbered segments of earlier answers (`annotations/user-message`).
   `:question-pins` is an Ask over a file's open pins
   (`question-map/open-pins-message`, plans/anchored-questions.md §6.2.5)."
  #{nil :annotated :question-pins})

(defn document-ref-for
  "The repository file a run about `page-topic-id` is about: the file a
   walkthrough screen shows on that repository node, else the page itself
   when it is a repository file (plans/commit-range-walkthrough.md §6.3).
   Post: a document ref, or nil."
  [user-id page-topic-id]
  (or (walkthrough/focused-ref user-id page-topic-id)
    (docref/for-topic user-id page-topic-id)))

(defn- branch-content
  "The text of one branch's system message (§4.4.2). An answer's evidence
   tokens are removed, because they name sources of another run."
  [titles fork-text checkpoint turns]
  (str "# Branch " (str/join " › " (map #(str "\"" % "\"") titles))
    ", forked at \"" (tree/excerpt (answer-context-text fork-text) 120) "\"\n\n"
    "This is a separate conversation that branched from this chat. "
    "Use what it found when it helps the learner. Its sources are not supplied "
    "now, so do not cite them."
    (when checkpoint
      (str "\n\n## Checkpoint\n\n" (:summary checkpoint)))
    (when (seq turns)
      (str "\n\n## Transcript\n\n"
        (str/join "\n\n"
          (map (fn [{:keys [role content]}]
                 (str (if (= "user" role) "Learner: " "Assistant: ")
                   (answer-context-text content)))
            turns))))))

(defn- branch-messages
  "The branch step of an answer in `chat-id` (§4.4): one system message for
   each included descendant, in `tree/included-branches` order. A branch
   contributes its own checkpoint and its own rows after it, never what it
   inherited, so no turn reaches the request twice.
   Post: a vector, empty for a chat with no included descendant."
  [user-id chat-id]
  (let [descendants (map chat-node (db/get-assistant-chat-descendants user-id chat-id))]
    (mapv (fn [{branch-id :chat-id :keys [titles fork-message-id]}]
            (let [checkpoint (db/latest-assistant-checkpoint user-id branch-id "own")]
              {:role "system"
               :content (branch-content titles
                          (:content (db/get-assistant-message user-id fork-message-id))
                          checkpoint
                          (provider-turns (db/get-assistant-messages branch-id)
                            (:covered_through_message_id checkpoint)))}))
      (tree/included-branches descendants chat-id))))

(defn- answer-request
  "Render the exact provider request for one new answer: `request-head`, then
   the branch step (`branch-messages`), this turn's @ references, the annotation instructions when the turn carries
   annotations, the walkthrough message while a walkthrough screen is open on
   the page, the question-map message when the page's file has a map or the
   turn is an Ask, and the new user turn. While the learner stays on one page,
   the previous request is a prefix of this one, minus its tail.
   Post: also :reply-blocks, {:question-map {:map-id :diagnosis? :ref}
         :walkthrough {:walkthrough-id :repo-id :as-of-sha}} with each key
         present when the request carried that message, or nil for neither.
         The run worker applies the reply's leading block to each.
         The walkthrough's evidence follows the head's, numbered after it."
  ([user-id chat page-topic-id user-text ref-topic-ids model-fields
    web-search-supported? cache-breakpoint?]
   (answer-request user-id chat page-topic-id user-text ref-topic-ids model-fields
     web-search-supported? cache-breakpoint? nil))
  ([user-id chat page-topic-id user-text ref-topic-ids model-fields
    web-search-supported? cache-breakpoint? turn]
   {:pre [(contains? turn-kinds turn)]}
   (let [head (request-head user-id chat page-topic-id ref-topic-ids cache-breakpoint?)
         diagnosis? (= :question-pins turn)
         ref (document-ref-for user-id page-topic-id)
         walk-tail (walkthrough/request-tail user-id page-topic-id
                     (inc (count (:evidence head))))
         map-tail (question-map/request-tail user-id ref diagnosis?
                    (mode-system-prompt "tutor"))
         evidence (into (vec (:evidence head)) (:evidence walk-tail))
         messages (-> (:messages head)
                    (into (branch-messages user-id (:assistant_chats/id chat)))
                    (into (:reference-messages head))
                    (cond-> (= :annotated turn)
                      (conj {:role "system" :content annotation-instructions}))
                    (cond-> walk-tail
                      (conj {:role "system" :content (:content walk-tail)}))
                    (cond-> map-tail
                      (conj {:role "system" :content (:content map-tail)}))
                    (conj {:role "user" :content user-text}))]
     {:provider-request (cond-> (assoc model-fields
                                  :stream true
                                  :session_id (:assistant_chats/provider_session_id chat)
                                  :messages messages)
                          (and (:assistant_chats/web_search chat)
                            web-search-supported?)
                          (merge (openrouter/web-search-fields)))
      :source-snapshot evidence
      :evidence evidence
      :reply-blocks (when (or map-tail walk-tail)
                      (cond-> {}
                        map-tail (assoc :question-map {:map-id (:map-id map-tail)
                                                       :diagnosis? diagnosis? :ref ref})
                        walk-tail (assoc :walkthrough (:run walk-tail))))})))

(defn- side-request
  "Render the provider request for one side-question turn: `request-head`, the
   side instructions about `passage`, then the side thread `turns`, whose last
   turn is the new question. Web search stays off: a side question is about a
   passage already on screen.
   Pre:  `turns` is a non-empty vector of {:role :content}, ending with a user turn."
  [user-id chat page-topic-id passage turns model-fields cache-breakpoint?]
  (let [head (request-head user-id chat page-topic-id nil cache-breakpoint?)]
    {:provider-request (assoc model-fields
                         :stream true
                         :session_id (:assistant_chats/provider_session_id chat)
                         :messages (-> (:messages head)
                                     (conj {:role "system"
                                            :content (side-instructions passage)})
                                     (into turns)))
     :evidence (:evidence head)}))

(defn citation-target
  "Relative viewer target for one retained evidence record."
  [source]
  (evidence/citation-target source))

(defn- markdown-link-label [label]
  (-> (or label "document-level evidence")
    (str/replace "\\" "\\\\")
    (str/replace "[" "\\[")
    (str/replace "]" "\\]")))

(defn- web-sources-markdown
  "Render provider URL citations outside retained document evidence.
   Post: blank means the provider returned no usable web citations."
  [sources]
  (when (seq sources)
    (str "\n\n### Web sources\n\n"
      (str/join "\n"
        (map (fn [{:keys [title url]}]
               (str "- [" (markdown-link-label title) "](" url ")"))
          sources)))))

(defn render-evidence-citations
  "Replace valid evidence tokens with Markdown links and remove invalid tokens.
   Pre: evidence belongs to the answer run. Post: every rendered citation has a
   retained target. The assistant panel opens retained evidence in a dialog."
  [answer evidence]
  (let [by-key (into {} (map (juxt :citation_key identity)) evidence)]
    (str/replace (or answer "") evidence-token-pattern
      (fn [[_ key-list]]
        (str/join "; "
          (keep (fn [citation-key]
                  (when-let [source (get by-key citation-key)]
                    (when-let [target (citation-target source)]
                      (str "[" (markdown-link-label (:display_label source))
                        "](" target ")"))))
            (token-keys key-list)))))))

;; ── Commands ───────────────────────────────────────────────────────────────

(defn- start-chat!
  "Create a chat titled `title` for (user-id, root-topic-id) in `mode-id`.
   Returns the new chat id. It bumps nothing: its one caller sends the first
   message at once, and that send bumps.
   Pre: root-topic-id belongs to user-id. A violation throws because the caller
   supplied a foreign document. `mode-id` can be nil or unrecognized.
   Post: the row's mode is a canonical registry :id, never nil and never junk."
  [user-id root-topic-id mode-id web-search? title]
  (when-not (db/get-topic-for-user user-id root-topic-id)
    (throw (ex-info "Cannot create a chat for a document the user does not own."
             {:type ::foreign-chat-document :user-id user-id
              :root-topic-id root-topic-id})))
  (let [entry (or (card-models/resolve-model
                    (settings/get-assistant-model user-id))
                  (card-models/resolve-model settings/assistant-default-model-id))
        web-search? (and web-search?
                         (card-models/web-search-supported? (:id entry)))]
    (db/create-assistant-chat! user-id root-topic-id title
      (:id (modes/resolve-mode mode-id)) web-search?)))

(def ^:private max-chat-title-chars
  "Longest title `rename-chat!` stores. A title is one line in a narrow panel,
   so this bounds a paste, not a typed name."
  200)

(defn rename-chat!
  "Rename an owned live chat.
   Pre: none — the title comes from a text field and is cleaned here.
   Post: a blank title is a no-op returning {:success true :changed? false}.
   Otherwise the trimmed title, cut to 200 characters, is stored and
   :rename-assistant-chat is bumped. A foreign or staged chat returns an error
   and changes no row."
  [user-id chat-id title]
  (let [cleaned (str/trim (str title))
        cleaned (subs cleaned 0 (min (count cleaned) max-chat-title-chars))]
    (cond
      (str/blank? cleaned) {:success true :changed? false}
      (db/set-assistant-chat-title! user-id chat-id cleaned)
      (do (commands/bump! user-id :rename-assistant-chat)
          {:success true :changed? true})
      :else {:success false :error "Chat not found."})))

(defn set-chat-pinned!
  "Pin or unpin an owned live chat.
   Post: pinned_at is now() or NULL; updated_at is untouched.
   :set-assistant-chat-pinned is bumped on success."
  [user-id chat-id pinned?]
  (if (db/set-assistant-chat-pinned! user-id chat-id (boolean pinned?))
    (do (commands/bump! user-id :set-assistant-chat-pinned)
        {:success true})
    {:success false :error "Chat not found."}))

(defn mark-chat-opened!
  "Record that the user opened an owned live chat, for the last-chat seed.
   Bumps nothing: :mark-assistant-chat-opened declares no views, because no
   view orders or filters on last_opened_at."
  [user-id chat-id]
  (if (db/mark-assistant-chat-opened! user-id chat-id)
    (do (commands/bump! user-id :mark-assistant-chat-opened)
        {:success true})
    {:success false :error "Chat not found."}))

(defn stage-chat-deletion!
  "Delete an owned chat and its forks with a 12-hour undo window.
   Pre: none — a foreign, missing or already staged chat returns an error.
   Post: the chat and its live descendants are hidden from every chat read, no
   run in them is active, one live undo entry names them, and an Undo toast
   that counts the branches is queued (plans/assistant-chat-forks.md §4.1.5). A run that was
   streaming is stopped first: its provider request keeps draining and its
   credits are spent, but no further text reaches the chat."
  [user-id chat-id]
  (if-let [{:keys [entry-id count]} (db/stage-assistant-chat-for-deletion! user-id chat-id)]
    (do
      (toasts/push! user-id
        {:level :success
         :message (case (dec count)
                    0 "Chat deleted. You can undo this for 12 hours."
                    1 "Chat and its 1 branch deleted. You can undo this for 12 hours."
                    (str "Chat and its " (dec count) " branches deleted. "
                      "You can undo this for 12 hours."))
         :dedup? false
         :actions [{:label "Undo" :undo-id entry-id}]})
      (commands/bump! user-id :delete-assistant-chat)
      {:success true})
    {:success false :error "Chat not found."}))

(defn set-chat-excluded!
  "Exclude an owned fork from its parent's answers, or include it again
   (plans/assistant-chat-forks.md §4.6.3). Excluding a chat also excludes its
   descendants, because the branch walk stops at it.
   Post: {:success true} and :set-assistant-chat-excluded bumped, or {:error}
         for a root, foreign or staged chat."
  [user-id chat-id excluded?]
  (if (db/set-assistant-chat-excluded! user-id chat-id (boolean excluded?))
    (do (commands/bump! user-id :set-assistant-chat-excluded)
        {:success true})
    {:success false :error "Only a forked chat can be excluded."}))

(defn chat-branch-state*
  "[chat-id excluded?] when `chat-id` is a live owned fork, else nil. The
   palette reads it to offer Exclude or Include. `_rev` forces a re-query."
  [_rev user-id chat-id]
  (when-let [chat (and chat-id (db/get-assistant-chat user-id chat-id))]
    (when (:assistant_chats/parent_chat_id chat)
      [chat-id (some? (:assistant_chats/excluded_at chat))])))

(defn apply-chat-history-action!
  "Run one request from the history view (freememo.assistant-chat-history).
   Pre: `op` ∈ #{:open :rename :pin :delete :exclude}; :rename carries :title,
   :pin carries :pinned?, :exclude carries :excluded?.
   Post: the matching operation's result. A failed rename, pin or delete is
   also reported as an error toast, because the view that asked may be closed
   by the time it fails. A failed :open is silent: it only feeds the seed."
  [user-id {:keys [op chat-id title pinned? excluded?]}]
  (let [result (case op
                 :open (mark-chat-opened! user-id chat-id)
                 :rename (rename-chat! user-id chat-id title)
                 :pin (set-chat-pinned! user-id chat-id pinned?)
                 :delete (stage-chat-deletion! user-id chat-id)
                 :exclude (set-chat-excluded! user-id chat-id excluded?))]
    (when (and (not (:success result)) (not= :open op))
      (toasts/push! user-id {:level :error :message (:error result)}))
    result))

(defn last-opened-chat-id*
  "The chat the history seed reopens on this document, or nil."
  [user-id root-topic-id]
  (db/get-last-opened-assistant-chat-id user-id root-topic-id))

(defn chat-liveness*
  "[chat-id live?] for the panel's active chat. `_rev` forces a re-query on
   every :assistant-mutations bump, which is how a delete in another tab
   reaches this one. Post: nil for a nil chat-id; otherwise live? is a boolean,
   false for a missing, foreign or staged chat. The id rides along so the
   caller acts only on the answer for the chat it still shows."
  [_rev user-id chat-id]
  (when chat-id
    [chat-id (some? (db/get-assistant-chat user-id chat-id))]))

(defn set-chat-mode!
  "Switch `chat-id`'s persona to `mode-id`.
   Pre:  none — both a foreign chat-id and an unrecognized mode-id are rejected
         here rather than trusted (the client offers both).
   Post: on success the chat's mode = mode-id and :assistant-mutations is bumped
   so the panel's pills and chat list re-query. The transcript is never touched:
   past turns keep whatever their own persona produced.
   Returns {:success true} | {:success false :error s}."
  [user-id chat-id mode-id]
  (cond
    (not (modes/known-id? mode-id))
    {:success false :error (str "Unknown assistant mode: " mode-id)}

    (nil? (db/get-assistant-chat user-id chat-id))
    {:success false :error "Chat not found."}

    :else
    (try
      (db/set-assistant-chat-mode! user-id chat-id mode-id)
      (bump! user-id)
      {:success true}
      (catch Exception e
        (tel/error! {:id ::set-chat-mode :data {:user-id user-id :chat-id chat-id
                                                :mode mode-id}} e)
        {:success false :error "Failed to switch mode."}))))

(defn set-chat-web-search!
  "Set an owned chat's web-search preference.
   Pre: `web-search?` is boolean. Post: enabled search always has a selected
   model that accepts the native tool. Unsupported models leave the row intact."
  [user-id chat-id web-search?]
  (if-let [chat (db/get-assistant-chat user-id chat-id)]
    (let [root-topic-id (:assistant_chats/root_topic_id chat)
          entry (or (card-models/resolve-model
                      (settings/get-assistant-model user-id))
                    (card-models/resolve-model settings/assistant-default-model-id))]
      (if (and web-search?
               (not (card-models/web-search-supported? (:id entry))))
        {:success false :error "This model does not support web search."}
        (try
          (db/set-assistant-chat-web-search! user-id chat-id web-search?)
          (bump! user-id)
          {:success true}
          (catch Exception error
            (tel/error! {:id ::set-chat-web-search
                         :data {:user-id user-id :chat-id chat-id
                                :web-search? web-search?}} error)
            {:success false :error "Failed to change web search."}))))
    {:success false :error "Chat not found."}))

(defonce ^:private assistant-run-executor
  (Executors/newFixedThreadPool 4))

(defn- model-entry
  "Resolve the user's assistant model. `root-topic-id` is unused since documents
   stopped overriding the model (2026-09-23); callers still pass it."
  [user-id _root-topic-id]
  (let [model-id (settings/get-assistant-model user-id)]
    (or (card-models/resolve-model model-id)
      (card-models/resolve-model settings/assistant-default-model-id))))

(defn- poll-generation-metadata
  "Fetch finalized metadata after a stream ended without complete usage.
   Post: normalized facts or nil after bounded retries."
  [api-key generation-id user-id]
  (when generation-id
    (loop [[delay-ms & remaining] [0 250 1000 3000]]
      (when delay-ms
        (when (pos? delay-ms) (Thread/sleep (long delay-ms)))
        (let [{:keys [metadata error]}
              (try
                {:metadata (openrouter/generation-metadata! api-key generation-id
                             {:feature :assistant-metadata :user-id user-id})}
                (catch Exception error
                  {:error error}))]
          (if metadata
            metadata
            (do
              (when (empty? remaining)
                (tel/log! {:level :warn :id ::metadata-reconciliation-failed
                           :data {:user-id user-id :generation-id generation-id
                                  :error (ex-message error)}}
                  "Assistant metadata reconciliation failed"))
              (recur remaining))))))))

(defn- charge-run!
  "Record one provider-reported run charge in the independent billing ledger.
   `metadata` carries the run's token counts under openrouter/token-keys."
  [user-id endpoint model-id metadata]
  (when-some [cost (:cost-usd metadata)]
    (credits/record-cost-charge! user-id endpoint model-id (double cost)
      [(select-keys metadata openrouter/token-keys)])))

(def ^:private map-only-reply
  "What the transcript stores when a reply was only a map block. A diagnosis
   must ask a question after its block, so this is a model that did not."
  "I updated the question map.")

(defn- apply-question-map-reply!
  "Write the map block `response` starts with into the run's question map, and
   store the reply's prose without the block (plans/anchored-questions.md
   §6.3.2). A block that cannot be applied writes nothing and tells the reader
   in a toast. Never throws: the map is a side effect of the answer, and a
   failure here must not fail the answer the reader already watched stream."
  [user-id run-uuid question-map-run response]
  (try
    (let [{:keys [prose problem]} (question-map/apply-reply! user-id
                                    (assoc question-map-run :run-uuid run-uuid)
                                    response)]
      (when (not= prose response)
        (db/replace-assistant-run-text! run-uuid
          (if (str/blank? prose) map-only-reply prose)))
      (when problem
        (tel/log! {:level :warn :id ::question-map-not-applied
                   :data {:user-id user-id :run-uuid run-uuid :problem problem}})
        (toasts/push! user-id {:level :warn
                               :message (str "The question map was not updated. " problem)})))
    (catch Throwable t
      (tel/error! {:id ::question-map-apply :data {:user-id user-id :run-uuid run-uuid}} t)
      (toasts/push! user-id {:level :warn
                             :message "The question map was not updated. Try asking again."}))))

(defn- apply-walkthrough-reply!
  "Record the ideas the block `response` starts with lists as known, with the
   walkthrough's version (plans/commit-range-walkthrough.md §6.7.1). When no
   question map also reads the block, store the prose without it. Never
   throws, for the reason `apply-question-map-reply!` gives."
  [user-id run-uuid walkthrough-run response replace-text?]
  (try
    (let [{:keys [prose problem]} (walkthrough/apply-reply! user-id walkthrough-run response)]
      (when (and replace-text? (not= prose response))
        (db/replace-assistant-run-text! run-uuid
          (if (str/blank? prose) "I noted what you know." prose)))
      (when problem
        (tel/log! {:level :warn :id ::walkthrough-known-not-applied
                   :data {:user-id user-id :run-uuid run-uuid :problem problem}})))
    (catch Throwable t
      (tel/error! {:id ::walkthrough-known-apply :data {:user-id user-id :run-uuid run-uuid}} t))))

(defn- apply-reply-blocks!
  "Apply the leading block of `response` to each reader of `reply-blocks`."
  [user-id run-uuid {:keys [question-map walkthrough]} response]
  (when walkthrough
    (apply-walkthrough-reply! user-id run-uuid walkthrough response (nil? question-map)))
  (when question-map
    (apply-question-map-reply! user-id run-uuid question-map response)))

(def oversized-answer-error
  "The error of an answer that does not fit after every compaction (§4.5.7)."
  (str "This chat and its branches are too large even after compaction. "
    "Exclude a branch to continue."))

(defn context-overflow-error?
  "True when a provider error says the request is longer than the model's
   context. No overflow reply from OpenRouter has been captured yet
   (plans/assistant-chat-forks.md §5, item 6), so these patterns cover the
   wordings providers are known to use, and HTTP 413.
   Pre: `error` is a Throwable, or nil."
  [error]
  (let [message (str/lower-case (or (ex-message error) ""))]
    (or (= 413 (:status (ex-data error)))
      (boolean (re-find (re-pattern (str "context.{0,12}(length|window|limit)"
                                      "|maximum context|too many tokens"
                                      "|prompt is too long|input is too long"
                                      "|exceeds? the (maximum|model)"))
                 message)))))

(declare claim-preparation! retry-after-overflow!)

(defn- execute-provider-run!
  "Drain one provider stream and settle its durable answer or checkpoint.

   Pre: run is queued and provider-request is its immutable request snapshot.
   `compact-range` is {:from :through :scope :for-chat-id} for a compaction.
   `answer` is the answer's parameters (`start-answer!`) for an answer that may
   be rebuilt, else nil.
   Post: the run is completed, stopped, or failed. Provider metadata can update
   every state. Billing uses the final known cost once in this worker. An
   answer that fails because the request is longer than the model's context
   is compacted once and sent again as a new request (§4.5.6).
   Invariant: on-delta writes through the database's active-state predicate.
   This worker never writes late response text after Stop."
  [user-id chat-id run-uuid run-kind api-key model-id provider-request
   compact-range reply-blocks answer]
  (let [started-ns (System/nanoTime)
        metadata* (volatile! {})
        charged?* (volatile! false)
        publish-count* (volatile! 0)
        charge-known! (fn [metadata]
                        (when (and (some? (:cost-usd metadata))
                                (not @charged?*))
                          (charge-run! user-id
                            (if (= "compact" run-kind)
                              :assistant.compact :assistant.chat)
                            model-id metadata)
                          (vreset! charged?* true)))
        last-publish-ns* (volatile! 0)
        publish! (fn []
                   (let [now (System/nanoTime)]
                     (when (> (- now (long @last-publish-ns*)) 100000000)
                       (vreset! last-publish-ns* now)
                       (vswap! publish-count* inc)
                       (bump-stream! user-id))))]
    (try
      (let [result (openrouter/chat-completion-stream!
                     api-key provider-request
                     {:feature (if (= "compact" run-kind)
                                 :assistant-compact :assistant)
                      :user-id user-id}
                     (fn [delta]
                       (when (db/append-assistant-run-text! run-uuid delta)
                         (publish!)))
                     (fn [metadata]
                       (vswap! metadata* merge metadata)
                       (db/update-assistant-run-metadata! run-uuid metadata)))
            reported @metadata*
            reconciled (if (and (:generation-id reported)
                             (or (nil? (:cost-usd reported))
                               (nil? (:provider-name reported))))
                         (or (poll-generation-metadata
                               api-key (:generation-id reported) user-id) {})
                         {})
            metadata (merge reported reconciled)
            response (:text result)
            sources-markdown (web-sources-markdown (:web-sources result))]
        (db/update-assistant-run-metadata! run-uuid metadata)
        (if (str/blank? response)
          (db/fail-assistant-run! run-uuid "The model returned an empty response.")
          (if (= "compact" run-kind)
            (db/complete-assistant-compact-run! run-uuid (str/trim response)
              (:from compact-range) (:through compact-range)
              (:scope compact-range "own") (:for-chat-id compact-range))
            (do
              (when reply-blocks
                (apply-reply-blocks! user-id run-uuid reply-blocks response))
              (when sources-markdown
                (db/append-assistant-run-text! run-uuid sources-markdown))
              (db/complete-assistant-answer-run! run-uuid))))
        (charge-known! metadata)
        (bump! user-id)
        (let [run (db/get-assistant-run user-id run-uuid)]
          (tel/log! {:level :info :id ::assistant-run-terminal
                     :data {:user-id user-id :chat-id chat-id
                            :run-uuid run-uuid :kind run-kind
                            :model-id model-id :status (:status run)
                            :outcome :ok
                            :duration-ms (quot (- (System/nanoTime) started-ns)
                                           1000000)
                            :web-search-requests
                            (:web-search-requests metadata)
                            ;; Loki can then show prompt-cache reads, which
                            ;; were otherwise only in the database.
                            :input-tokens (:input-tokens metadata)
                            :cached-tokens (:cached-tokens metadata)
                            :cache-write-tokens (:cache-write-tokens metadata)
                            :cost-usd (:cost-usd metadata)
                            :response-chars (count (or (:response_text run) ""))
                            :publish-count @publish-count*}}
            "Assistant run reached a terminal state"))
        {:success true})
      (catch Throwable error
        (let [known @metadata*
              reconciled (or (poll-generation-metadata
                               api-key (:generation-id known) user-id) {})
              metadata (merge known reconciled)
              ;; Claimed before the run fails, so the chat never reads as idle
              ;; between the failure and the retry: a send in that gap would
              ;; take the turn the retry is about to write.
              retry? (and answer (= "answer" run-kind) (not (:overflow-retried? answer))
                       (context-overflow-error? error)
                       (claim-preparation! chat-id))]
          (db/update-assistant-run-metadata! run-uuid metadata)
          (charge-known! metadata)
          (db/fail-assistant-run! run-uuid
            (or (ex-message error) "Assistant request failed."))
          (tel/error! {:id ::assistant-run
                       :data {:user-id user-id :chat-id chat-id
                              :run-uuid run-uuid :kind run-kind
                              :model-id model-id :status "failed"
                              :outcome :error
                              :duration-ms (quot (- (System/nanoTime) started-ns)
                                             1000000)
                              :response-chars
                              (count (or (:response_text
                                           (db/get-assistant-run user-id run-uuid))
                                       ""))
                              :publish-count @publish-count*}} error)
          (bump! user-id)
          (when retry?
            (retry-after-overflow! answer api-key run-uuid))
          {:success false :error (ex-message error)})))))

(defn- start-provider-run!
  "Submit one accepted run to the bounded assistant executor.
   Post: returns immediately. The worker owns every terminal transition."
  [user-id chat-id run-uuid run-kind api-key model-id provider-request compact-range
   reply-blocks answer]
  (tel/log! {:level :info :id ::assistant-run-started
             :data {:user-id user-id :chat-id chat-id :run-uuid run-uuid
                    :kind run-kind :model-id model-id}}
    "Assistant run started")
  ((m/via assistant-run-executor
     (execute-provider-run! user-id chat-id run-uuid run-kind api-key model-id
       provider-request compact-range reply-blocks answer))
   (fn [_result] nil)
   (fn [error]
     (db/fail-assistant-run! run-uuid
       (or (ex-message error) "Assistant request failed."))
     (tel/error! {:id ::assistant-run-executor
                  :data {:user-id user-id :chat-id chat-id
                         :run-uuid run-uuid :kind run-kind
                         :model-id model-id}} error)
     (bump! user-id)))
  nil)

;; ── Answers ────────────────────────────────────────────────────────────────
;;
;; An answer starts in one of two ways. When its request fits the model's
;; budget, the run is created at once and a worker streams it. When it does
;; not, the chat enters PREPARATION: a worker compacts until the request fits,
;; and only then creates the run (plans/assistant-chat-forks.md §4.5.4). The
;; order is forced by `idx_assistant_runs_one_active`: a compaction of the chat
;; itself is a run, so the answer's run cannot exist while it is active.
;;
;; A preparing chat has no run row, so `!preparing` stands for one. It refuses
;; a second send, `active-run*` reports it as compacting, and Stop cancels it.

(defonce ^:private !preparing
  ;; chat id → {:cancelled? bool}
  (atom {}))

(defn- preparing? [chat-id]
  (contains? @!preparing chat-id))

(defn- claim-preparation!
  "Mark `chat-id` as preparing. Post: true when this call claimed it."
  [chat-id]
  (let [[before _] (swap-vals! !preparing
                     #(if (contains? % chat-id) % (assoc % chat-id {:cancelled? false})))]
    (not (contains? before chat-id))))

(defn- release-preparation!
  "End `chat-id`'s preparation. Idempotent."
  [user-id chat-id]
  (let [[before _] (swap-vals! !preparing dissoc chat-id)]
    (when (contains? before chat-id)
      (bump! user-id))))

(defn- cancel-preparation!
  "Ask the preparation of the owned chat `chat-id` to stop.
   Post: true when a preparation was running."
  [user-id chat-id]
  (boolean
    (when (and (preparing? chat-id) (db/get-assistant-chat user-id chat-id))
      (swap! !preparing #(cond-> % (contains? % chat-id)
                           (assoc-in [chat-id :cancelled?] true)))
      true)))

(defn- preparation-cancelled? [chat-id]
  (boolean (:cancelled? (get @!preparing chat-id))))

(def ^:private compaction-overhead-tokens
  "Tokens a compaction request keeps for its instructions and the previous
   checkpoint's summary, outside the rows it summarizes."
  4000)

(def ^:private summarize-instructions
  (str "Summarize this conversation for future model turns. "
    "Preserve decisions, definitions, unresolved questions, "
    "and source references. Return only the summary."))

(defn- compaction-request
  "The provider request that summarizes `turns` into a checkpoint of `chat`,
   building on `prior`, the checkpoint it replaces, when there is one."
  [model-fields chat prior turns]
  (assoc model-fields
    :stream true
    :session_id (:assistant_chats/provider_session_id chat)
    :messages (-> [{:role "system" :content summarize-instructions}]
                (cond-> prior
                  (conj {:role "system"
                         :content (str "Existing conversation checkpoint:\n\n"
                                    (:summary prior))}))
                (into turns))))

(defn- run-model
  "The model entry and request fields for one run of `user-id`. An exact
   model id from a replayed request wins over the current setting."
  [user-id exact-model-id]
  (let [entry (or (some-> exact-model-id card-models/resolve-model)
                (model-entry user-id nil))]
    {:entry entry
     :model-id (or exact-model-id (:id entry))
     :model-fields (openrouter/chat-completion-fields entry
                     {:reasoning_effort (settings/get-assistant-reasoning user-id)})}))

(defn- render-answer
  "The request for `answer` in its chat as the chat is now, and the budget it
   must fit.
   Post: {:chat :entry :model-id :rendered :budget}, or nil when the chat is
         gone. :budget is nil when the model's context length is unknown."
  [{:keys [user-id chat-id page-topic-id user-text ref-topic-ids turn]}]
  (when-let [chat (db/get-assistant-chat user-id chat-id)]
    (let [{:keys [entry model-id model-fields]} (run-model user-id nil)]
      {:chat chat :entry entry :model-id model-id
       :rendered (answer-request user-id chat page-topic-id user-text ref-topic-ids
                   model-fields (card-models/web-search-supported? model-id)
                   (card-models/cache-breakpoint? model-id) turn)
       :budget (budget/budget-tokens
                 (model-context/context-length (:openrouter-model entry)))})))

(defn- request-fits? [{:keys [rendered budget]}]
  (budget/fits? (get-in rendered [:provider-request :messages]) budget))

(defn- create-answer-run!
  "Create the durable run of `answer` from `rendered`.
   Post: {:ok true :run-uuid} with :assistant-mutations bumped, or {:error}."
  [{:keys [user-id chat-id user-text client-id retry-of-run-uuid]} chat model-id rendered]
  (let [run-uuid (str (random-uuid))
        created (db/create-assistant-answer-run!
                  chat-id user-text client-id
                  {:run-uuid run-uuid
                   :retry-of-run-uuid retry-of-run-uuid
                   :provider-session-id (:assistant_chats/provider_session_id chat)
                   :requested-model model-id
                   :request-snapshot (:provider-request rendered)
                   :source-snapshot (:source-snapshot rendered)
                   :evidence (:evidence rendered)})]
    (cond
      (:chat-missing? created)
      {:error "Chat not found."}

      (not (:created created))
      {:error "This chat already has an active operation."
       :active-run-uuid (:active-run-uuid created)}

      :else
      (do (bump! user-id)
          {:ok true :run-uuid run-uuid}))))

(defn- answer-parameters
  "The parts of `answer` a rebuilt request needs, carried by its run worker."
  [answer]
  (select-keys answer [:user-id :chat-id :page-topic-id :user-text :ref-topic-ids
                       :turn :overflow-retried?]))

(defn- sendable-sizes
  "{:id :chars} for each of `rows` a request carries after `after-id`."
  [rows after-id]
  (mapv (fn [row] {:id (:assistant_messages/id row)
                   :chars (count (:assistant_messages/content row))})
    (sendable-rows rows after-id)))

(defn- compaction-targets
  "The plan for an answer in `chat` that does not fit `budget-tokens`
   (`budget/compaction-plan`). An unknown budget plans with
   `budget/fallback-budget-tokens`."
  [user-id chat budget-tokens]
  (let [chat-id (:assistant_chats/id chat)
        path (inherited-path user-id chat)
        descendants (map chat-node (db/get-assistant-chat-descendants user-id chat-id))
        activity (into {} (map (juxt :id :activity-ms)) descendants)
        own-checkpoint (db/latest-assistant-checkpoint user-id chat-id "own")
        inherited-checkpoint (when (seq path)
                               (db/latest-assistant-checkpoint user-id chat-id "inherited"))
        budget-tokens (or budget-tokens budget/fallback-budget-tokens)]
    (budget/compaction-plan
      {:branches (mapv (fn [{branch-id :chat-id}]
                         (let [checkpoint (db/latest-assistant-checkpoint user-id branch-id "own")]
                           {:chat-id branch-id
                            :activity-ms (activity branch-id)
                            :rows (sendable-sizes (db/get-assistant-messages branch-id)
                                    (:covered_through_message_id checkpoint))}))
                   (tree/included-branches descendants chat-id))
       :inherited (when (seq path)
                    {:chat-id chat-id
                     :rows (sendable-sizes (inherited-rows path)
                             (:covered_through_message_id inherited-checkpoint))})
       :own {:chat-id chat-id
             :rows (sendable-sizes (db/get-assistant-messages chat-id)
                     (:covered_through_message_id own-checkpoint))}
       :slice-budget (max 1000 (- budget-tokens compaction-overhead-tokens))})))

(defn- compact-slice!
  "Summarize `turns` into a new checkpoint of `target-chat`, in this thread.
   Pre:  `covered` is {:from :through :scope :for-chat-id}. `prior` is the
         target's newest checkpoint of that scope, or nil.
   Post: true when the checkpoint landed. False when the chat had an active
         run, or the compaction failed or was stopped."
  [user-id api-key {:keys [entry model-fields]} target-chat covered prior turns]
  (let [chat-id (:assistant_chats/id target-chat)
        run-uuid (str (random-uuid))
        request (compaction-request model-fields target-chat prior turns)
        created (db/create-assistant-compact-run!
                  chat-id {:run-uuid run-uuid
                           :provider-session-id (:assistant_chats/provider_session_id target-chat)
                           :requested-model (:id entry)
                           :request-snapshot request
                           :source-snapshot []})]
    (boolean
      (when (:created created)
        (bump! user-id)
        (execute-provider-run! user-id chat-id run-uuid "compact" api-key (:id entry)
          request covered nil nil)
        (= "completed" (:status (db/get-assistant-run user-id run-uuid)))))))

(defn- compact-target!
  "Compact one plan target, slice by slice, oldest first, for an answer in
   `for-chat-id`. Each slice builds on the checkpoint the one before it made.
   Post: true when every slice landed."
  [user-id api-key model for-chat-id {:keys [target chat-id slices]}]
  (let [target-chat (db/get-assistant-chat user-id chat-id)
        scope (if (= :inherited target) "inherited" "own")
        rows (when target-chat
               (if (= :inherited target)
                 (inherited-rows (inherited-path user-id target-chat))
                 (db/get-assistant-messages chat-id)))]
    (boolean
      (and target-chat
        (every? (fn [[_ through]]
                  (let [prior (db/latest-assistant-checkpoint user-id chat-id scope)
                        turns (provider-turns
                                (filter #(<= (:assistant_messages/id %) through) rows)
                                (:covered_through_message_id prior))]
                    (or (empty? turns)
                      (compact-slice! user-id api-key model target-chat
                        {:from (or (:covered_from_message_id prior)
                                 (:assistant_messages/id (first rows)))
                         :through through :scope scope :for-chat-id for-chat-id}
                        prior turns))))
          slices)))))

(defn- settle-prepared-answer!
  "Create the run of a prepared `answer` from `prepared` and end the
   preparation. `outcome` is :run (stream it in this thread), :stop (stop it
   at once, for a cancelled preparation) or :oversized (fail it with
   `oversized-answer-error`). Creating the run in every case is what puts the
   learner's turn in the transcript, so the optimistic echo retires."
  [{:keys [user-id chat-id] :as answer} api-key {:keys [chat model-id rendered]} outcome]
  (let [created (create-answer-run! answer chat model-id rendered)]
    (release-preparation! user-id chat-id)
    (when-let [run-uuid (:run-uuid created)]
      (case outcome
        :run (execute-provider-run! user-id chat-id run-uuid "answer" api-key model-id
               (:provider-request rendered) nil (:reply-blocks rendered)
               (answer-parameters answer))
        :stop (db/stop-assistant-run! user-id chat-id)
        :oversized (db/fail-assistant-run! run-uuid oversized-answer-error))
      (bump! user-id))
    created))

(defn- prepare-answer!
  "Compact until `answer`'s request fits, then create and stream its run, in
   this thread (§4.5.3, §4.5.4).

   Pre:  this call owns the chat's preparation (`claim-preparation!`).
   Post: the preparation is released. The answer's run exists and is
         completed, failed or stopped, unless the chat is gone. Each target is
         tried once. With :force-compaction?, one target is compacted even when
         the estimate says the request fits, because the provider said it did
         not."
  [{:keys [user-id chat-id force-compaction?] :as answer} api-key]
  (try
    (loop [tried #{} force? (if force-compaction? true false)]
      (when-let [prepared (render-answer answer)]
        (cond
          (preparation-cancelled? chat-id)
          (settle-prepared-answer! answer api-key prepared :stop)

          (and (not force?) (request-fits? prepared))
          (settle-prepared-answer! answer api-key prepared :run)

          :else
          (let [model (run-model user-id nil)
                target (->> (compaction-targets user-id (:chat prepared) (:budget prepared))
                         (remove #(tried [(:target %) (:chat-id %)]))
                         first)]
            (cond
              target
              (do (compact-target! user-id api-key model chat-id target)
                  (recur (conj tried [(:target target) (:chat-id target)]) false))

              force? (recur tried false)

              :else
              (settle-prepared-answer! answer api-key prepared :oversized))))))
    (finally
      (release-preparation! user-id chat-id))))

(defn- start-prepared-answer!
  "Start `answer`'s preparation on the run executor.
   Post: {:ok true :preparing? true}, or {:error} when the chat already has
         an active operation. Returns before the provider is called."
  [{:keys [user-id chat-id] :as answer} api-key]
  (if-not (claim-preparation! chat-id)
    {:error "This chat already has an active operation."}
    (do
      (bump! user-id)
      ((m/via assistant-run-executor (prepare-answer! answer api-key))
       (fn [_] nil)
       (fn [error]
         (release-preparation! user-id chat-id)
         (tel/error! {:id ::assistant-preparation
                      :data {:user-id user-id :chat-id chat-id}} error)))
      {:ok true :preparing? true})))

(defn- retry-after-overflow!
  "Answer `answer` again after the provider refused its request as too long
   (§4.5.6): compact at least once, rebuild the request, and send it as a
   retry of `failed-run-uuid`, in this thread. Never a second time for the
   same turn.
   Pre: the caller claimed the chat's preparation before it failed the run."
  [{:keys [user-id] :as answer} api-key failed-run-uuid]
  (bump! user-id)
  (prepare-answer! (assoc answer
                     :force-compaction? true
                     :overflow-retried? true
                     :retry-of-run-uuid failed-run-uuid
                     :client-id (str (random-uuid)))
    api-key))

(defn- start-answer!
  "Create and start one streamed answer in an owned chat.

   Pre:  `answer` is {:user-id :chat-id :page-topic-id :user-text
         :ref-topic-ids :client-id :turn :retry-of-run-uuid}, plus
         :exact-request, :exact-evidence and :exact-model-id for a replay.
         `turn` is one of `turn-kinds`.
   Post: {:ok true :run-uuid} after durable creation and worker submission,
         {:ok true :preparing? true} when the request must be compacted first,
         or {:error}. The call does not wait for the provider.
   Invariant: the database rejects a second active operation in the chat, and
   a preparing chat refuses one before it reaches the database."
  [{:keys [user-id chat-id user-text exact-request exact-evidence exact-model-id]
    :as answer}]
  (let [chat (db/get-assistant-chat user-id chat-id)
        api-key (settings/get-openrouter-api-key user-id)]
    (cond
      (nil? chat) {:error "Chat not found."}
      (str/blank? user-text) {:error "Message is empty."}
      (str/blank? api-key) {:error "OpenRouter API key not configured."}
      (preparing? chat-id) {:error "This chat already has an active operation."}
      :else
      (let [gate (credits/check-cost-billed-balance! user-id)]
        (cond
          (not (:ok gate))
          {:error (:error gate)}

          exact-request
          (let [{:keys [model-id]} (run-model user-id exact-model-id)
                rendered {:provider-request exact-request
                          :source-snapshot exact-evidence
                          :evidence exact-evidence}
                created (create-answer-run! answer chat model-id rendered)]
            (when (:ok created)
              (start-provider-run! user-id chat-id (:run-uuid created) "answer" api-key
                model-id exact-request nil nil (answer-parameters answer)))
            created)

          :else
          (let [prepared (render-answer answer)]
            (if (request-fits? prepared)
              (let [{:keys [model-id rendered]} prepared
                    created (create-answer-run! answer chat model-id rendered)]
                (when (:ok created)
                  (start-provider-run! user-id chat-id (:run-uuid created) "answer"
                    api-key model-id (:provider-request rendered) nil
                    (:reply-blocks rendered) (answer-parameters answer)))
                created)
              (start-prepared-answer! answer api-key))))))))

(defn start-answer-run!
  "Create and start one streamed answer in an owned chat (`start-answer!`).

   Pre: user-text is non-blank. `turn` is one of `turn-kinds`: `:annotated`
   exactly when user-text is an `annotations/user-message`, `:question-pins`
   exactly when it is `question-map/open-pins-message` text."
  ([user-id chat-id page-topic-id user-text ref-topic-ids client-id]
   (start-answer-run! user-id chat-id page-topic-id user-text ref-topic-ids
     client-id nil))
  ([user-id chat-id page-topic-id user-text ref-topic-ids client-id turn]
   (start-answer! {:user-id user-id :chat-id chat-id :page-topic-id page-topic-id
                   :user-text user-text :ref-topic-ids ref-topic-ids
                   :client-id client-id :turn turn}))
  ([user-id chat-id page-topic-id user-text ref-topic-ids client-id
    retry-of-run-uuid exact-request exact-evidence exact-model-id]
   (start-answer! {:user-id user-id :chat-id chat-id :page-topic-id page-topic-id
                   :user-text user-text :ref-topic-ids ref-topic-ids
                   :client-id client-id :retry-of-run-uuid retry-of-run-uuid
                   :exact-request exact-request :exact-evidence exact-evidence
                   :exact-model-id exact-model-id})))

(defn- start-answer-run-in-new-chat!
  "Create a chat titled from `user-text` and start its first answer.

   Pre: no chat is selected. Post: on success the chat holds the first turn, is
   marked opened, and its id is :chat-id. On an error result or a throw, the
   chat this call created is deleted again and :chat-id is nil, so a failed
   first send leaves no empty chat behind.
   Owner: this function created the row, so it removes it. Callers do not."
  [user-id root-topic-id page-topic-id mode-id web-search? user-text ref-topic-ids
   client-id turn]
  (if (str/blank? user-text)
    {:error "Message is empty." :chat-id nil}
    (let [chat-id (start-chat! user-id root-topic-id mode-id web-search?
                    (if (= :question-pins turn) "Pinned questions" (title-from user-text)))
          result (try
                   (start-answer-run! user-id chat-id page-topic-id user-text
                     ref-topic-ids client-id turn)
                   (catch Throwable t
                     (db/delete-empty-assistant-chat! user-id chat-id)
                     (throw t)))]
      (if (:error result)
        (do (db/delete-empty-assistant-chat! user-id chat-id)
            (assoc result :chat-id nil))
        (do (db/mark-assistant-chat-opened! user-id chat-id)
            (assoc result :chat-id chat-id))))))

(def ^:private wait-for-answer-error "Wait for the answer to finish.")

(defn- in-transcript?
  "True when `message`, a row of `get-assistant-message`, is in `chat`'s
   transcript: one of its own rows, or one it inherits (D1b)."
  [user-id chat message]
  (or (= (:assistant_chats/id chat) (:chat_id message))
    (boolean (some (fn [[chat-id through]]
                     (and (= chat-id (:chat_id message)) (<= (:id message) through)))
               (inherited-path user-id chat)))))

(defn fork-and-start-answer!
  "Fork the owned chat `parent-chat-id` at `message-id`, or at its latest
   message when `message-id` is nil, and start the child's first answer
   (plans/assistant-chat-forks.md §4.3.2, §4.3.3).

   Pre:  `message-id` is a message the learner saw in the parent's
         transcript, which may be one the parent inherited (D1b).
   Post: on success, :chat-id is the child. It holds the first turn, is
         marked opened, and has the parent's mode and web-search preference
         (§4.3.5). On an error, :chat-id is the parent, so the parent stays
         open, and no child row remains. A message whose run is still active
         cannot be forked at (§4.3.4).
   Owner: this function created the child, so it removes it on failure."
  [user-id parent-chat-id message-id page-topic-id user-text ref-topic-ids client-id turn]
  (let [parent (db/get-assistant-chat user-id parent-chat-id)
        fork-id (or message-id
                  (when parent
                    (:assistant_messages/id (last (db/get-assistant-messages parent-chat-id)))))
        message (when fork-id (db/get-assistant-message user-id fork-id))
        result
        (cond
          (nil? parent) {:error "Chat not found."}
          (str/blank? user-text) {:error slash/fork-usage-error}
          (or (nil? message) (not (in-transcript? user-id parent message)))
          {:error "That message is not in this chat."}
          (db/assistant-message-run-active? fork-id) {:error wait-for-answer-error}
          :else
          (if-let [child-id (db/create-assistant-fork! user-id parent-chat-id fork-id
                              (title-from user-text))]
            (let [started (try
                            (start-answer! {:user-id user-id :chat-id child-id
                                            :page-topic-id page-topic-id
                                            :user-text user-text
                                            :ref-topic-ids ref-topic-ids
                                            :client-id client-id :turn turn})
                            (catch Throwable t
                              (db/delete-empty-assistant-chat! user-id child-id)
                              (throw t)))]
              (if (:error started)
                (do (db/delete-empty-assistant-chat! user-id child-id)
                    started)
                (do (db/mark-assistant-chat-opened! user-id child-id)
                    (assoc started :chat-id child-id))))
            {:error "Chat not found."}))]
    (if (:error result)
      (assoc result :chat-id (when parent parent-chat-id))
      result)))

(defn ensure-and-start-answer-run!
  "Start an answer in `chat-id`, or in a new chat when `chat-id` is nil.
   See start-answer-run-in-new-chat! for the new-chat contract. `turn` is one of
   `turn-kinds`. `:annotated` comes with a chat only: annotations point at its
   answers, so `submit-chat-input!` never sends them without one. An Ask over
   pins can start a chat."
  ([user-id root-topic-id page-topic-id chat-id mode-id web-search? user-text
    ref-topic-ids client-id]
   (ensure-and-start-answer-run! user-id root-topic-id page-topic-id chat-id mode-id
     web-search? user-text ref-topic-ids client-id nil))
  ([user-id root-topic-id page-topic-id chat-id mode-id web-search? user-text
    ref-topic-ids client-id turn]
   (if chat-id
     (assoc (start-answer-run! user-id chat-id page-topic-id user-text
              ref-topic-ids client-id turn)
       :chat-id chat-id)
     (start-answer-run-in-new-chat! user-id root-topic-id page-topic-id mode-id
       web-search? user-text ref-topic-ids client-id turn))))

(defn submit-question-pins!
  "Ask the tutor about every open pin of the file `page-topic-id`
   (plans/anchored-questions.md §6.2.5). The pane button, the composer button
   and the palette all reach this one function, and it builds the pin set, so
   they send the same pins (§6.2.5.1).
   Post: as `ensure-and-start-answer-run!`, plus :command :answer. With no open
         pins, {:error} and no run."
  [user-id root-topic-id page-topic-id chat-id mode-id web-search? client-id]
  (let [{:keys [text error]} (question-map/open-pins-message user-id
                                (document-ref-for user-id page-topic-id))]
    (if error
      {:error error :chat-id chat-id :command :answer}
      (assoc (ensure-and-start-answer-run! user-id root-topic-id page-topic-id
               chat-id mode-id web-search? text [] client-id :question-pins)
        :command :answer))))

(defn submit-chat-input!
  "Dispatch composer input at the assistant boundary. A slash command
   (`freememo.assistant-slash`) is an application command and never becomes a
   user message as typed: `/compact` compacts the open chat, and
   `/fork <question>` forks it at its latest message with that question.

   `annotation-list` is the composer's annotations as `annotations/wire-annotations`
   gives them, or empty. With annotations, the stored and sent user turn is
   `annotations/user-message` over them and `draft`, and `draft` may be blank.
   `fork` is the composer's pending fork, {:message-id id-or-nil}, or nil. It
   makes the send the first turn of a new fork of `chat-id` (§4.3.3).
   Pre:  an annotation needs an answer to point at, so a non-empty list comes
         with a `chat-id`. So does a pending fork.
   Post: {:error} without a run when the list fails `annotations/problem`.
         :command is :compact, :fork or :answer."
  ([user-id root-topic-id page-topic-id chat-id mode-id web-search? draft ref-topic-ids
    client-id]
   (submit-chat-input! user-id root-topic-id page-topic-id chat-id mode-id web-search?
     draft ref-topic-ids client-id [] nil))
  ([user-id root-topic-id page-topic-id chat-id mode-id web-search? draft ref-topic-ids
    client-id annotation-list]
   (submit-chat-input! user-id root-topic-id page-topic-id chat-id mode-id web-search?
     draft ref-topic-ids client-id annotation-list nil))
  ([user-id root-topic-id page-topic-id chat-id mode-id web-search? draft ref-topic-ids
    client-id annotation-list fork]
   (let [annotation-list (annotations/wire-annotations annotation-list)
         annotated? (boolean (seq annotation-list))
         command (when-not annotated? (slash/parse draft))
         text (if annotated? (annotations/user-message annotation-list draft) draft)
         turn (when annotated? :annotated)]
     (cond
       (= :compact (:command command))
       (if chat-id
         (assoc (start-compact-run! user-id chat-id) :chat-id chat-id :command :compact)
         {:error "Start a chat before compacting context." :chat-id nil :command :compact})

       (= :fork (:command command))
       (if chat-id
         (assoc (fork-and-start-answer! user-id chat-id nil page-topic-id
                  (:question command) ref-topic-ids client-id nil)
           :command :fork)
         {:error "Start a chat before forking it." :chat-id nil :command :fork})

       (and annotated? (nil? chat-id))
       {:error "Annotations need an open chat." :chat-id nil :command :answer}

       (and annotated? (annotations/problem annotation-list))
       {:error (annotations/problem annotation-list) :chat-id chat-id :command :answer}

       (and fork chat-id)
       (assoc (fork-and-start-answer! user-id chat-id (:message-id fork) page-topic-id
                text ref-topic-ids client-id turn)
         :command :fork)

       :else
       (assoc (ensure-and-start-answer-run! user-id root-topic-id page-topic-id
                chat-id mode-id web-search? text ref-topic-ids client-id turn)
         :command :answer)))))

(defn start-compact-run!
  "Start `/compact` as a separate paid run without adding a user message.
   It covers the chat's own rows only, so in a fork the inherited rows stay as
   they are (plans/assistant-chat-forks.md §4.5.8).
   Post: a completed worker creates one `own` checkpoint event. Original
   messages stay unchanged. Empty transcripts and concurrent operations return
   an error."
  [user-id chat-id]
  (let [chat (db/get-assistant-chat user-id chat-id)
        api-key (settings/get-openrouter-api-key user-id)]
    (cond
      (nil? chat) {:error "Chat not found."}
      (str/blank? api-key) {:error "OpenRouter API key not configured."}
      (preparing? chat-id) {:error "This chat already has an active operation."}
      :else
      (let [gate (credits/check-cost-billed-balance! user-id)
            messages (db/get-assistant-messages chat-id)]
        (cond
          (not (:ok gate)) {:error (:error gate)}
          (empty? messages) {:error "There is no chat context to compact."}
          :else
          (let [{:keys [entry model-fields]} (run-model user-id nil)
                checkpoint (db/latest-assistant-checkpoint user-id chat-id "own")
                provider-request (compaction-request model-fields chat checkpoint
                                   (provider-turns messages
                                     (:covered_through_message_id checkpoint)))
                run-uuid (str (random-uuid))
                covered {:from (:assistant_messages/id (first messages))
                         :through (:assistant_messages/id (last messages))
                         :scope "own"}
                created (db/create-assistant-compact-run!
                          chat-id {:run-uuid run-uuid
                                   :provider-session-id
                                   (:assistant_chats/provider_session_id chat)
                                   :requested-model (:id entry)
                                   :request-snapshot provider-request
                                   :source-snapshot []})]
            (cond
              (:chat-missing? created)
              {:error "Chat not found."}

              (not (:created created))
              {:error "This chat already has an active operation."
               :active-run-uuid (:active-run-uuid created)}

              :else
              (do
                (bump! user-id)
                (start-provider-run! user-id chat-id run-uuid "compact" api-key
                  (:id entry) provider-request covered nil nil)
                {:ok true :run-uuid run-uuid}))))))))

(defn stop-active-run!
  "Stop local delivery for the active run in an owned chat, and cancel the
   chat's preparation if one is compacting before its answer.
   The provider reader keeps draining for usage and cost metadata. A cancelled
   preparation still records the learner's turn, as a stopped answer."
  [user-id chat-id]
  (let [cancelled? (cancel-preparation! user-id chat-id)
        result (db/stop-assistant-run! user-id chat-id)]
    (when (or cancelled? (:stopped result)) (bump! user-id))
    (cond-> result cancelled? (assoc :stopped true))))

(defn- imported-evidence->creation-evidence
  [source]
  {:citation-key (:citation_key source)
   :document-topic-id (:document_topic_id source)
   :page-topic-id (:page_topic_id source)
   :page-number (:page_number source)
   :anchor (:anchor source)
   :quoted-text (:quoted_text source)
   :document-title (:document_title source)
   :display-label (:display_label source)
   :source-revision (:source_revision source)
   :walkthrough-id (:walkthrough_id source)})

(defn- rebuild-on-retry?
  "True when a failed run's request must be built again rather than replayed:
   it was refused for its size, and replaying it would be refused again. The
   learner may have excluded a branch since (§4.5.7)."
  [failed-run]
  (let [error-text (:error_text failed-run)]
    (or (= oversized-answer-error error-text)
      (context-overflow-error? (ex-info (or error-text "") {})))))

(defn retry-answer-run!
  "Retry one failed answer with its exact provider request and evidence
   snapshot. An answer refused for its size is built again instead, from the
   chat as it is now.
   Post: creates a new user turn and run. The failed run stays unchanged."
  [user-id run-uuid client-id]
  (if-let [failed-run (db/get-assistant-run user-id run-uuid)]
    (cond
      (not= "answer" (:kind failed-run)) {:error "Only answers can be retried."}
      (not= "failed" (:status failed-run)) {:error "Only failed answers can be retried."}
      (rebuild-on-retry? failed-run)
      (start-answer! {:user-id user-id :chat-id (:chat_id failed-run)
                      :user-text (:content (db/get-assistant-message
                                             user-id (:user_message_id failed-run)))
                      :ref-topic-ids [] :client-id client-id
                      :retry-of-run-uuid run-uuid})
      :else
      (let [user-message (db/get-assistant-message user-id (:user_message_id failed-run))
            snapshots (into {} (map (juxt :citation-key identity))
                        (:source_snapshot failed-run))
            evidence (mapv (fn [source]
                             (let [creation (imported-evidence->creation-evidence source)]
                               (merge (get snapshots (:citation-key creation)) creation)))
                       (db/get-assistant-run-evidence user-id run-uuid))]
        (start-answer-run! user-id (:chat_id failed-run) nil
          (:content user-message) [] client-id run-uuid
          (:request_snapshot failed-run) evidence (:requested_model failed-run))))
    {:error "Answer run not found."}))

(def ^:private feedback-anchor
  "Verbatim heading the Socratic persona emits to open the feedback section. The
   parser keys on it so card capture uses only the feedback, never the Socratic
   question that follows under the question anchor."
  "**Where you are**")

(def ^:private question-anchor
  "Verbatim heading that ends the feedback section (start of the question)."
  "**Consider next**")

(defn- cardable-text
  "The part of an assistant reply a card may be generated from.

   Keyed on the reply's own text, NOT on the chat's mode. Two reasons: a reply
   is captured long after it was produced and a chat's mode can be switched in
   between, so the mode at ✦-press time is not necessarily the mode that wrote
   the reply; and the Socratic persona's two headings are a MUST the model can
   still ignore, which used to make capture fail outright.

   Pre:  `reply` is a non-blank assistant reply.
   Post: the trimmed text between the two anchors when the feedback anchor is
   present (end-of-string when the question anchor is not) — so a Socratic
   question is never carded; otherwise the whole trimmed reply, which is what
   Tutor and General produce. May be blank (an anchored reply whose feedback
   section is empty); the caller rejects blank."
  [reply]
  (if-let [start (some-> reply (str/index-of feedback-anchor))]
    (let [after (subs reply (+ start (count feedback-anchor)))
          end (str/index-of after question-anchor)]
      (str/trim (if end (subs after 0 end) after)))
    (str/trim (or reply ""))))

(defn reply->cards!
  "Generate basic flashcards from an assistant reply and save them under the
   reading page (`page-topic-id`). `cardable-text` decides what part of the reply
   is used: a Socratic reply contributes its `**Where you are**` body only (never
   the question), a Tutor or General reply contributes the whole thing. Bypasses
   the KG fact-routing detour that the toolbar Generate applies, reporting the
   outcome via a toast.
   Pre: page-topic-id / root-topic-id are the learner's current reading topic;
   reply-text is an assistant reply in any persona. Post: on success (count :ids)
   basic cards exist under page-topic-id and :card-mutations is bumped; when the
   captured text is blank (a Socratic reply that is only a question) nothing is
   saved and a toast says so; on failure nothing is saved. Returns
   {:success true :ids v :count n} | {:success false :error s}."
  [user-id page-topic-id root-topic-id reply-text]
  (let [capture (cardable-text reply-text)
        result (if (str/blank? capture)
                 {:success false
                  :error "No feedback to capture yet — the reply is only a question."}
                 (let [gen (cards/generate-basic-cards
                             {:content capture
                              :card-count (settings/get-card-count user-id)
                              :model (settings/get-model user-id)
                              :user-id user-id
                              :topic-id page-topic-id})]
                   (if-not (:success gen)
                     {:success false :error (:error gen)}
                     (let [saved (cards/save-cards user-id page-topic-id root-topic-id "basic" (:cards gen))]
                       (if (:success saved)
                         (do (commands/bump! user-id :generate)
                           {:success true :ids (:ids saved) :count (count (:ids saved))})
                         {:success false :error (:error saved)})))))]
    (if (:success result)
      (let [n (:count result)]
        (toasts/push! user-id {:level :success
                               :message (str "Added " n " card" (when (not= 1 n) "s")
                                          " from this reply.")}))
      (toasts/push! user-id {:level :error
                             :message (or (:error result) "Card capture failed.")}))
    result))

;; ── Cards from assistant text ──────────────────────────────────────────────

(defn- answer-context-text
  "An answer's text as card-generation context: its evidence tokens removed,
   because they name sources the card model never sees."
  [answer]
  (str/trim (str/replace (or answer "") evidence-token-pattern "")))

(defn- escape-html [s]
  (str/escape (or s "") {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"}))

(defn- enqueue-assistant-cards!
  "Queue one card job for text taken from the assistant, through the toolbar's
   Generate queue and with the toolbar's settings: card type, count, use-context
   and KG fact routing all behave as they do for a toolbar selection.

   Pre:  `page-topic-id` is the topic the panel is open on. `content-html` is
         the subject. `context-text` is what a toolbar selection's surrounding
         page would be.
   Post: {:ok true} after the job is queued, or {:error}. The cards land on
         `page-topic-id`, with the root the toolbar would use, and a toast
         reports the count. Progress and Cancel are the topic's Activity row."
  [user-id page-topic-id content-html context-text]
  (let [page (db/get-topic-for-user user-id page-topic-id)
        root-id (or (db/get-root-topic-id page-topic-id) page-topic-id)
        root-kind (:topics/kind (if (= root-id page-topic-id) page (db/get-topic root-id)))
        content (cleaner/clean-html-fragment content-html)]
    (cond
      (nil? page) {:error "Topic not found."}
      (str/blank? (text/strip-html (or content ""))) {:error "There is no text to make cards from."}
      :else
      (do (card-gen/enqueue-card-gen!
            {:id (str (random-uuid))
             :content content
             :context (when (settings/get-context-enabled user-id) context-text)
             :card-type (settings/get-card-type user-id)
             :card-count (settings/get-card-count user-id)
             :user-id user-id
             :enc-key nil
             :topic-id page-topic-id
             :root-topic-id (or (topic-kinds/pdf-root-id (:topics/kind page) page-topic-id
                                  root-kind root-id)
                              root-id)
             :pre-prompt nil
             :pre-prompt-mode nil
             :success-toast? true})
        {:ok true}))))

(defn selection->cards!
  "Queue cards from text selected in one settled answer. The whole answer is the
   context, as a toolbar selection's page is.
   Pre:  `message-id` is an assistant message in a chat `user-id` owns.
   Post: as `enqueue-assistant-cards!`."
  [user-id page-topic-id message-id selection-html]
  (if-let [answer (db/get-assistant-message user-id message-id)]
    (enqueue-assistant-cards! user-id page-topic-id selection-html
      (answer-context-text (:content answer)))
    {:error "Answer not found."}))

;; ── Side questions ─────────────────────────────────────────────────────────
;;
;; A side question is a thread about one passage of an answer. It lives in one
;; atom that its floating panel's server frame owns, and nowhere else: it writes
;; no assistant_runs or assistant_messages row and bumps no assistant channel.
;; A run row would meet the chat's one-active-run index, turn on the main
;; panel's Stop, and every per-user bump would re-read the main transcript.
;; plans/assistant-answer-selection-actions.md §5.3.

(defonce ^:private side-run-executor
  ;; Its own pool, so open side questions cannot hold the threads main answers
  ;; stream on.
  (Executors/newFixedThreadPool 4))

(defn new-side-thread
  "The empty state of a side question about `passage`, a quote from the answer
   `message-id`."
  [passage message-id]
  {:passage passage :message-id message-id :closed? false :turns []})

(defn close-side-thread!
  "Mark the thread closed. A run in flight drains for its cost, then writes
   nothing more. Post: nil."
  [!thread]
  (swap! !thread assoc :closed? true)
  nil)

(defn- side-turn-streaming? [thread]
  (= "streaming" (:status (peek (:turns thread)))))

(defn- update-side-answer!
  "Apply `f` to the thread's newest turn, the answer `answer-id`, unless the
   thread is closed or that turn is no longer the newest."
  [!thread answer-id f]
  (swap! !thread
    (fn [thread]
      (let [last-index (dec (count (:turns thread)))]
        (if (and (not (:closed? thread))
              (= answer-id (get-in thread [:turns last-index :id])))
          (update-in thread [:turns last-index] f)
          thread)))))

(defn- creation->evidence-row
  "Evidence as `request-head` gives it, in the column shape `transcript-message-view`
   and the evidence dialog read."
  [source]
  {:citation_key (:citation-key source)
   :document_topic_id (:document-topic-id source)
   :page_topic_id (:page-topic-id source)
   :page_number (:page-number source)
   :anchor (:anchor source)
   :quoted_text (:quoted-text source)
   :quoted_html (:quoted-html source)
   :document_title (:document-title source)
   :display_label (:display-label source)
   :walkthrough_id (:walkthrough-id source)})

(defn- execute-side-run!
  "Drain one side-question stream into the thread.
   Post: the answer turn is completed or failed, unless the thread closed first.
   The run is charged once under :assistant.side either way."
  [!thread user-id answer-id api-key model-id provider-request]
  (let [text* (volatile! "")
        metadata* (volatile! {})
        last-publish-ns* (volatile! 0)
        charge! (fn [metadata]
                  (charge-run! user-id :assistant.side model-id metadata))
        publish! (fn [force?]
                   (let [now (System/nanoTime)]
                     (when (or force? (> (- now (long @last-publish-ns*)) 100000000))
                       (vreset! last-publish-ns* now)
                       (let [text @text*]
                         (update-side-answer! !thread answer-id #(assoc % :content text))))))]
    (try
      (let [result (openrouter/chat-completion-stream!
                     api-key provider-request {:feature :assistant-side :user-id user-id}
                     (fn [delta]
                       (vswap! text* str delta)
                       (when-not (:closed? @!thread) (publish! false)))
                     (fn [metadata] (vswap! metadata* merge metadata)))
            reported @metadata*
            metadata (merge reported
                       (when (and (:generation-id reported) (nil? (:cost-usd reported)))
                         (poll-generation-metadata api-key (:generation-id reported) user-id)))
            response (str/trim (or (:text result) @text*))]
        (charge! metadata)
        (update-side-answer! !thread answer-id
          #(if (str/blank? response)
             (assoc % :status "failed" :error "The model returned an empty response.")
             (assoc % :status "completed" :content response)))
        (tel/log! {:level :info :id ::side-run-terminal
                   :data {:user-id user-id :model-id model-id
                          :closed? (:closed? @!thread)
                          :cached-tokens (:cached-tokens metadata)
                          :cost-usd (:cost-usd metadata)}}
          "Assistant side question reached a terminal state"))
      (catch Throwable error
        (let [known @metadata*]
          (charge! (merge known
                     (when (:generation-id known)
                       (poll-generation-metadata api-key (:generation-id known) user-id)))))
        (tel/error! {:id ::side-run :data {:user-id user-id :model-id model-id}} error)
        (update-side-answer! !thread answer-id
          #(assoc % :status "failed"
             :error (or (ex-message error) "The side question failed.")))))))

(defn- side-history
  "The provider turns of the thread so far. Turns come in question and answer
   pairs, and a pair whose answer did not complete is left out, so the request
   never carries two user turns in a row."
  [turns]
  (into []
    (comp (partition-all 2)
      (filter (fn [[_ answer]] (= "completed" (:status answer))))
      cat
      (map #(select-keys % [:role :content])))
    turns))

(defn ask-side-question!
  "Add `question` to the side thread and start its answer.

   Pre:  `!thread` holds a `new-side-thread` value. `chat-id` is the chat the
         passage came from.
   Post: {:ok true} with a user turn and a streaming answer turn appended, or
         {:error} and the thread unchanged. The call does not wait for the
         provider."
  [!thread user-id chat-id page-topic-id question]
  (let [thread @!thread
        question (str/trim (or question ""))
        chat (db/get-assistant-chat user-id chat-id)
        api-key (settings/get-openrouter-api-key user-id)]
    (cond
      (:closed? thread) {:error "This side question is closed."}
      (str/blank? question) {:error "Type a question."}
      (side-turn-streaming? thread) {:error "Wait for the answer to finish."}
      (nil? chat) {:error "Chat not found."}
      (str/blank? api-key) {:error "OpenRouter API key not configured."}
      :else
      (let [gate (credits/check-cost-billed-balance! user-id)]
        (if-not (:ok gate)
          {:error (:error gate)}
          (let [entry (model-entry user-id (:assistant_chats/root_topic_id chat))
                model-fields (openrouter/chat-completion-fields entry
                               {:reasoning_effort (settings/get-assistant-reasoning user-id)})
                turns (conj (side-history (:turns thread))
                        {:role "user" :content question})
                rendered (side-request user-id chat page-topic-id (:passage thread) turns
                           model-fields (card-models/cache-breakpoint? (:id entry)))
                answer-id (str (random-uuid))
                evidence (mapv creation->evidence-row (:evidence rendered))]
            (swap! !thread update :turns into
              [{:id (str (random-uuid)) :role "user" :content question}
               {:id answer-id :role "assistant" :content "" :status "streaming"
                :evidence evidence}])
            ((m/via side-run-executor
               (execute-side-run! !thread user-id answer-id api-key (:id entry)
                 (:provider-request rendered)))
             (fn [_] nil)
             (fn [error]
               (tel/error! {:id ::side-run-executor :data {:user-id user-id}} error)))
            {:ok true}))))))

(defn side-thread-rows
  "The side thread as transcript rows, so the panel renders a side answer
   through the same view, citations and math as a main answer.
   Post: a vector in turn order."
  [thread]
  (mapv (fn [{:keys [id role content status error evidence]}]
          (transcript-message-view
            {:assistant_messages/id id
             :assistant_messages/role role
             :assistant_messages/content content
             :assistant_run/status status
             :assistant_run/error error
             :assistant_run/evidence (or evidence [])}))
    (:turns thread)))

(defn side-thread->cards!
  "Queue cards from the whole side thread: the passage, each question and each
   answer. The answer the passage came from is the context.
   Post: as `enqueue-assistant-cards!`, or {:error} when no answer has finished."
  [user-id page-topic-id thread]
  (tel/log! {:level :info :id ::side-thread-cards-debug
             :data {:user-id user-id :page-topic-id page-topic-id
                    :turns (count (:turns thread))}}
    "side-thread->cards! entered")
  (let [answered (filter #(and (= "assistant" (:role %)) (= "completed" (:status %)))
                   (:turns thread))]
    (if (empty? answered)
      {:error "Ask a question and wait for its answer first."}
      (enqueue-assistant-cards! user-id page-topic-id
        (apply str
          "<blockquote><p>" (escape-html (:passage thread)) "</p></blockquote>"
          (for [{:keys [role content status]} (:turns thread)
                :when (or (= "user" role) (= "completed" status))]
            (if (= "user" role)
              (str "<p><strong>Question:</strong> " (escape-html content) "</p>")
              (markdown/markdown-with-math->html (answer-context-text content)))))
        (some-> (db/get-assistant-message user-id (:message-id thread))
          :content answer-context-text)))))

(def ^:private max-referenceable-files
  "Most repo files the @ picker lists for one query. A repository can hold
   thousands of files, so the learner narrows the list by typing."
  50)

(def ^:private unreferenceable-kinds
  "Root kinds the @ picker never offers, because a reference to one sends the
   model no text. A repository root holds none: `referenceable-docs` offers
   its files instead. A score holds a sheet-music PDF and a recording. No
   import makes text from either."
  #{"repo" "score"})

(defn referenceable-docs
  "Return what a learner can reference from a chat: root documents, then repo
   files.

   A repository root holds no text of its own. Its files live in `topic_code`
   under commit and directory nodes, so a reference to the root gave the model
   nothing. The list offers the files instead, from the newest commit of each
   repository, and only those with stored source. A blank query lists no files:
   the panel computes this list while the menu is closed, and a bare `@` would
   bury the documents under files the learner did not ask for.

   Pre: `query` is the unfinished text after `@`, or an empty string.
   Post: the documents exclude the open document and every root of an
         `unreferenceable-kinds` kind, in newest-first order. At most
         `max-referenceable-files` files follow them, each titled
         `<repo title>/<path>`.
   Invariant: every result has an `:id` and a displayable `:title`."
  ([user-id current-root-topic-id]
   (referenceable-docs user-id current-root-topic-id ""))
  ([user-id current-root-topic-id query]
   (let [needle (str/lower-case (or query ""))
         documents (into []
                     (comp
                       (remove #(= current-root-topic-id (:topics/id %)))
                       (remove #(unreferenceable-kinds (:topics/kind %)))
                       (map (fn [topic]
                              {:id (:topics/id topic)
                               :title (or (:topics/title topic) "Untitled")}))
                       (filter #(str/includes? (str/lower-case (:title %)) needle)))
                     (db/get-root-topics user-id))]
     (if (str/blank? needle)
       documents
       (into documents
         (map (fn [{:keys [id label]}] {:id id :title label}))
         (db/get-referenceable-repo-files user-id needle max-referenceable-files))))))

;; ── Reactive read wrappers (first arg = the relevant watched revision) ──────

(def max-rich-answer-chars
  "Maximum answer size for the formatted terminal renderer."
  100000)

(def ^:private transcript-wire-keys
  [:assistant_messages/id :assistant_messages/role
   :assistant_messages/content :assistant_messages/client_id
   :assistant_messages/inherited?
   :assistant_run/uuid :assistant_run/status :assistant_run/error
   :assistant_run/retry_of :assistant_run/evidence
   :assistant_event/type :assistant_event/covered_from
   :assistant_event/covered_through :assistant_event/chat_title])

(def ^:private evidence-wire-keys
  [:citation_key :document_topic_id :page_topic_id :page_number :anchor
   :quoted_text :quoted_html :document_title :display_label :walkthrough_id])

(def ^:private retained-html-pattern
  #"(?is)<(?:blockquote|code|div|figure|h[1-6]|li|ol|p|pre|table|td|th|tr|ul)\b")

(defn- cited-keys
  "The evidence keys that `answer` cites with a complete evidence token, such
   as `[evidence:E1]` or `[evidence:E1, E2]`. A token still streaming in cites
   nothing yet."
  [answer]
  (into #{} (mapcat (comp token-keys second))
    (re-seq evidence-token-pattern (or answer ""))))

(defn- evidence-wire-row
  "One evidence row as the browser gets it, with ONE quote form. The evidence
   dialog shows `:quoted_html` when it exists and falls back to `:quoted_text`,
   so a row with HTML sends no text. The text is what the model read, and the
   database keeps it for the run details."
  [row]
  (let [wire-row (select-keys row evidence-wire-keys)]
    (cond
      (:quoted_html row)
      (-> wire-row
        (dissoc :quoted_text)
        (assoc :quoted_html (cleaner/clean-html-fragment (:quoted_html row))))

      (and (:quoted_text row) (re-find retained-html-pattern (:quoted_text row)))
      (assoc wire-row :quoted_text (text/strip-html (:quoted_text row)))

      :else wire-row)))

(defn- plain-evidence-citations [answer evidence-rows]
  (let [by-key (into {} (map (juxt :citation_key identity)) evidence-rows)]
    (str/replace (or answer "") evidence-token-pattern
      (fn [[_ key-list]]
        (str/join "; "
          (keep #(:display_label (get by-key %)) (token-keys key-list)))))))

(defn transcript-message-view
  "Prepare one transcript row for the browser.

   Pre: message comes from messages*. The answer content is trusted Markdown,
   but its HTML is not trusted.
   Post: returns a wire-safe row with one render mode. Rich HTML is sanitized.
   `:assistant_run/evidence` holds only the rows the answer cites.

   The browser reads evidence in one place: the citation click, which opens the
   row the clicked link names. An answer links only what it cites. This row
   used to carry the run's whole evidence, which for a repo file is the whole
   file twice, as text and as HTML. Each stream update re-sent the answer's
   row, so every update was a 700 KB WebSocket message.

   A streaming answer renders through the SAME mode as its terminal answer.
   Formatting only at the end made the panel show unformatted Markdown source
   for the whole run and then swap renderers on the last frame, and that swap
   unmounts one node and mounts another — the answer blanked for a frame right
   before it settled. One mode for both states removes the swap and formats the
   text as it arrives. Partial Markdown is re-parsed on each publication, so an
   unclosed fence formats itself once its closing fence lands."
  [message]
  (let [;; A question-map block is data for the server, never text for the
        ;; reader. Stripping it here covers the stream, a stopped run and a
        ;; terminal one alike, and an open block shows as an empty answer
        ;; until it closes (plans/anchored-questions.md §6.3.2.1).
        message (cond-> message
                  (= "assistant" (:assistant_messages/role message))
                  (update :assistant_messages/content map-block/prose))
        wire-message (select-keys message transcript-wire-keys)
        cited? (cited-keys (:assistant_messages/content message))
        wire-message (if (contains? wire-message :assistant_run/evidence)
                       (update wire-message :assistant_run/evidence
                         (fn [rows]
                           (into []
                             (comp (filter #(cited? (:citation_key %)))
                               (map evidence-wire-row))
                             rows)))
                       wire-message)
        assistant? (= "assistant" (:assistant_messages/role message))]
    (if-not assistant?
      wire-message
      (let [content (or (:assistant_messages/content message) "")
            rich? (and (not (str/blank? content))
                    (<= (count content) max-rich-answer-chars)
                    (config/assistant-rich-render?))]
        (cond
          (str/blank? content)
          (assoc wire-message :assistant_messages/render-mode "empty")

          rich?
          (try
            (assoc wire-message
              :assistant_messages/render-mode "rich-html"
              :assistant_messages/safe-html
              (markdown/markdown-with-math->html
                (render-evidence-citations content
                  (:assistant_run/evidence message))))
            (catch Throwable error
              (tel/error! {:id ::assistant-rich-render
                           :data {:message-id (:assistant_messages/id message)
                                  :run-uuid (:assistant_run/uuid message)
                                  :response-chars (count content)}} error)
              (assoc wire-message
                :assistant_messages/render-mode "plain-text"
                :assistant_messages/content
                (plain-evidence-citations content
                  (:assistant_run/evidence message)))))

          :else
          (assoc wire-message
            :assistant_messages/render-mode "plain-text"
            :assistant_messages/content
            (plain-evidence-citations content
              (:assistant_run/evidence message))))))))

(defn chats*
  "Live chats for the panel and its history view, as
   db/get-assistant-chats returns them. `_rev` forces re-query on bump."
  [_rev user-id root-topic-id]
  (db/get-assistant-chats user-id root-topic-id))

(defn- message-events
  "`messages` of the chat `chat-id` as transcript rows. An answer row reads its
   durable text from its run and carries only the evidence that text cites."
  [user-id chat-id messages]
  (let [runs (db/get-assistant-run-summaries-for-chat user-id chat-id)
        run-by-message (into {} (keep (fn [run]
                                        (when-let [message-id (:assistant_message_id run)]
                                          [message-id run]))) runs)
        evidence-by-run (group-by :run_uuid
                          (db/get-cited-assistant-evidence-for-chat user-id chat-id))]
    (mapv (fn [message]
            (if-let [run (get run-by-message (:assistant_messages/id message))]
              (assoc message
                :assistant_messages/content (:response_text run)
                :assistant_run/uuid (:run_uuid run)
                :assistant_run/status (:status run)
                :assistant_run/error (:error_text run)
                :assistant_run/retry_of (:retry_of_run_uuid run)
                :assistant_run/evidence (get evidence-by-run (:run_uuid run) []))
              message))
      messages)))

(defn- checkpoint-event
  "One checkpoint as a transcript event row. A checkpoint of another chat,
   made for this chat's answer, names that chat (§4.5.5)."
  [checkpoint]
  (cond-> {:assistant_messages/id (str "checkpoint-" (:id checkpoint))
           :assistant_messages/role "event"
           :assistant_messages/content (:summary checkpoint)
           :assistant_event/type "compact"
           :assistant_event/covered_from (:covered_from_message_id checkpoint)
           :assistant_event/covered_through (:covered_through_message_id checkpoint)
           :assistant_run/uuid (:run_uuid checkpoint)}
    (:chat_title checkpoint)
    (assoc :assistant_event/chat_title (:chat_title checkpoint))))

(defn messages*
  "Transcript and checkpoint events for an owned chat. `_rev` forces re-query.
   Active answer rows read their durable response text from the run authority.
   An answer row carries only the evidence its text cites.

   A fork shows the rows it inherited first, each flagged
   :assistant_messages/inherited?, then a \"fork\" event that names the parent,
   then its own rows (plans/assistant-chat-forks.md §4.2.3). Inherited answers
   keep their own runs, so their citations open the ancestor's evidence.
   Checkpoints sort after the last row they cover. A checkpoint of another
   chat made for this chat's answer sorts after the last own row written
   before it."
  [_rev user-id chat-id]
  (when-let [chat (and chat-id (db/get-assistant-chat user-id chat-id))]
    (let [lineage (chat-lineage user-id chat)
          path (tree/inherited-path lineage chat-id)
          order (fn [section n] #(assoc % ::order [section n]))
          inherited (mapcat (fn [[ancestor-id through]]
                              (map #(assoc % :assistant_messages/inherited? true)
                                (message-events user-id ancestor-id
                                  (db/get-assistant-messages ancestor-id through))))
                      path)
          own (message-events user-id chat-id (db/get-assistant-messages chat-id))
          checkpoints (db/get-assistant-checkpoints user-id chat-id)
          after (fn [id] (inc (* 2 (or id 0))))]
      (->> (concat
             (map #((order 0 (* 2 (:assistant_messages/id %))) %) inherited)
             (keep (fn [checkpoint]
                     (when (= "inherited" (:scope checkpoint))
                       ((order 0 (after (:covered_through_message_id checkpoint)))
                        (checkpoint-event checkpoint))))
               checkpoints)
             (when (seq path)
               [((order 1 0)
                 {:assistant_messages/id "fork"
                  :assistant_messages/role "event"
                  :assistant_event/type "fork"
                  :assistant_messages/content
                  (:title (get lineage (:assistant_chats/parent_chat_id chat)))})])
             (map #((order 2 (* 2 (:assistant_messages/id %))) %) own)
             (keep (fn [checkpoint]
                     (when-not (= "inherited" (:scope checkpoint))
                       ((order 2 (after (:covered_through_message_id checkpoint)))
                        (checkpoint-event checkpoint))))
               checkpoints)
             (map (fn [checkpoint]
                    ((order 2 (after (:after_message_id checkpoint)))
                     (checkpoint-event checkpoint)))
               (db/get-assistant-checkpoints-made-for user-id chat-id)))
        (sort-by ::order)
        (mapv #(dissoc % ::order))))))

(defn message-client-id-present?
  "Return true when `rows` already carry the optimistic user message.

   The optimistic echo is retired against THE ROWS THE TRANSCRIPT RENDERS, never
   against a query of its own. A second query is a second server value, and the
   two land in different frames: the cheap boolean arrived first, the panel
   dropped the echo, and the rows it was waiting for were still crossing the
   wire — so the transcript painted empty white between the two. Reading one
   result closes that window by construction.

   Pre: `rows` is the messages* result for the open chat. `client-id` is a
   string or nil, and a nil ID returns false.
   Post: true only for a row already in `rows`."
  [rows client-id]
  (boolean
    (and client-id
      (some #(= client-id (:assistant_messages/client_id %)) rows))))

(defn active-run*
  "Active run summary for an owned chat. `_rev` forces re-query.
   A chat whose answer is being prepared has no run yet. It reports a
   compaction with status \"preparing\", so the panel shows Compacting
   context… and offers Stop (plans/assistant-chat-forks.md §4.5.4)."
  [_rev user-id chat-id]
  (when chat-id
    (or (some-> (db/get-active-assistant-run user-id chat-id)
          (select-keys [:run_uuid :kind :status]))
      (when (and (preparing? chat-id) (db/get-assistant-chat user-id chat-id))
        {:run_uuid nil :kind "compact" :status "preparing"}))))

(defn run-details*
  "Owner-scoped answer details with wire-safe timestamps.

   The result crosses to the client whole, so it carries no sources. The
   evidence rows come from `run-sources*`, and `:source_snapshot` is dropped:
   for a run over one repo file it held 1.1 MB that the modal never read."
  [_rev user-id run-uuid]
  (when run-uuid
    (when-let [run (some-> (db/get-assistant-run user-id run-uuid)
                     (dissoc :source_snapshot))]
      (let [timestamp-keys [:created_at :started_at :first_token_at :stopped_at
                            :completed_at :failed_at]
            wire-run (reduce (fn [result key]
                               (update result key #(some-> % str)))
                       run timestamp-keys)]
        (assoc wire-run
          :cost_usd (some-> (:cost_usd wire-run) double))))))

(defn run-sources*
  "Owner-scoped evidence rows of one run, for the Sources table in Answer
   details. `_rev` forces re-query.

   A run retains every block it showed the model, so a run over one repo file
   can hold about 3000 rows and 700 KB. These rows are therefore not part of
   `run-details*`, whose value crosses to the client whole. The caller binds
   this result as a server form and reads one window row at a time.

   Post: a vector, empty for a nil run or a run the user does not own."
  [_rev user-id run-uuid]
  (if run-uuid
    (vec (db/get-assistant-run-evidence user-id run-uuid))
    []))
