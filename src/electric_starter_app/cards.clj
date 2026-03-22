(ns electric-starter-app.cards
  "Flashcard generation using OpenAI API with prompt templates."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.settings :as settings]
    [wkok.openai-clojure.api :as api]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.string :as str]))

(defn- humanize-error [msg]
  (cond
    (re-find #"(?i)API key not configured" (str msg))
    "No API key configured. Set one in Settings."

    (re-find #"(?i)401|unauthorized|invalid.*key" (str msg))
    "Invalid API key. Check your key in Settings."

    (re-find #"(?i)429|rate.?limit" (str msg))
    "Rate limit reached. Wait a moment and try again."

    (re-find #"(?i)No content provided" (str msg))
    "No text on this page. Extract text first."

    (re-find #"(?i)timeout|timed.?out" (str msg))
    "Request timed out. Try again."

    :else (str msg)))

;; Prompt template loading
(defn load-prompt-template
  "Load a prompt template from resources/prompts/.
   Returns the file content as a string, or nil if not found."
  [filename]
  (try
    (-> (str "prompts/" filename)
        io/resource
        slurp)
    (catch Exception e
      (println "ERROR [load-prompt-template]:" (.getMessage e))
      nil)))

;; Prompt building
(defn build-basic-prompt
  "Build prompt for basic Q&A cards.
   Concatenates system.md + optional pre-prompt + basic.md + optional context.md."
  [card-count has-context? pre-prompt]
  (let [system (load-prompt-template "system.md")
        basic (load-prompt-template "basic.md")
        context (when has-context? (load-prompt-template "context.md"))]
    (when (and system basic)
      (str system
           (when (and pre-prompt (not (str/blank? pre-prompt)))
             (str "\n\n## Custom Question Format\n\n"
                  "Use the following pattern for your questions:\n\n"
                  pre-prompt
                  "\n\n"))
           basic
           (when context (str "\n\n" context))
           "\n\n# Instructions\n\n"
           "Generate EXACTLY " card-count " flashcards from the provided text. "
           "Return EXACTLY " card-count " items in the EDN vector — no more, no fewer. "
           "Do not exceed this count even if the content seems to warrant more cards."))))

(defn build-cloze-prompt
  "Build prompt for cloze deletion cards.
   Concatenates system.md + optional pre-prompt + cloze.md + optional context-cloze.md."
  [card-count has-context? pre-prompt]
  (let [system (load-prompt-template "system.md")
        cloze (load-prompt-template "cloze.md")
        context (when has-context? (load-prompt-template "context-cloze.md"))]
    (when (and system cloze)
      (str system
           (when (and pre-prompt (not (str/blank? pre-prompt)))
             (str "\n\n## Custom Question Format\n\n"
                  "Use the following pattern for your questions:\n\n"
                  pre-prompt
                  "\n\n"))
           cloze
           (when context (str "\n\n" context))
           "\n\n# Instructions\n\n"
           "Generate EXACTLY " card-count " flashcards from the provided text. "
           "Return EXACTLY " card-count " items in the EDN vector — no more, no fewer. "
           "Do not exceed this count even if the content seems to warrant more cards."))))

;; Context retrieval
(defn get-context-pages
  "Get text from previous N pages for context.
   parent-id is the root PDF topic. Returns concatenated text from pages in range."
  [parent-id page-number context-window]
  (let [start-page (max 1 (- page-number context-window))
        end-page (dec page-number)]
    (when (>= end-page start-page)
      (let [pages (db/get-context-pages parent-id start-page end-page)]
        (when (seq pages)
          (->> pages
               (map :topics/content)
               (str/join "\n\n---\n\n")))))))

(defn get-extract-page-context
  "Get the original PDF page text for use as context when generating cards from an extract."
  [parent-id page-number]
  (when-let [page-text (:topics/content (first (db/get-context-pages parent-id page-number page-number)))]
    page-text))

;; OpenAI API calls
(defn parse-edn-response
  "Parse OpenAI response as EDN. Handles markdown code fences.
   Returns parsed EDN or throws exception."
  [raw-text]
  (let [cleaned (-> raw-text
                    str/trim
                    (str/replace #"^```(?:clojure|edn)?\s*\n?" "")
                    (str/replace #"\n?```\s*$" "")
                    str/trim)]
    (try
      (edn/read-string cleaned)
      (catch Exception e
        (println "ERROR [parse-edn-response]: Failed to parse EDN")
        (println "Raw response:" raw-text)
        (println "Cleaned response:" cleaned)
        (throw (ex-info "Failed to parse EDN response from OpenAI"
                        {:raw raw-text :cleaned cleaned}))))))

(defn- generate-cards*
  "Shared implementation for card generation with up to 3 retry attempts.
   Validates that the returned card count matches the requested count."
  [{:keys [content context card-count model user-id pre-prompt enc-key]} prompt-builder-fn]
  (let [api-key      (settings/get-openai-api-key user-id enc-key)
        _            (when (empty? api-key) (throw (ex-info "OpenAI API key not configured" {})))
        _            (when (empty? content) (throw (ex-info "No content provided" {})))
        card-count   (or card-count (settings/get-card-count user-id))
        model        (or model (settings/get-model user-id))
        reasoning    (settings/get-reasoning user-id)
        verbosity    (settings/get-verbosity user-id)
        has-context? (not (empty? context))
        prompt       (prompt-builder-fn card-count has-context? pre-prompt)
        _            (when-not prompt (throw (ex-info "Failed to load prompt templates" {})))
        content-text (if has-context? (pr-str {:content content :context context}) content)]
    (loop [attempt 1]
      (let [response     (api/create-chat-completion
                           {:model    model
                            :messages [{:role "system" :content prompt}
                                       {:role "user"   :content content-text}]
                            :reasoning {:effort reasoning}
                            :text      {:verbosity verbosity}}
                           {:api-key   api-key
                            :reasoning {:effort reasoning}
                            :verbosity verbosity})
            raw-text     (-> response :choices first :message :content)
            cards        (parse-edn-response raw-text)
            actual-count (count cards)]
        (cond
          (= actual-count card-count)
          {:success true :cards cards}

          (>= attempt 3)
          (do (println "ERROR [generate-cards*]: Count mismatch after 3 attempts."
                       "Expected:" card-count "Got:" actual-count)
              {:success false
               :error (str "LLM returned " actual-count " cards instead of "
                           card-count " after 3 attempts")})

          :else
          (do (println "WARN [generate-cards*]: Attempt" attempt "returned" actual-count
                       "cards, expected" card-count "— retrying")
              (recur (inc attempt))))))))

(defn generate-basic-cards
  "Generate basic Q&A flashcards using OpenAI API.
   Options:
   - :content - The main text to generate cards from (required)
   - :context - Optional context text from previous pages
   - :card-count - Number of cards to generate (default from settings)
   - :model - OpenAI model to use (default from settings)
   - :pre-prompt - Optional custom formatting instructions
   - :enc-key - Base64 key from session for API key decryption
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [opts]
  (try
    (generate-cards* opts build-basic-prompt)
    (catch Exception e
      (println "ERROR [generate-basic-cards]:" (.getMessage e))
      {:success false :error (humanize-error (.getMessage e))})))

(defn generate-cloze-cards
  "Generate cloze deletion flashcards using OpenAI API.
   Options same as generate-basic-cards.
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [opts]
  (try
    (generate-cards* opts build-cloze-prompt)
    (catch Exception e
      (println "ERROR [generate-cloze-cards]:" (.getMessage e))
      {:success false :error (humanize-error (.getMessage e))})))

;; Card persistence
(defn save-cards
  "Save generated cards to the database.
   For basic cards: expects [{:q \"...\" :a \"...\"}]
   For cloze cards: expects [{:c \"...\"}]
   topic-id: the page or extract topic that owns these cards.
   root-topic-id: the root PDF/EPUB/web/basic topic.
   Optional source-reference propagates document source info to each card."
  ([topic-id root-topic-id kind cards]
   (save-cards topic-id root-topic-id kind cards nil))
  ([topic-id root-topic-id kind cards source-reference]
   (try
     (let [rows (map (fn [card]
                       (cond-> (if (= kind "basic")
                                 {:topic_id topic-id
                                  :root_topic_id root-topic-id
                                  :kind kind
                                  :question (:q card)
                                  :answer (:a card)
                                  :cloze nil}
                                 {:topic_id topic-id
                                  :root_topic_id root-topic-id
                                  :kind kind
                                  :question nil
                                  :answer nil
                                  :cloze (:c card)})
                         source-reference
                         (assoc :source_reference source-reference)))
                     cards)]
       (db/insert-flashcards! rows)
       {:success true})
     (catch Exception e
       (println "ERROR [save-cards]:" (.getMessage e))
       {:success false :error (.getMessage e)}))))

(defn add-card
  "Manually add a single flashcard.
   topic-id: the page or extract topic.
   root-topic-id: the root topic.
   Optional source-reference propagates document source info to the card."
  ([topic-id root-topic-id kind fields]
   (add-card topic-id root-topic-id kind fields nil))
  ([topic-id root-topic-id kind fields source-reference]
   (try
     (let [cards (if (= kind "basic")
                   [{:q (:question fields) :a (:answer fields)}]
                   [{:c (:cloze fields)}])]
       (save-cards topic-id root-topic-id kind cards source-reference))
     (catch Exception e
       (println "ERROR [add-card]:" (.getMessage e))
       {:success false :error (.getMessage e)}))))

(defn get-cards
  "Get all flashcards for a specific topic (page or extract)."
  [topic-id]
  (try
    (let [cards (db/get-flashcards topic-id)]
      {:success true :cards cards})
    (catch Exception e
      (println "ERROR [get-cards]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn delete-card
  "Delete a single flashcard by ID. Returns {:success true :anki-note-id N} if the card was synced."
  [card-id]
  (try
    (let [deleted (db/delete-flashcard! card-id)
          note-id (:flashcards/anki_note_id deleted)]
      {:success true :anki-note-id note-id})
    (catch Exception e
      (println "ERROR [delete-card]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn update-card
  "Update a flashcard with validation. Returns {:success bool :error string}"
  [card-id updated-fields]
  (try
    (when (and (contains? updated-fields :question)
               (str/blank? (:question updated-fields)))
      (throw (ex-info "Question cannot be empty" {})))
    (when (and (contains? updated-fields :answer)
               (str/blank? (:answer updated-fields)))
      (throw (ex-info "Answer cannot be empty" {})))
    (when (and (contains? updated-fields :cloze)
               (str/blank? (:cloze updated-fields)))
      (throw (ex-info "Cloze text cannot be empty" {})))

    (db/update-flashcard! card-id updated-fields)
    {:success true}

    (catch Exception e
      (println "ERROR [update-card]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

;; CSV Export
(defn export-cards-csv
  "Export flashcards as CSV for Anki import.
   Options:
   - root-topic-id: root topic ID (replaces document-id)
   - topic-id: specific topic to export (nil = all under root)
   - kind: \"basic\", \"cloze\", or nil (all kinds)
   - header-text: Optional text to prepend to each card's front
   Returns {:success true :csv \"...\" :filename \"...\"}
   or {:success false :error \"...\"}"
  [{:keys [root-topic-id topic-id kind header-text]}]
  (try
    (let [root (db/get-topic root-topic-id)
          _ (when-not root
              (throw (ex-info "Topic not found" {:root-topic-id root-topic-id})))
          doc-filename (-> (:topics/title root)
                           (str/replace #"\.pdf$" "")
                           (str/replace #"[^a-zA-Z0-9_-]" "_"))
          ;; Query cards
          all-cards (if topic-id
                     (db/get-flashcards topic-id)
                     (db/get-all-flashcards root-topic-id))
          _ (println "  Found" (count all-cards) "cards before filtering")
          _ (when (seq all-cards)
              (println "  Card kinds in DB:" (set (map :flashcards/kind all-cards))))
          ;; Filter by kind if specified
          cards (if kind
                  (do
                    (println "  Filtering for kind:" (pr-str kind))
                    (filter #(= (:flashcards/kind %) kind) all-cards))
                  all-cards)
          _ (println "  Found" (count cards) "cards after kind filter")
          _ (when-not (seq cards)
              (throw (ex-info "No cards to export" {:count (count all-cards) :kind kind})))
          ;; Format header text if provided
          header-html (when (and header-text (not (str/blank? header-text)))
                        (str "<p>" header-text "</p>"))
          ;; Format as CSV (comma-separated, with quoted fields)
          csv-lines (map (fn [card]
                          (let [card-kind (:flashcards/kind card)
                                tag doc-filename
                                front (if (= card-kind "basic")
                                       (str (when header-html header-html)
                                            "<p>" (:flashcards/question card) "</p>")
                                       (str "<p>" (:flashcards/cloze card) "</p>"))
                                back (if (= card-kind "basic")
                                      (str "<p>" (:flashcards/answer card) "</p>")
                                      "")
                                escape-csv (fn [s]
                                            (str "\"" (str/replace (or s "") "\"" "\"\"") "\""))]
                            (str (escape-csv front) ","
                                 (escape-csv back) ","
                                 (escape-csv tag))))
                        cards)
          csv-string (str/join "\n" csv-lines)
          kind-suffix (if kind (str "_" kind) "")
          topic-suffix (if topic-id (str "_topic" topic-id) "_all")
          filename (str doc-filename topic-suffix kind-suffix ".csv")]
      (println "  SUCCESS: Generated" filename "with" (count cards) "cards")
      (let [card-ids (mapv :flashcards/id cards)]
        (db/mark-cards-exported card-ids))
      {:success true :csv csv-string :filename filename})
    (catch Exception e
      (let [error-msg (or (ex-message e) (.getMessage e))]
        (println "ERROR [export-cards-csv]:" error-msg)
        (println "  Root topic ID:" root-topic-id ", Topic ID:" topic-id ", Kind:" kind)
        {:success false :error error-msg}))))
