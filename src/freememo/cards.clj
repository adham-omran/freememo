(ns freememo.cards
  "Flashcard generation using OpenAI API with prompt templates."
  (:require
   [freememo.db :as db]
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [wkok.openai-clojure.api :as api]
   [taoensso.telemere :as tel]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defn- root-cause
  "Walk an exception chain to the outermost-thrown cause (last in chain)."
  [e]
  (if-let [c (.getCause e)] (recur c) e))

(defn- error-type
  "Map the root cause's ex-data :type to a UI error-type keyword.
   nil if not a recognized class. Walks the chain because tel/trace! wraps."
  [e]
  (when (= ::insufficient-credits (:type (ex-data (root-cause e))))
    :insufficient-credits))

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
      (tel/error! {:id ::load-prompt-template} e)
      nil)))

;; Prompt building
(defn build-basic-prompt
  "Build prompt for basic Q&A cards.
   Concatenates system prompt + optional pre-prompt + basic.md + optional context.md."
  [card-count has-context? pre-prompt user-id]
  (let [system (settings/get-system-prompt user-id)
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
   Concatenates system prompt + optional pre-prompt + cloze.md + optional context-cloze.md."
  [card-count has-context? pre-prompt user-id]
  (let [system (settings/get-system-prompt user-id)
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
        (tel/error! {:id ::parse-edn-response
                     :data {:raw raw-text :cleaned cleaned}}
          "Failed to parse EDN response from OpenAI")
        (throw (ex-info "Failed to parse EDN response from OpenAI"
                 {:raw raw-text :cleaned cleaned}))))))

(defn- generate-cards*
  "Shared implementation for card generation with up to 3 retry attempts.
   Validates that the returned card count matches the requested count.
   endpoint-tag is :cards.basic or :cards.cloze for usage logging."
  [{:keys [content context card-count model user-id pre-prompt enc-key]} prompt-builder-fn endpoint-tag]
  (let [api-key (settings/get-openai-api-key user-id enc-key)
        _ (when (empty? api-key) (throw (ex-info "OpenAI API key not configured" {})))
        _ (when (empty? content) (throw (ex-info "No content provided" {})))
        card-count (or card-count (settings/get-card-count user-id))
        model (or model (settings/get-model user-id))
        _ (let [gate (credits/check-balance! user-id model)]
            (when-not (:ok gate)
              (throw (ex-info (:error gate) {:type ::insufficient-credits}))))
        max-retries (settings/get-card-gen-max-retries user-id)
        reasoning (settings/get-reasoning user-id)
        verbosity (settings/get-verbosity user-id)
        has-context? (not (empty? context))
        prompt (prompt-builder-fn card-count has-context? pre-prompt user-id)
        _ (when-not prompt (throw (ex-info "Failed to load prompt templates" {})))
        content-text (if has-context? (pr-str {:content content :context context}) content)]
    (loop [attempt 1
           attempts-tokens []]
      (let [t-start (System/nanoTime)
            response (api/create-chat-completion
                       {:model model
                        :messages [{:role "system" :content prompt}
                                   {:role "user" :content content-text}]
                        :reasoning {:effort reasoning}
                        :text {:verbosity verbosity}}
                       {:api-key api-key
                        :reasoning {:effort reasoning}
                        :verbosity verbosity})
            duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))
            usage (:usage response)
            raw-text (-> response :choices first :message :content)
            _ (tel/log! {:level :info :id ::openai-completion
                         :data {:user-id user-id
                                :model model
                                :endpoint endpoint-tag
                                :prompt-tokens (:prompt_tokens usage)
                                :completion-tokens (:completion_tokens usage)
                                :cached-tokens (get-in usage [:prompt_tokens_details :cached_tokens])
                                :reasoning-tokens (get-in usage [:completion_tokens_details :reasoning_tokens])
                                :duration-ms duration-ms
                                :attempt attempt}}
                "OpenAI completion")
            cards (parse-edn-response raw-text)
            actual-count (count cards)
            attempts-tokens' (conj attempts-tokens (credits/usage->tokens usage))]
        (cond
          (= actual-count card-count)
          ;; Success — bill all attempts (§5.4.5). record-charge! is total: a
          ;; billing failure logs ::credit-charge-failed and returns nil, never
          ;; discarding generated cards.
          (do (credits/record-charge! user-id endpoint-tag model attempts-tokens')
            {:success true :cards cards})

          (>= attempt max-retries)
          (do (tel/log! {:level :error :id ::generate-cards-count-mismatch
                         :data {:expected card-count :got actual-count :attempts attempt}}
                "Count mismatch after max retries")
            {:success false
             :error (str "LLM returned " actual-count " cards instead of "
                      card-count " after " attempt " attempts")})

          :else
          (do (tel/log! {:level :warn :id ::generate-cards-retry
                         :data {:attempt attempt :expected card-count :got actual-count}}
                "Card count mismatch, retrying")
            (recur (inc attempt) attempts-tokens')))))))

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
    (tel/trace! {:id ::generate-basic}
      (generate-cards* opts build-basic-prompt :cards.basic))
    (catch Exception e
      ;; Out-of-credits refusal is a normal business outcome, not a pipeline
      ;; failure — keep it off the :error channel (alert email noise).
      (if (= :insufficient-credits (error-type e))
        (tel/log! {:level :info :id ::spend-refused :data {:user-id (:user-id opts)}}
          "Card generation refused: out of credits")
        (tel/error! {:id ::generate-basic-cards} e))
      {:success false
       :error (humanize-error (.getMessage (root-cause e)))
       :error-type (error-type e)})))

(defn generate-cloze-cards
  "Generate cloze deletion flashcards using OpenAI API.
   Options same as generate-basic-cards.
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [opts]
  (try
    (tel/trace! {:id ::generate-cloze}
      (generate-cards* opts build-cloze-prompt :cards.cloze))
    (catch Exception e
      (if (= :insufficient-credits (error-type e))
        (tel/log! {:level :info :id ::spend-refused :data {:user-id (:user-id opts)}}
          "Card generation refused: out of credits")
        (tel/error! {:id ::generate-cloze-cards} e))
      {:success false
       :error (humanize-error (.getMessage (root-cause e)))
       :error-type (error-type e)})))

;; Card persistence

(defn effective-pins-for-bake
  "Return pin rows for topic-id ordered by ord ASC.
   Under EC-snapshot semantics this is a direct local lookup — no ancestor walk."
  [topic-id]
  (db/get-pins topic-id))

(defn bake-card-html
  "Append pinned <img> tags to card field HTML at gen/Add-Item time.

  topic-id  – int: the topic the card belongs to.
  kind      – \"basic\" or \"cloze\".
  fields    – map:
                basic: {:q html :a html}
                cloze: {:c html :a html-or-nil}  (:a is back-extra, unchanged)

  Returns fields map with img tags appended per placement.

  Basic:
    front pins → <p><img src=\"/api/media/<id>\"></p> appended to :q
    back  pins → same appended to :a

  Cloze (F2-uniform):
    ALL pins (front + back) appended to :c in ord order.
    :a (back-extra) is never modified by bake."
  [topic-id kind fields]
  (let [pins (effective-pins-for-bake topic-id)]
    (if (empty? pins)
      fields
      (let [img-tag (fn [media-id]
                      (str "<p><img src=\"/api/media/" media-id "\"></p>"))]
        (if (= kind "basic")
          (let [front-imgs (->> pins
                             (filter #(= "front" (:topic_pins/placement %)))
                             (map #(img-tag (:topic_pins/media_id %)))
                             (apply str))
                back-imgs (->> pins
                            (filter #(= "back" (:topic_pins/placement %)))
                            (map #(img-tag (:topic_pins/media_id %)))
                            (apply str))]
            (cond-> fields
              (not (str/blank? front-imgs)) (update :q str front-imgs)
              (not (str/blank? back-imgs)) (update :a str back-imgs)))
          ;; cloze — F2-uniform: all pins appended to :c regardless of placement
          (let [all-imgs (->> pins
                           (map #(img-tag (:topic_pins/media_id %)))
                           (apply str))]
            (cond-> fields
              (not (str/blank? all-imgs)) (update :c str all-imgs))))))))

(defn save-cards
  "Save generated cards to the database.
   For basic cards: expects [{:q \"...\" :a \"...\"}]
   For cloze cards: expects [{:c \"...\" :a html-or-nil}]
   topic-id: the page or extract topic that owns these cards.
   root-topic-id: the root PDF/EPUB/web/basic topic.
   Bakes pinned images into each card's HTML before insert."
  [topic-id root-topic-id kind cards]
  (try
    (let [rows (map (fn [card]
                      (let [baked (bake-card-html topic-id kind card)]
                        (if (= kind "basic")
                          {:topic_id topic-id
                           :root_topic_id root-topic-id
                           :kind kind
                           :question (:q baked)
                           :answer (:a baked)
                           :cloze nil}
                          {:topic_id topic-id
                           :root_topic_id root-topic-id
                           :kind kind
                           :question nil
                           :answer (:a baked)
                           :cloze (:c baked)})))
                 cards)]
      (db/insert-flashcards! rows)
      {:success true})
    (catch Exception e
      (tel/error! {:id ::save-cards} e)
      {:success false :error (.getMessage e)})))

(defn add-card
  "Manually add a single flashcard.
   topic-id: the page or extract topic.
   root-topic-id: the root topic."
  [topic-id root-topic-id kind fields]
  (try
    (let [cards (if (= kind "basic")
                  [{:q (:question fields) :a (:answer fields)}]
                  [{:c (:cloze fields)}])]
      (save-cards topic-id root-topic-id kind cards))
    (catch Exception e
      (tel/error! {:id ::add-card} e)
      {:success false :error (.getMessage e)})))

(defn get-cards
  "Get all flashcards for a specific topic (page or extract)."
  [topic-id]
  (try
    (let [cards (db/get-flashcards topic-id)]
      {:success true :cards cards})
    (catch Exception e
      (tel/error! {:id ::get-cards} e)
      {:success false :error (.getMessage e)})))

(defn delete-card
  "Delete a single flashcard by ID. Returns {:success true :anki-note-id N} if the card was synced."
  [card-id]
  (try
    (let [deleted (db/delete-flashcard! card-id)
          note-id (:flashcards/anki_note_id deleted)]
      {:success true :anki-note-id note-id})
    (catch Exception e
      (tel/error! {:id ::delete-card} e)
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
      (tel/error! {:id ::update-card} e)
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
          _ (tel/log! {:level :debug :id ::export-pre-filter
                       :data {:count (count all-cards)
                              :kinds (set (map :flashcards/kind all-cards))}}
              "Cards loaded before filtering")
          ;; Filter by kind if specified
          cards (if kind
                  (filter #(= (:flashcards/kind %) kind) all-cards)
                  all-cards)
          _ (tel/log! {:level :debug :id ::export-post-filter
                       :data {:count (count cards) :kind kind}}
              "Cards after kind filter")
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
      (tel/log! {:level :info :id ::export-success
                 :data {:filename filename :count (count cards)}}
        "CSV export complete")
      (let [card-ids (mapv :flashcards/id cards)]
        (db/mark-cards-exported card-ids))
      {:success true :csv csv-string :filename filename})
    (catch Exception e
      (let [error-msg (or (ex-message e) (.getMessage e))]
        (tel/error! {:id ::export-cards-csv
                     :data {:root-topic-id root-topic-id :topic-id topic-id :kind kind}}
          error-msg)
        {:success false :error error-msg}))))
