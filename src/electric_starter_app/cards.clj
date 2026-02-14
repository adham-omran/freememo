(ns electric-starter-app.cards
  "Flashcard generation using OpenAI API with prompt templates."
  (:require
    [electric-starter-app.db :as db]
    [electric-starter-app.settings :as settings]
    [wkok.openai-clojure.api :as api]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    [clojure.string :as str]))

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
   Concatenates system.md + basic.md + optional context.md."
  [card-count has-context?]
  (let [system (load-prompt-template "system.md")
        basic (load-prompt-template "basic.md")
        context (when has-context? (load-prompt-template "context.md"))]
    (when (and system basic)
      (str system "\n\n" basic
           (when context (str "\n\n" context))
           "\n\n# Instructions\n\n"
           "Generate " card-count " flashcards from the provided text."))))

(defn build-cloze-prompt
  "Build prompt for cloze deletion cards.
   Concatenates system.md + cloze.md + optional context-cloze.md."
  [card-count has-context?]
  (let [system (load-prompt-template "system.md")
        cloze (load-prompt-template "cloze.md")
        context (when has-context? (load-prompt-template "context-cloze.md"))]
    (when (and system cloze)
      (str system "\n\n" cloze
           (when context (str "\n\n" context))
           "\n\n# Instructions\n\n"
           "Generate " card-count " flashcards from the provided text."))))

;; Context retrieval
(defn get-context-pages
  "Get text from previous N pages for context.
   Returns concatenated text from pages in range."
  [document-id page-number context-window]
  (let [start-page (max 1 (- page-number context-window))
        end-page (dec page-number)]
    (when (>= end-page start-page)
      (let [pages (db/get-context-pages document-id start-page end-page)]
        (when (seq pages)
          (->> pages
               (map :pages/text)
               (str/join "\n\n---\n\n")))))))

;; OpenAI API calls
(defn parse-edn-response
  "Parse OpenAI response as EDN. Handles markdown code fences.
   Returns parsed EDN or throws exception."
  [raw-text]
  (let [;; Strip markdown code fences if present
        cleaned (-> raw-text
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

(defn generate-basic-cards
  "Generate basic Q&A flashcards using OpenAI API.
   Options:
   - :content - The main text to generate cards from (required)
   - :context - Optional context text from previous pages
   - :card-count - Number of cards to generate (default from settings)
   - :model - OpenAI model to use (default from settings)
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [{:keys [content context card-count model]}]
  (try
    (let [api-key (settings/get-openai-api-key)
          _ (when (empty? api-key)
              (throw (ex-info "OpenAI API key not configured" {})))
          _ (when (empty? content)
              (throw (ex-info "No content provided" {})))
          card-count (or card-count (settings/get-card-count))
          model (or model (settings/get-model))
          has-context? (not (empty? context))
          prompt (build-basic-prompt card-count has-context?)
          _ (when-not prompt
              (throw (ex-info "Failed to load prompt templates" {})))
          ;; Build content message
          content-text (if has-context?
                         (pr-str {:content content :context context})
                         content)
          response (api/create-chat-completion
                     {:model model
                      :messages [{:role "system" :content prompt}
                                 {:role "user" :content content-text}]}
                     {:api-key api-key})
          raw-text (-> response :choices first :message :content)
          cards (parse-edn-response raw-text)]
      {:success true :cards cards})
    (catch Exception e
      (println "ERROR [generate-basic-cards]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn generate-cloze-cards
  "Generate cloze deletion flashcards using OpenAI API.
   Options:
   - :content - The main text to generate cards from (required)
   - :context - Optional context text from previous pages
   - :card-count - Number of cards to generate (default from settings)
   - :model - OpenAI model to use (default from settings)
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [{:keys [content context card-count model]}]
  (try
    (let [api-key (settings/get-openai-api-key)
          _ (when (empty? api-key)
              (throw (ex-info "OpenAI API key not configured" {})))
          _ (when (empty? content)
              (throw (ex-info "No content provided" {})))
          card-count (or card-count (settings/get-card-count))
          model (or model (settings/get-model))
          has-context? (not (empty? context))
          prompt (build-cloze-prompt card-count has-context?)
          _ (when-not prompt
              (throw (ex-info "Failed to load prompt templates" {})))
          ;; Build content message
          content-text (if has-context?
                         (pr-str {:content content :context context})
                         content)
          response (api/create-chat-completion
                     {:model model
                      :messages [{:role "system" :content prompt}
                                 {:role "user" :content content-text}]}
                     {:api-key api-key})
          raw-text (-> response :choices first :message :content)
          cards (parse-edn-response raw-text)]
      {:success true :cards cards})
    (catch Exception e
      (println "ERROR [generate-cloze-cards]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

;; Card persistence
(defn save-cards
  "Save generated cards to the database.
   For basic cards: expects [{:q \"...\" :a \"...\"}]
   For cloze cards: expects [{:c \"...\"}]"
  [document-id page-number kind cards]
  (try
    (let [rows (map (fn [card]
                      (if (= kind "basic")
                        {:document_id document-id
                         :page_number page-number
                         :kind kind
                         :question (:q card)
                         :answer (:a card)
                         :cloze nil}
                        {:document_id document-id
                         :page_number page-number
                         :kind kind
                         :question nil
                         :answer nil
                         :cloze (:c card)}))
                    cards)]
      (db/insert-flashcards rows)
      {:success true})
    (catch Exception e
      (println "ERROR [save-cards]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn get-cards
  "Get all flashcards for a specific document page."
  [document-id page-number]
  (try
    (let [cards (db/get-flashcards document-id page-number)]
      {:success true :cards cards})
    (catch Exception e
      (println "ERROR [get-cards]:" (.getMessage e))
      {:success false :error (.getMessage e)})))

(defn delete-card
  "Delete a single flashcard by ID."
  [card-id]
  (try
    (db/delete-flashcard card-id)
    {:success true}
    (catch Exception e
      (println "ERROR [delete-card]:" (.getMessage e))
      {:success false :error (.getMessage e)})))
