(ns freememo.cards
  "Flashcard generation via OpenRouter with prompt templates."
  (:require
   [freememo.db :as db]
   [freememo.settings :as settings]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [freememo.openrouter :as openrouter]
   [freememo.card-models :as card-models]
   [freememo.llm-edn :as llm-edn]
   [freememo.overlapping :as overlap]
   [freememo.logging :as log]
   [taoensso.telemere :as tel]
   [clojure.java.io :as io]
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

    (re-find #"(?i)parse.*model response" (str msg))
    "The AI returned an unreadable response. Please try again."

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
(def ^:private prompt-configs
  "Per-card-type knobs for build-prompt: the body template file, the optional
   context template file (nil = no context support), and the two nouns that
   vary in the trailing count-instruction sentence."
  {:basic {:template "basic.md" :context-template "context.md"
           :item-noun "flashcards" :collection-noun "items"}
   :cloze {:template "cloze.md" :context-template "context-cloze.md"
           :item-noun "flashcards" :collection-noun "items"}
   :overlapping {:template "overlapping.md" :context-template "context.md"
                 :item-noun "overlapping-cloze cards" :collection-noun "maps"}})

(defn- build-prompt
  "Build a card-generation prompt per `config` (a prompt-configs entry).
   Concatenates effective system prompt (global + nearest per-item prompt for
   topic-id) + optional pre-prompt + the config's template + optional context
   template."
  [{:keys [template context-template item-noun collection-noun]}
   card-count has-context? pre-prompt user-id topic-id]
  (let [system (settings/get-effective-system-prompt user-id topic-id)
        body (load-prompt-template template)
        context (when has-context? (load-prompt-template context-template))]
    (when (and system body)
      (str system
        (when (and pre-prompt (not (str/blank? pre-prompt)))
          (str "\n\n## Custom Question Format\n\n"
            "Use the following pattern for your questions:\n\n"
            pre-prompt
            "\n\n"))
        body
        (when context (str "\n\n" context))
        "\n\n# Instructions\n\n"
        "Generate EXACTLY " card-count " " item-noun " from the provided text. "
        "Return EXACTLY " card-count " " collection-noun " in the EDN vector — no more, no fewer. "
        "Do not exceed this count even if the content seems to warrant more cards."))))

(defn build-basic-prompt
  "Build prompt for basic Q&A cards.
   Concatenates effective system prompt (global + nearest per-item prompt for
   topic-id) + optional pre-prompt + basic.md + optional context.md."
  [card-count has-context? pre-prompt user-id topic-id]
  (build-prompt (:basic prompt-configs) card-count has-context? pre-prompt user-id topic-id))

(defn build-cloze-prompt
  "Build prompt for cloze deletion cards.
   Concatenates effective system prompt (global + nearest per-item prompt for
   topic-id) + optional pre-prompt + cloze.md + optional context-cloze.md."
  [card-count has-context? pre-prompt user-id topic-id]
  (build-prompt (:cloze prompt-configs) card-count has-context? pre-prompt user-id topic-id))

(defn build-overlapping-prompt
  "Build prompt for overlapping-cloze cards.
   Concatenates effective system prompt + optional pre-prompt + overlapping.md.
   card-count is the number of LISTS to produce (each becomes one note that
   Anki fans out into N+1 cards)."
  [card-count has-context? pre-prompt user-id topic-id]
  (build-prompt (:overlapping prompt-configs) card-count has-context? pre-prompt user-id topic-id))

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

;; Model response parsing
(defn parse-card-response
  "Parse the model's card response into a collection of maps — delegates to
   freememo.llm-edn/parse-response (EDN preferred, JSON fallback, fences
   stripped; throws ex-info on unparseable input)."
  [raw-text]
  (llm-edn/parse-response raw-text))

(defn- generate-cards*
  "Shared implementation for card generation with up to `max-retries` attempts.
   Validates that the returned card count matches the requested count; on success
   bills the summed OpenRouter usage.cost of all attempts (no charge on final
   failure). endpoint-tag is :cards.basic or :cards.cloze for the ledger.
   Pre:  opts has non-empty :content; :model (or the user's saved model) resolves
         to a card-models entry; the OpenRouter key is configured.
   Post: {:success true :cards v} with (count v) = card-count, else
         {:success false :error s}."
  [{:keys [content context card-count model user-id pre-prompt topic-id goal]} prompt-builder-fn endpoint-tag]
  (let [api-key (settings/get-openrouter-api-key user-id)
        _ (when (empty? api-key) (throw (ex-info "OpenRouter API key not configured" {})))
        _ (when (empty? content) (throw (ex-info "No content provided" {})))
        card-count (or card-count (settings/get-card-count user-id))
        model-id (or model (settings/get-model user-id))
        entry (or (card-models/resolve-model model-id)
                  (throw (ex-info (str "Unknown card model: " model-id) {})))
        slug (:openrouter-model entry)
        _ (let [gate (credits/check-cost-billed-balance! user-id)]
            (when-not (:ok gate)
              (throw (ex-info (:error gate) {:type ::insufficient-credits}))))
        max-retries (settings/get-card-gen-max-retries user-id)
        reasoning (settings/get-reasoning user-id)
        verbosity (settings/get-verbosity user-id)
        has-context? (not (empty? context))
        prompt (prompt-builder-fn card-count has-context? pre-prompt user-id topic-id)
        _ (when-not prompt (throw (ex-info "Failed to load prompt templates" {})))
        ;; Document-scoped learning goal (per root-topic), appended so generated
        ;; cards serve why the learner is studying. Absent/blank ⇒ prompt unchanged.
        prompt (cond-> prompt
                 (and goal (not (str/blank? goal)))
                 (str "\n\n# Learning goal\n\n"
                   "The learner is studying this material to: " goal "\n"
                   "Favor cards that serve this goal."))
        content-text (if has-context? (pr-str {:content content :context context}) content)]
    (loop [attempt 1
           cost-acc 0.0]
      (let [t-start (System/nanoTime)
            body (openrouter/chat-completion! api-key
                   {:model slug
                    :messages [{:role "system" :content prompt}
                               {:role "user" :content content-text}]
                    :reasoning_effort reasoning
                    :verbosity verbosity})
            duration-ms (long (/ (- (System/nanoTime) t-start) 1000000))
            usage (:usage body)
            cost-usd (:cost usage)
            raw-text (-> body :choices first :message :content)
            _ (tel/log! {:level :info :id ::cards-completion
                         :data {:user-id user-id
                                :model (:id entry)
                                :openrouter-model slug
                                :endpoint endpoint-tag
                                :prompt-tokens (:prompt_tokens usage)
                                :completion-tokens (:completion_tokens usage)
                                :cost-usd cost-usd
                                :duration-ms duration-ms
                                :attempt attempt}}
                "Card generation completion")
            cards (parse-card-response raw-text)
            actual-count (count cards)
            cost-acc' (+ cost-acc (double (or cost-usd 0)))]
        (cond
          (= actual-count card-count)
          ;; Success — bill the summed cost of all attempts (§5.4.5).
          ;; record-cost-charge! is total: a billing failure logs
          ;; ::credit-charge-failed and returns nil, never discarding cards.
          ;; Its return is the charged IQD — reflecting the summed retry cost,
          ;; nil when credits are disabled or the debit failed — surfaced as
          ;; :cost-credits for the compare UI (the normal gen path ignores it).
          (let [charged (credits/record-cost-charge! user-id endpoint-tag (:id entry) cost-acc')]
            {:success true :cards cards :cost-credits charged})

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
            (recur (inc attempt) cost-acc')))))))

(defn- generate-cards-wrapped
  "Shared try/catch wrapper around generate-cards* for one card type.
   trace-id names the tel/trace! span; error-id names the tel/error! log on an
   unrecognized failure (kept distinct per call site for log/trace filtering).
   Out-of-credits refusal is a normal business outcome, not a pipeline
   failure — keep it off the :error channel (alert email noise)."
  [opts prompt-builder-fn endpoint-tag trace-id error-id]
  (try
    (tel/trace! {:id trace-id}
      (generate-cards* opts prompt-builder-fn endpoint-tag))
    (catch Exception e
      (if (= :insufficient-credits (error-type e))
        (tel/log! {:level :info :id ::spend-refused :data {:user-id (:user-id opts)}}
          "Card generation refused: out of credits")
        (tel/error! {:id error-id} e))
      {:success false
       :error (humanize-error (.getMessage (root-cause e)))
       :error-type (error-type e)})))

(defn generate-basic-cards
  "Generate basic Q&A flashcards via OpenRouter.
   Options:
   - :content - The main text to generate cards from (required)
   - :context - Optional context text from previous pages
   - :card-count - Number of cards to generate (default from settings)
   - :model - card-models id to use (default from settings/get-model)
   - :pre-prompt - Optional custom formatting instructions
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [opts]
  (generate-cards-wrapped opts build-basic-prompt :cards.basic
    ::generate-basic ::generate-basic-cards))

(defn generate-cloze-cards
  "Generate cloze deletion flashcards via OpenRouter.
   Options same as generate-basic-cards.
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}"
  [opts]
  (generate-cards-wrapped opts build-cloze-prompt :cards.cloze
    ::generate-cloze ::generate-cloze-cards))

(defn generate-overlapping-cards
  "Generate overlapping-cloze cards via OpenRouter.
   Each returned map is {:question s :items [s ...]}. Options as generate-basic-cards.
   Returns {:success true :cards [...]} or {:success false :error \"msg\"}."
  [opts]
  (generate-cards-wrapped opts build-overlapping-prompt :cards.overlapping
    ::generate-overlapping ::generate-overlapping-cards))

;; Card persistence

(defn effective-pins-for-bake
  "Return pin rows for topic-id ordered by ord ASC.
   Under EC-snapshot semantics this is a direct local lookup — no ancestor walk."
  [topic-id]
  (db/get-pins topic-id))

(defn- pin-img-tag
  "Markup for one pinned image. Shared by bake-card-html and pins-prefill-html
   so baked and Add-Card-prefilled cards produce byte-identical HTML."
  [media-id]
  (str "<p><img src=\"/api/media/" media-id "\"></p>"))

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
      (if (= kind "basic")
        (let [front-imgs (->> pins
                           (filter #(= "front" (:topic_pins/placement %)))
                           (map #(pin-img-tag (:topic_pins/media_id %)))
                           (apply str))
              back-imgs (->> pins
                          (filter #(= "back" (:topic_pins/placement %)))
                          (map #(pin-img-tag (:topic_pins/media_id %)))
                          (apply str))]
          (cond-> fields
            (not (str/blank? front-imgs)) (update :q str front-imgs)
            (not (str/blank? back-imgs)) (update :a str back-imgs)))
        ;; cloze — F2-uniform: all pins appended to :c regardless of placement
        (let [all-imgs (->> pins
                         (map #(pin-img-tag (:topic_pins/media_id %)))
                         (apply str))]
          (cond-> fields
            (not (str/blank? all-imgs)) (update :c str all-imgs)))))))

(defn pins-prefill-html
  "Front/back pinned-image HTML for prefilling the manual Add-Card editors.
   front pin → primary editor (Question/Cloze), back pin → answer editor
   (Answer/Back-Extra). Reuses pin-img-tag so a prefilled card is byte-identical
   to a baked one. Returns {:front html :back html}; each concatenates that
   placement's tags in ord order, or \"\" when none.

   Contract: callers that prefill with this MUST save via
   (save-cards … bake? false) so the images are not appended a second time."
  [topic-id]
  (let [pins (effective-pins-for-bake topic-id)
        for-placement (fn [placement]
                        (->> pins
                          (filter #(= placement (:topic_pins/placement %)))
                          (map #(pin-img-tag (:topic_pins/media_id %)))
                          (apply str)))]
    {:front (for-placement "front")
     :back (for-placement "back")}))

(defn save-cards
  "Save generated cards to the database.
   For basic cards: expects [{:q \"...\" :a \"...\"}]
   For cloze cards: expects [{:c \"...\" :a html-or-nil}]
   user-id: owner, for the audit log (§3.1).
   topic-id: the page or extract topic that owns these cards.
   root-topic-id: the root PDF/EPUB/web/basic topic.
   Bakes pinned images into each card's HTML before insert.
   Returns {:success true :ids [id...]} — the ids of the newly inserted cards,
   in order (may be shorter than `cards` if ON CONFLICT skipped duplicates) —
   or {:success false :error msg} on failure.

   bake? false skips bake-card-html — used by the manual Add-Card path, which
   prefills pinned images into the editor content itself (pins-prefill-html), so
   baking would duplicate them."
  ([user-id topic-id root-topic-id kind cards]
   (save-cards user-id topic-id root-topic-id kind cards true))
  ([user-id topic-id root-topic-id kind cards bake?]
   (try
     (let [rows (map (fn [card]
                       (if (= kind "overlapping")
                         (let [items (vec (:items card))
                               settings (merge overlap/default-settings (:settings card))]
                           {:topic_id topic-id
                            :root_topic_id root-topic-id
                            :kind kind
                            :question nil
                            :answer nil
                            :cloze nil
                            :overlapping {:question (:question card)
                                          :items items
                                          :settings settings
                                          :fields (overlap/expand items settings)}})
                         (let [baked (if bake? (bake-card-html topic-id kind card) card)]
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
                              :cloze (:c baked)}))))
                  cards)
             ids (vec (db/insert-flashcards! rows))]
       (log/audit! {:id ::save-cards :user-id user-id :action :create
                    :entity :card :n (count ids)})
       (tel/log! {:level :info :id ::save-cards-detail
                  :data {:user-id user-id :topic-id topic-id :kind kind
                         :ids ids :n (count ids) :card-hash (hash cards)}}
         "save-cards detail")
       {:success true :ids ids})
     (catch Exception e
       (tel/error! {:id ::save-cards} e)
       {:success false :error (.getMessage e)}))))

;; Compare-models: generate the same content with several models, unsaved, for
;; the compare modal (client-held staging — nothing persists until the user picks).

(defn compare-card-gen
  "Run one card generation per model-id on the same `opts` content, WITHOUT
   saving. Each model is a real billed generation — the credit gate + charge
   happen inside generate-cards*. Fans out concurrently (the model subset is
   small) with a per-model timeout so one slow/hung model can't stall the batch.
   Pre:  opts has :content/:card-type/:card-count/:user-id (…); model-ids non-empty.
   Post: [{:model-id id :result {:success bool :cards [...] :cost-credits long-or-nil
         :error str}}] — :cost-credits rides through unchanged from generate-cards*;
         nothing written to the flashcards table."
  [opts model-ids]
  (let [gen-one (fn [mid]
                  (let [o (assoc opts :model mid)]
                    (case (:card-type opts)
                      "cloze" (generate-cloze-cards o)
                      "overlapping" (generate-overlapping-cards o)
                      (generate-basic-cards o))))
        futs (mapv (fn [mid] [mid (future (gen-one mid))]) model-ids)]
    (mapv (fn [[mid f]]
            {:model-id mid
             :result (deref f 60000 {:success false :error "Timed out"})})
      futs)))

(defn commit-card-set!
  "Persist one model's generated set (the chosen compare candidate) via the same
   save path as normal generation, then bump the card table. Returns the save
   result {:success :ids}.
   Pre:  cards is a non-empty seq of generated card maps.
   Post: rows inserted; :card-mutations bumped (via :generate) on success."
  [user-id topic-id root-topic-id card-type cards]
  (let [r (save-cards user-id topic-id root-topic-id card-type cards)]
    (when (:success r)
      (commands/bump! user-id :generate))
    r))

(defn add-card
  "Manually add a single flashcard.
   user-id: owner, for the audit log (logged by save-cards).
   topic-id: the page or extract topic.
   root-topic-id: the root topic."
  [user-id topic-id root-topic-id kind fields]
  (try
    (let [cards (case kind
                  "basic" [{:q (:question fields) :a (:answer fields)}]
                  "overlapping" [{:question (:question fields)
                                  :items (:items fields)
                                  :settings (:settings fields)}]
                  [{:c (:cloze fields)}])]
      (save-cards user-id topic-id root-topic-id kind cards))
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

(defn- cleanup-occlusion-mask!
  "After an occlusion flashcard row is deleted: retire its rect, dirty the
   sibling rows (their masks must regenerate without it on the next push),
   and drop the group once empty. No-op for other kinds."
  [deleted-row]
  (when (= "occlusion" (:flashcards/kind deleted-row))
    (db/remove-occlusion-mask!
      (:flashcards/occlusion_group_id deleted-row)
      (:flashcards/mask_ordinal deleted-row))))

(defn- cleanup-score-group!
  "After a score flashcard row is deleted: drop the group once no direction
   rows remain (its clip/crop media rows stay — orphan-tolerated). Deleting
   one direction keeps the other card intact. No-op for other kinds."
  [deleted-row]
  (when (= "score" (:flashcards/kind deleted-row))
    (db/remove-score-card-cleanup!
      (:flashcards/score_group_id deleted-row))))

(defn delete-card
  "Delete a single flashcard, snapshotting it for undo and pushing an
   Undo toast. Returns {:success true :anki-note-id N :undo-id L}; :undo-id
   is the undo_log entry id (nil if the card did not exist)."
  [user-id card-id]
  (try
    (let [deleted (db/delete-flashcard! user-id card-id)
          _ (when deleted
              (cleanup-occlusion-mask! deleted)
              (cleanup-score-group! deleted))
          note-id (:flashcards/anki_note_id deleted)
          occlusion? (= "occlusion" (:flashcards/kind deleted))
          ;; No undo for occlusion masks (rect retired from the group geometry
          ;; on delete) or score cards (the group row may be dropped with the
          ;; last direction) — restoring the row alone would dangle.
          score? (= "score" (:flashcards/kind deleted))
          undo-id (when (and deleted (not occlusion?) (not score?))
                    (db/insert-undo-entry! user-id "delete-card" "flashcard"
                      [card-id] [deleted]))]
      (cond
        undo-id
        (toasts/push! user-id {:level :success
                               :message "Card deleted"
                               :dedup? false
                               :actions [{:label "Undo" :undo-id undo-id}]})
        (and deleted occlusion?)
        (toasts/push! user-id {:level :success
                               :message "Occlusion mask deleted"
                               :dedup? false}))
      (when deleted
        (log/audit! {:id ::delete-card :user-id user-id :action :delete
                     :entity :card :entity-id card-id}))
      {:success true :anki-note-id note-id :undo-id undo-id})
    (catch Exception e
      (tel/error! {:id ::delete-card} e)
      {:success false :error (.getMessage e)})))

(defn delete-cards!
  "Bulk delete of user-id's cards (Library cards view selection).
   pre:  card-ids from the user's selection; ownership re-checked in SQL.
   post: {:success true :deleted n :anki-note-ids [...]} — note ids of
   deleted cards that were pushed, for the client's fire-and-forget Anki
   note deletion. Bumps :card-mutations once iff anything was deleted."
  [user-id card-ids]
  (try
    (let [deleted (db/delete-user-flashcards! user-id card-ids)
          note-ids (into [] (keep :flashcards/anki_note_id) deleted)]
      (doseq [row deleted]
        (cleanup-occlusion-mask! row))
      (when (seq deleted)
        (commands/bump! user-id :bulk-delete-cards)
        (log/audit! {:id ::delete-cards! :user-id user-id :action :delete
                     :entity :card :n (count deleted)})
        ;; Occlusion rows are excluded from the undo snapshot — their rects
        ;; are retired from the group geometry above, so a row-only restore
        ;; would be inconsistent (and the group itself may be gone).
        (let [n (count deleted)
              undoable (into [] (remove #(= "occlusion" (:flashcards/kind %))) deleted)
              undo-id (when (seq undoable)
                        (db/insert-undo-entry! user-id "bulk-delete-cards" "flashcard"
                          (mapv :flashcards/id undoable) undoable))]
          (toasts/push! user-id
            (cond-> {:level :success
                     :message (str "Deleted " n " card" (when (not= 1 n) "s"))
                     :dedup? false}
              undo-id (assoc :actions [{:label "Undo" :undo-id undo-id}])))))
      {:success true :deleted (count deleted) :anki-note-ids note-ids})
    (catch Exception e
      (tel/error! {:id ::delete-cards!} e)
      {:success false :error (.getMessage e)})))

(defn update-card
  "Update a flashcard with validation. Returns {:success bool :error string}.
   user-id: owner, for the audit log (§3.1)."
  [user-id card-id updated-fields]
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

    (db/update-flashcard! user-id card-id updated-fields)
    (log/audit! {:id ::update-card :user-id user-id :action :update
                 :entity :card :entity-id card-id})
    {:success true}

    (catch Exception e
      (tel/error! {:id ::update-card} e)
      {:success false :error (.getMessage e)})))

(defn update-overlapping-card
  "Re-derive an overlapping card's Anki fields from an edited list + settings
   and persist the whole overlapping JSONB.
   Pre:  items non-empty and ≤ overlapping/max-items (expand enforces both).
   Post: the row's :overlapping is {:question :items :settings :fields}, :fields
         consistent with :items/:settings; updated_at bumped for sync.
   Returns {:success bool :error string}.
   user-id: owner, for the audit log (§3.1)."
  [user-id card-id {:keys [question items settings]}]
  (try
    (let [items (vec items)
          settings (merge overlap/default-settings settings)
          ol {:question question :items items :settings settings
              :fields (overlap/expand items settings)}]
      (db/update-flashcard! user-id card-id {:overlapping ol})
      (log/audit! {:id ::update-overlapping-card :user-id user-id :action :update
                   :entity :card :entity-id card-id})
      {:success true})
    (catch Exception e
      (tel/error! {:id ::update-overlapping-card} e)
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
  [{:keys [user-id root-topic-id topic-id kind header-text]}]
  (try
    (let [root (db/get-topic-for-user user-id root-topic-id)
          _ (when-not root
              (throw (ex-info "Topic not found" {:root-topic-id root-topic-id})))
          _ (when (and topic-id (not (db/owns-topic? user-id topic-id)))
              (throw (ex-info "Topic not found" {:topic-id topic-id})))
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
