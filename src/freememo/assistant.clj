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
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-evidence-excerpt :as evidence-excerpt]
   [freememo.assistant-modes :as modes]
   [freememo.card-models :as card-models]
   [freememo.cards :as cards]
   [freememo.cards-from-facts :as cff]
   [freememo.config :as config]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.html-cleaner :as cleaner]
   [freememo.markdown :as markdown]
   [freememo.text :as text]
   [freememo.toasts :as toasts]
   [freememo.db :as db]
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

(declare start-compact-run!)

;; ── Context resolution ─────────────────────────────────────────────────────

(def ^:private max-context-facts
  "Upper bound on KG facts injected into one send's grounding — keeps a large
   document (or code repo) from overflowing the prompt. Nearest facts win."
  50)

(def ^:private max-window-chars
  "Upper bound on the combined characters of the PDF page window in one send.
   Farthest pages are dropped first; the current page is never dropped."
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

(def ^:private evidence-token-pattern
  #"\[evidence:([A-Za-z0-9_-]+)\]")

(def ^:private evidence-system-instructions
  (str "\n\n# Evidence citations\n\n"
    "Cite supplied sources only with their exact token, such as [evidence:E1]. "
    "Do not invent evidence keys. Use the narrowest source block that contains the claim. "
    "Place each citation after the claim it supports."))

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

(defn- content-evidence
  "Build block-level evidence for one retained HTML source."
  [content {:keys [document-topic-id page-topic-id page-number document-title
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
       :snapshot-content (or (:html block) (:text block))})
    (range)
    (evidence-excerpt/evidence-blocks content)))

(defn- evidence-message-content [sources]
  (->> sources
    (map (fn [source]
           (str "Source " (:citation-key source) " — " (:document-title source)
             (when-let [page (:page-number source)] (str ", page " page))
             " — " (:display-label source) "\n"
             (:snapshot-content source))))
    (str/join "\n\n")))

(defn- pdf-context-packet
  "Build block-level evidence and one rendered source turn for the current PDF.
   Pre: ctx identifies a PDF page. Post: {:messages :evidence :next-index}.
   Every source block has one evidence key. The page budget matches the legacy
   reading window."
  [ctx root-title first-index]
  (let [center (:center-page ctx)
        window (:window ctx)
        pages (if center
                (db/get-context-pages (:root-id ctx) (max 1 (- center window)) (+ center window))
                [])
        candidates (->> pages
                     (keep (fn [page]
                             (when-let [text (some-> (:topics/content page) str/trim not-empty)]
                               {:topic page :page (:topics/page_number page) :text text})))
                     (sort-by #(Math/abs (- (int (:page %)) (int center)))))
        kept (->> candidates
               (reduce (fn [{:keys [pages total]} page]
                         (let [length (count (:text page))]
                           (if (or (empty? pages) (<= (+ total length) max-window-chars))
                             {:pages (conj pages page) :total (+ total length)}
                             {:pages pages :total total})))
                 {:pages [] :total 0})
               :pages
               (sort-by :page)
               vec)
        evidence (loop [[{:keys [topic page text]} & remaining] kept
                        next-index first-index
                        result []]
                   (if-not topic
                     result
                     (let [sources (content-evidence text
                                     {:document-topic-id (:root-id ctx)
                                      :page-topic-id (:topics/id topic)
                                      :page-number page
                                      :document-title root-title
                                      :source-revision (source-revision topic)}
                                     next-index)]
                       (recur remaining (+ next-index (count sources))
                         (into result sources)))))
        content (evidence-message-content evidence)]
    {:messages (if (str/blank? content) [] [{:role "user" :content content}])
     :evidence evidence
     :next-index (+ first-index (count evidence))}))

(defn- topic-context-packet
  "Build block-level sources for the current non-PDF topic."
  [ctx root-title first-index]
  (if-let [topic (some-> (:page-topic-id ctx) db/get-topic)]
    (if-let [content (some-> (:topics/content topic) str/trim not-empty)]
      (let [sources (content-evidence content
                      {:document-topic-id (:root-id ctx)
                       :page-topic-id (:topics/id topic)
                       :page-number (:topics/page_number topic)
                       :document-title root-title
                       :source-revision (source-revision topic)}
                      first-index)
            rendered (evidence-message-content sources)]
        {:messages (if (str/blank? rendered) [] [{:role "user" :content rendered}])
         :evidence sources :next-index (+ first-index (count sources))})
      {:messages [] :evidence [] :next-index first-index})
    {:messages [] :evidence [] :next-index first-index}))

(defn- reference-context-packet
  "Build retained block-level sources for owned @ references."
  [user-id ref-topic-ids first-index]
  (reduce
    (fn [{:keys [messages evidence next-index]} topic-id]
      (if-let [topic (db/get-topic-for-user user-id topic-id)]
        (if-let [content (some-> (:topics/content topic) str/trim not-empty)]
          (let [title (or (:topics/title topic) "Untitled")
                sources (content-evidence content
                          {:document-topic-id topic-id
                           :page-topic-id topic-id
                           :page-number (:topics/page_number topic)
                           :document-title title
                           :source-revision (source-revision topic)}
                          next-index)
                rendered (evidence-message-content sources)]
            (if (str/blank? rendered)
              {:messages messages :evidence evidence :next-index next-index}
              {:messages (conj messages {:role "user" :content rendered})
               :evidence (into evidence sources)
               :next-index (+ next-index (count sources))}))
          {:messages messages :evidence evidence :next-index next-index})
        {:messages messages :evidence evidence :next-index next-index}))
    {:messages [] :evidence [] :next-index first-index}
    (or ref-topic-ids [])))

(defn- run-source-packet
  "Resolve current reading sources, @ references, and grounded fact evidence.
   Post: every source shown to the model has one retained evidence record."
  [user-id ctx mode ref-topic-ids]
  (let [root (db/get-topic (:root-id ctx))
        root-title (or (:topics/title root) "Untitled")
        current (if (:pdf? ctx)
                  (pdf-context-packet ctx root-title 1)
                  (topic-context-packet ctx root-title 1))
        refs (reference-context-packet user-id ref-topic-ids (:next-index current))
        grounding (grounding-system-suffix user-id ctx mode)
        facts-source (when-not (str/blank? grounding)
                       {:citation-key (evidence-key (:next-index refs))
                        :document-topic-id (:root-id ctx)
                        :page-topic-id (:page-topic-id ctx)
                        :page-number (:center-page ctx)
                        :document-title root-title
                        :display-label "document-level evidence"
                        :source-revision (source-revision root)
                        :quoted-text (evidence-excerpt grounding)
                        :snapshot-content grounding})]
    {:messages (into (:messages current) (:messages refs))
     :evidence (cond-> (into (:evidence current) (:evidence refs))
                 facts-source (conj facts-source))
     :grounding-suffix (when grounding
                         (str grounding "\n\nEvidence key for these established facts: "
                           "[evidence:" (:citation-key facts-source) "]."))}))

(defn- transcript-messages-after-checkpoint
  "Provider transcript after the newest checkpoint's covered range.
   Blank assistant placeholders never enter a request."
  [chat-id checkpoint]
  (into []
    (comp
      (filter #(> (:assistant_messages/id %) (or (:covered_through_message_id checkpoint) 0)))
      (filter #(or (= "user" (:assistant_messages/role %))
                 (not (str/blank? (:assistant_messages/content %)))))
      (map (fn [message]
             {:role (:assistant_messages/role message)
              :content (:assistant_messages/content message)})))
    (db/get-assistant-messages chat-id)))

(defn- answer-request
  "Render the exact provider request for one new answer.
   Stable instructions and the latest checkpoint form the request prefix.
   Later transcript, current sources, and the new user turn follow it."
  [user-id chat page-topic-id user-text ref-topic-ids model-fields]
  (let [chat-id (:assistant_chats/id chat)
        root-topic-id (:assistant_chats/root_topic_id chat)
        mode (modes/resolve-mode (:assistant_chats/mode chat))
        ctx (resolve-context user-id root-topic-id page-topic-id)
        sources (run-source-packet user-id ctx mode ref-topic-ids)
        checkpoint (db/latest-assistant-checkpoint user-id chat-id)
        system-content (str (mode-system-prompt (:id mode))
                         (mission-suffix user-id root-topic-id)
                         evidence-system-instructions)
        prefix (cond-> [{:role "system" :content system-content}]
                 checkpoint (conj {:role "system"
                                    :content (str "# Conversation checkpoint\n\n"
                                               (:summary checkpoint))}))
        later-transcript (transcript-messages-after-checkpoint chat-id checkpoint)
        dynamic-grounding (when-let [grounding (:grounding-suffix sources)]
                            [{:role "system" :content grounding}])
        messages (into (into (into (into prefix later-transcript)
                                dynamic-grounding)
                         (:messages sources))
                   [{:role "user" :content user-text}])]
    {:provider-request (assoc model-fields
                         :stream true
                         :session_id (:assistant_chats/provider_session_id chat)
                         :messages messages)
     :source-snapshot (:evidence sources)
     :evidence (:evidence sources)}))

(defn citation-target
  "Relative viewer target for one retained evidence record."
  [source]
  (evidence/citation-target source))

(defn- markdown-link-label [label]
  (-> (or label "document-level evidence")
    (str/replace "\\" "\\\\")
    (str/replace "[" "\\[")
    (str/replace "]" "\\]")))

(defn render-evidence-citations
  "Replace valid evidence tokens with Markdown links and remove invalid tokens.
   Pre: evidence belongs to the answer run. Post: every rendered citation has a
   retained target. The assistant panel opens retained evidence in a dialog."
  [answer evidence]
  (let [by-key (into {} (map (juxt :citation_key identity)) evidence)]
    (str/replace (or answer "") evidence-token-pattern
      (fn [[_ citation-key]]
        (if-let [source (get by-key citation-key)]
          (if-let [target (citation-target source)]
            (str "[" (markdown-link-label (:display_label source)) "](" target ")")
            "")
          "")))))

;; ── Commands ───────────────────────────────────────────────────────────────

(defn start-chat!
  "Create a chat for (user-id, root-topic-id) in `mode-id`. Returns the new chat
   id and bumps :assistant-mutations. The page context is resolved for each run.
   Pre: root-topic-id belongs to user-id. A violation throws because the caller
   supplied a foreign document. `mode-id` can be nil or unrecognized.
   Post: the row's mode is a canonical registry :id, never nil and never junk."
  [user-id root-topic-id mode-id]
  (when-not (db/get-topic-for-user user-id root-topic-id)
    (throw (ex-info "Cannot create a chat for a document the user does not own."
             {:type ::foreign-chat-document :user-id user-id
              :root-topic-id root-topic-id})))
  (let [chat-id (db/create-assistant-chat! user-id root-topic-id "New chat"
                  (:id (modes/resolve-mode mode-id)))]
    (bump! user-id)
    chat-id))

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

(defonce ^:private assistant-run-executor
  (Executors/newFixedThreadPool 4))

(defn- model-entry
  "Resolve the effective assistant model for one document."
  [user-id root-topic-id]
  (let [model-id (settings/effective-assistant-model user-id root-topic-id)]
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
  "Record one provider-reported run charge in the independent billing ledger."
  [user-id endpoint model-id metadata]
  (when-some [cost (:cost-usd metadata)]
    (credits/record-cost-charge! user-id endpoint model-id (double cost))))

(defn- execute-provider-run!
  "Drain one provider stream and settle its durable answer or checkpoint.

   Pre: run is queued and provider-request is its immutable request snapshot.
   Post: the run is completed, stopped, or failed. Provider metadata can update
   every state. Billing uses the final known cost once in this worker.
   Invariant: on-delta writes through the database's active-state predicate.
   This worker never writes late response text after Stop."
  [user-id chat-id run-uuid run-kind api-key model-id provider-request
   compact-range]
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
            response (:text result)]
        (db/update-assistant-run-metadata! run-uuid metadata)
        (if (str/blank? response)
          (db/fail-assistant-run! run-uuid "The model returned an empty response.")
          (if (= "compact" run-kind)
            (db/complete-assistant-compact-run! run-uuid (str/trim response)
              (:from compact-range) (:through compact-range))
            (db/complete-assistant-answer-run! run-uuid)))
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
                            :response-chars (count (or (:response_text run) ""))
                            :publish-count @publish-count*}}
            "Assistant run reached a terminal state"))
        {:success true})
      (catch Throwable error
        (let [known @metadata*
              reconciled (or (poll-generation-metadata
                               api-key (:generation-id known) user-id) {})
              metadata (merge known reconciled)]
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
          {:success false :error (ex-message error)})))))

(defn- start-provider-run!
  "Submit one accepted run to the bounded assistant executor.
   Post: returns immediately. The worker owns every terminal transition."
  [user-id chat-id run-uuid run-kind api-key model-id provider-request compact-range]
  (tel/log! {:level :info :id ::assistant-run-started
             :data {:user-id user-id :chat-id chat-id :run-uuid run-uuid
                    :kind run-kind :model-id model-id}}
    "Assistant run started")
  ((m/via assistant-run-executor
     (execute-provider-run! user-id chat-id run-uuid run-kind api-key model-id
       provider-request compact-range))
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

(defn start-answer-run!
  "Create and start one streamed answer in an owned chat.

   Pre: user-text is non-blank. Post: {:ok true :run-uuid} after durable creation
   and worker submission, or {:error}. The call does not wait for the provider.
   Invariant: the database rejects a second active operation in the chat."
  ([user-id chat-id page-topic-id user-text ref-topic-ids client-id]
   (start-answer-run! user-id chat-id page-topic-id user-text ref-topic-ids
     client-id nil nil nil nil))
  ([user-id chat-id page-topic-id user-text ref-topic-ids client-id
    retry-of-run-uuid exact-request exact-evidence exact-model-id]
   (let [chat (db/get-assistant-chat user-id chat-id)
         api-key (settings/get-openrouter-api-key user-id)]
     (cond
       (nil? chat) {:error "Chat not found."}
       (str/blank? user-text) {:error "Message is empty."}
       (str/blank? api-key) {:error "OpenRouter API key not configured."}
       :else
       (let [gate (credits/check-cost-billed-balance! user-id)]
         (if-not (:ok gate)
           {:error (:error gate)}
           (let [entry (or (some-> exact-model-id card-models/resolve-model)
                         (model-entry user-id (:assistant_chats/root_topic_id chat)))
                 run-model-id (or exact-model-id (:id entry))
                 model-fields (openrouter/chat-completion-fields entry
                                {:reasoning_effort (settings/get-reasoning user-id)})
                 rendered (if exact-request
                            {:provider-request exact-request
                             :source-snapshot exact-evidence
                             :evidence exact-evidence}
                            (answer-request user-id chat page-topic-id user-text
                              ref-topic-ids model-fields))
                 run-uuid (str (random-uuid))
                 created (db/create-assistant-answer-run!
                           chat-id user-text client-id
                           {:run-uuid run-uuid
                            :retry-of-run-uuid retry-of-run-uuid
                            :provider-session-id
                            (:assistant_chats/provider_session_id chat)
                            :requested-model run-model-id
                            :request-snapshot (:provider-request rendered)
                            :source-snapshot (:source-snapshot rendered)
                            :evidence (:evidence rendered)})]
             (if-not (:created created)
               {:error "This chat already has an active operation."
                :active-run-uuid (:active-run-uuid created)}
               (do
                 (when (= "New chat" (:assistant_chats/title chat))
                   (db/set-assistant-chat-title! chat-id (title-from user-text)))
                 (bump! user-id)
                 (start-provider-run! user-id chat-id run-uuid "answer" api-key
                   run-model-id (:provider-request rendered) nil)
                 {:ok true :run-uuid run-uuid})))))))))

(defn ensure-and-start-answer-run!
  "Start an answer, creating the selected-mode chat when necessary."
  [user-id root-topic-id page-topic-id chat-id mode-id user-text ref-topic-ids client-id]
  (let [resolved-chat-id (or chat-id (start-chat! user-id root-topic-id mode-id))]
    (assoc (start-answer-run! user-id resolved-chat-id page-topic-id user-text
             ref-topic-ids client-id)
      :chat-id resolved-chat-id)))

(defn submit-chat-input!
  "Dispatch composer input at the assistant boundary.
   `/compact` is an application command and never becomes a user message."
  [user-id root-topic-id page-topic-id chat-id mode-id user-text ref-topic-ids client-id]
  (if (= "/compact" (str/trim user-text))
    (if chat-id
      (assoc (start-compact-run! user-id chat-id) :chat-id chat-id :command :compact)
      {:error "Start a chat before compacting context." :chat-id nil :command :compact})
    (assoc (ensure-and-start-answer-run! user-id root-topic-id page-topic-id
             chat-id mode-id user-text ref-topic-ids client-id)
      :command :answer)))

(defn start-compact-run!
  "Start `/compact` as a separate paid run without adding a user message.
   Post: a completed worker creates one checkpoint event. Original messages stay
   unchanged. Empty transcripts and concurrent operations return an error."
  [user-id chat-id]
  (let [chat (db/get-assistant-chat user-id chat-id)
        api-key (settings/get-openrouter-api-key user-id)]
    (cond
      (nil? chat) {:error "Chat not found."}
      (str/blank? api-key) {:error "OpenRouter API key not configured."}
      :else
      (let [gate (credits/check-cost-billed-balance! user-id)
            messages (db/get-assistant-messages chat-id)]
        (cond
          (not (:ok gate)) {:error (:error gate)}
          (empty? messages) {:error "There is no chat context to compact."}
          :else
          (let [entry (model-entry user-id (:assistant_chats/root_topic_id chat))
                model-fields (openrouter/chat-completion-fields entry
                               {:reasoning_effort (settings/get-reasoning user-id)})
                checkpoint (db/latest-assistant-checkpoint user-id chat-id)
                later (transcript-messages-after-checkpoint chat-id checkpoint)
                summary-input (cond-> []
                                checkpoint
                                (conj {:role "system"
                                       :content (str "Existing conversation checkpoint:\n\n"
                                                  (:summary checkpoint))})
                                true (into later))
                provider-request (assoc model-fields
                                   :stream true
                                   :session_id (:assistant_chats/provider_session_id chat)
                                   :messages
                                   (into [{:role "system"
                                           :content (str "Summarize this conversation for future model turns. "
                                                      "Preserve decisions, definitions, unresolved questions, "
                                                      "and source references. Return only the summary.")}]
                                     summary-input))
                run-uuid (str (random-uuid))
                covered {:from (:assistant_messages/id (first messages))
                         :through (:assistant_messages/id (last messages))}
                created (db/create-assistant-compact-run!
                          chat-id {:run-uuid run-uuid
                                   :provider-session-id
                                   (:assistant_chats/provider_session_id chat)
                                   :requested-model (:id entry)
                                   :request-snapshot provider-request
                                   :source-snapshot []})]
            (if-not (:created created)
              {:error "This chat already has an active operation."
               :active-run-uuid (:active-run-uuid created)}
              (do
                (bump! user-id)
                (start-provider-run! user-id chat-id run-uuid "compact" api-key
                  (:id entry) provider-request covered)
                {:ok true :run-uuid run-uuid}))))))))

(defn stop-active-run!
  "Stop local delivery for the active run in an owned chat.
   The provider reader keeps draining for usage and cost metadata."
  [user-id chat-id]
  (let [result (db/stop-assistant-run! user-id chat-id)]
    (when (:stopped result) (bump! user-id))
    result))

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
   :source-revision (:source_revision source)})

(defn retry-answer-run!
  "Retry one failed answer with its exact provider request and evidence snapshot.
   Post: creates a new user turn and run. The failed run stays unchanged."
  [user-id run-uuid client-id]
  (if-let [failed-run (db/get-assistant-run user-id run-uuid)]
    (cond
      (not= "answer" (:kind failed-run)) {:error "Only answers can be retried."}
      (not= "failed" (:status failed-run)) {:error "Only failed answers can be retried."}
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
                              :model (settings/effective-card-model user-id root-topic-id)
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

(defn referenceable-docs
  "Return root documents that a learner can reference from a chat.

   Pre: `query` is the unfinished text after `@`, or an empty string.
   Post: the result excludes the open document and keeps newest-first order.
   Invariant: every result has an `:id` and a displayable `:title`."
  ([user-id current-root-topic-id]
   (referenceable-docs user-id current-root-topic-id ""))
  ([user-id current-root-topic-id query]
   (let [needle (str/lower-case (or query ""))]
     (into []
       (comp
         (remove #(= current-root-topic-id (:topics/id %)))
         (map (fn [topic]
                {:id (:topics/id topic)
                 :title (or (:topics/title topic) "Untitled")}))
         (filter #(str/includes? (str/lower-case (:title %)) needle)))
       (db/get-root-topics user-id)))))

;; ── Reactive read wrappers (first arg = the relevant watched revision) ──────

(def max-rich-answer-chars
  "Maximum answer size for the formatted terminal renderer."
  100000)

(def ^:private transcript-wire-keys
  [:assistant_messages/id :assistant_messages/role
   :assistant_messages/content :assistant_messages/client_id
   :assistant_run/uuid :assistant_run/status :assistant_run/error
   :assistant_run/retry_of :assistant_run/evidence
   :assistant_event/type :assistant_event/covered_from
   :assistant_event/covered_through])

(def ^:private evidence-wire-keys
  [:citation_key :document_topic_id :page_topic_id :page_number :anchor
   :quoted_text :quoted_html :document_title :display_label])

(def ^:private retained-html-pattern
  #"(?is)<(?:blockquote|code|div|figure|h[1-6]|li|ol|p|pre|table|td|th|tr|ul)\b")

(defn- evidence-wire-row [row]
  (let [wire-row (select-keys row evidence-wire-keys)]
    (cond
      (:quoted_html row)
      (assoc wire-row :quoted_html
        (cleaner/clean-html-fragment (:quoted_html row)))

      (and (:quoted_text row) (re-find retained-html-pattern (:quoted_text row)))
      (assoc wire-row :quoted_text (text/strip-html (:quoted_text row)))

      :else wire-row)))

(defn- plain-evidence-citations [answer evidence-rows]
  (let [by-key (into {} (map (juxt :citation_key identity)) evidence-rows)]
    (str/replace (or answer "") evidence-token-pattern
      (fn [[_ citation-key]]
        (or (:display_label (get by-key citation-key)) "")))))

(defn transcript-message-view
  "Prepare one transcript row for the browser.

   Pre: message comes from messages*. The answer content is trusted Markdown,
   but its HTML is not trusted.
   Post: returns a wire-safe row with one render mode. Rich HTML is sanitized.

   A streaming answer renders through the SAME mode as its terminal answer.
   Formatting only at the end made the panel show unformatted Markdown source
   for the whole run and then swap renderers on the last frame, and that swap
   unmounts one node and mounts another — the answer blanked for a frame right
   before it settled. One mode for both states removes the swap and formats the
   text as it arrives. Partial Markdown is re-parsed on each publication, so an
   unclosed fence formats itself once its closing fence lands."
  [message]
  (let [wire-message (select-keys message transcript-wire-keys)
        wire-message (if (contains? wire-message :assistant_run/evidence)
                       (update wire-message :assistant_run/evidence
                         (fn [rows]
                           (mapv evidence-wire-row rows)))
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
  "Chat list for the panel. `_rev` forces re-query on bump."
  [_rev user-id root-topic-id]
  (db/get-assistant-chats user-id root-topic-id))

(defn messages*
  "Transcript and checkpoint events for an owned chat. `_rev` forces re-query.
   Active answer rows read their durable response text from the run authority."
  [_rev user-id chat-id]
  (when (and chat-id (db/get-assistant-chat user-id chat-id))
    (let [messages (db/get-assistant-messages chat-id)
          runs (db/get-assistant-run-summaries-for-chat user-id chat-id)
          run-by-message (into {} (keep (fn [run]
                                          (when-let [message-id (:assistant_message_id run)]
                                            [message-id run]))) runs)
          evidence-by-run (group-by :run_uuid
                            (db/get-assistant-evidence-for-chat user-id chat-id))
          message-events
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
            messages)
          checkpoint-events
          (mapv (fn [checkpoint]
                  {:assistant_messages/id (str "checkpoint-" (:id checkpoint))
                   :assistant_messages/role "event"
                   :assistant_messages/content (:summary checkpoint)
                   :assistant_event/type "compact"
                   :assistant_event/covered_from (:covered_from_message_id checkpoint)
                   :assistant_event/covered_through (:covered_through_message_id checkpoint)
                   :assistant_run/uuid (:run_uuid checkpoint)
                   :timeline/order (inc (* 2 (:covered_through_message_id checkpoint)))})
            (db/get-assistant-checkpoints user-id chat-id))]
      (->> (concat
             (map #(assoc % :timeline/order (* 2 (:assistant_messages/id %))) message-events)
             checkpoint-events)
        (sort-by :timeline/order)
        (mapv #(dissoc % :timeline/order))))))

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
  "Active run summary for an owned chat. `_rev` forces re-query."
  [_rev user-id chat-id]
  (when chat-id
    (some-> (db/get-active-assistant-run user-id chat-id)
      (select-keys [:run_uuid :kind :status]))))

(defn run-details*
  "Owner-scoped answer details with retained evidence and wire-safe timestamps."
  [_rev user-id run-uuid]
  (when run-uuid
    (when-let [run (db/get-assistant-run user-id run-uuid)]
      (let [timestamp-keys [:created_at :started_at :first_token_at :stopped_at
                            :completed_at :failed_at]
            wire-run (reduce (fn [result key]
                               (update result key #(some-> % str)))
                       run timestamp-keys)]
        (assoc wire-run
          :cost_usd (some-> (:cost_usd wire-run) double)
          :evidence (db/get-assistant-run-evidence user-id run-uuid))))))
