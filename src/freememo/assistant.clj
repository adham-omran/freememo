(ns freememo.assistant
  "Server-side Socratic AI assistant for the reading view.

   One chat is a conversation about one document (root topic). At chat creation
   the current page's text is resolved server-side and persisted as message #1
   (role 'user') so the context is established once and frozen to that page.
   Each send assembles [system persona] ++ stored transcript, calls OpenRouter,
   persists the reply, and bills usage.cost through the same credit path as card
   generation (no-op on self-host). No streaming (v1)."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.openrouter :as openrouter]
   [freememo.settings :as settings]
   [freememo.card-models :as card-models]
   [freememo.cards :as cards]
   [freememo.cards-from-facts :as cff]
   [freememo.credits :as credits]
   [freememo.commands :as commands]
   [freememo.toasts :as toasts]
   [freememo.db :as db]
   [taoensso.telemere :as tel]))

(def ^:private socratic-system-prompt
  (delay (try (slurp (io/resource "prompts/assistant-socratic.md"))
           (catch Exception _ nil))))

(defn- bump! [user-id]
  (commands/bump-channels! user-id #{:assistant-mutations}))

;; ── Context resolution ─────────────────────────────────────────────────────

(defn page-context-text
  "The current page's text for `page-topic-id`, resolved server-side and wrapped
   in a fixed preamble. Works for any topic kind: a PDF page topic and a
   basic/wiki/web/epub topic both carry their content in topics.content.
   Post: a non-blank string (a placeholder when no text is extracted yet)."
  [page-topic-id]
  (let [content (some-> page-topic-id db/get-topic :topics/content str/trim)]
    (str "Material the learner is currently reading:\n\n"
      (if (str/blank? content)
        "(This page has no extracted text yet.)"
        content))))

(defn- title-from
  "A short chat title derived from the learner's first message."
  [text]
  (let [one-line (-> text (str/replace #"\s+" " ") str/trim)]
    (if (> (count one-line) 48)
      (str (subs one-line 0 48) "…")
      one-line)))

(defn- grounding-system-suffix
  "System-prompt suffix grounding the tutor in the document's approved KG facts.
   The persona forbids direct answers, so the suffix instructs the model to use
   the facts to steer its questions, not to recite them.
   Pre: user-id owns root-topic-id's document. Post: a non-blank string iff the
   document has ≥1 approved fact, else nil (persona left unchanged)."
  [user-id root-topic-id]
  (let [facts (db/get-kg-facts user-id root-topic-id "approved" nil)]
    (when (seq facts)
      (str "\n\n# Facts established in this document\n\n"
        "Use these only to ground and steer your questions — do not recite them "
        "as answers.\n\n"
        (cff/render-facts facts)))))

(defn- reference-context-messages
  "Transient OpenRouter context turns for @-referenced documents — one per
   referenced doc, full content, not persisted. Each id is re-checked for
   ownership here (the client-offered list is not trusted); foreign, missing,
   or empty docs are skipped.
   Pre: ref-topic-ids is a (possibly empty/nil) seq of root-topic ids.
   Post: order follows ref-topic-ids; every emitted turn belongs to user-id."
  [user-id ref-topic-ids]
  (into []
    (keep (fn [id]
            (when-let [t (db/get-topic-for-user user-id id)]
              (let [content (some-> (:topics/content t) str/trim)]
                (when-not (str/blank? content)
                  {:role "user"
                   :content (str "Referenced document \""
                              (or (:topics/title t) "Untitled") "\":\n\n" content)})))))
    (or ref-topic-ids [])))

;; ── Message assembly ───────────────────────────────────────────────────────

(defn- assemble-messages
  "[{:role \"system\" persona+suffix}] ++ transient context turns ++ stored
   transcript, all mapped to OpenRouter shape. `system-suffix` (string|nil)
   grounds the persona; `extra-context-msgs` are non-persisted turns (referenced
   documents). Post: called with (nil, nil) the output equals the pre-grounding
   behavior."
  [chat-id system-suffix extra-context-msgs]
  (into (into [{:role "system" :content (str @socratic-system-prompt system-suffix)}]
          (or extra-context-msgs []))
    (map (fn [m] {:role (:assistant_messages/role m)
                  :content (:assistant_messages/content m)}))
    (db/get-assistant-messages chat-id)))

;; ── Commands ───────────────────────────────────────────────────────────────

(defn start-chat!
  "Create a chat for (user-id, root-topic-id) and persist the page context as
   message #1. Returns the new chat id. Bumps :assistant-mutations."
  [user-id root-topic-id page-topic-id]
  (let [chat-id (db/create-assistant-chat! user-id root-topic-id "New chat")]
    (db/insert-assistant-message! chat-id "user" (page-context-text page-topic-id))
    (bump! user-id)
    chat-id))

(defn send!
  "Send `user-text` to `chat-id` and return {:ok true :reply s} or {:error s}.
   `ref-topic-ids` (seq, may be empty/nil) are @-referenced documents injected as
   transient context for this turn only. Approved KG facts for the document are
   injected into the system prompt each send.
   Order: ownership gate → key/blank checks → credit gate → persist user turn →
   call model → persist reply → charge usage.cost → title backfill → bump.
   Pre: chat-id owned by user-id (else {:error}). Post: on success exactly one
   assistant row is appended and one usage charge recorded; on model failure the
   user turn persists but no assistant row is added; referenced docs and facts
   are never persisted to the transcript."
  [user-id chat-id user-text ref-topic-ids]
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
          (do
            (db/insert-assistant-message! chat-id "user" user-text)
            (bump! user-id) ; show the learner's turn immediately
            (let [model-id (settings/effective-assistant-model
                             user-id (:assistant_chats/root_topic_id chat))
                  entry (or (card-models/resolve-model model-id)
                          (card-models/resolve-model settings/assistant-default-model-id))
                  slug (:openrouter-model entry)]
              (try
                (let [body (openrouter/chat-completion! api-key
                             {:model slug
                              :messages (assemble-messages chat-id
                                          (grounding-system-suffix user-id (:assistant_chats/root_topic_id chat))
                                          (reference-context-messages user-id ref-topic-ids))})
                      reply (-> body :choices first :message :content)
                      cost (double (or (-> body :usage :cost) 0))]
                  (if (str/blank? reply)
                    {:error "The model returned an empty response."}
                    (do
                      (db/insert-assistant-message! chat-id "assistant" reply)
                      (credits/record-cost-charge! user-id :assistant.chat (:id entry) cost)
                      (when (= "New chat" (:assistant_chats/title chat))
                        (db/set-assistant-chat-title! chat-id (title-from user-text)))
                      (bump! user-id)
                      {:ok true :reply reply})))
                (catch Exception e
                  (tel/error! {:id ::assistant-send :data {:user-id user-id :chat-id chat-id}} e)
                  {:error (or (ex-message e) "Assistant request failed.")})))))))))

(defn ensure-and-send!
  "Send to `chat-id`, creating a fresh chat first when `chat-id` is nil (the
   learner typed before any chat existed). `ref-topic-ids` are @-referenced
   documents for this turn. Returns the send result plus the :chat-id actually
   used, so the client can select it."
  [user-id root-topic-id page-topic-id chat-id user-text ref-topic-ids]
  (let [cid (or chat-id (start-chat! user-id root-topic-id page-topic-id))]
    (assoc (send! user-id cid user-text ref-topic-ids) :chat-id cid)))

(defn reply->cards!
  "Generate basic flashcards from an assistant `reply-text` and save them under
   the reading page (`page-topic-id`). Uses the reply as the card source
   verbatim — it bypasses the KG fact-routing detour that the toolbar Generate
   applies — and reports the outcome via a toast.
   Pre: page-topic-id / root-topic-id are the learner's current reading topic;
   reply-text is an assistant reply. Post: on success (count :ids) basic cards
   exist under page-topic-id and :card-mutations is bumped; on failure nothing
   is saved. Returns {:success true :ids v :count n} | {:success false :error s}."
  [user-id page-topic-id root-topic-id reply-text]
  (let [result (if (str/blank? reply-text)
                 {:success false :error "Nothing to capture — the reply is empty."}
                 (let [gen (cards/generate-basic-cards
                             {:content reply-text
                              :card-count (settings/get-card-count user-id)
                              :model (settings/effective-card-model user-id root-topic-id)
                              :user-id user-id
                              :topic-id page-topic-id})]
                   (if-not (:success gen)
                     {:success false :error (:error gen)}
                     (let [saved (cards/save-cards page-topic-id root-topic-id "basic" (:cards gen))]
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
  "Root topics the learner can @-reference from a chat, excluding the current
   document. Returns [{:id t :title s}] in get-root-topics order (newest first).
   Post: current-root-topic-id never appears."
  [user-id current-root-topic-id]
  (into []
    (comp (remove #(= current-root-topic-id (:topics/id %)))
      (map (fn [t] {:id (:topics/id t) :title (or (:topics/title t) "Untitled")})))
    (db/get-root-topics user-id)))

;; ── Reactive read wrappers (first arg = watched :assistant-mutations rev) ────

(defn chats*
  "Chat list for the panel. `_rev` forces re-query on bump."
  [_rev user-id root-topic-id]
  (db/get-assistant-chats user-id root-topic-id))

(defn messages*
  "Transcript for `chat-id` IFF owned by `user-id`. `_rev` forces re-query."
  [_rev user-id chat-id]
  (when (and chat-id (db/get-assistant-chat user-id chat-id))
    (db/get-assistant-messages chat-id)))
