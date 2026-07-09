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
   [freememo.credits :as credits]
   [freememo.commands :as commands]
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

;; ── Message assembly ───────────────────────────────────────────────────────

(defn- assemble-messages
  "[{:role \"system\" persona}] ++ stored transcript mapped to OpenRouter shape."
  [chat-id]
  (into [{:role "system" :content @socratic-system-prompt}]
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
   Order: ownership gate → key/blank checks → credit gate → persist user turn →
   call model → persist reply → charge usage.cost → title backfill → bump.
   Pre: chat-id owned by user-id (else {:error}). Post: on success exactly one
   assistant row is appended and one usage charge recorded; on model failure the
   user turn persists but no assistant row is added."
  [user-id chat-id user-text]
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
                             {:model slug :messages (assemble-messages chat-id)})
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
   learner typed before any chat existed). Returns the send result plus the
   :chat-id actually used, so the client can select it."
  [user-id root-topic-id page-topic-id chat-id user-text]
  (let [cid (or chat-id (start-chat! user-id root-topic-id page-topic-id))]
    (assoc (send! user-id cid user-text) :chat-id cid)))

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
