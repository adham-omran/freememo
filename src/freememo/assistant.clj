(ns freememo.assistant
  "Server-side Socratic AI assistant for the reading view.

   One chat is a conversation about one document (root topic). Each send
   assembles [system persona (+ approved KG facts)] ++ a transient turn holding
   the learner's LIVE reading context (breadcrumb + the current page, or a
   ±window page span for PDFs) ++ any @-referenced documents ++ the stored
   transcript, calls OpenRouter, persists the reply, and bills usage.cost through
   the same credit path as card generation (no-op on self-host). The reading
   context is recomputed per send (never persisted), so it tracks the page on
   screen. No streaming (v1)."
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

(def ^:private max-context-facts
  "Upper bound on KG facts injected into one send's grounding — keeps a large
   document (or code repo) from overflowing the prompt. Nearest facts win."
  50)

(def ^:private max-window-chars
  "Upper bound on the combined characters of the PDF page window in one send.
   Farthest pages are dropped first; the current page is never dropped."
  40000)

(defn- resolve-context
  "Resolve the learner's live reading context for one send, from the currently
   open topic. Single lookup point so grounding and the context turn agree.
   Pre: root-topic-id owned by user-id; page-topic-id is the on-screen topic
   (a PDF page topic, or the topic itself for non-PDF kinds).
   Post: {:root-id :root-kind :pdf? :page-topic-id :center-page :window};
   :center-page is the 1-based page number for a PDF page, else nil."
  [user-id root-topic-id page-topic-id]
  (let [root (db/get-topic root-topic-id)
        pdf? (= "pdf" (:topics/kind root))
        center-page (when pdf?
                      (:topics/page_number (db/get-topic page-topic-id)))]
    {:root-id root-topic-id
     :root-kind (:topics/kind root)
     :pdf? pdf?
     :page-topic-id page-topic-id
     :center-page center-page
     :window (settings/get-assistant-pdf-window user-id)}))

(defn- breadcrumb-line
  "One line locating the open node within its document tree, root→node, so the
   model knows it is reading a section rather than the whole document.
   Pre: page-topic-id may be nil. Post: a non-blank string when the node has ≥1
   ancestor (path length > 1), else nil (nothing useful to say for a bare root)."
  [page-topic-id]
  (let [ids (db/get-ancestor-ids page-topic-id)] ; [self … root], nearest-first
    (when (> (count ids) 1)
      (let [titles (db/get-topic-titles ids)
            path (mapv #(or (get titles %) "Untitled") (reverse ids))]
        (str "You are reading a section of the document \"" (first path) "\".\n"
          "Location (root → current): " (str/join " › " path) ".")))))

(defn- pdf-window-text
  "Text of the PDF pages within ±window of `center-page`, in reading order, each
   page delimited and the current page marked. Blank/unextracted pages are
   skipped. The combined length is capped at `max-window-chars`, dropping the
   pages farthest from center first; the center page is always kept.
   Pre: center-page is a 1-based page number; window ≥ 0.
   Post: a non-blank string, or nil when no page in range has extracted text."
  [root-id center-page window]
  (when (and root-id center-page)
    (let [start (max 1 (- center-page window))
          end   (+ center-page window)
          pages (->> (db/get-context-pages root-id start end)
                  (keep (fn [row]
                          (when-let [c (some-> (:topics/content row) str/trim not-empty)]
                            {:page (:topics/page_number row) :text c}))))
          ;; nearest-first so the farthest pages fall off the char budget first
          ordered (sort-by #(Math/abs (- (int (:page %)) (int center-page))) pages)
          {:keys [kept omitted]}
          (reduce (fn [{:keys [kept total omitted]} {:keys [text] :as p}]
                    (let [len (count text)]
                      (if (or (empty? kept) (<= (+ total len) max-window-chars))
                        {:kept (conj kept p) :total (+ total len) :omitted omitted}
                        {:kept kept :total total :omitted (inc omitted)})))
            {:kept [] :total 0 :omitted 0} ordered)]
      (when (seq kept)
        (let [body (->> kept
                     (sort-by :page)
                     (map (fn [{:keys [page text]}]
                            (str "--- Page " page
                              (when (= page center-page) " (current page)") " ---\n"
                              text)))
                     (str/join "\n\n"))]
          (if (pos? omitted)
            (str body "\n\n(" omitted " nearby page(s) omitted for length.)")
            body))))))

(defn- reading-material
  "The text the learner is currently reading: a ±window page span for a PDF,
   else the open topic's own content. Post: non-blank string or nil."
  [ctx]
  (if (:pdf? ctx)
    (pdf-window-text (:root-id ctx) (:center-page ctx) (:window ctx))
    (some-> (:page-topic-id ctx) db/get-topic :topics/content str/trim not-empty)))

(defn- current-context-messages
  "The learner's live reading context as a single transient user turn: a
   breadcrumb locating the open node plus the reading material. Recomputed each
   send and never persisted, so it always reflects the page on screen now.
   Pre: ctx from resolve-context. Post: [] when nothing locatable, else a
   one-element vector [{:role \"user\" :content s}]."
  [ctx]
  (let [crumb    (breadcrumb-line (:page-topic-id ctx))
        material (reading-material ctx)
        sections (remove str/blank?
                   [crumb
                    (when material
                      (str "Material the learner is currently reading:\n\n" material))])]
    (if (empty? sections)
      []
      [{:role "user" :content (str/join "\n\n" sections)}])))

(defn- title-from
  "A short chat title derived from the learner's first message."
  [text]
  (let [one-line (-> text (str/replace #"\s+" " ") str/trim)]
    (if (> (count one-line) 48)
      (str (subs one-line 0 48) "…")
      one-line)))

(defn- grounding-system-suffix
  "System-prompt suffix grounding the tutor in the document's approved KG facts.
   For a PDF the facts are limited to the current ±window pages (nearest first);
   otherwise the whole document's facts are used. Either way the count is capped
   at max-context-facts so a large document or code repo cannot overflow the
   prompt. The persona forbids direct answers, so the suffix says to steer with
   the facts, not recite them.
   Pre: ctx from resolve-context (root owned by user-id).
   Post: a non-blank string iff ≥1 fact selected, else nil (persona unchanged)."
  [user-id ctx]
  (let [facts (if (and (:pdf? ctx) (:center-page ctx))
                (let [c (:center-page ctx) w (:window ctx)]
                  (->> (db/get-kg-facts-context user-id (:root-id ctx)
                         {:start-page (max 1 (- c w)) :end-page (+ c w)})
                    (sort-by #(Math/abs (- (int (or (:page_number %) c)) (int c))))
                    (take max-context-facts)))
                (db/get-kg-facts-context user-id (:root-id ctx)
                  {:limit max-context-facts}))]
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
  "Create a chat for (user-id, root-topic-id). Returns the new chat id and bumps
   :assistant-mutations. The page context is no longer frozen here — each send
   injects the learner's live reading context (see current-context-messages)."
  [user-id root-topic-id]
  (let [chat-id (db/create-assistant-chat! user-id root-topic-id "New chat")]
    (bump! user-id)
    chat-id))

(defn send!
  "Send `user-text` to `chat-id` and return {:ok true :reply s} or {:error s}.
   `page-topic-id` is the learner's on-screen topic NOW (a PDF page topic or a
   plain topic); its live reading context (breadcrumb + page/window) is injected
   as a transient turn this send. `ref-topic-ids` (seq, may be empty/nil) are
   @-referenced documents, also transient. Approved KG facts (page-windowed for
   PDFs) ground the system prompt each send.
   Order: ownership gate → key/blank checks → credit gate → persist user turn →
   resolve live context → call model → persist reply → charge usage.cost →
   title backfill → bump.
   `client-id` is the client's echo-correlation id, stored on the user turn so
   the optimistic echo retires when its persisted row lands (nil = no echo).
   Pre: chat-id owned by user-id (else {:error}). Post: on success exactly one
   assistant row is appended and one usage charge recorded; on model failure the
   user turn persists but no assistant row is added; reading context, referenced
   docs, and facts are never persisted to the transcript."
  [user-id chat-id page-topic-id user-text ref-topic-ids client-id]
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
            (db/insert-assistant-message! chat-id "user" user-text client-id)
            (bump! user-id) ; show the learner's turn immediately
            (let [root-topic-id (:assistant_chats/root_topic_id chat)
                  ctx (resolve-context user-id root-topic-id page-topic-id)
                  context-msgs (into (current-context-messages ctx)
                                 (reference-context-messages user-id ref-topic-ids))
                  model-id (settings/effective-assistant-model user-id root-topic-id)
                  entry (or (card-models/resolve-model model-id)
                          (card-models/resolve-model settings/assistant-default-model-id))
                  slug (:openrouter-model entry)]
              (try
                (let [body (openrouter/chat-completion! api-key
                             {:model slug
                              :messages (assemble-messages chat-id
                                          (grounding-system-suffix user-id ctx)
                                          context-msgs)
                              :reasoning_effort (settings/get-reasoning user-id)})
                      reply (-> body :choices first :message :content)
                      cost (double (or (-> body :usage :cost) 0))]
                  (if (str/blank? reply)
                    {:error "The model returned an empty response."}
                    (do
                      (db/insert-assistant-message! chat-id "assistant" reply nil)
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
   documents for this turn; `client-id` correlates the client's optimistic echo.
   Returns the send result plus the :chat-id actually used, so the client can
   select it."
  [user-id root-topic-id page-topic-id chat-id user-text ref-topic-ids client-id]
  (let [cid (or chat-id (start-chat! user-id root-topic-id))]
    (assoc (send! user-id cid page-topic-id user-text ref-topic-ids client-id) :chat-id cid)))

(def ^:private feedback-anchor
  "Verbatim heading the persona emits to open the feedback section. The parser
   keys on it so card capture uses only the feedback, never the Socratic
   question that follows under the question anchor."
  "**Where you are**")

(def ^:private question-anchor
  "Verbatim heading that ends the feedback section (start of the question)."
  "**Consider next**")

(defn- extract-feedback
  "The feedback body from a structured assistant reply, or nil when the reply
   lacks the feedback anchor. Pre: `reply` is a non-blank assistant reply. Post:
   returns the trimmed text between the feedback anchor and the question anchor
   (or end-of-string when the question anchor is absent); returns nil when the
   feedback anchor is missing — an unstructured reply the caller must not card."
  [reply]
  (when-let [start (some-> reply (str/index-of feedback-anchor))]
    (let [after (subs reply (+ start (count feedback-anchor)))
          end (str/index-of after question-anchor)]
      (str/trim (if end (subs after 0 end) after)))))

(defn reply->cards!
  "Generate basic flashcards from the FEEDBACK section of an assistant reply
   (the `**Where you are**` body) and save them under the reading page
   (`page-topic-id`). Cards only the feedback — never the Socratic question —
   and bypasses the KG fact-routing detour that the toolbar Generate applies,
   reporting the outcome via a toast.
   Pre: page-topic-id / root-topic-id are the learner's current reading topic;
   reply-text is a structured assistant reply. Post: on success (count :ids)
   basic cards exist under page-topic-id and :card-mutations is bumped; when the
   reply has no feedback anchor or an empty feedback body, nothing is saved and
   a toast says so; on failure nothing is saved. Returns
   {:success true :ids v :count n} | {:success false :error s}."
  [user-id page-topic-id root-topic-id reply-text]
  (let [feedback (extract-feedback reply-text)
        result (if (str/blank? feedback)
                 {:success false
                  :error "No feedback to capture yet — the reply is only a question."}
                 (let [gen (cards/generate-basic-cards
                             {:content feedback
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
