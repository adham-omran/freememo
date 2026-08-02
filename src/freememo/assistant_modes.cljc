(ns freememo.assistant-modes
  "Catalog of assistant personas the learner picks per chat.

   A selection is stored on `assistant_chats.mode` as an :id. The server splices
   the entry's :prompt file into the send's system message
   (freememo.assistant/mode-system-prompt); the panel renders :label as a pill
   and seeds an empty chat's :starters and :hint (freememo.assistant-panel).

   Dual-peer (.cljc) because both peers read it — pills, starters and hint on
   the client, persona selection on the server. Same reason card_models.cljc is
   .cljc: a registry named in a reactive body must resolve on the CLJS peer too.

   Mode is per CHAT, not per document or per user: `send!` reads it off the chat
   row it already fetches, so a chat's transcript is one persona end to end
   unless the learner deliberately switches it.")

(def registry
  "Ordered left-to-right as the panel's Mode pills. :id is the durable stored
   value — never rename it, `assistant_chats.mode` rows reference it. :prompt is
   a resource path. :starters is exactly four first-message suggestions shown on
   an empty chat; :hint is the one-line empty-state line above them.

   :answers-directly? splits the catalog on whether the persona may state a fact
   as an answer. It is read by freememo.assistant/grounding-system-suffix, which
   otherwise wraps the injected KG facts in a ban ('do not recite them as
   answers') that contradicts General's whole purpose."
  [{:id "general"
    :label "General"
    :prompt "prompts/assistant-general.md"
    :answers-directly? true
    :hint "Ask about this page and I'll answer directly, naming where it says so."
    :starters ["Summarize the main points of this page."
               "Define the key terms used on this page."
               "Walk me through the hardest part of this page step by step."
               "What background does this page assume I already have?"]}
   {:id "tutor"
    :label "Tutor"
    :prompt "prompts/assistant-tutor.md"
    :answers-directly? false
    :hint "Ask about this page and I'll help you work it out."
    :starters ["Help me work through the main idea of this page."
               "What should I be able to explain after reading this?"
               "Where am I likely to go wrong with this material?"
               "How does this build on what came before?"]}
   {:id "socratic"
    :label "Socratic"
    :prompt "prompts/assistant-socratic.md"
    :answers-directly? false
    :hint "Ask a question about this page and I'll help you think it through."
    :starters ["What prerequisites do I need to understand this material?"
               "Explain the core idea of this page in simple terms."
               "What questions should I be able to answer after reading this?"
               "How does this connect to things I might already know?"]}])

(def default-id
  "Mode a chat gets when it carries none. Every chat that predates the mode
   column was this persona, so it is also what a NULL column resolves to."
  "socratic")

(def ^:private by-id
  (into {} (map (juxt :id identity)) registry))

(defn resolve-mode
  "Registry entry for `id`, falling back to `default-id`.

   Total by design, unlike card-models/resolve-model (which returns nil and
   makes every caller handle it): a persona is always needed, and the fallback
   is the same at every call site. Matches right-side-panel/resolve-tab.

   Pre:  `id` is a string or nil — an unrecognized value is legitimate input
         (a NULL column, a stale row, a client that sent junk).
   Post: a registry map, never nil."
  [id]
  (or (get by-id id) (get by-id default-id)))

(defn known-id?
  "True when `id` names a registry entry. The gate for a WRITE, where an
   unrecognized value is a caller bug rather than something to paper over —
   resolve-mode's silent fallback is for reads only.
   Pre: `id` is a string or nil. Post: boolean."
  [id]
  (contains? by-id id))
