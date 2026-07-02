(ns freememo.ocr-models
  "Catalog of user-selectable OCR models for \"Scan Page\".

   A selection is stored per-document as an :id (see settings/get-ocr-model);
   the user's global default is settings/get-ocr-model-default. All lanes route through one
   OpenRouter key (topology A1). :shape tells freememo.ocr/extract-text how to
   build the request — it is the single dispatch key for the request layer.

   Caveats carried from the spec (plans/ocr-multi-model-picker.md):
   - :openrouter-model ids are NOT yet verified live-callable (spec 2.2).
   - The :plugin (Mistral OCR) lane is gated by the Phase-0 plugin probe
     (spec 0.1/0.2); its request wiring (spec 3.3) is intentionally unbuilt.")

(def registry
  "Ordered for display. :id is the durable settings value — never rename it,
   stored rows reference it. :shape is :chat (vision chat-completion, the
   current code path) or :plugin (OpenRouter file-parser, engine mistral-ocr)."
  [{:id "openai-gpt-5.1"
    :label "OpenAI · GPT-5.1"
    :shape :chat
    :openrouter-model "openai/gpt-5.1"}
   {:id "gemini-3-flash"
    :label "Google · Gemini 3 Flash"
    :shape :chat
    :openrouter-model "google/gemini-3-flash-preview"}
   {:id "mistral-ocr-4"
    :label "Mistral · OCR 4"
    :shape :plugin
    :plugin-engine "mistral-ocr"}])

(def default-id
  "System-wide default OCR model — used when a user has set neither a per-document
   nor a global selection. Gemini 3 Flash: best Arabic accuracy at the lowest cost
   in the 0.3 bake-off (see plans/ocr-multi-model-picker.md)."
  "gemini-3-flash")

(def ^:private by-id
  (into {} (map (juxt :id identity)) registry))

(defn resolve-model
  "Registry entry for `id`, or nil when `id` is unknown/unset.
   Pre: id is a string or nil. Post: a registry map or nil — callers MUST
   handle nil (treat as: fall back to the global default)."
  [id]
  (get by-id id))
