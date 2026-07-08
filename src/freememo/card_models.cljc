(ns freememo.card-models
  "Catalog of user-selectable models for flashcard generation.

   A selection is stored per-user as an :id (see settings/get-model); the prod
   default is freememo.config/!prod-model. All lanes route through one OpenRouter
   key (like OCR, topology A1) and bill from OpenRouter's returned usage.cost.

   These :ids are card-lane-only and intentionally differ from
   freememo.ocr-models ids for the same underlying model — the two registries are
   independent (design decision D1-B).")

(def registry
  "Ordered for display. :id is the durable settings value — never rename it,
   stored rows reference it. :openrouter-model is the slug sent to OpenRouter."
  [{:id "gpt-5.1"
    :label "OpenAI · GPT-5.1"
    :openrouter-model "openai/gpt-5.1"}
   {:id "gemini-3-flash"
    :label "Google · Gemini 3 Flash"
    :openrouter-model "google/gemini-3-flash-preview"}
   {:id "gemini-3.5-flash"
    :label "Google · Gemini 3.5 Flash"
    :openrouter-model "google/gemini-3.5-flash"}
   {:id "claude-haiku-4.5"
    :label "Anthropic · Claude Haiku 4.5"
    :openrouter-model "anthropic/claude-haiku-4.5"}
   {:id "claude-sonnet-5"
    :label "Anthropic · Claude Sonnet 5"
    :openrouter-model "anthropic/claude-sonnet-5"}
   {:id "deepseek-v4-flash"
    :label "DeepSeek · V4 Flash"
    :openrouter-model "deepseek/deepseek-v4-flash"}])

(def default-id
  "System-wide default card model when a user has no saved selection and no prod
   pin applies. Matches the current !prod-model value."
  "gpt-5.1")

(def ^:private by-id
  (into {} (map (juxt :id identity)) registry))

(defn resolve-model
  "Registry entry for `id`, or nil when `id` is unknown/unset.
   Pre: id is a string or nil. Post: a registry map or nil — callers MUST handle
   nil (treat as: fall back to the default)."
  [id]
  (get by-id id))
