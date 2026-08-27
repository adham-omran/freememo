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
   stored rows reference it. :openrouter-model is the slug sent to OpenRouter.
   :provider, when present, is OpenRouter's provider-routing request object,
   sent verbatim by openrouter/model-fields — the only way a lane names a
   provider. Absent means OpenRouter picks the provider."
  [{:id "gpt-5.1"
    :label "OpenAI · GPT-5.1"
    :openrouter-model "openai/gpt-5.1"}
   {:id "gpt-5.6-luna"
    :label "OpenAI · GPT-5.6 Luna"
    :openrouter-model "openai/gpt-5.6-luna"}
   {:id "gpt-5.6-terra"
    :label "OpenAI · GPT-5.6 Terra"
    :openrouter-model "openai/gpt-5.6-terra"}
   {:id "gpt-5.6-sol"
    :label "OpenAI · GPT-5.6 Sol"
    :openrouter-model "openai/gpt-5.6-sol"}
   {:id "gemini-3-flash"
    :label "Google · Gemini 3 Flash"
    :openrouter-model "google/gemini-3-flash-preview"}
   {:id "gemini-3.5-flash"
    :label "Google · Gemini 3.5 Flash"
    :openrouter-model "google/gemini-3.5-flash"}
   {:id "gemini-3.6-flash"
    :label "Google · Gemini 3.6 Flash"
    :openrouter-model "google/gemini-3.6-flash"}
   {:id "gemini-3.5-flash-lite"
    :label "Google · Gemini 3.5 Flash-Lite"
    :openrouter-model "google/gemini-3.5-flash-lite"}
   {:id "claude-haiku-4.5"
    :label "Anthropic · Claude Haiku 4.5"
    :openrouter-model "anthropic/claude-haiku-4.5"}
   {:id "claude-sonnet-5"
    :label "Anthropic · Claude Sonnet 5"
    :openrouter-model "anthropic/claude-sonnet-5"}
   ;; A dated snapshot, not an alias. OpenRouter publishes no `-latest` slug for
   ;; this model, and the one written here earlier was rejected with
   ;; "deepseek/deepseek-v4-flash-latest is not a valid model ID" — an HTTP 400
   ;; on every call in every lane that selected it. Verify a slug against
   ;; https://openrouter.ai/api/v1/models before shipping it; this one retires
   ;; when DeepSeek withdraws the snapshot.
   {:id "deepseek-v4-flash"
    :label "DeepSeek · V4 Flash"
    :openrouter-model "deepseek/deepseek-v4-flash-0731"}
   ;; Pinned to Z.ai: every other provider of this slug listed 2× the price on
   ;; 2026-08-27, and the pin fails the call on a Z.ai outage rather than pay
   ;; it. See plans/add-glm-5.3-flash-z-ai-pin.md.
   {:id "glm-5.3-flash"
    :label "Z.ai · GLM 5.3 Flash"
    :openrouter-model "z-ai/glm-5.3-flash"
    :provider {:only ["z-ai"]}}])

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
