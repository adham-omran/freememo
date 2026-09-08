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
   current code path) or :plugin (OpenRouter file-parser, engine mistral-ocr).
   :provider, when present, is OpenRouter's provider-routing request object,
   sent verbatim by openrouter/model-fields — the only way a lane names a
   provider. Absent means OpenRouter picks the provider."
  [{:id "openai-gpt-5.1"
    :label "OpenAI · GPT-5.1"
    :shape :chat
    :openrouter-model "openai/gpt-5.1"}
   {:id "openai-gpt-5.6-luna"
    :label "OpenAI · GPT-5.6 Luna"
    :shape :chat
    :openrouter-model "openai/gpt-5.6-luna"}
   {:id "openai-gpt-5.6-terra"
    :label "OpenAI · GPT-5.6 Terra"
    :shape :chat
    :openrouter-model "openai/gpt-5.6-terra"}
   {:id "openai-gpt-5.6-sol"
    :label "OpenAI · GPT-5.6 Sol"
    :shape :chat
    :openrouter-model "openai/gpt-5.6-sol"}
   {:id "gemini-3-flash"
    :label "Google · Gemini 3 Flash"
    :shape :chat
    :openrouter-model "google/gemini-3-flash-preview"}
   {:id "gemini-3.6-flash"
    :label "Google · Gemini 3.6 Flash"
    :shape :chat
    :openrouter-model "google/gemini-3.6-flash"}
   {:id "gemini-3.8-flash"
    :label "Google · Gemini 3.8 Flash"
    :shape :chat
    :openrouter-model "google/gemini-3.8-flash"}
   {:id "gemini-3.5-flash-lite"
    :label "Google · Gemini 3.5 Flash-Lite"
    :shape :chat
    :openrouter-model "google/gemini-3.5-flash-lite"}
   ;; Routed cheapest-first under a price ceiling — same reasoning as the
   ;; card-models entry, including why the ceiling sits at the undiscounted rate
   ;; and why the key must be :max_price and not :max-price.
   {:id "glm-5.3-flash"
    :label "Z.ai · GLM 5.3 Flash"
    :shape :chat
    :openrouter-model "z-ai/glm-5.3-flash"
    :provider {:sort "price"
               :max_price {:prompt 0.15 :completion 0.50}}}
   {:id "mistral-ocr-4"
    :label "Mistral · OCR 4"
    :shape :plugin
    :plugin-engine "mistral-ocr"}])

(def retired
  "OCR ids withdrawn from the picker. Same contract as
   freememo.card-models/retired, and the same reason it is a redirect rather
   than a deletion — read that docstring. The two registries stay independent
   (design decision D1-B), so an id retired in one is not retired in the other
   by implication; this one happens to share both id and successor with the
   card lane."
  [{:id "gemini-3.5-flash"
    :label "Google · Gemini 3.5 Flash"
    :superseded-by "gemini-3.6-flash"}])

(def ^:private supersessions
  (into {} (map (juxt :id :superseded-by)) retired))

(defn effective-id
  "The id a stored selection names today; see card-models/effective-id.
   Pre: id is a string or nil. Post: a string or nil."
  [id]
  (get supersessions id id))

(def default-id
  "System-wide default OCR model — used when a user has set neither a per-document
   nor a global selection. Gemini 3 Flash: best Arabic accuracy at the lowest cost
   in the 0.3 bake-off (see plans/ocr-multi-model-picker.md)."
  "gemini-3-flash")

(def ^:private by-id
  (into {} (map (juxt :id identity)) registry))

(defn resolve-model
  "Registry entry to CALL for `id`, or nil when `id` is unknown/unset. A retired
   id resolves to its successor's entry, so `(:id (resolve-model id))` may
   differ from `id`.
   Pre: id is a string or nil. Post: a registry map or nil — callers MUST
   handle nil (treat as: fall back to the global default)."
  [id]
  (get by-id (effective-id id)))
