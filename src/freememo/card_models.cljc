(ns freememo.card-models
  "Catalog of user-selectable models for flashcard generation.

   A selection is stored per-user as an :id (see settings/get-model); the prod
   default is freememo.config/!prod-model. All lanes route through one OpenRouter
   key (like OCR, topology A1) and bill from OpenRouter's returned usage.cost.

   These :ids are card-lane-only and intentionally differ from
   freememo.ocr-models ids for the same underlying model — the two registries are
   independent (design decision D1-B).")

(def ^:private json-and-reasoning-parameters
  "OpenRouter parameters this application sends to most structured models.
   This set is the application-relevant subset of the model metadata."
  #{:reasoning_effort :response_format})

(def ^:private json-reasoning-and-verbosity-parameters
  (conj json-and-reasoning-parameters :verbosity))

(def registry
  "Ordered for display. :id is the durable settings value — never rename it,
   stored rows reference it. :openrouter-model is the slug sent to OpenRouter.
   :provider, when present, is OpenRouter's provider-routing request object,
   sent verbatim by openrouter/model-fields — the only way a lane names a
   provider. Absent means OpenRouter picks the provider.
   :supported-parameters is the application-relevant subset of OpenRouter's
   model-level parameter metadata, recorded on 2026-09-01. Structured lanes
   require :response_format. Request builders omit unsupported optional fields.
   :reasoning-mandatory? marks a model whose provider REFUSES to have reasoning
   turned off — OpenRouter's `reasoning.mandatory`, recorded 2026-09-08. Sending
   it reasoning_effort \"none\" returns HTTP 400 \"Reasoning is mandatory for this
   endpoint and cannot be disabled\", so openrouter/chat-completion-fields drops
   the field for these entries. Every other unsupported effort value is clamped
   by OpenRouter instead of refused (measured), so this flag covers the whole
   failure and no per-value table is needed."
  [{:id "gpt-5.1"
    :label "OpenAI · GPT-5.1"
    :openrouter-model "openai/gpt-5.1"
    :supported-parameters json-and-reasoning-parameters}
   {:id "gpt-5.6-luna"
    :label "OpenAI · GPT-5.6 Luna"
    :openrouter-model "openai/gpt-5.6-luna"
    :supported-parameters json-and-reasoning-parameters}
   {:id "gpt-5.6-terra"
    :label "OpenAI · GPT-5.6 Terra"
    :openrouter-model "openai/gpt-5.6-terra"
    :supported-parameters json-and-reasoning-parameters}
   {:id "gpt-5.6-sol"
    :label "OpenAI · GPT-5.6 Sol"
    :openrouter-model "openai/gpt-5.6-sol"
    :supported-parameters json-and-reasoning-parameters}
   {:id "gemini-3-flash"
    :label "Google · Gemini 3 Flash"
    :openrouter-model "google/gemini-3-flash-preview"
    :supported-parameters json-and-reasoning-parameters}
   {:id "gemini-3.6-flash"
    :label "Google · Gemini 3.6 Flash"
    :openrouter-model "google/gemini-3.6-flash"
    :supported-parameters json-and-reasoning-parameters
    :reasoning-mandatory? true}
   ;; Same OpenRouter price as Gemini 3.6 Flash — $0.75 / $3.75 per million —
   ;; and Google states no replacement relationship between any two 3.x Flash
   ;; models, so this is a sibling entry and not a supersession: 3.6 and the
   ;; Gemini 3 Flash Preview that OCR defaults to both stay. The method is
   ;; plans/model-supersession-and-benchmark-review-2026-09-02.md: a route has a
   ;; replacement only when the vendor or OpenRouter states one.
   ;;
   ;; It is also the SLOWEST of the three at that price. OpenRouter p50 over a
   ;; 30-minute window on 2026-09-08, traffic-weighted across endpoints: 3.8 is
   ;; 2.27 s to first token at 67 tok/s, 3.7 is 2.26 s at 96, and 3.6 is 1.58 s
   ;; at 104 — so an 800-token card batch takes ~14 s on 3.8 against ~9 s on 3.6.
   ;; Benchmarks and speed disagree here; pick per lane, and re-measure before
   ;; trusting these numbers, which move.
   {:id "gemini-3.8-flash"
    :label "Google · Gemini 3.8 Flash"
    :openrouter-model "google/gemini-3.8-flash"
    :supported-parameters json-and-reasoning-parameters
    :reasoning-mandatory? true}
   {:id "gemini-3.5-flash-lite"
    :label "Google · Gemini 3.5 Flash-Lite"
    :openrouter-model "google/gemini-3.5-flash-lite"
    :supported-parameters json-and-reasoning-parameters
    :reasoning-mandatory? true}
   {:id "claude-haiku-4.5"
    :label "Anthropic · Claude Haiku 4.5"
    :openrouter-model "anthropic/claude-haiku-4.5"
    :supported-parameters #{:response_format}}
   {:id "claude-sonnet-5"
    :label "Anthropic · Claude Sonnet 5"
    :openrouter-model "anthropic/claude-sonnet-5"
    :supported-parameters json-reasoning-and-verbosity-parameters}
   ;; A dated snapshot, not an alias. OpenRouter publishes no `-latest` slug for
   ;; this model, and the one written here earlier was rejected with
   ;; "deepseek/deepseek-v4-flash-latest is not a valid model ID" — an HTTP 400
   ;; on every call in every lane that selected it. Verify a slug against
   ;; https://openrouter.ai/api/v1/models before shipping it; this one retires
   ;; when DeepSeek withdraws the snapshot.
   {:id "deepseek-v4-flash"
    :label "DeepSeek · V4 Flash"
    :openrouter-model "deepseek/deepseek-v4-flash-0731"
    :supported-parameters json-and-reasoning-parameters}
   ;; Routed fastest-first under a price ceiling, NOT pinned to Z.ai. The
   ;; ceiling says what the pin it replaces was standing in for — never overpay
   ;; for this model — without naming a provider, which is how the pin went
   ;; stale.
   ;;
   ;; The pin read {:only ["z-ai"]}, leaving one allowed provider, so
   ;; `allow_fallbacks` had nowhere to route and a Z.ai throttle failed the call.
   ;; That was deliberate while Z.ai was alone at its price. It stopped being
   ;; true. On 2026-09-08 the stale pin shipped four HTTP 429s to one user in
   ;; eight minutes while Z.ai served the same model between the bursts; the
   ;; platform key was nowhere near a cap, so the throttle was Z.ai's own.
   ;;
   ;; Prices are USD per MILLION tokens. The ceiling is 0.15/0.50 because that is
   ;; the undiscounted market rate, and the 0.075/0.25 that Z.ai, DeepInfra,
   ;; Novita and GMICloud all showed on 2026-09-08 is a 50% DISCOUNT expiring
   ;; 2026-09-09 16:00 UTC — OpenRouter reports it as `pricing.discount 0.5` per
   ;; endpoint. A ceiling at the discounted price would therefore have left one
   ;; qualifying provider the next day (Relace, the only one with no discount
   ;; field), rebuilding the single point of failure this change removes.
   ;;
   ;; `:sort` is "throughput", not "price". Cheapest-first sent every call to
   ;; the most-discounted endpoint — on 2026-09-23 DeepInfra's fp4 build at 50%
   ;; off — and the cheap endpoints are the crowded ones, so card generation was
   ;; slow at times. About twenty endpoints sit at or under the ceiling, so the
   ;; ceiling alone stops an overpay; the cost of this sort is the discount only.
   ;;
   ;; The key MUST be :max_price. `openrouter/model-fields` sends this map
   ;; verbatim and cheshire writes the keyword name as-is, so :max-price reaches
   ;; the wire as "max-price" and OpenRouter rejects the whole request: HTTP 400,
   ;; `provider: Unrecognized key: "max-price"`. Loud rather than silent, but
   ;; total — every call in both lanes, card generation and OCR. Measured live
   ;; 2026-09-08, along with the ceiling itself: a ceiling under every provider's
   ;; price returns HTTP 404 "No endpoints found that satisfy the max price", so
   ;; it is enforced and not advisory. openrouter-test guards the encoded name.
   ;; See plans/add-glm-5.3-flash-z-ai-pin.md.
   {:id "glm-5.3-flash"
    :label "Z.ai · GLM 5.3 Flash"
    :openrouter-model "z-ai/glm-5.3-flash"
    :supported-parameters json-and-reasoning-parameters
    :reasoning-mandatory? true
    :provider {:sort "throughput"
               :max_price {:prompt 0.15 :completion 0.50}}}
   ;; A 124B MoE model with 5.1B active parameters. It reads text only, so the
   ;; OCR registry has no entry for it. The one vision variant,
   ;; `inclusionai/ling-3.0-flash-vl`, exists only as a `:free` route.
   ;;
   ;; Two providers serve it, and only DeepInfra accepts response_format. The
   ;; structured lanes send `require_parameters true`, so card generation and the
   ;; KG steps always run on DeepInfra. There they pay 0.06 / 0.18 per million
   ;; tokens and get a 131K context. The assistant lane sends no such filter, and
   ;; OpenRouter routed it to Novita: 0.021 / 0.063, a 65% discount with no stated
   ;; end. A DeepInfra outage fails the structured lanes. That is a capability
   ;; gap, not a pin, so no :provider object can fix it. Both providers list the
   ;; same undiscounted price, so there is no overpay to cap either.
   ;;
   ;; Neither endpoint lists reasoning_effort, but OpenRouter accepts it and still
   ;; routes it under require_parameters. Measured 2026-09-11 on both providers:
   ;; "none" returns HTTP 200 with 0 reasoning tokens, so the Settings None works.
   ;; "low" and "high" did not differ, so any value other than "none" means on.
   {:id "ling-3.0-flash"
    :label "inclusionAI · Ling 3.0 Flash"
    :openrouter-model "inclusionai/ling-3.0-flash"
    :supported-parameters json-and-reasoning-parameters}])

(def retired
  "Entries withdrawn from the picker, kept because an :id is a durable settings
   value: `settings` rows and `credit_transactions.model` rows still name them.
   :superseded-by is the id a stored selection now RUNS.

   Retirement is not deletion, because every reader of a stored id already tests
   it against the allow-list built from `registry` and falls back on a miss — so
   a bare deletion would silently move a card-lane user from Gemini 3.5 Flash to
   whatever `default-id` happens to be, a different vendor at a different price.
   `effective-id` maps the stored id onto the intended successor instead, which
   is what makes this a retirement rather than a fallback. It also means no
   settings migration and no DB write: the redirect is read-side and total.

   These entries carry NO :openrouter-model, so a path that reaches one without
   going through `resolve-model` cannot call the retired model by accident."
  [{:id "gemini-3.5-flash"
    :label "Google · Gemini 3.5 Flash"
    :superseded-by "gemini-3.6-flash"}])

(def ^:private supersessions
  (into {} (map (juxt :id :superseded-by)) retired))

(defn effective-id
  "The id a stored selection names TODAY: `id` itself, the successor when `id`
   was retired, or `id` unchanged when it is unknown — an unknown id is for the
   caller's allow-list test to reject, not for this fn to invent a value for.
   Pre: id is a string or nil. Post: a string or nil."
  [id]
  (get supersessions id id))

(def default-id
  "System-wide default card model when a user has no saved selection and no prod
   pin applies. Matches the current !prod-model value."
  "gpt-5.1")

(def ^:private by-id
  (into {} (map (juxt :id identity)) registry))

(defn resolve-model
  "Registry entry to CALL for `id`, or nil when `id` is unknown/unset. A retired
   id resolves to its successor's entry, so `(:id (resolve-model id))` may differ
   from `id` — that is the model the request will name, and the one to bill and
   log against.
   Pre: id is a string or nil. Post: a registry map or nil — callers MUST handle
   nil (treat as: fall back to the default)."
  [id]
  (get by-id (effective-id id)))

(def ^:private native-web-search-model-ids
  "Assistant models that accept OpenRouter's native web-search tool.
   This is an allow-list because OpenRouter rejects the tool for models that do
   not support it. Keep it with the user-selectable model catalog."
  #{"gpt-5.1"
    "gpt-5.6-luna"
    "gpt-5.6-terra"
    "gpt-5.6-sol"
    "gemini-3-flash"
    "gemini-3.6-flash"
    "gemini-3.8-flash"
    "gemini-3.5-flash-lite"
    "claude-haiku-4.5"
    "claude-sonnet-5"})

(defn native-web-search-supported?
  "True when the effective model accepts the native web-search tool.
   Pre: `id` is a stored model id or nil. Post: false for unknown and retired
   models that do not resolve to an allow-listed current model."
  [id]
  (some-> id resolve-model :id native-web-search-model-ids boolean))

(defn web-search-supported?
  "True when the effective model can use OpenRouter's web-search server tool.
   OpenRouter uses native search where the provider supports it. Otherwise it
   uses its configured fallback search engine."
  [id]
  (boolean (resolve-model id)))
