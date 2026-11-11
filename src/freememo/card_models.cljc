(ns freememo.card-models
  "Catalog of user-selectable models for flashcard generation.

   A selection is stored per-user as an :id (see settings/get-model); the prod
   default is freememo.config/!prod-model. All lanes route through one OpenRouter
   key (like OCR, topology A1) and bill from OpenRouter's returned usage.cost.

   These :ids are card-lane-only and intentionally differ from
   freememo.ocr-models ids for the same underlying model — the two registries are
   independent (design decision D1-B).")

(def ^:private json-and-reasoning-parameters
  "OpenRouter parameters this application sends to a structured model that has
   no structured-output provider. This set is the application-relevant subset
   of the model metadata."
  #{:reasoning_effort :response_format})

(def ^:private schema-json-and-reasoning-parameters
  "As above, plus `structured_outputs`: a provider can enforce a JSON schema
   (`openrouter/with-response-schema`). Recorded on 2026-11-03 from each
   entry's OpenRouter endpoints, inside its :provider limits."
  (conj json-and-reasoning-parameters :structured_outputs))

(def ^:private schema-json-reasoning-and-verbosity-parameters
  (conj schema-json-and-reasoning-parameters :verbosity))

(def registry
  "Ordered for display. :id is the durable settings value — never rename it,
   stored rows reference it. :openrouter-model is the slug sent to OpenRouter.
   :provider, when present, is OpenRouter's provider-routing request object,
   sent verbatim by openrouter/model-fields — the only way a lane names a
   provider. Absent means OpenRouter picks the provider.
   :supported-parameters is the application-relevant subset of OpenRouter's
   model-level parameter metadata, recorded on 2026-09-01. Structured lanes
   require :response_format. :structured_outputs was recorded on 2026-11-03. Request builders omit unsupported optional fields.
   :reasoning-mandatory? marks a model whose provider REFUSES to have reasoning
   turned off — OpenRouter's `reasoning.mandatory`, recorded 2026-09-08. Sending
   it reasoning_effort \"none\" returns HTTP 400 \"Reasoning is mandatory for this
   endpoint and cannot be disabled\", so openrouter/chat-completion-fields drops
   the field for these entries. Every other unsupported effort value is clamped
   by OpenRouter instead of refused (measured), so this flag covers the whole
   failure and no per-value table is needed.
   :usd-per-mtok is OpenRouter's list price in USD per million input and output
   tokens, read from /api/v1/models on 2026-11-03. It feeds only the cost
   estimate shown before an exam is generated (assessment/estimated-cost). The
   ledger charges usage.cost, which a provider route can make lower or higher."
  [;; GPT-6.1 Sol replaced GPT-6 Sol on 2026-10-26. On OpenRouter that day both
   ;; cost $2 / $10 per million tokens, and 6.1 cached input at $0.10 against
   ;; $0.20. minimaxir.com/2026/10/cheap-llms calls GPT-6 Sol "a dud" and 6.1
   ;; comparable to GPT-6 Astra, at a fifth of Astra's price. Nobody measured
   ;; 6.1 on card generation before the switch.
   {:id "gpt-6.1-sol"
    :label "GPT-6.1 Sol (OpenAI)"
    :openrouter-model "openai/gpt-6.1-sol"
    :usd-per-mtok {:input 2.0 :output 10.0}
    :supported-parameters schema-json-and-reasoning-parameters}
   ;; GPT-6 Luna, added 2026-09-23 at $0.10 / $0.50, half of GPT-5.6 Luna. The
   ;; GPT-5.x entries in `retired` redirect to the GPT-6 line by product
   ;; decision (2026-09-23), not because the vendor names GPT-6 their
   ;; successor: that statement could not be read (HTTP 403). Luna and GPT-6
   ;; Sol accepted `openrouter:web_search` with engine "native" on 2026-09-23.
   {:id "gpt-6-luna"
    :label "GPT-6 Luna (OpenAI)"
    :openrouter-model "openai/gpt-6-luna"
    :usd-per-mtok {:input 0.1 :output 0.5}
    :supported-parameters schema-json-and-reasoning-parameters}
   ;; $0.75 / $3.75 per million tokens. Gemini 3.6 Flash and Gemini 3 Flash
   ;; Preview were retired from the picker on 2026-09-23 and redirect here (see
   ;; `retired`).
   ;;
   ;; It is also the SLOWEST of the three at that price. OpenRouter p50 over a
   ;; 30-minute window on 2026-09-08, traffic-weighted across endpoints: 3.8 is
   ;; 2.27 s to first token at 67 tok/s, 3.7 is 2.26 s at 96, and 3.6 is 1.58 s
   ;; at 104 — so an 800-token card batch takes ~14 s on 3.8 against ~9 s on 3.6.
   ;; Benchmarks and speed disagree here; pick per lane, and re-measure before
   ;; trusting these numbers, which move.
   {:id "gemini-3.8-flash"
    :label "Gemini 3.8 Flash (Google)"
    :openrouter-model "google/gemini-3.8-flash"
    :usd-per-mtok {:input 0.825 :output 4.125}
    :supported-parameters schema-json-and-reasoning-parameters
    :reasoning-mandatory? true}
   {:id "gemini-3.5-flash-lite"
    :label "Gemini 3.5 Flash-Lite (Google)"
    :openrouter-model "google/gemini-3.5-flash-lite"
    :usd-per-mtok {:input 0.3 :output 2.5}
    :supported-parameters schema-json-and-reasoning-parameters
    :reasoning-mandatory? true}
   {:id "claude-sonnet-5"
    :label "Claude Sonnet 5 (Anthropic)"
    :openrouter-model "anthropic/claude-sonnet-5"
    :usd-per-mtok {:input 2.0 :output 10.0}
    :supported-parameters schema-json-reasoning-and-verbosity-parameters}
   ;; A dated snapshot, not an alias. OpenRouter publishes no `-latest` slug for
   ;; this model, and the one written here earlier was rejected with
   ;; "deepseek/deepseek-v4-flash-latest is not a valid model ID" — an HTTP 400
   ;; on every call in every lane that selected it. Verify a slug against
   ;; https://openrouter.ai/api/v1/models before shipping it; this one retires
   ;; when DeepSeek withdraws the snapshot.
   ;;
   ;; V4.1 Flash 1017 replaced V4 Flash 0731 on 2026-10-26. Artificial Analysis
   ;; scores it 45 against 24 for the April V4 Flash. It is NOT cheaper: on
   ;; 2026-10-26 its ~35 endpoints listed $0.02–0.33 in and $0.15–1.32 out,
   ;; against $0.05–0.15 and $0.16–0.41 for 0731. DeepSeek's own endpoint is
   ;; $0.15 / $0.60. No endpoint reported mandatory reasoning. The model reads
   ;; images too, but the OCR registry has no entry for it yet.
   {:id "deepseek-v4.1-flash"
    :label "V4.1 Flash (DeepSeek)"
    :openrouter-model "deepseek/deepseek-v4.1-flash-1017"
    :usd-per-mtok {:input 0.15 :output 0.6}
    :supported-parameters schema-json-and-reasoning-parameters}
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
    :label "GLM 5.3 Flash (Z.ai)"
    :openrouter-model "z-ai/glm-5.3-flash"
    :usd-per-mtok {:input 0.0313 :output 0.0846}
    :supported-parameters schema-json-and-reasoning-parameters
    :reasoning-mandatory? true
    :provider {:sort "throughput"
               :max_price {:prompt 0.15 :completion 0.50}}}])

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
    :label "Gemini 3.5 Flash (Google)"
    ;; Pointed at 3.6 until 3.6 itself retired. effective-id takes one hop,
    ;; so a retired successor must be followed by hand here.
    :superseded-by "gemini-3.8-flash"}
   ;; Removed from the picker on 2026-09-23 by product decision. Each points
   ;; at the nearest current model of its vendor and tier. The Sol-tier ones
   ;; pointed at gpt-6-sol until it retired; effective-id takes one hop.
   {:id "gpt-5.1"
    :label "GPT-5.1 (OpenAI)"
    :superseded-by "gpt-6.1-sol"}
   {:id "gpt-5.6-luna"
    :label "GPT-5.6 Luna (OpenAI)"
    :superseded-by "gpt-6-luna"}
   {:id "gpt-5.6-terra"
    :label "GPT-5.6 Terra (OpenAI)"
    :superseded-by "gpt-6.1-sol"}
   {:id "gpt-5.6-sol"
    :label "GPT-5.6 Sol (OpenAI)"
    :superseded-by "gpt-6.1-sol"}
   {:id "gemini-3-flash"
    :label "Gemini 3 Flash (Google)"
    :superseded-by "gemini-3.8-flash"}
   {:id "gemini-3.6-flash"
    :label "Gemini 3.6 Flash (Google)"
    :superseded-by "gemini-3.8-flash"}
   {:id "claude-haiku-4.5"
    :label "Claude Haiku 4.5 (Anthropic)"
    :superseded-by "claude-sonnet-5"}
   ;; Removed from the picker on 2026-10-26: each has a successor of the same
   ;; vendor that scores higher. See the registry entries.
   {:id "gpt-6-sol"
    :label "GPT-6 Sol (OpenAI)"
    :superseded-by "gpt-6.1-sol"}
   {:id "deepseek-v4-flash"
    :label "V4 Flash (DeepSeek)"
    :superseded-by "deepseek-v4.1-flash"}
   ;; Removed from the picker on 2026-11-03. Its structured lanes had a 131K
   ;; context and no provider with structured outputs, and every other entry
   ;; has about 1M tokens and a strict schema. GLM 5.3 Flash is the nearest
   ;; current price (0.075 / 0.25 against 0.06 / 0.18 per million tokens).
   {:id "ling-3.0-flash"
    :label "Ling 3.0 Flash (inclusionAI)"
    :superseded-by "glm-5.3-flash"}])

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
   pin applies. Matches the current !prod-model value, and the Best level."
  "gpt-6.1-sol")

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
   not support it. Keep it with the user-selectable model catalog.
   gpt-6.1-sol took gpt-6-sol's place here on 2026-10-26 without a measurement:
   its OpenRouter parameter metadata matches gpt-6-sol's."
  #{"gpt-6.1-sol"
    "gpt-6-luna"
    "gemini-3.8-flash"
    "gemini-3.5-flash-lite"
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

(def ^:private exa-web-search-model-ids
  "Models that must search with Exa, not with OpenRouter's \"auto\" engine.
   For Gemini, \"auto\" picks Google's native search. With tool_choice
   \"required\", that search ran for 75 to 87 s and then ended the stream with
   finish_reason \"error\", no text and $0 cost: 4 of 4 runs on
   gemini-3.8-flash, measured 2026-11-11. The same requests with Exa answered
   with sources. plans/arabic-corpus-and-ocr-findings.md records 0 of 9 for
   the same engine earlier."
  #{"gemini-3.8-flash"
    "gemini-3.5-flash-lite"})

(defn web-search-engine
  "The OpenRouter web-search engine for the effective model: \"exa\" for a
   model in `exa-web-search-model-ids`, else \"auto\".
   Post: nil when `id` resolves to no model, which has no web search."
  [id]
  (when-let [entry (resolve-model id)]
    (if (exa-web-search-model-ids (:id entry)) "exa" "auto")))

(def ^:private cache-breakpoint-model-ids
  "Models whose prompt cache needs an explicit `cache_control` breakpoint.
   Measured 2026-10-01 on gemini-3.8-flash: with no breakpoint, a repeated
   13,500-token prefix read 0 cached tokens on Vertex and on AI Studio. With
   one, it read the whole prefix. OpenRouter bills the request that creates the
   cache at 5 minutes of storage plus a cache read, so even a first turn costs
   less. claude-sonnet-5 also needs a breakpoint, but its write costs 1.25
   times the input price. See plans/assistant-request-prefix-cache.md §12."
  #{"gemini-3.8-flash"
    "gemini-3.5-flash-lite"})

(defn cache-breakpoint?
  "True when requests to the effective model should carry a `cache_control`
   breakpoint.
   Pre: `id` is a stored model id or nil. Post: false for an unknown id."
  [id]
  (some-> id resolve-model :id cache-breakpoint-model-ids boolean))
