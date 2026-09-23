(ns freememo.ai-tiers
  "AI levels: one named level per AI task, resolved to concrete settings at read
   time. See plans/ai-settings-simple-advanced.md.

   A task's level is stored as a settings row, `ai_tier_<task>`, holding
   \"economy\", \"balanced\", \"best\" or \"custom\". A named level reads its
   values from `presets`, so one edit here moves every user on that level, with
   no migration. \"custom\" reads the task's own settings rows, which is what
   every setting read before levels existed.

   This namespace has NO requires, so both peers can read it. The Settings UI
   renders these literals on the client. A CLJ-only def read from client scope
   crashes the WebSocket instead of raising an error (CLAUDE.md, \"The same rule
   governs a plain `def` of data\").

   The setting keys below repeat constants from `freememo.settings`, because this
   namespace cannot require it. `ai_tiers_test` asserts that the two agree.")

(def levels
  "The named levels, cheapest first. Display order."
  ["economy" "balanced" "best"])

(def custom
  "The level of a task whose settings a user chose one by one in Advanced."
  "custom")

(def default-level
  "The level of a task that has no level row and no settings rows of its own.
   Best by product decision (2026-09-23): a new user gets the most careful
   results, and chooses a cheaper level in Settings."
  "best")

(def tasks
  "Every AI task that has a level. Display order."
  [:cards :scan :assistant :quiz :kg])

(def task-settings
  "The settings rows each task's level controls. A named level supplies a value
   for every key here; `custom` reads the rows themselves."
  {:cards     ["model" "reasoning"]
   :scan      ["ocr_model_default" "scan_dpi"]
   :assistant ["assistant_model" "assistant_reasoning"]
   :quiz      ["kg_grade_model"]
   :kg        ["kg_extract_model" "kg_link_model" "kg_atomic_question_model"
               "kg_synthesis_question_model" "kg_fact_select_model"]})

(def legacy-settings
  "Rows that mark a task custom without being among its `task-settings`.
   Before levels, card generation and the assistant shared the one `reasoning`
   row. A user who set it chose the assistant's reasoning too, so that row keeps
   the assistant custom, where `get-assistant-reasoning` still reads it."
  {:assistant ["reasoning"]})

(def presets
  "task → level → {setting-key value}.

   Measured 2026-09-23 on card generation: GLM 5.3 Flash low and high cost the
   same and reasoned 0–13 tokens, so a level boundary cannot sit between them.
   Per 5-card batch, GLM low : Gemini 3.8 Flash low : GPT-6 Sol medium cost
   1 : 5 : 20. The KG lanes send no reasoning parameter, so :quiz and :kg set
   models only. Scanning ids are `freememo.ocr-models` ids, which differ from the
   card ids for the same model."
  {:cards
   {"economy"  {"model" "glm-5.3-flash"    "reasoning" "low"}
    "balanced" {"model" "gemini-3.8-flash" "reasoning" "low"}
    "best"     {"model" "gpt-6-sol"        "reasoning" "medium"}}
   :scan
   {"economy"  {"ocr_model_default" "gemini-3.5-flash-lite" "scan_dpi" "150"}
    "balanced" {"ocr_model_default" "gemini-3.5-flash-lite" "scan_dpi" "300"}
    "best"     {"ocr_model_default" "gemini-3-flash"        "scan_dpi" "300"}}
   :assistant
   {"economy"  {"assistant_model" "glm-5.3-flash"    "assistant_reasoning" "low"}
    "balanced" {"assistant_model" "gemini-3.8-flash" "assistant_reasoning" "low"}
    "best"     {"assistant_model" "gpt-6-sol"        "assistant_reasoning" "medium"}}
   :quiz
   {"economy"  {"kg_grade_model" "glm-5.3-flash"}
    "balanced" {"kg_grade_model" "gemini-3.8-flash"}
    "best"     {"kg_grade_model" "gpt-6-sol"}}
   :kg
   {"economy"  {"kg_extract_model" "glm-5.3-flash"
                "kg_link_model" "glm-5.3-flash"
                "kg_atomic_question_model" "glm-5.3-flash"
                "kg_synthesis_question_model" "glm-5.3-flash"
                "kg_fact_select_model" "glm-5.3-flash"}
    "balanced" {"kg_extract_model" "gemini-3.8-flash"
                "kg_link_model" "gemini-3.8-flash"
                "kg_atomic_question_model" "gemini-3.8-flash"
                "kg_synthesis_question_model" "gemini-3.8-flash"
                "kg_fact_select_model" "gemini-3.8-flash"}
    "best"     {"kg_extract_model" "gpt-6-sol"
                "kg_link_model" "glm-5.3-flash"
                "kg_atomic_question_model" "glm-5.3-flash"
                "kg_synthesis_question_model" "gpt-6-sol"
                "kg_fact_select_model" "glm-5.3-flash"}}})

(def level-labels
  {"economy" "Economy" "balanced" "Balanced" "best" "Best" "custom" "Custom"})

(def level-prices
  {"economy" "$" "balanced" "$$" "best" "$$$"})

(def level-descriptions
  {"economy"  "The lowest cost. Good for everyday reading and cards."
   "balanced" "More careful results for a higher cost."
   "best"     "The most careful results. Costs the most."})

(def task-labels
  {:cards "Flashcards" :scan "Page scanning" :assistant "Reading assistant"
   :quiz "Quiz grading" :kg "Knowledge graph"})

(def task-descriptions
  {:cards     "Writes cards from your pages."
   :scan      "Reads the text of scanned PDF pages."
   :assistant "Answers questions beside the page you read."
   :quiz      "Grades the answers you type in a quiz."
   :kg        "Finds facts in a document and writes quiz questions."})

(defn level? [s] (boolean (some #{s} levels)))

(defn level-key
  "The settings key that stores `task`'s level."
  [task]
  (str "ai_tier_" (name task)))

(def ^:private setting->task
  (into {} (for [[task ks] task-settings, k ks] [k task])))

(defn task-of-setting
  "The task whose level controls settings key `k`, or nil."
  [k]
  (setting->task k))

(defn all-keys
  "Every settings key level resolution reads: each task's level key and the
   keys it controls."
  []
  (into (mapv level-key tasks)
    (distinct (mapcat #(concat (task-settings %) (legacy-settings %)) tasks))))

(defn task-level
  "`task`'s level from `rows`, a {settings-key value} map holding at least the
   task's level key and controlled keys.
   Post: a named level, or `custom`. A stored level wins. With no stored level, a
   task with any controlled or legacy row reads `custom`, so a user who chose
   settings before levels existed keeps them. Otherwise `default-level`."
  [rows task]
  (let [stored (get rows (level-key task))]
    (cond
      (or (level? stored) (= custom stored)) stored
      (some #(some? (get rows %)) (concat (task-settings task) (legacy-settings task))) custom
      :else default-level)))

(defn preset-value
  "The value a named `level` gives settings key `k` of `task`, or nil for
   `custom` or a key the task does not control."
  [task level k]
  (get-in presets [task level k]))

(defn uniform-level
  "The one named level every task holds in `level-map` ({task level}), or nil
   when the tasks differ or any is `custom`."
  [level-map]
  (let [ls (set (map #(get level-map %) tasks))]
    (when (and (= 1 (count ls)) (level? (first ls)))
      (first ls))))
