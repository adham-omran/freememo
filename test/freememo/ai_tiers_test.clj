(ns freememo.ai-tiers-test
  "AI levels: the preset table, level resolution, and the settings getters and
   writers that honour a level. Runs without PostgreSQL: the settings table is an
   atom behind with-redefs."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.ai-tiers :as ai-tiers]
            [freememo.card-models :as card-models]
            [freememo.config :as config]
            [freememo.db :as db]
            [freememo.ocr-models :as ocr-models]
            [freememo.settings :as settings]))

(defn- with-store*
  "Run `f` with the settings table replaced by an atom seeded from `rows`.
   Returns [result final-rows]."
  [rows f]
  (let [!rows (atom rows)]
    (with-redefs [config/credits-enabled? (constantly false)
                  db/get-setting (fn [_ k] (get @!rows k))
                  db/get-settings (fn [_ ks] (select-keys @!rows ks))
                  db/set-setting (fn [_ k v] (swap! !rows assoc k v) nil)
                  db/set-settings! (fn [_ kvs] (swap! !rows merge kvs) nil)]
      [(f) @!rows])))

(defmacro ^:private with-store [rows & body]
  `(first (with-store* ~rows (fn [] ~@body))))

(deftest the-level-keys-match-the-settings-constants
  (is (= settings/MODEL (first (ai-tiers/task-settings :cards))))
  (is (= settings/REASONING (second (ai-tiers/task-settings :cards))))
  (is (= settings/ASSISTANT_MODEL (first (ai-tiers/task-settings :assistant))))
  (is (= settings/ASSISTANT_REASONING (second (ai-tiers/task-settings :assistant))))
  (is (= settings/SCAN_DPI (second (ai-tiers/task-settings :scan))))
  (testing "every KG step setting belongs to exactly one task"
    (is (= (set (map :setting settings/kg-model-steps))
           (set (concat (ai-tiers/task-settings :quiz)
                        (ai-tiers/task-settings :kg)))))))

(deftest every-level-sets-every-key-of-its-task-to-a-real-value
  (let [card-ids (set (map :id card-models/registry))
        ocr-ids (set (map :id ocr-models/registry))]
    (doseq [task ai-tiers/tasks
            level ai-tiers/levels
            k (ai-tiers/task-settings task)
            :let [v (ai-tiers/preset-value task level k)]]
      (testing (str task " " level " " k)
        (is (string? v))
        (cond
          (= k "ocr_model_default") (is (ocr-ids v))
          (= k "scan_dpi") (is (<= 72 (parse-long v) 300))
          (re-find #"reasoning" k) (is (#{"low" "medium" "high"} v))
          :else (is (card-ids v)))))))

(deftest a-task-level-comes-from-its-row-then-its-settings-then-the-default
  (testing "a stored level wins"
    (is (= "best" (ai-tiers/task-level {"ai_tier_cards" "best" "model" "gpt-5.1"} :cards))))
  (testing "no level row, but a setting row: the user chose settings before levels"
    (is (= "custom" (ai-tiers/task-level {"model" "gpt-5.1"} :cards))))
  (testing "nothing stored"
    (is (= ai-tiers/default-level (ai-tiers/task-level {} :cards))))
  (testing "an unknown stored value is ignored"
    (is (= ai-tiers/default-level (ai-tiers/task-level {"ai_tier_cards" "turbo"} :cards)))))

(deftest simple-shows-a-level-only-when-every-task-holds-it
  (is (= "best" (ai-tiers/uniform-level (zipmap ai-tiers/tasks (repeat "best")))))
  (is (nil? (ai-tiers/uniform-level (assoc (zipmap ai-tiers/tasks (repeat "best")) :kg "economy"))))
  (is (nil? (ai-tiers/uniform-level (zipmap ai-tiers/tasks (repeat "custom"))))))

(deftest a-new-user-runs-the-default-level
  (with-store {}
    (let [p #(ai-tiers/preset-value %1 ai-tiers/default-level %2)]
      (is (= (p :cards "model") (settings/get-model 1)))
      (is (= (p :cards "reasoning") (settings/get-reasoning 1)))
      (is (= (p :assistant "assistant_model") (settings/get-assistant-model 1)))
      (is (= (p :assistant "assistant_reasoning") (settings/get-assistant-reasoning 1)))
      (is (= (p :quiz "kg_grade_model") (settings/get-kg-model 1 :grade)))
      (is (= (p :kg "kg_extract_model") (settings/get-kg-model 1 :extract)))
      (is (= (p :scan "ocr_model_default") (settings/get-ocr-model-default 1)))
      (is (= (parse-long (p :scan "scan_dpi")) (settings/get-scan-dpi 1))))))

(deftest a-user-with-settings-from-before-levels-keeps-them
  (with-store {"model" "claude-sonnet-5" "reasoning" "high" "scan_dpi" "72"}
    (is (= "claude-sonnet-5" (settings/get-model 1)))
    (is (= "high" (settings/get-reasoning 1)))
    (is (= 72 (settings/get-scan-dpi 1)))
    (testing "the assistant shared the card reasoning row, and still reads it"
      (is (= "high" (settings/get-assistant-reasoning 1))))
    (testing "so the assistant is custom, not on a level"
      (is (= "custom" (settings/get-ai-tier 1 :assistant))))
    (testing "a task with no rows of its own is on the default level"
      (is (= ai-tiers/default-level (settings/get-ai-tier 1 :kg))))))

(deftest a-named-level-overrides-an-older-row
  (with-store {"ai_tier_cards" "best" "model" "claude-sonnet-5"}
    (is (= "gpt-6-sol" (settings/get-model 1)))
    (is (= "medium" (settings/get-reasoning 1)))))

(deftest an-advanced-edit-keeps-the-rest-of-the-level
  (let [[_ rows] (with-store* {"ai_tier_cards" "best" "model" "claude-sonnet-5"}
                   #(settings/save-reasoning 1 "high"))]
    (is (= "custom" (get rows "ai_tier_cards")))
    (is (= "high" (get rows "reasoning")))
    (is (= "gpt-6-sol" (get rows "model"))
        "the model the level was giving, not the row from before levels"))
  (testing "an edit to a custom task writes only that key"
    (let [[_ rows] (with-store* {"ai_tier_cards" "custom" "model" "claude-sonnet-5"}
                     #(settings/save-reasoning 1 "high"))]
      (is (= {"ai_tier_cards" "custom" "model" "claude-sonnet-5" "reasoning" "high"}
             (select-keys rows ["ai_tier_cards" "model" "reasoning"])))))
  (testing "a KG step edit makes only its own task custom"
    (let [[_ rows] (with-store* (zipmap (map ai-tiers/level-key ai-tiers/tasks) (repeat "economy"))
                     #(settings/save-kg-model 1 :grade "gpt-6-sol"))]
      (is (= "custom" (get rows "ai_tier_quiz")))
      (is (= "economy" (get rows "ai_tier_kg"))))))

(deftest an-edit-to-one-task-does-not-move-another
  (let [[_ rows] (with-store* {} #(settings/save-reasoning 1 "high"))]
    (testing "the shared reasoning row would read as a custom assistant"
      (is (= ai-tiers/default-level (get rows "ai_tier_assistant"))))
    (is (= (zipmap ai-tiers/tasks (repeat ai-tiers/default-level))
           (assoc (with-store rows (settings/get-ai-tiers 1)) :cards ai-tiers/default-level)))
    (is (= (ai-tiers/preset-value :assistant ai-tiers/default-level "assistant_reasoning")
           (with-store rows (settings/get-assistant-reasoning 1))))))

(deftest simple-sets-every-task-at-once
  (let [[r rows] (with-store* {"ai_tier_cards" "custom"}
                   #(settings/save-ai-tier-all 1 "economy"))]
    (is (:success r))
    (is (= (zipmap ai-tiers/tasks (repeat "economy"))
           (with-store rows (settings/get-ai-tiers 1))))))

(deftest an-unknown-level-is-refused
  (with-store {}
    (is (false? (:success (settings/save-ai-tier 1 :cards "turbo"))))
    (is (false? (:success (settings/save-ai-tier 1 :nope "best"))))
    (is (false? (:success (settings/save-ai-tier-all 1 "custom"))))))
