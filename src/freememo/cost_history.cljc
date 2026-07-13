(ns freememo.cost-history
  "Settings → Account 'AI costs' card: the user's credit ledger as a searchable,
   filterable, virtual-scrolled table. Read-only view over credit_transactions,
   scoped to the current user. Mirrors search-page's Scroll-window + Tape
   windowing — the full filtered result set stays server-side; only the visible
   window rows + the count cross the wire. Extracted into its own file (like
   storage-section / ai-features-section) to keep each e/defn under the JVM 64KB
   bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.scroll :refer [Scroll-window]]
   [freememo.card-models :as card-models]
   [freememo.ocr-models :as ocr-models]
   [clojure.string :as str]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.credits :as credits])
   #?(:clj [freememo.user-state :as us])))

;; ── Display labels ──────────────────────────────────────────────────────────

(def endpoint-labels
  "Friendly names for known billing endpoints; the raw string is the fallback."
  {"cards.basic"       "Basic cards"
   "cards.cloze"       "Cloze cards"
   "cards.overlapping" "Overlapping cards"
   "cards.fact-select" "Fact selection"
   "assistant.chat"    "Assistant chat"
   "kg.grade"          "KG grading"
   "kg.distill"        "KG distill"
   "kg.questions"      "KG questions"
   "ocr.extract"       "OCR extract"})

(def ^:private model-labels
  "model id → label, unioned across the card + OCR registries (ledger `model`
   ids are drawn from these)."
  (into {} (map (juxt :id :label)) (concat card-models/registry ocr-models/registry)))

(defn endpoint-label [ep] (get endpoint-labels ep (or ep "—")))
(defn model-label [m] (get model-labels m (or m "—")))

(defn kind-badge
  "[label bg-color-var] for a ledger kind."
  [kind]
  (case kind
    "debit"      ["Debit"    "var(--color-badge-pdf)"]
    "grant"      ["Grant"    "var(--color-badge-epub)"]
    "purchase"   ["Purchase" "var(--color-badge-web)"]
    "adjustment" ["Adjust"   "var(--color-badge-web)"]
    [(or kind "—") "var(--color-badge-web)"]))

(def date-presets
  "Ordered [value label days] for the date filter; days nil = all time."
  [["all" "All time"     nil]
   ["7d"  "Last 7 days"  7]
   ["30d" "Last 30 days" 30]
   ["90d" "Last 90 days" 90]])

(def ^:private preset->days
  (into {} (map (fn [[v _ d]] [v d])) date-presets))

;; ── Server reads (plain defns w/ reader conditional so :refer works in cljs) ──

(defn list-transactions*
  "Server read enriched for display: adds :when-str (created_at → 'YYYY-MM-DD
   HH:MM') and :usd-str (USD approx of |amount|), and drops the raw timestamp so
   no java.sql.Timestamp crosses the wire. `_refresh` (a :credits-refresh watch)
   forces a re-query on bump. `filters` is the map from list-credit-transactions."
  [_refresh user-id filters]
  #?(:clj (mapv (fn [r]
                  (-> r
                    (assoc :when-str (when-let [ts (:created_at r)]
                                       (let [s (str/replace (str ts) \T \space)]
                                         (subs s 0 (min 16 (count s)))))
                           :usd-str (credits/iqd->usd-str (Math/abs (long (:amount_iqd r)))))
                    (dissoc :created_at)))
            (db/list-credit-transactions user-id filters))
     :cljs []))

(defn facets* [_refresh user-id]
  #?(:clj (db/credit-transaction-facets user-id)
     :cljs {:endpoints [] :models []}))

;; ── Row + section ───────────────────────────────────────────────────────────

(defn- cell-style
  "Shared td style; `align` :start (text) or :end (numeric, tabular)."
  [align]
  {:padding "4px 10px" :overflow "hidden" :white-space "nowrap"
   :text-overflow "ellipsis" :display "flex" :align-items "center"
   :justify-content (if (= align :end) "flex-end" "flex-start")
   :font-size "13px" :color "var(--color-text-primary)"
   :font-variant-numeric (when (= align :end) "tabular-nums")})

(e/defn TxRow [row i row-height]
  (e/client
    (let [{:keys [kind endpoint model amount_iqd balance_after
                  input_tokens output_tokens cached_tokens when-str usd-str]} row
          [badge-text badge-color] (kind-badge kind)
          debit? (neg? (or amount_iqd 0))
          toks (str (or input_tokens 0) "/" (or output_tokens 0) "/" (or cached_tokens 0))]
      (dom/tr
        (dom/props {:class (when (odd? i) "row-alt")
                    :style {:height (str row-height "px") :--order i}})
        (dom/td (dom/props {:style {:display "flex" :align-items "center" :padding "4px 10px"}})
          (dom/span (dom/props {:class "type-badge" :style {:background badge-color}})
            (dom/text badge-text)))
        (dom/td (dom/props {:style (cell-style :start)}) (dom/text (or when-str "")))
        (dom/td (dom/props {:style (cell-style :start) :title (or endpoint "")})
          (dom/text (endpoint-label endpoint)))
        (dom/td (dom/props {:style (cell-style :start) :title (or model "")})
          (dom/text (model-label model)))
        (dom/td (dom/props {:style (assoc (cell-style :end)
                                     :color (if debit? "var(--color-danger)" "var(--color-success)"))
                            :title (or usd-str "")})
          (dom/text (str amount_iqd)))
        (dom/td (dom/props {:style (cell-style :end)}) (dom/text (str balance_after)))
        (dom/td (dom/props {:style (cell-style :end) :title "input / output / cached tokens"})
          (dom/text (if (= toks "0/0/0") "—" toks)))))))

(e/defn CostHistorySection
  "AI costs card for the Account settings tab. Read-only; user-scoped."
  [user-id]
  (e/client
    (let [!kind (atom "")     kind (e/watch !kind)
          !endpoint (atom "") endpoint (e/watch !endpoint)
          !model (atom "")    model (e/watch !model)
          !preset (atom "all") preset (e/watch !preset)
          !q (atom "")        q (e/watch !q)
          refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          facets (e/server (facets* refresh user-id))
          endpoints (:endpoints facets)
          models (:models facets)
          filters {:kind kind :endpoint endpoint :model model :q q
                   :since-days (get preset->days preset)}
          results (e/server (e/Offload #(list-transactions* refresh user-id filters)))
          result-count (e/server (count results))
          loading? (nil? result-count)
          rc (or result-count 0)
          reset-key [kind endpoint model preset q]
          row-height 36
          min-width "830px"
          grid-cols "90px 150px minmax(120px,1fr) minmax(120px,1fr) 110px 110px 130px"]
      (dom/div
        (dom/props {:class "card"})
        (dom/h3 (dom/props {:class "section-title"}) (dom/text "AI costs"))
        (dom/div (dom/props {:class "hint" :style {:margin-bottom "10px"}})
          (dom/text "Your AI usage charges and top-ups. Amounts are in credits (IQD); hover an amount for the USD approximation."))

        ;; ── Filter row ──
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :flex-wrap "wrap" :margin-bottom "10px"}})
          (dom/select (dom/props {:class "input" :aria-label "Kind"})
            (dom/option (dom/props {:value ""}) (dom/text "All kinds"))
            (dom/option (dom/props {:value "debit"}) (dom/text "Debits"))
            (dom/option (dom/props {:value "grant"}) (dom/text "Grants"))
            (dom/option (dom/props {:value "purchase"}) (dom/text "Purchases"))
            (set! (.-value dom/node) kind)
            (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil))
          (dom/select (dom/props {:class "input" :aria-label "Feature"})
            (dom/option (dom/props {:value ""}) (dom/text "All features"))
            (e/for [ep (e/diff-by identity endpoints)]
              (dom/option (dom/props {:value ep}) (dom/text (endpoint-label ep))))
            (set! (.-value dom/node) endpoint)
            (dom/On "change" (fn [e] (reset! !endpoint (-> e .-target .-value))) nil))
          (dom/select (dom/props {:class "input" :aria-label "Model"})
            (dom/option (dom/props {:value ""}) (dom/text "All models"))
            (e/for [m (e/diff-by identity models)]
              (dom/option (dom/props {:value m}) (dom/text (model-label m))))
            (set! (.-value dom/node) model)
            (dom/On "change" (fn [e] (reset! !model (-> e .-target .-value))) nil))
          (dom/select (dom/props {:class "input" :aria-label "Date range"})
            (e/for [[v label _] (e/diff-by first date-presets)]
              (dom/option (dom/props {:value v}) (dom/text label)))
            (set! (.-value dom/node) preset)
            (dom/On "change" (fn [e] (reset! !preset (-> e .-target .-value))) nil))
          (dom/input (dom/props {:type "text" :class "input"
                                 :placeholder "Search feature or model…"
                                 :style {:flex "1" :min-width "160px"}})
            (set! (.-value dom/node) q)
            (dom/On "input" (fn [e] (reset! !q (-> e .-target .-value))) nil))
          (when loading?
            (dom/span
              (dom/props {:style {:display "inline-flex" :align-items "center"
                                  :color "var(--color-text-secondary)" :font-size "13px"}})
              (dom/span (dom/props {:class "spinner"}))
              (dom/text "Loading…"))))

        ;; ── Table (header + virtual-scrolled body share one h-scroll wrapper) ──
        (dom/div
          (dom/props {:style {:overflow-x "auto"}})
          (dom/table
            (dom/props {:style {:width "100%" :min-width min-width :display "grid"
                                :grid-template-columns grid-cols}})
            (dom/thead (dom/props {:style {:display "contents"}})
              (dom/tr (dom/props {:style {:display "contents"}})
                (let [th-style {:padding "8px 10px" :border-bottom "2px solid var(--color-border)"
                                :font-weight "600" :font-size "12px"
                                :color "var(--color-text-primary)" :text-align "left"}]
                  (dom/th (dom/props {:style th-style}) (dom/text "Kind"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Date"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Feature"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Model"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Amount"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Balance"))
                  (dom/th (dom/props {:style th-style}) (dom/text "Tokens"))))))
          (dom/div
            (dom/props {:class "tape-scroll"
                        :style {:max-height "480px" :overflow-y "auto"
                                :scrollbar-gutter "stable"
                                :--count rc :--grid-cols grid-cols
                                :--row-height (str row-height "px")}})
            (let [[offset limit] (Scroll-window row-height rc dom/node
                                   {:overquery-factor 2 :reset-key reset-key})]
              (dom/table
                (dom/props {:style {:width "100%" :min-width min-width :font-size "13px"}})
                (e/for [i (Tape offset limit)]
                  (let [row (e/server (nth results i nil))]
                    (when row
                      (TxRow row i row-height))))))))

        ;; ── Empty state (table stays mounted; message is a sibling) ──
        (when (and (not loading?) (= rc 0))
          (dom/div
            (dom/props {:style {:padding "24px" :text-align "center"
                                :color "var(--color-text-secondary)" :font-size "13px"}})
            (dom/text "No transactions match these filters.")))))))
