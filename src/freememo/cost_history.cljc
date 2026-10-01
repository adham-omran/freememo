(ns freememo.cost-history
  "Settings → Usage 'AI costs' group: the user's credit ledger as a searchable,
   filterable, virtual-scrolled table. Read-only view over credit_transactions,
   scoped to the current user. The table is freememo.tape-table/TapeTable, the
   shared component: a header table above a capped scroll box, 15 mounted rows,
   and an empty-state overlay. The full filtered result stays server-side; only
   the count and each window row cross the wire. Extracted into its own file (like
   storage-section / ai-features-section) to keep each e/defn under the JVM 64KB
   bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.card-models :as card-models]
   [freememo.ocr-models :as ocr-models]
   [freememo.settings-group :as group]
   [freememo.tape-table :as tape]
   [freememo.util :as util]
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
   "cards.grade"       "Card grading"
   "assistant.chat"    "Assistant chat"
   "kg.grade"          "KG grading"
   "kg.distill"        "KG distill"
   "kg.questions"      "KG questions"
   "ocr.extract"       "OCR extract"
   "curriculum.exam-form"  "Exam form generation"
   "curriculum.exam-grade" "Exam grading"
   ;; One endpoint for both the audio kind and video: same model, same price,
   ;; same operation — the source kind is not a billing distinction, and two
   ;; labels would put "Transcription" in the Feature filter twice.
   "transcribe"        "Transcription"
   ;; Not an AI lane — storage rent, accrued by freememo.storage-meter.
   "storage"           "Storage"})

(def ^:private model-labels
  "model id → label, unioned across the card + OCR registries (ledger `model`
   ids are drawn from these). Retired entries are included because the ledger is
   history: a row charged to a model withdrawn since would otherwise render its
   raw id in a column every other row labels."
  (into {} (map (juxt :id :label))
    (concat card-models/registry card-models/retired
            ocr-models/registry ocr-models/retired)))

(defn endpoint-label [ep] (get endpoint-labels ep (or ep "—")))
(defn model-label [m] (get model-labels m (or m "—")))

(defn kind-badge
  "[label badge-class] for a ledger kind. A class, not an inline colour: the
   badge mounts in every window row, and a class is one prop (CLAUDE.md,
   \"Cell styling belongs in CSS\")."
  [kind]
  (case kind
    "debit"      ["Debit"    "type-badge type-badge-debit"]
    "grant"      ["Grant"    "type-badge type-badge-grant"]
    "purchase"   ["Purchase" "type-badge type-badge-purchase"]
    "adjustment" ["Adjust"   "type-badge type-badge-purchase"]
    [(or kind "—") "type-badge type-badge-purchase"]))

(def date-presets
  "Ordered [value label days] for the date filter; days nil = all time."
  [["all" "All time"     nil]
   ["7d"  "Last 7 days"  7]
   ["30d" "Last 30 days" 30]
   ["90d" "Last 90 days" 90]])

(def ^:private preset->days
  (into {} (map (fn [[v _ d]] [v d])) date-presets))

(defn parse-min-amount
  "Parse the min-amount input to a non-negative long, or nil for blank/invalid.
   Takes the magnitude, so a user-typed '-500' and '500' both mean
   |amount_iqd| >= 500."
  [s]
  #?(:cljs (let [n (js/parseInt (str s) 10)]
             (when-not (js/isNaN n) (js/Math.abs n)))
     :clj nil))

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

(def ^:private headers
  ["Kind" "Date" "Feature" "Model" "Amount" "Balance" "Tokens"])

(def ^:private grid-cols
  "Fixed tracks for the short columns, shares of the rest for the two text
   columns. No min-width: the table fits the card, and Feature and Model ellipsize
   with their full text in a tooltip, rather than the table scrolling sideways.
   Below 640 px, index.css replaces this with three columns (`.cost-history-table`)."
  "84px 132px minmax(0,1fr) minmax(0,1.3fr) 76px 80px 120px")

(e/defn TxRow
  "One ledger row. Layout is in `.tape-table-body` classes; only the amount's
   colour and the tooltips are data."
  [row i]
  (e/client
    (let [{:keys [kind endpoint model amount_iqd balance_after
                  input_tokens output_tokens cached_tokens when-str usd-str]} row
          [badge-text badge-class] (kind-badge kind)
          debit? (neg? (or amount_iqd 0))
          toks (str (or input_tokens 0) "/" (or output_tokens 0) "/" (or cached_tokens 0))]
      (dom/tr
        (dom/props {:class (when (odd? i) "row-alt") :style {:--order i}})
        (dom/td
          (dom/span (dom/props {:class badge-class}) (dom/text badge-text)))
        (dom/td (dom/props {:class "tape-cell-meta"}) (dom/text (or when-str "")))
        (dom/td
          (dom/span (dom/props {:class "tape-cell-text" :data-tooltip (or endpoint "")})
            (dom/text (endpoint-label endpoint))))
        (dom/td
          (dom/span (dom/props {:class "tape-cell-text" :data-tooltip (or model "")})
            (dom/text (model-label model))))
        (dom/td (dom/props {:class "tape-cell-num tape-cell-strong"
                            :style {:color (if debit? "var(--color-danger-text)"
                                                      "var(--color-success-dark)")}})
          (dom/span (dom/props {:data-tooltip (or usd-str "")})
            (dom/text (str amount_iqd))))
        (dom/td (dom/props {:class "tape-cell-num"}) (dom/text (str balance_after)))
        (dom/td (dom/props {:class "tape-cell-num tape-cell-meta"})
          (if (= toks "0/0/0")
            (dom/span (dom/props {:class "tape-cell-absent"}) (dom/text "—"))
            (dom/span (dom/props {:data-tooltip "input / output / cached tokens"})
              (dom/text toks))))))))
(e/defn CostHistorySection
  "AI costs group for the Usage settings tab. Read-only; user-scoped."
  [user-id]
  (e/client
    (let [!kind (atom "")     kind (e/watch !kind)
          !endpoint (atom "") endpoint (e/watch !endpoint)
          !model (atom "")    model (e/watch !model)
          !preset (atom "all") preset (e/watch !preset)
          !min-amount (atom "") min-amount (e/watch !min-amount)
          !q (atom "")        q (e/watch !q)
          ;; Debounced twins — the inputs' displayed values track !min-amount/!q
          ;; directly (instant keystrokes); the filters below track the
          ;; debounced pair, which only commit after a short idle so typing
          ;; doesn't fire a query per keystroke.
          !min-amount-debounced (atom "") min-amount-debounced (e/watch !min-amount-debounced)
          !min-amount-timer (atom nil)
          !q-debounced (atom "")        q-debounced (e/watch !q-debounced)
          !q-timer (atom nil)
          refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          facets (e/server (e/Offload #(facets* refresh user-id)))
          endpoints (:endpoints facets)
          models (:models facets)
          filters {:kind kind :endpoint endpoint :model model :q q-debounced
                   :since-days (get preset->days preset)
                   :min-amount (parse-min-amount min-amount-debounced)}
          results (e/server (e/Offload #(list-transactions* refresh user-id filters)))
          result-count (e/server (count results))
          loading? (nil? result-count)
          rc (or result-count 0)
          reset-key [kind endpoint model preset min-amount-debounced q-debounced]]
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "AI costs")
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
          (dom/input (dom/props {:type "number" :class "input" :min "0"
                                 :placeholder "Min amount"
                                 :aria-label "Minimum amount (IQD)"
                                 :style {:width "120px"}})
            (set! (.-value dom/node) min-amount)
            (dom/On "input"
              (fn [e]
                (let [v (-> e .-target .-value)]
                  (reset! !min-amount v)
                  (util/debounce! !min-amount-timer !min-amount-debounced v 300)))
              nil))
          (dom/input (dom/props {:type "text" :class "input"
                                 :placeholder "Search feature or model…"
                                 :style {:flex "1" :min-width "160px"}})
            (set! (.-value dom/node) q)
            (dom/On "input"
              (fn [e]
                (let [v (-> e .-target .-value)]
                  (reset! !q v)
                  (util/debounce! !q-timer !q-debounced v 300)))
              nil))
          (when loading?
            (dom/span
              (dom/props {:style {:display "inline-flex" :align-items "center"
                                  :color "var(--color-text-secondary)" :font-size "13px"}})
              (dom/span (dom/props {:class "spinner"}))
              (dom/text "Loading…"))))

        ;; ── Table: the shared TapeTable. It owns the header, the capped scroll
        ;;    box, the row window and the empty overlay; the caller owes it a flex
        ;;    column with min-height 0. ──
        (dom/div
          (dom/props {:class "cost-history-table"
                      :style {:display "flex" :flex-direction "column" :min-height "0"}})
          (tape/TapeTable headers grid-cols rc reset-key
            (if loading? "" "No transactions match these filters.")
            (e/fn [i]
              ;; One window row crosses the wire, never the whole result.
              (when-let [row (e/server (nth results i nil))]
                (TxRow row i)))))))))
