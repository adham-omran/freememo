(ns freememo.assistant-budget
  "How large an assistant request may be, and what to compact when it is too
   large (plans/assistant-chat-forks.md §4.5.2, §4.5.3). Pure: the caller
   supplies the model's context length and the sizes of the parts.

   The size of a request is an estimate, characters ÷ 4. The estimate is not a
   tokenizer, so the budget keeps a margin below the model's context length
   and a reserve for the answer. A request the estimate passes can still
   overflow. `freememo.assistant` then compacts once and sends again (§4.5.6).")

(def chars-per-token
  "The estimate's divisor."
  4)

(def context-margin
  "The fraction of a model's context length a request may use before the
   output reserve. Proposed in §4.5.2. The user has not confirmed it."
  0.9)

(def output-reserve-tokens
  "Tokens kept free for the answer. Proposed in §4.5.2. The user has not
   confirmed it."
  8000)

(def fallback-context-length
  "The context length assumed for slicing a compaction when the model's own
   length is unknown. The smallest context in the model registry is about
   131K tokens (`assistant/max-repo-file-chars`), so a slice sized for this
   fits every registry model."
  128000)

(defn tokens
  "The estimated tokens of `chars` characters."
  [chars]
  (long (Math/ceil (/ (double chars) chars-per-token))))

(defn content-chars
  "The characters of one message's content: a string, or a vector of text
   parts as `with-cache-breakpoint` makes."
  [content]
  (if (string? content)
    (count content)
    (reduce + 0 (map #(count (str (:text %))) content))))

(defn request-tokens
  "The estimated tokens of a provider request's `messages`."
  [messages]
  (tokens (reduce + 0 (map (comp content-chars :content) messages))))

(defn budget-tokens
  "The most tokens a request may estimate at for a model with
   `context-length`.
   Post: nil when the length is unknown, which means no request is refused
         before a send. Never negative."
  [context-length]
  (when (and (integer? context-length) (pos? context-length))
    (max 0 (- (long (* context-length context-margin)) output-reserve-tokens))))

(def fallback-budget-tokens
  "The budget of `fallback-context-length`."
  (budget-tokens fallback-context-length))

(defn fits?
  "True when `messages` estimate within `budget`, or when the budget is
   unknown."
  [messages budget]
  (or (nil? budget) (<= (request-tokens messages) budget)))

(defn slices
  "Split `rows` into runs of consecutive rows, oldest first, where each run
   estimates at most `slice-budget` tokens.

   Pre:  each row is {:id :chars}, in id order.
   Post: a vector of [first-id last-id] pairs that covers every row once, in
         order. A row larger than the budget is a slice of its own, because a
         message cannot be split."
  [rows slice-budget]
  (loop [out [] current [] total 0 [row & more :as remaining] rows]
    (if-not row
      (mapv (juxt (comp :id first) (comp :id last))
        (cond-> out (seq current) (conj current)))
      (let [cost (tokens (:chars row))]
        (if (or (empty? current) (<= (+ total cost) slice-budget))
          (recur out (conj current row) (+ total cost) more)
          (recur (conj out current) [] 0 remaining))))))

(defn compaction-plan
  "The compaction targets for an answer request that does not fit, in the
   order they are tried (§4.5.3, D5).

   1. Each included branch, stalest first by last activity. A branch's own
      rows are compacted, never what it inherited.
   2. The chat's inherited history, which becomes an `inherited` checkpoint.
   3. The chat's own history.

   Pre:  `branches` is a seq of {:chat-id :activity-ms :rows}. `inherited` and
         `own` are {:chat-id :rows}, or nil. Every :rows is the {:id :chars}
         rows that the request still sends, after any checkpoint.
         `slice-budget` is the most tokens one compaction request's rows may
         estimate at.
   Post: a vector of {:target :chat-id :slices}, with :target one of :branch,
         :inherited and :own. A part with no rows is not a target. The caller
         compacts one target, measures the request again, and stops once it
         fits, because a summary's size is known only after it is written."
  [{:keys [branches inherited own slice-budget]}]
  (let [target (fn [kind {:keys [chat-id rows]}]
                 (when (seq rows)
                   {:target kind :chat-id chat-id
                    :slices (slices rows slice-budget)}))]
    (into []
      (keep identity)
      (concat
        (map #(target :branch %) (sort-by (juxt :activity-ms :chat-id) branches))
        [(target :inherited inherited)
         (target :own own)]))))
