(ns freememo.assistant-context-usage
  "How full the open chat's context is, as the composer's meter shows it
   (plans/assistant-context-meter-and-slash-menu.md §1). Pure, so the server
   computes the count and the browser formats it from the same definitions.")

(defn used-tokens
  "The tokens the chat's context held after one answer run: the request the
   provider counted, plus the answer it wrote. Reasoning tokens are counted in
   the output but no later request carries them, so they are subtracted.
   Pre:  `run` holds the run's token columns, any of which may be nil.
   Post: nil when the provider reported no input count, else a count >= 0. A
         missing output or reasoning count reads as 0."
  [{:keys [input_tokens output_tokens reasoning_tokens]}]
  (when (some? input_tokens)
    (max 0 (- (+ input_tokens (or output_tokens 0)) (or reasoning_tokens 0)))))

(defn- tenths-text
  "`tenths` tenths as text, with no `.0`."
  [tenths]
  (let [whole (quot tenths 10)
        part (rem tenths 10)]
    (str whole (when (pos? part) (str "." part)))))

(defn count-text
  "A token count, short: 999, 1k, 101.6k, 1M, 1.2M.
   Pre:  `n` is an integer >= 0."
  [n]
  (let [k-tenths (quot (+ n 50) 100)
        m-tenths (quot (+ n 50000) 100000)]
    (cond
      (< n 1000) (str n)
      (< k-tenths 10000) (str (tenths-text k-tenths) "k")
      :else (str (tenths-text m-tenths) "M"))))

(defn usage-text
  "The meter's text, such as `101.6k / 1M tokens`.
   Pre:  `used` is an integer >= 0. `total` is the model's context length, or
         nil when it is unknown, and then the total is left out."
  [{:keys [used total]}]
  (str (count-text used)
    (when total (str " / " (count-text total)))
    " tokens"))

(def usage-tooltip
  "Why the count moves when the chat does not."
  "Size of the last request and its answer. Includes the open page's text.")
