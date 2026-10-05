(ns freememo.question-map-block
  "The data block a tutor reply starts with when it builds or updates a
   question map (plans/anchored-questions.md §6.3.2).

   A reply about a document with a question map MAY start with one fenced
   block whose info string is `fence-tag` and whose body is JSON:

     ```freememo-map
     {\"roots\":   [{\"label\": \"Signature axes\", \"in_document\": true,
                    \"citations\": [\"E3\"]}],
      \"pins\":    {\"1\": \"Signature axes\", \"3\": \"Subtyping\"},
      \"next\":    \"Signature axes\",
      \"learned\": [\"Subtyping\"]}
     ```

   Every key is optional. This namespace splits the block from the prose,
   checks the parsed data, and turns it into a plan for
   `db/apply-question-map-plan!`. It has no requires on purpose: the transcript
   view strips the block on every stream tick, and a test reaches every rule
   here without a database or a model. JSON parsing stays with the caller.")

(def fence-tag "freememo-map")

(def ^:private opener (str "```" fence-tag))

(def max-roots
  "Roots one reply may add. A diagnosis of 20 pins that needs more than this
   has not found what they share."
  12)

(def max-label-chars 120)

(defn- trim-start [s]
  (let [n (count s)]
    (loop [i 0]
      (if (and (< i n) (#{\space \tab \newline \return} (nth s i)))
        (recur (inc i))
        (subs s i)))))

(defn- prefix-of? [s whole]
  (and (<= (count s) (count whole)) (= s (subs whole 0 (count s)))))

(defn split-reply
  "Split reply `text` into its leading map block and the prose after it.
   Post: {:block json-or-nil :prose string :pending? bool}.
     - A reply that starts with a closed block gives its body and the prose
       after the closing fence.
     - A reply that is still inside the block, or is only a prefix of the
       opener so far, gives :pending? true and empty prose. The transcript
       then shows nothing rather than raw map data (§6.3.2.1).
     - Any other reply is all prose."
  [text]
  (let [text (or text "")
        lead (trim-start text)]
    (cond
      (and (seq lead) (< (count lead) (count opener)) (prefix-of? lead opener))
      {:block nil :prose "" :pending? true}

      (prefix-of? opener lead)
      (let [after-tag (subs lead (count opener))
            nl (.indexOf ^String after-tag "\n")]
        (if (neg? nl)
          {:block nil :prose "" :pending? true}
          (let [body-start (inc nl)
                close (.indexOf ^String after-tag "```" (int body-start))]
            (if (neg? close)
              {:block nil :prose "" :pending? true}
              {:block (subs after-tag body-start close)
               :prose (trim-start (subs after-tag (+ close 3)))
               :pending? false}))))

      :else {:block nil :prose text :pending? false})))

(defn prose
  "The part of reply `text` a reader sees."
  [text]
  (:prose (split-reply text)))

(defn- fold [label] (some-> label str .trim .toLowerCase))

(defn- label-text? [s]
  (and (string? s) (seq (.trim ^String s)) (<= (count s) max-label-chars)))

(defn- pin-label [k]
  (let [s (if (keyword? k) (name k) (str k))]
    (when (re-matches #"[1-9][0-9]{0,6}" s)
      #?(:clj (Long/parseLong s) :cljs (js/parseInt s 10)))))

(defn problem
  "Why parsed block `data` cannot be applied, or nil when it can.
   `existing-roots` are the map's root labels and `pin-labels` its pin labels.
   Every root the block names must be one it adds or one the map has, and every
   pin it names must exist."
  [data existing-roots pin-labels]
  (let [roots (:roots data)
        known (into (set (map fold existing-roots))
                (keep #(when (map? %) (fold (:label %))))
                (when (sequential? roots) roots))
        pins (:pins data)
        learned (:learned data)
        next-root (:next data)
        pin-labels (set pin-labels)]
    (cond
      (not (map? data)) "The map block is not an object."
      (and (some? roots) (not (sequential? roots))) "\"roots\" is not a list."
      (> (count roots) max-roots) (str "The map block adds more than " max-roots " roots.")
      (not-every? #(and (map? %) (label-text? (:label %))) roots)
      "A root has no label, or its label is too long."
      (not-every? #(boolean? (:in_document %)) roots)
      "A root does not say whether it is in the document."
      (not-every? #(let [c (:citations %)]
                     (or (nil? c)
                       (and (sequential? c)
                         (every? (fn [k] (and (string? k) (re-matches #"E[0-9]+" k))) c))))
        roots)
      "A root has citations that are not evidence keys."
      (and (some? pins) (not (map? pins))) "\"pins\" is not an object."
      (not-every? (fn [[k v]] (and (pin-labels (pin-label k)) (known (fold v)))) pins)
      "\"pins\" names a pin or a root that does not exist."
      (and (some? learned) (not (and (sequential? learned) (every? string? learned))))
      "\"learned\" is not a list of labels."
      (not-every? #(known (fold %)) learned) "\"learned\" names a root that does not exist."
      (and (some? next-root) (not (and (string? next-root) (known (fold next-root)))))
      "\"next\" names a root that does not exist.")))

(defn- own-root-label
  "The label of the root a pin gets when no root fits it (§6.3.3)."
  [{:keys [label question quote]}]
  (let [text (.trim (str (if (seq (.trim (str question))) question quote)))
        text (if (> (count text) 80) (str (subs text 0 77) "...") text)]
    (str "Pin " label ": " text)))

(defn plan
  "Turn valid block `data` into a write plan for one map.
   Pre: (problem data …) is nil.
   `ctx`:
     :diagnosis?   true for an Ask run, which must place every open pin
     :roots        the map's roots, [{:label :status :known?}]
     :open-pins    the map's open pins, [{:label :question :quote :root-label}]
   Post: {:new-roots [{:label :in-document? :citation-keys}]
          :pin-roots {pin-label root-label}
          :learned   #{root-label}
          :next      root-label or nil}
     Labels are the map's existing spelling when one matches ignoring case.
     On a diagnosis, every open pin has a root afterwards, and a map with an
     unlearned root that is not known has a `next` root. A known root is never
     chosen here: the tutor teaches it only when the reader asks (§6.4.3)."
  [data {:keys [diagnosis? roots open-pins]}]
  (let [existing (into {} (map (juxt (comp fold :label) :label)) roots)
        added (reduce (fn [acc {:keys [label in_document citations]}]
                        (let [label (.trim ^String label)
                              k (fold label)]
                          (if (or (existing k) (some #(= k (fold (:label %))) acc))
                            acc
                            (conj acc {:label label :in-document? in_document
                                       :citation-keys (vec citations)}))))
                [] (:roots data))
        spelling (merge existing (into {} (map (juxt (comp fold :label) :label)) added))
        canon (fn [label] (spelling (fold label)))
        named (into {} (map (fn [[k v]] [(pin-label k) (canon v)])) (:pins data))
        unplaced (when diagnosis?
                   (remove #(or (named (:label %)) (:root-label %)) open-pins))
        own-roots (mapv (fn [pin] {:label (own-root-label pin) :in-document? true
                                   :citation-keys []})
                    unplaced)
        new-roots (into added (remove #(spelling (fold (:label %)))) own-roots)
        pin-roots (into named (map (fn [pin] [(:label pin) (own-root-label pin)])) unplaced)
        learned (into #{} (map canon) (:learned data))
        status (merge (into {} (map (juxt :label :status)) roots)
                 (into {} (map (juxt :label (constantly "open"))) new-roots))
        known? (into #{} (comp (filter :known?) (map :label)) roots)
        candidates (fn [] (->> (concat (map :label roots) (map :label new-roots))
                            (remove #(or (learned %) (= "learned" (status %))))))
        asked (some-> (:next data) canon)
        has-next? (some #(and (= "next" (:status %)) (not (learned (:label %)))) roots)
        next-root (cond
                    (and asked (not (learned asked))) asked
                    (and diagnosis? (not has-next?))
                    (first (remove known? (candidates))))]
    {:new-roots new-roots
     :pin-roots pin-roots
     :learned learned
     :next next-root}))
