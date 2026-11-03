(ns freememo.exam-grid
  "The shape of a generated exam, computed from its target minutes before any
   model call: how many questions of each format, their points, roles and
   thinking levels. The server builds its blueprint request from these slots,
   and the minutes slider shows the same counts as it moves, so the two cannot
   disagree. No requires: the client reads this namespace too.
   See plans/blueprint-first-exams.md §3 and §4.")

(def min-minutes 5)
(def max-minutes 90)

(def max-questions
  "The question limit that validate-form! and curriculum_exam_forms' CHECK hold."
  40)

(def format-minutes {"multiple-choice" 1.25 "short-essay" 5 "long-essay" 12})
(def format-points {"multiple-choice" 1 "short-essay" 4 "long-essay" 10})

(def levels
  "Bloom's revised taxonomy, lowest first."
  ["remember" "understand" "apply" "analyze" "evaluate" "create"])

(def essay-levels
  "The levels an essay may take in the :mixed grid: apply or higher."
  #{"apply" "analyze" "evaluate" "create"})

(def recall-level "remember")

(def max-recall-share
  "The largest share of a grid's points that recall slots may hold (plan 4.5)."
  0.2)

(def role-levels
  {"explain" "understand" "compare" "analyze" "apply" "apply"
   "evaluate" "evaluate" "synthesize" "create"})

(def short-essay-roles ["explain" "compare" "apply" "evaluate"])
(def long-essay-roles ["synthesize" "evaluate"])

(def max-short-essays 8)
(def max-long-essays 4)

(def grid-kinds #{:mixed :essay})

(defn minutes-in-range? [minutes]
  (and (integer? minutes) (<= min-minutes minutes max-minutes)))

(defn- floor-int [x] (int (Math/floor x)))
(defn- round-int
  "Round half up. Post: the same integer on the JVM and in JavaScript."
  [x]
  (int (Math/floor (+ x 0.5))))

(defn mixed-counts
  "Counts of the :mixed grid for t minutes (plan 4.3).
   Pre: (minutes-in-range? t). Post: {:long :short :mcq}, at most 40 in total."
  [t]
  (let [long-n (cond (< t 30) 0 (< t 90) 1 :else 2)
        short-n (if (< t 15) 0 (max 1 (quot t 20)))
        essay-minutes (+ (* 12 long-n) (* 5 short-n))]
    {:long long-n :short short-n
     :mcq (max 0 (min (- max-questions long-n short-n)
                      (floor-int (/ (- t essay-minutes) 1.25))))}))

(defn essay-counts
  "Counts of the :essay grid for t minutes (plan 4.4): short essays fill the
   time first, up to 8, and the rest becomes long essays, up to 4.
   Pre: (minutes-in-range? t). Post: {:long :short :mcq 0}."
  [t]
  (let [first-long (if (< t 25) 0 1)
        short-n (max 1 (min max-short-essays (round-int (/ (- t (* 12 first-long)) 5))))
        extra (max 0 (min (dec max-long-essays)
                          (floor-int (/ (- t (* 12 first-long) (* 5 short-n)) 12))))]
    {:long (+ first-long extra) :short short-n :mcq 0}))

(defn counts
  "Pre: kind ∈ grid-kinds and (minutes-in-range? t)."
  [kind t]
  (case kind :mixed (mixed-counts t) :essay (essay-counts t)))

(defn slots
  "The grid for kind at t minutes, one slot per question.
   Post: slots in order (multiple-choice, short essays, long essays), each
   {:id \"s1\"… :format :points :role :level}. :role and :level are nil where
   the blueprint call chooses them: every slot of the :mixed grid."
  [kind t]
  (let [{:keys [mcq] short-n :short long-n :long} (counts kind t)
        essay? (= kind :essay)
        roles (fn [pool n] (if essay? (take n (cycle pool)) (repeat n nil)))
        specs (concat (map (fn [_] ["multiple-choice" nil]) (range mcq))
                      (map (fn [role] ["short-essay" role]) (roles short-essay-roles short-n))
                      (map (fn [role] ["long-essay" role]) (roles long-essay-roles long-n)))]
    (vec (map-indexed (fn [i [format role]]
                        {:id (str "s" (inc i)) :format format :points (format-points format)
                         :role role :level (when role (role-levels role))})
           specs))))

(defn slot-minutes [slots] (reduce + (map (comp format-minutes :format) slots)))
(defn slot-points [slots] (reduce + (map :points slots)))

(defn summary
  "What a grid asks for. Post: {:long :short :mcq :questions :points :minutes}."
  [kind t]
  (let [grid (slots kind t)]
    (assoc (counts kind t) :questions (count grid)
      :points (slot-points grid) :minutes (slot-minutes grid))))

(defn- counted [n one many] (str n " " (if (= 1 n) one many)))

(defn summary-text
  "The line beside the minutes slider."
  [kind t]
  (let [{:keys [mcq points] short-n :short long-n :long} (summary kind t)
        parts (cond-> []
                (pos? mcq) (conj (counted mcq "multiple-choice question" "multiple-choice questions"))
                (pos? short-n) (conj (counted short-n "short essay" "short essays"))
                (pos? long-n) (conj (counted long-n "long essay" "long essays")))]
    (str t " minutes: " (apply str (interpose ", " parts)) " (" points " points)")))

(defn output-tokens
  "The output allowance of both calls for a grid: the plan, then the questions.
   Calibrated on 2026-11-03 replays: 21 questions cost 0.095 USD on
   gemini-3.8-flash, and 5 essays cost 0.027 USD. Set above both."
  [kind t]
  (+ 4000 (* 900 (count (slots kind t)))))

(defn estimate
  "The cost of an exam of kind at t minutes from a price sheet
   (assessment/estimated-cost). Post: {:usd :credits}; :credits is nil when
   the sheet carries no credit rate."
  [{:keys [input-usd usd-per-output-token credits-per-usd]} kind t]
  (let [usd (+ input-usd (* usd-per-output-token (output-tokens kind t)))]
    {:usd usd :credits (when credits-per-usd (long (Math/ceil (* usd credits-per-usd))))}))
