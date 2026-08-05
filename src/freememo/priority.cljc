(ns freememo.priority
  "Review priority: the value domain, and the one rounding rule that serves it.

   A priority is a number in [0,100] carrying three decimal places. 0 is the
   highest priority, 100 is the lowest, 50 is the default. `12.345` is legal.

   `normalize` is the WRITE BOUNDARY. Every value bound for `topics.priority`
   passes through it, so no call site repeats a clamp or a round.
   `topics_priority_range_check` in the database backs it up for the insert
   paths that never call it (create-topic!, insert-topic-rows!).

   `round-to` is GENERIC and is not about priority. `freememo.number-stepper`
   calls it for all three steppers it renders, including the integer
   card-count and context-pages controls. Do not move it without updating that
   caller: one rounding rule shared by both peers is what keeps the client's
   optimistic mirror equal to the value the server stores.

   `freememo.db` requires this namespace, so it requires nothing itself —
   an Electric require here would pull Electric onto the server load path.")

(def priority-min 0)
(def priority-max 100)
(def priority-decimals 3)
(def priority-default 50)

(defn round-to
  "Round `v` to `decimals` places, half-up.
   Pre : `v` is a finite number. `decimals` is a non-negative integer.
   Post: a floating value, exact whenever |v · 10^decimals| < 2^53. The
         priority domain tops out at 100 · 10^3 = 100000, far inside that."
  [v decimals]
  (let [factor (Math/pow 10 decimals)
        scaled (* v factor)]
    (/ #?(:clj (Math/round (double scaled))
          :cljs (js/Math.round scaled))
      factor)))

(defn- finite-number?
  "True when `v` is a number that is neither NaN nor infinite."
  [v]
  (and (number? v)
    #?(:clj (Double/isFinite (double v))
       :cljs (js/isFinite v))))

(defn- clamp [lo hi v] (max lo (min hi v)))

(defn normalize
  "Canonical priority for `v`.
   Pre : none. `v` may be nil, non-numeric, NaN or infinite.
   Post: a floating value in [priority-min, priority-max] with at most
         `priority-decimals` places.
   Invariant: an unusable `v` yields `priority-default` and never an error.
         The only caller is a UI write whose control surfaces no validation
         error (see PriorityControl), so a throw here would strand a token."
  [v]
  (round-to
    (clamp priority-min priority-max
      (if (finite-number? v) v priority-default))
    priority-decimals))
