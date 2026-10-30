(ns freememo.window-ranges
  "Group mounted indices into bounded database reads.")

(defn index-ranges
  "Return [offset limit] ranges for distinct indices, including distant pins."
  [indices]
  (reduce (fn [ranges index]
            (if-let [[start size] (peek ranges)]
              (if (and (= index (+ start size)) (< size 100))
                (conj (pop ranges) [start (inc size)])
                (conj ranges [index 1]))
              [[index 1]]))
    [] (sort (into #{} (filter #(and (integer? %) (<= 0 %))) indices))))
