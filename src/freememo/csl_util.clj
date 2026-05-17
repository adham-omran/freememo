(ns freememo.csl-util
  "Leaf helpers for CSL-JSON. Kept dependency-free so importers can use them
   without pulling in the form or db layers.")

(defn pad-date-parts
  "Normalize CSL date-parts to a full [[Y M D]] triple. Year-only `[[Y]]` →
   `[[Y 1 1]]`; year-month `[[Y M]]` → `[[Y M 1]]`; full or nil unchanged.
   Importers route through this so newly-persisted CSL carries uniform
   precision; pre-existing partial rows remain untouched until edited via
   the form."
  [dp]
  (if (and (sequential? dp) (sequential? (first dp)))
    (let [[y m d] (first dp)]
      (cond
        (and y m d) [[y m d]]
        (and y m)   [[y m 1]]
        y           [[y 1 1]]
        :else       dp))
    dp))
