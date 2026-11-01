(ns freememo.focus-window-model
  "Page identity and retained text spans for overlapping editor windows."
  (:require [clojure.set :as set]
            [freememo.page-merge :as page-merge]))

(defn page-spans [ops lowest-page]
  (let [state (reduce
                (fn [{:keys [index line-start] :as state} op]
                  (let [insert (:insert op)
                        length (if (string? insert) (count insert) 1)
                        end (+ index length)
                        page (page-merge/parse-marker-id (get-in op [:attributes :id]))
                        last-newline (when (string? insert)
                                       #?(:clj (.lastIndexOf ^String insert "\n")
                                          :cljs (.lastIndexOf insert "\n")))]
                    (cond-> (assoc state :index end)
                      page (update :markers conj [page line-start])
                      (and last-newline (>= last-newline 0))
                      (assoc :line-start (+ index last-newline 1)))))
                {:index 0 :line-start 0 :markers []} ops)
        markers (:markers state)
        markers (cond
                  (empty? markers) [[lowest-page 0]]
                  (= lowest-page (ffirst markers)) (assoc-in markers [0 1] 0)
                  (pos? (second (first markers))) (into [[lowest-page 0]] markers)
                  :else markers)]
    (into {} (map (fn [[[page start] [_ end]]] [page [start end]])
                   (partition 2 1 (conj markers [nil (:index state)]))))))

(defn patch-plan [old-spans old-pages new-pages length]
  (let [old-pages (vec old-pages)
        retained (vals (select-keys old-spans (set/intersection (set old-pages) (set new-pages))))]
    {:keep-start (if (seq retained) (apply min (map first retained)) length)
     :keep-end (if (seq retained) (apply max (map second retained)) length)
     :prefix-pages (vec (take-while #(< % (first old-pages)) new-pages))
     :suffix-pages (vec (drop-while #(<= % (last old-pages)) new-pages))}))

(defn prefix-reload? [old-pages new-pages marker-index lowest-span]
  (and (< (first new-pages) (first old-pages))
       (if (some? marker-index)
         (pos? marker-index)
         (and lowest-span (< (first lowest-span) (second lowest-span))))))
