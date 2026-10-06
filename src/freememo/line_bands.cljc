(ns freememo.line-bands
  "One rectangle per visual line of a range, from the range's client rects.

   `Range.getClientRects` returns a rect for every inline fragment the range
   touches, and for an element the range covers whole it returns the element's
   box AND its text's box. A `<code>` run therefore gives two nested rects that
   are taller than the text beside them. Outlining each rect draws boxes inside
   boxes with ragged edges. Outlining one band per line draws a shape the eye
   reads as one mark.

   Pure rectangle arithmetic, no DOM. `.cljc` with no requires so `test/` can
   reach it, as `freememo.selection-placement` does.
   `freememo.question-pin-dom/paint-finger!` is the one caller.")

(defn- centre [{:keys [top bottom]}] (/ (+ top bottom) 2))

(defn- same-line?
  "True when either rect's vertical centre lies inside the other's span. A
   padded `<code>` box and the text beside it share a line; two lines of text
   do not, because a line's centre is outside its neighbour's span."
  [a b]
  (or (<= (:top a) (centre b) (:bottom a))
      (<= (:top b) (centre a) (:bottom b))))

(defn- union [a b]
  {:left (min (:left a) (:left b)) :top (min (:top a) (:top b))
   :right (max (:right a) (:right b)) :bottom (max (:bottom a) (:bottom b))})

(defn bands
  "Pre : `rects` is a seq of maps {:left :top :right :bottom}.
   Post: one rect per visual line, ordered top to bottom. Each is the union of
         the rects on its line, so it covers every one of them."
  [rects]
  (reduce (fn [acc r]
            (let [prev (peek acc)]
              (if (and prev (same-line? prev r))
                (conj (pop acc) (union prev r))
                (conj acc r))))
    []
    (sort-by (juxt :top :left) rects)))
