(ns freememo.menu-placement
  "Where the context menu goes: pure arithmetic, so a clj test can sweep it.

   Two anchors. A POINT is where the pointer was on a right-click, and the
   menu opens with its top-left corner there. A RECT is the box of the thing a
   keyboard open or a ⋯ button targets, and the menu opens under its left edge.

   Either way the menu flips to the other side of the anchor when it does not
   fit, and is then clamped into the viewport. It never changes size: a menu
   taller than the viewport is clamped to the top margin and scrolls inside
   itself (`max-height` in index.css).")

(def margin
  "Gap kept between the menu and every viewport edge, in px."
  8)

(defn- clamp [lo hi x]
  (max lo (min hi x)))

(defn- flip
  "The menu's start on one axis: at `preferred` when the menu fits before the
   far edge, else ending at `fallback-end`, then clamped into the viewport."
  [preferred fallback-end size viewport]
  (let [hi (- viewport margin size)
        start (if (<= (+ preferred size) (- viewport margin))
                preferred
                (- fallback-end size))]
    (clamp margin (max margin hi) start)))

(defn place
  "Top-left corner of a `menu` box for `anchor` inside `viewport`.

   Pre : `anchor` is {:x :y} (a point) or {:left :top :right :bottom} (a rect),
         in viewport (client) coordinates. `menu` and `viewport` are
         {:width :height}, all non-negative.
   Post: {:left :top}. When the menu fits the viewport less two margins, the
         whole menu lies inside [margin, viewport - margin] on both axes.
         Otherwise its left or top edge sits at the margin."
  [anchor menu viewport]
  (let [{mw :width mh :height} menu
        {vw :width vh :height} viewport]
    (if (contains? anchor :x)
      (let [{:keys [x y]} anchor]
        {:left (flip x x mw vw)
         :top (flip y y mh vh)})
      (let [{:keys [left top right bottom]} anchor]
        {:left (flip left right mw vw)
         :top (flip bottom top mh vh)}))))
