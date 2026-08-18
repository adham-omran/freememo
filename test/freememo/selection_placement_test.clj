(ns freememo.selection-placement-test
  "Tiers, mirroring and the never-cover invariant for the format menu's
   placement. One deftest per row of the verification table in
   plans/format-menu-never-obscure-selection.md §4.2, for the rows a pure
   function can decide — the rows about DOM classes and dropdown flips are
   browser checks.

   Needs no database: freememo.selection-placement has no requires."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.selection-placement :as placement]))

;; A tall editor pane in an 800px window: the document editor's shape.
(def ^:private pane {:top 100 :bottom 700})
(def ^:private viewport {:width 1200 :height 800})
(def ^:private menu {:width 320 :height 300})

(defn- place
  "place with the pane/viewport/menu defaults, overridable per case."
  [m]
  (placement/place (merge {:container pane :viewport viewport :menu menu
                           :focus-x 500 :rtl? false}
                     m)))

(defn- visible-band
  "Selection rows the user can see: the selection clipped to the container AND to
   the viewport. Nil when no row survives both — nothing visible cannot be
   covered, which is why the sweep treats nil as a pass."
  [{sel-top :top sel-bottom :bottom} {con-top :top con-bottom :bottom} {view-h :height}]
  (let [top (max sel-top con-top placement/margin)
        bottom (min sel-bottom con-bottom (- view-h placement/margin))]
    (when (< top bottom) [top bottom])))

(defn- covers-selection?
  "True when the placement's vertical band overlaps the visible selection band."
  ([result selection container menu-height]
   (covers-selection? result selection container viewport menu-height))
  ([result selection container view menu-height]
   (when result
     (when-let [[b-top b-bottom] (visible-band selection container view)]
       (let [top (:top result)
             bottom (+ top menu-height)]
         (and (< top b-bottom) (> bottom b-top)))))))

;; ---------------------------------------------------------------------------
;; Tier selection — §4.2 rows 1, 3, 4, 5, 6, 10, 11
;; ---------------------------------------------------------------------------

(deftest row-1-multi-line-selection-mid-pane-goes-below
  (let [sel {:top 200 :bottom 380}
        r (place {:selection sel})]
    (is (= :below (:side r)))
    (is (= (+ 380 placement/gap) (:top r))
      "anchored to the box BOTTOM, not to the first visual line")))

(deftest row-3-no-room-below-in-pane-flips-above
  ;; Selection ends at 500: below needs 500+8+300 = 808 > pane bottom 700.
  ;; Above needs 420-8-300 = 112 >= pane top 100.
  (let [sel {:top 420 :bottom 500}
        r (place {:selection sel})]
    (is (= :above (:side r)))
    (is (= (- 420 placement/gap 300) (:top r)))
    (is (not (covers-selection? r sel pane 300)))))

(deftest row-4-no-room-either-side-in-pane-overflows-below
  ;; An editor with a bottom panel under it: pane ends at 500 in an 800 window.
  ;; Above needs 150-8-300 = -158 < 100. Below in pane needs 488+300 = 788 > 500.
  ;; Tier 3 takes 488, which clears the pane bottom but fits the 792 floor.
  (let [short-pane {:top 100 :bottom 500}
        sel {:top 150 :bottom 480}
        r (place {:selection sel :container short-pane})]
    (is (= :below (:side r)))
    (is (= (+ 480 placement/gap) (:top r)))
    (is (> (+ (:top r) 300) (:bottom short-pane))
      "deliberately overflows the pane, over whatever sits under the editor")
    (is (not (covers-selection? r sel short-pane 300)))))

(deftest row-5-selection-scrolled-above-pane-anchors-to-visible-bottom
  ;; Box top is above the pane, so tier 2 can never fire; tier 1 fails on
  ;; height, and tier 3 anchors below the box bottom (still inside the pane).
  (let [sel {:top -400 :bottom 480}
        r (place {:selection sel})]
    (is (= :below (:side r)))
    (is (= (+ 480 placement/gap) (:top r)))
    (is (not (covers-selection? r sel pane 300)))))

(deftest row-5b-selection-extends-past-both-pane-edges
  ;; Nothing above, nothing below the box — tier 3 anchors below the pane
  ;; bottom, because that is the lowest row the user can see.
  (let [sel {:top -200 :bottom 1400}
        r (place {:selection sel :menu {:width 320 :height 80}})]
    (is (= :below (:side r)))
    (is (= (+ (:bottom pane) placement/gap) (:top r)))
    (is (not (covers-selection? r sel pane 80)))))

(deftest row-6-taller-than-pane-and-viewport-is-hidden
  (is (nil? (place {:selection {:top -200 :bottom 1400}}))
    "300px menu fits under neither the box nor the pane bottom → hide"))

(deftest row-10-card-field-container-falls-straight-to-viewport-tier
  ;; A card-modal field: 80px tall container. No tier-1/tier-2 fit is possible.
  (let [field {:top 300 :bottom 380}
        sel {:top 310 :bottom 370}
        r (place {:selection sel :container field})]
    (is (= :below (:side r)))
    (is (= (+ 370 placement/gap) (:top r)))
    (is (not (covers-selection? r sel field 300)))))

(deftest row-11-single-line-selection-still-goes-below
  (let [sel {:top 300 :bottom 322}
        r (place {:selection sel})]
    (is (= :below (:side r)))
    (is (= (+ 322 placement/gap) (:top r)))))

;; ---------------------------------------------------------------------------
;; Hide — §1.6 out of view
;; ---------------------------------------------------------------------------

(deftest selection-entirely-above-the-pane-is-hidden
  (is (nil? (place {:selection {:top -300 :bottom 100}}))
    "box bottom touching the pane top is not a visible row"))

(deftest selection-entirely-below-the-pane-is-hidden
  (is (nil? (place {:selection {:top 700 :bottom 900}}))))

;; ---------------------------------------------------------------------------
;; Horizontal — §1.5, §4.2 rows 2 and 8
;; ---------------------------------------------------------------------------

(deftest row-2-left-edge-sits-at-the-focus-point
  ;; A backward drag puts the focus at the TOP of the box: the side still comes
  ;; from the box, the x still comes from the focus.
  (let [r (place {:selection {:top 200 :bottom 380} :focus-x 640})]
    (is (= 640 (:left r)))
    (is (= :below (:side r)) "the box picks the side, not the focus end")))

(deftest row-8-rtl-puts-the-right-edge-at-the-focus-point
  (let [r (place {:selection {:top 200 :bottom 380} :focus-x 640 :rtl? true})]
    (is (= (- 640 320) (:left r)))))

(deftest left-is-clamped-to-the-viewport-edges
  (testing "focus near the right edge slides the menu back in"
    (is (= (- 1200 320 placement/margin)
          (:left (place {:selection {:top 200 :bottom 380} :focus-x 1180})))))
  (testing "RTL focus near the left edge clamps to the margin"
    (is (= placement/margin
          (:left (place {:selection {:top 200 :bottom 380} :focus-x 40 :rtl? true})))))
  (testing "a menu wider than the viewport lands on the margin"
    (is (= placement/margin
          (:left (place {:selection {:top 200 :bottom 380} :focus-x 600
                         :menu {:width 2000 :height 300}}))))))

;; ---------------------------------------------------------------------------
;; The invariant itself — §1.4
;; ---------------------------------------------------------------------------

;; Every container shape the app produces, plus the two that broke the unclipped
;; version: a pane an ancestor scrolled off the top, and a pane taller than the
;; window.
(def ^:private container-shapes
  [{:top 100 :bottom 700}     ; document editor
   {:top 100 :bottom 500}     ; editor with a bottom panel under it
   {:top 300 :bottom 380}     ; card-modal field
   {:top -300 :bottom 500}    ; scrolled off the top of the viewport
   {:top 100 :bottom 1400}])  ; taller than the window

(def ^:private viewport-shapes
  [{:width 1200 :height 800}
   {:width 640 :height 500}])

(deftest never-covers-a-visible-selection-row
  (let [failures (for [con container-shapes
                       view viewport-shapes
                       menu-height [80 300 520]
                       sel-top (range -400 1000 37)
                       sel-height [18 40 120 300 600 1200]
                       :let [sel {:top sel-top :bottom (+ sel-top sel-height)}
                             m {:width 320 :height menu-height}
                             r (place {:selection sel :container con :viewport view :menu m})]
                       :when (covers-selection? r sel con view menu-height)]
                   {:selection sel :container con :viewport view
                    :menu-height menu-height :result r})]
    (is (empty? failures)
      (str "placements covering visible selected rows: " (pr-str (take 5 failures))))))

(deftest never-places-above-the-top-of-the-viewport
  (let [failures (for [con container-shapes
                       view viewport-shapes
                       menu-height [80 300 520]
                       sel-top (range -400 1000 37)
                       sel-height [18 40 120 300 600 1200]
                       :let [sel {:top sel-top :bottom (+ sel-top sel-height)}
                             r (place {:selection sel :container con :viewport view
                                       :menu {:width 320 :height menu-height}})]
                       :when (and r (< (:top r) placement/margin))]
                   {:selection sel :container con :viewport view :result r})]
    (is (empty? failures)
      (str "placements off the top of the viewport: " (pr-str (take 5 failures))))))

(deftest a-container-scrolled-off-the-top-does-not-place-off-screen
  ;; Pane top −300, so an unclipped above-tier reads −108 as "inside the pane"
  ;; and puts the card off the screen while the user can see selected text.
  (let [high-pane {:top -300 :bottom 500}
        sel {:top 200 :bottom 460}
        r (place {:selection sel :container high-pane})]
    (is (= :below (:side r)))
    (is (= (+ 460 placement/gap) (:top r)))
    (is (>= (:top r) placement/margin))
    (is (not (covers-selection? r sel high-pane 300)))))

(deftest a-container-taller-than-the-window-clips-to-the-window
  ;; Pane bottom 1400 against a 800px window: rows past the window are not
  ;; visible, so the below-tier may not treat 1400 as room.
  (let [deep-pane {:top 100 :bottom 1400}
        r (place {:selection deep-pane :container deep-pane})]
    (is (nil? r) "a selection filling the whole visible pane leaves no position"))
  (let [deep-pane {:top 100 :bottom 1400}
        sel {:top 200 :bottom 300}
        r (place {:selection sel :container deep-pane})]
    (is (= (+ 300 placement/gap) (:top r)))
    (is (<= (+ (:top r) 300) (- (:height viewport) placement/margin))
      "the card stays inside the window, not merely inside the pane")))

(deftest a-placement-exists-whenever-one-fits
  (testing "a short menu beside a huge selection is placed, not hidden"
    (is (some? (place {:selection {:top -500 :bottom 1500}
                       :menu {:width 320 :height 80}}))))
  (testing "a menu taller than the room below the pane is hidden"
    (is (nil? (place {:selection {:top -500 :bottom 1500}
                      :menu {:width 320 :height 200}}))
      "pane bottom 700 + 8 + 200 = 908 > viewport floor 792")))
