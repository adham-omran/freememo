(ns freememo.selection-placement-test
  "Check placement priority, visibility, and clearance for the selection menu.
   These pure tests need no database. Browser tests check DOM measurements."
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
                           :focus-x 500 :rtl? false :allow-overlap? true}
                     m)))

(defn- visible-band
  "Return the selection's visible vertical band."
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

(defn- visible-box
  [{:keys [selection container viewport]}]
  (when-let [[top bottom] (visible-band selection container viewport)]
    (let [left (max (or (:left selection) 0) (or (:left container) 0) placement/margin)
          right (min (or (:right selection) (:width viewport))
                  (or (:right container) (:width viewport))
                  (- (:width viewport) placement/margin))]
      (when (< left right)
        {:top top :bottom bottom :left left :right right}))))

(defn- intersects?
  [result {:keys [width height]} box]
  (and result box
    (< (:left result) (:right box))
    (> (+ (:left result) width) (:left box))
    (< (:top result) (:bottom box))
    (> (+ (:top result) height) (:top box))))

(defn- clear-region-fits?
  "Measure each permitted free region independently of the placement result."
  [{:keys [selection container viewport menu reserve] :as opts}]
  (when-let [box (visible-box opts)]
    (let [clearance (+ placement/gap (or reserve 0))
          top (max (:top container) placement/margin)
          right (- (:width viewport) placement/margin)
          bottom (- (:height viewport) placement/margin)
          free-heights [(- (:top selection) clearance top)
                        (- bottom (:bottom box) clearance)]
          free-widths [(- (:left box) clearance placement/margin)
                       (- right (:right box) clearance)]]
      (or (some #(>= % (:height menu)) free-heights)
          (some #(>= % (:width menu)) free-widths)))))

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

(deftest row-6-taller-than-pane-and-viewport-stays-visible
  (let [r (place {:selection {:top -200 :bottom 1400}})]
    (is (some? r) "Select All must keep the menu available")
    (is (= :overlap (:side r)))
    (when r
      (is (<= placement/margin (:top r) (- 800 placement/margin 300))))))

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

(deftest selections-outside-the-viewport-are-hidden
  (testing "selected text lies above the viewport"
    (is (nil? (place {:selection {:top -500 :bottom -100}
                      :container {:top -600 :bottom 700}}))))
  (testing "selected text lies below the viewport"
    (is (nil? (place {:selection {:top 900 :bottom 1000}
                      :container {:top 100 :bottom 1400}})))))

(deftest a-container-outside-the-viewport-has-no-visible-selection
  (doseq [con [{:top -800 :bottom -100}
               {:top 900 :bottom 1500}]]
    (is (nil? (place {:selection {:top -1000 :bottom 2000} :container con}))
      (str "A hidden container must hide its menu: " con))))

(deftest horizontal-offscreen-selections-are-hidden
  (doseq [[sel con] [[{:left -600 :right -400} {:left -800 :right -300}]
                     [{:left 1450 :right 1750} {:left 1400 :right 1800}]]]
    (is (nil? (place {:selection (merge {:top -200 :bottom 1400} sel)
                      :container (merge pane con)}))
      (str "An offscreen selection must hide its menu: " sel))))

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

(deftest full-width-selection-uses-the-overlap-fallback
  (let [r (place {:selection {:top -200 :bottom 1400 :left 0 :right 1200}})]
    (is (= {:top 100 :left 500 :side :overlap} r))))

(deftest overlap-requires-explicit-permission
  (let [opts {:selection {:top -200 :bottom 1400 :left 0 :right 1200}
              :container pane :viewport viewport :menu menu
              :focus-x 500 :rtl? false}]
    (doseq [flag [nil false]]
      (is (nil? (placement/place (assoc opts :allow-overlap? flag)))))
    (is (nil? (placement/place opts)))))

(deftest right-pane-selection-places-the-menu-on-the-left
  (let [con {:top 100 :bottom 700 :left 650 :right 1200}
        sel {:top -200 :bottom 1400 :left 680 :right 1180}
        r (place {:selection sel :container con})]
    (is (= {:top 100 :left 352 :side :left} r))
    (is (not (intersects? r menu (visible-box {:selection sel :container con :viewport viewport}))))))

(deftest left-pane-selection-places-the-menu-on-the-right
  (let [r (place {:selection {:top -200 :bottom 1400 :left 30 :right 500}
                  :container {:top 100 :bottom 700 :left 0 :right 550}})]
    (is (= {:top 100 :left 508 :side :right} r))))

(deftest horizontal-placement-uses-the-clipped-selection
  (testing "a selection past both pane edges leaves room beside the visible pane"
    (let [r (place {:selection {:top -200 :bottom 1400 :left -400 :right 1700}
                    :container {:top 100 :bottom 700 :left 650 :right 1180}})]
      (is (= {:top 100 :left 322 :side :left} r))))
  (testing "viewport clipping prevents a position beyond the right viewport edge"
    (let [r (place {:selection {:top -200 :bottom 1400 :left -400 :right 1700}
                    :container {:top 100 :bottom 700 :left -100 :right 1500}})]
      (is (= :overlap (:side r))))))

(deftest short-viewports-keep-the-overlap-menu-inside-the-window
  (let [r (place {:selection {:top 80 :bottom 1500 :left 0 :right 500}
                  :container {:top 100 :bottom 600 :left 0 :right 500}
                  :viewport {:width 500 :height 300}
                  :menu {:width 320 :height 250}})]
    (is (= {:top 42 :left 172 :side :overlap} r))))

(deftest rtl-fallback-keeps-the-focus-at-the-menu-right-edge
  (let [r (place {:selection {:top -200 :bottom 1400}
                  :focus-x 640 :rtl? true})]
    (is (= {:top 100 :left 320 :side :overlap} r))))

(deftest rtl-does-not-prevent-a-clear-horizontal-placement
  (let [r (place {:selection {:top -200 :bottom 1400 :left 680 :right 1180}
                  :container {:top 100 :bottom 700 :left 650 :right 1200}
                  :focus-x 1180 :rtl? true})]
    (is (= {:top 100 :left 352 :side :left} r))))

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
;; Placement invariants
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

(defn- placement-cases []
  (for [con container-shapes
        view viewport-shapes
        menu-height [80 300 480]
        sel-top (range -400 1000 37)
        sel-height [18 40 120 300 600 1200]
        horizontal [nil
                    {:left 30 :right 400}
                    {:left 600 :right 1180}
                    {:left -800 :right -300}
                    {:left 1400 :right 1800}]
        reserve [0 placement/touch-reserve]]
    {:selection (merge {:top sel-top :bottom (+ sel-top sel-height)} horizontal)
     :container (merge con horizontal)
     :viewport view :menu {:width 320 :height menu-height}
     :focus-x 500 :rtl? false :reserve reserve :allow-overlap? true}))

(deftest visible-selections-always-have-a-placement
  (let [failures (for [opts (placement-cases)
                       :let [r (placement/place opts)]
                       :when (not= (some? (visible-box opts)) (some? r))]
                   {:opts opts :result r})]
    (is (empty? failures)
      (str "Visibility mismatches: " (pr-str (take 5 failures))))))

(deftest only-the-final-tier-can-cover-selected-text
  (let [failures (for [opts (placement-cases)
                       :let [r (placement/place opts)]
                       :when (and (intersects? r (:menu opts) (visible-box opts))
                               (not= :overlap (:side r)))]
                   {:opts opts :result r})]
    (is (empty? failures)
      (str "Clear tiers covered selected text: " (pr-str (take 5 failures))))))

(deftest overlap-requires-that-no-clear-region-fits
  (let [failures (for [opts (placement-cases)
                       :let [r (placement/place opts)]
                       :when (and (= :overlap (:side r)) (clear-region-fits? opts))]
                   {:opts opts :result r})]
    (is (empty? failures)
      (str "Overlap ignored a clear region: " (pr-str (take 5 failures))))))

(deftest placements-stay-inside-the-viewport
  (let [failures (for [opts (placement-cases)
                       :let [r (placement/place opts)
                             {:keys [width height]} (:menu opts)
                             view (:viewport opts)]
                       :when (and r
                               (not (and (<= placement/margin (:left r)
                                            (- (:width view) placement/margin width))
                                      (<= placement/margin (:top r)
                                        (- (:height view) placement/margin height)))))]
                   {:opts opts :result r})]
    (is (empty? failures)
      (str "Placements outside the viewport: " (pr-str (take 5 failures))))))

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
  (let [deep-pane {:top 100 :bottom 1400}]
    (testing "a selection fills the visible pane"
      (let [r (place {:selection deep-pane :container deep-pane})]
        (is (= :overlap (:side r)))
        (is (<= placement/margin (:top r) (- 800 placement/margin 300)))))
    (testing "a shorter selection leaves room below"
      (let [r (place {:selection {:top 200 :bottom 300} :container deep-pane})]
        (is (= (+ 300 placement/gap) (:top r)))
        (is (<= (+ (:top r) 300) (- (:height viewport) placement/margin)))))))

(deftest a-placement-exists-whenever-one-fits
  (testing "a short menu beside a huge selection is placed, not hidden"
    (is (some? (place {:selection {:top -500 :bottom 1500}
                       :menu {:width 320 :height 80}}))))
  (testing "a menu taller than the room below the pane remains available"
    (is (= :overlap (:side (place {:selection {:top -500 :bottom 1500}
                                   :menu {:width 320 :height 200}}))))))

;; ---------------------------------------------------------------------------
;; Touch: the browser's own selection bar
;; ---------------------------------------------------------------------------

(deftest touch-reserve-clears-the-native-selection-bar
  (testing "the iPhone report: selection near the pane top, card goes below the bar"
    ;; 414x896 viewport, pane under a ~140px command bar, card ~210px tall.
    (let [phone {:width 414 :height 896}
          phone-pane {:top 140 :bottom 650}
          sel {:top 150 :bottom 285}
          r (place {:selection sel :container phone-pane :viewport phone
                    :menu {:width 270 :height 210}
                    :reserve placement/touch-reserve})]
      (is (= :below (:side r)))
      (is (= (+ 285 placement/gap placement/touch-reserve) (:top r)))))
  (testing "above placement keeps the same clearance"
    ;; Below needs 560+68+300 = 928 > 700; above gives 480-68-300 = 112 >= 100.
    (let [sel {:top 480 :bottom 560}
          r (place {:selection sel :reserve placement/touch-reserve})]
      (is (= :above (:side r)))
      (is (= (- 480 placement/gap placement/touch-reserve 300) (:top r)))))
  (testing "clear tiers preserve the reserve across every shape"
    (let [clearance (+ placement/gap placement/touch-reserve)
          failures (for [opts (placement-cases)
                         :when (= placement/touch-reserve (:reserve opts))
                         :let [r (placement/place opts)
                               box (visible-box opts)]
                         :when (and r box (not= :overlap (:side r)))
                         :let [reserved-box (-> box
                                              (update :top - clearance)
                                              (update :bottom + clearance)
                                              (update :left - clearance)
                                              (update :right + clearance))]
                         :when (intersects? r (:menu opts) reserved-box)]
                     {:opts opts :result r})]
      (is (empty? failures)
        (str "Clear tiers inside the reserve: " (pr-str (take 5 failures)))))))
