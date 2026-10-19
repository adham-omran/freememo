(ns freememo.menu-placement-test
  "plans/custom-context-menu.md 3.2: a sweep over anchors near every edge finds
   no overflow."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.menu-placement :as placement]))

(def m placement/margin)

(defn- inside? [{:keys [left top]} {mw :width mh :height} {vw :width vh :height}]
  (and (<= m left) (<= (+ left mw) (- vw m))
       (<= m top) (<= (+ top mh) (- vh m))))

(def viewports [{:width 1600 :height 900} {:width 390 :height 700} {:width 800 :height 300}])
(def menus [{:width 220 :height 180} {:width 260 :height 290} {:width 200 :height 40}])

(defn- fits? [menu vp]
  (and (<= (+ (:width menu) (* 2 m)) (:width vp))
       (<= (+ (:height menu) (* 2 m)) (:height vp))))

(deftest a-point-anchor-never-overflows
  (doseq [vp viewports menu menus :when (fits? menu vp)
          x (range 0 (inc (:width vp)) 13)
          y (range 0 (inc (:height vp)) 11)]
    (let [pos (placement/place {:x x :y y} menu vp)]
      (is (inside? pos menu vp) (pr-str {:vp vp :menu menu :x x :y y :pos pos})))))

(deftest a-rect-anchor-never-overflows
  (doseq [vp viewports menu menus :when (fits? menu vp)
          x (range 0 (- (:width vp) 20) 17)
          y (range 0 (- (:height vp) 20) 19)]
    (let [rect {:left x :top y :right (+ x 20) :bottom (+ y 20)}
          pos (placement/place rect menu vp)]
      (is (inside? pos menu vp) (pr-str {:vp vp :menu menu :rect rect :pos pos})))))

(deftest a-point-with-room-opens-at-the-point
  (is (= {:left 100 :top 120}
        (placement/place {:x 100 :y 120} {:width 200 :height 100} {:width 1000 :height 800}))))

(deftest a-point-near-the-far-corner-flips-to-end-at-the-point
  (is (= {:left 700 :top 650}
        (placement/place {:x 900 :y 750} {:width 200 :height 100} {:width 1000 :height 800}))))

(deftest a-rect-opens-below-and-flips-above
  (testing "below its left edge when there is room"
    (is (= {:left 50 :top 140}
          (placement/place {:left 50 :top 120 :right 80 :bottom 140}
            {:width 200 :height 100} {:width 1000 :height 800}))))
  (testing "above its top edge near the bottom"
    (is (= {:left 50 :top 650}
          (placement/place {:left 50 :top 750 :right 80 :bottom 770}
            {:width 200 :height 100} {:width 1000 :height 800})))))

(deftest a-menu-taller-than-the-viewport-sits-at-the-top-margin
  (is (= m (:top (placement/place {:x 10 :y 200} {:width 100 :height 2000}
                   {:width 500 :height 400})))))
