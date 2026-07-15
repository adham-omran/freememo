(ns freememo.live-doc-test
  "Geometry guards for the Live-Document image pipeline: rotate-image swaps
   dimensions on quarter turns, crop-image cuts the right normalized region in
   post-rotation space, and the rotate→crop composition (as add-image-page!
   wires it) lands the expected pixels."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.live-doc :as live-doc])
  (:import [java.awt.image BufferedImage]))

(def ^:private red   (unchecked-int 0xFFFF0000))
(def ^:private green (unchecked-int 0xFF00FF00))

(defn- solid
  "w×h TYPE_INT_RGB image; (fill x y) → packed RGB for that pixel."
  ^BufferedImage [w h fill]
  (let [img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)]
    (doseq [x (range w) y (range h)]
      (.setRGB img x y (fill x y)))
    img))

(defn- rgb [^BufferedImage img x y] (bit-and (.getRGB img x y) 0xFFFFFF))
(def ^:private rotate-image #'live-doc/rotate-image)
(def ^:private crop-image #'live-doc/crop-image)

(deftest rotate-image-dims
  (testing "0° is an identity (same object)"
    (let [img (solid 30 10 (constantly red))]
      (is (identical? img (rotate-image img 0)))))
  (testing "90°/270° swap width and height"
    (let [img (solid 30 10 (constantly red))]
      (doseq [d [90 270]]
        (let [r (rotate-image img d)]
          (is (= 10 (.getWidth r)) (str "width after " d))
          (is (= 30 (.getHeight r)) (str "height after " d))))))
  (testing "180° preserves dimensions"
    (let [img (solid 30 10 (constantly red))
          r (rotate-image img 180)]
      (is (= 30 (.getWidth r)))
      (is (= 10 (.getHeight r))))))

(deftest crop-image-region
  (testing "nil crop is an identity (same object)"
    (let [img (solid 20 20 (constantly red))]
      (is (identical? img (crop-image img nil)))))
  (testing "full-frame crop is a no-op (same object)"
    (let [img (solid 20 20 (constantly red))]
      (is (identical? img (crop-image img {:x 0.0 :y 0.0 :w 1.0 :h 1.0})))))
  (testing "right-half crop keeps only the right half's pixels"
    ;; left half red, right half green.
    (let [img (solid 100 100 (fn [x _] (if (< x 50) red green)))
          r (crop-image img {:x 0.5 :y 0.0 :w 0.5 :h 1.0})]
      (is (= 50 (.getWidth r)))
      (is (= 100 (.getHeight r)))
      ;; result(0,0) is src(50,0) → green.
      (is (= (bit-and green 0xFFFFFF) (rgb r 0 0)))
      (is (= (bit-and green 0xFFFFFF) (rgb r 49 99)))))
  (testing "out-of-range fractions clamp to the image bounds without throwing"
    (let [img (solid 40 40 (constantly red))
          r (crop-image img {:x 0.9 :y 0.9 :w 1.0 :h 1.0})]
      (is (<= 1 (.getWidth r) 40))
      (is (<= 1 (.getHeight r) 40)))))

(deftest rotate-then-crop-composition
  (testing "rotate 90° then crop top-left quarter yields the expected size"
    ;; add-image-page! composes as (-> decoded (rotate-image deg) (crop-image crop)).
    (let [img (solid 200 100 (constantly green))
          rotated (rotate-image img 90)          ; → 100×200
          cropped (crop-image rotated {:x 0.0 :y 0.0 :w 0.5 :h 0.5})]
      (is (= 100 (.getWidth rotated)))
      (is (= 200 (.getHeight rotated)))
      (is (= 50 (.getWidth cropped)))
      (is (= 100 (.getHeight cropped))))))
