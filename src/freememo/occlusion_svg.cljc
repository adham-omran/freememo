(ns freememo.occlusion-svg
  "Pure generation of image-occlusion mask SVGs, format-compatible with the
   Image Occlusion Enhanced add-on (empty Labels group, Masks group of rects,
   question rect in the add-on's red with class=\"qshape\").

   Coordinates and the svg width/height are natural-image pixels; the pushed
   CSS forces both the overlay svg and the image to width:100%, so they scale
   together and the masks stay aligned.

   Naming scheme (fm- prefix avoids collisions with add-on media):
     fm-<anki-key>-<ordinal>-Q.svg   question mask, per note
     fm-<anki-key>-<ordinal>-A.svg   answer mask, per note
     fm-<anki-key>-O.svg             original mask (all rects), per group
     fm-<anki-key>-<ordinal>         note's 'ID (hidden)' field

   Mode semantics (mode lives entirely in the SVG contents — one Anki card
   template serves both):
     hide-all : Q = every rect, the asked one highlighted; A = every rect
                minus the asked one (asked area revealed, others stay hidden).
     hide-one : Q = only the asked rect; A = no rects (everything revealed)."
  (:require [clojure.string :as str]))

(def question-fill "#FF7E7E")
(def other-fill "#FFEBA2")
(def stroke-color "#2D2D2D")

(defn note-hidden-id
  "Value of the note's 'ID (hidden)' field."
  [anki-key ordinal]
  (str "fm-" anki-key "-" ordinal))

(defn media-filenames
  "Anki media filenames for one mask: {:q .. :a ..} plus the group's :o."
  [anki-key ordinal]
  {:q (str "fm-" anki-key "-" ordinal "-Q.svg")
   :a (str "fm-" anki-key "-" ordinal "-A.svg")
   :o (str "fm-" anki-key "-O.svg")})

(defn- rect-el
  [{:keys [x y w h ordinal]} anki-key question?]
  (str "  <rect fill=\"" (if question? question-fill other-fill) "\""
    " stroke=\"" stroke-color "\""
    " x=\"" x "\" y=\"" y "\""
    " width=\"" w "\" height=\"" h "\""
    " id=\"" (note-hidden-id anki-key ordinal) "\""
    (when question? " class=\"qshape\"")
    "/>"))

(defn- mask-svg
  [width height rect-lines]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
    " width=\"" width "\" height=\"" height "\">\n"
    " <!-- Created with FreeMemo -->\n"
    " <g>\n  <title>Labels</title>\n </g>\n"
    " <g>\n  <title>Masks</title>\n"
    (str/join "\n" rect-lines)
    (when (seq rect-lines) "\n")
    " </g>\n</svg>"))

(defn question-mask-svg
  "The mask shown on the card front for the mask `ordinal`."
  [anki-key {:keys [width height rects]} mode ordinal]
  (mask-svg width height
    (case mode
      "hide-all" (mapv (fn [r] (rect-el r anki-key (= ordinal (:ordinal r)))) rects)
      "hide-one" (mapv (fn [r] (rect-el r anki-key true))
                   (filter #(= ordinal (:ordinal %)) rects)))))

(defn answer-mask-svg
  "The mask shown on the card back for the mask `ordinal`."
  [anki-key {:keys [width height rects]} mode ordinal]
  (mask-svg width height
    (case mode
      "hide-all" (mapv (fn [r] (rect-el r anki-key false))
                   (remove #(= ordinal (:ordinal %)) rects))
      "hide-one" [])))

(defn original-mask-svg
  "All rects, neutral color — the group's shared Original Mask field."
  [anki-key {:keys [width height rects]}]
  (mask-svg width height
    (mapv (fn [r] (rect-el r anki-key false)) rects)))
