(ns freememo.occlusion-svg
  "Pure generation of image-occlusion mask SVGs, format-compatible with the
   Image Occlusion Enhanced add-on (Labels group, Masks group of rects, question
   rect in the add-on's red with class=\"qshape\").

   Coordinates and the svg width/height are natural-image pixels; the pushed
   CSS forces both the overlay svg and the image to width:100%, so they scale
   together and the masks stay aligned.

   Naming scheme (fm- prefix avoids collisions with add-on media):
     fm-<anki-key>-<ordinal>-Q.svg   question mask, per note
     fm-<anki-key>-<ordinal>-A.svg   answer mask, per note
     fm-<anki-key>-O.svg             original mask (all rects), per group
     fm-<anki-key>-<ordinal>         note's 'ID (hidden)' field

   Mask groups: an ordinal identifies a mask GROUP, so several rects may carry
   the same one and every rect of the asked group is highlighted/revealed
   together — the ordinal filters below need no grouping logic of their own.
   Rects of one group therefore share an `id`; the ids are inert here (no
   selector in freememo-anki.css and no reader in this app) and the note's
   'ID (hidden)' field stays the per-group value.

   Mode semantics (mode lives entirely in the SVG contents — one Anki card
   template serves both):
     hide-all : Q = every rect, the asked group highlighted; A = every rect
                minus the asked group (asked area revealed, others stay hidden).
     hide-one : Q = only the asked group's rects; A = no rects (all revealed).

   LABELS are user-authored text the occlusion editor places on the image, so
   an unlabelled diagram can be occluded at all. Every label goes into EVERY
   generated svg — Q, A and O alike, in both modes — because a label is content
   the image lacks, not a per-card mask. The Labels group is emitted BEFORE the
   Masks group, and that order is the whole occlusion mechanism: a rect drawn
   over a label paints on top of it on the question, and the answer drops the
   rect and reveals the label underneath.

   Three properties of the emitted text are load-bearing:

   - `text-anchor=\"start\"` with `dominant-baseline=\"text-before-edge\"` makes
     the stored (x, y) the top-left corner, which is also Konva.Text's origin.
     The editor and the card therefore place a label from the SAME two numbers,
     with nothing measured or converted. `label-baseline` is the single place to
     retune if a webview disagrees.
   - NO `dir`, `direction` or `unicode-bidi`. Unstyled text is what the add-on
     writes for Arabic (verified against a real note), and it is what keeps the
     editor and the card in agreement: Konva draws through canvas fillText,
     whose default base direction is LTR with bidi reordering inside the run —
     the same thing an unstyled svg text does.
   - `xml:space=\"preserve\"`, so a leading space survives and an empty line can
     hold its height with a single space. Without it a blank line collapses,
     because `dy` needs a character to apply to.

   Text does NOT wrap. `label-line-height` and one tspan per line are the whole
   multi-line mechanism, and only the user's own newlines break a line."
  (:require [clojure.string :as str]))

(def question-fill "#FF7E7E")
(def other-fill "#FFEBA2")
(def stroke-color "#2D2D2D")

;; Label vocabulary. Public because the Konva editor draws the same label and
;; must match it glyph for glyph — it is the WYSIWYG authority for what ships.
(def label-fill "#000000")
(def label-halo "#FFFFFF")
(def label-font-family
  "System families only. An svg inside an <img> renders in secure static mode
   and cannot load a font, so a webfont here would silently fall back."
  "Arial, Helvetica, sans-serif")
(def label-line-height 1.2)
(def label-baseline "text-before-edge")

;; Size bounds live here, not in the editor, for two reasons. The Konva editor
;; clamps to them and the modal's size control states them as input bounds, so
;; one home keeps the two from drifting. And this namespace is a .cljc with no
;; reader conditional, so BOTH peers see plain literals — a CLJS-only def read
;; from a reactive body would leave the peers disagreeing on frame shape (see
;; CLAUDE.md on transcribe_language.cljc).
(def min-label-size
  "Smallest font size a label may hold, in natural px. Below this the halo
   swallows the glyph."
  6)

(def max-label-size
  "Largest font size a label may hold, in natural px."
  400)

(defn halo-width
  "Halo stroke width for a label at `size`, in natural pixels.
   Post: proportional to the font size and never below 1 — a fixed width chokes
         a small glyph and disappears behind a large one."
  [size]
  (max 1 (int (+ 0.5 (/ size 8)))))

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

(defn- xml-escape
  "Escape label text for XML character data. `&` first, or the escapes of the
   other two get escaped again."
  [s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")))

(defn- rect-el
  [{:keys [x y w h ordinal]} anki-key question?]
  (str "  <rect fill=\"" (if question? question-fill other-fill) "\""
    " stroke=\"" stroke-color "\""
    " x=\"" x "\" y=\"" y "\""
    " width=\"" w "\" height=\"" h "\""
    " id=\"" (note-hidden-id anki-key ordinal) "\""
    (when question? " class=\"qshape\"")
    "/>"))

(defn- text-el
  "One label. Pre: element is {:type \"text\" :x :y :size :text} in natural px,
   with :text non-blank (freememo.occlusion/checked-geometry guarantees it).
   Post: one <text> holding one <tspan> per line; the first line sits at the
         element's own y and each later line steps down by label-line-height."
  [{:keys [x y size text]}]
  (let [lines (str/split text #"\n" -1)]
    (str "  <text xml:space=\"preserve\""
      " x=\"" x "\" y=\"" y "\""
      " font-family=\"" label-font-family "\" font-size=\"" size "\""
      " text-anchor=\"start\" dominant-baseline=\"" label-baseline "\""
      " fill=\"" label-fill "\""
      " stroke=\"" label-halo "\" stroke-width=\"" (halo-width size) "\""
      " paint-order=\"stroke fill\">"
      (str/join
        (map-indexed
          (fn [i line]
            (str "<tspan x=\"" x "\""
              (when (pos? i) (str " dy=\"" label-line-height "em\""))
              ">"
              ;; A blank line needs a character for its dy to apply to.
              (if (= "" line) " " (xml-escape line))
              "</tspan>"))
          lines))
      "</text>")))

(defn- group-el
  "One titled svg group. An empty `lines` yields exactly the group the previous
   label-free generator emitted, so a group with no labels still pushes
   byte-identical media."
  [title lines]
  (str " <g>\n  <title>" title "</title>\n"
    (str/join "\n" lines)
    (when (seq lines) "\n")
    " </g>\n"))

(defn- mask-svg
  [width height label-lines rect-lines]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\""
    " width=\"" width "\" height=\"" height "\">\n"
    " <!-- Created with FreeMemo -->\n"
    (group-el "Labels" label-lines)
    (group-el "Masks" rect-lines)
    "</svg>"))

(defn- label-lines
  "The Labels group's lines. Post: one per element, in stored order — the same
   in every svg of the group."
  [elements]
  (mapv text-el elements))

(defn question-mask-svg
  "The mask shown on the card front for the mask `ordinal`."
  [anki-key {:keys [width height rects elements]} mode ordinal]
  (mask-svg width height
    (label-lines elements)
    (case mode
      "hide-all" (mapv (fn [r] (rect-el r anki-key (= ordinal (:ordinal r)))) rects)
      "hide-one" (mapv (fn [r] (rect-el r anki-key true))
                   (filter #(= ordinal (:ordinal %)) rects)))))

(defn answer-mask-svg
  "The mask shown on the card back for the mask `ordinal`."
  [anki-key {:keys [width height rects elements]} mode ordinal]
  (mask-svg width height
    (label-lines elements)
    (case mode
      "hide-all" (mapv (fn [r] (rect-el r anki-key false))
                   (remove #(= ordinal (:ordinal %)) rects))
      "hide-one" [])))

(defn original-mask-svg
  "All rects, neutral color — the group's shared Original Mask field."
  [anki-key {:keys [width height rects elements]}]
  (mask-svg width height
    (label-lines elements)
    (mapv (fn [r] (rect-el r anki-key false)) rects)))
