(ns freememo.reading-anchor
  "The pure half of reading-position memory (plans/reading-position-memory.md
   §4.2): where a reader is, as a block anchor, and where that anchor lands in
   content that may have changed.

   No reader conditionals and no require beyond `clojure.string`, so the JVM
   tests reach every rule here and both peers read the same vocabulary
   (`surfaces`, `editor-kinds`).

   A BLOCK is one unit of the surface's content in document order: a child of
   `.ql-editor`, a `.poem-verse`, a child of the rendered Markdown, or one
   source line. Its `top` and `height` are in the scroller's CONTENT
   coordinates, so `top` of the first block is about the padding and a
   `scroll-top` compares with it directly.

   The core fns take a block COUNT and accessor fns rather than a vector. A
   capture runs once per animation frame while a reader scrolls, and a binary
   search over accessors reads O(log n) element rects where a vector would read
   all n. The vector forms (`capture`, `resolve-index`, `target-scroll-top`) are
   the same rules over a vector of `{:top :height :text}`, for the tests and for
   callers that already hold one."
  (:require [clojure.string :as str]))

(def surfaces
  "Every scrollable surface a position is kept for. The CHECK constraint on
   `reading_positions.surface` lists the same four strings."
  #{"editor" "poem" "code-rendered" "code-source"})

(def editor-kinds
  "Topic kinds whose notes editor keeps a position (§4 0.1). An extract and an
   EPUB chapter are `basic`. `poem` is absent because a poem has no editor: its
   reader is the `poem` surface. Every PDF-backed and media kind is absent
   (§4 0.2)."
  #{"basic" "web" "wikipedia" "epub" "markdown" "supermemo" "code"})

(def fingerprint-chars
  "Length of a block fingerprint. The server rejects a longer one (§4 1.6)."
  64)

(defn- clamp [lo hi x] (max lo (min hi x)))

(defn fingerprint
  "Post: `text` with every whitespace run collapsed to one space, trimmed, and
   cut to `fingerprint-chars`. Two blocks that differ only in whitespace give
   the same string. nil text gives \"\"."
  [text]
  (let [collapsed (str/trim (str/replace (str (or text "")) #"[\s\u00a0]+" " "))]
    (if (> (count collapsed) fingerprint-chars)
      (subs collapsed 0 fingerprint-chars)
      collapsed)))

(defn first-visible-index
  "Post: the least index i in [0, n) whose block bottom lies below `scroll-top`,
   or n − 1 when none does, or nil when n = 0.
   Pre : `bottom-of` is non-decreasing in i, which document order gives."
  [n bottom-of scroll-top]
  (when (pos? n)
    (loop [lo 0 hi (dec n)]
      (if (< lo hi)
        (let [mid (quot (+ lo hi) 2)]
          (if (> (bottom-of mid) scroll-top)
            (recur lo mid)
            (recur (inc mid) hi)))
        lo))))

(defn block-fraction
  "Post: how far `scroll-top` sits into a block at `top` of `height`, in [0, 1].
   A zero-height block reads 0."
  [top height scroll-top]
  (if (pos? height)
    (double (clamp 0 1 (/ (- scroll-top top) height)))
    0.0))

(defn scroll-fraction
  "Post: progress through the whole scroller, in [0, 1]. A scroller with nothing
   to scroll reads 0."
  [scroll-top client-height scroll-height]
  (double (clamp 0 1 (/ scroll-top (max 1 (- scroll-height client-height))))))

(defn capture*
  "Post: {:anchor {:i :fp :frac} :fraction}, or nil when n = 0.
   Pre : `top-of`, `height-of` and `text-of` take an index in [0, n)."
  [n top-of height-of text-of scroll-top client-height scroll-height]
  (when-let [i (first-visible-index n #(+ (top-of %) (height-of %)) scroll-top)]
    {:anchor {:i i
              :fp (fingerprint (text-of i))
              :frac (block-fraction (top-of i) (height-of i) scroll-top)}
     :fraction (scroll-fraction scroll-top client-height scroll-height)}))

(defn capture
  "§4 2.1 over a vector of `{:top :height :text}`."
  [blocks scroll-top client-height scroll-height]
  (capture* (count blocks) #(:top (nth blocks %)) #(:height (nth blocks %))
    #(:text (nth blocks %)) scroll-top client-height scroll-height))

(defn resolve-index*
  "Post: the block `anchor` names in content of `n` blocks (§4 2.3):
   1. the block whose fingerprint equals the anchor's, nearest to its `i`,
      the lower index on a tie;
   2. else `i` clamped to [0, n − 1];
   3. nil when n = 0.
   Pre : `text-of` takes an index in [0, n)."
  [n text-of anchor]
  (when (pos? n)
    (let [i (clamp 0 (dec n) (or (:i anchor) 0))
          fp (:fp anchor)
          match? (fn [j] (and (< -1 j n) (= fp (fingerprint (text-of j)))))
          reach (max i (- n 1 i))]
      (or (when (some? fp)
            (some (fn [d]
                    (cond (match? (- i d)) (- i d)
                          (match? (+ i d)) (+ i d)))
              (range 0 (inc reach))))
        i))))

(defn resolve-index
  "§4 2.3 over a vector of `{:top :height :text}`."
  [blocks anchor]
  (resolve-index* (count blocks) #(:text (nth blocks %)) anchor))

(defn target-scroll-top*
  "Post: the scroll-top that puts `frac` of block `index`, at `top` of
   `height`, above the scroller's top edge. Block 0 at fraction 0 is the top of
   the scroller instead, because a capture above the first block clamps to
   that anchor, and whatever sits above block 0 (padding, a poem's header) was
   in view."
  [index top height frac]
  (let [frac (clamp 0 1 (or frac 0))]
    (if (and (zero? index) (zero? frac))
      0
      (+ top (* frac height)))))

(defn target-scroll-top
  "§4 2.4 over a vector of `{:top :height :text}`."
  [blocks index frac]
  (let [b (nth blocks index)]
    (target-scroll-top* index (:top b) (:height b) frac)))

(defn valid-anchor?
  "§4 1.6: whether a posted anchor is one this namespace could have made."
  [{:keys [i fp frac]} fraction]
  (and (int? i) (<= 0 i)
    (string? fp) (<= (count fp) fingerprint-chars)
    (number? frac) (<= 0 frac 1)
    (number? fraction) (<= 0 fraction 1)))
