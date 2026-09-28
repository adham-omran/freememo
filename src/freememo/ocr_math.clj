(ns freememo.ocr-math
  "Rewrite math that an OCR model wrote as HTML into stored-form `\\(TeX\\)`.

   The OCR prompt asks for `\\(…\\)`, and a model can still answer with
   `<em>h</em><sub><em>qm</em></sub>` and invented entities such as `&partial;`.
   Nothing after the model call reads that as math, and `clean-html` escapes an
   unknown entity to literal text. This pass runs in `freememo.ocr/run-ocr`,
   before `clean-html`, for both OCR lanes.

   Two shapes are converted. An EQUATION PARAGRAPH (`p`/`li` holding a relation,
   a script or a math symbol, and no prose word) becomes one formula, with a
   leading `(n)` equation number and trailing punctuation left outside. Anywhere
   else, a `sub`/`sup` SEED grows greedily over adjacent math tokens and becomes
   an inline formula. See plans/ocr-math-html-to-tex.md §3."
  (:require [clojure.string :as str]
            [freememo.math :as math])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element Node TextNode]))

;; ── Vocabulary ──────────────────────────────────────────────────────────────

(def ^:private invented-entities
  "Entity names a model writes that HTML does not define, and the character each
   meant. Jsoup keeps an unknown entity as literal text, so the repair reads text
   nodes. `&part;` is the real name for ∂ and needs no entry."
  {"&partial;" "∂"})

(def ^:private greek
  (merge
    (zipmap "αβγδεζηθικλμνξπρστυχψω"
      (map #(str "\\" %) ["alpha" "beta" "gamma" "delta" "varepsilon" "zeta" "eta"
                          "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "pi" "rho"
                          "sigma" "tau" "upsilon" "chi" "psi" "omega"]))
    {\ο "o" \ς "\\varsigma" \φ "\\varphi" \ϕ "\\phi" \ϵ "\\epsilon" \ϑ "\\vartheta"
     \ϖ "\\varpi"}
    (zipmap "ΓΔΘΛΞΠΣΥΦΨΩ"
      (map #(str "\\" %) ["Gamma" "Delta" "Theta" "Lambda" "Xi" "Pi" "Sigma"
                          "Upsilon" "Phi" "Psi" "Omega"]))
    ;; No TeX command exists for these capitals. They are the Latin glyphs.
    (zipmap "ΑΒΕΖΗΙΚΜΝΟΡΤΧ" (map str "ABEZHIKMNOPTX"))))

(def ^:private symbols
  {\∂ "\\partial" \− "-" \′ "'" \″ "''" \‴ "'''" \× "\\times" \· "\\cdot"
   \≤ "\\le" \≥ "\\ge" \≠ "\\ne" \≈ "\\approx" \≡ "\\equiv" \∞ "\\infty"
   \∑ "\\sum" \∫ "\\int"})

(def ^:private tex-specials
  {\% "\\%" \# "\\#" \& "\\&" \{ "\\{" \} "\\}"})

(def ^:private relations #{\= \< \> \≤ \≥ \≠ \≈ \≡})

(def ^:private operators
  "Characters an inline formula grows over (§3.5.2), plus the symbols of
   `symbols`, which are math wherever they sit."
  (into (set "=<>+-/^()") (keys symbols)))

(def ^:private stops
  "Punctuation that ends an inline formula when whitespace or the block end
   follows it (§3.5.3)."
  #{\. \, \; \:})

(def ^:private code-selector
  "Mirrors `math/code-selector`: text in these is source, not prose."
  "pre, code, .ql-code-block-container")

;; ── TeX translation (§3.6) ──────────────────────────────────────────────────

(def ^:private word-end
  "Marks the end of an emitted control word. `finish-tex` turns it into a space
   when a letter follows, so `∂` + `m` gives `\\partial m`, not `\\partialm`,
   across element boundaries too."
  \uE000)

(defn- char-tex [c]
  (if-let [cmd (or (symbols c) (greek c))]
    (if (re-matches #"\\[A-Za-z]+" cmd) (str cmd word-end) cmd)
    (or (tex-specials c) (str c))))

(defn- text-tex [s]
  (apply str (map char-tex (str/replace s #"\s+" " "))))

(defn- node-tex
  "TeX for `node`: text translated, `sub`/`sup` as scripts, every other tag
   dropped with its content kept."
  [^Node node]
  (if (instance? TextNode node)
    (text-tex (.getWholeText ^TextNode node))
    (let [inner (apply str (map node-tex (.childNodes node)))]
      (case (.normalName ^Element node)
        "sub" (str "_{" inner "}")
        "sup" (str "^{" inner "}")
        inner))))

(defn- finish-tex [s]
  (-> s
    (str/replace (re-pattern (str word-end "(?=[A-Za-z])")) " ")
    (str/replace (str word-end) "")
    str/trim))

;; ── Tokens ──────────────────────────────────────────────────────────────────
;; A block's direct children become a flat token vector. Text splits into
;; :ws / :num / :greek / :word / :char, an existing `\(…\)` region is one opaque
;; :tex token, and an element is :em, :script or an opaque :other.

(def ^:private text-token-re
  (re-pattern (str "\\s+|\\d+(?:\\.\\d+)?|[" (apply str (keys greek)) "]"
                "|[\\p{L}&&[^\\u0370-\\u03FF]]+|[\\s\\S]")))

(defn- text-tokens [s]
  (for [m (re-seq text-token-re s)]
    {:type (cond (re-matches #"\s+" m) :ws
                 (re-matches #"\d.*" m) :num
                 (greek (first m)) :greek
                 (re-matches #"\p{L}+" m) :word
                 :else :char)
     :s m}))

(defn- text-node-tokens [s]
  (let [parts (str/split s math/tex-region-re -1)
        regions (map first (re-seq math/tex-region-re s))]
    (mapcat (fn [part region]
              (concat (text-tokens part) (when region [{:type :tex :s region}])))
      parts (concat regions [nil]))))

(defn- element-token [^Element el]
  (case (.normalName el)
    ("em" "i") {:type :em :node el}
    ("sub" "sup") {:type :script :node el}
    {:type :other :node el}))

(defn- block-tokens [^Element block]
  (vec (mapcat (fn [n]
                 (if (instance? TextNode n)
                   (text-node-tokens (.getWholeText ^TextNode n))
                   [(element-token n)]))
         (.childNodes block))))

(defn- token-text [{:keys [s node]}]
  (or s (.text ^Element node)))

(defn- token-tex [{:keys [s node]}]
  (if s (text-tex s) (node-tex node)))

(defn- has-script? [{:keys [type node]}]
  (or (= :script type)
      (and (= :em type) (seq (.select ^Element node "sub, sup")))))

(defn- footnote?
  "A digit-only `sup` with no math base before it is a footnote marker (§3.5.4)."
  [tokens i]
  (let [{:keys [type node]} (tokens i)]
    (and (= :script type)
         (= "sup" (.normalName ^Element node))
         (re-matches #"\d+" (.text ^Element node))
         (not (#{:em :num :greek} (:type (get tokens (dec i))))))))

(defn- seed? [tokens i]
  (and (has-script? (tokens i)) (not (footnote? tokens i))))

(defn- prose-word?
  "A plain word of 3+ letters that is not a script's base. `vmh` in
   `vmh<sub>m</sub>` is a base, so an equation the model wrote without `<em>`
   still reads as math."
  [tokens i]
  (let [{:keys [type s]} (tokens i)]
    (and (= :word type) (>= (count s) 3)
         (not (has-script? (get tokens (inc i) {}))))))

;; ── Equation paragraphs (§3.4) ──────────────────────────────────────────────

(defn- equation-block?
  "Pre: `tokens` are one block's. Post: true iff the block is one formula, with
   at most an equation number and trailing punctuation outside it."
  [tokens]
  (let [text (apply str (map token-text tokens))]
    (and (not-any? (comp #{:other :tex} :type) tokens)
         (or (some has-script? tokens)
             (some #(or (symbols %) (greek %)) text))
         (some relations text)
         (not-any? #(prose-word? tokens %) (range (count tokens))))))

(defn- equation-number-end
  "Index after a leading `(n)` and the whitespace behind it, else 0."
  [tokens]
  (let [i (if (= :ws (:type (get tokens 0))) 1 0)
        [open n close] (map #(:s (get tokens %)) (range i (+ i 3)))]
    (if (and (= "(" open) (re-matches #"\d+" (str n)) (= ")" close))
      (let [j (+ i 3)] (if (= :ws (:type (get tokens j))) (inc j) j))
      0)))

;; ── Inline seeds (§3.5) ─────────────────────────────────────────────────────

(defn- grows-over? [tokens i]
  (let [{:keys [type s node] :as t} (get tokens i)]
    (case type
      (:num :greek) true
      :word (= 1 (count s))
      :char (let [c (first s)]
              (or (operators c)
                  ;; `(q,m)` keeps its comma, `q, m` does not.
                  (and (stops c)
                       (not (#{:ws nil} (:type (get tokens (inc i))))))))
      :em (let [txt (.text ^Element node)]
            (not (re-find #"\s|\p{L}{4}" txt)))
      :script (not (footnote? tokens i))
      false)))

(defn- grow
  "Extend [lo hi] (inclusive) one token or one whitespace-plus-token at a time,
   in direction `step`, while the next non-whitespace token is math."
  [tokens lo hi]
  (let [next-math (fn [i step]
                    (let [j (if (= :ws (:type (get tokens i))) (+ i step) i)]
                      (when (grows-over? tokens j) j)))
        lo (loop [lo lo] (if-let [j (next-math (dec lo) -1)] (recur j) lo))
        hi (loop [hi hi] (if-let [j (next-math (inc hi) 1)] (recur j) hi))]
    [lo hi]))

(defn- balance
  "Shrink [lo hi] until its ends are neither whitespace, stop punctuation nor an
   unmatched parenthesis. Nil when nothing is left."
  [tokens lo hi]
  (let [s #(:s (get tokens %))
        ws? #(= :ws (:type (get tokens %)))
        depth (fn [lo hi] (reduce (fn [d i] (case (s i) "(" (inc d) ")" (dec d) d))
                            0 (range lo (inc hi))))]
    (loop [lo lo hi hi]
      (cond
        (> lo hi) nil
        (ws? lo) (recur (inc lo) hi)
        (or (ws? hi) (stops (first (s hi)))) (recur lo (dec hi))
        (and (= ")" (s hi)) (neg? (depth lo hi))) (recur lo (dec hi))
        (and (= "(" (s lo)) (pos? (depth lo hi))) (recur (inc lo) hi)
        :else [lo hi]))))

(defn- inline-ranges
  "Disjoint [lo hi] formula ranges, one per maximal growth around a seed."
  [tokens]
  (loop [i 0 acc []]
    (if (>= i (count tokens))
      acc
      (if (seed? tokens i)
        (let [lo (if (= :word (:type (get tokens (dec i)))) (dec i) i)
              [lo hi] (grow tokens lo i)
              lo (max lo (inc (or (second (peek acc)) -1)))
              r (balance tokens lo hi)]
          (recur (inc (max i hi)) (cond-> acc r (conj r))))
        (recur (inc i) acc)))))

;; ── Rebuild ─────────────────────────────────────────────────────────────────

(defn- formula [tokens lo hi]
  (math/wrap-tex (finish-tex (apply str (map token-tex (subvec tokens lo (inc hi)))))))

(defn- rebuild!
  "Replace `block`'s children: each range in `ranges` becomes one `\\(…\\)` text
   node, every other token goes back as it was."
  [^Element block tokens ranges]
  (let [starts (into {} (map (juxt first identity)) ranges)
        nodes (loop [i 0 out []]
                (cond
                  (>= i (count tokens)) out
                  (starts i) (let [[_ hi] (starts i)]
                               (recur (inc hi) (conj out (formula tokens i hi))))
                  :else (let [{:keys [s node]} (tokens i)]
                          (recur (inc i) (conj out (or node s))))))]
    (.empty block)
    (doseq [part (partition-by string? nodes)]
      (if (string? (first part))
        (.appendChild block (TextNode. (apply str part)))
        (doseq [n part] (.appendChild block ^Node n))))))

(defn- rewrite-block! [^Element block]
  (let [tokens (block-tokens block)
        ranges (if (equation-block? tokens)
                 (some-> (balance tokens (equation-number-end tokens) (dec (count tokens)))
                   vector)
                 (inline-ranges tokens))]
    (when (seq ranges)
      (rebuild! block tokens ranges))))

(defn- repair-entities! [^Element root]
  (doseq [^TextNode tn (.textNodes root)]
    (let [s (.getWholeText tn)
          fixed (reduce-kv str/replace s invented-entities)]
      (when (not= s fixed) (.text tn fixed))))
  (doseq [^Element child (.children root)]
    (repair-entities! child)))

(defn html-math->tex
  "Rewrite HTML-written math in OCR output as `\\(…\\)` text.

   pre : `html` is a string or nil.
   post: nil in → nil out. Otherwise HTML in which invented entities are
         repaired, each equation paragraph outside code is one formula, and each
         non-footnote script outside code sits inside a formula. Text inside an
         existing `\\(…\\)` region is unchanged.
   inv : idempotent — a second pass changes nothing."
  [html]
  (when html
    (let [doc (Jsoup/parseBodyFragment html)
          body (.body doc)]
      (.prettyPrint (.outputSettings doc) false)
      (repair-entities! body)
      (doseq [^Element block (.select body "p, li")]
        (when-not (.closest block code-selector)
          (rewrite-block! block)))
      (.html body))))
