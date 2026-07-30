(ns freememo.math
  "The canonical representation of math in stored content, plus the conversion at
   every boundary that touches it. See `plans/math-support.md`.

   Stored form: `\\(TeX\\)` inline in HTML **text**. That choice is what makes the
   rest of the pipeline free:

   - `html-cleaner/clean-html` needs no allow-list entry — text passes through, so
     none of its twelve consumers change. (A `span.ql-formula[data-value]` form
     would be erased twice over: `ql-formula` is not in `quill-class-allow-list`
     and `data-value` is on no safelist.)
   - `text/strip-html` carries the delimiters into `topics.content_text`, so math
     is searchable and appears in search snippets.
   - `render-math!` scans text, not attributes, so it renders stored content with
     no change.

   Four boundaries convert:

   | boundary   | fn                     |
   |------------|------------------------|
   | editor in  | `stored->editor-html`  |
   | editor out | `editor->stored-html`  |
   | Anki push  | `stored->anki-html`    |
   | Anki pull  | `anki->stored-html`    |
   | Anki diff  | `stored->compare-html` |

   The push/pull pair is not symmetric: push spaces TeX close braces for
   cloze-bearing kinds and pull does not undo it, so `stored->compare-html` is
   what makes a local field comparable to what Anki holds. All three read the kind
   through `cloze-bearing-kinds` — if they ever disagree, affected cards read as
   perpetually Anki-modified.

   Both Anki directions carry the TeX as HTML text on both sides, so entity
   escaping is unchanged and they are pure string swaps. Both editor directions go
   through the DOM instead: the browser owns text-vs-attribute escaping, which is
   what makes TeX containing `<`, `>`, `&` or `\\\"` survive a round-trip.

   Known limitation: the pure (Anki-direction) fns are regex-based and cannot see
   document structure, so a literal `\\(`…`\\)` pair typed inside a code block is
   rewritten as math on push. Display-only and reversed by the next pull — no data
   loss. The editor-direction fns DO exclude code (see `code-selector`)."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; The canonical form
;; ---------------------------------------------------------------------------
;; Every regex here is flag-free on purpose. ClojureScript regex literals carry
;; no flags, and JS rejects Clojure's inline `(?i)` / `(?s)` groups outright, so
;; `[\s\S]` stands in for dotAll and tag matching is lowercase-only (Anki and
;; Quill both emit lowercase tag names).

(def tex-region-re
  "One `\\(…\\)` math region in stored HTML. Group 1 is the TeX body."
  #"\\\(([\s\S]*?)\\\)")

(defn wrap-tex
  "The stored form of TeX body `tex`."
  [tex]
  (str "\\(" tex "\\)"))

;; ---------------------------------------------------------------------------
;; Pure conversions — cloze validation and the Anki boundary
;; ---------------------------------------------------------------------------

(defn strip-tex
  "Remove every math region from `html`, leaving surrounding markup untouched.

   Cloze validation needs this: `freememo.cloze/validate` balances `{{cN::` against
   `}}`, and TeX brace nesting (`x^{a^{b}}`) emits `}}` that would be counted as a
   cloze closer.

   pre : `html` is a string or nil.
   post: the result contains no `\\(…\\)` region."
  [html]
  (str/replace (or html "") tex-region-re ""))

(def ^:private nested-close-brace-re
  "A `}` immediately followed by another `}`."
  #"\}(?=\})")

(defn space-tex-close-braces
  "Separate consecutive closing braces inside each math region with a space, so
   TeX cannot close an Anki cloze deletion.

   TeX ignores whitespace between braces, so `x^{a^{b}}` and `x^{a^{b} }` render
   identically — while only the first can be misread as the `}}` that ends
   `{{c1::…}}`. Applied at push only; storage keeps the original TeX.

   pre : `html` is stored-form HTML, or nil.
   post: no math region in the result contains `}}`; bytes outside math regions
         are unchanged."
  [html]
  (str/replace (or html "") tex-region-re
    (fn [[_ tex]] (wrap-tex (str/replace tex nested-close-brace-re "} ")))))

(def cloze-bearing-kinds
  "Card kinds whose fields carry `{{cN::…}}` markers, and so need TeX close braces
   spaced apart before they reach Anki.

   THE single definition. Push, CSV export, and the Anki-modified diff must all
   agree on it: spacing on push but not when comparing (or the reverse) makes
   every affected card read as perpetually modified in the library overlay."
  #{"cloze" "overlapping"})

(defn cloze-bearing-kind?
  "True iff `kind`'s fields carry cloze markers. See `cloze-bearing-kinds`."
  [kind]
  (contains? cloze-bearing-kinds kind))

(defn- tex->anki-mathjax
  "Rewrite every math region as Anki's `<anki-mathjax>` element. Private — callers
   want `stored->anki-html`, which also handles the cloze brace spacing."
  [html]
  (str/replace (or html "") tex-region-re
    (fn [[_ tex]] (str "<anki-mathjax>" tex "</anki-mathjax>"))))

(defn stored->anki-html
  "Convert one field's stored HTML to the form Anki should hold, for a card of
   `kind`.

   Two steps, in this order: cloze-bearing kinds get their TeX close braces
   spaced (spacing operates on `\\(…\\)` regions, which no longer exist after the
   tag swap), then every region becomes an `<anki-mathjax>` element.

   pre : `html` is stored-form HTML or nil; `kind` is a card kind string.
   post: the result contains no `\\(…\\)` region, and no math region of a
         cloze-bearing kind contains `}}`.
   inv : the pairing with `stored->compare-html` — a field pushed through here and
         read back must compare equal to its local original, so the same kind
         predicate gates both."
  [html kind]
  (tex->anki-mathjax
    (cond-> (or html "")
      (cloze-bearing-kind? kind) space-tex-close-braces)))

(defn stored->compare-html
  "Normalise one field's stored HTML for comparison against what Anki holds.

   Applies exactly the brace spacing `stored->anki-html` applies, and nothing
   else: the caller has already converted Anki's side back to stored form and
   stripped both sides, so the spacing is the only remaining divergence. Spacing
   is idempotent, so this is safe on a field that never went to Anki.

   pre : `html` is stored-form HTML or nil; `kind` is a card kind string.
   post: comparable to `(-> anki-field anki->stored-html)` for the same card."
  [html kind]
  (cond-> (or html "")
    (cloze-bearing-kind? kind) space-tex-close-braces))

(def ^:private anki-mathjax-re
  "One `<anki-mathjax>` element. Group 1 is the TeX body. Attributes (e.g. a
   `block` flag written by Anki) are matched and dropped — this app stores inline
   math only."
  #"<anki-mathjax\b[^>]*>([\s\S]*?)</anki-mathjax>")

(defn anki->stored-html
  "Rewrite every `<anki-mathjax>` element back to stored form.

   Inverse of `stored->anki-html` except for the brace spacing, which is NOT
   reversed: the extra space is valid TeX and undoing it would have to guess which
   spaces the user typed. `stored->compare-html` is how the two sides are made
   comparable instead.

   pre : `html` is Anki field HTML, or nil.
   post: the result contains no `<anki-mathjax>` element; each became one math
         region."
  [html]
  (str/replace (or html "") anki-mathjax-re
    (fn [[_ tex]] (wrap-tex tex))))

;; ---------------------------------------------------------------------------
;; Editor boundary — DOM-based, so the browser owns escaping
;; ---------------------------------------------------------------------------

(def ^:private code-selector
  "Elements whose text is source, not prose. A `\\(` inside one of these is a
   character literal or an escape, not the start of a formula — Clojure's `\\(`
   being the motivating case. Mirrors `html-cleaner/inside-code-block-container?`."
  "pre, code, .ql-code-block-container")

(defn- in-code?
  "True iff `text-node` sits inside a `code-selector` element."
  [text-node]
  #?(:cljs (let [parent (.-parentElement text-node)]
             (and (some? parent) (some? (.closest parent code-selector))))
     :clj nil))

(defn- formula-span
  "An EMPTY `span.ql-formula` carrying `tex` in `data-value`.

   Empty is correct and sufficient: Quill's built-in `Formula` blot declares
   `className = 'ql-formula'` / `tagName = 'SPAN'` and its `static value` reads
   `data-value`, so Parchment resolves this span during `clipboard.convert` with
   no custom matcher and no `Quill.register` call. The blot renders KaTeX into the
   node itself."
  [tex]
  #?(:cljs (let [el (.createElement js/document "span")]
             (.setAttribute el "class" "ql-formula")
             (.setAttribute el "data-value" tex)
             el)
     :clj nil))

(defn- splice-formulas!
  "Replace `text-node` with alternating text nodes and formula spans, iff it
   contains at least one math region. No-op otherwise.

   `String.split` on a one-group regex interleaves the captures, so odd indices
   are TeX bodies and even indices are the surrounding text."
  [text-node]
  #?(:cljs
     (let [parts (.split (.-nodeValue text-node) tex-region-re)]
       (when (> (.-length parts) 1)
         (let [frag (.createDocumentFragment js/document)]
           (dotimes [i (.-length parts)]
             (let [part (aget parts i)]
               (if (odd? i)
                 (.appendChild frag (formula-span part))
                 (when (pos? (.-length part))
                   (.appendChild frag (.createTextNode js/document part))))))
           (.replaceWith text-node frag))))
     :clj nil))

(defn stored->editor-html
  "Convert stored-form math into the DOM shape Quill's `Formula` blot reads.

   Text inside `pre`, `code`, or a Quill code-block container is left alone.

   Returns `html` untouched when it contains no delimiter. That guard is
   load-bearing, not a micro-optimization: this runs on every editor load and on
   every reactive content update, over the whole document, and the body of it is
   a full HTML parse plus a walk of every text node. A document without math must
   cost one substring search.

   pre : `html` is stored-form HTML, or nil; a DOM is available (CLJS).
   post: every math region outside code is an empty `span.ql-formula[data-value]`,
         and all other markup and text is unchanged. CLJ: nil.
   inv : `(editor->stored-html (stored->editor-html h))` ≡ `h`, modulo the
         browser's own HTML normalization."
  [html]
  #?(:cljs
     (let [s (or html "")]
       (if-not (str/includes? s "\\(")
         s
         (let [holder (.createElement js/document "div")]
           (set! (.-innerHTML holder) s)
           ;; Collect before mutating — a TreeWalker over a DOM being restructured
           ;; underneath it is not safe to advance.
           (let [walker (.createTreeWalker js/document holder (.-SHOW_TEXT js/NodeFilter) nil)
                 nodes (loop [acc []]
                         (if-let [n (.nextNode walker)]
                           (recur (conj acc n))
                           acc))]
             (doseq [n nodes]
               (when-not (in-code? n)
                 (splice-formulas! n))))
           (.-innerHTML holder))))
     :clj nil))

(defn editor->stored-html
  "Convert a Quill root's HTML to stored form.

   Replaces each `span.ql-formula` — including the KaTeX subtree the blot rendered
   into it — with `\\(TeX\\)` text. Serializing through a text node is what makes
   TeX containing `<`, `>` or `&` survive: the browser escapes for text context,
   where the attribute it came from used different rules.

   Returns `html` untouched when it contains no formula span. **This guard is the
   difference between a working autosave path and a broken one**: `editor-html`
   calls this from `text-change`, i.e. once per keystroke, and the body is a full
   HTML parse plus a re-serialize of the entire document. A document without math
   must cost one substring search.

   pre : `html` is a Quill root's innerHTML, or nil; a DOM is available (CLJS).
   post: the result contains no `span.ql-formula`; each became one math region.
         Math already present as literal text — a formula revealed in source form
         for editing — passes through unchanged. CLJ: nil."
  [html]
  #?(:cljs
     (let [s (or html "")]
       (if-not (str/includes? s "ql-formula")
         s
         (let [holder (.createElement js/document "div")]
           (set! (.-innerHTML holder) s)
           (doseq [el (js/Array.from (.querySelectorAll holder "span.ql-formula"))]
             (.replaceWith el (.createTextNode js/document
                                (wrap-tex (or (.getAttribute el "data-value") "")))))
           (.-innerHTML holder))))
     :clj nil))

;; ---------------------------------------------------------------------------
;; KaTeX readiness — the gate every Quill mount waits on
;; ---------------------------------------------------------------------------

(def katex-wait-ms
  "How long an editor mount waits for KaTeX before giving up on math.

   `window.__katexReady` never rejects and never times out — a blocked CDN leaves
   it pending forever (see `render-math!`). Awaiting it unguarded would mean no
   editor at all, silently and permanently, so the wait is raced against this."
  3000)

#?(:cljs
   (defonce ^:private katex-ready
     ;; Promise<boolean>. Resolved ONCE per page load, not once per editor mount:
     ;; a page can mount the document editor and several card fields, and a
     ;; per-mount timer would race N ways for no benefit.
     (if-let [ready (.-__katexReady js/window)]
       (js/Promise.race
         #js [(.then ready (fn [_] true))
              (js/Promise. (fn [resolve] (js/setTimeout #(resolve false) katex-wait-ms)))])
       (js/Promise.resolve false))))

(defn on-katex-ready!
  "Call `k` with true once KaTeX is usable, or false if it did not arrive within
   `katex-wait-ms`.

   Callers use the boolean to decide whether to enable Quill's formula module. On
   false they MUST also skip `stored->editor-html`: converting math to spans with
   no blot registered would drop `data-value` and destroy the formula on the next
   save. Leaving the delimiters as text costs rendering and keeps the bytes.

   pre : `k` is a 1-arg fn.
   post: `k` is called exactly once, asynchronously, with a boolean. CLJ: no-op."
  [k]
  #?(:cljs (.then katex-ready k)
     :clj nil))

;; ---------------------------------------------------------------------------
;; Display
;; ---------------------------------------------------------------------------

(defn render-math!
  "CLJS-only: render KaTeX math (`\\(…\\)` inline, `\\[…\\]` display) in `node`.
   CLJ no-op. Call AFTER node's innerHTML is set.

   KaTeX's auto-render script loads async from a CDN; `window.__katexReady`
   (defined in index.html) resolves with `renderMathInElement` once it is
   available. Chaining each render on that promise means a node mounted before
   KaTeX loads still renders the instant it arrives — no polling, and no bounded
   timer that could lose the race or leak. A blocked CDN simply leaves the promise
   pending, so math stays literal (no crash, no hang).

   No `$` delimiter is configured: stored content carries `\\(…\\)` (this ns) and
   assistant replies are rewritten to the same form server-side by
   `freememo.markdown/dollar-math->tex`, so a currency `$` can never open math.
   `throwOnError:false` shows a bad expression as source instead of throwing.
   Code/`pre` are skipped (KaTeX default ignoredTags).

   `after-render` (optional) runs once typesetting has completed. It exists for
   callers whose own work depends on the post-typeset layout — scrolling a
   snippet to centre its match, for example — since rendering is async and would
   otherwise land after them.

   Plain defn so the reader conditional stays invisible to Electric's reactive
   compiler (CLJ/CLJS signal parity)."
  ([node] (render-math! node nil))
  ([node after-render]
   #?(:cljs
      (when-let [ready (.-__katexReady js/window)]
        (.then ready
          (fn [render]
            (render node
              #js {:delimiters #js [#js {:left "\\[" :right "\\]" :display true}
                                    #js {:left "\\(" :right "\\)" :display false}]
                   :throwOnError false})
            (when after-render (after-render)))))
      :clj nil)))

(defn set-html!
  "Write `html` into `node`, then render any math in it. CLJ no-op.

   The one call every non-Quill display site uses, so a new site cannot forget the
   render half. Ordering matters and is why this is one fn rather than two calls:
   `render-math!` walks the text that `innerHTML` has to have written first.

   The delimiter check is load-bearing, not a micro-optimization: card rows call
   this inside virtual-scroll tables where per-row mount cost dominates. Content
   without math pays one substring search instead of a promise callback and a full
   DOM text walk.

   `after-render` (optional) runs after typesetting completes, or immediately when
   there is no math to typeset. Use it for anything that reads the resulting
   layout: it fires exactly once either way, so the caller does not branch on
   whether the content happened to contain a formula.

   pre : `node` is a DOM element (CLJS); `html` is a string or nil.
   post: `node.innerHTML` is `html`, every math region in it is typeset once KaTeX
         is available, and `after-render` has been called exactly once. Returns
         nil — callers use this as a statement, and some sit in an Electric
         component's tail position where leaking a promise would be noise."
  ([node html] (set-html! node html nil))
  ([node html after-render]
   #?(:cljs (let [s (str (or html ""))]
              (set! (.-innerHTML node) s)
              (if (str/includes? s "\\(")
                (render-math! node after-render)
                (when after-render (after-render)))
              nil)
      :clj nil)))
