(ns freememo.format-menu
  "Custom floating format menu for the bubble doc editor (Notion-inspired).

   Replaces Quill's native `.ql-tooltip`: we own the card, its placement, and
   its dismissal; Quill still owns the text model, so every button drives the
   Quill format API (`format`/`removeFormat`) against the current selection.

   Placement contract (the reason this exists — the native tooltip collided
   with the app command bar and covered content):
     - position:fixed, so no editor-pane overflow ancestor clips it.
     - NEVER covers the visible part of the selection. The vertical side comes
       from the selection's whole bounding box, so a multi-line selection is
       cleared rather than straddled.
     - four tiers, first fit wins: below the box inside the Quill container,
       above the box inside the container, below the VISIBLE box inside the
       viewport (over whatever sits under the editor), else hidden. No tier
       above the container top — that is the app command bar's floor. The
       container is clipped to the viewport first, so a container an ancestor
       scrolled off screen still cannot place the card off screen.
     - on a touch device, also clear of the browser's own Cut/Copy/Paste bar,
       which sits just above or below the selection. The page cannot read where
       that bar is, so a band of `sel-place/touch-reserve` px is kept clear on
       both sides of the selection.
     - horizontal: the card's near edge sits at the selection's focus point,
       mirrored for RTL, clamped to the viewport edges.
     - hidden while the selection is scrolled out of the container, and shown
       again on the way back. `!open?` (not the card's display) is what says a
       selection still wants the card.
     - one size. No height cap and no scrolling card: a tier that does not fit
       is not taken.
   freememo.selection-placement holds the arithmetic and its tests; this ns
   measures the rects and applies the result.

   `install!` wires the selection / scroll / resize / dismiss listeners, builds
   the card once, and returns a teardown fn that removes them and the card.

   `opts` (2-arity install!) turns on optional controls: `:image?` adds an
   image-insert button, `:audio?` an audio-insert button, `:cloze?` adds a
   cloze-deletion row (all three drive freememo.editor-actions), `:card-gen?`
   adds the document editor's card-count, Generate-here, and Parse-questions
   actions.
   The document editor passes `:card-gen?/:audio?`; card fields pass
   `:image?/:audio?/:cloze?`. The document editor takes images by paste and drop
   only, which is why it passes no `:image?` — audio has no such history, so it
   gets its button on both surfaces.

   Math has no control here: formulas are created by typing `\\(…\\)` and edited
   through the anchored popover in freememo.formula-ui."
  (:require [clojure.string :as str]
            [freememo.editor-actions :as editor-actions]
            [freememo.card-count :as cc]
            [freememo.command-bus :as bus]
            [freememo.commands :as commands]
            [freememo.editor-popovers :as editor-popovers]
            [freememo.selection-placement :as sel-place]))

;; ---------------------------------------------------------------------------
;; Glyphs for the opt-in controls. Inline SVG with stroke="currentColor" (NOT
;; the toolbar's `.ql-stroke` class — that resolves only inside `.ql-toolbar`,
;; and the format-menu is not one) so they inherit the button's text colour
;; like the HTML glyphs on the other buttons. The cloze braces reproduce the
;; { } design formerly registered as Quill toolbar icons in quill_field.
;; ---------------------------------------------------------------------------

(def ^:private cloze-braces
  (str "<path d=\"M6.5 3.5 Q4.5 3.5 4.5 6.5 Q4.5 8 3 9 Q4.5 10 4.5 11.5 Q4.5 14.5 6.5 14.5\"/>"
    "<path d=\"M11.5 3.5 Q13.5 3.5 13.5 6.5 Q13.5 8 15 9 Q13.5 10 13.5 11.5 Q13.5 14.5 11.5 14.5\"/>"))

(def ^:private cloze-inc-glyph
  (str "<svg viewBox=\"0 0 18 18\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\">"
    cloze-braces
    "<line x1=\"9\" y1=\"6.5\" x2=\"9\" y2=\"11.5\"/><line x1=\"6.8\" y1=\"9\" x2=\"11.2\" y2=\"9\"/></svg>"))

(def ^:private cloze-eq-glyph
  (str "<svg viewBox=\"0 0 18 18\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\">"
    cloze-braces
    "<line x1=\"6.8\" y1=\"7.5\" x2=\"11.2\" y2=\"7.5\"/><line x1=\"6.8\" y1=\"10.5\" x2=\"11.2\" y2=\"10.5\"/></svg>"))

(def ^:private image-glyph
  (str "<svg viewBox=\"0 0 18 18\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\">"
    "<rect x=\"2.5\" y=\"3.5\" width=\"13\" height=\"11\" rx=\"1.5\"/>"
    "<circle cx=\"6.3\" cy=\"7.3\" r=\"1.3\" fill=\"currentColor\" stroke=\"none\"/>"
    "<path d=\"M3 13 L7 9 L9.5 11.5 L12 9 L15 12.5\"/></svg>"))


;; Speaker with two waves — the Insert audio button. Same 18-viewBox stroke
;; construction as image-glyph above, so the two sit at one weight in the row.
(def ^:private audio-glyph
  (str "<svg viewBox=\"0 0 18 18\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\" stroke-linejoin=\"round\">"
    "<path d=\"M3.5 6.8 H6.2 L9.6 3.8 V14.2 L6.2 11.2 H3.5 Z\"/>"
    "<path d=\"M12.1 6.9 A3.1 3.1 0 0 1 12.1 11.1\"/>"
    "<path d=\"M13.9 5.1 A5.5 5.5 0 0 1 13.9 12.9\"/></svg>"))

;; Indent arrows — three text lines with an arrow beside them, pointing out
;; (increase) or back (decrease). Same 18-viewBox stroke construction as
;; image-glyph. The arrows are drawn for LTR text. In an RTL line the indent
;; still moves the line away from its start edge, so the arrow points the
;; other way from the move.
(defn- indent-glyph [arrow-points]
  (str "<svg viewBox=\"0 0 18 18\" width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"1.4\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
    "<line x1=\"8.5\" y1=\"4.5\" x2=\"15.5\" y2=\"4.5\"/>"
    "<line x1=\"8.5\" y1=\"9\" x2=\"15.5\" y2=\"9\"/>"
    "<line x1=\"8.5\" y1=\"13.5\" x2=\"15.5\" y2=\"13.5\"/>"
    "<line x1=\"2.5\" y1=\"9\" x2=\"6\" y2=\"9\"/>"
    "<polyline points=\"" arrow-points "\"/></svg>"))

(def ^:private increase-indent-glyph (indent-glyph "4.3 7 6.3 9 4.3 11"))
(def ^:private decrease-indent-glyph (indent-glyph "4.2 7 2.2 9 4.2 11"))

;; Sparkles — matches freememo.icons :sparkles (24-viewBox Lucide path); marks
;; the Generate-here action on the card-gen row.
(def ^:private sparkles-glyph
  (str "<svg viewBox=\"0 0 24 24\" width=\"15\" height=\"15\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\">"
    "<path d=\"M9.937 15.5A2 2 0 0 0 8.5 14.063l-6.135-1.582a.5.5 0 0 1 0-.962L8.5 9.936A2 2 0 0 0 9.937 8.5l1.582-6.135a.5.5 0 0 1 .963 0L14.063 8.5A2 2 0 0 0 15.5 9.937l6.135 1.581a.5.5 0 0 1 0 .964L15.5 14.063a2 2 0 0 0-1.437 1.437l-1.582 6.135a.5.5 0 0 1-.963 0z\"/>"
    "<path d=\"M20 3v4\"/><path d=\"M22 5h-4\"/><path d=\"M4 17v2\"/><path d=\"M5 18H3\"/></svg>"))

;; ---------------------------------------------------------------------------
;; Quill format actions — all operate on a cached range so focus/selection
;; loss (e.g. opening a native colour picker) can't drop the target text.
;; ---------------------------------------------------------------------------

(defn- reselect! [^js q rng]
  (when rng (.setSelection q (:index rng) (:length rng) "silent")))

(defn- active-format
  "Quill's active-format map over the cached range, without moving selection."
  [^js q rng]
  (if rng (.getFormat q (:index rng) (:length rng)) #js {}))

(defn- toggle-inline! [^js q rng fmt]
  (let [on? (boolean (aget (active-format q rng) fmt))]
    (reselect! q rng)
    (.format q fmt (not on?) "user")))

(defn- set-format! [^js q rng fmt value]
  (reselect! q rng)
  (.format q fmt value "user"))

(defn- toggle-value! [^js q rng fmt value]
  ;; List/direction-style formats: same value again clears it.
  (let [cur (aget (active-format q rng) fmt)]
    (reselect! q rng)
    (.format q fmt (if (= cur value) false value) "user")))

(defn- clear-format! [^js q rng]
  (when rng (.removeFormat q (:index rng) (:length rng) "user")))

(defn- insert-table! [^js q rng]
  (reselect! q rng)
  (when-let [tbl (.getModule q "table")]
    (.insertTable tbl 3 3)))

;; ---------------------------------------------------------------------------
;; DOM builders
;; ---------------------------------------------------------------------------

(defn- el [tag class]
  (let [n (js/document.createElement tag)]
    (set! (.-className n) class)
    n))

(defn- keep-selection!
  "Buttons must not steal the editor selection: block the mousedown that would
   blur Quill before the click handler reads the cached range."
  [node]
  (.addEventListener node "mousedown" (fn [^js e] (.preventDefault e))))

(def ^:private page-break-glyph
  "A rule with a downward arrow through it — a boundary being put back.

   Inline SVG whose stroke is currentColor, NOT the toolbar's `.ql-stroke`
   class: that class resolves only inside `.ql-toolbar`, and the format menu is
   not one."
  (str "<svg viewBox=\"0 0 18 18\" width=\"14\" height=\"14\" fill=\"none\" "
       "stroke=\"currentColor\" stroke-width=\"1.6\" stroke-linecap=\"round\" "
       "stroke-linejoin=\"round\" aria-hidden=\"true\">"
       "<line x1=\"2\" y1=\"9\" x2=\"6\" y2=\"9\"/>"
       "<line x1=\"12\" y1=\"9\" x2=\"16\" y2=\"9\"/>"
       "<polyline points=\"9 3 9 12\"/>"
       "<polyline points=\"6.5 9.5 9 12 11.5 9.5\"/>"
       "</svg>"))

(defn- glyph-button
  "A single icon/glyph toolbar button. `on-click` fires with no args.

   `label` is the accessible name; `tooltip` (default `label`) is the hover text.
   The two diverge exactly where a chord belongs in the tooltip but not in the
   accessible name — `ExtractTopicButton`/`AddCardButton`'s established shape."
  ([glyph label on-click] (glyph-button glyph label on-click label))
  ([glyph label on-click tooltip]
   (let [b (el "button" "fmt-btn")]
     (set! (.-type b) "button")
     (set! (.-innerHTML b) glyph)
     (.setAttribute b "title" tooltip)
     (.setAttribute b "aria-label" label)
     (keep-selection! b)
     (.addEventListener b "click" (fn [^js e] (.stopPropagation e) (on-click)))
     b)))

(defn- dropdown
  "A labelled dropdown: a trigger button that toggles a popup list of
   {:label :on-select} items. Returns {:wrap :label :popup}; callers may reset
   the `:label` text on refresh (block type does; size/align stay static)."
  [initial-label items]
  (let [wrap    (el "div" "fmt-dd")
        trigger (el "button" "fmt-dd-trigger")
        caret   (el "span" "fmt-dd-caret")
        label   (el "span" "fmt-dd-label")
        popup   (el "div" "fmt-dd-popup")]
    (set! (.-type trigger) "button")
    (set! (.-textContent label) initial-label)
    (set! (.-innerHTML caret) "⌄")
    (.appendChild trigger label)
    (.appendChild trigger caret)
    (keep-selection! trigger)
    (keep-selection! popup)
    (doseq [{:keys [label on-select]} items]
      (let [item (el "button" "fmt-dd-item")]
        (set! (.-type item) "button")
        (set! (.-textContent item) label)
        (keep-selection! item)
        (.addEventListener item "click"
          (fn [^js e]
            (.stopPropagation e)
            (set! (.-className popup) "fmt-dd-popup")   ; close
            (on-select)))
        (.appendChild popup item)))
    (.addEventListener trigger "click"
      (fn [^js e]
        (.stopPropagation e)
        (let [open? (.contains (.-classList popup) "open")]
          (set! (.-className popup) (if open? "fmt-dd-popup" "fmt-dd-popup open")))))
    (.appendChild wrap trigger)
    (.appendChild wrap popup)
    {:wrap wrap :label label :popup popup}))

;; Notion-style colour presets. `false` = Default (removes the format).
(def ^:private text-colours
  [["Default" false] ["Gray" "#9b9a97"] ["Brown" "#64473a"] ["Orange" "#d9730d"]
   ["Yellow" "#dfab01"] ["Green" "#0f7b6c"] ["Blue" "#0b6e99"] ["Purple" "#6940a5"]
   ["Pink" "#ad1a72"] ["Red" "#e03e3e"]])

(def ^:private background-colours
  [["Default" false] ["Gray" "#ebeced"] ["Brown" "#e9e5e3"] ["Orange" "#faebdd"]
   ["Yellow" "#fbf3db"] ["Green" "#ddedea"] ["Blue" "#ddebf1"] ["Purple" "#eae4f2"]
   ["Pink" "#f4dfeb"] ["Red" "#fbe4e4"]])

(defn- colour-swatch
  "One preset swatch. `kind` is :text (shows a coloured A) or :bg (filled box).
   `value` false ⇒ the Default swatch (clears the format)."
  [kind label value pick!]
  (let [b (el "button" (str "fmt-swatch fmt-swatch-" (name kind) (when-not value " fmt-swatch-default")))]
    (set! (.-type b) "button")
    (.setAttribute b "title" label)
    (.setAttribute b "aria-label" (str label " " (name kind) " colour"))
    (when (= kind :text)
      (set! (.-textContent b) "A")
      (when value (set! (.. b -style -color) value)))
    (when (and (= kind :bg) value)
      (set! (.. b -style -backgroundColor) value))
    (keep-selection! b)
    (.addEventListener b "click" (fn [^js e] (.stopPropagation e) (pick!)))
    b))

(defn- colour-section
  "A labelled swatch grid for one format (text `color` or `background`)."
  [^js q get-rng after fmt kind label presets close!]
  (let [sec  (el "div" "fmt-colour-section")
        head (el "div" "fmt-section-label")
        grid (el "div" "fmt-swatch-grid")]
    (set! (.-textContent head) label)
    (doseq [[nm value] presets]
      (.appendChild grid
        (colour-swatch kind nm value
          (after (fn [] (set-format! q (get-rng) fmt value) (close!))))))
    (.appendChild sec head)
    (.appendChild sec grid)
    sec))

(defn- colour-popover
  "The `A` trigger opening a popup with Text-colour and Background-colour preset
   grids (Notion merges both under one control). Returns {:wrap}."
  [^js q get-rng after]
  (let [wrap    (el "div" "fmt-dd")
        trigger (el "button" "fmt-btn fmt-colour-trigger")
        popup   (el "div" "fmt-dd-popup fmt-colour-popup")
        close!  (fn [] (set! (.-className popup) "fmt-dd-popup fmt-colour-popup"))]
    (set! (.-type trigger) "button")
    (set! (.-textContent trigger) "A")
    (.setAttribute trigger "title" "Text & background colour")
    (.setAttribute trigger "aria-label" "Text and background colour")
    (keep-selection! trigger)
    (keep-selection! popup)
    (.appendChild popup (colour-section q get-rng after "color" :text "Text color" text-colours close!))
    (.appendChild popup (colour-section q get-rng after "background" :bg "Background color" background-colours close!))
    (.addEventListener trigger "click"
      (fn [^js e]
        (.stopPropagation e)
        (let [open? (.contains (.-classList popup) "open")]
          (set! (.-className popup) (if open? "fmt-dd-popup fmt-colour-popup" "fmt-dd-popup fmt-colour-popup open")))))
    (.appendChild wrap trigger)
    (.appendChild wrap popup)
    {:wrap wrap}))

;; ---------------------------------------------------------------------------
;; Active-state
;; ---------------------------------------------------------------------------

(defn- block-label [^js fmt]
  (cond
    (aget fmt "code-block")          "Code block"
    (= 1 (aget fmt "header"))        "Heading 1"
    (= 2 (aget fmt "header"))        "Heading 2"
    (= 3 (aget fmt "header"))        "Heading 3"
    :else                            "Normal text"))

;; ---------------------------------------------------------------------------
;; Card assembly
;; ---------------------------------------------------------------------------

(defn- card-gen-row
  "Bottom card-gen row: a `Cards −/+` stepper over the shared cc/!card-count
   global + a `Generate here` action. Returns {:row :sync}, where `sync`
   repaints the number and −/+ disabled state from the global (called on build
   and on every external change via install!'s add-watch).

   Pre:  `card` is the menu card element (hidden to dismiss); `get-rng` returns
         the cached selection {:index :length}.
   Post: −/+ clamp to [card-count-min, card-count-max], mutate the global, and
         persist via the :persist-card-count invoker (no-op if unpublished, e.g.
         reading-mode). `Generate here` restores the selection, fires :generate,
         and hides the card. `keep-selection!` blocks the mousedown blur so the
         cached range survives the click."
  [^js q get-rng ^js card]
  (let [row      (el "div" "fmt-row fmt-cardgen-row")
        stepper  (el "div" "number-stepper")
        lbl      (el "span" "number-stepper-label")
        dec-btn  (el "button" "number-stepper-btn")
        numspan  (el "span" "number-stepper-value")
        inc-btn  (el "button" "number-stepper-btn")
        gen-btn  (el "button" "fmt-btn fmt-cardgen-btn")
        current  (fn [] (or @cc/!card-count cc/card-count-min))
        persist! (fn [] (when-let [f (get @bus/!invokers :persist-card-count)] (f)))
        sync!    (fn []
                   (let [v (current)]
                     (set! (.-textContent numspan) (str v))
                     (set! (.-disabled dec-btn) (<= v cc/card-count-min))
                     (set! (.-disabled inc-btn) (>= v cc/card-count-max))))
        step!    (fn [delta]
                   (reset! cc/!card-count (cc/clamp (+ (current) delta)))
                   (persist!)
                   (sync!))]
    (set! (.-textContent lbl) "Cards")
    (set! (.-type dec-btn) "button")
    (set! (.-textContent dec-btn) "−")
    (.setAttribute dec-btn "aria-label" "Fewer cards to generate")
    (set! (.-type inc-btn) "button")
    (set! (.-textContent inc-btn) "+")
    (.setAttribute inc-btn "aria-label" "More cards to generate")
    (set! (.-type gen-btn) "button")
    (set! (.-innerHTML gen-btn) (str sparkles-glyph "<span>Generate here</span>"))
    (.setAttribute gen-btn "title" "Generate cards from the selection")
    (.setAttribute gen-btn "aria-label" "Generate cards from the selection")
    (keep-selection! dec-btn)
    (keep-selection! inc-btn)
    (keep-selection! gen-btn)
    (.addEventListener dec-btn "click" (fn [^js e] (.stopPropagation e) (step! -1)))
    (.addEventListener inc-btn "click" (fn [^js e] (.stopPropagation e) (step! 1)))
    (.addEventListener gen-btn "click"
      (fn [^js e]
        (.stopPropagation e)
        (reselect! q (get-rng))            ; restore the cached range for :generate
        (bus/dispatch! :generate)
        (set! (.. card -style -display) "none")))
    (.appendChild stepper lbl)
    (.appendChild stepper dec-btn)
    (.appendChild stepper numspan)
    (.appendChild stepper inc-btn)
    (.appendChild row stepper)
    (.appendChild row gen-btn)
    {:row row :sync sync!}))

(defn- parse-questions-row
  "Build the document editor's selected-line card action.

   Pre : `get-rng` returns the cached non-empty selection.
   Post: a click restores that selection, dispatches :parse-questions once,
         and hides the format menu.
   Inv : parsing and persistence stay in the mounted Electric invoker."
  [^js q get-rng ^js card]
  (let [row (el "div" "fmt-row fmt-parse-questions-row")
        parse-btn (el "button" "fmt-btn fmt-parse-questions-btn")]
    (set! (.-type parse-btn) "button")
    (set! (.-innerHTML parse-btn) "<span aria-hidden=\"true\">Q?</span><span>Parse questions</span>")
    (.setAttribute parse-btn "title" "Create basic cards from selected question-and-answer lines")
    (.setAttribute parse-btn "aria-label" "Parse selected questions into basic cards")
    (keep-selection! parse-btn)
    (.addEventListener parse-btn "click"
      (fn [^js e]
        (.stopPropagation e)
        (reselect! q (get-rng))
        (bus/dispatch! :parse-questions)
        (set! (.. card -style -display) "none")))
    (.appendChild row parse-btn)
    row))

(defn- build-card
  "Build the menu card and its controls. Returns
   {:card <el> :refresh (fn []) :!sync <atom>}.
   `refresh` syncs active-state from the cached range. Every control, after
   mutating the document, calls `(@!sync)` — install! sets that to refresh
   ALONE, deliberately not re-placing: a format that changes the text metrics
   would otherwise jitter the card under the cursor. `get-rng` returns the cached
   {:index :length}. `opts` = {:image? :cloze? :card-gen?}: :image?/:cloze?
   toggle card-editor controls; :card-gen? adds the document-editor card actions
   (§ ns docstring). Returns :card-count-sync (nil unless :card-gen?)."
  [^js q get-rng opts]
  (let [card    (el "div" "format-menu")
        section (fn [] (el "div" "fmt-row"))
        toggles (atom [])            ; [{:fmt :el}] inline toggle buttons
        !sync   (atom (fn []))       ; set by install! to (refresh + re-place)
        sync!   (fn [] (@!sync))
        after   (fn [thunk] (fn [] (thunk) (sync!)))    ; apply, then resync
        add-toggle! (fn [row glyph title fmt]
                      (let [b (glyph-button glyph title (after #(toggle-inline! q (get-rng) fmt)))]
                        (swap! toggles conj {:fmt fmt :el b})
                        (.appendChild row b)
                        b))
        block-dd (dropdown "Normal text"
                   [{:label "Normal text" :on-select (after #(do (set-format! q (get-rng) "header" false)
                                                                 (set-format! q (get-rng) "code-block" false)))}
                    {:label "Heading 1"   :on-select (after #(set-format! q (get-rng) "header" 1))}
                    {:label "Heading 2"   :on-select (after #(set-format! q (get-rng) "header" 2))}
                    {:label "Heading 3"   :on-select (after #(set-format! q (get-rng) "header" 3))}
                    {:label "Code block"  :on-select (after #(set-format! q (get-rng) "code-block" true))}])
        size-dd  (dropdown "Size"
                   [{:label "Small"  :on-select (after #(set-format! q (get-rng) "size" "small"))}
                    {:label "Normal" :on-select (after #(set-format! q (get-rng) "size" false))}
                    {:label "Large"  :on-select (after #(set-format! q (get-rng) "size" "large"))}
                    {:label "Huge"   :on-select (after #(set-format! q (get-rng) "size" "huge"))}])
        align-dd (dropdown "Align"
                   [{:label "Left"    :on-select (after #(set-format! q (get-rng) "align" false))}
                    {:label "Center"  :on-select (after #(set-format! q (get-rng) "align" "center"))}
                    {:label "Right"   :on-select (after #(set-format! q (get-rng) "align" "right"))}
                    {:label "Justify" :on-select (after #(set-format! q (get-rng) "align" "justify"))}])
        row-block (section)
        row-inline (section)
        row-blockfmt (section)
        cardgen (when (:card-gen? opts) (card-gen-row q get-rng card))
        parse-questions (when (:card-gen? opts)
                          (parse-questions-row q get-rng card))
        page-break-btn (glyph-button page-break-glyph "Restore page boundary"
                         (after #(editor-actions/insert-page-break! q (:index (get-rng)))))]
    ;; Row 1 — block type + size dropdowns
    (.appendChild row-block (:wrap block-dd))
    (.appendChild row-block (:wrap size-dd))
    ;; Row 2 — inline formatting (A = merged text + background colour presets)
    (.appendChild row-inline (:wrap (colour-popover q get-rng after)))
    (add-toggle! row-inline "<b>B</b>" "Bold" "bold")
    (add-toggle! row-inline "<i>I</i>" "Italic" "italic")
    (add-toggle! row-inline "<u>U</u>" "Underline" "underline")
    (add-toggle! row-inline "<s>S</s>" "Strikethrough" "strike")
    (add-toggle! row-inline "&lt;/&gt;" "Inline code" "code")
    (.appendChild row-inline (glyph-button "T✕" "Clear formatting" (after #(clear-format! q (get-rng)))))
    ;; Row 3 — block-level: lists, align, direction, highlight, table
    (.appendChild row-blockfmt (glyph-button "•—" "Bullet list" (after #(toggle-value! q (get-rng) "list" "bullet"))))
    (.appendChild row-blockfmt (glyph-button "1." "Numbered list" (after #(toggle-value! q (get-rng) "list" "ordered"))))
    ;; Every selected line moves one level. Card fields have no other way to
    ;; indent: Tab moves between fields there (a11y/free-quill-tab!).
    (.appendChild row-blockfmt (glyph-button decrease-indent-glyph "Decrease indent" (after #(set-format! q (get-rng) "indent" "-1"))))
    (.appendChild row-blockfmt (glyph-button increase-indent-glyph "Increase indent" (after #(set-format! q (get-rng) "indent" "+1"))))
    (.appendChild row-blockfmt (:wrap align-dd))
    (.appendChild row-blockfmt (glyph-button "⇄" "Right-to-left" (after #(toggle-value! q (get-rng) "direction" "rtl"))))
    (.appendChild row-blockfmt (glyph-button "⊞" "Insert table" (after #(insert-table! q (get-rng)))))
    ;; Image insert (card editors only). Uploads via the picker → /api/media,
    ;; inserting at the cached range index; never embeds a data-URI.
    (when (:image? opts)
      (.appendChild row-blockfmt
        (glyph-button image-glyph "Insert image"
          (after #(editor-actions/insert-image! q (:index (get-rng)))))))
    ;; Audio insert. MP3 only, because `api/upload-media-handler` accepts
    ;; exactly audio/mpeg beside image/*; the picker's accept filter says so
    ;; before the server has to. Drag and paste reach the same upload through
    ;; `editor-actions/install-attachment-upload!`, so this button is the discoverable
    ;; half of one path, not a second one.
    (when (:audio? opts)
      (.appendChild row-blockfmt
        (glyph-button audio-glyph "Insert audio"
          (after #(editor-actions/insert-audio! q (:index (get-rng)))))))
    ;; Restore a page boundary (plans/merged-page-reading.md §4.3-4.7).
    ;;
    ;; Built ALWAYS and hidden by `refresh` below, NOT gated by `opts`:
    ;; install! takes its opts once per editor construction and the document
    ;; editor is persistent, so opts cannot carry a flag the reader toggles.
    ;;
    ;; It restores a lost boundary and never creates a page. `restorable-page`
    ;; answers nil when there is nothing to restore, and insert-page-break!
    ;; refuses on nil, so the button cannot mint a page number.
    (.appendChild row-blockfmt page-break-btn)
    (.appendChild card row-block)
    (.appendChild card (el "div" "fmt-divider"))
    (.appendChild card row-inline)
    (.appendChild card (el "div" "fmt-divider"))
    (.appendChild card row-blockfmt)
    ;; Cloze deletion row (cloze card only). reselect! restores the cached
    ;; range before insert-cloze! reads the live selection — focus moved to the
    ;; button, so the editor's own getSelection would otherwise be stale. The
    ;; chords in each tooltip need no reselect!: they fire with focus still in
    ;; the editor, so getSelection is already live.
    (when (:cloze? opts)
      (let [cloze-row (section)
            inc-label "New cloze deletion"
            eq-label  "Same cloze deletion"
            ;; One formatter, so a tooltip cannot state a chord the binding does
            ;; not fire. The ids name the action rather than a registry command —
            ;; these chords are deliberately absent from freememo.commands, and
            ;; display-chord reads `id` only for the palette's Firefox carve-out.
            chord (fn [id mode]
                    (str " (" (commands/display-chord
                                id (get editor-actions/cloze-chords mode)) ")"))]
        (.appendChild cloze-row
          (glyph-button cloze-inc-glyph inc-label
            (after #(do (reselect! q (get-rng)) (editor-actions/insert-cloze! q :inc)))
            (str inc-label (chord :cloze-inc :inc))))
        (.appendChild cloze-row
          (glyph-button cloze-eq-glyph eq-label
            (after #(do (reselect! q (get-rng)) (editor-actions/insert-cloze! q :eq)))
            (str eq-label (chord :cloze-eq :eq))))
        (.appendChild card (el "div" "fmt-divider"))
        (.appendChild card cloze-row)))
    ;; Document card actions, via :card-gen?: count, Generate-here, then the
    ;; selected-line parser. They share one bottom section and one divider.
    (when cardgen
      (.appendChild card (el "div" "fmt-divider"))
      (.appendChild card (:row cardgen))
      (.appendChild card parse-questions))
    {:card card
     :!sync !sync
     :card-count-sync (:sync cardgen)      ; nil when no card-gen row
     :refresh
     (fn []
       (let [af (active-format q (get-rng))]
         (doseq [{:keys [fmt el]} @toggles]
           (let [on? (boolean (aget af fmt))]
             (if on? (.add (.-classList el) "active") (.remove (.-classList el) "active"))
             (.setAttribute el "aria-pressed" (str on?))))
         (set! (.-textContent (:label block-dd)) (block-label af))
         ;; The page-break control's visibility, per selection. It is present
         ;; only in focus reader's merged mode, and disabled there when every
         ;; page between the neighbours already has a marker — which is the
         ;; refusal item 4.7 asks for, stated in the control rather than in a
         ;; toast the reader would have to trigger to read.
         (let [ctx @editor-actions/!page-break-context
               page (when ctx (editor-actions/restorable-page q (:index (get-rng))))]
           (set! (.. page-break-btn -style -display) (if ctx "" "none"))
           (set! (.-disabled page-break-btn) (not page))
           (.setAttribute page-break-btn "title"
             (cond (nil? ctx) "Restore page boundary"
                   page (str "Restore the boundary for page " page)
                   :else "No page is missing a boundary here")))))}))

;; ---------------------------------------------------------------------------
;; Placement
;; ---------------------------------------------------------------------------

(defn- focus-line-rtl?
  "True when the line holding the selection's focus point resolves right-to-left.
   Reads the COMPUTED direction, not Quill's `direction` format: a document that
   inherits RTL from an ancestor carries no format on the range.

   Read ONCE when the selection changes, never per placement. A scroll must not
   change which edge the card anchors by, and applying the ⇄ control mid-selection
   deliberately does not re-place the card (see build-card), so re-reading here
   would make the anchor edge jump on the next scroll instead."
  []
  (let [^js sel (.getSelection js/window)
        node (when sel (.-focusNode sel))
        ^js el (when node (if (= 3 (.-nodeType node)) (.-parentElement node) node))]
    (boolean (and el (= "rtl" (.-direction (js/window.getComputedStyle el)))))))

(defn- focus-x
  "Viewport x of the selection's focus point — the end the user dragged to.
   Falls back to the box's left edge when the collapsed range yields no rect
   (an element focusNode does): the horizontal result cannot break the
   never-cover invariant, which the vertical tiers carry alone."
  [^js sel ^js box]
  (or (when-let [node (.-focusNode sel)]
        (let [^js r (js/document.createRange)]
          (try
            (.setStart r node (.-focusOffset sel))
            (.collapse r true)
            (let [^js rect (.getBoundingClientRect r)]
              (when (pos? (.-height rect)) (.-left rect)))
            (catch :default _ nil))))
    (.-left box)))

(defn- selection-geometry
  "Vertical span and focus x of the live DOM selection, or nil when there is no
   non-collapsed selection INSIDE `container`. The span is the WHOLE box
   (`getBoundingClientRect` over the range), which is what makes a multi-line
   selection unstraddleable.

   The containment test is what makes a card provably about its own editor. The
   DOM selection is global while `!open?` is per install, so an editor whose open
   state outlived its focus would otherwise place its card against a DIFFERENT
   editor's selection — a card modal over the document editor puts the two in
   overlapping coordinates. The test holds whatever Quill does or does not emit on
   blur, so no part of this depends on that behaviour.

   The focus x is NOT cached across placements: it is a viewport coordinate, so a
   scroll moves it. Direction is cached — see focus-line-rtl?."
  [^js container]
  (let [^js sel (.getSelection js/window)]
    (when (and sel (pos? (.-rangeCount sel)) (not (.-isCollapsed sel))
            (.contains container (.-focusNode sel)))
      (let [^js box (.getBoundingClientRect (.getRangeAt sel 0))]
        {:selection {:top (.-top box) :bottom (.-bottom box)}
         :focus-x (focus-x sel box)}))))

(defn- touch?
  "True on a touch-first device, where the browser draws its own selection bar
   (Cut/Copy/Paste) beside the selection. Read per placement: a tablet can gain
   or lose a mouse while the page is open."
  []
  (.-matches (.matchMedia js/window "(pointer: coarse)")))

(defn- place!
  "Position `card` (fixed) where it covers no visible selected text, or hide it
   when no tier fits. freememo.selection-placement decides; this measures.

   Pre  : `card` is in the document, `container` is the Quill container element,
          `rtl?` was read when the selection changed (focus-line-rtl?).
   Post : the card's display is \"block\" with left/top applied, or \"none\".
          The `fmt-above` class is set exactly when the card sits above the
          selection, which flips its dropdowns upward (index.css).
   Inv  : the applied rect never intersects the selection's visible rows."
  [^js card ^js container rtl?]
  ;; Display first: offsetWidth/Height read 0 while display is none. Both writes
  ;; land in one task, so a card that ends up hidden never paints.
  (set! (.. card -style -display) "block")
  (let [^js con (.getBoundingClientRect container)
        placement (when-let [geom (selection-geometry container)]
                    (sel-place/place
                      (assoc geom
                        :rtl? rtl?
                        :reserve (if (touch?) sel-place/touch-reserve 0)
                        :container {:top (.-top con) :bottom (.-bottom con)}
                        :viewport {:width (.-innerWidth js/window)
                                   :height (.-innerHeight js/window)}
                        :menu {:width (.-offsetWidth card)
                               :height (.-offsetHeight card)})))]
    (if placement
      (do (set! (.. card -style -left) (str (js/Math.round (:left placement)) "px"))
          (set! (.. card -style -top) (str (js/Math.round (:top placement)) "px"))
          (if (= :above (:side placement))
            (.add (.-classList card) "fmt-above")
            (.remove (.-classList card) "fmt-above")))
      (set! (.. card -style -display) "none"))))

;; ---------------------------------------------------------------------------
;; install! — lifecycle
;; ---------------------------------------------------------------------------

(defn install!
  "Build the format menu for `editor` and wire its listeners. Returns a
   teardown fn removing the listeners and the card.

   Pre:  `editor` is an initialized Quill instance whose native `.ql-tooltip`
         is hidden by CSS.
   Post: a non-empty user selection in `editor` places the card clear of every
         visible selected row, or shows nothing when no position clears them; a
         collapse / Escape / outside-click hides it; scroll & resize re-place it,
         hiding the card while the selection is out of view and placing it again
         on the way back. Teardown leaves no listeners or DOM behind.

   `opts` (default {}) = {:image? :cloze?}; see the ns docstring. Multiple
   editors each build their own card — the card carries no id, only the
   `.format-menu` class, so N instances never collide."
  ([^js editor] (install! editor {}))
  ([^js editor opts]
   (let [q editor
        container (.-container q)
        !rng (atom nil)                                   ; cached selection
        {:keys [card refresh !sync card-count-sync]} (build-card q #(deref !rng) opts)
        ;; Unique per-install key so N editors' watches never collide.
        watch-key (gensym "fmt-cardcount")
        ;; A selection wants the card — true even while place! keeps it hidden
        ;; because the selection scrolled out of the container. Guarding on the
        ;; card's own display instead would strand it: the scroll that hides it
        ;; would also stop the scroll back from ever re-placing it.
        !open? (atom false)
        ;; Read when the selection changes, not per placement — focus-line-rtl?.
        !rtl? (atom false)
        open? #(deref !open?)
        place-now! #(place! card container @!rtl?)
        hide! (fn [] (reset! !open? false) (set! (.. card -style -display) "none"))
        show! (fn [] (reset! !open? true) (reset! !rtl? (focus-line-rtl?))
                (refresh) (place-now!))
        ;; After a control mutates the doc: refresh active-state only — do NOT
        ;; re-place. Applying a format (e.g. bold widens the word) nudges the
        ;; selection's centre, and re-placing would jitter the card under the
        ;; cursor. Position changes only on a new selection / scroll / resize.
        _ (reset! !sync (fn [] (when (open?) (refresh))))
        on-selection (fn [^js range _old _source]
                       (if (and range (pos? (.-length range)))
                         (do (reset! !rng {:index (.-index range) :length (.-length range)})
                             (show!))
                         (hide!)))
        reposition (fn [_] (when (open?) (place-now!)))
        on-doc-down (fn [^js e]
                      (when (and (open?)
                              (not (.contains card (.-target e)))
                              (not (.contains (.-root q) (.-target e))))
                        (hide!)))
        on-key (fn [^js e] (when (and (open?) (= "Escape" (.-key e))) (hide!)))
        unregister-popover! (editor-popovers/register! open? hide!)]
    (set! (.. card -style -display) "none")
    (.appendChild js/document.body card)
    (.on q "selection-change" on-selection)
    (.addEventListener (.-root q) "scroll" reposition false)
    (.addEventListener js/document "scroll" reposition true)   ; capture: outer-ancestor scroll
    (.addEventListener js/window "resize" reposition)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "keydown" on-key)
    ;; Card-gen row (if present): paint from the shared global now, and keep it
    ;; live when the toolbar stepper changes the count while the popup is open.
    (when card-count-sync
      (card-count-sync)
      (add-watch cc/!card-count watch-key (fn [_ _ _ _] (card-count-sync))))
    (fn teardown []
      (.off q "selection-change" on-selection)
      (.removeEventListener (.-root q) "scroll" reposition false)
      (.removeEventListener js/document "scroll" reposition true)
      (.removeEventListener js/window "resize" reposition)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "keydown" on-key)
      (unregister-popover!)
      (when card-count-sync (remove-watch cc/!card-count watch-key))
      (when (.-parentNode card) (.remove card))))))
