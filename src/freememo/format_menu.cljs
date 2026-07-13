(ns freememo.format-menu
  "Custom floating format menu for the bubble doc editor (Notion-inspired).

   Replaces Quill's native `.ql-tooltip`: we own the card, its placement, and
   its dismissal; Quill still owns the text model, so every button drives the
   Quill format API (`format`/`removeFormat`) against the current selection.

   Placement contract (the reason this exists — the native tooltip collided
   with the app command bar and covered content):
     - position:fixed, so no editor-pane overflow ancestor clips it.
     - never crosses ABOVE the editor content top (below the command bar).
     - anchored to the selection's first visual line; prefers BELOW it, flips
       above only when the card would overflow the viewport bottom, and caps its
       height + scrolls when neither side fits.

   `install!` wires the selection / scroll / resize / dismiss listeners, builds
   the card once, and returns a teardown fn that removes them and the card.

   `opts` (2-arity install!) turns on card-editor-only controls: `:image?`
   adds an image-insert button, `:cloze?` adds a cloze-deletion row. Both drive
   freememo.editor-actions; the document editor calls the no-opts arity."
  (:require [clojure.string :as str]
            [freememo.editor-actions :as editor-actions]))

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

(defn- glyph-button
  "A single icon/glyph toolbar button. `on-click` fires with no args."
  [glyph title on-click]
  (let [b (el "button" "fmt-btn")]
    (set! (.-type b) "button")
    (set! (.-innerHTML b) glyph)
    (.setAttribute b "title" title)
    (.setAttribute b "aria-label" title)
    (keep-selection! b)
    (.addEventListener b "click" (fn [^js e] (.stopPropagation e) (on-click)))
    b))

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

(defn- build-card
  "Build the menu card and its controls. Returns
   {:card <el> :refresh (fn []) :!sync <atom>}.
   `refresh` syncs active-state from the cached range. Every control, after
   mutating the document, calls `(@!sync)` — install! sets that to
   refresh + re-place, so active-state and geometry stay current after a format
   (e.g. a heading changes line height). `get-rng` returns the cached
   {:index :length}. `opts` = {:image? :cloze?} toggles the card-editor-only
   controls (§ ns docstring)."
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
        row-blockfmt (section)]
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
    (.appendChild row-blockfmt (:wrap align-dd))
    (.appendChild row-blockfmt (glyph-button "⇄" "Right-to-left" (after #(toggle-value! q (get-rng) "direction" "rtl"))))
    (.appendChild row-blockfmt (glyph-button "⊞" "Insert table" (after #(insert-table! q (get-rng)))))
    ;; Image insert (card editors only). Uploads via the picker → /api/media,
    ;; inserting at the cached range index; never embeds a data-URI.
    (when (:image? opts)
      (.appendChild row-blockfmt
        (glyph-button image-glyph "Insert image"
          (after #(editor-actions/insert-image! q (:index (get-rng)))))))
    (.appendChild card row-block)
    (.appendChild card (el "div" "fmt-divider"))
    (.appendChild card row-inline)
    (.appendChild card (el "div" "fmt-divider"))
    (.appendChild card row-blockfmt)
    ;; Cloze deletion row (cloze card only). reselect! restores the cached
    ;; range before insert-cloze! reads the live selection — focus moved to the
    ;; button, so the editor's own getSelection would otherwise be stale.
    (when (:cloze? opts)
      (let [cloze-row (section)]
        (.appendChild cloze-row
          (glyph-button cloze-inc-glyph "New cloze deletion"
            (after #(do (reselect! q (get-rng)) (editor-actions/insert-cloze! q :inc)))))
        (.appendChild cloze-row
          (glyph-button cloze-eq-glyph "Same cloze deletion"
            (after #(do (reselect! q (get-rng)) (editor-actions/insert-cloze! q :eq)))))
        (.appendChild card (el "div" "fmt-divider"))
        (.appendChild card cloze-row)))
    {:card card
     :!sync !sync
     :refresh
     (fn []
       (let [af (active-format q (get-rng))]
         (doseq [{:keys [fmt el]} @toggles]
           (let [on? (boolean (aget af fmt))]
             (if on? (.add (.-classList el) "active") (.remove (.-classList el) "active"))
             (.setAttribute el "aria-pressed" (str on?))))
         (set! (.-textContent (:label block-dd)) (block-label af))))}))

;; ---------------------------------------------------------------------------
;; Placement
;; ---------------------------------------------------------------------------

(defn- first-line-rect
  "Viewport rect of the selection's first visual line (where it starts)."
  []
  (let [sel (.getSelection js/window)]
    (when (and sel (pos? (.-rangeCount sel)))
      (let [rects (.getClientRects (.getRangeAt sel 0))]
        (when (pos? (.-length rects)) (aget rects 0))))))

(defn- place!
  "Position `card` (fixed) relative to the selection, clamped to the region
   [content-top, viewport-bottom]. Prefer below; flip above if it overflows the
   viewport; cap height + scroll if neither side fits."
  [^js card ^js container]
  (when-let [line (first-line-rect)]
    (let [m 8, gap 8
          vw (.-innerWidth js/window)
          vh (.-innerHeight js/window)
          region-top (.-top (.getBoundingClientRect container))   ; below the command bar
          region-bottom (- vh m)
          ;; measure at natural height, overflow visible so popups aren't clipped
          _ (set! (.. card -style -maxHeight) "")
          _ (set! (.. card -style -overflowY) "")
          w (.-offsetWidth card)
          h (.-offsetHeight card)
          below-top (+ (.-bottom line) gap)
          above-top (- (.-top line) gap h)
          [top capped?] (cond
                          (<= (+ below-top h) region-bottom) [below-top false]
                          (>= above-top region-top)          [above-top false]
                          :else                              [region-top true])
          left (max m (min (- (+ (.-left line) (/ (.-width line) 2)) (/ w 2))
                          (- vw w m)))]
      (set! (.. card -style -left) (str (js/Math.round left) "px"))
      (set! (.. card -style -top) (str (js/Math.round top) "px"))
      (when capped?
        (set! (.. card -style -maxHeight) (str (js/Math.round (- region-bottom region-top)) "px"))
        (set! (.. card -style -overflowY) "auto")))))

;; ---------------------------------------------------------------------------
;; install! — lifecycle
;; ---------------------------------------------------------------------------

(defn install!
  "Build the format menu for `editor` and wire its listeners. Returns a
   teardown fn removing the listeners and the card.

   Pre:  `editor` is an initialized Quill instance whose native `.ql-tooltip`
         is hidden by CSS.
   Post: a non-empty user selection shows the card near the selection; a
         collapse / Escape / outside-click hides it; scroll & resize keep it
         placed. Teardown leaves no listeners or DOM behind.

   `opts` (default {}) = {:image? :cloze?}; see the ns docstring. Multiple
   editors each build their own card — the card carries no id, only the
   `.format-menu` class, so N instances never collide."
  ([^js editor] (install! editor {}))
  ([^js editor opts]
   (let [q editor
        container (.-container q)
        !rng (atom nil)                                   ; cached selection
        {:keys [card refresh !sync]} (build-card q #(deref !rng) opts)
        visible? #(= "block" (.. card -style -display))
        hide! (fn [] (set! (.. card -style -display) "none"))
        show! (fn [] (set! (.. card -style -display) "block") (refresh) (place! card container))
        ;; After a control mutates the doc: refresh active-state only — do NOT
        ;; re-place. Applying a format (e.g. bold widens the word) nudges the
        ;; selection's centre, and re-placing would jitter the card under the
        ;; cursor. Position changes only on a new selection / scroll / resize.
        _ (reset! !sync (fn [] (when (visible?) (refresh))))
        on-selection (fn [^js range _old _source]
                       (if (and range (pos? (.-length range)))
                         (do (reset! !rng {:index (.-index range) :length (.-length range)})
                             (show!))
                         (hide!)))
        reposition (fn [_] (when (visible?) (place! card container)))
        on-doc-down (fn [^js e]
                      (when (and (visible?)
                              (not (.contains card (.-target e)))
                              (not (.contains (.-root q) (.-target e))))
                        (hide!)))
        on-key (fn [^js e] (when (and (visible?) (= "Escape" (.-key e))) (hide!)))]
    (set! (.. card -style -display) "none")
    (.appendChild js/document.body card)
    (.on q "selection-change" on-selection)
    (.addEventListener (.-root q) "scroll" reposition false)
    (.addEventListener js/document "scroll" reposition true)   ; capture: outer-ancestor scroll
    (.addEventListener js/window "resize" reposition)
    (.addEventListener js/document "mousedown" on-doc-down true)
    (.addEventListener js/document "keydown" on-key)
    (fn teardown []
      (.off q "selection-change" on-selection)
      (.removeEventListener (.-root q) "scroll" reposition false)
      (.removeEventListener js/document "scroll" reposition true)
      (.removeEventListener js/window "resize" reposition)
      (.removeEventListener js/document "mousedown" on-doc-down true)
      (.removeEventListener js/document "keydown" on-key)
      (when (.-parentNode card) (.remove card))))))
