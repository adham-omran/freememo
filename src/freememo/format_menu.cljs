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

   Layout (plans/format-menu-notion-layout.md), top to bottom: a full-width
   block-type row; a 5×2 icon grid whose last cell, More formatting, opens Size, Align,
   indent, direction and table; an Insert section; then a labelled Cloze or
   Cards section. Every glyph is a freememo.icon-paths entry, so the whole card
   draws on one grid at one stroke weight. At most one popup is open, and
   hiding the card closes it.

   `opts` (2-arity install!) turns on optional controls: `:image?` adds an
   image-insert row, `:audio?` an audio-insert row, `:cloze?` adds the Cloze
   section (all three drive freememo.editor-actions), `:card-gen?` adds the
   document editor's Cards section: count, Generate here, and Parse
   questions.
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
            [freememo.icon-paths :as icon-paths]
            [freememo.selection-placement :as sel-place]))

;; ---------------------------------------------------------------------------
;; Icons. Every control draws a freememo.icon-paths entry: Lucide's 24-unit
;; grid and stroke contract, inline SVG with stroke="currentColor" so it takes
;; the button's text colour. One size for cells and labelled rows alike.
;; ---------------------------------------------------------------------------

(def ^:private icon-px 18)

(defn- icon [k] (icon-paths/svg-markup k icon-px))

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

(defn- on-click!
  "Run `f` on a click of `node`, keeping the click inside the card."
  [node f]
  (.addEventListener node "click" (fn [^js e] (.stopPropagation e) (f))))

(defn- glyph-button
  "A single icon button, one grid cell. `on-click` fires with no args.

   `label` is the accessible name; `tooltip` (default `label`) is the hover text.
   The two diverge exactly where a chord belongs in the tooltip but not in the
   accessible name — `ExtractTopicButton`/`AddCardButton`'s established shape."
  ([icon-k label on-click] (glyph-button icon-k label on-click label))
  ([icon-k label on-click tooltip]
   (let [b (el "button" "fmt-btn")]
     (set! (.-type b) "button")
     (set! (.-innerHTML b) (icon icon-k))
     (.setAttribute b "title" tooltip)
     (.setAttribute b "aria-label" label)
     (keep-selection! b)
     (on-click! b on-click)
     b)))

(defn- item-button
  "A labelled row button: icon, label, and an optional trailing `end` text in
   secondary colour (a chord). The label is the accessible name; `tooltip`
   (default `label`) is the hover text."
  [icon-k label on-click & {:keys [tooltip end]}]
  (let [b   (el "button" "fmt-btn fmt-item")
        txt (el "span" "fmt-item-label")]
    (set! (.-type b) "button")
    (set! (.-innerHTML b) (icon icon-k))
    (set! (.-textContent txt) label)
    (.appendChild b txt)
    (when end
      (let [e (el "span" "fmt-item-end")]
        (set! (.-textContent e) end)
        (.appendChild b e)))
    (.setAttribute b "title" (or tooltip label))
    (.setAttribute b "aria-label" label)
    (keep-selection! b)
    (on-click! b on-click)
    b))

(defn- section
  "One card section. Consecutive sections draw a rule between them (index.css)."
  []
  (el "div" "fmt-section"))

(defn- heading [text]
  (let [h (el "div" "fmt-heading")]
    (set! (.-textContent h) text)
    h))

(defn- popup-group
  "The card's popups (block type, colour, overflow), of which at most one is
   open.

   Post: `:toggle!` opens `popup` and closes any other, or closes `popup` when
         it was the open one; the open popup's trigger carries `.active` and
         `aria-expanded=\"true\"`. `:close!` leaves none open.
   Inv : at most one popup in the group has the `open` class."
  []
  (let [!open  (atom nil)                       ; {:popup :trigger} or nil
        close! (fn []
                 (when-let [{:keys [popup trigger]} @!open]
                   (.remove (.-classList popup) "open")
                   (.remove (.-classList trigger) "active")
                   (.setAttribute trigger "aria-expanded" "false")
                   (reset! !open nil)))]
    {:close! close!
     :toggle! (fn [popup trigger]
                (let [was-open? (= popup (:popup @!open))]
                  (close!)
                  (when-not was-open?
                    (.add (.-classList popup) "open")
                    (.add (.-classList trigger) "active")
                    (.setAttribute trigger "aria-expanded" "true")
                    (reset! !open {:popup popup :trigger trigger}))))}))

(defn- popover
  "Wire `trigger` to open `popup` within `group`. Returns the wrapping element
   that anchors the popup."
  [{:keys [toggle!]} ^js trigger ^js popup]
  (let [wrap (el "div" "fmt-dd")]
    (.setAttribute trigger "aria-haspopup" "true")
    (.setAttribute trigger "aria-expanded" "false")
    (keep-selection! trigger)
    (keep-selection! popup)
    (on-click! trigger #(toggle! popup trigger))
    (.appendChild wrap trigger)
    (.appendChild wrap popup)
    wrap))

;; Block types, in list order. `apply!` sets the type on the cached range.
(def ^:private block-types
  [{:key :normal :label "Normal text" :icon :pilcrow
    :apply! (fn [q rng] (set-format! q rng "header" false)
              (set-format! q rng "code-block" false))}
   {:key :h1 :label "Heading 1" :icon :heading-1
    :apply! (fn [q rng] (set-format! q rng "header" 1))}
   {:key :h2 :label "Heading 2" :icon :heading-2
    :apply! (fn [q rng] (set-format! q rng "header" 2))}
   {:key :h3 :label "Heading 3" :icon :heading-3
    :apply! (fn [q rng] (set-format! q rng "header" 3))}
   {:key :code :label "Code block" :icon :code-xml
    :apply! (fn [q rng] (set-format! q rng "code-block" true))}])

(defn- block-type
  "The block-types entry for Quill format map `fmt`. A mixed selection reads as
   Normal text, because Quill reports a mixed format as an array."
  [^js fmt]
  (let [k (cond (aget fmt "code-block")   :code
                (= 1 (aget fmt "header")) :h1
                (= 2 (aget fmt "header")) :h2
                (= 3 (aget fmt "header")) :h3
                :else                     :normal)]
    (first (filter #(= k (:key %)) block-types))))

(defn- block-type-menu
  "The full-width block-type row: current type's icon, its label, a chevron.
   Opens the type list. Returns {:wrap :icon :label}; refresh repaints :icon
   and :label."
  [group ^js q get-rng after]
  (let [trigger (el "button" "fmt-dd-trigger fmt-block-trigger")
        ico     (el "span" "fmt-block-icon")
        label   (el "span" "fmt-dd-label")
        caret   (el "span" "fmt-dd-caret")
        popup   (el "div" "fmt-dd-popup fmt-block-popup")]
    (set! (.-type trigger) "button")
    (.setAttribute trigger "aria-label" "Block type")
    (set! (.-innerHTML caret) (icon :chevron-down))
    (.appendChild trigger ico)
    (.appendChild trigger label)
    (.appendChild trigger caret)
    (doseq [{item-label :label item-icon :icon apply! :apply!} block-types]
      (let [item (el "button" "fmt-dd-item")
            txt  (el "span" "fmt-item-label")]
        (set! (.-type item) "button")
        (set! (.-innerHTML item) (icon item-icon))
        (set! (.-textContent txt) item-label)
        (.appendChild item txt)
        (keep-selection! item)
        (on-click! item (after #(do ((:close! group)) (apply! q (get-rng)))))
        (.appendChild popup item)))
    (let [wrap (popover group trigger popup)]
      (.add (.-classList wrap) "fmt-block")
      {:wrap wrap :icon ico :label label})))

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
    (on-click! b pick!)
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
  "The colour cell, opening Text-colour and Background-colour preset grids
   (Notion merges both under one control). Returns the wrapping element."
  [group ^js q get-rng after]
  (let [trigger (el "button" "fmt-btn")
        popup   (el "div" "fmt-dd-popup fmt-colour-popup")
        close!  (:close! group)]
    (set! (.-type trigger) "button")
    (set! (.-innerHTML trigger) (icon :baseline))
    (.setAttribute trigger "title" "Text and background colour")
    (.setAttribute trigger "aria-label" "Text and background colour")
    (.appendChild popup (colour-section q get-rng after "color" :text "Text color" text-colours close!))
    (.appendChild popup (colour-section q get-rng after "background" :bg "Background color" background-colours close!))
    (popover group trigger popup)))

(defn- overflow-popover
  "The More formatting cell: Size, Align, then indent, direction and table, flat in one
   popup. Returns the wrapping element.

   Post: a Size, Align, direction or table control applies and closes the
         popup. Outdent and indent apply and leave it open, because a reader
         often moves a line more than one level."
  [group ^js q get-rng after]
  (let [trigger (el "button" "fmt-btn")
        popup   (el "div" "fmt-dd-popup fmt-overflow-popup")
        close!  (:close! group)
        closing (fn [f] (after #(do (close!) (f))))
        sizes   (el "div" "fmt-segments")
        aligns  (el "div" "fmt-grid fmt-grid-4")
        blocks  (el "div" "fmt-grid fmt-grid-4")
        size-sec  (section)
        align-sec (section)
        block-sec (section)]
    (set! (.-type trigger) "button")
    (set! (.-innerHTML trigger) (icon :ellipsis))
    (.setAttribute trigger "title" "More formatting")
    (.setAttribute trigger "aria-label" "More formatting")
    (doseq [[label value] [["Small" "small"] ["Normal" false] ["Large" "large"] ["Huge" "huge"]]]
      (let [b (el "button" "fmt-segment")]
        (set! (.-type b) "button")
        (set! (.-textContent b) label)
        (.setAttribute b "aria-label" (str label " size"))
        (keep-selection! b)
        (on-click! b (closing #(set-format! q (get-rng) "size" value)))
        (.appendChild sizes b)))
    (doseq [[icon-k label value] [[:text-align-start "Align left" false]
                                  [:text-align-center "Align center" "center"]
                                  [:text-align-end "Align right" "right"]
                                  [:text-align-justify "Justify" "justify"]]]
      (.appendChild aligns
        (glyph-button icon-k label (closing #(set-format! q (get-rng) "align" value)))))
    ;; Every selected line moves one level. Card fields have no other way to
    ;; indent: Tab moves between fields there (a11y/free-quill-tab!).
    (.appendChild blocks (glyph-button :list-indent-decrease "Decrease indent"
                           (after #(set-format! q (get-rng) "indent" "-1"))))
    (.appendChild blocks (glyph-button :list-indent-increase "Increase indent"
                           (after #(set-format! q (get-rng) "indent" "+1"))))
    (.appendChild blocks (glyph-button :pilcrow-left "Right-to-left"
                           (closing #(toggle-value! q (get-rng) "direction" "rtl"))))
    (.appendChild blocks (glyph-button :table "Insert table"
                           (closing #(insert-table! q (get-rng)))))
    (.appendChild size-sec (heading "Size"))
    (.appendChild size-sec sizes)
    (.appendChild align-sec (heading "Align"))
    (.appendChild align-sec aligns)
    (.appendChild block-sec blocks)
    (.appendChild popup size-sec)
    (.appendChild popup align-sec)
    (.appendChild popup block-sec)
    (popover group trigger popup)))

;; ---------------------------------------------------------------------------
;; Card assembly
;; ---------------------------------------------------------------------------

(defn- card-gen-section
  "The document editor's Cards section: a `Count −/+` stepper over the shared
   cc/!card-count global, `Generate here`, and `Parse questions`. Returns
   {:section :sync}, where `sync` repaints the number and −/+ disabled state
   from the global (called on build and on every external change via
   install!'s add-watch).

   Pre:  `card` is the menu card element (hidden to dismiss); `get-rng` returns
         the cached selection {:index :length}.
   Post: −/+ clamp to [card-count-min, card-count-max], mutate the global, and
         persist via the :persist-card-count invoker (no-op if unpublished, e.g.
         reading-mode). `Generate here` and `Parse questions` each restore the
         selection, dispatch their command once (:generate, :parse-questions),
         and hide the card. Parsing and persistence stay in the mounted
         Electric invokers. `keep-selection!` blocks the mousedown blur so the
         cached range survives the click."
  [^js q get-rng ^js card]
  (let [sec      (section)
        row      (el "div" "fmt-cardgen-row")
        stepper  (el "div" "number-stepper")
        lbl      (el "span" "number-stepper-label")
        dec-btn  (el "button" "number-stepper-btn")
        numspan  (el "span" "number-stepper-value")
        inc-btn  (el "button" "number-stepper-btn")
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
                   (sync!))
        dispatch-hiding! (fn [command-id]
                           (reselect! q (get-rng))   ; restore the cached range
                           (bus/dispatch! command-id)
                           (set! (.. card -style -display) "none"))]
    (set! (.-textContent lbl) "Count")
    (set! (.-type dec-btn) "button")
    (set! (.-textContent dec-btn) "−")
    (.setAttribute dec-btn "aria-label" "Fewer cards to generate")
    (set! (.-type inc-btn) "button")
    (set! (.-textContent inc-btn) "+")
    (.setAttribute inc-btn "aria-label" "More cards to generate")
    (keep-selection! dec-btn)
    (keep-selection! inc-btn)
    (on-click! dec-btn #(step! -1))
    (on-click! inc-btn #(step! 1))
    (.appendChild stepper lbl)
    (.appendChild stepper dec-btn)
    (.appendChild stepper numspan)
    (.appendChild stepper inc-btn)
    (.appendChild row stepper)
    (.appendChild sec (heading "Cards"))
    (.appendChild sec row)
    (.appendChild sec (item-button :sparkles "Generate here" #(dispatch-hiding! :generate)
                        :tooltip "Generate cards from the selection"))
    (.appendChild sec (item-button :circle-help "Parse questions" #(dispatch-hiding! :parse-questions)
                        :tooltip "Create basic cards from selected question-and-answer lines"))
    {:section sec :sync sync!}))

(defn- cloze-section
  "The cloze card's Cloze section: New cloze and Same cloze, each with its
   chord. reselect! restores the cached range before insert-cloze! reads the
   live selection — focus moved to the button, so the editor's own
   getSelection would otherwise be stale. The chords themselves need no
   reselect!: they fire with focus still in the editor."
  [^js q get-rng after]
  (let [sec (section)
        ;; One formatter, so a row cannot state a chord the binding does not
        ;; fire. The ids name the action rather than a registry command — these
        ;; chords are deliberately absent from freememo.commands, and
        ;; display-chord reads `id` only for the palette's Firefox carve-out.
        chord (fn [id mode] (commands/display-chord id (get editor-actions/cloze-chords mode)))
        row   (fn [icon-k label id mode]
                (let [c (chord id mode)]
                  (item-button icon-k label
                    (after #(do (reselect! q (get-rng)) (editor-actions/insert-cloze! q mode)))
                    :end c
                    :tooltip (str label " (" c ")"))))]
    (.appendChild sec (heading "Cloze"))
    (.appendChild sec (row :cloze-new "New cloze" :cloze-inc :inc))
    (.appendChild sec (row :cloze-same "Same cloze" :cloze-eq :eq))
    sec))

(defn- build-card
  "Build the menu card and its controls. Returns
   {:card <el> :refresh (fn []) :!sync <atom> :close-popups! (fn [])
    :card-count-sync (fn [] | nil)}.

   Layout, top to bottom: the block-type row; a 5×2 grid (colour, bold,
   italic, underline, clear / strike, code, bullet, numbered, More formatting); Insert;
   then Cloze (:cloze?) or Cards (:card-gen?).

   `refresh` syncs active-state from the cached range. Every control, after
   mutating the document, calls `(@!sync)` — install! sets that to refresh
   ALONE, deliberately not re-placing: a format that changes the text metrics
   would otherwise jitter the card under the cursor. `get-rng` returns the cached
   {:index :length}. `opts` = {:image? :audio? :cloze? :card-gen?}
   (§ ns docstring). :card-count-sync is nil unless :card-gen?."
  [^js q get-rng opts]
  (let [card    (el "div" "format-menu")
        group   (popup-group)
        toggles (atom [])            ; [{:fmt :value :el}]; :value nil = boolean format
        !sync   (atom (fn []))       ; set by install! to refresh
        sync!   (fn [] (@!sync))
        after   (fn [thunk] (fn [] (thunk) (sync!)))    ; apply, then resync
        add-toggle! (fn [grid icon-k title fmt value on-click]
                      (let [b (glyph-button icon-k title (after on-click))]
                        (swap! toggles conj {:fmt fmt :value value :el b})
                        (.appendChild grid b)))
        inline! (fn [grid icon-k title fmt]
                  (add-toggle! grid icon-k title fmt nil #(toggle-inline! q (get-rng) fmt)))
        list!   (fn [grid icon-k title value]
                  (add-toggle! grid icon-k title "list" value #(toggle-value! q (get-rng) "list" value)))
        block-menu (block-type-menu group q get-rng after)
        grid       (el "div" "fmt-grid")
        format-sec (section)
        insert-sec (section)
        media?     (or (:image? opts) (:audio? opts))
        cardgen    (when (:card-gen? opts) (card-gen-section q get-rng card))
        page-break-btn (item-button :page-boundary "Restore page boundary"
                         (after #(editor-actions/insert-page-break! q (:index (get-rng)))))
        block-sec  (section)]
    (.appendChild block-sec (:wrap block-menu))
    ;; Row 1 of the grid — colour, then character formats.
    (.appendChild grid (colour-popover group q get-rng after))
    (inline! grid :bold "Bold" "bold")
    (inline! grid :italic "Italic" "italic")
    (inline! grid :underline "Underline" "underline")
    (.appendChild grid (glyph-button :remove-formatting "Clear formatting"
                         (after #(clear-format! q (get-rng)))))
    ;; Row 2 — the rest of the character formats, the lists, and the overflow.
    (inline! grid :strikethrough "Strikethrough" "strike")
    (inline! grid :code-xml "Inline code" "code")
    (list! grid :list "Bullet list" "bullet")
    (list! grid :list-ordered "Numbered list" "ordered")
    (.appendChild grid (overflow-popover group q get-rng after))
    (.appendChild format-sec grid)
    ;; Insert. Image (card editors only) uploads via the picker → /api/media,
    ;; inserting at the cached range index; never embeds a data-URI. Audio is
    ;; MP3 only, because `api/upload-media-handler` accepts exactly audio/mpeg
    ;; beside image/*; the picker's accept filter says so before the server has
    ;; to. Drag and paste reach the same upload through
    ;; `editor-actions/install-attachment-upload!`, so these buttons are the
    ;; discoverable half of one path, not a second one.
    (when (:image? opts)
      (.appendChild insert-sec
        (item-button :image "Image" (after #(editor-actions/insert-image! q (:index (get-rng))))
          :tooltip "Insert image")))
    (when (:audio? opts)
      (.appendChild insert-sec
        (item-button :volume-2 "Audio" (after #(editor-actions/insert-audio! q (:index (get-rng))))
          :tooltip "Insert audio")))
    ;; Restore a page boundary (plans/merged-page-reading.md §4.3-4.7).
    ;;
    ;; Built ALWAYS and hidden by `refresh` below, NOT gated by `opts`:
    ;; install! takes its opts once per editor construction and the document
    ;; editor is persistent, so opts cannot carry a flag the reader toggles.
    ;;
    ;; It restores a lost boundary and never creates a page. `restorable-page`
    ;; answers nil when there is nothing to restore, and insert-page-break!
    ;; refuses on nil, so the button cannot mint a page number.
    (.appendChild insert-sec page-break-btn)
    (.add (.-classList insert-sec) "fmt-insert")
    (.appendChild card block-sec)
    (.appendChild card format-sec)
    (.appendChild card insert-sec)
    (when (:cloze? opts)
      (.appendChild card (cloze-section q get-rng after)))
    (when cardgen
      (.appendChild card (:section cardgen)))
    {:card card
     :!sync !sync
     :close-popups! (:close! group)
     :card-count-sync (:sync cardgen)      ; nil when no Cards section
     :refresh
     (fn []
       (let [af (active-format q (get-rng))
             {type-label :label type-icon :icon} (block-type af)]
         (doseq [{:keys [fmt value el]} @toggles]
           (let [cur (aget af fmt)
                 on? (if (some? value) (= value cur) (boolean cur))]
             (if on? (.add (.-classList el) "active") (.remove (.-classList el) "active"))
             (.setAttribute el "aria-pressed" (str on?))))
         (set! (.-textContent (:label block-menu)) type-label)
         (set! (.-innerHTML (:icon block-menu)) (icon type-icon))
         ;; The page-break control's visibility, per selection. It is present
         ;; only in focus reader's merged mode, and disabled there when every
         ;; page between the neighbours already has a marker — which is the
         ;; refusal item 4.7 asks for, stated in the control rather than in a
         ;; toast the reader would have to trigger to read. With no media
         ;; control and no page-break, the Insert section has nothing to show.
         (let [ctx @editor-actions/!page-break-context
               page (when ctx (editor-actions/restorable-page q (:index (get-rng))))]
           (set! (.. page-break-btn -style -display) (if ctx "" "none"))
           (set! (.. insert-sec -style -display) (if (or media? ctx) "" "none"))
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
        {:keys [card refresh !sync card-count-sync close-popups!]} (build-card q #(deref !rng) opts)
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
        hide! (fn [] (reset! !open? false) (close-popups!)
                (set! (.. card -style -display) "none"))
        show! (fn [] (reset! !open? true) (reset! !rtl? (focus-line-rtl?))
                (refresh) (place-now!))
        ;; After a control mutates the doc: refresh active-state only — do NOT
        ;; re-place. Applying a format (e.g. bold widens the word) nudges the
        ;; selection's centre, and re-placing would jitter the card under the
        ;; cursor. Position changes only on a new selection / scroll / resize.
        _ (reset! !sync (fn [] (when (open?) (refresh))))
        on-selection (fn [^js range _old _source]
                       (if (and range (pos? (.-length range)))
                         (let [rng {:index (.-index range) :length (.-length range)}]
                           ;; A popup belongs to the selection it opened on.
                           (when (not= rng @!rng) (close-popups!))
                           (reset! !rng rng)
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
