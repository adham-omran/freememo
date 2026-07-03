(ns freememo.a11y
  "Keyboard-operability helpers for click-only elements (WCAG 2.1.1).

   `KeyActivate` follows the modal-shell composition idiom: a called e/defn
   runs in the caller's DOM context, so dom/props and dom/On here apply to
   the ENCLOSING element."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Quill toolbar labeling. Quill 2 renders its dropdown pickers (size, color,
;; background, align, header) as span-buttons with aria-expanded/aria-controls
;; but NO accessible name (axe: aria-command-name), and custom toolbar buttons
;; (cloze) get no label either. Called once after editor init — the toolbar
;; DOM is static from then on.
;; ---------------------------------------------------------------------------

;; Ordered: both color pickers carry the ql-color-picker class, so
;; "ql-background" must match before the "ql-color" substring.
(def ^:private quill-picker-labels
  [["ql-background" "Highlight color"]
   ["ql-size" "Text size"]
   ["ql-header" "Heading level"]
   ["ql-font" "Font"]
   ["ql-align" "Text alignment"]
   ["ql-color" "Text color"]])

(def ^:private quill-button-labels
  {".ql-cloze-inc" "Insert new cloze deletion"
   ".ql-cloze-eq" "Insert same-number cloze deletion"})

(defn label-quill-toolbar!
  "Give every Quill picker, picker option, and custom button under `root-el`
   an aria-label. Idempotent — skips elements that already have one."
  [root-el]
  #?(:clj nil
     :cljs
     (when root-el
       (doseq [^js picker (array-seq (.querySelectorAll root-el ".ql-picker"))]
         (let [cls (str (.-className picker))
               kind (some (fn [[k v]] (when (str/includes? cls k) v)) quill-picker-labels)
               ^js label-el (.querySelector picker ".ql-picker-label")]
           (when (and kind label-el (not (.getAttribute label-el "aria-label")))
             (.setAttribute label-el "aria-label" kind))
           (doseq [^js item (array-seq (.querySelectorAll picker ".ql-picker-item"))]
             (when-not (.getAttribute item "aria-label")
               (let [v (.getAttribute item "data-value")]
                 (.setAttribute item "aria-label"
                   (if (str/blank? v) "Default" v)))))))
       (doseq [[sel lbl] quill-button-labels]
         (doseq [^js b (array-seq (.querySelectorAll root-el sel))]
           (when-not (.getAttribute b "aria-label")
             (.setAttribute b "aria-label" lbl)))))))

(defn free-quill-tab!
  "Remove Quill's Tab keybindings so Tab / Shift-Tab move focus (the browser
   default for contenteditable when nothing preventDefaults) instead of
   indenting — the direct WCAG 2.1.2 no-keyboard-trap fix. Indentation inside
   code blocks is typed as spaces; Quill's table cell-hop on Tab also yields
   to focus movement.

   Pre  : `editor` is an initialized Quill instance (bindings registered).
   Post : no Quill binding matches Tab; keydown Tab bubbles unprevented and
          the browser moves focus out of the editor."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js bindings (.-bindings (.-keyboard ^js editor))]
       ;; Quill 2 keys bindings by key name; older code paths used keycode 9.
       (js-delete bindings "Tab")
       (js-delete bindings "tab")
       (js-delete bindings "9"))))

(defn install-quill-tab-escape!
  "Keyboard-trap relief (WCAG 2.1.2): Quill binds Tab to indent, so a keyboard
   user who tabs into an editor cannot tab out. Escape blurs the editor, and
   the next Tab continues past it. Skipped while the bubble tooltip is open —
   Escape then belongs to closing the tooltip (Quill handles that itself).

   Pre  : `editor` is an initialized Quill instance.
   Post : Escape pressed in the editor (tooltip closed) moves focus to <body>;
          the listener lives on the editor root and dies with its DOM."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js root (.-root ed)]
       (.addEventListener root "keydown"
         (fn [e]
           (when (= (.-key e) "Escape")
             (let [^js container (.-container ed)
                   ^js tooltip (.querySelector container ".ql-tooltip")]
               (when (or (nil? tooltip)
                         (.contains (.-classList tooltip) "ql-hidden"))
                 (.blur root)))))))))

(defn focused-enter-or-space?
  "True for an Enter/Space keydown on the focused element itself
   (target = currentTarget). A focused inner activator (e.g. an expand
   button inside an activatable cell) bubbles its keydown here with a
   different target and must not double-activate the outer element."
  [e]
  #?(:cljs (and (or (= (.-key e) "Enter") (= (.-key e) " "))
                (identical? (.-target e) (.-currentTarget e)))
     :clj false))

(e/defn KeyActivate
  "Make the enclosing click-target keyboard-operable: focusable via Tab;
   Enter/Space invoke `activate!` — the SAME fn the element's (or its row's)
   click handler uses, so the activation exists once, shared by both input
   modalities, instead of the keyboard path synthesizing a DOM click.

   `label` (nilable) sets aria-label when the element's text content is not a
   usable accessible name. No role is forced — table cells keep their cell
   role; pass `role` explicitly for generic containers (e.g. \"button\").

   Pre  : `activate!` is the click-path activation fn (called with the event);
          the enclosing element generates a box (NOT display:contents — focus
          outline would be invisible).
   Post : element is in the Tab order; Enter/Space call (activate! e) with the
          key's default (scroll/submit) suppressed; keydowns bubbling up from
          focused descendants are ignored."
  [{:keys [label role]} activate!]
  (e/client
    (dom/props (cond-> {:tabindex "0"}
                 label (assoc :aria-label label)
                 role (assoc :role role)))
    (dom/On "keydown"
      (fn [e]
        (when (focused-enter-or-space? e)
          (.preventDefault e)
          (activate! e)))
      nil)))
