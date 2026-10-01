(ns freememo.a11y
  "Keyboard-operability helpers for click-only elements (WCAG 2.1.1), and the
   keyboard contract of every Quill editor (Tab indents, Escape leaves).

   `KeyActivate` follows the modal-shell composition idiom: a called e/defn
   runs in the caller's DOM context, so dom/props and dom/On here apply to
   the ENCLOSING element."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [clojure.string :as str]
   #?(:cljs [freememo.editor-popovers :as editor-popovers])))

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

;; ---------------------------------------------------------------------------
;; Quill keyboard contract. Each editor takes ONE of two Tab contracts:
;;   - install-tab-indent! (the document editor): Tab and Shift+Tab indent and
;;     outdent. Escape is the way out, which WCAG 2.1.2 permits because the
;;     editor announces it (`editor-hint-id`).
;;   - free-quill-tab! (card fields): Tab moves focus like any control, and the
;;     format menu's indent buttons indent.
;; install-editor-escape! serves both. See plans/bullet-indent-tab-behavior.md.
;; ---------------------------------------------------------------------------

(def editor-hint-id
  "Id of the one visually hidden element that every Tab-indenting editor
   root names in aria-describedby."
  "editor-keyboard-hint")

(def editor-hint-text
  "Tab indents. Press Escape, then Tab, to leave the editor.")

#?(:cljs
   (defn- quill-keeps-tab?
     "True for the Quill Tab bindings this app keeps: code-block indentation
      (format {\"code-block\" true}) and table cell-hop (format [\"table\"])."
     [^js binding]
     (let [fmt (.-format binding)]
       (boolean
         (cond
           (array? fmt) (some #{"table"} fmt)
           (some? fmt)  (true? (aget fmt "code-block")))))))

#?(:cljs
   (defn- line-indent-handler
     "Quill binding handler that changes the indent of every selected line by
      `delta` (\"+1\" or \"-1\"). Returns true — pass the key on to the next
      binding — on a code-block or table line, whose own Tab binding owns it."
     [^js editor delta]
     (fn [_range ^js context _binding]
       (let [^js fmt (.-format context)]
         (if (or (.hasOwnProperty fmt "code-block") (.hasOwnProperty fmt "table"))
           true
           (do (.format editor "indent" delta "user")
               false))))))

#?(:cljs
   (defn- ensure-editor-hint!
     "Append the shared hint element to document.body once. A foreign node is
      safe there: Electric adds and removes body children by identity."
     []
     (when-not (.getElementById js/document editor-hint-id)
       (let [^js hint (.createElement js/document "span")]
         (set! (.-id hint) editor-hint-id)
         (set! (.-className hint) "sr-only")
         (set! (.-textContent hint) editor-hint-text)
         (.appendChild (.-body js/document) hint)))))

(defn free-quill-tab!
  "Remove every Quill Tab binding, so Tab and Shift+Tab move focus like any
   control. The card fields take this contract: a modal of several fields is
   navigated with Tab, and the format menu's indent buttons indent.

   Pre  : `editor` is an initialized Quill instance (bindings registered).
   Post : no Quill binding matches Tab; keydown Tab is not prevented by Quill.
   Inv  : exclusive with `install-tab-indent!` — an editor takes one."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js bindings (.-bindings (.-keyboard ^js editor))]
       ;; Quill 2 keys bindings by key name; older code paths used keycode 9.
       (js-delete bindings "Tab")
       (js-delete bindings "tab")
       (js-delete bindings "9"))))

(defn install-tab-indent!
  "Make Tab indent and Shift+Tab outdent every selected line, with the caret
   anywhere in the line, and announce the way out.

   Pre  : `editor` is an initialized Quill instance (bindings registered).
   Post : the editor root names the keyboard hint in aria-describedby, because
          Tab no longer leaves this editor.
          bindings[\"Tab\"] is, in order: indent, outdent, then Quill's own
          code-block and table Tab bindings. Quill's `tab` binding (insert a
          tab character) and `remove tab` binding are gone. So a Tab keydown
          in the editor is always prevented and never moves focus; in a code
          block it indents the code, and in a table it moves to the next cell.
   Inv  : at level 8 Tab, and at level 0 Shift+Tab, change nothing but still
          take the key — the editor owns Tab on every line.
          Quill's `match` requires an omitted modifier to be absent, so the
          explicit `:shiftKey false` keeps the two bindings apart.
          Exclusive with `free-quill-tab!` — an editor takes one."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js bindings (.-bindings (.-keyboard ed))
           kept (filter quill-keeps-tab? (array-seq (or (aget bindings "Tab") #js [])))]
       (ensure-editor-hint!)
       (.setAttribute (.-root ed) "aria-describedby" editor-hint-id)
       (aset bindings "Tab"
         (into-array
           (concat [#js {:key "Tab" :shiftKey false :handler (line-indent-handler ed "+1")}
                    #js {:key "Tab" :shiftKey true  :handler (line-indent-handler ed "-1")}]
                   kept)))
       ;; Quill 2 keys bindings by key name; older code paths used keycode 9.
       (js-delete bindings "tab")
       (js-delete bindings "9"))))

(defn install-editor-escape!
  "Escape inside the editor first closes an open editor popover, and otherwise
   leaves the editor, so the next Tab reaches the control after it.

   Pre  : `editor` is an initialized Quill instance.
   Post : Escape whose target is inside the root never reaches an ancestor —
          so an enclosing modal's ModalEscape does not close on it. It either
          closes the open popovers (focus stays) or gives the Quill container
          tabindex=-1 and focuses it. The container drops the tabindex when it
          loses focus, so a later mousedown on its padding cannot focus it.
          Escape whose target is the container itself passes through: the
          second Escape closes the modal.
   Note : no teardown. Both listeners live on the editor's own DOM and die
          with it, as the Quill bindings die with the keyboard module."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js root (.-root ed)
           ^js container (.-container ed)]
       (.addEventListener container "keydown"
         (fn [^js e]
           (when (and (= "Escape" (.-key e))
                      (not (.-isComposing e))
                      (.contains root (.-target e)))
             (.stopPropagation e)
             (when-not (editor-popovers/close-open!)
               (.setAttribute container "tabindex" "-1")
               (.focus container #js {:preventScroll true})))))
       (.addEventListener container "blur"
         (fn [_] (.removeAttribute container "tabindex"))))))

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
