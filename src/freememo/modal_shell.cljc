(ns freememo.modal-shell
  "Shared modal-chrome helpers.

   `ModalEscape` is the cross-modal Escape-to-close behavior. The codebase has no
   single wrapping modal component (each modal is hand-rolled), so rather than a
   higher-order shell this is a child component that operates on its ENCLOSING DOM
   node — the established Electric composition idiom (a called e/defn runs in the
   caller's DOM context, so its dom/On attaches to the enclosing element)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

;; Frame-safe focus-on-mount. HTML :autofocus does NOT fire on dynamically
;; inserted elements (a modal opened on click never receives focus from it),
;; so rAF schedules .focus after DOM attach + layout. Electric itself uses the
;; identical rAF dance for the :autofocus option on Forms5 inputs
;; (electric_forms5.cljc `Input`) — the skill's ":autofocus over rAF" rule
;; refers to those Forms5 options, not to raw dom/props on modal containers.
;; Top-level platform-split defn called uniformly from the reactive body — no
;; #?(:cljs …) in the e/defn body (which would diverge CLJ/CLJS frame slot
;; counts).
#?(:cljs
   (defn focus-on-mount! [node]
     (when node (.requestAnimationFrame js/window (fn [] (.focus node))))))
#?(:clj (defn focus-on-mount! [_node] nil))

;; Trap Tab within `container` so focus cycles the modal's focusable elements
;; instead of escaping to the page behind it. Call from a keydown handler on
;; the modal container; top-level platform-split defn keeps it frame-safe.
#?(:cljs
   (defn trap-tab! [container e]
     (when (= (.-key e) "Tab")
       (let [els (.querySelectorAll container
                   "a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex='-1'])")
             n (.-length els)]
         (when (pos? n)
           (let [first-el (.item els 0)
                 last-el (.item els (dec n))
                 active (.-activeElement js/document)]
             (cond
               (and (.-shiftKey e) (or (= active first-el) (= active container)))
               (do (.preventDefault e) (.focus last-el))

               (and (not (.-shiftKey e)) (= active last-el))
               (do (.preventDefault e) (.focus first-el)))))))))
#?(:clj (defn trap-tab! [_container _e] nil))

;; Focus restore (WCAG dialog pattern): remember the element that had focus
;; when the modal opened; put focus back there when the modal unmounts.
;; Platform-split so the e/defn body stays frame-safe.

;; Capture-then-focus in ONE synchronous call so the ordering cannot race:
;; the opener is read before .focus is even scheduled (the focus itself is
;; rAF-deferred, and rAF callbacks cannot run mid-mount on a single thread).
#?(:cljs
   (defn capture-opener-then-focus!
     "Returns document.activeElement (the modal's opener), then schedules
      focus of `node` on the next animation frame."
     [node]
     (let [opener (.-activeElement js/document)]
       (focus-on-mount! node)
       opener)))
#?(:clj (defn capture-opener-then-focus! [_node] nil))

#?(:cljs
   (defn opener-element [] (.-activeElement js/document)))
#?(:clj (defn opener-element [] nil))

#?(:cljs
   (defn restore-focus! [opener]
     (when (and opener (.contains js/document.documentElement opener))
       (.focus opener))))
#?(:clj (defn restore-focus! [_opener] nil))

(e/defn FocusReturn
  "Capture the focused element at mount; refocus it when the frame unmounts
   (the WCAG dialog focus-restore half, for modals with custom key handling).

   Pre  : any focus movement into the modal is DEFERRED (rAF, as
          focus-on-mount! does) — the capture here runs synchronously during
          mount, so a synchronous .focus elsewhere in the same frame body
          could win the race and be captured as the opener.
   Post : on frame unmount, focus returns to the captured element if it is
          still in the document."
  []
  (e/client
    (let [opener (e/snapshot (opener-element))]
      (e/on-unmount #(restore-focus! opener)))))

(e/defn ModalEscape
  "Dialog chrome for the enclosing modal: ARIA semantics, focus management,
   Escape-to-close, Tab trap.

   Call as a child of the modal's outermost element. Sets `role=dialog`,
   `aria-modal`, `aria-label` (from `label`) and `tabindex=-1` on the
   enclosing node, focuses it on mount (via rAF — `:autofocus` does not fire
   on dynamically inserted elements), traps Tab inside it, closes on Escape,
   and restores focus to the element that was focused when the modal opened.
   Listeners attach to the enclosing dom/node — the established Electric
   composition idiom.

   Pre  : `close!` is a 0-arg thunk; `label` names the dialog for AT.
   Post : the modal is focused on mount and announced as a dialog; Escape with
          focus anywhere inside it runs close!; Tab cycles within the modal;
          on unmount focus returns to the opener (if still in the document).
   Invariant: the opener is captured in the same synchronous call that
          schedules the (deferred) focus — capture-before-focus by
          construction, not by luck of concurrent body ordering."
  [close! label]
  (e/client
    (dom/props {:role "dialog" :aria-modal "true" :aria-label label
                :tabindex "-1"})
    (let [opener (e/snapshot (capture-opener-then-focus! dom/node))]
      (e/on-unmount #(restore-focus! opener)))
    (dom/On "keydown"
      (fn [e]
        (cond
          (= (.-key e) "Escape") (close!)
          (= (.-key e) "Tab")    (trap-tab! (.-currentTarget e) e)))
      nil)))
