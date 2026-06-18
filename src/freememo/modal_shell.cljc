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
;; so rAF schedules .focus after DOM attach + layout. Top-level platform-split
;; defn called uniformly from the reactive body — no #?(:cljs …) in the e/defn
;; body (which would diverge CLJ/CLJS frame slot counts).
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

(e/defn ModalEscape
  "Close the enclosing modal on Escape, and focus it on mount.

   Call as a child of the modal's outermost element, which MUST carry
   `:tabindex \"-1\"` so it can take keyboard focus and receive keydown.
   Focuses the enclosing node on mount (via rAF — `:autofocus` does not fire
   on dynamically inserted elements), so Escape and tabbing work before the
   user clicks anything. The keydown listener attaches to the enclosing
   dom/node — the established Electric composition idiom.

   Pre  : enclosing dom/node has tabindex=-1; `close!` is a 0-arg thunk.
   Post : the modal is focused on mount; Escape with focus anywhere inside it
          runs close!."
  [close!]
  (e/client
    (focus-on-mount! dom/node)
    (dom/On "keydown"
      (fn [e]
        (cond
          (= (.-key e) "Escape") (close!)
          (= (.-key e) "Tab")    (trap-tab! (.-currentTarget e) e)))
      nil)))
