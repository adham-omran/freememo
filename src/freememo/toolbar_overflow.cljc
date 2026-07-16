(ns freememo.toolbar-overflow
  "Content-aware toolbar collapse — shared by the main ContentToolbar and the
   PDF second bar (PdfToolbar). A ResizeObserver + MutationObserver finds the
   smallest `.collapse-N` tier where the toolbar's content fits without
   horizontal overflow and writes that class onto the container.

   The per-tier CSS (which elements hide at which tier) lives in index.css and
   is owned by each toolbar's own markup; this fn only measures and tags.")

;; Pre:  `container` is a mounted DOM element with class "toolbar-container";
;;       `toolbar` is its child with class "toolbar". `!tier` and `!overflow-open`
;;       are atoms whose values must reach the rendered className.
;; Post: A ResizeObserver is installed on both nodes. On each observed size
;;       change, the tier loop (0..7) writes `.toolbar-container collapse-N`
;;       imperatively and emits the smallest fitting tier into `!tier`. Returns
;;       a 0-arg cleanup fn that disconnects the observer.
;; Invariant: kept as a plain (defn) outside any e/defn so its body is opaque
;;            to Electric's analyzer — slot counts on the e/defn caller stay
;;            identical between JVM and CLJS compile (the divergence-causing
;;            #?(:cljs …) lives inside this fn, never inside Electric AST).
(defn install-overflow-detector! [container toolbar !tier !overflow-open]
  #?(:cljs
     (let [classlist (.-classList container)
           tier-classes #js ["collapse-0" "collapse-1" "collapse-2"
                             "collapse-3" "collapse-4" "collapse-5"
                             "collapse-6" "collapse-7"]
           apply-tier!
           (fn [t]
             ;; classList ops touch only collapse-N; Electric's reactive
             ;; class binding owns the rest of the className. No race.
             (.apply (.-remove classlist) classlist tier-classes)
             (.add classlist (aget tier-classes t)))
           ;; Pure measure — CSS owns the trigger's space (see index.css
           ;; `.toolbar-overflow-trigger` per-tier rules). At tier 1 the
           ;; trigger is `display:flex; visibility:hidden` so its width is in
           ;; `scrollWidth` even before it's visible: the algorithm naturally
           ;; escalates to tier 2 when content + trigger slot can't fit.
           recompute
           (fn []
             (loop [t 0]
               (when (<= t 7)
                 (apply-tier! t)
                 (if (<= (.-scrollWidth toolbar) (+ (.-clientWidth toolbar) 1))
                   (reset! !tier t)
                   (recur (inc t))))))
           resize-obs (js/ResizeObserver. recompute)
           ;; ResizeObserver fires only on the observed element's own box
           ;; changes (parent resize). Internal content growth (e.g. an
           ;; Export label gaining "(2)" suffix) doesn't change the toolbar's
           ;; box, so we ALSO observe DOM subtree mutations to catch text /
           ;; child-list / attribute changes that may grow content width.
           mut-obs (js/MutationObserver. recompute)]
       (.observe resize-obs toolbar)
       (.observe resize-obs container)
       (.observe mut-obs toolbar
         #js {:childList true :subtree true :characterData true :attributes true})
       (recompute)
       (fn []
         (.disconnect resize-obs)
         (.disconnect mut-obs)))
     :clj (fn [] nil)))

;; Pre:  a dropdown menu (class `menu-class`) opened from a trigger (class
;;       `trigger-class`) is about to mount as a descendant of `.toolbar`, whose
;;       `overflow-x: clip` (the tier system above) would otherwise horizontally
;;       clip a menu wider than — or offset from — the toolbar's box.
;; Post: pins the menu with INLINE `position: fixed` + top/left directly under the
;;       trigger, clamped into the viewport (left-align to the trigger, else
;;       right-align, else clamp to the right edge). `fixed` escapes the ancestor
;;       overflow clip entirely while the coords keep it anchored to the trigger.
;;       Re-pins on window resize. Returns a 0-arg cleanup fn.
;; Why inline (not CSS): only the toolbar dropdowns that call this become fixed,
;;       so other `.toolbar-dropdown-menu` users OUTSIDE the toolbar (e.g. library
;;       row actions) keep their CSS `position: absolute` anchor untouched.
(defn install-dropdown-menu-position! [trigger-class menu-class]
  #?(:cljs
     (let [trigger-sel (str "." trigger-class)
           menu-sel    (str "." menu-class)
           margin 8
           reposition
           (fn []
             (when-let [trigger (.querySelector js/document trigger-sel)]
               (when-let [menu (.querySelector js/document menu-sel)]
                 (let [tr (.getBoundingClientRect trigger)
                       mw (.-offsetWidth menu)
                       vw (.-innerWidth js/window)
                       left (cond
                              (<= (+ (.-left tr) mw) (- vw margin)) (.-left tr)
                              (>= (- (.-right tr) mw) margin)       (- (.-right tr) mw)
                              :else                                 (- vw mw margin))
                       s (.-style menu)]
                   (set! (.-position s) "fixed")
                   (set! (.-right s) "auto")
                   (set! (.-left s) (str (max margin left) "px"))
                   (set! (.-top s) (str (+ (.-bottom tr) 4) "px"))))))]
       ;; The menu mounts on the same reactive turn, right after this installer
       ;; runs — defer one frame so offsetWidth/layout is measurable.
       (js/requestAnimationFrame reposition)
       (.addEventListener js/window "resize" reposition)
       (fn [] (.removeEventListener js/window "resize" reposition)))
     :clj (fn [] nil)))
