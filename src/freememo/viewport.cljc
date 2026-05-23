(ns freememo.viewport
  "Reactive viewport signal. `!phone?` mirrors `window.matchMedia('(max-width: 600px)')`
   and updates on the MediaQueryList's `change` event. Declared on both CLJ and
   CLJS so `(e/watch !phone?)` compiles identically across the dual-compiler.")

(defonce !phone? (atom false))

#?(:cljs
   (defonce phone-mq-installed?
     (let [mq (.matchMedia js/window "(max-width: 600px)")]
       (reset! !phone? (.-matches mq))
       (.addEventListener mq "change" (fn [e] (reset! !phone? (.-matches e))))
       true)))
