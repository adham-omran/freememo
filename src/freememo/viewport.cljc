(ns freememo.viewport
  "Reactive viewport signals mirroring `window.matchMedia` queries, each seeded
   on load and kept live on its MediaQueryList `change` event. Declared on both
   CLJ and CLJS so `(e/watch ...)` compiles identically across the dual-compiler:
     !phone?   — max-width 600px (phone; distraction-free reading mode)
     !compact? — max-width 900px (phone + portrait tablet; collapse side panels)
     !coarse?  — pointer: coarse (touch; suppress text-input autofocus)")

(defonce !phone? (atom false))
(defonce !compact? (atom false))
(defonce !coarse? (atom false))

#?(:cljs
   (defn- install-mq!
     "Seed `!signal` from `query`'s current match and keep it live on change.
      Pre:  `!signal` is a top-level atom; `query` a media-query string.
      Post: `@!signal` always equals the MediaQueryList's `.matches`.
      Returns true so several installs can sit behind one defonce guard."
     [!signal query]
     (let [mq (.matchMedia js/window query)]
       (reset! !signal (.-matches mq))
       (.addEventListener mq "change" (fn [e] (reset! !signal (.-matches e))))
       true)))

#?(:cljs
   (defonce media-queries-installed?
     (and (install-mq! !phone?   "(max-width: 600px)")
          (install-mq! !compact? "(max-width: 900px)")
          (install-mq! !coarse?  "(pointer: coarse)"))))
