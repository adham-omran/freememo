(ns freememo.card-count
  "Shared card-count state — the number of flashcards a Generate action produces.

   A single client-global atom so the plain-CLJS format menu (the Quill
   selection popup) and the Electric toolbar/dropdown read and write the SAME
   value. Defined on BOTH peers (the keyboard.cljc idiom) so the var can be
   named inside an e/defn body without a frame-signal mismatch.

   Lifecycle: `nil` until ContentToolbar seeds it from the server setting
   (`settings/get-card-count`); the toolbar stepper mutates + persists it
   directly; the popup mutates it and persists via the :persist-card-count
   invoker. Leaf ns (no app requires) — both the Electric and the plain-CLJS
   sides depend on it without a cycle.")

(def card-count-min 1)
(def card-count-max 20)

(defn clamp
  "Clamp n to [card-count-min, card-count-max]."
  [n]
  (max card-count-min (min card-count-max n)))

;; nil until ContentToolbar seeds from the server value. Readers fall back to
;; card-count-min while unseeded.
(defonce !card-count (atom nil))
