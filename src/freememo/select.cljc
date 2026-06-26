(ns freememo.select
  "Shared UI: native single-choice <select> bound to an atom (A1-fallback).
   Forms5 has no tracked <select> primitive; this is the project default for
   dropdown UI (see CLAUDE.md Forms5 section)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn AtomSelect
  "Native <select> bound to !value. Options are rendered sorted A-Z. Shows a
   disabled placeholder when the current value is nil or absent from options,
   so the control never silently mis-displays.

   On change: (reset! !value v); and (reset! !committed v) when !committed is
   given — a native change is always a definitive selection, so it doubles as
   the commit/save trigger.

   Returns nothing meaningful — callers read !value (A1-fallback control).

   Pre:  !value is an atom holding a string (or nil); options is a seq of strings.
   Post: !value holds a chosen option after any user change."
  [!value options & {:keys [placeholder !committed]}]
  (e/client
    (let [value      (e/watch !value)
          opts       (vec (sort options))
          unmatched? (or (nil? value) (not (some #{value} opts)))]
      (dom/select
        (dom/props {:class "select" :style {:font-size "15px"}
                    :value (if unmatched? "" (str value))})
        (when placeholder
          (dom/option (dom/props {:value "" :disabled true}) (dom/text placeholder)))
        (e/for [opt (e/diff-by identity opts)]
          (dom/option (dom/props {:value opt}) (dom/text opt)))
        (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
          (when (and (some? v) (not= v ""))
            (reset! !value v)
            (when !committed (reset! !committed v))))))))
