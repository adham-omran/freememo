(ns freememo.select
  "Shared UI: native single-choice <select> bound to an atom (A1-fallback).
   Forms5 has no tracked <select> primitive; this is the project default for
   dropdown UI (see CLAUDE.md Forms5 section)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(defn ellipsize
  "Middle-truncate s to ~max-len chars with an ellipsis, keeping head + tail so
   a hierarchical name (e.g. Anki 'Root::…::Leaf') shows both its root and leaf.
   macOS renders the native option list at OS level and ignores CSS truncation,
   so the option DISPLAY text must be shortened here; the full string stays as
   the <option> value. Returns s unchanged when short."
  [s max-len]
  (let [s (str s)
        n (count s)]
    (if (<= n max-len)
      s
      (let [keep (dec max-len)
            head (quot (* keep 3) 5)
            tail (- keep head)]
        (str (subs s 0 head) "…" (subs s (- n tail)))))))

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
        (dom/props {:class "select"
                    ;; Cap the closed control to its container and ellipsize the
                    ;; selected label; option text is shortened below for the
                    ;; (OS-rendered) open popup.
                    :style {:font-size "15px" :width "100%" :max-width "100%"
                            :white-space "nowrap" :overflow "hidden" :text-overflow "ellipsis"}
                    :value (if unmatched? "" (str value))})
        (when placeholder
          (dom/option (dom/props {:value "" :disabled true}) (dom/text placeholder)))
        (e/for [opt (e/diff-by identity opts)]
          (dom/option (dom/props {:value opt}) (dom/text (ellipsize opt 48))))
        (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
          (when (and (some? v) (not= v ""))
            (reset! !value v)
            (when !committed (reset! !committed v))))))))
