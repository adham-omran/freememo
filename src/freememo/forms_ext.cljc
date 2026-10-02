(ns freememo.forms-ext
  "Tracked fields that Forms5 does not ship, built to its field contract.
   Forms5 has Input!, Checkbox! and radio pickers, and no tracked <select>
   (plans/document-options-autosave-sheet.md section 6). A field here returns
   what a Forms5 field returns, so a Form! or a service consumes it unchanged."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]))

(e/defn Select!
  "A tracked native <select>, with the contract of forms/Input!.

   Pre:  `v` is the authoritative option value, a string. nil reads as \"\".
         `options` is a seq of [value label], with string values.
   Post: returns an empty e/amb until the user picks an option. Then it returns
         [t {k value}] while the token is held. The <select> carries aria-busy
         while the token is held, and aria-invalid after the token fails.
   Invariant: while a token is held, the user's pick stays selected whatever
         `v` does — the \"don't damage user input\" rule of Input!. Once the
         token is spent, `v` is shown again, so the caller owns updating `v`."
  [k v options & {:as props}]
  (e/client
    v ; consume v, as Input! does
    (dom/select
      (dom/props (or props {}))
      ;; The event object, never its value: two picks of one value (a retry
      ;; after a failed save) must still be two tokens.
      (let [event (dom/On "change" identity nil)
            [t err] (e/Token event)
            waiting? (some? t)
            picked (when waiting? ((fn [] (-> event .-target .-value))))
            shown (if waiting? picked (str (or v "")))]
        (e/for [[value label] (e/diff-by first options)]
          (dom/option (dom/props {:value value :selected (= value shown)})
            (dom/text label)))
        (when (forms/tx-error? err) (dom/props {:aria-invalid true}))
        (e/When waiting?
          (dom/props {:aria-busy true})
          [t {k picked}])))))
