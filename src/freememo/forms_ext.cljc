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

(e/defn RadioGroup!
  "A tracked group of native radio inputs, with the contract of forms/Input!.

   Pre:  `v` is the authoritative option value, a string. nil or \"\" reads as
         no choice. `options` is a seq of [value label] with unique string
         values. `group` is a radio name that is unique on the page.
   Post: returns an empty e/amb until the user picks an option. Then it returns
         [t {k value}] while the token is held. The <fieldset> carries
         aria-busy while the token is held, and aria-invalid after the token
         fails.
   Invariant: while a token is held, the user's pick stays checked whatever
         `v` does. Once the token is spent, `v` is shown again, so the caller
         owns updating `v`. The same rule as Select!."
  [k v options group legend & {:as props}]
  (e/client
    v ; consume v, as Input! does
    (dom/fieldset
      (dom/props (or props {}))
      (dom/legend (dom/text legend))
      ;; "change" bubbles from each radio to the fieldset. The event object,
      ;; never its value: two picks of one value must still be two tokens.
      (let [event (dom/On "change" identity nil)
            [t err] (e/Token event)
            waiting? (some? t)
            picked (when waiting? ((fn [] (-> event .-target .-value))))
            shown (if waiting? picked (str (or v "")))]
        ;; The loop has an element of its own, so the legend beside it never
        ;; shares its child slots. See "An e/for Gets an Element of Its Own".
        (dom/div
          (dom/props {:class "radio-group-options"})
          (e/for [[value label] (e/diff-by first options)]
            (dom/label
              (dom/input (dom/props {:type "radio" :name group :value value
                                     :checked (= value shown)}))
              (dom/span (dom/text label)))))
        (when (forms/tx-error? err) (dom/props {:aria-invalid true}))
        (e/When waiting?
          (dom/props {:aria-busy true})
          [t {k picked}])))))
