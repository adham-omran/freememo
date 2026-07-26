(ns freememo.credits-section
  "Credit balance + top-up panel for official deployments.

   Lived inside `ai_features_section` until storage metering shipped, gated
   behind the Enable-LLM-features toggle. That is now wrong: storage rent debits
   credits by the gigabyte-month whether or not the user has ever turned on an
   AI feature, so a user with LLM features off could run to zero, lose video
   playback, and have no way to see a balance or top it up. Credits are an
   account concern with two spend sources — AI actions and stored bytes — so the
   panel belongs in the Account tab beside Storage and the cost ledger.

   Self-host (CREDITS_ENABLED unset) never renders this; the AI card shows
   provider-key status instead, which genuinely is an AI concern."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.config :as config])
   #?(:clj [freememo.credits :as credits])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.storage-meter :as meter])
   #?(:clj [freememo.user-state :as us])))

;; Defined on both platforms (per CLAUDE.md) so referencing them in e/defn
;; bodies never causes a CLJ/CLJS frame-signal mismatch.
#?(:cljs (defn navigate-external! [url] (when url (set! (.. js/window -location -href) url)))
   :clj (defn navigate-external! [_url] nil))

(defn credit-balance*
  "Reactive wrapper — _refresh forces a re-query on :credits-refresh bump."
  [_refresh user-id]
  #?(:clj (db/get-credit-balance user-id)
     :cljs 0))

(defn storage-rate-label*
  "The storage price as one line, or nil when metering is off.
   Server-sited because the rate is deployment config the client never holds.

   `_refresh` is unused but MUST be passed. A zero-argument server call bound in
   a client-sited `let` has no reactive input, and the two compilers then emit
   different node counts for it: the client sends a Slot index the server's
   frame does not have and `runtime3/frame-signal` dies on an out-of-bounds
   aget, killing the websocket. Cost the Settings page an outage; every other
   server wrapper in this codebase takes a `_refresh` for the same reason."
  [_refresh]
  #?(:clj (meter/storage-rate-label)
     :cljs nil))

(e/defn CreditsSection
  "Balance, what spends it, and the top-up presets.

   `base-url` is the public origin (derived from ring-request at Main) — used
   for the Wayl webhook + redirection URLs so dev (localhost) and prod work
   without a config knob. `client-country` is the ISO-3166 alpha-2 code resolved
   from the client IP at session boot (nil = unknown → USD)."
  [user-id base-url client-country]
  (e/client
    (let [credits-refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          balance (e/server (credit-balance* credits-refresh user-id))
          rate-label (e/server (storage-rate-label* credits-refresh))
          presets (e/server (mapv (fn [amt] {:iqd amt :usd-str (credits/iqd->usd-str amt)})
                              (config/presets)))
          !checkout-error (atom nil)
          checkout-error (e/watch !checkout-error)]
      (dom/div
        (dom/props {:class "field"
                    :style {:padding "14px" :background "var(--color-bg-subtle)"
                            :border-radius "var(--radius-md)" :border "1px solid var(--color-bg-hover)"}})
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                              :margin-bottom "10px"}})
          (dom/span
            (dom/props {:style {:font-size "13px" :font-weight "500" :color "var(--color-text-label)"}})
            (dom/text "Credit Balance"))
          (dom/span
            (dom/props {:style {:font-size "16px" :font-weight "600"
                                :color (if (and balance (> balance 0))
                                         "var(--color-text-primary)" "var(--color-danger)")}})
            (dom/text (str (or balance 0) " credits"))))

        ;; What spends credits. Storage is named with its rate because it is the
        ;; one charge that accrues with no user action — an AI charge follows a
        ;; button press, rent does not.
        (dom/div
          (dom/props {:class "hint" :style {:margin-bottom "10px"}})
          (dom/text (if rate-label
                      (str "Spent by OCR and flashcard generation, and by stored videos at "
                        rate-label ".")
                      "Spent by OCR and flashcard generation.")))

        (dom/div (dom/props {:class "hint" :style {:margin-bottom "6px"}}) (dom/text "Top up:"))
        (dom/div
          (dom/props {:style {:display "flex" :gap "8px" :flex-wrap "wrap"}})
          (e/for [{:keys [iqd usd-str]} (e/diff-by :iqd presets)]
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"
                          :style {:padding "6px 14px" :font-size "13px" :cursor "pointer"}})
              (dom/text (str iqd " credits" (when usd-str (str " (" usd-str ")"))))
              (let [ev (dom/On "click" identity nil)
                    [t _] (e/Token ev)]
                (when t
                  (let [r (e/server (e/Offload #(credits/start-checkout! user-id iqd base-url client-country)))]
                    (case r
                      (if (:ok r)
                        (do (navigate-external! (:url r)) (t))
                        (do (reset! !checkout-error (:error r)) (t (:error r)))))))))))

        (dom/div
          (dom/props {:class "hint" :style {:margin-top "8px" :font-size "12px"
                                            :color "var(--color-text-secondary)"}})
          (dom/text "USD prices shown here are approximate and may be adjusted if we change AI models. The amount you pay is the credits amount shown."))

        (when checkout-error
          (dom/div
            (dom/props {:style {:margin-top "8px" :font-size "13px" :color "var(--color-danger-text)"}})
            (dom/text checkout-error)))))))
