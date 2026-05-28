(ns freememo.credits-return-page
  "Pay-return page (§5.8.6). Wayl redirects users here after checkout with
   `?referenceId=fm-…&orderid=…` in the query string. The page reactively shows
   the order state (pending → complete → ✓ N credits added), with a manual
   reconciliation button for environments where Wayl's webhook can't reach us
   (dev on localhost, or a missed delivery in prod)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.credits :as credits])
   #?(:clj [freememo.user-state :as us])))

;; Defined on both platforms so referencing in e/defn bodies doesn't shift
;; frame signal counts between CLJ and CLJS.
#?(:cljs (defn url-query-param [k]
           (when (exists? js/URLSearchParams)
             (let [p (js/URLSearchParams. (.-search js/location))]
               (.get p k))))
   :clj  (defn url-query-param [_k] nil))

#?(:clj (defn order-snapshot*
          "Reactive wrapper — `_refresh` (a :credits-refresh watch) forces a
           re-query whenever the webhook credits an order."
          [_refresh reference-id]
          (when reference-id
            (when-let [o (db/get-credit-order reference-id)]
              {:status (:credit_orders/status o)
               :amount (:credit_orders/amount_iqd o)
               :user-id (:credit_orders/user_id o)}))))

(e/defn CreditsReturnPage [user-id navigate!]
  (e/client
    (let [reference-id (url-query-param "referenceId")
          credits-refresh (e/server (e/watch (us/get-atom user-id :credits-refresh)))
          order (e/server (order-snapshot* credits-refresh reference-id))
          status (:status order)
          amount (:amount order)
          !reconcile-error (atom nil)
          reconcile-error (e/watch !reconcile-error)]
      (dom/div
        (dom/props {:style {:max-width "560px" :margin "60px auto" :padding "32px"
                            :background "var(--color-bg-card, #fff)" :border-radius "var(--radius-md)"
                            :border "1px solid var(--color-border)" :text-align "center"}})

        (cond
          (nil? reference-id)
          (dom/div
            (dom/h2 (dom/text "No order to confirm"))
            (dom/p (dom/props {:class "hint"})
              (dom/text "This page expects a referenceId in the URL.")))

          (= status "complete")
          (dom/div
            (dom/h2 (dom/props {:style {:color "var(--color-success, #2a8a4a)" :margin-top "0"}})
              (dom/text "✓ Payment confirmed"))
            (dom/p (dom/text (str (or amount 0) " credits added to your account."))))

          (= status "failed")
          (dom/div
            (dom/h2 (dom/props {:style {:color "var(--color-danger)" :margin-top "0"}})
              (dom/text "Payment failed"))
            (dom/p (dom/text "The order could not be completed. Your balance is unchanged.")))

          :else
          (dom/div
            (dom/h2 (dom/props {:style {:margin-top "0"}}) (dom/text "Confirming payment…"))
            (dom/p (dom/props {:class "hint"})
              (dom/text "This usually takes a few seconds. If it lingers, the payment provider's confirmation may have been missed — use Check status to reconcile."))
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"
                          :style {:margin-top "12px" :padding "6px 14px" :font-size "13px"}})
              (let [ev (dom/On "click" identity nil)
                    [t _] (e/Token ev)]
                (dom/text (if (some? t) "Checking…" "Check status"))
                (when t
                  (let [r (e/server (e/Offload #(credits/reconcile-order! reference-id)))]
                    (case r
                      (if (:ok r)
                        ;; Bump :credits-refresh so order-snapshot* re-queries —
                        ;; without this the UI stays on "Confirming…" even after
                        ;; the server has marked the order complete.
                        (case (e/server (swap! (us/get-atom user-id :credits-refresh) inc))
                          (do (if (:credited r)
                                (reset! !reconcile-error nil)
                                (when-not (#{"Complete" "Delivered"} (:wayl-status r))
                                  (reset! !reconcile-error
                                    (str "Still " (or (:wayl-status r) "pending")
                                      " on Wayl's side."))))
                              (t)))
                        (do (reset! !reconcile-error (:error r))
                            (t (:error r)))))))))))

        (when reconcile-error
          (dom/div
            (dom/props {:style {:margin-top "10px" :font-size "13px" :color "var(--color-danger)"}})
            (dom/text reconcile-error)))

        (dom/button
          (dom/props {:type "button" :class "btn btn-primary"
                      :style {:margin-top "20px" :padding "8px 20px"}})
          (dom/text "Back to Settings")
          (dom/On "click" (fn [_] (navigate! :settings)) nil))))))
