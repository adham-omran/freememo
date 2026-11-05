(ns freememo.contact-support
  "Contact support link for Help and Settings
   (plans/paying-customer-commitments.md, \"Contact support\"), and the line that
   links the availability page.

   The text functions are pure. The server decides who is a Paying Customer
   (freememo.db/support-contact). The client gets the boolean and the account
   label, never order rows."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])))

(def support-address "contact@adham-omran.com")

(defn reply-line
  "The line under the link. `paying?` is the Paying Customer boolean."
  [paying?]
  (str "You get a reply within "
       (if paying? "1 working day" "3 working days")
       ". Working days are Saturday to Wednesday."))

(def availability-href "/availability")

(def availability-line
  "The line under the reply time: [before link-text after]. A signed-in user
   never sees the landing page, so this is the app's way to the availability
   page. No command palette entry: the page is outside the app, and Help and
   Settings both show this line."
  ["The " "availability page" " shows FreeMemo's availability for each month."])

(defn- encode [s]
  #?(:clj (-> (java.net.URLEncoder/encode ^String s "UTF-8")
              (str/replace "+" "%20"))
     :cljs (js/encodeURIComponent s)))

(defn mailto-href
  "The mailto: URL. `account` is the label from `freememo.db/support-contact`."
  [paying? account]
  (str "mailto:" support-address
       "?subject=" (encode (if paying? "[Paid] FreeMemo support" "FreeMemo support"))
       "&body=" (encode (str "Account: " account "\n\n\n"))))

(e/defn ContactSupport
  "The link and the reply-time line. Shows nothing until the server answers, so
   that the link never opens with the wrong subject. Pass the user id."
  [user-id]
  (e/client
    (when-some [{:keys [paying? account]}
                (peek (e/as-vec (e/server (e/Offload #(db/support-contact user-id)))))]
      (dom/div
        (dom/props {:class "contact-support"
                    :style {:margin "16px 0" :font-size "13px"}})
        (dom/a
          (dom/props {:href (mailto-href paying? account)
                      :style {:color "var(--color-primary-text)" :font-weight "600"
                              :text-decoration "underline"}})
          (dom/text "Contact support"))
        (dom/p
          (dom/props {:style {:margin "4px 0 0" :color "var(--color-text-secondary)"}})
          (dom/text (reply-line paying?)))
        ;; A new tab, so that the user keeps their place in the app.
        (let [[before link after] availability-line]
          (dom/p
            (dom/props {:class "contact-support-availability"
                        :style {:margin "4px 0 0" :color "var(--color-text-secondary)"}})
            (dom/text before)
            (dom/a
              (dom/props {:href availability-href :target "_blank" :rel "noopener"
                          :style {:color "var(--color-primary-text)" :text-decoration "underline"}})
              (dom/text link))
            (dom/text after)))))))
