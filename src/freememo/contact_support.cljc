(ns freememo.contact-support
  "Contact support link for Help and Settings
   (plans/paying-customer-commitments.md, \"Contact support\").

   The text functions are pure. The server decides who is a Paying Customer
   (freememo.db/support-contact). The client gets the boolean and the account
   label, never order rows."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])))

(def support-address "contact@adham-omran.com")

(defn account-label
  "The account as the mail body names it: the email, or the username when the
   account has no email. `user` is a map with :email and :username."
  [{:keys [email username]}]
  (if (and (string? email) (not (str/blank? email)))
    email
    username))

(defn reply-line
  "The line under the link. `paying?` is the Paying Customer boolean."
  [paying?]
  (str "You get a reply within "
       (if paying? "1 working day" "3 working days")
       ". Working days are Saturday to Wednesday."))

(defn- encode [s]
  #?(:clj (-> (java.net.URLEncoder/encode ^String s "UTF-8")
              (str/replace "+" "%20"))
     :cljs (js/encodeURIComponent s)))

(defn mailto-href
  "The mailto: URL. `account` is the label from `account-label`."
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
                    :style {:margin-top "16px" :font-size "13px"}})
        (dom/a
          (dom/props {:href (mailto-href paying? account)
                      :style {:color "var(--color-primary-text)" :font-weight "600"
                              :text-decoration "underline"}})
          (dom/text "Contact support"))
        (dom/p
          (dom/props {:style {:margin "4px 0 0" :color "var(--color-text-secondary)"}})
          (dom/text (reply-line paying?)))))))
