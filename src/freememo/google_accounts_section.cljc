(ns freememo.google-accounts-section
  "Settings → Google Accounts: the Google accounts FreeMemo reads Docs from
   (plans/google-docs-pull.md, row 2.5).

   Connecting is a full-page trip to Google's account chooser, so it is a link
   and not a form. Disconnecting deletes the stored grant. Every Doc imported
   through the account keeps its snapshots, and its next Pull says that the
   account is not connected.

   A token never reaches the client. The list shows the email only."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.google-accounts :as accounts])
   #?(:clj [freememo.user-state :as us])))

(defn accounts*
  "Post: [{:sub :email :reconnect?} …] for every connected account."
  [_refresh user-id]
  #?(:clj (accounts/accounts user-id)
     :cljs []))

(defn disconnect!*
  "Post: account `sub` is no longer connected; :settings-refresh is bumped."
  [user-id sub]
  #?(:clj (do (accounts/disconnect! user-id sub)
              (commands/bump! user-id :set-setting)
              {:ok true})
     :cljs nil))

(def settings-path
  "This section's own address, so a connection returns the user to it."
  "/settings#settings-google")

(defn connect-href
  "Post: the route that starts connecting an account and returns to `return-to`."
  [return-to]
  (str "/auth/google/account?return_to="
    #?(:cljs (js/encodeURIComponent return-to)
       :clj return-to)))

(e/defn ConnectLink
  "A link that sends the browser through Google's account chooser."
  [return-to label]
  (e/client
    (dom/a
      (dom/props {:class "btn btn-sm btn-secondary" :href (connect-href return-to)})
      (dom/text label))))

(e/defn GoogleAccountsSection
  "Pre : `user-id` is signed in.
   Post: the connected accounts are listed by email, each removable, and a link
         connects another."
  [user-id]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          saved (e/server (e/Offload #(accounts* settings-refresh user-id)))]
      (dom/div
        (dom/props {:class "forge-tokens-section"})
        (dom/p
          (dom/props {:class "hint"})
          (dom/text (str "Connect a Google account to import Google Docs from it. "
                      "It can be another account than the one you sign in with, such as a work account. "
                      "FreeMemo only reads, and only the Docs you pick.")))
        (if (empty? saved)
          (dom/p (dom/props {:class "hint"}) (dom/text "No Google account is connected."))
          (dom/ul
            (dom/props {:class "forge-tokens"})
            (e/for [{:keys [sub email reconnect?]} (e/diff-by :sub saved)]
              (dom/li
                (dom/props {:class "forge-tokens__row"})
                (dom/span (dom/props {:dir "ltr"})
                  (dom/text (if reconnect? (str email " (connect again)") email)))
                (when reconnect? (ConnectLink settings-path "Connect again"))
                (e/for [[t _] (forms/Button! [::disconnect] :class "btn btn-sm btn-danger" :label "Disconnect")]
                  (case (e/server (e/Offload #(disconnect!* user-id sub)))
                    (t)))))))
        (ConnectLink settings-path "Connect a Google account")))))
