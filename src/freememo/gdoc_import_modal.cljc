(ns freememo.gdoc-import-modal
  "Import → Google Doc (plans/google-docs-pull.md, rows 4.1 and 4.2).

   The modal lists the connected Google accounts. Choose a Doc beside one opens
   Google's Picker on THAT account's Drive, and the pick is imported as a
   `gdoc` root with its first snapshot. With no account connected, the modal
   offers the connection instead, and Google's account chooser returns here."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.google-accounts-section :as accounts-section :refer [ConnectLink]]
   [freememo.modal-shell :as modal]
   [freememo.navigation :as nav]
   #?(:cljs [freememo.gdoc-import :as gdoc-import])
   #?(:clj [freememo.user-state :as us])))

;; A top-level platform split, never a #? in the e/defn body (docs/electric-patterns.md).
#?(:cljs
   (defn- start-import!
     "Post: the pick and import run, and `!status` follows them. A finished
      import closes the modal and opens the Doc."
     [sub !status close! navigate!]
     (gdoc-import/import-doc! sub
       (fn [{:keys [phase topic-id] :as status}]
         (if (= :done phase)
           (do (close!) (navigate! :viewer (nav/nav-topic topic-id nil)))
           (reset! !status status)))))
   :clj
   (defn- start-import! [_sub _!status _close! _navigate!] nil))

(defn- status-line
  "Post: one line for the user about `status`, or nil when nothing runs."
  [{:keys [phase message]}]
  (case phase
    :picking "Opening Google's file picker…"
    :importing "Importing the Doc…"
    :cancelled "No Doc was picked."
    :error message
    nil))

(e/defn AccountRow
  "One connected account and its Choose a Doc button."
  [{:keys [sub email reconnect?]} busy? choose!]
  (e/client
    (dom/li
      (dom/props {:class "forge-tokens__row"})
      (dom/span (dom/props {:dir "ltr"}) (dom/text email))
      (if reconnect?
        (ConnectLink "/import" "Connect again")
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-primary" :disabled busy?})
          (dom/text "Choose a Doc")
          (dom/On "click" (fn [_] (choose! sub)) nil))))))

(e/defn GdocImportModal
  "Pre : `!show` holds this modal's source id while it is open; writing false
         closes it. `user-id` is signed in.
   Post: a picked Doc is imported and opened, or the modal says why not."
  [!show user-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          saved (e/server (e/Offload #(accounts-section/accounts* refresh user-id)))
          !status (atom nil)
          status (e/watch !status)
          busy? (contains? #{:picking :importing} (:phase status))
          close! (fn [] (reset! !show false))
          choose! (fn [sub] (start-import! sub !status close! navigate!))]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (modal/ModalEscape close! "Import a Google Doc")
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (close!))) nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "520px" :max-width "92%"}})
          (dom/h3 (dom/text "Import a Google Doc"))
          (dom/p
            (dom/props {:class "hint"})
            (dom/text (str "The Doc is read-only in FreeMemo. Pull on the Doc's page adds a new "
                        "snapshot when the Doc changed.")))
          (if (empty? saved)
            (dom/p (dom/props {:class "hint"}) (dom/text "No Google account is connected."))
            (dom/ul
              (dom/props {:class "forge-tokens"})
              (e/for [account (e/diff-by :sub saved)]
                (AccountRow account busy? choose!))))
          (dom/div
            (dom/props {:style {:margin-top "var(--sp-3)"}})
            (ConnectLink "/import" (if (empty? saved) "Connect a Google account" "Connect another account")))
          (when-let [line (status-line status)]
            (dom/p
              (dom/props {:class (if (= :error (:phase status)) "wt-picker__error" "hint") :role "status"})
              (dom/text line)))
          (when-let [existing (when (= :error (:phase status)) (:topic-id status))]
            (dom/button
              (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
              (dom/text "Open it")
              (dom/On "click" (fn [_] (close!) (navigate! :viewer (nav/nav-topic existing nil))) nil)))
          (dom/div
            (dom/props {:class "modal-actions"})
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"})
              (dom/text "Close")
              (dom/On "click" (fn [_] (close!)) nil))))))))
