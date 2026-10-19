(ns freememo.desktop-section
  "Settings → Desktop App (plans/electron-local-db-sync.md 3.2, 5.2, 13.7).

   One tab, two bodies, chosen by the peer:
   - On the desktop: sync state, Sync now, the full-sync choice (2.8), the
     cached license (13.6), and the conflict report with Restore (5.2).
   - On freememo.net: the license and how to buy it (13.1), and the desktop
     installs signed in to the account, each revocable (3.2).

   Command palette decision (8.2, 13.8.2): no entries. Sync runs by itself
   (2.11), and buying is a once-per-major action. Both live here only.
   Activity monitor decision (8.3): no entry. A sync reports its state on
   this tab and in no other place.
   Tab title decision (8.4): the conflict report is a Settings group, not a
   route, so `Main` titles it as Settings."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.settings-group :as group]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.desktop-auth :as desktop-auth])
   #?(:clj [freememo.desktop-license :as license])
   #?(:clj [freememo.desktop-version :as desktop-version])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.peer :as peer])
   #?(:clj [freememo.sync-client :as sync-client])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [next.jdbc :as jdbc])
   #?(:clj [next.jdbc.result-set :as rs])))

#?(:cljs (defn navigate-external! [url] (when url (set! (.. js/window -location -href) url)))
   :clj (defn navigate-external! [_url] nil))

;; ── Server wrappers ────────────────────────────────────────────────
;; Each takes `_refresh` so it re-queries on :desktop-mutations, and each is
;; defined on both peers because reactive bodies name it (CLAUDE.md).

(defn desktop?* [_refresh]
  #?(:clj (peer/desktop?) :cljs false))

(defn sync-status* [_refresh]
  #?(:clj (sync-client/status) :cljs nil))

(defn sync-now!* []
  #?(:clj (sync-client/sync-now!) :cljs nil))

(defn choose-full-sync!* [direction]
  #?(:clj (sync-client/choose-full-sync! (keyword direction)) :cljs nil))

(defn conflicts* [_refresh]
  #?(:clj (sync-client/open-conflicts) :cljs []))

(defn restore-conflict!* [user-id conflict-id parent-id]
  #?(:clj (let [r (sync-client/restore-conflict! conflict-id parent-id)]
            (commands/bump-channels! user-id #{:desktop-mutations :refresh :tree-mutations :card-mutations})
            r)
     :cljs nil))

(defn discard-conflict!* [user-id conflict-id]
  #?(:clj (let [r (sync-client/discard-conflict! conflict-id)]
            (commands/bump-channels! user-id #{:desktop-mutations})
            r)
     :cljs nil))

(defn root-topics* [_refresh user-id]
  #?(:clj (jdbc/execute! db/ds ["SELECT id, title FROM topics
                                 WHERE parent_id IS NULL AND user_id = ? AND staged_delete_id IS NULL
                                 ORDER BY title" user-id]
            {:builder-fn rs/as-unqualified-lower-maps})
     :cljs []))

(defn license-summary* [_refresh user-id]
  #?(:clj (license/access db/ds user-id desktop-version/desktop-major) :cljs nil))

(defn price-label* [_refresh]
  #?(:clj (str "$" license/price-usd
            (when-let [iqd (license/price-iqd)] (str " (" iqd " credits)")))
     :cljs nil))

(defn devices* [_refresh user-id]
  #?(:clj (mapv #(-> % (update :created_at str) (update :last_used_at str) (update :revoked_at str))
            (desktop-auth/list-tokens user-id))
     :cljs []))

(defn revoke-device!* [user-id token-id]
  #?(:clj (let [r (desktop-auth/revoke-token! user-id token-id)]
            (commands/bump-channels! user-id #{:desktop-mutations})
            r)
     :cljs nil))

(defn buy-with-credits!* [user-id]
  #?(:clj (let [r (license/buy-with-credits! user-id)]
            (when (:ok r) (commands/bump-channels! user-id #{:desktop-mutations :credits-refresh}))
            r)
     :cljs nil))

(defn start-license-checkout!* [user-id base-url country]
  #?(:clj (license/start-checkout! user-id base-url country) :cljs nil))

;; ── Text ───────────────────────────────────────────────────────────

(defn short-time
  "A timestamp string cut to the minute, or \"never\"."
  [t]
  (let [t (str t)] (if (= "" t) "never" (subs t 0 (min 16 (count t))))))

(defn license-text
  "13.7 — one line per license state."
  [{:strs [state trial-days-left trial-ends grace-ends covered-majors]}]
  (case state
    "licensed" (str "Licensed for desktop versions "
                 (clojure.string/join ", " (map (fn [[a b]] (str a " and " b)) covered-majors)) ".")
    "trial" (str "Trial: " trial-days-left " days left (ends " (subs (str trial-ends) 0 10) ").")
    "grace" (str "Your license does not cover this version. Downloads only, until "
              (subs (str grace-ends) 0 10) ".")
    "expired" "Your license does not cover this version. Sync is off. Local use continues."
    "unlicensed" "The trial has ended. Sync is off. Local use continues."
    "Not checked yet."))

(defn sync-text
  "The sync line. `status` comes from `sync-client/status`."
  [{:keys [state error last-sync-at]}]
  (case state
    :syncing "Syncing now."
    :offline "Offline. FreeMemo syncs when the connection returns."
    :update-required "This version can no longer sync. Download the new version from freememo.net."
    :license-refused "Sync is off for this account."
    :full-sync-choice "This device and freememo.net need a full sync. Choose which copy to keep."
    :signed-out "This device was signed out. Quit FreeMemo and open it again to sign in."
    :error (str "The last sync failed: " error)
    (if last-sync-at
      (str "Synced at " (subs (str last-sync-at) 0 16) ".")
      "Not synced yet.")))

;; ── Desktop body ───────────────────────────────────────────────────

(e/defn SyncNowButton [label Run]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn btn-secondary" :style {:margin-right "8px"}})
      (dom/text label)
      (let [[t _] (e/Token (dom/On "click" identity nil))]
        (when t
          (case (Run) (t)))))))

(e/defn ConflictRow [user-id conflict parent-id]
  (e/client
    (dom/tr
      (dom/td (dom/text (:label conflict)))
      (dom/td (dom/text (:reason conflict)))
      (dom/td
        (dom/button
          (dom/props {:type "button" :class "btn btn-secondary"})
          (dom/text "Restore")
          (let [[t ?error] (e/Token (dom/On "click" identity nil))]
            (when t
              (let [r (e/server (e/Offload #(restore-conflict!* user-id (:id conflict) parent-id)))]
                (case r (if (:ok r) (t) (t (:error r))))))))
        (dom/button
          (dom/props {:type "button" :class "btn btn-secondary" :style {:margin-left "6px"}})
          (dom/text "Discard")
          (let [[t _] (e/Token (dom/On "click" identity nil))]
            (when t
              (case (e/server (e/Offload #(discard-conflict!* user-id (:id conflict)))) (t)))))))))

(e/defn DesktopSyncBody [user-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :desktop-mutations)))
          status (e/server (sync-status* refresh))
          conflicts (e/server (conflicts* refresh))
          roots (e/server (root-topics* refresh user-id))
          !parent (atom nil)
          parent (e/watch !parent)]
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "Sync")
        (dom/p (dom/props {:class "hint"}) (dom/text (sync-text status)))
        (dom/p (dom/props {:class "hint"}) (dom/text (license-text (:license status))))
        (dom/div
          (SyncNowButton "Sync now" (e/fn [] (e/server (e/Offload #(sync-now!*)))))
          (when (= :full-sync-choice (:state status))
            (dom/div
              (dom/props {:style {:display "contents"}})
              (SyncNowButton "Keep freememo.net's copy"
                (e/fn [] (e/server (e/Offload #(choose-full-sync!* "download")))))
              (SyncNowButton "Keep this device's copy"
                (e/fn [] (e/server (e/Offload #(choose-full-sync!* "upload")))))))))
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "Sync conflicts")
        (dom/p (dom/props {:class "hint"})
          (dom/text (if (zero? (count conflicts))
                      "Nothing was left out of a sync."
                      "These items could not sync, usually because their document was deleted on another device. Restore puts an item back under the document you choose.")))
        (dom/label
          (dom/props {:class "hint"})
          (dom/text "Restore into ")
          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value ""}) (dom/text "Top level"))
            (e/for [{:keys [id title]} (e/diff-by :id roots)]
              (dom/option (dom/props {:value (str id)}) (dom/text title)))
            (let [picked (dom/On "change" #(-> % .-target .-value) nil)]
              (when (some? picked)
                (reset! !parent (parse-long picked))))))
        (dom/table
          (dom/props {:style {:width "100%" :font-size "13px"}})
          (dom/tbody
            (e/for [c (e/diff-by :id conflicts)]
              (ConflictRow user-id c parent))))))))

;; ── Server body ────────────────────────────────────────────────────

(e/defn LicenseBody [user-id base-url client-country]
  (e/client
    (let [refresh (+ (e/server (e/watch (us/get-atom user-id :desktop-mutations)))
                    (e/server (e/watch (us/get-atom user-id :credits-refresh))))
          summary (e/server (license-summary* refresh user-id))
          price (e/server (price-label* refresh))
          devices (e/server (devices* refresh user-id))
          !error (atom nil)
          error (e/watch !error)]
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "License")
        (dom/p (dom/props {:class "hint"})
          (dom/text (license-text (into {} (map (fn [[k v]] [(name k) (if (keyword? v) (name v) v)])) summary))))
        (dom/p (dom/props {:class "hint"})
          (dom/text (str "A license costs " price ". It covers the desktop version it is bought for and the next one, on all your computers. Purchases are final.")))
        (dom/button
          (dom/props {:type "button" :class "btn btn-secondary" :style {:margin-right "8px"}})
          (dom/text "Buy with credits")
          (let [[t _] (e/Token (dom/On "click" identity nil))]
            (when t
              (let [r (e/server (e/Offload #(buy-with-credits!* user-id)))]
                (case r (if (:ok r) (t) (do (reset! !error (:error r)) (t (:error r)))))))))
        (dom/button
          (dom/props {:type "button" :class "btn btn-secondary"})
          (dom/text "Buy by card")
          (let [[t _] (e/Token (dom/On "click" identity nil))]
            (when t
              (let [r (e/server (e/Offload #(start-license-checkout!* user-id base-url client-country)))]
                (case r (if (:ok r)
                          (do (navigate-external! (:url r)) (t))
                          (do (reset! !error (:error r)) (t (:error r)))))))))
        (when error
          (dom/p (dom/props {:style {:color "var(--color-danger-text)"}}) (dom/text error))))
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "Signed-in computers")
        (dom/table
          (dom/props {:style {:width "100%" :font-size "13px"}})
          (dom/tbody
            (e/for [d (e/diff-by :id devices)]
              (dom/tr
                (dom/td (dom/text (or (:device_name d) "Desktop")))
                (dom/td (dom/text (str "Last used " (short-time (:last_used_at d)))))
                (dom/td
                  (if (not= "" (str (:revoked_at d)))
                    (dom/text "Signed out")
                    (dom/button
                      (dom/props {:type "button" :class "btn btn-danger"})
                      (dom/text "Sign out")
                      (let [[t _] (e/Token (dom/On "click" identity nil))]
                        (when t
                          (case (e/server (e/Offload #(revoke-device!* user-id (:id d)))) (t)))))))))))))))

(e/defn DesktopAppSection [user-id base-url client-country]
  (e/client
    (if (e/server (desktop?* (e/watch (us/get-atom user-id :desktop-mutations))))
      (DesktopSyncBody user-id)
      (LicenseBody user-id base-url client-country))))
