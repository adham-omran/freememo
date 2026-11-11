(ns freememo.failure-banners
  "The banner strip: the one fixed place where state the user must know about
   stays on screen until they deal with it (plans/remove-toasts.md 1.2, 1.3).

   Two kinds of banner, one strip. Account-state banners come first (A-10):
   the pause banner while video playback is blocked, and the removal notice
   after the sweep removed video files. Failure banners follow, one for each
   unacknowledged `activities` failure, newest first.

   Everything is DERIVED from the server, never pushed. The strip re-queries on
   `:activity-mutations` (failures) and `:credits-refresh` (account state), so a
   failure survives a reload, shows in a second tab, and leaves every tab when
   one tab acknowledges it.

   Failures that share a non-null `error_kind` collapse into one banner that
   names their count (A-11): twelve runs that all hit an empty balance need one
   Top up, not twelve.

   The pure builders (`collapse-failures`, `account-banners`) are plain fns so
   a test can reach them without a browser, and so they compile to methods of
   their own."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.navigation :as nav]
   #?(:clj [freememo.activity :as activity])
   #?(:clj [freememo.storage-meter :as meter])
   #?(:clj [freememo.update-feed :as update-feed])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Pure builders. A banner is plain data:
;;   :key         identity for the keyed e/for, stable while the banner lives
;;   :tone        :danger | :warning
;;   :message     the sentence
;;   :ids         activity ids that Dismiss, Open and Retry acknowledge
;;   :subject-id  topic for Open, or nil
;;   :retry?      the Retry button shows
;;   :topup?      the Top up button shows
;;   :dismiss     nil (cannot be dismissed) | :failures | :removal
;;   :href        a URL for a link button (the update banners: the installer), or nil
;;   :href-label  the text of that link; both keys are set together
;; ---------------------------------------------------------------------------

(defn failure-lead
  "What the failure was doing, in a few words. A one-shot `action-failure` row
   stores the action's name in `:what`. Every other row is a run, named by its
   type's label."
  [{:keys [type label what]}]
  (or (not-empty (if (= "action-failure" type) what label))
      "Action"))

(defn failure-message [{:keys [error] :as failure}]
  (let [lead (failure-lead failure)]
    (if (str/blank? error)
      (str lead " failed.")
      (str lead " failed: " error))))

(defn single-failure-banner
  [{:keys [id subject-id retry? error-kind] :as failure}]
  {:key (str "failure-" id)
   :tone :danger
   :message (failure-message failure)
   :ids [id]
   :subject-id subject-id
   :retry? (boolean retry?)
   :topup? (= "insufficient-credits" error-kind)
   :dismiss :failures})

(defn collapsed-failure-banner
  "One banner for several failures with the same `error_kind`. It offers no Open
   and no Retry, because it stands for many subjects. Dismiss acknowledges all."
  [kind failures]
  (let [n (count failures)]
    {:key (str "kind-" kind)
     :tone :danger
     :message (case kind
                "insufficient-credits" (str n " actions need credits.")
                (str n " actions failed."))
     :ids (mapv :id failures)
     :subject-id nil
     :retry? false
     :topup? (= "insufficient-credits" kind)
     :dismiss :failures}))

(defn collapse-failures
  "Banners for `failures` (newest first). Failures that share a non-null
   `:error-kind` become one banner at the position of the newest of them, when
   there are two or more. Every other failure keeps its own banner.

   Post: every failure id appears in exactly one banner's `:ids`."
  [failures]
  (let [by-kind (group-by :error-kind (filter :error-kind failures))
        many? (fn [kind] (and (some? kind) (> (count (get by-kind kind)) 1)))]
    (loop [remaining (seq failures), done-kinds #{}, out []]
      (if-some [[failure & more] remaining]
        (let [kind (:error-kind failure)]
          (cond
            (not (many? kind))
            (recur more done-kinds (conj out (single-failure-banner failure)))

            (contains? done-kinds kind)
            (recur more done-kinds out)

            :else
            (recur more (conj done-kinds kind)
              (conj out (collapsed-failure-banner kind (get by-kind kind))))))
        out))))

(defn account-banners
  "The account-state banners for `state`, which is
   `storage-meter/account-banner-state`'s answer, or nil."
  [{:keys [paused removal]}]
  (cond-> []
    paused
    (conj {:key "pause"
           :tone :warning
           :message (str "Video playback is paused. Your videos are kept until "
                      (:until paused) ".")
           :ids []
           :subject-id nil
           :retry? false
           :topup? true
           :dismiss nil})

    removal
    (conj {:key "removal"
           :tone :warning
           :message (str (:count removal)
                      (if (= 1 (:count removal))
                        " video was removed on "
                        " videos were removed on ")
                      (:on removal) ".")
           :ids []
           :subject-id nil
           :retry? false
           :topup? false
           :dismiss :removal})))

(defn update-banners
  "The Local Install's update banners (plans/local-install.md, New: updates).
   `state` is `update-feed/banner-state`'s answer.
   - A refused Sync Protocol shows \"Update to keep syncing\", with the installer
     link when the feed has one.
   - A Release that needs a newer Runtime (`:installer`, ticket 20) says that
     edits are not yet synced, with no link, until nothing is pending. Then it
     shows the link \"Download the new installer\". When the sync state is
     `:update-required`, the sync cannot finish, so the link shows at once with
     the count of edits that stay on this computer.
   A newer Release with the same Runtime shows none: an App Update installs
   itself, and the tray offers \"Restart to update\"."
  [{:keys [update-available update-required? installer]}]
  (let [link (when-let [url (:url update-available)]
               {:href url :href-label "Download"})
        base {:tone :warning :ids [] :subject-id nil :retry? false :topup? false :dismiss nil}]
    (cond-> []
      update-required?
      (conj (merge base {:key "update-required" :message "Update to keep syncing"} link))

      (= :unsynced (:status installer))
      (conj (merge base {:key "installer-unsynced"
                         :message "A new installer is ready, but your edits are not yet synced. Sync them first."}))

      (= :ready (:status installer))
      (conj (merge base {:key "installer-ready"
                         :message (if-let [n (:pending installer)]
                                    (str "This version of FreeMemo can sync no more. It needs a new installer. "
                                      n (if (= 1 n) " edit is" " edits are")
                                      " not synced. They stay on this computer.")
                                    "This version of FreeMemo needs a new installer.")
                         :href (:url installer) :href-label "Download the new installer"})))))

;; ---------------------------------------------------------------------------
;; Server wrappers. Body-level #?(:clj … :cljs …), because reactive bodies name
;; them inside `(e/server …)` and the var must resolve on both peers.
;; ---------------------------------------------------------------------------

(defn banners*
  "Every banner for `user-id`. `_rev` is the pair of channel counters the strip
   watches; it exists to make this call re-run when either bumps."
  [_rev user-id]
  #?(:clj (-> (account-banners (meter/account-banner-state user-id))
            (into (update-banners (update-feed/banner-state)))
            (into (collapse-failures (activity/unseen-failures user-id))))
     :cljs []))

(defn dismiss!*
  "Dismiss a banner: acknowledge its failures, or mark the removal notice read.
   Returns :done."
  [user-id kind ids]
  #?(:clj (if (= :removal kind)
            (meter/dismiss-removal! user-id)
            (activity/acknowledge! user-id ids))
     :cljs nil))

(defn retry!*
  "Retry the failed action behind `activity-id`. The client sends only the id.
   Returns :done.

   The retry runs in a `future`, and this thread only waits for it. `retry!`
   marks the row seen first, so any re-query of the strip during the command
   unmounts the Retry button and interrupts its `e/Offload` thread. The
   interrupt then stops the wait. It never reaches the command, so a failed
   retry still records its banner."
  [user-id activity-id]
  #?(:clj (let [run (future (activity/retry! user-id activity-id))]
            (deref run)
            :done)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn OpenAction
  "Acknowledge, then open the subject. The navigation is registered with
   `e/on-unmount` inside `(when t …)`, so it runs when the token spends. A
   navigation in the click handler changed `navigate!` (it closes over the
   active tab) under the pending token, and the acknowledgement never ran."
  [ids subject-id user-id navigate!]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
      (dom/text "Open")
      (let [click (dom/On "click" identity nil)
            [t _] (e/Token click)]
        (when t
          (e/on-unmount #(navigate! :viewer (nav/nav-topic subject-id :activity)))
          (case (e/server (dismiss!* user-id :failures ids)) (t)))))))

(e/defn RetryAction
  "Retry sends the activity id and nothing else. The server reads the stored
   command from the row. The call runs in `e/Offload`: the command may take
   seconds, and a blocked server graph would freeze the session."
  [id user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :type "button"})
      (dom/text "Retry")
      (let [click (dom/On "click" identity nil)
            [t _] (e/Token click)]
        (when t
          (case (e/server (e/Offload #(retry!* user-id id))) (t)))))))

(e/defn DismissAction [kind ids user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
      (dom/text "Dismiss")
      (let [click (dom/On "click" identity nil)
            [t _] (e/Token click)]
        (when t
          (case (e/server (dismiss!* user-id kind ids)) (t)))))))

(e/defn Banner
  [{:keys [tone message ids subject-id retry? topup? dismiss href href-label]} user-id navigate!]
  (e/client
    (dom/div
      (dom/props {:class (str "banner banner--" (name tone))
                  :role (if (= :danger tone) "alert" "status")})
      (dom/span
        (dom/props {:class "banner__icon"})
        (icons/Icon (if (= :danger tone) :circle-alert :triangle-alert) :size 16))
      (dom/div
        (dom/props {:class "banner__message"})
        (dom/text message))
      (dom/div
        (dom/props {:class "banner__actions"})
        (when (some? subject-id)
          (OpenAction ids subject-id user-id navigate!))
        (when retry?
          (RetryAction (first ids) user-id))
        (when (some? href)
          (dom/a
            (dom/props {:class "btn btn-sm btn-primary" :href href :target "_blank"
                        :rel "noopener noreferrer"})
            (dom/text href-label)))
        (when topup?
          (dom/button
            (dom/props {:class "btn btn-sm btn-primary" :type "button"})
            (dom/text "Top up")
            (dom/On "click" (fn [_] (navigate! :settings)) nil)))
        (when (some? dismiss)
          (DismissAction dismiss ids user-id))))))

(e/defn FailureBanners
  "The strip. Mount once in `Main`, in the page column above the page.

   The strip element is always mounted and the keyed `e/for` is its only child,
   so a banner appearing or leaving never changes the count of children the DOM
   layer places by index. It diffs the VIEW (`:key`), so a changed banner
   re-crosses whole but an unchanged one crosses nothing."
  [user-id navigate!]
  (e/client
    (dom/div
      (dom/props {:class "banner-strip" :aria-live "polite"})
      (let [activity-rev (e/server (e/watch (us/get-atom user-id :activity-mutations)))
            credits-rev (e/server (e/watch (us/get-atom user-id :credits-refresh)))
            desktop-rev (e/server (e/watch (us/get-atom user-id :desktop-mutations)))]
        (e/for [b (e/server (e/diff-by :key (banners* [activity-rev credits-rev desktop-rev] user-id)))]
          (Banner b user-id navigate!))))))
