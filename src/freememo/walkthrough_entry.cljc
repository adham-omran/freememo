(ns freememo.walkthrough-entry
  "\"Walk through commits…\" on a repository node
   (plans/commit-range-walkthrough.md §6.6.1): the button, the picker that
   takes commits A and B from the clone's history, and the list of this
   repository's walkthroughs.

   Its own namespace for the 64KB method cap: `repo-update-bar` mounts it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.forms-ext :as forms-ext]
   [freememo.modal-shell :as modal]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.walkthrough :as wt])))

(defn history* [user-id repo-id]
  #?(:clj (wt/history user-id repo-id) :cljs nil))

(defn start!* [user-id repo-id sha-a sha-b]
  #?(:clj (wt/start! user-id repo-id sha-a sha-b) :cljs nil))

(defn walkthroughs* [_rev user-id repo-id]
  #?(:clj (wt/list-for-repo user-id repo-id) :cljs nil))

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

(def ^:private default-span
  "How far back A starts, in commits, when the picker opens."
  10)

(defn commit-options
  "Picker options for `commits`, newest first: [[sha label] …]."
  [commits]
  (mapv (fn [{:keys [sha subject date]}]
          [sha (str (short-sha sha) " · " subject (when date (str " · " (subs date 0 (min 10 (count date))))))])
    commits))

(e/defn PickerBody
  "Two commits and Start. Post: on a started walkthrough, `on-started` gets
   its id; a refused range shows why."
  [user-id repo-id commits on-started]
  (e/client
    (let [options (commit-options commits)
          !b (atom (first (first options)))
          !a (atom (first (nth options (min default-span (dec (count options))) nil)))
          a (e/watch !a)
          b (e/watch !b)
          !error (atom nil)
          error (e/watch !error)]
      (dom/label
        (dom/props {:class "wt-picker__field"})
        (dom/span (dom/text "From (A, excluded)"))
        (e/for [[t {:keys [sha]}] (forms-ext/Select! :sha a options :class "input wt-picker__a"
                                    :aria-label "First commit")]
          (case (reset! !a sha) (t))))
      (dom/label
        (dom/props {:class "wt-picker__field"})
        (dom/span (dom/text "To (B)"))
        (e/for [[t {:keys [sha]}] (forms-ext/Select! :sha b options :class "input wt-picker__b"
                                    :aria-label "Last commit")]
          (case (reset! !b sha) (t))))
      (dom/p
        (dom/props {:class "wt-picker__error" :role "alert"})
        (dom/props {:style {:display (if error "block" "none")}})
        (dom/text (or error "")))
      (e/for [[t _] (forms/Button! [::start] :class "btn btn-primary wt-picker__start"
                      :label "Start walkthrough")]
        (let [sha-a (e/snapshot a)
              sha-b (e/snapshot b)
              r (e/server (e/Offload #(start!* user-id repo-id sha-a sha-b)))]
          (case r
            (if (:ok r)
              (case (on-started (:walkthrough-id r)) (t))
              (case (reset! !error (:error r)) (t (:error r))))))))))

(e/defn Picker
  "The modal that takes A and B from the clone's history (§6.6.1)."
  [user-id repo-id !open? navigate!]
  (e/client
    (dom/div
      (dom/props {:class "modal-backdrop" :tabindex "-1"})
      (modal/ModalEscape (fn [] (reset! !open? false)) "Walk through commits")
      (dom/On "click" (fn [_] (reset! !open? false)) nil)
      (dom/div
        (dom/props {:class "modal-content wt-picker" :data-testid "walkthrough-picker"})
        (dom/On "click" (fn [e] (.stopPropagation e)) nil)
        (dom/h3 (dom/text "Walk through commits"))
        (dom/p (dom/props {:class "wt-panel__hint"})
          (dom/text "Choose two commits on one branch. FreeMemo groups the changes between them by concept, at most 50 commits."))
        (let [h (e/server (e/Offload #(history* user-id repo-id)))]
          (cond
            (e/server (:error h))
            (dom/p (dom/props {:class "wt-picker__error" :role "alert"})
              (dom/text (e/server (:error h))))
            (e/server (:ok h))
            (PickerBody user-id repo-id (e/server (:commits h))
              (fn [wid]
                (reset! !open? false)
                (navigate! :viewer (nav/nav-walkthrough repo-id wid))))
            :else (dom/p (dom/props {:role "status"}) (dom/text "Reading the history…"))))
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
          (dom/text "Close")
          (dom/On "click" (fn [_] (reset! !open? false)) nil))))))

(e/defn WalkthroughEntry
  "The repository node's walkthrough control and its walkthroughs."
  [user-id repo-id navigate!]
  (e/client
    (let [!open? (atom false)
          open? (e/watch !open?)
          rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          rows (e/server (e/Offload #(walkthroughs* rev user-id repo-id)))]
      (bus/publish-invoker! :start-walkthrough #(reset! !open? true))
      (e/on-unmount #(bus/retract-invoker! :start-walkthrough))
      (dom/div
        (dom/props {:class "wt-entry"})
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-entry__start"})
          (dom/text "Walk through commits…")
          (dom/On "click" (fn [_] (reset! !open? true)) nil))
        (dom/ul
          (dom/props {:class "wt-entry__list"})
          (e/for [w (e/server (e/diff-by :id rows))]
            (dom/li
              (dom/button
                (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-entry__open"})
                (dom/text (str (short-sha (:sha_a w)) " → " (short-sha (:sha_b w)) " · "
                            (:commit_count w) " commits · "
                            (case (:status w) "ready" (str "step " (:step w) " of " (:concepts w))
                                             "preparing" "preparing"
                                             (str "failed"))))
                (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-walkthrough repo-id (:id w)))) nil))))))
      (dom/div
        (dom/props {:class "wt-picker-host"})
        (when open? (Picker user-id repo-id !open? navigate!))))))
