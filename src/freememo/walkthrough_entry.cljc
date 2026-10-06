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
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.typeahead :as typeahead]
   [freememo.navigation :as nav]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.walkthrough :as wt])))

(defn history* [user-id repo-id]
  #?(:clj (wt/history user-id repo-id) :cljs nil))

(defn start!* [user-id repo-id from-sha to-sha]
  #?(:clj (wt/start! user-id repo-id from-sha to-sha) :cljs nil))

(defn walkthroughs* [_rev user-id repo-id]
  #?(:clj (wt/list-for-repo user-id repo-id) :cljs nil))

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

(defn commit-options
  "Picklist options for `commits`, newest first. A label holds the short sha
   and the subject, so typing either finds a commit."
  [commits]
  (mapv (fn [{:keys [sha subject date author]}]
          {:id sha :label (str (short-sha sha) " · " subject)
           :description (str (subs (str date) 0 (min 10 (count (str date)))) " · " author)})
    commits))

(defn- index-of [options id]
  (first (keep-indexed (fn [i o] (when (= id (:id o)) i)) options)))

(defn first-options
  "The commits that may be the first one taught: `last-id` or older. The list
   is newest first, so that is `last-id`'s position and after."
  [options last-id]
  (if-let [i (index-of options last-id)] (subvec options i) options))

(defn last-options
  "The commits that may be the last one taught: `first-id` or newer."
  [options first-id]
  (if-let [i (index-of options first-id)] (subvec options 0 (inc i)) options))

(defn taught-count
  "How many commits the walkthrough from `first-id` to `last-id` teaches,
   both included, or nil when either is not chosen. The options are the
   branch's first-parent line, so the positions count the range exactly."
  [options first-id last-id]
  (let [i (index-of options first-id) j (index-of options last-id)]
    (when (and i j (<= j i)) (inc (- i j)))))

(def max-commits
  "The longest range a walkthrough takes (walkthrough-range/max-commits)."
  50)

(e/defn PickerBody
  "Two picklists and Start (§6.6.1). Each list offers only what is legal with
   the other's pick. Post: on a started walkthrough, `on-started` gets its
   id; a refused range shows why."
  [user-id repo-id commits on-started]
  (e/client
    (let [options (commit-options commits)
          !first (atom nil)
          !last (atom (:id (first options)))
          first-id (e/watch !first)
          last-id (e/watch !last)
          firsts (first-options options last-id)
          lasts (last-options options first-id)
          n (taught-count options first-id last-id)
          !error (atom nil)
          error (e/watch !error)]
      (dom/div
        (dom/props {:class "wt-picker__field"})
        (dom/span (dom/text "First commit to learn"))
        (typeahead/Typeahead !first firsts "Search by sha or message" nil true))
      (dom/div
        (dom/props {:class "wt-picker__field"})
        (dom/span (dom/text "Last commit to learn"))
        (typeahead/Typeahead !last lasts "Search by sha or message" nil nil))
      (dom/p
        (dom/props {:class "wt-panel__hint" :role "status"})
        (dom/text (cond
                    (nil? n) "Choose the first and the last commit. Both are taught."
                    (> n max-commits) (str "That is " n " commits. A walkthrough takes at most "
                                        max-commits ". Choose a later first commit.")
                    :else (str "This walkthrough teaches " n " commit" (when (not= 1 n) "s") "."))))
      (dom/p
        (dom/props {:class "wt-picker__error" :role "alert"})
        (dom/props {:style {:display (if error "block" "none")}})
        (dom/text (or error "")))
      (e/for [[t _] (forms/Button! [::start] :class "btn btn-primary wt-picker__start"
                      :label "Start walkthrough"
                      :disabled (not (and n (<= n max-commits))))]
        (let [from-sha (e/snapshot first-id)
              to-sha (e/snapshot last-id)
              r (e/server (e/Offload #(start!* user-id repo-id from-sha to-sha)))]
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
          (dom/text "Choose the first and the last commit to learn, on one branch. FreeMemo groups their changes by concept, at most 50 commits."))
        ;; The history read fetches the clone first, which takes seconds on a
        ;; large repository, so the modal says so until it lands.
        (loading/WithLoading
          (e/fn [] (e/server (e/Offload #(history* user-id repo-id))))
          (e/fn [h]
            (if (:error h)
              (dom/p (dom/props {:class "wt-picker__error" :role "alert"})
                (dom/text (:error h)))
              (PickerBody user-id repo-id (:commits h)
                (fn [wid]
                  (reset! !open? false)
                  (navigate! :viewer (nav/nav-walkthrough repo-id wid))))))
          (e/fn [] (loading/Spinner "Reading the repository's history…")))
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
                (dom/text (str (short-sha (:sha_from w)) " → " (short-sha (:sha_b w)) " · "
                            (:commit_count w) " commits · "
                            (case (:status w) "ready" (str "step " (:step w) " of " (:concepts w))
                                             "preparing" "preparing"
                                             (str "failed"))))
                (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-walkthrough repo-id (:id w)))) nil))))))
      (dom/div
        (dom/props {:class "wt-picker-host"})
        (when open? (Picker user-id repo-id !open? navigate!))))))
