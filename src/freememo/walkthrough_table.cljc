(ns freememo.walkthrough-table
  "The repository home's walkthroughs (plans/repository-home.md §4.2.2): one
   row each, with its name, range, progress and last progress, and the
   rename dialog the row and the walkthrough screen share (§4.3.3).

   Its own namespace for the 64KB method cap."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.modal-shell :as modal]
   [freememo.navigation :as nav]
   [freememo.tape-table :as tape]
   [freememo.walkthrough-name :as wname]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.walkthrough :as wt])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn walkthroughs* [_rev user-id repo-id]
  #?(:clj (wt/list-for-repo user-id repo-id) :cljs nil))

(defn rename!* [user-id wid typed]
  #?(:clj (wt/rename! user-id wid typed) :cljs nil))

(defn delete!* [user-id wid]
  #?(:clj (wt/delete! user-id wid) :cljs nil))

;; ── Rename ─────────────────────────────────────────────────────────

(e/defn RenameDialog
  "Rename the walkthrough `@!renaming` names, {:id :name}; nil hides the
   dialog. A blank name brings back the name derived from the steps.
   Post: on a saved name, `!renaming` is nil; a refused one shows why."
  [user-id !renaming]
  (e/client
    (when-some [{:keys [id name]} (e/watch !renaming)]
      (let [!error (atom nil)
            error (e/watch !error)]
        (dom/div
          (dom/props {:class "modal-backdrop" :tabindex "-1"})
          (modal/ModalEscape (fn [] (reset! !renaming nil)) "Rename walkthrough")
          (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e))
                                    (reset! !renaming nil))) nil)
          (dom/div
            (dom/props {:class "modal-content modal-sm wt-rename" :data-testid "walkthrough-rename"})
            (dom/h3 (dom/text "Rename walkthrough"))
            (dom/p (dom/props {:class "wt-panel__hint"})
              (dom/text "Leave it empty to name the walkthrough after its first step."))
            (let [commits (forms/Form! {:name (or name "")}
                            (e/fn [{:keys [name]}]
                              (e/amb
                                (forms/Input! :name (or name "")
                                  :class "input input-full wt-rename__input"
                                  :maxlength (str wname/max-name-chars)
                                  :aria-label "Walkthrough name" :autofocus true)
                                (dom/div
                                  (dom/props {:class "wt-rename__actions"})
                                  (e/amb
                                    (forms/SubmitButton! :label "Save" :class "btn btn-primary")
                                    (dom/button
                                      (dom/props {:type "button" :class "btn btn-secondary"})
                                      (dom/text "Cancel")
                                      (dom/On "click" (fn [_] (reset! !renaming nil)) nil)
                                      (e/amb))))))
                            :Parse (e/fn [{:keys [name]} _] [`RenameWalkthrough id name])
                            :type :command
                            :show-buttons false)]
              (e/for [[t [_ wid typed]] (e/diff-by first (e/as-vec commits))]
                (let [r (e/server (e/Offload #(rename!* user-id wid typed)))]
                  (when (some? r)
                    (if (:ok r)
                      (do (e/on-unmount #(reset! !renaming nil)) (t))
                      (case (reset! !error (:error r)) (t (:error r))))))))
            (dom/p
              (dom/props {:class "wt-picker__error" :role "alert"})
              (dom/props {:style {:display (if error "block" "none")}})
              (dom/text (or error "")))))))))

;; ── Table ──────────────────────────────────────────────────────────

(def ^:private headers ["Name" "Range" "Understood" "Progressed" ""])
;; The name takes what the fixed columns leave; the range clips with an
;; ellipsis and a tooltip, so a narrow content column still shows the name.
(def ^:private grid-cols "minmax(0, 2fr) minmax(0, 1.4fr) 84px 96px 168px")

(e/defn WalkthroughRow
  "One walkthrough. A click opens it at its saved step (§4.2.2.3); Rename and
   Delete act on the row without opening it."
  [repo-id w i navigate! !renaming !deleting]
  (e/client
    (dom/tr
      (dom/props {:class "wt-row" :style {:--order i} :data-walkthrough-id (str (:id w))})
      (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-walkthrough repo-id (:id w)))) nil)
      (dom/td (dom/props {:class "tape-cell-strong tape-cell-click wt-row__name" :dir "auto"})
        (dom/span (dom/props {:class "tape-cell-text" :data-tooltip (:display-name w)})
          (dom/text (:display-name w))))
      (dom/td (dom/props {:class "tape-cell-muted tape-cell-click" :dir "ltr"})
        (dom/span (dom/props {:class "tape-cell-text" :data-tooltip (:range w)})
          (dom/text (:range w))))
      (dom/td (dom/props {:class (str "tape-cell-sm tape-cell-click wt-row__progress"
                                   (when (= "failed" (:status w)) " wt-row__progress--failed"))})
        (dom/text (:progress w)))
      (dom/td (dom/props {:class "tape-cell-meta tape-cell-click"})
        (dom/text (or (:progressed w) "")))
      (dom/td (dom/props {:class "tape-cell-actions"})
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-row__rename"})
          (dom/text "Rename")
          (dom/On "click" (fn [e] (.stopPropagation e)
                            (reset! !renaming {:id (:id w) :name (:name w)})) nil))
        ;; A preparation still writes into its walkthrough, so it has no
        ;; Delete until it ends (walkthrough/delete!).
        (when (not= "preparing" (:status w))
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-row__delete"})
            (dom/text "Delete")
            (dom/On "click" (fn [e] (.stopPropagation e)
                              (reset! !deleting {:id (:id w) :name (:display-name w)})) nil)))))))

(e/defn WalkthroughTable
  "This repository's walkthroughs, the most recently progressed first."
  [user-id repo-id navigate!]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          ;; SERVER-FORM binding: one window row crosses at a time.
          rows (e/server (e/Offload #(walkthroughs* rev user-id repo-id)))
          n (e/server (some-> rows count))
          !renaming (atom nil)
          !deleting (atom nil)
          deleting (e/watch !deleting)]
      (dom/div
        (dom/props {:class "repo-home__table" :data-testid "walkthrough-table"})
        (tape/TapeTable headers grid-cols (or n 0) nil
          (if (nil? n)
            ""
            "No walkthroughs yet. Walk through commits… teaches what changed between two commits, one concept at a time.")
          (e/fn [i]
            (when-let [w (e/server (nth rows i nil))]
              (WalkthroughRow repo-id w i navigate! !renaming !deleting)))))
      (RenameDialog user-id !renaming)
      (modal/ConfirmModal !deleting "Delete walkthrough?"
        (str "Delete \"" (:name deleting) "\"? Its steps, your progress and its step fixes "
          "are deleted. Its pins and known ideas stay on the repository's files.")
        "Delete" "btn btn-danger"
        (e/fn [{:keys [id]}] (e/server (e/Offload #(delete!* user-id id))))))))
