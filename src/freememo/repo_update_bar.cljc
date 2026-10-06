(ns freememo.repo-update-bar
  "The Update control of a repository node (plans/git-forge-integration.md §4.6).

   It exists on the `repo` node and nowhere else (§4.6 6.1): an update adds a
   commit under the repository, so the repository is its only subject. An update
   resolves the stored ref again, downloads, applies the stored selection, and
   adds a commit node beside the old ones (§4.6 6.2). A sha that is already a
   child adds nothing and says so (§4.6 6.3).

   The user starts every update. No schedule exists.

   It is the header of the repository home (`repo-home`)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.doc-context :as dctx]
   [freememo.forge-kinds :as kinds]
   [freememo.navigation :as nav]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.repo-import :as repo-import])
   #?(:clj [freememo.user-state :as us])))

(defn repo-summary*
  "Post: {:repo-url :forge :ref :commits} for `user-id`'s repository node, or nil."
  [_refresh user-id repo-id]
  #?(:clj (when-let [r (db/get-repo-summary user-id repo-id)]
            {:repo-url (:repo_url r) :forge (:forge r) :ref (:ref r) :commits (:commits r)})
     :cljs nil))

(defn update-repository!*
  "Post: `repo-import/update-repository!`'s result."
  [user-id repo-id]
  #?(:clj (repo-import/update-repository! user-id repo-id)
     :cljs nil))

(defn repository-phase*
  "Post: the phase of the running import or update of `repo-url`, or nil."
  [phases repo-url]
  #?(:clj (repo-import/phase-in phases repo-url)
     :cljs nil))

(defn result-message
  "Post: one line for the user about an `update-repository!` result."
  [{:keys [ok cancelled? unchanged? sha error report]}]
  (let [short-sha (when sha (subs sha 0 (min 7 (count sha))))]
    (cond
      cancelled? "Update cancelled."
      (not ok) error
      unchanged? (str "Already at " short-sha ". Nothing changed.")
      :else (str "Added commit " short-sha " with " (:topics-created report) " topics."))))

(e/defn RepoUpdateBar
  "Pre : dctx/page-topic-id names a `repo` topic.
   Post: the repository's source and an Update button. After an update, one line
         says what happened, and a control opens the commit it names."
  []
  (e/client
    (let [user-id dctx/user-id
          repo-id dctx/page-topic-id
          navigate! dctx/navigate!
          tree-mutations (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          summary (e/server (e/Offload #(repo-summary* tree-mutations user-id repo-id)))
          phase (e/server (repository-phase* (e/watch (us/get-atom user-id :importing-repos))
                            (:repo-url summary)))
          !result (atom nil)
          result (e/watch !result)]
      (dom/div
        (dom/props {:class "repo-update-bar"})
        (dom/span
          (dom/props {:class "repo-update-bar__source" :dir "ltr"})
          (dom/text (if summary
                      (str (kinds/forge-label (:forge summary)) " · " (:repo-url summary)
                        " · follows " (:ref summary) " · " (:commits summary)
                        (if (= 1 (:commits summary)) " commit" " commits"))
                      "")))
        (e/for [[t _] (forms/Button! [::update] :class "btn btn-sm btn-primary" :label "Update")]
          ;; The thunk closes over the two ids only. The update bumps
          ;; :tree-mutations, which re-reads `summary`, and nothing here reads
          ;; that back — so the write cannot restart its own Offload.
          (let [r (e/server (e/Offload #(update-repository!* user-id repo-id)))]
            (case (reset! !result r)
              (if (:ok r) (t) (t (:error r))))))
        (when phase
          (dom/span
            (dom/props {:class "repo-update-bar__phase" :role "status"})
            (dom/text (str phase "…"))))
        (when result
          (dom/span
            (dom/props {:class (if (or (:ok result) (:cancelled? result))
                                 "repo-update-bar__result"
                                 "repo-update-bar__result repo-update-bar__result--error")
                        :role "status"})
            (dom/text (result-message result))))
        (when (:ok result)
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
            (dom/text "Open commit")
            (dom/On "click"
              (fn [_] (navigate! :viewer (nav/nav-topic (:topic-id result) nil)))
              nil)))))))
