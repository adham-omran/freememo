(ns freememo.repo-home
  "The repository node's home (plans/repository-home.md §4.2): its source and
   Update, its walkthroughs, and its imported snapshots. It stands in for the
   notes editor, which a repository node does not have.

   Its own namespace for the 64KB method cap."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.navigation :as nav]
   [freememo.repo-update-bar :refer [RepoUpdateBar]]
   [freememo.tape-table :as tape]
   [freememo.walkthrough-entry :refer [WalkthroughStart]]
   [freememo.walkthrough-table :refer [WalkthroughTable]]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.repo-commits :as commits])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.util :as util])))

(defn snapshots* [_rev user-id repo-id]
  #?(:clj (mapv (fn [r] {:id (:id r) :title (:title r) :sha (:commit_sha r)
                         :imported (util/format-timestamp (:created_at r))
                         :files (:files r)})
            (db/repo-snapshots user-id repo-id))
     :cljs nil))

(defn snapshot-subjects* [_rev user-id repo-id]
  #?(:clj (commits/snapshot-subjects user-id repo-id) :cljs nil))

(def ^:private headers ["Commit" "Subject" "Imported" "Files"])
(def ^:private grid-cols "minmax(0, 1.2fr) minmax(0, 3fr) 140px 80px")

(e/defn SnapshotRow
  "One imported commit snapshot; a click opens its node."
  [s subject i navigate!]
  (e/client
    (dom/tr
      (dom/props {:class "repo-snapshot" :style {:--order i} :data-topic-id (str (:id s))})
      (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-topic (:id s) nil))) nil)
      (dom/td (dom/props {:class "tape-cell-strong tape-cell-click" :dir "ltr"})
        (dom/span (dom/props {:class "tape-cell-text"}) (dom/text (:title s))))
      (dom/td (dom/props {:class "tape-cell-click" :dir "auto"})
        (dom/span (dom/props {:class "tape-cell-text repo-snapshot__subject"})
          (dom/text (or subject ""))))
      (dom/td (dom/props {:class "tape-cell-meta tape-cell-click"})
        (dom/text (or (:imported s) "")))
      (dom/td (dom/props {:class "tape-cell-num tape-cell-click"})
        (dom/text (str (:files s)))))))

(e/defn SnapshotTable
  "The commits imported under this repository, newest first (§4.2.3). The
   rows come from the database; the subjects come from the clone, and a row
   shows its subject once that read lands."
  [user-id repo-id navigate!]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          rows (e/server (e/Offload #(snapshots* rev user-id repo-id)))
          subjects (e/server (e/Offload #(snapshot-subjects* rev user-id repo-id)))
          n (e/server (some-> rows count))
          subjects-error (e/server (:error subjects))]
      (dom/div
        (dom/props {:class "repo-home__table" :data-testid "snapshot-table"})
        (tape/TapeTable headers grid-cols (or n 0) nil
          (if (nil? n) "" "No commit is imported yet.")
          (e/fn [i]
            (when-let [s (e/server (nth rows i nil))]
              (SnapshotRow s (e/server (get (:ok subjects) (:sha s))) i navigate!)))))
      (dom/p
        (dom/props {:class "wt-picker__error" :role "alert"})
        (dom/props {:style {:display (if subjects-error "block" "none")}})
        (dom/text (str "The commit messages could not be read. " subjects-error))))))

(e/defn RepoHome
  "The home of the repository node the viewer shows."
  []
  (e/client
    (let [user-id dctx/user-id
          repo-id dctx/page-topic-id
          navigate! dctx/navigate!]
      (dom/div
        (dom/props {:class "repo-home" :data-testid "repo-home"})
        (RepoUpdateBar)
        (dom/section
          (dom/props {:class "repo-home__section"})
          (dom/div
            (dom/props {:class "repo-home__head"})
            (dom/h3 (dom/text "Walkthroughs"))
            (WalkthroughStart user-id repo-id navigate!))
          (WalkthroughTable user-id repo-id navigate!))
        (dom/section
          (dom/props {:class "repo-home__section"})
          (dom/div
            (dom/props {:class "repo-home__head"})
            (dom/h3 (dom/text "Imported commits")))
          (SnapshotTable user-id repo-id navigate!))))))
