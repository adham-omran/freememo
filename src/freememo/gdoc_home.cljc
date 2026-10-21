(ns freememo.gdoc-home
  "The home of a Google Doc root (plans/google-docs-pull.md, rows 5.1 and 6.3):
   where the Doc comes from, Pull, and the snapshots, newest first.

   Pull exports the Doc again. When it changed, a snapshot is added and every
   earlier one is set to done (F2). When it did not, nothing is added and the
   result line says so. The user starts every Pull. No schedule exists.

   Its own namespace for the 64KB method cap."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.doc-context :as dctx]
   [freememo.navigation :as nav]
   [freememo.tape-table :as tape]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.google-accounts :as accounts])
   #?(:clj [freememo.google-docs :as gdocs])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.util :as util])))

(defn summary*
  "Post: {:email :link} for `user-id`'s Google Doc root `root-id`, or nil.
   `:email` is nil when the account the Doc was imported through is no longer
   connected."
  [_refresh user-id root-id]
  #?(:clj (when-let [r (db/gdoc-root user-id root-id)]
            {:email (accounts/account-email user-id (:google_account_sub r))
             :link (gdocs/doc-url (:drive_file_id r))})
     :cljs nil))

(defn snapshots* [_rev user-id root-id]
  #?(:clj (mapv (fn [r] {:id (:id r) :title (:title r)
                         :status (if (= "done" (:status r)) "Done" "Under review")
                         :pulled (util/format-timestamp (:created_at r))})
            (db/gdoc-snapshots user-id root-id))
     :cljs nil))

(defn pull!*
  "Post: `google-docs/pull!`'s result."
  [user-id root-id]
  #?(:clj (gdocs/pull! user-id root-id)
     :cljs nil))

(defn pulling?*
  "Post: true while a Pull of `root-id` runs."
  [pulling root-id]
  (contains? (or pulling #{}) root-id))

(defn result-message
  "Post: one line for the user about a `pull!` result."
  [{:keys [ok unchanged? error]}]
  (cond
    (not ok) error
    unchanged? "The Doc has not changed since the last Pull. No snapshot was added."
    :else "Added a snapshot. The earlier one is done."))

(e/defn PullBar
  "The Doc's source and Pull. Publishes the palette's :pull-google-doc invoker
   while mounted."
  [user-id root-id navigate!]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          summary (e/server (e/Offload #(summary* settings-refresh user-id root-id)))
          pulling? (e/server (pulling?* (e/watch (us/get-atom user-id :pulling-gdocs)) root-id))
          !request (atom nil)
          [t _] (e/Token (e/watch !request))
          !result (atom nil)
          result (e/watch !result)
          request! (fn [] (reset! !request (js/Date.now)))]
      (bus/publish-invoker! :pull-google-doc request!)
      (e/on-unmount #(bus/retract-invoker! :pull-google-doc))
      (dom/div
        (dom/props {:class "repo-update-bar"})
        (dom/span
          (dom/props {:class "repo-update-bar__source" :dir "ltr"})
          (dom/text (cond
                      (nil? summary) ""
                      (:email summary) (str "Google Doc, read through " (:email summary))
                      :else "Google Doc. Its Google account is not connected.")))
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-primary"})
          (dom/text "Pull")
          (dom/On "click" (fn [_] (request!)) nil))
        (when (:link summary)
          (dom/a
            (dom/props {:class "btn btn-sm btn-secondary" :href (:link summary)
                        :target "_blank" :rel "noopener noreferrer"})
            (dom/text "Open in Google Docs")))
        (when t
          ;; The thunk closes over the two ids only. The Pull bumps
          ;; :tree-mutations, which re-reads the snapshot table, and nothing
          ;; here reads that back — so the write cannot restart its own Offload.
          (let [r (e/server (e/Offload #(pull!* user-id root-id)))]
            (case (reset! !result r)
              (if (:ok r) (t) (t (:error r))))))
        (when pulling?
          (dom/span
            (dom/props {:class "repo-update-bar__phase" :role "status"})
            (dom/text "Pulling…")))
        (when result
          (dom/span
            (dom/props {:class (if (:ok result)
                                 "repo-update-bar__result"
                                 "repo-update-bar__result repo-update-bar__result--error")
                        :role "status"})
            (dom/text (result-message result))))
        (when (:topic-id result)
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
            (dom/text "Open snapshot")
            (dom/On "click"
              (fn [_] (navigate! :viewer (nav/nav-topic (:topic-id result) nil)))
              nil)))))))

(def ^:private headers ["Snapshot" "State"])
(def ^:private grid-cols "minmax(0, 2fr) 160px")

(e/defn SnapshotRow
  "One snapshot; a click opens it."
  [s i navigate!]
  (e/client
    (dom/tr
      (dom/props {:class "repo-snapshot" :style {:--order i} :data-topic-id (str (:id s))})
      (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-topic (:id s) nil))) nil)
      (dom/td (dom/props {:class "tape-cell-strong tape-cell-click" :dir "ltr"})
        (dom/span (dom/props {:class "tape-cell-text"}) (dom/text (str "Pulled " (:pulled s)))))
      (dom/td (dom/props {:class "tape-cell-meta tape-cell-click"})
        (dom/text (:status s))))))

(e/defn SnapshotTable
  "The snapshots of this Doc, newest first."
  [user-id root-id navigate!]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          rows (e/server (e/Offload #(snapshots* rev user-id root-id)))
          n (e/server (some-> rows count))]
      (dom/div
        (dom/props {:class "repo-home__table" :data-testid "gdoc-snapshot-table"})
        (tape/TapeTable headers grid-cols (or n 0) nil
          (if (nil? n) "" "No snapshot is stored yet.")
          (e/fn [i]
            (when-let [s (e/server (nth rows i nil))]
              (SnapshotRow s i navigate!))))))))

(e/defn GdocHome
  "The home of the Google Doc root the viewer shows."
  []
  (e/client
    (let [user-id dctx/user-id
          root-id dctx/page-topic-id
          navigate! dctx/navigate!]
      (dom/div
        (dom/props {:class "repo-home" :data-testid "gdoc-home"})
        (PullBar user-id root-id navigate!)
        (dom/section
          (dom/props {:class "repo-home__section"})
          (dom/div
            (dom/props {:class "repo-home__head"})
            (dom/h3 (dom/text "Snapshots")))
          (SnapshotTable user-id root-id navigate!))))))
