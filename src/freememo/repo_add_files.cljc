(ns freememo.repo-add-files
  "Add files to an imported repository (plans/repo-selection-widening.md §6):
   the button on the repository home, its modal, and the picker.

   The picker is the import picker's sections (`forge-import-modal`) started
   from the stored selection, with every stored choice locked. The selection
   only grows, and what it adds goes into the newest imported commit.

   While the run goes, `!running` names it and every exit goes through
   `request-exit!`, as in the import modal: Close leaves the run going and
   ends in a toast, and Cancel abandons it.

   Its own namespace for the 64KB method cap: `repo-update-bar` mounts it."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.forge-import-modal :as picker]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal-shell]
   [freememo.navigation :as nav]
   [freememo.repo-tree :as tree]
   #?(:clj [freememo.repo-import :as repo-import])
   #?(:clj [freememo.user-state :as us])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn add-files-target* [user-id repo-id]
  #?(:clj (repo-import/add-files-target user-id repo-id) :cljs nil))

(defn await-add-files!*
  "Post: `add-repository-files!`'s result. The run belongs to the server, so
   cancelling this wait — the picker closed — leaves it going."
  [user-id run-id repo-id commit-id selection]
  #?(:clj (deref (repo-import/start-add-files! user-id run-id repo-id commit-id selection))
     :cljs nil))

(defn repository-phase*
  "Post: the phase of the running work on `repo-url`, or nil."
  [phases repo-url]
  #?(:clj (repo-import/phase-in phases repo-url) :cljs nil))

(defn widening-counts
  "Post: {:insert N :fill M} for widening `target`'s selection to `dirs` and
   `extensions` — the numbers the summary shows (§6.2.4)."
  [target dirs extensions]
  (when (:ok target)
    (let [{:keys [insert fill]} (tree/widening-plan (:tree target) (:selection target)
                                  {:dirs dirs :extensions extensions} (:live target))]
      {:insert (count insert) :fill (count fill)})))

(defn summary-text
  "Post: the summary line for `counts`."
  [{:keys [insert fill]}]
  (cond
    (nil? insert) "Counting…"
    (and (zero? insert) (zero? fill)) "Select a directory or an extension to add."
    :else (str insert (if (= 1 insert) " topic" " topics") " will be added."
            (when (pos? fill)
              (str " " fill (if (= 1 fill) " file gets its" " files get their") " source.")))))

;; ── Picker ─────────────────────────────────────────────────────────

(e/defn AddFilesPicker
  "Pre : `target` is a server-form binding of a successful `add-files-target`.
   Post: Add widens the selection and opens the commit. While it runs, Close
         leaves it going and Cancel abandons it."
  [user-id repo-id target !running request-exit! open-topic!]
  (e/client
    (let [;; SERVER-FORM bindings: the tree and the live rows stay on the
          ;; server; the stored selection and a few strings cross.
          commit-tree (e/server (:tree target))
          old (e/server (:selection target))
          commit-id (e/server (:commit-id target))
          repo-url (e/server (:repo-url target))
          source-line (e/server (str (:path target) ", " (subs (:sha target) 0 7)
                                  (when-let [r (:ref target)] (str " (" r ")"))))
          running? (= :add-files (:step (e/watch !running)))
          !decisions (atom (:dirs old))
          decisions (e/watch !decisions)
          !extensions (atom (:extensions old))
          extensions (e/watch !extensions)
          counts (e/server (widening-counts target decisions extensions))
          !failure (atom nil)
          failure (e/watch !failure)]
      (dom/div
        (dom/props {:class "forge-picker" :data-testid "add-files-picker"})
        (dom/p
          (dom/props {:class "forge-picker__source" :dir "ltr"})
          (dom/text source-line))
        (dom/p
          (dom/props {:class "hint"})
          (dom/text "The new files go into this commit, the newest one imported. Directories and extensions already selected stay selected."))
        (picker/DirectoryChoices commit-tree !decisions (:dirs old) false)
        (picker/ExtensionChoices commit-tree !extensions (:extensions old) false)
        (picker/PickerSummary (summary-text counts) false)
        (when failure
          (dom/div
            (dom/props {:class "forge-import__error" :role "alert"})
            (dom/text failure)))
        (when running?
          (picker/RunStatus (e/server (repository-phase* (e/watch (us/get-atom user-id :importing-repos))
                                        repo-url))
            "You can close this window. The files are still added, and a notice appears when it is done."))
        (dom/div
          (dom/props {:class "forge-import__actions"})
          ;; Static props, for the reason StagePicker's Import button records:
          ;; an empty selection is refused inside the token instead.
          (e/for [[t _] (forms/Button! [::add] :class "btn btn-primary" :label "Add")]
            (let [selection (e/snapshot {:dirs decisions :extensions extensions})
                  blocked (e/snapshot (when (and (zero? (:insert counts 0)) (zero? (:fill counts 0)))
                                        "Select a directory or an extension to add."))]
              (if blocked
                (t (reset! !failure blocked))
                (let [run-id (e/snapshot (str (random-uuid)))
                      r (e/server (e/Offload #(await-add-files!* user-id run-id repo-id commit-id selection)))]
                  (picker/MarkRunning !running {:step :add-files :run-key [:add-files run-id]
                                                :repo-url repo-url})
                  (case r
                    (cond
                      (:ok r)
                      (do (e/on-unmount #(open-topic! (:topic-id r)))
                          (t))

                      (:cancelled? r) (t)

                      :else (t (reset! !failure (:error r)))))))))
          (when running?
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"})
              (dom/text "Close")
              (dom/On "click" (fn [_] (request-exit! :close)) nil)))
          (dom/button
            (dom/props {:type "button" :class "btn btn-secondary"})
            (dom/text (if running? "Cancel adding" "Cancel"))
            (dom/On "click" (fn [_] (request-exit! :cancel)) nil)))))))

;; ── Modal ──────────────────────────────────────────────────────────

(e/defn AddFilesModal
  "Pre : `!open?` is the button's open atom.
   Post: closing writes false to it.
   Invariant: after the user asks to leave a running run, the modal never
         navigates; the toast reports the result instead."
  [user-id repo-id !open? navigate!]
  (e/client
    (let [!running (atom nil)
          !leaving? (atom false)
          !exit (atom nil)
          exit (e/watch !exit)
          close! (fn [] (reset! !open? false))
          open-topic! (fn [topic-id]
                        (when-not @!leaving?
                          (reset! !open? false)
                          (navigate! :viewer (nav/nav-topic topic-id nil))))
          request-exit! (fn [kind]
                          (if @!running
                            (do (reset! !leaving? true)
                                (swap! !exit (fn [[n _]] [(inc (or n 0)) kind])))
                            (close!)))
          ;; SERVER-FORM binding: read once, and only its parts cross.
          target (e/server (e/Offload #(add-files-target* user-id repo-id)))
          state (e/server (cond (nil? target) :loading
                                (:ok target) :ready
                                :else :failed))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal-shell/ModalEscape #(request-exit! :close) "Add files to repository")
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (request-exit! :close))) nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg forge-import"})
          (dom/h3
            (dom/props {:class "forge-import__title"})
            (dom/text "Add files to repository"))
          (case state
            :ready (AddFilesPicker user-id repo-id target !running request-exit! open-topic!)
            :failed (dom/div
                      (dom/div
                        (dom/props {:class "forge-import__error" :role "alert"})
                        (dom/text (e/server (:error target))))
                      (dom/div
                        (dom/props {:class "forge-import__actions"})
                        (dom/button
                          (dom/props {:type "button" :class "btn btn-secondary"})
                          (dom/text "Close")
                          (dom/On "click" (fn [_] (close!)) nil))))
            (loading/Spinner "Reading the repository…"))))
      (picker/ExitRun user-id !running exit close!))))

;; ── Button ─────────────────────────────────────────────────────────

(e/defn AddFilesButton
  "The \"Add files…\" button and its modal (§6.3). Publishes the palette's
   :add-repository-files invoker while mounted."
  [user-id repo-id navigate!]
  (e/client
    (let [!open? (atom false)
          open? (e/watch !open?)]
      (bus/publish-invoker! :add-repository-files #(reset! !open? true))
      (e/on-unmount #(bus/retract-invoker! :add-repository-files))
      (dom/button
        (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
        (dom/text "Add files…")
        (dom/On "click" (fn [_] (reset! !open? true)) nil))
      (dom/div
        (dom/props {:class "repo-add-files-host"})
        (when open? (AddFilesModal user-id repo-id !open? navigate!))))))
