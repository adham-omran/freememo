(ns freememo.forge-import-modal
  "Import a repository from a git forge: the form, the picker, the import
   (plans/git-forge-integration.md §4.3).

   Its own namespace rather than a branch in `freememo.import-modal`, for the
   reason `video-import-modal` records: this flow has its own stages and a
   windowed picker, and the shared modal is large enough to approach the 64KB
   method cap.

   Two stages, held in one atom:
     :form     forge, host, repository and ref; Download stages the commit
     :picking  the whole tree, directory checkboxes and extension checkboxes

   The staged tree belongs to an upload session. `StagePicker` releases it when
   its frame ends — Cancel, Escape, a closed tab — unless the import holds it
   (`repo-import/cancel-stage!`), so an abandoned picker leaves nothing on disk
   (§4.3 3.2.3). A session nothing released is swept after six hours
   (§4.3 3.2.4).

   While a step runs, `!running` names it, and every exit goes through
   `request-exit!` (plans/repo-import-progress.md §4). A running download has
   one exit, which abandons it. A running import has two: Close, after which the
   import continues and ends in a toast, and Cancel import, which rolls it back."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Tape]]
   [freememo.forge-kinds :as kinds]
   [freememo.modal-shell :as modal-shell]
   [freememo.navigation :as nav]
   [freememo.repo-tree :as tree]
   [freememo.scroll :refer [Scroll-window]]
   [freememo.typeahead :as typeahead]
   [freememo.util :as util]
   #?(:clj [freememo.forge :as forge])
   #?(:clj [freememo.forge-tokens :as tokens])
   #?(:clj [freememo.repo-import :as repo-import])
   #?(:clj [freememo.user-state :as us])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn stage-repository!* [user-id form]
  #?(:clj (repo-import/stage-repository! user-id form) :cljs nil))

(defn staged-tree* [user-id stage-id]
  #?(:clj (repo-import/staged-tree user-id stage-id) :cljs nil))

(defn await-import!*
  "Post: `import-staged!`'s result. The run belongs to the server, so cancelling
   this wait — the picker closed — leaves the import running."
  [user-id stage-id selection]
  #?(:clj (deref (repo-import/start-import! user-id stage-id selection)) :cljs nil))

(defn download-phase*
  "Post: the phase of the download `form` started, or nil."
  [phases form]
  #?(:clj (get phases (repo-import/form-work-key form)) :cljs nil))

(defn import-phase*
  "Post: the phase of the import of the staged `source`, or nil."
  [phases source]
  #?(:clj (get phases (repo-import/source-work-key source)) :cljs nil))

(defn exit-running-step!*
  "Pre : `running` is the value of `!running` when the user asked to leave.
   Post: a running download is abandoned; a running import or Add files run
         (`repo-add-files`) is abandoned for `exit` :cancel and backgrounded for
         :close. Returns :done."
  [user-id running exit]
  #?(:clj (do (case (:step running)
                :download (repo-import/cancel-import! user-id
                            (repo-import/form-work-key (:form running)))
                :import (if (= :cancel exit)
                          (repo-import/cancel-import! user-id
                            (repo-import/source-work-key (:source running)))
                          (repo-import/background-import! user-id (:stage-id running)))
                :add-files (if (= :cancel exit)
                             (repo-import/cancel-import! user-id
                               (repo-import/repo-url-work-key (:repo-url running)))
                             (repo-import/background-import! user-id (:run-key running)))
                nil)
              :done)
     :cljs nil))

(defn cancel-stage!* [user-id stage-id]
  #?(:clj (repo-import/cancel-stage! user-id stage-id) :cljs nil))

(defn saved-hosts*
  "Post: the hosts that hold a saved token for `forge`, for the host suggestions."
  [user-id forge]
  #?(:clj (into [] (comp (filter #(= forge (:forge %))) (map :host))
            (tokens/saved-origins user-id))
     :cljs []))

(defn repository-suggestions*
  "Post: the repository paths the saved token for (`forge`, `host`) can read.
   Empty with no forge request when no token is saved for that host, and empty
   when the forge refuses. Never throws: a suggestion list is a convenience."
  [user-id forge host]
  #?(:clj (try
            (let [origin (forge/parse-origin (or (some-> host str/trim not-empty)
                                               (kinds/default-host forge)))]
              (if (and origin
                    (some #(and (= forge (:forge %)) (= (:host origin) (:host %)))
                      (tokens/saved-origins user-id)))
                (mapv :path (forge/list-repositories forge origin
                              (tokens/token user-id forge (:host origin))))
                []))
            (catch Exception _ []))
     :cljs []))

;; ── Running steps ──────────────────────────────────────────────────

(def ^:private phase-labels
  "The phases `repo-import` publishes, as the modal words them."
  {"cloning" "Cloning the history…"
   "fetching" "Fetching new commits…"
   "resolving" "Finding the commit…"
   "downloading" "Downloading the files…"
   "creating topics" "Creating topics…"})

(e/defn RunStatus
  "A spinner, the phase, and one line on what the user may do meanwhile."
  [phase hint]
  (dom/div
    (dom/props {:class "forge-import__status" :role "status"})
    (dom/p
      (dom/props {:class "forge-import__phase"})
      (dom/span (dom/props {:class "spinner"}))
      (dom/text (get phase-labels phase "Starting…")))
    (dom/p (dom/props {:class "hint"}) (dom/text hint))))

(e/defn MarkRunning
  "Post: `!running` holds `running` for the life of the calling frame."
  [!running running]
  (e/client
    (e/on-unmount #(reset! !running nil))
    (reset! !running running)))

(e/defn ExitRun
  "Carry out the exit the user asked for while a step ran, then close.
   The server call returns before `close!` runs, because closing unmounts the
   frame that makes it."
  [user-id !running exit close!]
  (e/client
    (let [[t ?error] (e/Token exit)]
      (when t
        (e/on-unmount close!)
        (let [running (e/snapshot (e/watch !running))
              kind (second exit)]
          (case (e/server (e/Offload #(exit-running-step!* user-id running kind)))
            (t)))))))

;; ── Form ───────────────────────────────────────────────────────────

(e/defn FieldLabel [text]
  (dom/label (dom/props {:class "label"}) (dom/text text))
  (e/amb))

(e/defn ForgeSelect
  "A1-fallback select (CLAUDE.md, Forms5 → Select dropdowns).
   Post: choosing a forge also replaces a host the user did not type — a blank
         one, or another forge's public host."
  [!forge !host]
  (let [forge (e/watch !forge)]
    (dom/select
      (dom/props {:class "input input-full"})
      (dom/On "change"
        (fn [e]
          (let [v (-> e .-target .-value)]
            (when (or (str/blank? @!host) (some #(= @!host (:default-host %)) kinds/forges))
              (reset! !host (kinds/default-host v)))
            (reset! !forge v)))
        nil)
      (e/for [{:keys [id label]} (e/diff-by :id kinds/forges)]
        (dom/option
          (dom/props {:value id :selected (= id forge)})
          (dom/text label)))))
  (e/amb))

(e/defn ForgeForm
  "The first stage: which repository, at which ref.
   Post: a successful download moves `!stage` to :picking with the stage result.
         A failure keeps what the user typed; Forms5 prints its message, and an
         already-imported repository also gets an Open repository button.
         While the download runs, its phase shows and Cancel abandons it."
  [user-id !stage !running request-exit! open-topic! use-zip!]
  (e/client
    (let [running (e/watch !running)
          !forge (atom "github")
          forge (e/watch !forge)
          !host (atom (kinds/default-host "github"))
          host (e/watch !host)
          !repository (atom "")
          !failure (atom nil)
          failure (e/watch !failure)
          saved-hosts (e/server (e/Offload #(saved-hosts* user-id forge)))
          suggestions (e/server (e/Offload #(repository-suggestions* user-id forge host)))
          commits (forms/Form! {:ref ""}
                    (e/fn Fields [{:keys [ref]}]
                      (e/amb
                        (FieldLabel "Forge")
                        (ForgeSelect !forge !host)
                        (FieldLabel "Host")
                        ;; Suggest and Typeahead are Forms5's documented
                        ;; exceptions; their atoms merge in through :Parse.
                        (do (typeahead/Suggest !host saved-hosts (kinds/default-host forge) false)
                            (e/amb))
                        (FieldLabel "Repository")
                        (do (typeahead/Suggest !repository suggestions
                              "owner/name, or paste the repository URL" true)
                            (e/amb))
                        (FieldLabel "Branch, tag or commit")
                        (forms/Input! :ref (or ref "")
                          :class "input input-full" :placeholder "The default branch")
                        (do (dom/p (dom/props {:class "hint"})
                              (dom/text "A private repository needs a token. Add one in Settings → Git Forges."))
                            (e/amb))
                        (dom/div
                          (dom/props {:class "forge-import__actions"})
                          (e/amb
                            (forms/SubmitButton! :label "Download" :class "btn btn-primary")
                            (do (dom/button
                                  (dom/props {:type "button" :class "btn btn-secondary"})
                                  (dom/text (if (= :download (:step running))
                                              "Cancel download"
                                              "Cancel"))
                                  (dom/On "click" (fn [_] (request-exit! :cancel)) nil))
                                (e/amb))))))
                    :Parse (e/fn [fields _tempid]
                             [`StageRepository {:forge (e/watch !forge)
                                                :host (e/watch !host)
                                                :repository (e/watch !repository)
                                                :ref (:ref fields)}])
                    :type :command
                    :show-buttons false)]
      (e/for [[token [_ form]] (e/diff-by first (e/as-vec commits))]
        ;; `form` is fixed per commit, so a keystroke while the download runs
        ;; cannot restart it.
        (let [phase (e/server (download-phase* (e/watch (us/get-atom user-id :importing-repos))
                                form))
              r (e/server (e/Offload #(stage-repository!* user-id form)))]
          (MarkRunning !running {:step :download :form form})
          (RunStatus phase "A large repository can take a few minutes.")
          (case r
            (cond
              (:ok r)
              (do (e/on-unmount #(reset! !stage {:stage :picking :staged r}))
                  (token))

              (:cancelled? r) (token)

              :else
              (case (reset! !failure r)
                (token (:error r)))))))
      ;; The failure's MESSAGE is Forms5's to print: the token is spent with it,
      ;; and the form shows it in `p[data-role=errormessage]`. Only the action a
      ;; message cannot carry is rendered here.
      (when-some [topic-id (:topic-id failure)]
        (dom/div
          (dom/props {:class "forge-import__error"})
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
            (dom/text "Open repository")
            (dom/On "click" (fn [_] (open-topic! topic-id)) nil))))
      (dom/button
        (dom/props {:type "button" :class "forge-import__link"})
        (dom/text "Upload a .zip file instead")
        (dom/On "click" (fn [_] (use-zip!)) nil)))))

;; ── Picker ─────────────────────────────────────────────────────────

(def ^:private row-height 30)

(e/defn DirectoryRow
  "One directory of the picker. Its checkbox reads the client's decisions, so a
   click answers with no round trip. A directory `old-dirs` selects stays
   checked (`repo-tree/dir-locked?`)."
  [i row decisions !decisions old-dirs]
  (e/client
    (let [{:keys [path name depth file-count match?]} row]
      (dom/tr
        (dom/props {:style {:--order i}})
        (dom/td
          (dom/label
            (dom/props {:class (if match? "forge-picker__dir" "forge-picker__dir forge-picker__dir--context")
                        :style {:padding-left (str (+ 8 (* 16 depth)) "px")}})
            (dom/input
              (dom/props {:type "checkbox" :checked (tree/dir-selected? decisions path)
                          :disabled (tree/dir-locked? old-dirs decisions path)})
              (dom/On "change" (fn [_] (swap! !decisions tree/toggle-dir path)) nil))
            (dom/span
              (dom/props {:class "forge-picker__name" :dir "ltr"})
              (dom/text (if (= "" path) "Top-level files" (str name "/"))))
            (dom/span
              (dom/props {:class "forge-picker__count"})
              (dom/text (str file-count (if (= 1 file-count) " file" " files"))))))))))

(e/defn DirectoryChoices
  "The Directories section of a picker: All, None, a search, and one checkbox
   per directory of `commit-tree`.

   Pre : `commit-tree` is a server-form binding; it never crosses whole.
         `old-dirs` is {} for a first import, else the stored decisions,
         already on the client.
   Post: `!decisions` holds the user's choices. With `none?` false, None is
         absent (plans/repo-selection-widening.md §6.2.3)."
  [commit-tree !decisions old-dirs none?]
  (e/client
    (let [decisions (e/watch !decisions)
          rows (e/server (tree/picker-rows commit-tree))
          ;; Searches are debounced client atoms and run on the server, so a
          ;; keystroke sends one string and the rows never cross whole.
          !dir-query (atom "")
          dir-query (e/watch !dir-query)
          !dir-query-timer (atom nil)
          shown-rows (e/server (tree/filter-picker-rows rows dir-query))
          row-count (e/server (count shown-rows))]
      (dom/div
        (dom/props {:class "forge-picker__heading"})
        (dom/span (dom/text "Directories"))
        ;; With no search this is [""], so All and None write the root as
        ;; they always did; with one, they write the matches and their subtrees.
        (let [outermost (e/server (tree/outermost-matches shown-rows))]
          (dom/button
            (dom/props {:type "button" :class "forge-import__link"})
            (dom/text "All")
            (dom/On "click" (fn [_] (swap! !decisions tree/set-dirs outermost true)) nil))
          (when none?
            (dom/button
              (dom/props {:type "button" :class "forge-import__link"})
              (dom/text "None")
              (dom/On "click" (fn [_] (swap! !decisions tree/set-dirs outermost false)) nil)))))
      (dom/input
        (dom/props {:type "text" :class "input forge-picker__search"
                    :placeholder "Search directories…" :aria-label "Search directories"})
        (dom/On "input" (fn [e] (util/debounce! !dir-query-timer !dir-query (-> e .-target .-value) 300)) nil))
      (dom/div
        (dom/props {:class "tape-scroll forge-picker__dirs"})
        (let [[offset limit] (Scroll-window row-height row-count dom/node
                               {:overquery-factor 2 :reset-key dir-query})]
          (dom/props {:style {:--count row-count :--grid-cols "1fr"
                              :--row-height (str row-height "px")}})
          (dom/table
            (e/for [i (Tape offset limit)]
              (let [row (e/server (nth shown-rows i nil))]
                (when row
                  (DirectoryRow i row decisions !decisions old-dirs)))))
          ;; A sibling overlay, never a branch that unmounts the table
          ;; (CLAUDE.md, "Never unmount a virtual-scroll table").
          (when (zero? row-count)
            (dom/div (dom/props {:class "tape-table-empty"})
              (dom/text "No directories match."))))))))

(e/defn ExtensionChoices
  "The File extensions section of a picker: All, None, a search, and one
   checkbox per extension of `commit-tree`.

   Pre : as `DirectoryChoices`; `old-extensions` is #{} for a first import.
   Post: `!extensions` holds the user's choices. An extension in
         `old-extensions` stays checked. With `none?` false, None is absent."
  [commit-tree !extensions old-extensions none?]
  (e/client
    (let [extensions (e/watch !extensions)
          extension-counts (e/server (tree/extension-counts commit-tree))
          !ext-query (atom "")
          ext-query (e/watch !ext-query)
          !ext-query-timer (atom nil)
          shown-extensions (e/server (tree/filter-extension-counts extension-counts ext-query))]
      (dom/div
        (dom/props {:class "forge-picker__heading"})
        (dom/span (dom/text "File extensions"))
        ;; All and None act on the extensions shown, which with no search
        ;; is every extension.
        (let [shown (e/server (mapv first shown-extensions))]
          (dom/button
            (dom/props {:type "button" :class "forge-import__link"})
            (dom/text "All")
            (dom/On "click" (fn [_] (swap! !extensions into shown)) nil))
          (when none?
            (dom/button
              (dom/props {:type "button" :class "forge-import__link"})
              (dom/text "None")
              (dom/On "click" (fn [_] (swap! !extensions #(reduce disj % shown))) nil)))))
      (dom/p
        (dom/props {:class "hint"})
        (dom/text "A file with an unselected extension is still a topic, with no source. You can mark it done later."))
      (dom/input
        (dom/props {:type "text" :class "input forge-picker__search"
                    :placeholder "Search extensions…" :aria-label "Search extensions"})
        (dom/On "input" (fn [e] (util/debounce! !ext-query-timer !ext-query (-> e .-target .-value) 300)) nil))
      (dom/div
        (dom/props {:class "forge-picker__extensions"})
        (when (e/server (empty? shown-extensions))
          (dom/span (dom/props {:class "hint"}) (dom/text "No extensions match.")))
        (e/for [[ext n] (e/diff-by first shown-extensions)]
          (dom/label
            (dom/props {:class "forge-picker__extension"})
            (dom/input
              (dom/props {:type "checkbox" :checked (contains? extensions ext)
                          :disabled (contains? old-extensions ext)})
              (dom/On "change"
                (fn [e] (swap! !extensions (if (-> e .-target .-checked) conj disj) ext))
                nil))
            (dom/span (dom/props {:dir "ltr"}) (dom/text (tree/extension-label ext)))
            (dom/span (dom/props {:class "forge-picker__count"}) (dom/text (str n)))))))))

(e/defn PickerSummary
  "The status line under a picker's choices."
  [text over?]
  (dom/p
    (dom/props {:class (if over?
                         "forge-picker__summary forge-picker__summary--over"
                         "forge-picker__summary")
                :role "status"})
    (dom/text text)))

(defn- import-summary
  "Post: [text over?] for an import of `node-count` topics."
  [node-count]
  (cond
    (<= node-count 1) ["Select at least one directory." false]
    (> node-count tree/max-selection-nodes)
    [(str node-count " topics. One import may create "
       tree/max-selection-nodes ". Select fewer directories.") true]
    :else [(str node-count " topics will be created.") false]))

(e/defn StagePicker
  "The second stage: the whole tree, directory checkboxes and extension
   checkboxes (§4.3 3.3).

   Pre : `staged` is a successful `stage-repository!` result.
   Post: Import creates the rows and opens the commit. While it runs, Close
         leaves it running and Cancel import rolls it back. Every other way out
         releases the stage."
  [user-id staged !running request-exit! open-topic!]
  (e/client
    (let [stage-id (:stage-id staged)
          source (:source staged)
          {:keys [path ref sha]} source
          importing? (= :import (:step (e/watch !running)))
          ;; SERVER-FORM bindings: the file list stays on the server, and only
          ;; window rows, the extension counts and one number cross.
          picked (e/server (e/Offload #(staged-tree* user-id stage-id)))
          commit-tree (e/server (:tree picked))
          !decisions (atom {})
          decisions (e/watch !decisions)
          !extensions (atom #{})
          extensions (e/watch !extensions)
          node-count (e/server (tree/node-count (tree/import-plan commit-tree decisions)))
          [summary over?] (import-summary node-count)
          !failure (atom nil)
          failure (e/watch !failure)]
      (dom/div
        (dom/props {:class "forge-picker"})
        (dom/p
          (dom/props {:class "forge-picker__source" :dir "ltr"})
          (dom/text (str path " · " (subs sha 0 7) (when ref (str " (" ref ")"))
                      " · " (:file-count staged) " files")))
        (DirectoryChoices commit-tree !decisions {} true)
        (ExtensionChoices commit-tree !extensions #{} true)
        (PickerSummary summary over?)
        (when failure
          (dom/div
            (dom/props {:class "forge-import__error" :role "alert"})
            (dom/text failure)))
        (when importing?
          (RunStatus (e/server (import-phase* (e/watch (us/get-atom user-id :importing-repos))
                                 source))
            "You can close this window. The import continues, and a notice appears when it is done."))
        (dom/div
          (dom/props {:class "forge-import__actions"})
          ;; The button's props stay static. A `:disabled` derived from
          ;; `node-count` would be absent for each round trip and null every prop
          ;; on the node (CLAUDE.md, "An Absent Props Map Nulls Every Prop"), so
          ;; an empty or oversized selection is refused inside the token instead.
          (e/for [[t _] (forms/Button! [::import] :class "btn btn-primary" :label "Import")]
            (let [selection (e/snapshot {:dirs decisions :extensions extensions})
                  blocked (e/snapshot (cond
                                        (<= node-count 1) "Select at least one directory."
                                        (> node-count tree/max-selection-nodes)
                                        (str "This selection creates " node-count
                                          " topics. One import may create "
                                          tree/max-selection-nodes ".")
                                        :else nil))]
              (if blocked
                (t (reset! !failure blocked))
                (let [r (e/server (e/Offload #(await-import!* user-id stage-id selection)))]
                  (MarkRunning !running {:step :import :stage-id stage-id :source source})
                  (case r
                    (cond
                      (:ok r)
                      (do (e/on-unmount #(open-topic! (:topic-id r)))
                          (t))

                      (:cancelled? r) (t)

                      :else (t (reset! !failure (:error r)))))))))
          (when importing?
            (dom/button
              (dom/props {:type "button" :class "btn btn-secondary"})
              (dom/text "Close")
              (dom/On "click" (fn [_] (request-exit! :close)) nil)))
          (dom/button
            (dom/props {:type "button" :class "btn btn-secondary"})
            (dom/text (if importing? "Cancel import" "Cancel"))
            (dom/On "click" (fn [_] (request-exit! :cancel)) nil))))
      ;; LAST form, so its value is the body's value and it gates no frame.
      ;; Releases the stage when this picker ends; `cancel-stage!` leaves a
      ;; stage alone while `import-staged!` holds it.
      (e/server (e/on-unmount #(cancel-stage!* user-id stage-id))))))

;; ── Modal ──────────────────────────────────────────────────────────

(e/defn ForgeImportModal
  "Pre : `!show` is the Import page's open-source atom.
   Post: closing writes false to it; `Upload a .zip file instead` writes
         :code-zip, which opens the archive upload.
   Invariant: after the user asks to leave a running step, the modal never
         navigates. A backgrounded import reports through a toast instead, so
         the user gets exactly one of the two."
  [!show user-id navigate!]
  (e/client
    (let [!stage (atom {:stage :form})
          stage (e/watch !stage)
          !running (atom nil)
          !leaving? (atom false)
          !exit (atom nil)
          exit (e/watch !exit)
          close! (fn [] (reset! !show false))
          open-topic! (fn [topic-id]
                        (when-not @!leaving?
                          (reset! !show false)
                          (navigate! :viewer (nav/nav-topic topic-id nil))))
          request-exit! (fn [kind]
                          ;; Escape and the backdrop pass :close. For a download
                          ;; that still abandons it: a download has no background.
                          (if @!running
                            (do (reset! !leaving? true)
                                (swap! !exit (fn [[n _]] [(inc (or n 0)) kind])))
                            (close!)))
          use-zip! (fn [] (reset! !show :code-zip))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal-shell/ModalEscape #(request-exit! :close) "Import a repository")
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (request-exit! :close))) nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg forge-import"})
          (dom/h3
            (dom/props {:class "forge-import__title"})
            (dom/text "Import a repository"))
          (case (:stage stage)
            :picking (StagePicker user-id (:staged stage) !running request-exit! open-topic!)
            (ForgeForm user-id !stage !running request-exit! open-topic! use-zip!))))
      (ExitRun user-id !running exit close!))))
