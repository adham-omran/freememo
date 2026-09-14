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
   (§4.3 3.2.4)."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [freememo.forge-kinds :as kinds]
   [freememo.modal-shell :as modal-shell]
   [freememo.navigation :as nav]
   [freememo.repo-tree :as tree]
   [freememo.typeahead :as typeahead]
   #?(:clj [freememo.forge :as forge])
   #?(:clj [freememo.forge-tokens :as tokens])
   #?(:clj [freememo.repo-import :as repo-import])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn stage-repository!* [user-id form]
  #?(:clj (repo-import/stage-repository! user-id form) :cljs nil))

(defn staged-tree* [user-id stage-id]
  #?(:clj (repo-import/staged-tree user-id stage-id) :cljs nil))

(defn import-staged!* [user-id stage-id selection]
  #?(:clj (repo-import/import-staged! user-id stage-id selection) :cljs nil))

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
         already-imported repository also gets an Open repository button."
  [user-id !stage close! open-topic! use-zip!]
  (e/client
    (let [!forge (atom "github")
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
                                  (dom/text "Cancel")
                                  (dom/On "click" (fn [_] (close!)) nil))
                                (e/amb))))))
                    :Parse (e/fn [fields _tempid]
                             [`StageRepository {:forge (e/watch !forge)
                                                :host (e/watch !host)
                                                :repository (e/watch !repository)
                                                :ref (:ref fields)}])
                    :type :command
                    :show-buttons false)]
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
        (dom/On "click" (fn [_] (use-zip!)) nil))
      (e/for [[token [_ form]] (e/diff-by first (e/as-vec commits))]
        ;; `form` is fixed per commit, so a keystroke while the download runs
        ;; cannot restart it.
        (let [r (e/server (e/Offload #(stage-repository!* user-id form)))]
          (case r
            (if (:ok r)
              (do (e/on-unmount #(reset! !stage {:stage :picking :staged r}))
                  (token))
              (case (reset! !failure r)
                (token (:error r))))))))))

;; ── Picker ─────────────────────────────────────────────────────────

(def ^:private row-height 30)

(e/defn DirectoryRow
  "One directory of the picker. Its checkbox reads the client's decisions, so a
   click answers with no round trip."
  [i row decisions !decisions]
  (e/client
    (let [{:keys [path name depth file-count]} row]
      (dom/tr
        (dom/props {:style {:--order i}})
        (dom/td
          (dom/label
            (dom/props {:class "forge-picker__dir"
                        :style {:padding-left (str (+ 8 (* 16 depth)) "px")}})
            (dom/input
              (dom/props {:type "checkbox" :checked (tree/dir-selected? decisions path)})
              (dom/On "change" (fn [_] (swap! !decisions tree/toggle-dir path)) nil))
            (dom/span
              (dom/props {:class "forge-picker__name" :dir "ltr"})
              (dom/text (if (= "" path) "Top-level files" (str name "/"))))
            (dom/span
              (dom/props {:class "forge-picker__count"})
              (dom/text (str file-count (if (= 1 file-count) " file" " files"))))))))))

(e/defn StagePicker
  "The second stage: the whole tree, directory checkboxes and extension
   checkboxes (§4.3 3.3).

   Pre : `staged` is a successful `stage-repository!` result.
   Post: Import creates the rows and opens the commit. Every other way out
         releases the stage."
  [user-id staged close! open-topic!]
  (e/client
    (let [stage-id (:stage-id staged)
          {:keys [path ref sha]} (:source staged)
          ;; SERVER-FORM bindings: the file list stays on the server, and only
          ;; window rows, the extension counts and one number cross.
          picked (e/server (e/Offload #(staged-tree* user-id stage-id)))
          commit-tree (e/server (:tree picked))
          rows (e/server (tree/picker-rows commit-tree))
          row-count (e/server (count rows))
          extension-counts (e/server (tree/extension-counts commit-tree))
          !decisions (atom {})
          decisions (e/watch !decisions)
          !extensions (atom #{})
          extensions (e/watch !extensions)
          node-count (e/server (tree/node-count (tree/import-plan commit-tree decisions)))
          !failure (atom nil)
          failure (e/watch !failure)]
      (dom/div
        (dom/props {:class "forge-picker"})
        (dom/p
          (dom/props {:class "forge-picker__source" :dir "ltr"})
          (dom/text (str path " · " (subs sha 0 7) (when ref (str " (" ref ")"))
                      " · " (:file-count staged) " files")))

        (dom/div
          (dom/props {:class "forge-picker__heading"})
          (dom/span (dom/text "Directories"))
          (dom/button
            (dom/props {:type "button" :class "forge-import__link"})
            (dom/text "All")
            (dom/On "click" (fn [_] (reset! !decisions {"" true})) nil))
          (dom/button
            (dom/props {:type "button" :class "forge-import__link"})
            (dom/text "None")
            (dom/On "click" (fn [_] (reset! !decisions {})) nil)))
        (dom/div
          (dom/props {:class "tape-scroll forge-picker__dirs"})
          (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 2})]
            (dom/props {:style {:--count row-count :--grid-cols "1fr"
                                :--row-height (str row-height "px")}})
            (dom/table
              (e/for [i (Tape offset limit)]
                (let [row (e/server (nth rows i nil))]
                  (when row
                    (DirectoryRow i row decisions !decisions)))))))

        (dom/div
          (dom/props {:class "forge-picker__heading"})
          (dom/span (dom/text "File extensions"))
          (let [all-extensions (e/server (mapv first extension-counts))]
            (dom/button
              (dom/props {:type "button" :class "forge-import__link"})
              (dom/text "All")
              (dom/On "click" (fn [_] (reset! !extensions (set all-extensions))) nil)))
          (dom/button
            (dom/props {:type "button" :class "forge-import__link"})
            (dom/text "None")
            (dom/On "click" (fn [_] (reset! !extensions #{})) nil)))
        (dom/p
          (dom/props {:class "hint"})
          (dom/text "A file with an unselected extension is still a topic, with no source. You can mark it done later."))
        (dom/div
          (dom/props {:class "forge-picker__extensions"})
          (e/for [[ext n] (e/diff-by first extension-counts)]
            (dom/label
              (dom/props {:class "forge-picker__extension"})
              (dom/input
                (dom/props {:type "checkbox" :checked (contains? extensions ext)})
                (dom/On "change"
                  (fn [e] (swap! !extensions (if (-> e .-target .-checked) conj disj) ext))
                  nil))
              (dom/span (dom/props {:dir "ltr"}) (dom/text (if (= "" ext) "no extension" ext)))
              (dom/span (dom/props {:class "forge-picker__count"}) (dom/text (str n))))))

        (dom/p
          (dom/props {:class (if (> node-count tree/max-selection-nodes)
                               "forge-picker__summary forge-picker__summary--over"
                               "forge-picker__summary")
                      :role "status"})
          (dom/text (cond
                      (<= node-count 1) "Select at least one directory."
                      (> node-count tree/max-selection-nodes)
                      (str node-count " topics. One import may create "
                        tree/max-selection-nodes ". Select fewer directories.")
                      :else (str node-count " topics will be created."))))
        (when failure
          (dom/div
            (dom/props {:class "forge-import__error" :role "alert"})
            (dom/text failure)))
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
                (let [r (e/server (e/Offload #(import-staged!* user-id stage-id selection)))]
                  (case r
                    (if (:ok r)
                      (do (e/on-unmount #(open-topic! (:topic-id r)))
                          (t))
                      (t (reset! !failure (:error r)))))))))
          (dom/button
            (dom/props {:type "button" :class "btn btn-secondary"})
            (dom/text "Cancel")
            (dom/On "click" (fn [_] (close!)) nil))))
      ;; LAST form, so its value is the body's value and it gates no frame.
      ;; Releases the stage when this picker ends; `cancel-stage!` leaves a
      ;; stage alone while `import-staged!` holds it.
      (e/server (e/on-unmount #(cancel-stage!* user-id stage-id))))))

;; ── Modal ──────────────────────────────────────────────────────────

(e/defn ForgeImportModal
  "Pre : `!show` is the Import page's open-source atom.
   Post: closing writes false to it; `Upload a .zip file instead` writes
         :code-zip, which opens the archive upload."
  [!show user-id navigate!]
  (e/client
    (let [!stage (atom {:stage :form})
          stage (e/watch !stage)
          close! (fn [] (reset! !show false))
          open-topic! (fn [topic-id]
                        (reset! !show false)
                        (navigate! :viewer (nav/nav-topic topic-id nil)))
          use-zip! (fn [] (reset! !show :code-zip))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal-shell/ModalEscape close! "Import a repository")
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (close!))) nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg forge-import"})
          (dom/h3
            (dom/props {:class "forge-import__title"})
            (dom/text "Import a repository"))
          (case (:stage stage)
            :picking (StagePicker user-id (:staged stage) close! open-topic!)
            (ForgeForm user-id !stage close! open-topic! use-zip!)))))))
