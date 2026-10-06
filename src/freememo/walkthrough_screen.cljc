(ns freememo.walkthrough-screen
  "The walkthrough screen inside the viewer, on the repository node
   (plans/commit-range-walkthrough.md §6.6): the step bar, the canvas and
   the rail replace the content column, and the assistant panel stays on the
   right with the tutor (§6.6.7.2).

   The step the screen shows is the walkthrough's stored step, so a reload
   opens where the reader left (§6.6.5). The file the canvas shows and the
   coloring mode are this screen's own state.

   The screen tells the server what it shows (`walkthrough/set-focus!`), and
   a run about this repository node reads it; that is how the tutor knows the
   step and the file (§6.7)."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.doc-context :as dctx]
   [freememo.forms-ext :as forms-ext]
   [freememo.modal-shell :as modal]
   [freememo.navigation :as nav]
   [freememo.question-pin-pane :as pin-pane]
   [freememo.question-pin-state :as pin-state]
   [freememo.walkthrough-canvas :as canvas]
   [freememo.walkthrough-state :as wstate]
   [freememo.walkthrough-table :refer [RenameDialog]]
   [freememo.walkthrough-text :as wtext]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.walkthrough :as wt])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn overview* [_rev user-id wid]
  #?(:clj (wt/overview user-id wid) :cljs nil))

(defn step-files* [_rev user-id wid position]
  #?(:clj (when (and position (pos? position)) (vec (wt/step-files user-id wid position)))
     :cljs nil))

(defn phase* [phases wid]
  #?(:clj (wt/phase-of phases wid) :cljs nil))

(defn understood!* [user-id wid]
  #?(:clj (wt/understood! user-id wid) :cljs nil))

(defn go-to-step!* [user-id wid step]
  #?(:clj (wt/go-to-step! user-id wid step) :cljs nil))

(defn rename!* [user-id concept-id label]
  #?(:clj (wt/rename-concept! user-id concept-id label) :cljs nil))

(defn merge!* [user-id from-id into-id]
  #?(:clj (wt/merge-concepts! user-id from-id into-id) :cljs nil))

(defn restart!* [user-id wid]
  #?(:clj (wt/restart! user-id wid) :cljs nil))

(defn cancel!* [user-id wid]
  #?(:clj (do (wt/cancel! user-id wid) {:ok true}) :cljs nil))

(defn set-focus!* [user-id repo-id focus]
  #?(:clj (wt/set-focus! user-id repo-id focus) :cljs nil))

(defn clear-focus!* [user-id repo-id wid position]
  #?(:clj (wt/clear-focus! user-id repo-id wid position) :cljs nil))

;; ── Client helpers ─────────────────────────────────────────────────

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

(defn click-control!
  "Click the screen's control matching `selector`, as a palette command
   does. Post: true when one was there."
  [selector]
  #?(:cljs (if-let [b (.querySelector js/document selector)] (do (.click b) true) false)
     :clj false))

(def opener-text
  "The reader's turn that starts the opener (§6.7.1)."
  "Before we start: test what I believe about the code at the first commit.")

#?(:cljs
   ;; Available while the opener shows (§6.7.1): the panel publishes the flag.
   (bus/register-preparer! :ask-walkthrough-tutor
     (fn [ctx] (when (:walkthrough-opener? ctx) {:text opener-text}))))

(defn ask-opener!
  "Send the opener's turn to the tutor. Post: the dispatch outcome."
  []
  (bus/dispatch! :ask-walkthrough-tutor {:text opener-text} :button))

(defn apply-canvas-request!
  "A citation the reader clicked names a file of this step
   (`question-pin-state/open-in-canvas!`): show it, then point at the
   passage. Post: nil."
  [request repo-id paths pick-path!]
  (when-let [[r p] (:source-doc request)]
    (when (and (= r repo-id) (contains? (set paths) p))
      (pick-path! p)
      (pin-state/point! (select-keys request [:quote :anchor :source-doc :scroll?]))))
  nil)

;; ── Route ──────────────────────────────────────────────────────────

(e/defn WalkthroughRoute
  "Tell the content column which walkthrough the viewer route names. Its
   effect is its body's value, so it gates nothing."
  [repo-id walkthrough-id]
  (e/client
    (e/on-unmount #(wstate/route! nil nil))
    (wstate/route! repo-id walkthrough-id)))

;; ── Text ───────────────────────────────────────────────────────────

(e/defn InlineCode
  "Model-written `text` with its inline code set as code
   (`walkthrough-text/code-spans`). The spans sit in an element of their
   own, so the loop shares its parent with nothing."
  [text]
  (e/client
    (dom/span
      (dom/props {:class "wt-rich"})
      (e/for [[_ [kind s]] (e/diff-by first (map-indexed vector (wtext/code-spans text)))]
        (if (= :code kind)
          (dom/code (dom/props {:class "wt-inline-code"}) (dom/text s))
          (dom/text s))))))

;; ── Panels ─────────────────────────────────────────────────────────

(e/defn PreparingPanel
  "While the walkthrough is grouped (§6.5.4): its phase and Cancel."
  [user-id wid phase]
  (e/client
    (dom/div
      (dom/props {:class "wt-panel" :role "status"})
      (dom/p (dom/props {:class "wt-panel__line"})
        (dom/span (dom/props {:class "spinner"}))
        (dom/text (str "Preparing the walkthrough" (when phase (str ": " phase)) "…")))
      (dom/p (dom/props {:class "wt-panel__hint"})
        (dom/text "FreeMemo reads every change of the range and groups it by concept. You can leave this page; the Activity panel shows the work."))
      (bus/publish-invoker! :cancel-walkthrough #(click-control! ".wt-cancel"))
      (e/on-unmount #(bus/retract-invoker! :cancel-walkthrough))
      (e/for [[t _] (forms/Button! [::cancel] :class "btn btn-sm btn-secondary wt-cancel"
                      :label "Cancel")]
        (let [r (e/server (e/Offload #(cancel!* user-id wid)))]
          (case r (t)))))))

(e/defn FailedPanel
  "A walkthrough whose preparation failed: why, and Try again."
  [user-id wid error]
  (e/client
    (dom/div
      (dom/props {:class "wt-panel"})
      (dom/p (dom/props {:class "wt-panel--error" :role "alert"})
        (dom/text (str "This walkthrough could not be prepared. " error)))
      (bus/publish-invoker! :restart-walkthrough #(click-control! ".wt-restart"))
      (e/on-unmount #(bus/retract-invoker! :restart-walkthrough))
      (e/for [[t _] (forms/Button! [::restart] :class "btn btn-sm btn-primary wt-restart"
                      :label "Try again")]
        (let [r (e/server (e/Offload #(restart!* user-id wid)))]
          (case r (if (:ok r) (t) (t (:error r)))))))))

(e/defn OpenerPanel
  "Step 0 (§6.7.1): the concepts ahead, and the tutor's test of what the
   reader believes about the starting commit."
  [concepts]
  (e/client
    (dom/div
      (dom/props {:class "wt-panel wt-opener"})
      (e/on-unmount #(bus/retract-ctx! [:walkthrough-opener?]))
      (bus/publish-ctx! {:walkthrough-opener? true})
      (dom/h3 (dom/text "Before the first step"))
      (dom/p (dom/props {:class "wt-panel__hint"})
        (dom/text "The tutor first asks what you believe about the code at the first commit, so it teaches what you lack and skips what you know."))
      (dom/button
        (dom/props {:type "button" :class "btn btn-primary wt-opener__ask"})
        (dom/text "Test what I know about the starting commit")
        (dom/On "click" (fn [_] (ask-opener!)) nil))
      (dom/ol
        (dom/props {:class "wt-opener__concepts"})
        (e/for [c concepts]
          (dom/li
            (dom/strong (InlineCode (:label c)))
            (dom/text " — ")
            (InlineCode (:summary c))))))))

(defn reveal-current-step!
  "Scroll the step list sideways so the current step's chip shows. A narrow
   screen lays the chips out in one scrolling row.
   Pre: `bar` is the mounted step bar, `_step` the step it marks current.
   Post: after the next frame, the current chip is inside the list's visible
   width. Nothing outside the list scrolls. Returns nil."
  [bar _step]
  #?(:cljs (js/requestAnimationFrame
             (fn []
               (let [steps (.querySelector bar ".wt-steps__list")
                     chip (some-> steps (.querySelector ".wt-step--current"))]
                 (when chip
                   (let [lr (.getBoundingClientRect steps)
                         cr (.getBoundingClientRect chip)]
                     (cond
                       (< (.-left cr) (.-left lr))
                       (set! (.-scrollLeft steps) (+ (.-scrollLeft steps) (- (.-left cr) (.-left lr)) -8))
                       (> (.-right cr) (.-right lr))
                       (set! (.-scrollLeft steps) (+ (.-scrollLeft steps) (- (.-right cr) (.-right lr)) 8))))))))
     :clj nil)
  nil)

(e/defn StepBar
  "The steps in order, the current one marked, and the coloring toggle
   (§6.6.2). Post: nothing; a click opens that step."
  [user-id wid step concepts mode !mode]
  (e/client
    (dom/nav
      (dom/props {:class "wt-steps" :aria-label "Walkthrough steps"})
      (dom/ol
        (dom/props {:class "wt-steps__list"})
        (let [commands (e/amb
                         (dom/li
                           (forms/Button! [::go 0]
                             :class (str "wt-step" (when (= 0 step) " wt-step--current"))
                             :label "Start"))
                         (e/for [c concepts]
                           (dom/li
                             (forms/Button! [::go (:position c)]
                               :class (str "wt-step"
                                        (when (= step (:position c)) " wt-step--current")
                                        (when (:understood? c) " wt-step--done")
                                        (when (:noise c) " wt-step--noise"))
                               :label (str (:position c) ". " (wtext/plain (:label c)))))))]
          (e/for [[t [_ n]] (e/diff-by first (e/as-vec commands))]
            (let [r (e/server (e/Offload #(go-to-step!* user-id wid n)))]
              (case r (t))))))
      (dom/div
        (dom/props {:class "wt-mode" :role "group" :aria-label "Which changes are colored"})
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                      :aria-pressed (if (= :previous mode) "true" "false")})
          (dom/text "Since previous step")
          (dom/On "click" (fn [_] (reset! !mode :previous)) nil))
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                      :aria-pressed (if (= :start mode) "true" "false")})
          (dom/text "Since A")
          (dom/On "click" (fn [_] (reset! !mode :start)) nil)))
      (reveal-current-step! dom/node step))))

(def merge-hint
  "What merging a step does, for the control's tooltip."
  "Use this when the model split one idea into two steps. This step's changes move into the step you pick, and this step is deleted.")

(defn merge-message
  "The confirmation text for merging step `from-label`, with `n` changes,
   into step `into-label`."
  [from-label into-label n]
  (str "Merge \"" from-label "\" into \"" into-label "\"? "
    "This step's " n " change" (when (not= 1 n) "s") " move into that step, this step is deleted, "
    "and the later steps are numbered again. A merge cannot be undone."))

(defn install-edit-dismiss!
  "Close the Edit step popover the way a menu closes.
   Pre: `details` is the mounted <details> of the popover.
   Post: a pointer press outside it, or Escape inside it, closes it, and
   Escape returns focus to its toggle. Returns the fn that removes both
   listeners."
  [details]
  #?(:cljs (let [press (fn [e]
                         (when (and (.-open details) (not (.contains details (.-target e))))
                           (set! (.-open details) false)))
                 key (fn [e]
                       (when (and (= "Escape" (.-key e)) (.-open details))
                         (.stopPropagation e)
                         (set! (.-open details) false)
                         (some-> (.querySelector details "summary") (.focus))))]
             (.addEventListener js/document "pointerdown" press true)
             (.addEventListener details "keydown" key)
             (fn []
               (.removeEventListener js/document "pointerdown" press true)
               (.removeEventListener details "keydown" key)))
     :clj (fn [])))

(e/defn UnderstoodButton
  "\"I understand\" (§6.6.5): marks the step and opens the next one."
  [user-id wid]
  (e/client
    (bus/publish-invoker! :walkthrough-step #(click-control! ".wt-understood"))
    (e/on-unmount #(bus/retract-invoker! :walkthrough-step))
    (e/for [[t _] (forms/Button! [::understood]
                    :class "btn btn-sm btn-primary wt-understood" :label "I understand")]
      (let [r (e/server (e/Offload #(understood!* user-id wid)))]
        (case r (t))))))

(e/defn RenameStep
  "The step name field and Rename (§6.5.7.2)."
  [user-id concept]
  (e/client
    (dom/div
      (dom/props {:class "wt-edit__row"})
      (let [!draft (atom (:label concept))
            draft (e/watch !draft)
            commands (e/amb
                       (forms/Input! :label/draft draft
                         :class "input wt-edit__name" :aria-label "Step name")
                       (forms/Button! [::rename] :class "btn btn-sm btn-secondary"
                         :label "Rename"))]
        (e/for [[t command] (e/diff-by first (e/as-vec commands))]
          (if (map? command)
            (case (reset! !draft (:label/draft command)) (t))
            (let [label (e/snapshot draft)
                  r (e/server (e/Offload #(rename!* user-id (:id concept) label)))]
              (case r (if (:ok r) (t) (t (:error r)))))))))))

(e/defn StepActions
  "\"I understand\" for the step at `position`, and an Edit step popover
   that holds rename and merge (§6.6.5, §6.5.7.2, §6.5.7.3). Rename and merge
   are rare corrections, so they stay out of the step's toolbar."
  [user-id wid concept merge-options]
  (e/client
    (dom/div
      (dom/props {:class "wt-actions"})
      (UnderstoodButton user-id wid)
      (when concept
        ;; A pick only asks: the merge runs from the confirmation. The pending
        ;; merge names both steps, so the work reads nothing the merge changes.
        (let [!pending (atom nil)
              pending (e/watch !pending)]
          (dom/details
            (dom/props {:class "wt-edit"})
            (let [remove-dismiss! (e/snapshot (install-edit-dismiss! dom/node))]
              (e/on-unmount remove-dismiss!))
            (dom/summary
              (dom/props {:class "btn btn-sm btn-secondary wt-edit__toggle"})
              (dom/text "Edit step"))
            (dom/div
              (dom/props {:class "wt-edit__panel"})
              (RenameStep user-id concept)
              (dom/div
                (dom/props {:class "wt-edit__row"})
                (e/for [[t {:keys [into]}] (forms-ext/Select! :into "" merge-options
                                             :class "input wt-edit__merge"
                                             :aria-label "Merge this step into another step")]
                  (case (reset! !pending
                          {:from (:id concept) :into (parse-long into)
                           :message (merge-message (str (:position concept) ". " (wtext/plain (:label concept)))
                                      (some (fn [[id label]] (when (= id into) label)) merge-options)
                                      (or (:hunk-count concept) 0))})
                    (t))))
              (dom/p (dom/props {:class "wt-edit__hint"}) (dom/text merge-hint))))
          (modal/ConfirmModal !pending "Merge steps?" (:message pending) "Merge" "btn btn-primary"
            (e/fn [{:keys [from into]}]
              (e/server (e/Offload #(merge!* user-id from into))))))))))

(e/defn StepView
  "One concept step: its summary, the canvas, the rail, and the pins
   (§6.6.3, §6.6.4, §6.6.6). Tells the server what it shows."
  [user-id wid repo-id step concept concept-options mode]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          files (e/server (e/Offload #(step-files* rev user-id wid step)))
          paths (e/server (mapv :path files))
          !picked (atom {})
          picked (e/watch !picked)
          path (or (get picked step) (first paths))
          sha (e/server (:sha (first (filter #(= path (:path %)) files))))
          pick-path! (fn [p] (swap! !picked assoc step p))
          !list-open? (atom false)
          list-open? (e/watch !list-open?)
          request (e/watch pin-state/!canvas-request)
          concept-id (:id concept)
          merge-options (into [["" "Merge this step into…"]]
                          (keep (fn [[id label]] (when (not= id (str concept-id)) [id label])))
                          concept-options)]
      (dom/div
        (dom/props {:class "wt-step-view"})
        (dom/div
          (dom/props {:class "wt-step-view__head"})
          (dom/h3 (dom/props {:class "wt-step-view__title" :dir "auto"})
            (dom/text (str (:position concept) ". "))
            (InlineCode (:label concept)))
          ;; The tools share the title's line when it fits, so the head
          ;; takes two lines on a laptop rather than four.
          (dom/div
            (dom/props {:class "wt-step-view__tools"})
            (pin-pane/PinHeaderControls user-id {:repo-id repo-id :path path :sha sha} !list-open?)
            (StepActions user-id wid concept merge-options))
          (dom/p (dom/props {:class "wt-step-view__summary" :dir "auto"})
            (InlineCode (:summary concept))))
        (dom/div
          (dom/props {:class "wt-step-view__main"})
          (canvas/Canvas user-id wid step path mode list-open? !list-open?)
          (canvas/Rail user-id repo-id (e/server (e/diff-by :path files)) path concept-id
            concept-options pick-path!)))
      (e/on-unmount #(bus/retract-ctx! [:walkthrough-docs]))
      (bus/publish-ctx! {:walkthrough-docs (mapv (fn [p] [repo-id p]) paths)})
      (apply-canvas-request! request repo-id paths pick-path!)
      (e/server
        (e/on-unmount #(clear-focus!* user-id repo-id wid step))
        (set-focus!* user-id repo-id {:walkthrough-id wid :position step :path path :sha sha})))))

(e/defn ReadyView
  "A prepared walkthrough: the step bar, then the opener or a step."
  [user-id wid repo-id]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          ov (e/server (e/Offload #(overview* rev user-id wid)))
          step (e/server (or (:step ov) 0))
          concepts (e/server (mapv #(select-keys % [:id :position :label :summary :understood?
                                                    :noise :hunk-count])
                               (:concepts ov)))
          concept-options (e/server (mapv (fn [c] [(str (:id c)) (str (:position c) ". " (wtext/plain (:label c)))])
                                      (:concepts ov)))
          concept (e/server (some #(when (= step (:position %)) %) concepts))
          !mode (atom :previous)
          mode (e/watch !mode)]
      (StepBar user-id wid step (e/server (e/diff-by :position concepts)) mode !mode)
      (dom/div
        (dom/props {:class "wt-ready"})
        (if (and concept (pos? step))
          (StepView user-id wid repo-id step concept concept-options mode)
          (dom/div
            (dom/props {:class "wt-opener-host"})
            (OpenerPanel (e/server (e/diff-by :position concepts)))
            (StepActions user-id wid nil [])
            (e/server
              (e/on-unmount #(clear-focus!* user-id repo-id wid 0))
              (set-focus!* user-id repo-id {:walkthrough-id wid :position 0}))))))))

(e/defn WalkthroughScreen
  "The walkthrough `wid` on the repository node the viewer shows (§6.6.7)."
  [wid]
  (e/client
    (let [user-id dctx/user-id
          repo-id dctx/page-topic-id
          navigate! dctx/navigate!
          rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          ov (e/server (e/Offload #(overview* rev user-id wid)))
          status (e/server (:status ov))
          error (e/server (:error ov))
          display-name (e/server (:display-name ov))
          own-name (e/server (:name ov))
          about (e/server (str (:repo-title ov) " · " (short-sha (:sha-from ov)) " → "
                            (short-sha (:sha-b ov)) " · " (:commit-count ov)
                            (if (= 1 (:commit-count ov)) " commit" " commits")))
          !renaming (atom nil)
          phase (e/server (phase* (e/watch (us/get-atom user-id :preparing-walkthroughs)) wid))]
      (dom/div
        (dom/props {:class "wt-screen" :data-testid "walkthrough-screen" :data-status (or status "")})
        (dom/header
          (dom/props {:class "wt-screen__header"})
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-back"})
            (dom/text "← Repository")
            (dom/On "click" (fn [_] (navigate! :viewer (nav/nav-topic repo-id nil))) nil))
          (dom/h2 (dom/props {:class "wt-screen__title" :dir "auto"})
            (dom/text (or display-name "")))
          (dom/span (dom/props {:class "wt-screen__about" :dir "auto"})
            (dom/text about))
          (dom/button
            (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-screen__rename"})
            (dom/text "Rename")
            (dom/On "click" (fn [_] (reset! !renaming {:id wid :name own-name})) nil)))
        (RenameDialog user-id !renaming)
        (dom/div
          (dom/props {:class "wt-screen__body"})
          (when (= "preparing" status) (PreparingPanel user-id wid phase))
          (when (= "failed" status) (FailedPanel user-id wid error))
          (when (= "ready" status) (ReadyView user-id wid repo-id)))))))
