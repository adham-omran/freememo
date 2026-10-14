(ns freememo.question-pin-pane
  "Question pins on a repository file (plans/anchored-questions.md §6.2,
   §6.5): the pin layer, the pin popover, the pin list, and a pane's header
   controls. The file is named by a REF SPEC, {:topic-id} for a snapshot topic
   or {:repo-id :path :sha} for a file at a commit a walkthrough shows
   (plans/commit-range-walkthrough.md §6.3). The server resolves and checks it.

   The rendered pane's `div` takes its children from `innerHTML`, and a
   block's index among them is half of every location the server reports. So
   this namespace adds NO Electric child inside that `div`. `PinLayer` mounts
   beside it, and everything it shows over the text is plain DOM appended to
   `document.body` (`freememo.question-pin-dom`).

   Its own namespace for the 64KB method cap: Electric inlines same-namespace
   e/defn calls into the caller's method, and `freememo.code-pane` is close
   to it."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.question-pin-state :as pin-state]
   #?(:cljs [freememo.question-pin-dom :as pin-dom])
   #?(:clj [freememo.document-ref :as docref])
   #?(:clj [freememo.question-map :as qm])
   #?(:clj [freememo.user-state :as us])))

;; ── Client wrappers ────────────────────────────────────────────────
;; Called from Electric bodies, so each var exists on both peers.

(defn- install-layer!
  "Attach the pin layer to `node`, whose text model is `model` (:blocks or
   :source). Post: the layer handle."
  [node model !popover]
  #?(:cljs (pin-dom/install! node
             {:model model
              :on-ask (fn [{:keys [quote anchor offset rect]}]
                        (reset! !popover {:id (random-uuid) :mode :new :quote quote
                                          :anchor anchor :offset offset :rect rect}))
              :on-open-pin (fn [pin rect]
                             (reset! !popover {:id (random-uuid) :mode :edit
                                               :pin pin :rect rect}))})
     :clj nil))

(defn- teardown-layer! [handle]
  #?(:cljs (when handle (pin-dom/teardown! handle)) :clj nil))

(defn- show-pins! [handle pins]
  #?(:cljs (when handle (pin-dom/set-pins! handle pins)) :clj nil))

(defn- show-finger! [handle location scroll?]
  #?(:cljs (when handle (pin-dom/set-finger! handle location scroll?)) :clj nil))

(defn- ask-current! [handle]
  #?(:cljs (when handle (pin-dom/ask-current! handle)) :clj nil))

(defn- popover-style
  "Viewport position of the popover for selection `rect`: below it, kept
   inside the window."
  [{:keys [top bottom left]}]
  #?(:cljs (let [width 320
                 vw (.-innerWidth js/window)
                 vh (.-innerHeight js/window)
                 x (max 8 (min left (- vw width 8)))
                 y (if (> (+ bottom 8 140) vh) (max 8 (- top 8 140)) (+ bottom 8))]
             {:left (str (js/Math.round x) "px") :top (str (js/Math.round y) "px")})
     :clj nil))

(defn- input-value
  "The popover field's text as typed now. Read from the DOM so an Enter or an
   outside click sees the last keystroke even before Forms5 has committed it."
  [node]
  #?(:cljs (some-> (.querySelector node ".question-pin-popover__input") .-value str/trim)
     :clj nil))

(defn- click-save! [node]
  #?(:cljs (some-> (.querySelector node ".question-pin-popover__save") .click) :clj nil))

(defn- popover-keydown!
  "Enter without Shift saves. Escape cancels and goes no further, so an
   enclosing handler does not see it."
  [e node !popover]
  #?(:cljs (case (.-key e)
             "Enter" (when-not (.-shiftKey e) (.preventDefault e) (click-save! node))
             "Escape" (do (.preventDefault e) (.stopPropagation e) (reset! !popover nil))
             nil)
     :clj nil))

(defn- install-outside-click!
  "An outside click saves when the field holds text or the pin already
   exists, and cancels otherwise, so a stray click never creates an empty pin
   (§6.2.2). Post: the teardown fn."
  [node mode !popover]
  #?(:cljs (let [f (fn [e]
                     (when-not (.contains node (.-target e))
                       (if (or (= :edit mode) (seq (input-value node)))
                         (click-save! node)
                         (reset! !popover nil))))]
             (.addEventListener js/document "mousedown" f true)
             #(.removeEventListener js/document "mousedown" f true))
     :clj (fn [])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn snapshot-view* [_rev user-id spec]
  #?(:clj (qm/snapshot-view user-id (docref/resolve-spec user-id spec)) :cljs nil))

(defn pin-counts* [_rev user-id spec]
  #?(:clj (qm/pin-counts user-id (docref/resolve-spec user-id spec)) :cljs nil))

(defn doc-key*
  "The document key [repo-id path] of `spec`, or nil."
  [user-id spec]
  #?(:clj (some-> (docref/resolve-spec user-id spec) docref/doc-key) :cljs nil))

(defn locate-for-finger* [user-id spec finger]
  #?(:clj (when finger
            (qm/locate-for-finger user-id (docref/resolve-spec user-id spec)
              (:source-doc finger) (:quote finger) (:anchor finger)))
     :cljs nil))

(defn create-pin!* [user-id spec passage]
  #?(:clj (qm/create-pin! user-id (docref/resolve-spec user-id spec) passage) :cljs nil))

(defn edit-pin!* [user-id pin-id edit]
  #?(:clj (qm/edit-pin! user-id pin-id edit) :cljs nil))

;; ── Commands ───────────────────────────────────────────────────────

#?(:cljs
   ;; Available while a rendered pane publishes open pins (§6.6.2). The pane's
   ;; publication is what scopes the command, not the tab.
   (bus/register-preparer! :ask-question-pins
     (fn [ctx] (when (pos? (or (:question-open-pins ctx) 0)) {}))))

;; Available while a rendered pane is mounted: a file it shows can have a
;; question map (plans/question-map-tab.md §4.6.1.2).
#?(:cljs
   (bus/register-preparer! :show-question-map
     (fn [ctx] (when (:question-pane-ref ctx) {}))))

(defn ask-open-pins!
  "Start an Ask over the open pins, from a button. Post: the dispatch outcome."
  []
  (bus/dispatch! :ask-question-pins nil :button))

;; ── Components ─────────────────────────────────────────────────────

(defn- excerpt [s n]
  (let [s (str/replace (str s) #"\s+" " ")]
    (if (> (count s) n) (str (subs s 0 (- n 1)) "…") s)))

(e/defn PinPopover
  "Create a pin from `popover` {:mode :new :quote :anchor :rect}, or edit the
   pin of {:mode :edit :pin :rect} (§6.2.2). `!popover` closes it when nil.
   A Forms5 field and buttons; Enter saves, Escape cancels, and an outside
   click follows `install-outside-click!`."
  [user-id spec popover !popover]
  (e/client
    (let [{:keys [mode pin rect]} popover
          edit? (= :edit mode)]
      (dom/div
        (dom/props {:class "question-pin-popover" :role "dialog"
                    :aria-label (if edit? (str "Pin " (:label pin)) "New pin")
                    :style (popover-style rect)})
        (let [node dom/node
              !error (atom nil)
              error (e/watch !error)
              ;; Forms5 shows `v` again when a commit's token spends, so the
              ;; field's value has to follow every commit.
              !draft (atom (if edit? (or (:question pin) "") ""))
              draft (e/watch !draft)]
          (dom/On "keydown" (fn [e] (popover-keydown! e node !popover)) nil)
          (e/on-unmount (install-outside-click! node mode !popover))
          (dom/div
            (dom/props {:class "question-pin-popover__quote" :dir "auto"})
            (dom/text (str (if edit? (str "Pin " (:label pin) ": ") "")
                        "“" (excerpt (if edit? (:quote pin) (:quote popover)) 90) "”")))
          (dom/div
            (dom/props {:class "question-pin-popover__error" :role "alert"
                        :style {:display (if error "block" "none")}})
            (dom/text (or error "")))
          (let [commands
                (e/amb
                  (forms/Input! :pin/question draft
                    :as :textarea :rows 2 :dir "auto" :autofocus true
                    :class "input question-pin-popover__input"
                    :placeholder "Your question (optional). Enter to pin."
                    :aria-label "Question about this passage")
                  (dom/div
                    (dom/props {:class "question-pin-popover__buttons"})
                    (e/amb
                      (if edit?
                        (forms/Button! [::delete]
                          :class "btn btn-sm btn-secondary" :label "Delete")
                        (e/amb))
                      (forms/Button! [::save]
                        :class "btn btn-sm btn-primary question-pin-popover__save"
                        :label (if edit? "Save" "Pin")))))]
            (e/for [[t command] (e/diff-by first (e/as-vec commands))]
              (cond
                (map? command) (case (reset! !draft (:pin/question command)) (t))

                (= [::save] command)
                (let [question (e/snapshot (or (input-value node) ""))
                      result (e/server
                               (e/Offload
                                 #(if edit?
                                    (edit-pin!* user-id (:id pin)
                                      {:op :question :text question})
                                    (create-pin!* user-id spec
                                      {:quote (:quote popover) :anchor (:anchor popover)
                                       :offset (:offset popover) :question question}))))]
                  (case result
                    (if (:error result)
                      (case (reset! !error (:error result)) (t))
                      (do (e/on-unmount #(reset! !popover nil))
                          (t)))))

                (= [::delete] command)
                (let [result (e/server
                               (e/Offload #(edit-pin!* user-id (:id pin) {:op :delete})))]
                  (case result
                    (if (:error result)
                      (case (reset! !error (:error result)) (t))
                      (do (e/on-unmount #(reset! !popover nil))
                          (t)))))))))))))

(e/defn PinPopoverHost
  "The popover for `popover`, or nothing. Keyed by (:id popover), so a new
   Ask or a click on another badge replaces the open popover. The host element
   is always mounted and holds only the popover."
  [user-id spec popover !popover]
  (e/client
    (dom/div
      (dom/props {:class "question-pin-popover-host"})
      (e/for-by identity [_k (if popover [(:id popover)] [])]
        (PinPopover user-id spec popover !popover)))))

(e/defn PinRow
  "One row of the pin list. Post: its button commands."
  [pin handle]
  (e/client
    (dom/li
      (dom/props {:class (str "question-pin-list__row question-pin-list__row--" (:group pin))})
      (dom/span
        (dom/props {:class "question-pin-list__label"})
        (dom/text (str (:label pin))))
      (dom/span
        (dom/props {:class "question-pin-list__text" :dir "auto"})
        (dom/text (if (seq (:question pin))
                    (excerpt (:question pin) 80)
                    (str "“" (excerpt (:quote pin) 80) "”"))))
      (when (:range pin)
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                      :aria-label (str "Scroll to pin " (:label pin))})
          (dom/text "Show")
          (dom/On "click" (fn [_] (show-finger! handle (:range pin) true)) nil)))
      (dom/span
        (dom/props {:class "question-pin-list__actions"})
        (e/amb
          (forms/Button! [::status (:id pin) (if (= "open" (:status pin)) "closed" "open")]
            :class "btn btn-sm btn-secondary"
            :label (if (= "open" (:status pin)) "Close" "Reopen"))
          (forms/Button! [::delete-row (:id pin)]
            :class "btn btn-sm btn-secondary" :label "Delete"))))))

(defn- grouped
  "Pins with their list group: `open` and `closed` when anchored here, and
   `unmatched` when the quote does not match this snapshot (§6.1.3), whatever
   the lifecycle. Order: open, closed, unmatched, each by label."
  [pins]
  (->> pins
    (map #(assoc % :group (cond (nil? (:range %)) "unmatched"
                                (= "open" (:status %)) "open"
                                :else "closed")))
    (sort-by (juxt #({"open" 0 "closed" 1 "unmatched" 2} (:group %)) :label))
    vec))

(e/defn PinList
  "Every pin of the document key, grouped (§6.2.6). Shown while `open?`."
  [user-id pins open? !list-open? handle]
  (e/client
    (dom/div
      (dom/props {:class "question-pin-list-host"})
      (when open?
        (dom/div
          (dom/props {:class "question-pin-list" :role "dialog" :aria-label "Pins"})
          (dom/On "keydown" (fn [e] (when (= "Escape" (.-key e)) (reset! !list-open? false))) nil)
          (dom/div
            (dom/props {:class "question-pin-list__header"})
            (dom/text "Pins")
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :type "button" :aria-label "Close pins"})
              (dom/text "✕")
              (dom/On "click" (fn [_] (reset! !list-open? false)) nil)))
          (dom/div
            (dom/props {:class "question-pin-list__empty"
                        :style {:display (if (seq pins) "none" "block")}})
            (dom/text "No pins yet. Select a passage and choose Ask."))
          (dom/ul
            (dom/props {:class "question-pin-list__rows"})
            (let [commands (e/for [pin (e/diff-by :id (grouped pins))]
                             (PinRow pin handle))]
              (e/for [[t command] (e/diff-by first (e/as-vec commands))]
                (let [[kind pin-id status] command
                      result (e/server
                               (e/Offload
                                 #(edit-pin!* user-id pin-id
                                    (if (= ::status kind)
                                      {:op :status :status status}
                                      {:op :delete}))))]
                  (case result (t)))))))))))

(e/defn PublishPinCommands
  "Make the pin commands available while this pane is mounted (§6.6.2), and
   tell the assistant panel which file the pane shows: its document key, so a
   citation of that document scrolls to its passage (§6.5.4), and its ref
   spec, so the map card reads the same file."
  [handle spec doc-key open-count]
  (e/client
    (bus/publish-invoker! :ask-about-selection (fn [] (ask-current! handle)))
    (e/on-unmount (fn []
                    (bus/retract-invoker! :ask-about-selection)
                    (bus/retract-ctx! [:question-open-pins :question-pane-doc :question-pane-ref])))
    (bus/publish-ctx! {:question-open-pins open-count :question-pane-doc doc-key
                       :question-pane-ref spec})))

(e/defn Finger
  "Outline the passage the assistant panel points at (§6.5.1)."
  [user-id spec handle]
  (e/client
    (let [finger (e/watch pin-state/!finger)
          location (e/server (when finger
                               (e/Offload #(locate-for-finger* user-id spec finger))))]
      (show-finger! handle location (boolean (:scroll? finger))))))

(e/defn PinLayer
  "Pins, the popover, the pin list and the finger for pane `node` showing the
   file `spec` names. `model` is the pane's text model, :blocks for rendered
   Markdown and :source for code. Mounted BESIDE the pane's element, never
   inside it: the layer adds no node to the pane."
  [user-id spec node model list-open? !list-open?]
  (e/client
    ;; Keyed by pane AND file at a commit (§6.3.6 of
    ;; plans/commit-range-walkthrough.md): a pane stays mounted when the route
    ;; or the step moves to another file, and the layer, its popover and its
    ;; pins belong to one.
    (e/for-by identity [_k [[node spec model]]]
      (let [!popover (atom nil)
            popover (e/watch !popover)
            handle (install-layer! node model !popover)
            rev (e/server (e/watch (us/get-atom user-id :question-mutations)))
            ;; SERVER-FORM binding: the view stays on the server, and only the
            ;; pins cross, with their locations in this file.
            view (e/server (e/Offload #(snapshot-view* rev user-id spec)))
            pins (e/server (vec (:pins view)))
            open-count (e/server (or (:open-count view) 0))
            doc-key (e/server (e/Offload #(doc-key* user-id spec)))]
        (e/on-unmount #(teardown-layer! handle))
        (PublishPinCommands handle spec doc-key open-count)
        (Finger user-id spec handle)
        (PinPopoverHost user-id spec popover !popover)
        (PinList user-id pins list-open? !list-open? handle)
        (show-pins! handle pins)))))

(e/defn PinHeaderControls
  "A pane header's Pins toggle and Ask button (§6.2.5, §6.2.6)."
  [user-id spec !list-open?]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :question-mutations)))
          counts (e/server (e/Offload #(pin-counts* rev user-id spec)))
          open (e/server (:open counts 0))
          total (e/server (:total counts 0))]
      (dom/span
        (dom/props {:class "question-pin-controls"})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                      :aria-label "Show pins"})
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (str "Pins" (when (pos? total) (str " " total)))))
          (dom/On "click" (fn [_] (swap! !list-open? not)) nil))
        (dom/button
          ;; Static props apart from the one that follows a server read: a
          ;; props map that goes absent for a round trip nulls every prop in
          ;; it (CLAUDE.md, "An Absent Props Map Nulls Every Prop").
          (dom/props {:class "btn btn-sm btn-primary" :type "button"
                      :aria-label "Ask the tutor about every open pin"})
          (dom/props {:style {:display (if (pos? open) "inline-flex" "none")}})
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (str "Ask " open " pin" (when (not= 1 open) "s"))))
          (dom/On "click" (fn [_] (ask-open-pins!)) nil))))))
