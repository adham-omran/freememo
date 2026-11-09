(ns freememo.learn-session
  "Active learning session — displays topics one at a time with Next advancement."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.topic-page :refer [TopicPage]]
   [freememo.page-title :refer [PageTitle]]
   [freememo.bibliography-form :as bibform]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.motion :as motion]
   [freememo.rich-text-editor :as editor]
   [freememo.merged-window :as merged-window]
   [freememo.editor-pane :as editor-pane]
   [freememo.reading-anchor :as reading-anchor]
   #?(:clj [freememo.activity :as activity])
   #?(:clj [freememo.db :as db])))

(defn pending-edit
  "The editor's edit that no save has acknowledged yet, or nil.

   Pre : client scope, read at the tap on Next.
   Post: nil, or `{:html :save-scope :edit-id}` for an edit by `user-id`. The
         save scope is the one captured when the edit was made, so the edit is
         written to the topic it was made in.
   Inv : the same test as the auto-save (`merged-window/save-needed?`). An edit
         whose auto-save already landed gives nil, so a Next with no edit sends
         no body."
  [user-id]
  (let [dirty @editor/!dirty-html]
    (when (merged-window/save-needed? dirty user-id @editor/!saved-edit-id nil)
      (select-keys dirty [:html :save-scope :edit-id]))))

(defn mark-edit-saved!
  "Tell the editor that the save Next made has landed, so the auto-save and the
   in-place body load see the edit as saved.

   Post: `editor/!saved-edit-id` is `edit-id` when the editor still holds that
         edit. A newer edit is left to the auto-save. Returns :marked."
  [edit-id]
  (when (and edit-id (= edit-id (:edit-id @editor/!dirty-html)))
    (reset! editor/!saved-edit-id edit-id))
  :marked)

(defn finish-topic!*
  "Save the topic's pending edit, then schedule the topic. Next and Postpone
   call this from `e/Offload`, so neither write holds the session thread.

   Pre : `pending` is nil or a `pending-edit` value. `schedule!` is a thunk
         that writes the topic's next review.
   Post: `{:success true :saved-edit-id id-or-nil}` after both writes. On a
         failure, `{:success false :error text}`: the schedule write did not
         run when the save failed, and one `action-failure` row puts the error
         in the banner strip. The caller then stays on the topic.
   Inv : the save runs before the schedule write, so a failed save never moves
         the session past the edit. Never throws, because an exception would
         cross the `e/server` boundary.

   Body-level reader conditional: Electric compiles the `e/Offload` call site
   on both peers, so the var must exist on CLJS."
  [user-id id pending what schedule!]
  #?(:clj
     (let [fail! (fn [action error]
                   (let [text (or (activity/error-message error) "The edit was not saved.")]
                     (try
                       (activity/record-failure! user-id {:error text :what action :subject-id id})
                       (catch Exception _ nil))
                     {:success false :error text}))]
       (try
         (let [saved (if pending
                       (editor-pane/save-editor-html!*
                         (merge (:save-scope pending)
                           {:user-id user-id :html (:html pending)}))
                       {:success true})]
           (if (:success saved)
             (do (schedule!)
                 {:success true :saved-edit-id (:edit-id pending)})
             (fail! (str "Save before " what) (:error saved))))
         (catch Exception ex
           (fail! what ex))))
     :cljs nil))

(defn advance-topic*
  "Next: save the pending edit, then advance the topic's schedule."
  [user-id id pending]
  #?(:clj (finish-topic!* user-id id pending "Next"
            (fn []
              (db/advance-topic! user-id id)
              (commands/bump! user-id :advance-topic)))
     :cljs nil))

(defn postpone-topic*
  "Postpone: save the pending edit, then postpone the topic by `days`."
  [user-id id days pending]
  #?(:clj (finish-topic!* user-id id pending "Postpone"
            (fn []
              (when (and days (pos? days))
                (db/postpone-topic! user-id id days)
                (commands/bump! user-id :postpone-topic))))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; The Next gate
;; (plans/learn-next-speed/issues/03-next-enabled-when-body-shown.md,
;;  plans/learn-next-speed/issues/08-gate-waits-after-early-body.md)
;;
;; A tap on Next closes the gate. The gate opens when the next topic shows:
;; for a topic whose body is in the editor, when `install-body!` (or a cold
;; editor build) puts that topic's body in. A slow network then cannot let a
;; second tap skip a topic that never appeared, and a fast device has no fixed
;; floor.
;;
;; The minimum blocks a double tap. It applies to the tap, not to the button:
;; a tap counts only when its input time is `next-min-ms` or more after the
;; previous tap (`:at`). A timer cannot hold the minimum, because a long task
;; of the topic switch often starts just after an early body and delays the
;; timer by 300 to 400 ms (ticket 08). The input time of a tap does not move
;; when a long task delays its dispatch.
;;
;; Each mark below is set by a timer of its own tap, after the advance:
;;
;;   :timer? `next-timer-ms` after the advance. It opens a topic of another
;;           kind (PDF, video, audio), which has no "content shown" signal.
;;   :cap?   `next-cap-ms` after the advance. It opens the gate on any topic,
;;           so a body that never arrives cannot hold Next disabled for ever.
;; ---------------------------------------------------------------------------

(def ^:private next-min-ms 150)
(def ^:private next-timer-ms 500)
(def ^:private next-cap-ms 10000)

(defn editor-body-kind?
  "Whether a topic of `kind` shows its body in the editor, so that the Next
   gate opens on the editor's signal. Every other kind keeps the timer."
  [kind]
  (contains? reading-anchor/editor-kinds kind))

(defn next-gate-closed?
  "Whether Next is disabled.

   Pre : `gate` is nil (no tap yet, or a failed Next) or
         `{:id :from :at & marks}`, where `:from` is the tapped topic and `:at`
         the tap's input time. `topic-id` is the topic in view. `shown-id` is
         the topic whose body is in the editor
         (`(:topic-id @editor/!editor-state)`). `body-kind?` is
         `editor-body-kind?` of the topic in view.
   Post: false with no gate and after the cap. Otherwise true until the queue
         has moved past the tapped topic and the new topic shows: its body is
         in the editor, or the timer of another kind has run.
   Inv : a plain fn, so `:disabled` changes only when the answer does. It has
         no minimum: `tap-allowed?` holds the minimum on the tap."
  [gate topic-id shown-id body-kind?]
  (boolean
    (and gate
         (not (:cap? gate))
         (or (= topic-id (:from gate))
             (if body-kind?
               (not= shown-id topic-id)
               (not (:timer? gate)))))))

(defn tap-allowed?
  "Whether a tap on Next at input time `t` counts.

   Pre : the arguments of `next-gate-closed?`, and `t`, the tap's
         `event.timeStamp` (the clock of `:at`).
   Post: true when the gate is open and `t` is `next-min-ms` or more after the
         previous tap's `:at`. So the second tap of a double tap never counts,
         even when the next body is already in or a long task dispatches the
         tap late."
  [gate topic-id shown-id body-kind? t]
  (and (not (next-gate-closed? gate topic-id shown-id body-kind?))
       (or (nil? (:at gate))
           (>= (- t (:at gate)) next-min-ms))))

(defn mark-gate
  "`gate` with `mark` set, when `gate` is still the gate of tap `id`. A timer
   of an earlier tap leaves a later gate as it is."
  [gate id mark]
  (if (and gate (= id (:id gate)))
    (assoc gate mark true)
    gate))

(defn- mark-gate-later!
  "Set `mark` on the gate of tap `id` after `ms`."
  [!gate id mark ms]
  #?(:cljs (js/setTimeout (fn [] (swap! !gate mark-gate id mark)) ms)
     :clj nil))

(defn tap-next!
  "The click handler of Next. It runs before Electric sees the click, so a
   second tap in the same frame finds the gate closed.

   Pre : `t` is the tap's `event.timeStamp`.
   Post: nil when the tap does not count (`tap-allowed?`). Otherwise the gate
         is closed on `topic-id` with `:at t`, and the result is the Next event
         `{:id :topic-id :pending}`."
  [!gate topic-id body-kind? user-id t]
  (when (tap-allowed? @!gate topic-id (:topic-id @editor/!editor-state) body-kind? t)
    (let [id (str (random-uuid))]
      (reset! !gate {:id id :from topic-id :at t})
      {:id id :topic-id topic-id :pending (pending-edit user-id)})))

(defn advanced!
  "The queue has moved past the tapped topic: start the timer of other kinds
   and the cap for the gate of tap `id`."
  [!gate id]
  (mark-gate-later! !gate id :timer? next-timer-ms)
  (mark-gate-later! !gate id :cap? next-cap-ms)
  :advanced)

;; Shared bottom bar with Postpone + Next
(e/defn BottomBar [user-id topic-id kind !queue-idx]
  (e/client
    (let [!show-postpone (atom false)
          show-postpone (e/watch !show-postpone)
          !postpone-days (atom "7")]
      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :justify-content "center"
                            :gap "12px" :padding "6px 16px" :flex-shrink "0"
                            :border-top "1px solid var(--color-border)"}})

        ;; Postpone toggle + input
        (if show-postpone
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"}})
            (dom/input
              (dom/props {:type "number" :min "1" :max "365"
                          :value (e/watch !postpone-days)
                          :class "input"
                          :style {:width "60px" :padding "4px 8px" :border-radius "var(--radius-sm)"}})
              (dom/On "input" (fn [e] (reset! !postpone-days (-> e .-target .-value))) nil))
            (dom/span
              (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text "days"))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "6px 16px"}})
              (dom/text "Go")
              ;; The tap captures the topic and its pending edit, so the
              ;; Offload thunk below never changes while it runs.
              (let [event (dom/On "click"
                            (fn [_]
                              (let [v @!postpone-days]
                                {:id (str (random-uuid))
                                 :days (js/parseInt v)
                                 :topic-id topic-id
                                 :pending (pending-edit user-id)}))
                            nil)
                    [t _error] (e/Token event)]
                (when t
                  (let [result (e/server
                                 (e/Offload
                                   #(postpone-topic* user-id (:topic-id event)
                                      (:days event) (:pending event))))]
                    ;; Empty until the answer, so neither branch mounts before it.
                    (if (:success result)
                      (do (e/on-unmount #(swap! !queue-idx inc))
                          (case (mark-edit-saved! (:saved-edit-id result))
                            (case (e/client (reset! !show-postpone false))
                              (t))))
                      ;; The banner strip shows the error. Stay on the topic.
                      (t (:error result)))))))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:padding "6px 12px"}})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show-postpone false)) nil)))

          ;; Collapsed postpone button
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:padding "8px 20px"}})
            (dom/text "Postpone")
            (let [node dom/node]
              (bus/publish-invoker! :postpone-topic (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :postpone-topic))))
            (dom/On "click" (fn [_] (reset! !show-postpone true)) nil)))

        ;; Next button. Disabled from the tap until the next topic shows (the
        ;; Next gate above), so a second tap cannot skip a topic that never
        ;; appeared. `!editor-state` changes only on an editor build, a body
        ;; install and a teardown, and this bar never writes it.
        (let [!gate (atom nil)
              body-kind? (editor-body-kind? kind)
              busy (next-gate-closed? (e/watch !gate) topic-id
                     (:topic-id (e/watch editor/!editor-state)) body-kind?)]
          (dom/button
            (dom/props {:class "btn btn-primary"
                        :disabled busy
                        :style {:padding "8px 28px" :font-size "15px" :font-weight "600"
                                :opacity (if busy "0.5" "1")
                                :cursor (if busy "not-allowed" "pointer")}})
            (dom/text "Next")
            (let [node dom/node]
              (bus/publish-invoker! :advance-topic (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :advance-topic))))
            ;; The tap captures the topic and its pending edit. Next saves
            ;; that edit first, then schedules the topic, in one Offload
            ;; (plans/learn-next-speed/issues/01-next-saves-pending-edit-first.md).
            ;; The Offload thunk reads only the event, so it never changes
            ;; while it runs. `tap-next!` closes the gate in the click
            ;; handler itself, so a double tap advances one topic.
            (let [event (dom/On "click"
                          (fn [e] (tap-next! !gate topic-id body-kind? user-id (.-timeStamp e)))
                          nil)
                  [t _error] (e/Token event)]
              (when t
                (let [result (e/server
                               (e/Offload
                                 #(advance-topic* user-id (:topic-id event) (:pending event))))]
                  ;; Empty until the answer, so neither branch mounts before it.
                  (if (:success result)
                    (do (e/on-unmount #(do (swap! !queue-idx inc)
                                         (advanced! !gate (:id event))))
                        (case (mark-edit-saved! (:saved-edit-id result))
                          (case (e/client (log/log-debug (str "Session advancing idx=" (inc @!queue-idx))))
                            (t))))
                    ;; The banner strip shows the error. Stay on the topic.
                    (case (reset! !gate nil)
                      (t (:error result)))))))))))))

(defn learn-queue-ctx
  "The queue context TopicPage gets for a Learn topic. A plain fn so that a
   PDF/text switch changes this map in place. A reactive `if` there shrinks one
   branch and grows the other, and TopicPage rebuilds its whole viewer on
   that (see topic-state/topic-facts).

   :origin :learn marks every learn topic so mobile reading-mode (TopicPage)
   can strip chrome on a phone, PDFs included, since a book PDF is the main
   thing read in a session. :on-done! (auto-advance on extract-done) is for
   non-PDF topics only: PDFs have no extract Done button and advance manually
   via Next."
  [is-pdf? !queue-idx]
  (if is-pdf?
    {:origin :learn}
    {:origin :learn :on-done! #(swap! !queue-idx inc)}))

(e/defn LearnSession [user-id enc-key queue-vec !queue-idx navigate! llm-enabled?]
  (e/client
    (let [idx (e/watch !queue-idx)
          ;; queue-vec is a server-sited value (form binding in ViewerContent)
          ;; — consume it only inside (e/server ...) so the full queue never
          ;; crosses the wire; one row crosses per review step.
          total (e/server (count queue-vec))
          branch-done? (>= idx total)]
      (dom/div
        (dom/props {:style {:height "100%" :display "flex" :flex-direction "column" :overflow "hidden"}})

        (if branch-done?
          ;; All done
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :flex-direction "column"
                                :align-items "center" :justify-content "center" :gap "16px"}})
            (dom/div
              (dom/props {:style {:font-size "24px" :color "var(--color-text-primary)"}})
              (dom/text "All caught up!"))
            (dom/div
              (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)"}})
              (dom/text (str "Reviewed " total " topic" (when (not= total 1) "s") ".")))
            (dom/div
              (dom/props {:style {:font-size "13px" :color "var(--color-text-hint)" :margin-top "4px"}})
              (dom/text "Return to Overview to browse your topics."))
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:padding "10px 28px" :font-size "15px" :font-weight "600"}})
              (dom/text "Back to Overview")
              (let [event (dom/On "click" (fn [_] :back) nil)
                    [t _error] (e/Token event)]
                (when t
                  (case (t) (navigate! :learn)))))
            (PageTitle user-id [:label :learn]))

          ;; Active topic
          ;; All three reads of `item` are server-sited. The row carries the
          ;; topic's content prefix, so a client-scope read of any one of them
          ;; pulled that text across on every review step — and only the kind
          ;; and the id are ever used here.
          (let [item (e/server (nth queue-vec idx nil))
                item-present? (e/server (some? item))
                kind (e/server (:topics/kind item))
                topic-id (e/server (:topics/id item))
                queue-ctx (learn-queue-ctx (= kind "pdf") !queue-idx)]

            (when item-present?
              (dom/div
                (dom/props {:style {:flex "1" :min-height "0" :display "flex" :flex-direction "column" :overflow "hidden"}})
                (dom/div
                  (dom/props {:style {:flex "1" :min-height "0" :overflow "hidden"}})
                  (TopicPage user-id enc-key topic-id
                    navigate!
                    llm-enabled?
                    queue-ctx nil)
                  ;; TopicPage stays mounted across items, so the next topic is
                  ;; a new key on this node, not a mount. The bar below stays
                  ;; still. Last form: an effect gates no frame.
                  (motion/on-new-key! dom/node topic-id motion/arrive-forward!))
                (BottomBar user-id topic-id kind !queue-idx)))
            ;; The session owns the title. TopicPage never writes one.
            (PageTitle user-id [:topic topic-id])))))))
