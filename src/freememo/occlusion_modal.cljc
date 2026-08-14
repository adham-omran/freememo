(ns freememo.occlusion-modal
  "Image-occlusion authoring modal: Masks Editor tab (Konva — mask rects plus
   the text labels a user places on an unlabelled image, so there is something
   to occlude) + Fields tab (the six FreeMemo IO text fields). Create mode ends with two
   generate actions — Hide All, Guess One / Hide One, Guess One — each saving
   one card per mask through the optimistic command queue (:add-occlusion).
   Edit mode reopens a whole group and saves a full reconcile
   (:update-occlusion); Anki notes of removed masks are deleted fire-and-forget
   from the client, like the card delete flow.

   Driven by a request atom:
     {:mode :create :image-media-id N :topic-id N :root-topic-id N}
     {:mode :edit   :group-id N}
   The host mounts (OcclusionModal !request user-id) while the atom is
   non-nil; closing resets it to nil."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.modal-shell :as modal]
   [freememo.occlusion-svg :as osvg]
   [freememo.quill-field :refer [QuillField]]
   [freememo.card-modals :refer [attach-modal-tab-nav!]]
   [freememo.card-history-modal :refer [CardHistoryModal]]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.card-components :refer [try-delete-anki-notes!]]
   [freememo.occlusion-ordinals :as ord]
   [freememo.tooltip :as tooltip]
   #?(:cljs [freememo.occlusion-editor :as occ-editor])
   #?(:cljs [freememo.vendor-libs :as vendor])
   #?(:clj [freememo.occlusion :as occ])
   [freememo.optimistic :as opt]
   #?(:clj [freememo.settings :as settings])))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in the
;; e/defn reactive bodies (CLJ/CLJS frame signal-count rule).
;; ---------------------------------------------------------------------------

(defn get-group-for-edit* [user-id group-id]
  #?(:clj (occ/get-group-for-edit user-id group-id)
     :cljs nil))

(defn enqueue-add-occlusion!* [id payload]
  (opt/enqueue-pending! :add-occlusion id payload))

(defn duplicate-group!* [user-id payload]
  #?(:clj (occ/duplicate-group! user-id payload)
     :cljs nil))

(defn enqueue-update-occlusion!* [payload]
  (opt/enqueue! {:type :update-occlusion :payload payload}))

(defn get-card-font-size* [user-id]
  #?(:clj (settings/get-card-font-size user-id)
     :cljs nil))

(defn init-occlusion-editor!
  "Deferred Konva init (container must be in the DOM first) — mirrors
   quill_field/schedule-quill-init!.
   Pre:  !tool holds the starting tool — :draw when creating (the first act is
         drawing), :select when editing (Anki's MaskEditor rule). It is read at
         init time, so a re-init resumes the user's current tool.
   Post: the editor writes tool switches back through on-tool-change, making
         !tool the single source of truth."
  [!handle container image-media-id rects elements !tool on-change
   on-tool-change on-selection-change on-label-selected]
  ;; setTimeout 0 lets Electric finish mounting the container; vendor/with!
  ;; then waits for Konva, which carries no <script> tag of its own.
  #?(:cljs (do (js/setTimeout
                 (fn []
                   (vendor/with! :konva
                     (fn []
                       (reset! !handle
                         (occ-editor/init! {:container container
                                            :image-url (str "/api/media/" image-media-id)
                                            :rects rects
                                            :elements elements
                                            :tool @!tool
                                            :on-change on-change
                                            :on-tool-change on-tool-change
                                            :on-selection-change on-selection-change
                                            :on-label-selected on-label-selected})))))
                 0)
             nil)
     :clj nil))

(defn set-editor-tool!
  "Switch the editor's tool. The editor echoes the change back through
   on-tool-change, so !tool needs no write here. No-op before the stage exists."
  [!handle tool]
  #?(:cljs (do (occ-editor/set-tool! @!handle tool) nil)
     :clj nil))

(defn group-editor-selection! [!handle]
  #?(:cljs (do (occ-editor/group-selection! @!handle) nil)
     :clj nil))

(defn ungroup-editor-selection! [!handle]
  #?(:cljs (do (occ-editor/ungroup-selection! @!handle) nil)
     :clj nil))

(defn delete-editor-selection! [!handle]
  #?(:cljs (do (occ-editor/delete-selection! @!handle) nil)
     :clj nil))

(defn set-editor-label-size! [!handle size]
  #?(:cljs (do (occ-editor/set-label-size! @!handle size) nil)
     :clj nil))

(defn destroy-occlusion-editor! [!handle]
  #?(:cljs (do (occ-editor/destroy! @!handle)
             (reset! !handle nil)
             nil)
     :clj nil))

(defn editor-rects
  "Save-time authoritative rect read; falls back to the mount-time rects when
   the editor never came up (e.g. image failed to load)."
  [!handle fallback]
  #?(:cljs (or (occ-editor/read-rects @!handle) fallback)
     :clj fallback))

(defn editor-elements
  "Save-time authoritative label read; falls back to the mount-time elements
   when the editor never came up."
  [!handle fallback]
  #?(:cljs (or (occ-editor/read-elements @!handle) fallback)
     :clj fallback))

(defn editor-geometry
  "{:width :height :rects :elements} in natural px, or the fallback (edit mode's
   loaded geometry) when the editor has no image dimensions.
   Post: :elements is always a vector. The server drops an empty one, so an
         unlabelled group still stores exactly the map it stored before labels
         existed — and reloads as [] here, matching an empty editor read."
  [!handle fallback-geometry]
  #?(:cljs (if-let [h @!handle]
             (let [{:keys [natural]} @h
                   rects (occ-editor/read-rects h)]
               (if (and natural rects)
                 {:width (:width natural) :height (:height natural)
                  :rects rects
                  :elements (vec (occ-editor/read-elements h))}
                 fallback-geometry))
             fallback-geometry)
     :clj fallback-geometry))

(defn occlusion-dirty?
  "True when masks, labels or text fields differ from their loaded state.
   Labels are read here for the same reason masks are: without it, placing a
   label and pressing Cancel discards it with no prompt."
  [!handle initial-rects initial-elements !fields initial-fields]
  #?(:cljs (or (not= (editor-rects !handle initial-rects) initial-rects)
             (not= (editor-elements !handle initial-elements) initial-elements)
             (not= @!fields initial-fields))
     :clj false))

(defn confirm-discard! []
  #?(:cljs (js/confirm "Discard unsaved occlusion changes?")
     :clj true))

(def empty-io-fields
  {:header "" :footer "" :remarks "" :sources "" :extra1 "" :extra2 ""})

(defn parse-label-size
  "The font size a size input's text names, or nil when it names none.
   Post: nil for blank or non-integral text, which set-label-size! then ignores —
         so a half-typed value leaves the label alone rather than clamping it to
         whatever the first digit happened to mean.
   Plain defn with no reader conditional: parse-long exists on both peers, and
   this is called from a reactive body."
  [s]
  (when (string? s) (parse-long s)))

(defn mask-summary
  "\"5 masks → 3 cards\" — masks are rects, cards are mask groups, and grouping
   is what makes the two numbers differ."
  [rects]
  (let [masks (count rects)
        cards (ord/card-count rects)]
    (str masks " mask" (when (not= 1 masks) "s")
      " → " cards " card" (when (not= 1 cards) "s"))))

;; ---------------------------------------------------------------------------
;; Sub-components (split per the JVM 64KB method-limit convention)
;; ---------------------------------------------------------------------------

(e/defn OcclusionToolButton
  "One toolbar button. The effects are local to the browser (Konva state), so
   there is no token — see the selection-side-effect pattern in CLAUDE.md."
  [label active? on-click]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm" :type "button"
                  :style {:background (if active? "var(--color-bg-subtle)" "transparent")
                          :font-weight (if active? "600" "400")
                          :border "1px solid var(--color-border)"}})
      (dom/text label)
      (dom/On "click" (fn [_] (on-click)) nil))))

(e/defn OcclusionDeleteButton
  "Delete the selection — masks, labels, or both. Disabled with nothing selected
   — the editor's own Delete key no-ops there, and a live button that does
   nothing would be a lie. Its own e/defn because it is the one button in this
   bar that reads a second signal."
  [!handle !selection]
  (e/client
    (let [n (e/watch !selection)
          none? (zero? (or n 0))]
      (dom/button
        (dom/props {:class "btn btn-sm" :type "button"
                    :disabled none?
                    :style {:background "transparent"
                            :border "1px solid var(--color-border)"
                            :color (if none? "var(--color-text-hint)" "var(--color-danger-text)")}})
        (tooltip/Tooltip! (if none?
                            "Select a mask or label first"
                            (str "Delete " n " selected — takes effect on Save")))
        (dom/text "Delete (⌫)")
        (dom/On "click" (fn [_] (when-not none? (delete-editor-selection! !handle))) nil)))))

(e/defn OcclusionLabelSizeInput
  "Font size of the selected label, in natural image pixels. Disabled with no
   label selected, because there is nothing to resize.

   Forms5's MANAGED `Input` (no bang), which is the right primitive for a live
   local control and gives three things a hand-rolled input has to reinvent:

   - It reflects `size` as the authoritative value, so a drag-resize updates the
     number without this component tracking anything.
   - It guards that write with dom/Focused?, so it never overwrites what the user
     is typing. That is what removes the e/for-by frame-isolation dance an
     imperative (set! (.-value …)) needed.
   - Its dom/On carries `size` in the authoritative lane, so an external change
     RESETS the returned value. Without that the value latches and re-typing the
     number the label last held is a silent no-op.

   `Input!` (bang) is the wrong one here: it holds its token until a form
   commits, and there is no form and no commit — this writes Konva state, with
   nothing to acknowledge, retry or fail. Tracked fields belong in a Form!.

   The bounds come from freememo.occlusion-svg, which the editor also clamps to.
   That namespace has no reader conditional, so both peers see plain literals."
  [!handle !label-size]
  (e/client
    (let [size (e/watch !label-size)]
      (dom/label
        (dom/props {:style {:display "flex" :align-items "center" :gap "4px"
                            :font-size "12px" :color "var(--color-text-secondary)"}})
        (dom/text "Size")
        ;; Last form of the body, so the resize IS this component's return value
        ;; and cannot be optimized away as an unused expression.
        (set-editor-label-size! !handle
          (parse-label-size
            (forms/Input (or size "")
              :type "number" :step 1
              :min osvg/min-label-size :max osvg/max-label-size
              :disabled (nil? size)
              :style {:width "56px" :font-size "12px" :padding "2px 4px"})))))))

(e/defn OcclusionMaskToolbar
  "Tool switch plus mask-group actions, mirroring the editor's own shortcuts.
   Watches !tool and !selection HERE, not in the parent: the parent's body hosts
   the Konva init side effect, and a signal there would put that call in a body
   Electric may re-evaluate (CLAUDE.md, JS library init). !tool is the single
   source of truth for the tool — the editor writes it back on a keyboard
   switch — and !selection is written the same way, from select-nodes!.
   !label-size is watched one level down, inside OcclusionLabelSizeInput, for the
   same reason: every signal this bar reads must be read below the editor host."
  [!handle !tool !selection !label-size]
  (e/client
    (let [tool (e/watch !tool)]
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :align-items "center"
                            :margin-bottom "var(--sp-2)" :flex-wrap "wrap"}})
        (OcclusionToolButton "Select (S)" (= tool :select)
          (fn [] (set-editor-tool! !handle :select)))
        (OcclusionToolButton "Draw (R)" (= tool :draw)
          (fn [] (set-editor-tool! !handle :draw)))
        (OcclusionToolButton "Text (T)" (= tool :text)
          (fn [] (set-editor-tool! !handle :text)))
        (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))
        (OcclusionToolButton "Group (G)" false
          (fn [] (group-editor-selection! !handle)))
        (OcclusionToolButton "Ungroup (U)" false
          (fn [] (ungroup-editor-selection! !handle)))
        (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))
        (OcclusionDeleteButton !handle !selection)
        (OcclusionLabelSizeInput !handle !label-size)))))

(e/defn OcclusionMaskSummary
  "\"N masks → M cards\". Own component so the !rects watch lives outside the
   editor-host body — same reason as OcclusionMaskToolbar."
  [!rects]
  (e/client
    (dom/div
      (dom/props {:style {:font-size "12px" :color "var(--color-text-hint)"
                          :margin-top "var(--sp-1)"}})
      (dom/text (mask-summary (e/watch !rects))))))

(e/defn OcclusionMasksTab
  "Konva editor host. Mounted once; hidden (not unmounted) when the Fields tab
   is active so drawn masks survive tab switches.
   Invariant: this body depends on NO signal, so the init-occlusion-editor! call
   below cannot be re-evaluated — every reactive value lives in a child
   component. Adding an (e/watch …) here would risk a second Konva stage."
  [!handle image-media-id initial-rects initial-elements !rects !tool !selection
   !label-size]
  (e/client
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (OcclusionMaskToolbar !handle !tool !selection !label-size)
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :margin-bottom "var(--sp-2)"}})
        (dom/text "Text (T): click to place a label, then type; double-click a label to edit it. Draw (R): drag on the image to add a mask — occlude a label by drawing a mask over it in this tool. Select (S): click a mask or label, Shift-click or drag a box to add more; drag to move, handles to resize. Delete (⌫ or the button) removes the selection. Group (G) makes the selected masks one card; Ungroup (U) splits them."))
      (dom/div
        (dom/props {:class "occlusion-editor-host"
                    :style {:min-height "200px" :max-height "58vh"
                            :overflow "auto" :background "var(--color-bg-subtle)"
                            :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-sm)"}})
        (let [host dom/node
              on-change (fn [rects] (reset! !rects rects))
              on-tool-change (fn [tool] (reset! !tool tool))
              on-selection-change (fn [n] (reset! !selection n))
              ;; The editor reports this on selection AND on resize, so the atom
              ;; is the live size of the selected label — the authoritative value
              ;; the size input reflects.
              on-label-selected (fn [size] (reset! !label-size size))]
          ;; The editor reads its starting tool from !tool, so a re-init (or a
          ;; remount) resumes the user's tool instead of resetting it.
          (init-occlusion-editor! !handle host image-media-id initial-rects
            initial-elements !tool on-change on-tool-change on-selection-change
            on-label-selected)
          (e/on-unmount (fn [] (destroy-occlusion-editor! !handle)))))
      (OcclusionMaskSummary !rects))))

(e/defn OcclusionFieldRow [label placeholder k initial-fields !fields field-key modal-font]
  (e/client
    (dom/label (dom/text label))
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
      (QuillField (or (get initial-fields k) "")
        (fn [html] (swap! !fields assoc k html))
        placeholder [k field-key] nil nil nil))))

(e/defn OcclusionFieldsTab
  "The six FreeMemo IO text fields. Values live in the shared !fields atom so
   they survive tab switches and feed the save payload."
  [initial-fields !fields field-key modal-font]
  (e/client
    (OcclusionFieldRow "Header:" "Shown above the image..." :header initial-fields !fields field-key modal-font)
    (OcclusionFieldRow "Footer:" "Shown below the image..." :footer initial-fields !fields field-key modal-font)
    (OcclusionFieldRow "Remarks:" "Back-side remarks..." :remarks initial-fields !fields field-key modal-font)
    (OcclusionFieldRow "Sources:" "Back-side sources..." :sources initial-fields !fields field-key modal-font)
    (OcclusionFieldRow "Extra 1:" "Back-side extra..." :extra1 initial-fields !fields field-key modal-font)
    (OcclusionFieldRow "Extra 2:" "Back-side extra..." :extra2 initial-fields !fields field-key modal-font)))

(e/defn OcclusionDuplicateButton
  "Duplicate — commit this group's edits, then create a second group over the
   same image and reopen the editor on it.

   Pre : edit mode only. `group` is the loaded source group, `!handle` the live
         Konva editor, `!fields` the IO text fields, `initial-*` the values the
         editor mounted with.
   Post: !request holds {:mode :edit :group-id <the copy>}, so the modal is
         editing the copy and the source is left saved and closed.
   Invariant: the source is committed FIRST, and only when dirty. The copy
         takes the editor buffer, so without that write the source would silently
         lose the very edits the copy keeps.

   The word is Duplicate, not Create copy…, because this writes on click while
   the text modal opens an unsaved draft. One label for two commitments would
   promise the wrong thing on one of them."
  [group !request !handle !fields initial-rects initial-elements initial-geometry
   initial-fields user-id]
  (e/client
    (let [[click-event btn-node]
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:order "0"}
                        :data-tooltip "Save this group, then start a second one over the same image"})
            (dom/text "Duplicate")
            ;; Same command as the text modal's Create copy… — see
            ;; commands/:copy-card. Keyed by node, because this button and the
            ;; card modal's can be mounted at the same time.
            (let [node dom/node]
              (bus/publish-invoker! :copy-card node (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :copy-card node))))
            [(dom/On "click" identity nil) dom/node])
          [t ?error] (e/Token click-event)]
      ;; Disable + aria-busy while the duplicate is in flight. e/Token alone
      ;; does not close the double-submit hole: a second click during the
      ;; round-trip fires a second concurrent e/server call, which here would
      ;; write a second group and race the !request reset below.
      (dom/props btn-node {:disabled (some? t) :aria-busy (some? t)})
      (when ?error
        (dom/div (dom/props {:style {:order "-1" :margin-right "auto"
                                     :color "var(--color-danger-text)" :font-size "12px"}})
          (dom/text "Error: " ?error)))
      (when t
        (let [geometry (editor-geometry !handle initial-geometry)
              ;; Deref HERE, in client scope. Inside the (e/server (e/Offload …))
              ;; thunk below, `!fields` would be a free variable Electric has to
              ;; transfer — and an atom is not serializable, so the socket throws
              ;; `[unserializable] … Value: [object Object]` and the whole
              ;; application crashes with no server-side trace. The atom's VALUE
              ;; is a plain map and crosses fine. Same rule as "never put an atom
              ;; in a map passed to an e/defn", one layer down: never let one be
              ;; captured by a server-sited closure either.
              io-fields (deref !fields)
              dirty? (occlusion-dirty? !handle initial-rects initial-elements
                       !fields initial-fields)]
          (if (or (nil? geometry) (empty? (:rects geometry)))
            (t "Draw at least one mask first")
            ;; `case` sequences the ENQUEUE, not the write. enqueue-update-
            ;; occlusion!* hands the command to the optimistic queue and returns;
            ;; the row is written later by the pump. The weaker guarantee is
            ;; enough here because the duplicate shares no state with the update:
            ;; it reads the source's topic ids from the database, which the
            ;; update never touches, and its geometry from this client buffer.
            ;; The one consequence is that a failed update still leaves the copy,
            ;; holding the buffer the user was looking at. That failure toasts.
            (case (if dirty?
                    (enqueue-update-occlusion!*
                      {:group-id (:group-id group) :mode (:mode group)
                       :geometry geometry :io-fields io-fields})
                    :clean)
              (let [result (e/server
                             (e/Offload
                               #(duplicate-group!* user-id
                                  {:group-id (:group-id group) :mode (:mode group)
                                   :geometry geometry :io-fields io-fields})))]
                (if (:success result)
                  ;; e/snapshot freezes the new group id as a plain number for
                  ;; the callback. An e/on-unmount closure reads a SIGNAL at fire
                  ;; time, not at creation time, so closing over `result` would
                  ;; have it read a server binding during its own teardown — the
                  ;; hazard video_pane records. A frozen scalar cannot move.
                  (let [copy-group-id (e/snapshot (:group-id result))]
                    ;; :kind is REQUIRED, not decoration. In ContentCardTable
                    ;; this atom IS !editing-card, and that host routes on :kind
                    ;; — a request without it falls through to EditCardModal,
                    ;; which then opens a card modal with a nil card id over an
                    ;; occlusion group. Matches the request CardRow writes.
                    (e/on-unmount
                      (fn [] (reset! !request {:kind "occlusion" :mode :edit
                                               :group-id copy-group-id})))
                    (case (e/server (commands/bump! user-id :add-occlusion))
                      (t)))
                  (t (:error result)))))))))))

(e/defn OcclusionActions
  "Create mode: two generate buttons (each = save with that mode).
   Edit mode: one Save. Plus Cancel with the dirty-discard guard.
   ctx = {:edit? :group :request :!request :!handle :!fields
          :initial-rects :initial-geometry :user-id :close!}
   !primary-btn / !history-open? are positional, NOT ctx keys — an atom inside
   a map passed to an e/defn is serialized as data and arrives unusable
   (CLAUDE.md 'Never Put Atoms in Maps Passed to e/defn')."
  [ctx !primary-btn !history-open?]
  (e/client
    (let [{:keys [edit? group request !request !handle !fields
                  initial-rects initial-geometry initial-elements initial-fields
                  user-id close!]} ctx
          !submit (atom nil)                    ; {:mode s :n int} — :n re-arms the token
          submit (e/watch !submit)
          submit! (fn [mode] (swap! !submit (fn [prev] {:mode mode :n (inc (:n prev 0))})))
          [t ?error] (e/Token submit)
          ;; One idempotency key per modal-open (the modal saves exactly one
          ;; group and closes on enqueue): a re-fired submit reuses it, so
          ;; enqueue-add-occlusion!* collapses duplicates to a single save.
          add-id (e/snapshot (random-uuid))]
      (dom/div
        (dom/props {:style {:display "flex" :justify-content "flex-end" :align-items "center"
                            :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
        (when ?error
          (dom/div (dom/props {:style {:order "-1" :margin-right "auto"
                                       :color "var(--color-danger-text)" :font-size "12px"}})
            (dom/text "Error: " ?error)))
        (if edit?
          (dom/button
            (dom/props {:class "btn btn-primary" :style {:order "1"} :disabled (some? t)})
            (reset! !primary-btn dom/node)
            (dom/text "Save")
            (dom/On "click" (fn [_] (submit! (:mode group))) nil))
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :order "1"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :disabled (some? t)
                          :title "One card per mask group; the front hides only the asked group's masks."})
              (dom/text "Hide One, Guess One")
              (dom/On "click" (fn [_] (submit! "hide-one")) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :disabled (some? t)
                          :title "One card per mask group; the front hides every mask and highlights the asked group's."})
              (reset! !primary-btn dom/node)
              (dom/text "Hide All, Guess One")
              (dom/On "click" (fn [_] (submit! "hide-all")) nil))))
        ;; Edit mode only: a group that does not exist yet has no history.
        ;; Opens the modal that OcclusionModalBody mounts — mounting it here
        ;; would nest a .modal-backdrop inside this modal's pointer-events:none
        ;; container, making its own backdrop and Close unclickable.
        (when edit?
          (OcclusionDuplicateButton group !request !handle !fields
            initial-rects initial-elements initial-geometry initial-fields user-id))
        (when edit?
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:order "0"}
                        :data-tooltip "View previous mask placements"})
            (dom/text "History")
            (dom/On "click" (fn [_] (reset! !history-open? true)) nil)))
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (close!)) nil)))
      (when t
        (let [geometry (editor-geometry !handle initial-geometry)]
          (if (or (nil? geometry) (empty? (:rects geometry)))
            (t "Draw at least one mask first")
            (if edit?
              (let [kept (set (keep :ordinal (:rects geometry)))
                    removed-note-ids (vec (keep (fn [[ordinal note-id]]
                                                  (when-not (contains? kept ordinal) note-id))
                                            (:note-ids-by-ordinal group)))
                    payload {:group-id (:group-id request)
                             :mode (:mode submit)
                             :geometry geometry
                             :io-fields @!fields}]
                ;; Fire-and-forget Anki note deletion for removed masks —
                ;; mirrors the card delete flow (server owns rows, client
                ;; owns AnkiConnect).
                (case (try-delete-anki-notes! removed-note-ids)
                  (case (enqueue-update-occlusion!* payload)
                    (do (e/on-unmount #(reset! !request nil))
                      (t)))))
              ;; :kind marks the overlay entry so PendingCardRow renders the
              ;; occlusion summary; create-group! ignores it.
              (let [payload {:kind "occlusion"
                             :topic-id (:topic-id request)
                             :root-topic-id (:root-topic-id request)
                             :image-media-id (:image-media-id request)
                             :mode (:mode submit)
                             :geometry geometry
                             :io-fields @!fields}]
                ;; Optimistic: overlay row + command, close immediately; the
                ;; CommandDispatcher persists the group (occlusion.clj).
                (case (enqueue-add-occlusion!* add-id payload)
                  (do (e/on-unmount #(reset! !request nil))
                    (t)))))))))))

(e/defn OcclusionModalBody
  "Inner dialog once edit data (if any) is loaded."
  [request !request user-id group]
  (e/client
    (let [edit? (= :edit (:mode request))
          image-media-id (if edit? (:image-media-id group) (:image-media-id request))
          initial-geometry (when edit? (:geometry group))
          initial-rects (vec (:rects initial-geometry))
          ;; vec, not the raw value: the server omits an empty :elements, and an
          ;; empty editor reads back [], so both sides must be [] to compare.
          initial-elements (vec (:elements initial-geometry))
          initial-fields (merge empty-io-fields (when edit? (:io-fields group)))
          field-key (if edit? [:io-edit (:group-id request)] [:io-new image-media-id])
          !handle (atom nil)
          !rects (atom initial-rects)
          !fields (atom initial-fields)
          ;; Upstream's rule: the first act when creating is drawing, the first
          ;; act when editing is selecting (Anki's MaskEditor.svelte). Sole
          ;; source of truth for the tool from here on.
          !tool (atom (if edit? :select :draw))
          ;; Selected node count, written from the editor's select-nodes!. A
          ;; mask group counts as one, matching what Delete removes.
          !selection (atom 0)
          ;; Live font size of the selected label; nil when none is selected.
          !label-size (atom nil)
          !tab (atom :masks)
          tab (e/watch !tab)
          !primary-btn (atom nil)
          !history-open? (atom false)
          close! (fn []
                   (when (or (not (occlusion-dirty? !handle initial-rects
                                    initial-elements !fields initial-fields))
                           (confirm-discard!))
                     (reset! !request nil)))
          card-font-sz (e/server (get-card-font-size* user-id))
          modal-font (str (or card-font-sz 14) "px")]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape close! (if edit? "Edit Image Occlusion" "Image Occlusion"))
        (dom/On "keydown"
          (fn [e] (modal/mod-enter-submit! e !primary-btn))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)"
                              :padding "var(--sp-6)" :width "720px" :max-width "95vw"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e] (modal/drag-modal-by-title! e))
            nil)
          (let [cleanup (attach-modal-tab-nav! dom/node)]
            (e/on-unmount (fn [] (when cleanup (cleanup)))))
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text (if edit? "Edit Image Occlusion" "Image Occlusion")))
          ;; Tab strip. Deliberately NOT btn-primary/btn-secondary — the modal
          ;; Tab navigation (attach-modal-tab-nav!) targets those classes for
          ;; its Save/Cancel stops and must not land on the tab strip.
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :margin-bottom "var(--sp-3)"}})
            (dom/button
              (dom/props {:class "btn btn-sm" :type "button"
                          :style {:background (if (= tab :masks) "var(--color-bg-subtle)" "transparent")
                                  :font-weight (if (= tab :masks) "600" "400")
                                  :border "1px solid var(--color-border)"}})
              (dom/text "Masks Editor")
              (dom/On "click" (fn [_] (reset! !tab :masks)) nil))
            (dom/button
              (dom/props {:class "btn btn-sm" :type "button"
                          :style {:background (if (= tab :fields) "var(--color-bg-subtle)" "transparent")
                                  :font-weight (if (= tab :fields) "600" "400")
                                  :border "1px solid var(--color-border)"}})
              (dom/text "Fields")
              (dom/On "click" (fn [_] (reset! !tab :fields)) nil)))
          ;; Both tab bodies stay mounted; the inactive one is display:none so
          ;; the Konva stage and Quill instances survive switches.
          (dom/div
            (dom/props {:style {:display (if (= tab :masks) "block" "none")}})
            (OcclusionMasksTab !handle image-media-id initial-rects initial-elements
              !rects !tool !selection !label-size))
          (dom/div
            (dom/props {:style {:display (if (= tab :fields) "block" "none")}})
            (OcclusionFieldsTab initial-fields !fields field-key modal-font))
          (OcclusionActions {:edit? edit? :group group :request request
                             :!request !request :!handle !handle :!fields !fields
                             :initial-rects initial-rects :initial-geometry initial-geometry
                             ;; Plain data, for Duplicate's dirty test — the
                             ;; same four values close! already compares.
                             :initial-elements initial-elements
                             :initial-fields initial-fields
                             :user-id user-id :close! close!}
            !primary-btn !history-open?)))
      ;; Sibling of the overlay div above, NOT a descendant of it — the overlay
      ;; is pointer-events:none and that property inherits, so nested the
      ;; history modal's backdrop and Close button were un-hit-testable and its
      ;; Escape bubbled into the overlay's ModalEscape. See the same comment in
      ;; card_modals.cljc/EditCardModal. Both overlays are z-index 1000, so this
      ;; call must stay LAST in the body. Group-scoped — mask placement lives on
      ;; occlusion_groups.geometry (plans/card-edit-history.md).
      (when edit?
        (CardHistoryModal [:occlusion-group (:group-id request)]
          user-id !history-open?)))))

(e/defn OcclusionModal
  "Host entry point. Mount while @!request is non-nil."
  [!request user-id]
  (e/client
    (let [request (e/watch !request)]
      (when request
        (if (= :edit (:mode request))
          (let [result (e/server (e/Offload #(get-group-for-edit* user-id (:group-id request))))]
            (cond
              (nil? result) nil                      ; server round-trip in flight
              ;; Keyed on the group id so moving from one group to another is an
              ;; honest unmount and mount, not an in-place swap. Duplicate ends
              ;; by pointing !request at the copy, and init-occlusion-editor!
              ;; carries no generation guard: re-evaluated in place it would
              ;; reset! !handle to a SECOND Konva stage while the first is still
              ;; mounted, and only the second would ever be destroyed. Same rule
              ;; and same remedy as video_pane's per-topic key.
              (:success result)
              (e/for-by identity [gid [(:group-id request)]]
                (OcclusionModalBody request !request user-id (:group result)))
              :else (dom/div
                      (dom/props {:style {:position "fixed" :bottom "20px" :right "20px"
                                          :background "var(--color-danger-bg)" :color "var(--color-danger-text)"
                                          :padding "var(--sp-3)" :border-radius "var(--radius-sm)"
                                          :z-index "1000"}})
                      (dom/text "Cannot open occlusion: " (:error result)))))
          (OcclusionModalBody request !request user-id nil))))))
