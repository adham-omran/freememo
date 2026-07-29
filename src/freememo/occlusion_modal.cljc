(ns freememo.occlusion-modal
  "Image-occlusion authoring modal: Masks Editor tab (Konva, rects only) +
   Fields tab (the six FreeMemo IO text fields). Create mode ends with two
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
   [freememo.modal-shell :as modal]
   [freememo.quill-field :refer [QuillField]]
   [freememo.card-modals :refer [attach-modal-tab-nav!]]
   [freememo.card-history-modal :refer [CardHistoryModal]]
   [freememo.card-components :refer [try-delete-anki-notes!]]
   [freememo.occlusion-ordinals :as ord]
   #?(:cljs [freememo.occlusion-editor :as occ-editor])
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
  [!handle container image-media-id rects !tool on-change on-tool-change]
  #?(:cljs (do (js/setTimeout
                 (fn []
                   (reset! !handle
                     (occ-editor/init! {:container container
                                        :image-url (str "/api/media/" image-media-id)
                                        :rects rects
                                        :tool @!tool
                                        :on-change on-change
                                        :on-tool-change on-tool-change})))
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

(defn editor-geometry
  "{:width :height :rects} in natural px, or the fallback (edit mode's loaded
   geometry) when the editor has no image dimensions."
  [!handle fallback-geometry]
  #?(:cljs (if-let [h @!handle]
             (let [{:keys [natural]} @h
                   rects (occ-editor/read-rects h)]
               (if (and natural rects)
                 {:width (:width natural) :height (:height natural) :rects rects}
                 fallback-geometry))
             fallback-geometry)
     :clj fallback-geometry))

(defn occlusion-dirty?
  "True when rects or text fields differ from their loaded state."
  [!handle initial-rects !fields initial-fields]
  #?(:cljs (or (not= (editor-rects !handle initial-rects) initial-rects)
             (not= @!fields initial-fields))
     :clj false))

(defn confirm-discard! []
  #?(:cljs (js/confirm "Discard unsaved occlusion changes?")
     :clj true))

(def empty-io-fields
  {:header "" :footer "" :remarks "" :sources "" :extra1 "" :extra2 ""})

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

(e/defn OcclusionMaskToolbar
  "Tool switch plus mask-group actions, mirroring the editor's own shortcuts.
   Watches !tool HERE, not in the parent: the parent's body hosts the Konva init
   side effect, and a signal there would put that call in a body Electric may
   re-evaluate (CLAUDE.md, JS library init). !tool is the single source of truth
   for the tool — the editor writes it back on a keyboard switch."
  [!handle !tool]
  (e/client
    (let [tool (e/watch !tool)]
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :align-items "center"
                            :margin-bottom "var(--sp-2)" :flex-wrap "wrap"}})
        (OcclusionToolButton "Select (S)" (= tool :select)
          (fn [] (set-editor-tool! !handle :select)))
        (OcclusionToolButton "Draw (R)" (= tool :draw)
          (fn [] (set-editor-tool! !handle :draw)))
        (dom/span (dom/props {:style {:color "var(--color-border)"}}) (dom/text "|"))
        (OcclusionToolButton "Group (G)" false
          (fn [] (group-editor-selection! !handle)))
        (OcclusionToolButton "Ungroup (U)" false
          (fn [] (ungroup-editor-selection! !handle)))))))

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
  [!handle image-media-id initial-rects !rects !tool]
  (e/client
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (OcclusionMaskToolbar !handle !tool)
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :margin-bottom "var(--sp-2)"}})
        (dom/text "Draw (R): drag on the image to add a mask. Select (S): click a mask, Shift-click or drag a box to add more; drag to move, handles to resize, Delete to remove. Group (G) makes the selected masks one card; Ungroup (U) splits them."))
      (dom/div
        (dom/props {:class "occlusion-editor-host"
                    :style {:min-height "200px" :max-height "58vh"
                            :overflow "auto" :background "var(--color-bg-subtle)"
                            :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-sm)"}})
        (let [host dom/node
              on-change (fn [rects] (reset! !rects rects))
              on-tool-change (fn [tool] (reset! !tool tool))]
          ;; The editor reads its starting tool from !tool, so a re-init (or a
          ;; remount) resumes the user's tool instead of resetting it.
          (init-occlusion-editor! !handle host image-media-id initial-rects
            !tool on-change on-tool-change)
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
                  initial-rects initial-geometry user-id close!]} ctx
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
          initial-fields (merge empty-io-fields (when edit? (:io-fields group)))
          field-key (if edit? [:io-edit (:group-id request)] [:io-new image-media-id])
          !handle (atom nil)
          !rects (atom initial-rects)
          !fields (atom initial-fields)
          ;; Upstream's rule: the first act when creating is drawing, the first
          ;; act when editing is selecting (Anki's MaskEditor.svelte). Sole
          ;; source of truth for the tool from here on.
          !tool (atom (if edit? :select :draw))
          !tab (atom :masks)
          tab (e/watch !tab)
          !primary-btn (atom nil)
          !history-open? (atom false)
          close! (fn []
                   (when (or (not (occlusion-dirty? !handle initial-rects !fields initial-fields))
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
            (OcclusionMasksTab !handle image-media-id initial-rects !rects !tool))
          (dom/div
            (dom/props {:style {:display (if (= tab :fields) "block" "none")}})
            (OcclusionFieldsTab initial-fields !fields field-key modal-font))
          (OcclusionActions {:edit? edit? :group group :request request
                             :!request !request :!handle !handle :!fields !fields
                             :initial-rects initial-rects :initial-geometry initial-geometry
                             :user-id user-id :close! close!}
            !primary-btn !history-open?))
        ;; Sibling of the modal container, not a descendant: .modal-backdrop and
        ;; this modal are both z-index 1000 so later DOM order must win, and the
        ;; container above is pointer-events:none, which a nested backdrop would
        ;; inherit. Group-scoped — mask placement lives on
        ;; occlusion_groups.geometry (plans/card-edit-history.md).
        (when edit?
          (CardHistoryModal [:occlusion-group (:group-id request)]
            user-id !history-open?))))))

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
              (:success result) (OcclusionModalBody request !request user-id (:group result))
              :else (dom/div
                      (dom/props {:style {:position "fixed" :bottom "20px" :right "20px"
                                          :background "var(--color-danger-bg)" :color "var(--color-danger-text)"
                                          :padding "var(--sp-3)" :border-radius "var(--radius-sm)"
                                          :z-index "1000"}})
                      (dom/text "Cannot open occlusion: " (:error result)))))
          (OcclusionModalBody request !request user-id nil))))))
