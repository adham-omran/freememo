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
   [freememo.quill-field :refer [QuillField]]
   [freememo.card-modals :refer [attach-modal-tab-nav!]]
   [freememo.card-components :refer [try-delete-anki-notes!]]
   #?(:cljs [freememo.occlusion-editor :as occ-editor])
   #?(:clj [freememo.occlusion :as occ])
   #?(:clj [freememo.optimistic :as opt])
   #?(:clj [freememo.settings :as settings])))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals live in plain defns, never in the
;; e/defn reactive bodies (CLJ/CLJS frame signal-count rule).
;; ---------------------------------------------------------------------------

(defn get-group-for-edit* [group-id]
  #?(:clj (occ/get-group-for-edit group-id)
     :cljs nil))

(defn enqueue-add-occlusion!* [user-id payload]
  #?(:clj (opt/enqueue-pending-card! user-id :add-occlusion payload)
     :cljs nil))

(defn enqueue-update-occlusion!* [user-id payload]
  #?(:clj (opt/enqueue-command! user-id {:type :update-occlusion :payload payload})
     :cljs nil))

(defn get-card-font-size* [user-id]
  #?(:clj (settings/get-card-font-size user-id)
     :cljs nil))

(defn init-occlusion-editor!
  "Deferred Konva init (container must be in the DOM first) — mirrors
   quill_field/schedule-quill-init!."
  [!handle container image-media-id rects on-change]
  #?(:cljs (do (js/setTimeout
                 (fn []
                   (reset! !handle
                     (occ-editor/init! {:container container
                                        :image-url (str "/api/media/" image-media-id)
                                        :rects rects
                                        :on-change on-change})))
                 0)
             nil)
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

(defn handle-occlusion-modal-keys!
  "Escape → close! (dirty-guarded by the caller's close!); Cmd/Ctrl-Enter →
   click the primary button. Top-level defn — NOT an inline #?(:cljs) fn body:
   an event handler that closes over reactive bindings must capture the SAME
   set on both peers, or the frame slot counts diverge and the server crashes
   with ArrayIndexOutOfBounds on mount (observed: index 23 vs length 11)."
  [e close! !primary-btn]
  #?(:cljs (cond
             (= (.-key e) "Escape")
             (close!)
             (and (= (.-key e) "Enter") (or (.-metaKey e) (.-ctrlKey e)))
             (when-let [btn @!primary-btn]
               (.preventDefault e)
               (.click btn)))
     :clj nil))

(defn drag-modal-by-title!
  "Pointer-capture drag of the modal by its <h3> title bar — the card_modals
   drag pattern, hoisted to a top-level defn for the same capture-symmetry
   reason as handle-occlusion-modal-keys!."
  [e]
  #?(:cljs
     (let [inner (.-currentTarget e)
           h3 (.querySelector inner "h3")]
       (when (and h3 (or (= (.-target e) h3)
                       (.contains h3 (.-target e))))
         (.preventDefault e)
         (let [rect (.getBoundingClientRect inner)
               sx (.-clientX e)
               sy (.-clientY e)
               px (.-left rect)
               py (.-top rect)
               move-fn (fn [me]
                         (set! (.-position (.-style inner)) "fixed")
                         (set! (.-left (.-style inner))
                           (str (+ px (- (.-clientX me) sx)) "px"))
                         (set! (.-top (.-style inner))
                           (str (+ py (- (.-clientY me) sy)) "px"))
                         (set! (.-margin (.-style inner)) "0"))
               up-fn (fn self [ue]
                       (.releasePointerCapture inner (.-pointerId ue))
                       (.removeEventListener inner "pointermove" move-fn)
                       (.removeEventListener inner "pointerup" self))]
           (.setPointerCapture inner (.-pointerId e))
           (.addEventListener inner "pointermove" move-fn)
           (.addEventListener inner "pointerup" up-fn))))
     :clj nil))

(def empty-io-fields
  {:header "" :footer "" :remarks "" :sources "" :extra1 "" :extra2 ""})

;; ---------------------------------------------------------------------------
;; Sub-components (split per the JVM 64KB method-limit convention)
;; ---------------------------------------------------------------------------

(e/defn OcclusionMasksTab
  "Konva editor host. Mounted once; hidden (not unmounted) when the Fields tab
   is active so drawn masks survive tab switches."
  [!handle image-media-id initial-rects !rects]
  (e/client
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-3)"}})
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :margin-bottom "var(--sp-2)"}})
        (dom/text "Drag on the image to draw a mask. Click a mask to select it — drag to move, handles to resize, Delete/Backspace to remove."))
      (dom/div
        (dom/props {:class "occlusion-editor-host"
                    :style {:min-height "200px" :max-height "58vh"
                            :overflow "auto" :background "var(--color-bg-subtle)"
                            :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-sm)"}})
        (let [host dom/node
              on-change (fn [rects] (reset! !rects rects))]
          (init-occlusion-editor! !handle host image-media-id initial-rects on-change)
          (e/on-unmount (fn [] (destroy-occlusion-editor! !handle)))))
      (let [rects (e/watch !rects)]
        (dom/div
          (dom/props {:style {:font-size "12px" :color "var(--color-text-hint)"
                              :margin-top "var(--sp-1)"}})
          (dom/text (str (count rects) " mask" (when (not= 1 (count rects)) "s"))))))))

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
          :initial-rects :initial-geometry :user-id :close!}"
  [ctx !primary-btn]
  (e/client
    (let [{:keys [edit? group request !request !handle !fields
                  initial-rects initial-geometry user-id close!]} ctx
          !submit (atom nil)                    ; {:mode s :n int} — :n re-arms the token
          submit (e/watch !submit)
          submit! (fn [mode] (swap! !submit (fn [prev] {:mode mode :n (inc (:n prev 0))})))
          [t ?error] (e/Token submit)]
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
                          :title "One card per mask; the front hides only the asked mask."})
              (dom/text "Hide One, Guess One")
              (dom/On "click" (fn [_] (submit! "hide-one")) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :disabled (some? t)
                          :title "One card per mask; the front hides every mask and highlights the asked one."})
              (reset! !primary-btn dom/node)
              (dom/text "Hide All, Guess One")
              (dom/On "click" (fn [_] (submit! "hide-all")) nil))))
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
                  (case (e/server (enqueue-update-occlusion!* user-id payload))
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
                (case (e/server (enqueue-add-occlusion!* user-id payload))
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
          !tab (atom :masks)
          tab (e/watch !tab)
          !primary-btn (atom nil)
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
        (dom/On "keydown"
          (fn [e] (handle-occlusion-modal-keys! e close! !primary-btn))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)"
                              :padding "var(--sp-6)" :width "720px" :max-width "95vw"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e] (drag-modal-by-title! e))
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
            (OcclusionMasksTab !handle image-media-id initial-rects !rects))
          (dom/div
            (dom/props {:style {:display (if (= tab :fields) "block" "none")}})
            (OcclusionFieldsTab initial-fields !fields field-key modal-font))
          (OcclusionActions {:edit? edit? :group group :request request
                             :!request !request :!handle !handle :!fields !fields
                             :initial-rects initial-rects :initial-geometry initial-geometry
                             :user-id user-id :close! close!}
            !primary-btn))))))

(e/defn OcclusionModal
  "Host entry point. Mount while @!request is non-nil."
  [!request user-id]
  (e/client
    (let [request (e/watch !request)]
      (when request
        (if (= :edit (:mode request))
          (let [result (e/server (get-group-for-edit* (:group-id request)))]
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
