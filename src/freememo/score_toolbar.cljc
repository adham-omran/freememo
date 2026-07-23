(ns freememo.score-toolbar
  "Score editor chrome: the waveform strip (wavesurfer + single drag-region),
   the PDF-toolbar rect button, and the score card bar (pending-card status +
   the Add-card direction dropdown / edit-mode Update actions).

   Card creation materializes assets CLIENT-SIDE AT CREATION TIME (design
   decision — push time only fetches stored bytes, so headless Quick Sync
   works): cut+encode the MP3 clip from wavesurfer's already-decoded
   AudioBuffer, crop every rect from a fresh page render, upload all blobs to
   /api/upload-media, then enqueue :add-score-group / :update-score-group
   through the optimistic command queue (freememo.score)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.score-rect-modal :refer [ScoreRectModal]]
   [freememo.tooltip :as tooltip]
   [freememo.client-errors :as ce]
   #?(:cljs [freememo.score-wavesurfer :as ws])
   #?(:cljs [freememo.score-audio :as score-audio])
   #?(:cljs [freememo.score-pdf :as score-pdf])
   #?(:clj [freememo.score :as score])
   #?(:clj [freememo.optimistic :as opt])))

;; ---------------------------------------------------------------------------
;; Platform wrappers — reader conditionals in plain defns only.
;; ---------------------------------------------------------------------------

(defn format-ms
  "Milliseconds → \"m:ss.t\"."
  [ms]
  #?(:cljs (let [s (/ ms 1000)
                 m (js/Math.floor (/ s 60))
                 r (- s (* 60 m))]
             (str m ":" (.padStart (.toFixed r 1) 4 "0")))
     :clj ""))

(defn init-waveform!
  "Deferred wavesurfer init (container must be in the DOM first)."
  [!handle container audio-url on-region]
  #?(:cljs (do (js/setTimeout
                 (fn []
                   (when (.-isConnected container)
                     (reset! !handle
                       (ws/init! {:container container
                                  :url audio-url
                                  :on-region on-region}))))
                 0)
             nil)
     :clj nil))

(defn destroy-waveform! [!handle]
  #?(:cljs (do (ws/destroy! @!handle)
             (reset! !handle nil)
             nil)
     :clj nil))

(defn play-region!* [] #?(:cljs (do (ws/play-region!) nil) :clj nil))
(defn play-pause!* [] #?(:cljs (do (ws/play-pause!) nil) :clj nil))
(defn set-waveform-zoom!* [factor] #?(:cljs (do (ws/set-zoom-factor! factor) nil) :clj nil))
(defn set-loop!* [on?] #?(:cljs (do (ws/set-loop! on?) nil) :clj nil))
(defn set-volume!* [v] #?(:cljs (do (ws/set-volume! v) nil) :clj nil))

(defn- pages-map->vec
  "{page {:width :height :rects}} → sorted [{:page :width :height :rects}],
   zero-rect pages pruned. Portable (server validates the same shape)."
  [pages-map]
  (->> pages-map
    (keep (fn [[page {:keys [width height rects]}]]
            (when (seq rects)
              {:page page :width width :height height :rects (vec rects)})))
    (sort-by :page)
    vec))

#?(:cljs
   (defn- crop-page!
     "Render one page and crop+upload each of its rects.
      Returns Promise<page-entry with :media-id on every rect>."
     [{:keys [page width height rects]}]
     (-> (score-pdf/render-page page)
       (.then (fn [canvas]
                (-> (js/Promise.all
                      (into-array
                        (map (fn [rect]
                               (-> (score-pdf/crop-blob canvas rect)
                                 (.then #(score-pdf/upload-media % "score-crop.png"))
                                 (.then (fn [id] (assoc rect :media-id id)))))
                          rects)))
                  (.then (fn [arr]
                           {:page page :width width :height height
                            :rects (vec arr)}))))))))

(defn produce-assets!
  "Cut+encode the clip, crop+upload every rect. done ← {:clip-media-id N
   :pages [...]}; fail ← message string. Async; returns nil immediately."
  [{:keys [start-ms end-ms pages]} done fail]
  #?(:cljs
     (if-let [buffer (ws/decoded-buffer)]
       (do (-> (score-audio/cut-mp3 buffer start-ms end-ms)
             (.then (fn [blob] (score-pdf/upload-media blob "score-clip.mp3")))
             (.then (fn [clip-id]
                      (-> (js/Promise.all (into-array (map crop-page! pages)))
                        (.then (fn [arr]
                                 (done {:clip-media-id clip-id
                                        :pages (vec arr)}))))))
             (.catch (fn [e] (ce/report! :score/produce-assets e)
                       (fail (str (or (.-message e) e))))))
         nil)
       (do (fail "Audio is still loading — try again in a moment") nil))
     :clj (do (fail "client only") nil)))

(defn set-waveform-region!* [start-ms end-ms]
  #?(:cljs (do (ws/set-region! start-ms end-ms) nil)
     :clj nil))

(defn clear-selection!
  "Reset the pending card state after a successful add/update or cancel."
  [!score-region !score-pages !score-edit]
  #?(:cljs (ws/clear-region!) :clj nil)
  (reset! !score-region nil)
  (reset! !score-pages {})
  (reset! !score-edit nil)
  nil)

(defn enqueue-add-score!* [user-id id payload]
  #?(:clj (opt/enqueue-pending-card! user-id :add-score-group id payload)
     :cljs nil))

(defn enqueue-update-score!* [user-id payload]
  #?(:clj (opt/enqueue-command! user-id {:type :update-score-group :payload payload})
     :cljs nil))

(defn get-score-group-for-edit* [user-id group-id]
  #?(:clj (score/get-group-for-edit user-id group-id)
     :cljs nil))

(defn log-load-error!
  [msg]
  #?(:cljs (do (js/console.error "score group load failed:" msg)
               (ce/report! :score/group-load msg))
     :clj nil))

(defn load-group-into-editor!
  "Seed the score editor state from a loaded group (edit mode)."
  [group !score-region !score-pages !score-edit group-id]
  (let [region {:start-ms (:start-ms group) :end-ms (:end-ms group)}]
    (reset! !score-region region)
    (reset! !score-pages
      (into {}
        (map (fn [p] [(:page p) (select-keys p [:width :height :rects])]))
        (get-in group [:geometry :pages])))
    (reset! !score-edit {:group-id group-id})
    (set-waveform-region!* (:start-ms region) (:end-ms region))
    nil))

;; ---------------------------------------------------------------------------
;; Components
;; ---------------------------------------------------------------------------

(e/defn ScoreWaveformStrip
  "Waveform + transport above the PDF pane. Writes the drag-selected region
   into dctx/!score-region. Zoom is factor-of-fit (1 = whole file visible);
   loop replays the selection until toggled off."
  []
  (e/client
    (let [score-id dctx/pdf-root-id
          !score-region dctx/!score-region
          !handle (atom nil)
          !zoom (atom 1)
          !loop? (atom false)
          loop? (e/watch !loop?)
          zoom-by! (fn [mult]
                     (let [z (swap! !zoom #(max 1 (min 20 (* % mult))))]
                       (set-waveform-zoom!* z)))]
      (dom/div
        ;; position+z-index: own stacking context so wavesurfer's internal
        ;; absolutely-positioned layers (cursor z:5, regions z:3) cap at this
        ;; strip's level instead of escaping to root and painting over the
        ;; toolbar tooltips/dropdowns above (toolbars sit at z 2-3).
        (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-2)"
                            :padding "6px 10px" :border-bottom "1px solid var(--color-border)"
                            :background "var(--color-bg-surface)"
                            :position "relative" :z-index "1"}})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :title "Play / pause"})
          (dom/text "⏯")
          (dom/On "click" (fn [_] (play-pause!*)) nil))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :title "Play selection"})
          (dom/text "▶ selection")
          (dom/On "click" (fn [_] (play-region!*)) nil))
        (dom/button
          (dom/props {:class (if loop? "btn btn-sm btn-primary" "btn btn-sm btn-secondary")
                      :title "Loop the selection while practicing"})
          (dom/text "⟲ loop")
          (dom/On "click" (fn [_]
                            (let [on? (swap! !loop? not)]
                              (set-loop!* on?)))
            nil))
        (dom/input
          (dom/props {:type "range" :min "0" :max "1" :step "0.05" :value "1"
                      :title "Volume"
                      :style {:width "72px"}})
          (dom/On "input"
            (fn [e]
              (set-volume!* (js/parseFloat (-> e .-target .-value))))
            nil))
        (dom/div
          (dom/props {:style {:display "flex" :flex-direction "column" :gap "2px"}})
          (dom/button
            (dom/props {:class "btn btn-secondary" :title "Zoom waveform in"
                        :style {:padding "0 6px" :font-size "12px" :line-height "1.4"}})
            (dom/text "+")
            (dom/On "click" (fn [_] (zoom-by! 1.5)) nil))
          (dom/button
            (dom/props {:class "btn btn-secondary" :title "Zoom waveform out"
                        :style {:padding "0 6px" :font-size "12px" :line-height "1.4"}})
            (dom/text "−")
            (dom/On "click" (fn [_] (zoom-by! (/ 1 1.5))) nil)))
        (dom/div
          (dom/props {:style {:flex "1" :min-width "0" :overflow-x "auto"}})
          (let [host dom/node]
            (init-waveform! !handle host (str "/api/audio/" score-id)
              (fn [region] (reset! !score-region region)))
            (e/on-unmount (fn [] (destroy-waveform! !handle)))))))))

(e/defn ScoreRectButton
  "PDF-toolbar entry: opens the notation-rect snapshot modal."
  []
  (e/client
    (let [!score-pages dctx/!score-pages
          total-rects (transduce (map (comp count :rects)) + 0
                        (vals (e/watch !score-pages)))]
      (dom/div
        (dom/props {:class "toolbar-group"
                    :style {:padding-left "12px"
                            :border-left "1px solid var(--color-border)"}})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"})
          (tooltip/Tooltip! "Draw rectangles around the notation for the pending card")
          (icons/Icon :rect-select :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if (pos? total-rects)
                        (str "Notation (" total-rects ")")
                        "Notation")))
          (dom/On "click" (fn [_] (reset! dctx/!score-modal-open? true)) nil))))))

(e/defn ScoreAddDropdown
  "Add card ▾ → Audio front / Sheet front / Both. Disabled until a region and
   at least one rect exist. Edit mode: Update / Cancel instead."
  []
  (e/client
    (let [user-id dctx/user-id
          score-id dctx/pdf-root-id
          !score-region dctx/!score-region
          !score-pages dctx/!score-pages
          !score-edit dctx/!score-edit
          region (e/watch !score-region)
          pages-vec (pages-map->vec (e/watch !score-pages))
          edit (e/watch !score-edit)
          ready? (and region (seq pages-vec))
          !menu-open? (atom false)
          menu-open? (e/watch !menu-open?)
          !busy (atom nil)
          busy (e/watch !busy)
          !pipe-error (atom nil)
          pipe-error (e/watch !pipe-error)
          ;; {:payload m :n int} — assets are ready, enqueue server-side.
          !submit (atom nil)
          submit (e/watch !submit)
          [t ?error] (e/Token submit)
          start-pipeline!
          (fn [directions]
            (reset! !menu-open? false)
            (reset! !pipe-error nil)
            (reset! !busy "Cutting audio & cropping…")
            (produce-assets!
              {:start-ms (:start-ms region) :end-ms (:end-ms region)
               :pages pages-vec}
              (fn [{:keys [clip-media-id pages]}]
                (reset! !busy nil)
                (swap! !submit
                  (fn [prev]
                    {:n (inc (:n prev 0))
                     :payload
                     (if edit
                       {:group-id (:group-id edit)
                        :start-ms (:start-ms region) :end-ms (:end-ms region)
                        :clip-media-id clip-media-id
                        :geometry {:pages pages}}
                       {:kind "score"
                        :topic-id score-id
                        :root-topic-id score-id
                        :start-ms (:start-ms region) :end-ms (:end-ms region)
                        :clip-media-id clip-media-id
                        :geometry {:pages pages}
                        :directions directions})})))
              (fn [msg]
                (reset! !busy nil)
                (reset! !pipe-error msg))))]
      (dom/div
        (dom/props {:style {:position "relative" :display "flex"
                            :align-items "center" :gap "var(--sp-2)"}})
        (when pipe-error
          (dom/span
            (dom/props {:style {:color "var(--color-danger-text)" :font-size "12px"}})
            (dom/text pipe-error)))
        (when ?error
          (dom/span
            (dom/props {:style {:color "var(--color-danger-text)" :font-size "12px"}})
            (dom/text (str ?error))))
        (when busy
          (dom/span
            (dom/props {:style {:color "var(--color-text-secondary)" :font-size "12px"}})
            (dom/text busy)))
        (if edit
          (e/amb
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"
                          :disabled (or (not ready?) (some? busy) (some? t))})
              (dom/text "Update cards")
              (dom/On "click" (fn [_] (start-pipeline! nil)) nil))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"})
              (dom/text "Cancel edit")
              (dom/On "click"
                (fn [_] (clear-selection! !score-region !score-pages !score-edit))
                nil)))
          (dom/button
            (dom/props {:class "btn btn-sm btn-primary"
                        :disabled (or (not ready?) (some? busy) (some? t))})
            (tooltip/Tooltip! (when-not ready?
                                "Select an audio region and draw at least one notation rectangle"))
            (dom/text "Add card ▾")
            (dom/On "click" (fn [_] (swap! !menu-open? not)) nil)))
        (when (and menu-open? ready? (not edit))
          (dom/div
            (dom/props {:style {:position "absolute" :top "100%" :right "0" :z-index "50"
                                :margin-top "4px" :padding "4px"
                                :background "var(--color-bg-card)"
                                :border "1px solid var(--color-border)"
                                :border-radius "var(--radius-sm)"
                                :box-shadow "0 4px 12px rgba(0,0,0,0.15)"
                                :display "flex" :flex-direction "column" :gap "2px"
                                :white-space "nowrap"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :title "Front: audio clip — Back: notation"})
              (dom/text "Audio front")
              (dom/On "click" (fn [_] (start-pipeline! ["audio-front"])) nil))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :title "Front: notation — Back: audio clip"})
              (dom/text "Sheet front")
              (dom/On "click" (fn [_] (start-pipeline! ["sheet-front"])) nil))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :title "Two Anki notes, one per direction, sharing the same media"})
              (dom/text "Both")
              (dom/On "click"
                (fn [_] (start-pipeline! ["audio-front" "sheet-front"])) nil))))
        ;; Enqueue once the assets are materialized.
        (when t
          ;; One idempotency key per submit (this toolbar is always mounted and
          ;; reused across adds): e/snapshot freezes it for the armed token's
          ;; lifetime, so a re-fired enqueue reuses it and collapses to one save.
          (let [payload (:payload submit)
                add-id (e/snapshot (random-uuid))]
            (if (:group-id payload)
              (case (e/server (enqueue-update-score!* user-id payload))
                (do (clear-selection! !score-region !score-pages !score-edit)
                  (t)))
              (case (e/server (enqueue-add-score!* user-id add-id payload))
                (do (clear-selection! !score-region !score-pages !score-edit)
                  (t))))))))))

(e/defn ScoreToolbar
  "The score card bar under the PDF toolbar: pending-selection status + the
   Add-card dropdown. Also hosts the rect snapshot modal."
  []
  (e/client
    (let [!score-region dctx/!score-region
          !score-pages dctx/!score-pages
          region (e/watch !score-region)
          total-rects (transduce (map (comp count :rects)) + 0
                        (vals (e/watch !score-pages)))
          edit (e/watch dctx/!score-edit)]
      (dom/div
        (dom/props {:class "toolbar-container score-toolbar-container"})
        (dom/div
          (dom/props {:class "toolbar pdf-toolbar-bar"
                      :style {:display "flex" :align-items "center" :gap "var(--sp-3)"}})
          (dom/div
            (dom/props {:class "toolbar-group"
                        :style {:font-size "12px" :color "var(--color-text-secondary)"}})
            (dom/text
              (str (if edit "Editing card — " "")
                (if region
                  (str "Audio " (format-ms (:start-ms region))
                    " – " (format-ms (:end-ms region)))
                  "Drag on the waveform to select audio")
                " · "
                total-rects " rect" (when (not= 1 total-rects) "s"))))
          (dom/div
            (dom/props {:style {:margin-left "auto"}})
            (ScoreAddDropdown))))
      (ScoreRectModal))))

(e/defn ScoreEditLoader
  "Loads a score group into the editor when a score card row is clicked.
   Mount while (:kind @!editing-card) = \"score\"; clears the atom when done."
  [user-id !editing-card]
  (e/client
    (let [group-id (:group-id (e/watch !editing-card))
          !score-region dctx/!score-region
          !score-pages dctx/!score-pages
          !score-edit dctx/!score-edit
          result (e/server (e/Offload #(get-score-group-for-edit* user-id group-id)))]
      (when (some? result)
        (if (:success result)
          (case (load-group-into-editor! (:group result)
                  !score-region !score-pages !score-edit group-id)
            (reset! !editing-card nil))
          (case (log-load-error! (:error result))
            (reset! !editing-card nil)))))))
