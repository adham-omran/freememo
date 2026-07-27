(ns freememo.video-import-modal
  "Video import modal — file picker, chunked upload, playlist grouping.

   Its own namespace rather than another branch in `freememo.import-modal`:
   video is the only import whose transfer is a multi-request protocol with
   progress, cancellation and compensation, and folding that into the 950-line
   shared modal would push its e/defns toward the 64KB method cap for no
   sharing benefit.

   Multi-select creates a `video-playlist` parent and hangs each video under it
   (§4.10 10.1). A single file becomes a root topic, as every other import does."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.modal-shell :as modal-shell]
   [freememo.navigation :as nav]
   [freememo.commands :as commands]
   [freememo.video-format :refer [format-bytes]]
   #?(:cljs [freememo.video-upload :as vu])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.video :as video])))

(def accept-attr
  "Containers we accept for upload.

   Wider than what a browser can play, because since §12 anything that is not
   already an MP4 is remuxed to one at ingest — a container rewrite, not a
   transcode, so §4.11 11.2 still holds. Matroska is here on that basis: it
   plays in Chromium and, as far as we have measured, not in Safari."
  (str "video/mp4,video/webm,video/ogg,video/quicktime,video/x-matroska,"
    ".mp4,.webm,.ogv,.mov,.mkv"))

(def accepted-extensions
  "The extension half of `accept-attr`, for filtering dropped files.

   `accept` constrains the file dialog only, and every OS lets the user switch
   it off; a drop bypasses it entirely. This is the check that actually holds."
  #{".mp4" ".webm" ".ogv" ".mov" ".qt" ".mkv"})

(defn video-file?
  "Whether `filename` names a container we accept.

   Extension, not MIME: browsers report an empty `File.type` for containers they
   do not recognise, which would reject every .mkv on a machine with no
   registered handler."
  [filename]
  (let [lower (some-> filename str/lower-case)]
    (boolean (and lower (some #(str/ends-with? lower %) accepted-extensions)))))

;; ---------------------------------------------------------------------------
;; Platform wrappers
;; ---------------------------------------------------------------------------

(defn upload-limits*
  "The per-video ceiling and the user's remaining quota, as display strings.
   `_refresh` keys the node on :refresh so the remaining figure re-reads after a
   delete — and because a zero-arity server call in a client `let` diverges
   between the two compilers and kills the websocket (cost the Settings page an
   outage; see freememo.credits-section/storage-rate-label*)."
  [_refresh user-id]
  #?(:clj (video/upload-limits user-id)
     :cljs nil))

(defn create-playlist!*
  "Create the `video-playlist` parent for a multi-file upload. nil for a single
   file — one video needs no container."
  [user-id title n]
  #?(:clj (when (> n 1)
            (let [id (db/create-video-playlist! user-id title)]
              (commands/bump! user-id :import-document)
              id))
     :cljs nil))

;; ^js hints only exist on the CLJS reader branch — a whole-form reader
;; conditional, not a body-level one, or the CLJ compiler tries to resolve `js`
;; as a class (CLAUDE.md, "WRONG — CLJ compiler chokes on ^js in parameter list").
#?(:cljs
   (defn file-list->vec
     "js FileList → a CLJS vector, so the reactive body holds a value rather than
      a live DOM collection that mutates under it."
     [^js file-list]
     (vec (array-seq file-list)))
   :clj
   (defn file-list->vec [_file-list] []))

#?(:cljs
   (defn total-bytes [files] (reduce + 0 (map (fn [^js f] (.-size f)) files)))
   :clj
   (defn total-bytes [_files] 0))

#?(:cljs
   (defn file-name [^js f] (.-name f))
   :clj
   (defn file-name [_f] ""))

(defn partition-accepted
  "Split chosen files into the ones we accept and the names of the ones we do not.
   Post: {:accepted [file …] :rejected [name …]}."
  [files]
  {:accepted (vec (filter #(video-file? (file-name %)) files))
   :rejected (mapv file-name (remove #(video-file? (file-name %)) files))})

(defn start-upload!
  "Kick off the transfer. Returns nil immediately; results land in the atoms.

   `!uploading` brackets the whole promise chain — the Electric token that
   ordered playlist-then-transfer is spent as soon as the transfer STARTS (a
   token held for the minutes an upload takes would pin that frame), so the
   in-flight state has to be tracked out of band.

   `!progress` is written per chunk, `!done` with the created topic ids,
   `!error` with a message. `!cancel` is a plain boolean atom the uploader
   polls between chunks."
  [files parent-id !uploading !progress !done !error !cancel]
  #?(:cljs (do
             (reset! !uploading true)
             (-> (vu/upload-files!
                   {:files files
                    :parent-id parent-id
                    :cancelled? (fn [] @!cancel)
                    :on-file (fn [p] (reset! !progress p))})
               (.then (fn [ids]
                        (reset! !uploading false)
                        (reset! !done (vec ids))))
               (.catch (fn [e]
                         (reset! !uploading false)
                         (reset! !error (if @!cancel
                                          "Upload cancelled"
                                          (or (.-message e) (str e)))))))
             nil)
     :clj nil))

;; ---------------------------------------------------------------------------
;; Modal
;; ---------------------------------------------------------------------------

(e/defn UploadProgress
  "Per-file byte progress. Shows the file being sent, not an aggregate: a
   playlist upload is sequential, so 'file 3 of 5, 62 %' is the honest reading
   and an overall percentage would jump backwards between files."
  [progress]
  (e/client
    (let [{:keys [index count filename sent total]} progress
          pct (if (and total (pos? total)) (long (* 100 (/ sent total))) 0)]
      (dom/div
        (dom/props {:style {:margin-bottom "var(--sp-3)"}})
        (dom/div
          (dom/props {:style {:font-size "13px" :margin-bottom "6px"
                              :white-space "nowrap" :overflow "hidden"
                              :text-overflow "ellipsis"}})
          (dom/text (str (when (> count 1) (str "File " (inc index) " of " count " · "))
                      filename " — " pct "%")))
        (dom/div
          (dom/props {:style {:height "6px" :background "var(--color-bg-subtle)"
                              :border-radius "3px" :overflow "hidden"}})
          (dom/div
            (dom/props {:style {:height "100%" :width (str pct "%")
                                :background "var(--color-primary)"
                                :transition "width 0.2s linear"}})))))))

(e/defn VideoImportModal
  "Pick one or more video files and upload them.

   Post: on success the modal closes and navigates to the playlist parent (or
   the single video). On failure the modal STAYS open with the message — the
   session has already been compensated server-side, so retrying is safe."
  [!show user-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          limits (e/server (upload-limits* refresh user-id))
          per-video-label (e/server (:per-video-label limits))
          remaining-label (e/server (:remaining-label limits))
          !files (atom [])
          files (e/watch !files)
          !file-input (atom nil)
          !progress (atom nil)
          progress (e/watch !progress)
          !done (atom nil)
          done (e/watch !done)
          !error (atom nil)
          error (e/watch !error)
          !cancel (atom false)
          !uploading (atom false)
          uploading? (e/watch !uploading)
          !start (atom nil)
          start-click (e/watch !start)
          [t ?token-error] (e/Token start-click)
          busy? (or uploading? (some? t))
          ;; Closing while a transfer is in flight MUST cancel it. Without the
          ;; `reset!` the promise chain outlives the frame, keeps POSTing chunks
          ;; into atoms nobody watches, and finishes by creating a topic the user
          ;; asked to abandon — the uploader then compensates the session, so the
          ;; bytes are reclaimed either way, but only after a wasted transfer.
          ;; Reached from the Cancel button, Escape (ModalEscape), and unmount.
          close! (fn [] (reset! !cancel true) (reset! !show false))
          ;; One entry point for both the picker and the drop zone: `accept`
          ;; only biases the file dialog, and a drop bypasses it entirely, so
          ;; the filter has to live where the files arrive rather than on the
          ;; input element.
          choose-files! (fn [file-list]
                          (let [{:keys [accepted rejected]}
                                (partition-accepted (file-list->vec file-list))]
                            (reset! !files accepted)
                            (reset! !error
                              (when (seq rejected)
                                (str "Not a supported video: "
                                  (str/join ", " (take 3 rejected))
                                  (when (> (count rejected) 3)
                                    (str " and " (- (count rejected) 3) " more")))))))]

      ;; Navigate out once the transfer finishes. Kept separate from the token
      ;; chain: the upload completes in a JS promise, long after the token's
      ;; server call returned, so it needs its own trigger.
      (when (and done (seq done))
        (let [[nav-t _] (e/Token done)]
          (when nav-t
            (e/on-unmount close!)
            (case (navigate! :viewer (nav/nav-topic (first done) nil))
              (nav-t)))))

      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/div
          (dom/props {:class "modal-content modal-lg"})
          (modal-shell/ModalEscape close! "Import video")

          (dom/h3 (dom/props {:style {:margin "0 0 12px 0"}}) (dom/text "Import video"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"
                                :color "var(--color-text-secondary)"}})
            (dom/text (str "MP4, MKV, WebM, OGV or MOV. Select several files to create a playlist. "
                        "Anything that is not already an MP4 is converted after upload, "
                        "then transcribed.")))

          ;; The two ceilings, stated before the picker. Discovering them by
          ;; having a 700 MB upload rejected is the failure this replaces.
          (dom/div
            (dom/props {:style {:display "flex" :gap "16px" :flex-wrap "wrap"
                                :padding "8px 10px" :margin-bottom "var(--sp-3)"
                                :background "var(--color-bg-subtle)"
                                :border-radius "var(--radius-md)" :font-size "12px"
                                :color "var(--color-text-secondary)"}})
            (dom/span
              (dom/text "Max per video: ")
              (dom/strong (dom/text (str per-video-label))))
            (when remaining-label
              (dom/span
                (dom/text "Storage left: ")
                (dom/strong (dom/text (str remaining-label))))))

          ;; Drop zone / picker
          (dom/div
            (dom/props {:style {:border "2px dashed var(--color-border)"
                                :border-radius "var(--radius-md)"
                                :padding "28px" :text-align "center"
                                :cursor (if busy? "default" "pointer")
                                :opacity (if busy? "0.6" "1")
                                :margin-bottom "var(--sp-3)"}})
            (dom/On "click" (fn [_] (when-not busy?
                                      (when-some [inp @!file-input] (.click inp))))
              nil)
            (dom/On "dragover" (fn [e] (.preventDefault e)) nil)
            (dom/On "drop"
              (fn [e]
                (.preventDefault e)
                (when-not busy?
                  (choose-files! (-> e .-dataTransfer .-files))))
              nil)
            (dom/div
              (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
              (dom/text (if (seq files)
                          (str (count files) " file" (when (> (count files) 1) "s")
                            " · " (format-bytes (total-bytes files)))
                          "Drop video files here or click to browse"))))
          (dom/input
            (dom/props {:type "file" :accept accept-attr :multiple true
                        :style {:display "none"}})
            (reset! !file-input dom/node)
            (dom/On "change"
              (fn [e] (choose-files! (-> e .-target .-files)))
              nil))

          (when progress (UploadProgress progress))

          (when-let [msg (or error ?token-error)]
            (dom/div
              (dom/props {:style {:font-size "13px" :color "var(--color-danger)"
                                  :margin-bottom "var(--sp-3)"}})
              (dom/text (str msg))))

          ;; Actions
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end"
                                :gap "var(--sp-2)"}})
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text (if busy? "Cancel upload" "Cancel"))
              ;; While busy, cancel the transfer but KEEP the modal open so the
              ;; "Upload cancelled" message lands somewhere the user can see it.
              (dom/On "click"
                (fn [_] (if busy? (reset! !cancel true) (close!)))
                nil))
            (dom/button
              (dom/props {:class "btn btn-primary"
                          :disabled (or busy? (empty? files))})
              (dom/text (if busy? "Uploading…" "Upload"))
              (dom/On "click"
                (fn [_]
                  (reset! !error nil)
                  (reset! !cancel false)
                  (reset! !start (js/Date.now)))
                nil)))

          ;; Token chain: create the playlist parent server-side (when needed),
          ;; THEN start the client transfer. `case` sequences them — a plain
          ;; `do` is concurrent in Electric and the upload would race the
          ;; parent's creation, orphaning every child at the root.
          (when t
            (let [n (count files)
                  title (str "Video playlist · " n " videos")
                  parent-id (e/server (e/Offload #(create-playlist!* user-id title n)))]
              (case parent-id
                (case (start-upload! files parent-id !uploading !progress !done !error !cancel)
                  ;; The token is spent here, not when the upload finishes: it
                  ;; gated the parent-then-transfer ordering, and holding it for
                  ;; minutes would keep this frame alive across the whole
                  ;; transfer. `busy?` is driven by the atoms instead.
                  (t))))))))))
