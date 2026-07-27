(ns freememo.video-import-modal
  "Video import modal — file picker, chunked upload, playlist grouping.

   Its own namespace rather than another branch in `freememo.import-modal`:
   video is the only import whose transfer is a multi-request protocol with
   progress, cancellation and compensation, and folding that into the 950-line
   shared modal would push its e/defns toward the 64KB method cap for no
   sharing benefit.

   Where the videos land is an explicit three-way choice (§14.3 2.1): separate
   root topics, a new `video-playlist` parent (§4.10 10.1), or an existing
   playlist. The file count only preselects it — two or more files still default
   to a new playlist, which is what the count used to decide on its own.

   The selection accumulates: a pick adds to what is staged rather than
   replacing it, so one video can become a playlist by adding a second."
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
  "Create the `video-playlist` parent and return its id.

   No file-count gate: it used to refuse when n ≤ 1, which made the target
   control's `new` + one file unreachable. Whether a playlist is wanted is the
   control's decision (§14.3 2.4), and a creation function that second-guesses
   its caller can only be wrong.

   Pre:  `title` is non-blank — the caller substitutes the derived default
         (§14.3 4.3); `input-check/check-length!` is the server-side backstop.
   Post: a kind='video-playlist' root topic exists and :import-document is
         bumped, so the tree and the picker both see it."
  [user-id title]
  #?(:clj (let [id (db/create-video-playlist! user-id title)]
            (commands/bump! user-id :import-document)
            id)
     :cljs nil))

(defn playlist-options*
  "The user's playlists as `[{:id :title}]`, for the target picker.

   `_refresh` is the watched :tree-mutations counter, passed in rather than
   closed over: it both re-reads the list after this modal creates a playlist
   and keeps the call from being a zero-arity server call in a client `let`,
   which diverges between the two compilers and kills the websocket (see
   `upload-limits*`)."
  [_refresh user-id]
  #?(:clj (db/list-video-playlists user-id)
     :cljs []))

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

#?(:cljs
   (defn file-identity
     "What makes two picks the same file.

      A `File` carries no id, so this is the strongest key available. It is not
      a guarantee — two genuinely different files could share all three — but a
      collision costs the user one re-pick, where a missed duplicate costs them
      a second upload of the same bytes and a second quota reservation."
     [^js f]
     [(.-name f) (.-size f) (.-lastModified f)])
   :clj
   (defn file-identity [_f] nil))

(defn add-to-selection
  "Merge `incoming` into the already-staged `staged`, sorting the outcome.

   Three-way split rather than two: a duplicate is neither accepted nor a wrong
   file type, and telling the user \"already staged\" is different from telling
   them \"not a video\".

   Pre:  `staged` holds files this function has already accepted.
   Post: {:accepted [file …] :rejected [name …] :duplicates [name …]}, where
         `:accepted` is `staged` followed by the new files in pick order, and no
         two entries share a `file-identity`.
   Invariant: every caller replaces the whole selection with `:accepted` — a
   caller that conjes onto its own list instead would reintroduce duplicates."
  [staged incoming]
  (let [seen (volatile! (set (map file-identity staged)))]
    (reduce (fn [acc f]
              (let [nm (file-name f)]
                (cond
                  (not (video-file? nm)) (update acc :rejected conj nm)
                  (contains? @seen (file-identity f)) (update acc :duplicates conj nm)
                  :else (do (vswap! seen conj (file-identity f))
                            (update acc :accepted conj f)))))
      {:accepted (vec staged) :rejected [] :duplicates []}
      incoming)))

(defn remove-from-selection
  "Drop the file at `idx`. Index, not identity: `add-to-selection` guarantees the
   vector is duplicate-free, so position is unambiguous and cheaper than
   re-deriving a key. Out-of-range leaves the selection untouched."
  [staged idx]
  (if (and (nat-int? idx) (< idx (count staged)))
    (into (subvec staged 0 idx) (subvec staged (inc idx)))
    staged))

;; ---------------------------------------------------------------------------
;; Target — where the uploaded videos land
;; ---------------------------------------------------------------------------

;; TEMPORARY diagnostic counter for the sticky-default investigation: a defonce
;; survives frame remounts, so it distinguishes "the component remounted" from
;; "the value changed".
(defonce !dbg-mounts (atom 0))

(def target-labels
  "The three places an upload can put its videos (§14.3 2.1). Order is the order
   they appear in the select."
  [["separate" "Separate documents"]
   ["new" "New playlist"]
   ["existing" "Add to existing playlist"]])

(defn default-target
  "The target to preselect for `n` staged files.

   Reproduces the rule the file count used to imply on its own — two or more
   files meant a playlist — so a user who never touches the control gets the
   behaviour they had before it existed. The caller stops consulting this once
   the user picks something (§14.3 2.3): re-deriving after that would revert an
   explicit `separate` the moment another file was added."
  [n]
  (if (> n 1) "new" "separate"))

(defn selection-message
  "One line naming what a pick threw away, or nil when it kept everything.
   Two causes stay distinct: 'already staged' and 'not a video' are different
   mistakes and lead the user to different corrections."
  [{:keys [rejected duplicates]}]
  (let [clause (fn [label names]
                 (when (seq names)
                   (str label (str/join ", " (take 3 names))
                     (when (> (count names) 3)
                       (str " and " (- (count names) 3) " more")))))]
    (not-empty
      (str/join " · " (remove nil? [(clause "Not a supported video: " rejected)
                                    (clause "Already staged: " duplicates)])))))

(defn derive-playlist-title
  "A playlist title from the first staged filename, extension stripped.

   An approximation of what the server will store: `input-check/prettify-title`
   and `sanitize-filename` live in a .clj namespace the client cannot call, and
   they still run on whatever is submitted. Post: \"\" when there is no file,
   which the caller treats as 'nothing to prefill'."
  [filename]
  (let [nm (or filename "")
        dot (str/last-index-of nm ".")]
    (str/trim (if (and dot (pos? dot)) (subs nm 0 dot) nm))))

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

(e/defn StagedFileList
  "The staged files, one row each, with a ✕ to drop one.

   Own e/defn for the 64KB method cap: `VideoImportModal`'s body is already
   long, and the documented remedy is to extract sibling groups rather than
   distort them.

   `remove!` is a plain fn taking the row index — `add-to-selection` keeps the
   vector duplicate-free, so position identifies a row unambiguously."
  [files busy? remove!]
  (e/client
    (when (seq files)
      (dom/div
        (dom/props {:style {:max-height "180px" :overflow-y "auto"
                            :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-md)"
                            :margin-bottom "var(--sp-3)"}})
        (e/for [[i f] (e/diff-by first (map-indexed vector files))]
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :padding "6px 8px" :font-size "13px"
                                :border-bottom "1px solid var(--color-bg-subtle)"}})
            (dom/span
              (dom/props {:style {:flex "1" :min-width "0" :overflow "hidden"
                                  :text-overflow "ellipsis" :white-space "nowrap"}})
              (dom/text (file-name f)))
            (dom/span
              (dom/props {:style {:color "var(--color-text-secondary)" :font-size "12px"
                                  :font-variant-numeric "tabular-nums"}})
              (dom/text (format-bytes (total-bytes [f]))))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"
                          :disabled busy?
                          :aria-label (str "Remove " (file-name f))})
              (dom/text "✕")
              (dom/On "click" (fn [_] (when-not busy? (remove! i))) nil))))))))

(e/defn UploadTargetControls
  "Where the upload lands: the three-way target, the playlist picker it reveals,
   and the title field for a new playlist (§14.3 2–4).

   Own e/defn for the same bytecode reason as `StagedFileList`. Atoms arrive as
   positional parameters, never inside a map: Electric serialises a map as data
   and an atom in one is unserialisable.

   None of these atoms feeds an `e/Token`, so passing them down is safe — the
   rule that bans it applies to token inputs, whose re-derivation on any map
   change would re-fire the token."
  [target playlist-id playlist-options title-default busy?
   !target !target-chosen? !playlist-id !title]
  (e/client
    (let [have-playlists? (pos? (count playlist-options))
          options (if have-playlists?
                    target-labels
                    ;; §14.3 2.2 — an option that cannot be satisfied is worse
                    ;; than an absent one.
                    (remove #(= "existing" (first %)) target-labels))]
      (dom/div
        (dom/props {:style {:display "flex" :flex-direction "column" :gap "8px"
                            :margin-bottom "var(--sp-3)"}
                    ;; TEMPORARY diagnostic for the sticky-default investigation.
                    :data-dbg-target (str target)
                    :data-dbg-chosen (str (e/watch !target-chosen?))
                    :data-dbg-choice (str (e/watch !target))
                    :data-dbg-mounts (str (e/snapshot (swap! !dbg-mounts inc)))})
        (dom/label
          (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                              :font-size "13px"}})
          (dom/text "Upload as")
          (dom/select
            (dom/props {:class "input" :disabled busy? :style {:flex "1"}})
            (e/for [[v label] (e/diff-by first options)]
              (dom/option
                (dom/props {:value v :selected (= v target)})
                (dom/text label)))
            (dom/On "change"
              (fn [e]
                ;; Read the event's value FIRST, before touching any watched
                ;; atom. `reset!` on a watched atom flushes a frame, and this
                ;; component's `:selected` props are derived from `target` — so
                ;; the intermediate frame, in which `!target-chosen?` is true but
                ;; `!target` is still nil, rewrites the select back to the
                ;; count-derived default. Reading `.value` after that read the
                ;; clobbered value and the user's choice was lost. Measured:
                ;; choosing "Add to existing playlist" with one file staged
                ;; stored "separate".
                (let [v (-> e .-target .-value)]
                  ;; Marking it chosen is what freezes the count-derived
                  ;; default: without this, adding another file would revert an
                  ;; explicit "Separate documents" back to "New playlist".
                  (reset! !target-chosen? true)
                  (reset! !target v)))
              nil)))

        (when (= target "existing")
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :font-size "13px"}})
            (dom/text "Playlist")
            (dom/select
              (dom/props {:class "input" :disabled busy? :style {:flex "1"}})
              (dom/option (dom/props {:value ""}) (dom/text "Choose a playlist…"))
              (e/for [p (e/diff-by :id playlist-options)]
                (dom/option
                  (dom/props {:value (str (:id p)) :selected (= (:id p) playlist-id)})
                  (dom/text (:title p))))
              (dom/On "change"
                (fn [e] (let [v (-> e .-target .-value)]
                          (reset! !playlist-id (when-not (str/blank? v) (parse-long v)))))
                nil))))

        (when (= target "new")
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                :font-size "13px"}})
            (dom/text "Title")
            (dom/input
              (dom/props {:class "input" :disabled busy? :style {:flex "1"}
                          :placeholder "Playlist title"})
              ;; Imperative, on mount, and ALWAYS writing the node — the derive
              ;; is what's conditional, not the write.
              ;;
              ;; Invariant: the field's DOM value equals `@!title`. An earlier
              ;; version guarded the whole block on a blank atom, which held
              ;; 4.2 (no re-derivation over a typed title) but broke the
              ;; invariant: switching the target away and back remounted the
              ;; input empty while the atom still held a title, so the field
              ;; looked blank and the upload used a value the user could not
              ;; see. Measured, not theorised.
              (let [node dom/node]
                (e/snapshot
                  (let [v (if (str/blank? @!title) title-default @!title)]
                    (set! (.-value node) v)
                    (reset! !title v))))
              (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))))))))

(e/defn VideoImportModal
  "Pick video files, choose where they land, and upload them.

   Post: on success the modal closes and navigates to the playlist when there is
   one, else to the first uploaded video. On failure the modal STAYS open with
   the message — the session has already been compensated server-side, so
   retrying is safe, and a file that finished before the failure is a complete
   topic rather than partial state."
  [!show user-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          limits (e/server (upload-limits* refresh user-id))
          per-video-label (e/server (:per-video-label limits))
          remaining-label (e/server (:remaining-label limits))
          ;; :tree-mutations, not :refresh — a playlist created here is a
          ;; tree-shape change, and it is the channel `:import-document` bumps.
          tree-rev (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          playlist-options (e/server (vec (playlist-options* tree-rev user-id)))
          !files (atom [])
          files (e/watch !files)
          !target (atom nil)               ;; nil ⇒ still following the count
          !target-chosen? (atom false)
          target-choice (e/watch !target)
          target-chosen? (e/watch !target-chosen?)
          ;; §14.3 2.3 — the count decides until the user does.
          target (if target-chosen?
                   (or target-choice (default-target (count files)))
                   (default-target (count files)))
          !playlist-id (atom nil)
          playlist-id (e/watch !playlist-id)
          !title (atom "")
          title (e/watch !title)
          title-default (derive-playlist-title (file-name (first files)))
          ;; Set at dispatch, read at navigation: the token frame that resolved
          ;; the parent is long gone by the time the transfer finishes.
          !uploaded-parent (atom nil)
          uploaded-parent (e/watch !uploaded-parent)
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
          ;; Appends rather than replaces (§14.3 1.1): a pick used to discard
          ;; whatever was already staged, so "add one more" was impossible.
          choose-files! (fn [file-list]
                          (let [outcome (add-to-selection @!files (file-list->vec file-list))]
                            (reset! !files (:accepted outcome))
                            (reset! !error (selection-message outcome))))
          remove-file! (fn [idx] (swap! !files remove-from-selection idx))]

      ;; Navigate out once the transfer finishes. Kept separate from the token
      ;; chain: the upload completes in a JS promise, long after the token's
      ;; server call returned, so it needs its own trigger.
      (when (and done (seq done))
        (let [[nav-t _] (e/Token done)]
          (when nav-t
            (e/on-unmount close!)
            ;; §14.3 5.2 — land on the playlist when there is one. It used to
            ;; always navigate to `(first done)`, the first VIDEO, which
            ;; contradicted this component's own docstring.
            (case (navigate! :viewer
                    (nav/nav-topic (or uploaded-parent (first done)) nil))
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
            (dom/text (str "MP4, MKV, WebM, OGV or MOV. Add as many files as you like, "
                        "then choose whether they become separate documents or a playlist. "
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
              (fn [e]
                (choose-files! (-> e .-target .-files))
                ;; Clear the input's own value, or picking the SAME file again
                ;; after removing it fires no "change" event and the row cannot
                ;; be restored.
                (set! (-> e .-target .-value) ""))
              nil))

          (StagedFileList files busy? remove-file!)

          ;; "＋ Add more" — the same input, without clearing what is staged.
          (when (seq files)
            (dom/div
              (dom/props {:style {:margin-bottom "var(--sp-3)"}})
              (dom/button
                (dom/props {:class "btn btn-sm btn-secondary" :disabled busy?})
                (dom/text "＋ Add more")
                (dom/On "click"
                  (fn [_] (when-not busy?
                            (when-some [inp @!file-input] (.click inp))))
                  nil))))

          (when (seq files)
            (UploadTargetControls target playlist-id playlist-options title-default busy?
              !target !target-chosen? !playlist-id !title))

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
              ;; §14.3 3.4 — "existing" with nothing chosen is a state the UI can
              ;; prevent, so it disables rather than erroring after the click.
              (dom/props {:class "btn btn-primary"
                          :disabled (or busy? (empty? files)
                                      (and (= target "existing") (nil? playlist-id)))})
              (dom/text (if busy? "Uploading…" "Upload"))
              (dom/On "click"
                (fn [_]
                  (reset! !error nil)
                  (reset! !cancel false)
                  (reset! !start (js/Date.now)))
                nil)))

          ;; Token chain: resolve the parent server-side (creating it when the
          ;; target says so), THEN start the client transfer. `case` sequences
          ;; them — a plain `do` is concurrent in Electric and the upload would
          ;; race the parent's creation, orphaning every child at the root.
          (when t
            (let [;; Blank falls back to the derived default rather than blocking
                  ;; the upload (§14.3 4.3); check-length! is the server's own
                  ;; backstop.
                  playlist-title (let [typed (str/trim (or title ""))]
                                   (if (str/blank? typed) title-default typed))
                  parent-id (e/server
                              (case target
                                "new" (e/Offload #(create-playlist!* user-id playlist-title))
                                "existing" playlist-id
                                nil))]
              (case parent-id
                (case (reset! !uploaded-parent parent-id)
                  (case (start-upload! files parent-id !uploading !progress !done !error !cancel)
                    ;; The token is spent here, not when the upload finishes: it
                    ;; gated the parent-then-transfer ordering, and holding it for
                    ;; minutes would keep this frame alive across the whole
                    ;; transfer. `busy?` is driven by the atoms instead.
                    (t)))))))))))
