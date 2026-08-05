(ns freememo.import-types
  "What the import page accepts, and which transport carries it.

   The single definition of the accepted extension set. Before this namespace the
   set existed four times — `import-modal`'s document and audio `accept` strings,
   `video-import-modal/accepted-extensions`, and `archive-upload/archive-file?` —
   and nothing kept them in agreement. The intake area's chips, the drop router,
   each card's file dialog, and the rejection message now all read from here, so
   adding an extension is one edit.

   `:transport` is the protocol that carries the bytes, NOT the server's import
   flow. Three exist, and they are genuinely different protocols:

     :multipart        one POST to /api/upload-file, capped by one HTTP request
     :video-session    init → chunk* → finalize against /api/video/*
     :archive-session  the same shape against /api/archive/*

   The server resolves the flow itself — an archive's flow comes from its entry
   names (`content-type/classify-archive`), not from its extension — so nothing
   here predicts it.

   `.mp4` and `.webm` route to :video-session. Both classifiers claimed them
   before: `content-type/classify-multipart` reads them as audio and
   `video-import-modal` reads them as video, and the card the user picked was
   what disambiguated. One intake area has no such signal, so the extension
   decides, and it decides video. The cost is that an audio-only file in either
   container lands as a video topic; it still transcribes."
  (:require
   [clojure.string :as str]
   #?(:clj [freememo.archive :as archive])
   #?(:clj [freememo.audio :as audio])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.video :as video])))

;; ── The registry ───────────────────────────────────────────────────

(def groups
  "Every accepted extension, grouped by what the user calls it.

   Order is display order: the intake area shows the first six `:label`s as
   chips and counts the rest behind `+ more`. `:ceiling` names which of the four
   ceilings `ceilings*` returns applies to the group — the numbers themselves are
   per-user or env-configured, so they are read at runtime, never inlined here."
  [{:label "PDF"      :transport :multipart       :ceiling :document
    :extensions [".pdf"]}
   {:label "EPUB"     :transport :multipart       :ceiling :document
    :extensions [".epub"]}
   {:label "HTML"     :transport :multipart       :ceiling :document
    :extensions [".html" ".htm"]}
   {:label "Markdown" :transport :multipart       :ceiling :document
    :extensions [".md" ".markdown"]}
   {:label "Audio"    :transport :multipart       :ceiling :audio
    :extensions [".mp3" ".m4a" ".wav" ".flac" ".ogg" ".oga" ".mpeg" ".mpga"]}
   {:label "Video"    :transport :video-session   :ceiling :video
    :extensions [".mp4" ".webm" ".mov" ".mkv" ".ogv" ".qt"]}
   {:label "Archive"  :transport :archive-session :ceiling :archive
    :extensions [".zip" ".7z"]}])

(def ^:private chip-limit
  "How many chips the intake area shows before `+ more` (spec §6.3)."
  6)

(defn chips
  "Post: {:labels [S…] :more N} — at most `chip-limit` labels, and how many
   groups the row does not name."
  []
  {:labels (mapv :label (take chip-limit groups))
   :more   (max 0 (- (count groups) chip-limit))})

(defn extension-of
  "Post: the lower-case extension of `filename` including the dot, or nil when
   the name carries none."
  [filename]
  (when filename
    (let [lower (str/lower-case filename)
          dot (str/last-index-of lower ".")]
      (when (and dot (pos? dot))
        (subs lower dot)))))

(defn group-for
  "The registry group that claims `filename`, or nil.

   Pre : `filename` is a string or nil.
   Post: nil means the import page accepts no such file — the caller owes the
         user the message in `rejection-message`, and MUST NOT upload."
  [filename]
  (when-some [ext (extension-of filename)]
    (first (filter #(some #{ext} (:extensions %)) groups))))

(defn transport-for
  "Post: :multipart | :video-session | :archive-session | nil (not accepted)."
  [filename]
  (:transport (group-for filename)))

(defn accept-attr
  "The `accept` value for a file dialog restricted to `labels`.

   Pre : every label names a group. Post: a comma-joined extension list.
   A dialog `accept` is a hint the user can switch off in every OS, so the drop
   router — not this string — is what actually holds the accepted set."
  [labels]
  (let [wanted (set labels)]
    (->> groups
      (filter #(contains? wanted (:label %)))
      (mapcat :extensions)
      (str/join ","))))

(defn accepted-summary
  "One line naming every accepted group, for the rejection message (§6.8)."
  []
  (str/join ", " (map :label groups)))

(defn rejection-message
  "The message for a file the page cannot read.

   Post: names the cause and the next step, and carries no apology (§12.1)."
  [filename]
  (str "FreeMemo cannot read this file type"
    (when-some [ext (extension-of filename)] (str " (" ext ")"))
    ". Accepted types: " (accepted-summary) "."))

;; ── Ceilings ───────────────────────────────────────────────────────

(defn ceilings*
  "The four upload ceilings that apply to this user, in bytes.

   One server round-trip for all four rather than one per group: the intake
   area's resting line names every ceiling (§6.6.1) and the drop router checks
   one of them per file, so both callers need the whole map.

   Pre : `user-id` owns the session. Post: {:document N :audio N :video N
         :archive N}, every value positive. 0 from `upload_max_bytes` means
         unlimited and is returned as-is — `refusal-for` treats a non-positive
         ceiling as no ceiling.
   Named in a reactive body through `e/server`, so the var must exist on CLJS."
  [_refresh user-id]
  #?(:clj {:document (long (quota/get-user-upload-max db/ds user-id))
           :audio    (long audio/max-bytes)
           :video    (long video/max-video-bytes)
           :archive  (long archive/max-archive-bytes)}
     :cljs nil))

(defn ceiling-for
  "The ceiling in `ceilings` that applies to `filename`, or nil.

   Pre : `ceilings` is a `ceilings*` result. Post: a byte count, or nil when the
         file is not accepted or the ceiling is unlimited."
  [ceilings filename]
  (when-some [k (:ceiling (group-for filename))]
    (let [n (get ceilings k)]
      (when (and (number? n) (pos? n)) n))))
