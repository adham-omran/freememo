(ns freememo.import-types
  "What the import page accepts, and which transport carries it.

   The single definition of the accepted extension set. Before this namespace the
   set existed four times — `import-modal`'s document and audio `accept` strings,
   `video-import-modal/accepted-extensions`, and `archive-upload/archive-file?` —
   and nothing kept them in agreement. The intake area's chips, the drop router,
   each card's file dialog, and the rejection message now all read from here, so
   adding an extension is one edit.

   `:transport` is the protocol that carries the bytes, NOT the server's import
   flow. Four exist, and they are genuinely different protocols:

     :multipart        one POST to /api/upload-file, capped by one HTTP request
     :video-session    init → chunk* → finalize against /api/video/*
     :archive-session  the same shape against /api/archive/*
     :epub-session     the same shape against /api/epub/*

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
   [freememo.video-format :refer [format-bytes]]
   #?(:clj [freememo.archive :as archive])
   #?(:clj [freememo.epub :as epub])
   #?(:clj [freememo.audio :as audio])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.video :as video])))

;; ── The registry ───────────────────────────────────────────────────

(def groups
  "Every accepted extension, grouped by what the user calls it.

   Order is display order: the intake area shows one chip per group, all of them.
   `:ceiling` names which of the five ceilings `ceilings*` returns applies to the
   group — the numbers themselves are per-user or env-configured, so they are read
   at runtime, never inlined here."
  [{:label "PDF"      :transport :multipart       :ceiling :document
    :extensions [".pdf"]}
   ;; Chunked, like video and archive: `/api/upload-file` reads its whole body
   ;; into a byte[] and one request is capped at `request-max-bytes`, and a large
   ;; book clears neither. Its own `:ceiling` rather than `:document`, because
   ;; PDF still goes through multipart and would be understated by a shared
   ;; number. See plans/large-epub-import.md §7.6.
   {:label "EPUB"     :transport :epub-session    :ceiling :epub
    :extensions [".epub"]}
   {:label "HTML"     :transport :multipart       :ceiling :document
    :extensions [".html" ".htm"]}
   {:label "Markdown" :transport :multipart       :ceiling :document
    :extensions [".md" ".markdown"]}
   {:label "Audio"    :transport :multipart       :ceiling :audio
    :extensions [".mp3" ".m4a" ".wav" ".flac" ".ogg" ".oga" ".mpeg" ".mpga"]}
   {:label "Video"    :transport :video-session   :ceiling :video
    :extensions [".mp4" ".webm" ".mov" ".mkv" ".ogv" ".qt"]}
   ;; Named for both containers it accepts. "Archive" would be the category, and
   ;; the Code card already says ".zip" — the chip row must use the words the rest
   ;; of the interface uses.
   {:label "ZIP/7z"   :transport :archive-session :ceiling :archive
    :extensions [".zip" ".7z"]}])

(defn chips
  "One chip per accepted group, each carrying the ceiling that governs it.

   Pre : `ceilings` is a `ceilings*` result, or nil before it arrives.
   Post: a vector of {:label S :limit S-or-nil}, one entry per group, in display
         order. NOTHING is elided behind a `+ more`: a format the interface names
         elsewhere — the Code card names .zip — has to appear in the row whose job
         is to enumerate formats.
   Invariant: `:limit` is nil only when that ceiling is unlimited or has not
         arrived yet, so a chip never shows a wrong number while loading.

   The limit rides the chip rather than a sentence below the row, so each number
   sits beside the type it governs. A single list of five ceilings reads as a
   comparison, and 25 MB next to 8 GB invites the wrong conclusion."
  [ceilings]
  (mapv (fn [{:keys [label ceiling]}]
          {:label label
           :limit (when-some [n (get ceilings ceiling)]
                    (when (pos? n) (format-bytes n)))})
    groups))

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
  "Post: :multipart | :video-session | :archive-session | :epub-session |
   nil (not accepted)."
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

(defn tighter-cap
  "The tighter of two ceilings, where 0 means unlimited on either side.

   Pre : both are non-negative byte counts. Post: 0 only when BOTH are 0.
   Exists because a ceiling with two sources drifts. `upload_max_bytes` and
   `STORAGE_REQUEST_MAX_BYTES` are both real bounds on a multipart upload, and
   reporting either one alone is how production came to advertise no limit while
   rejecting at 100 MB — see plans/large-epub-import.md §2.1."
  [a b]
  (cond
    (zero? a) b
    (zero? b) a
    :else (min a b)))

(defn ceilings*
  "The five upload ceilings that apply to this user, in bytes.

   One server round-trip for all five rather than one per group: the intake
   area's resting line names every ceiling (§6.6.1) and the drop router checks
   one of them per file, so both callers need the whole map.

   Pre : `user-id` owns the session. Post: {:document N :audio N :video N
         :archive N :epub N}. 0 means unlimited and is returned as-is — `ceiling-for`
         treats a non-positive ceiling as no ceiling.
   Invariant: every value is a ceiling some layer WILL enforce. A reported number
         no layer enforces is the defect this fn exists to prevent.

   `:document` and `:audio` take the tighter of their own cap and the
   single-request transport cap, because a multipart upload is one HTTP request
   and `wrap-route-body-size` rejects past that bound. `:video` and `:archive`
   deliberately do NOT: their transport is many requests, so the single-request
   cap does not bound them — the reasoning `video/max-video-bytes` records.
   Named in a reactive body through `e/server`, so the var must exist on CLJS."
  [_refresh user-id]
  #?(:clj (let [request-max (long quota/request-max-bytes)]
            {:document (tighter-cap (long (quota/get-user-upload-max db/ds user-id))
                         request-max)
             :audio    (tighter-cap (long audio/max-bytes) request-max)
             :video    (long video/max-video-bytes)
             :archive  (long archive/max-archive-bytes)
             :epub     (long epub/max-epub-bytes)})
     :cljs nil))

(defn ceiling-for
  "The ceiling in `ceilings` that applies to `filename`, or nil.

   Pre : `ceilings` is a `ceilings*` result. Post: a byte count, or nil when the
         file is not accepted or the ceiling is unlimited."
  [ceilings filename]
  (when-some [k (:ceiling (group-for filename))]
    (let [n (get ceilings k)]
      (when (and (number? n) (pos? n)) n))))
