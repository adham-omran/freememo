(ns freememo.transcribe-language
  "The set of spoken languages transcription accepts, as shared vocabulary.

   `.cljc` because both peers need it and for different reasons: the server
   validates a saved code against it (`settings/save-transcribe-language`), the
   client renders it as `<option>`s (`ai-features-section/TranscribeLanguageField`).

   It lived in `settings.clj` first, and that CRASHED the Settings page. A
   CLJ-only var read from client scope compiles without a warning but emits
   nothing on the CLJS peer, so the two peers disagree on the frame's shape and
   the mismatch blows up in `runtime3/socket-transfer` when the page mounts
   (patterns.md: \"Mismatched compilations produce corrupted nonsense over the
   wire\"). Keeping the list here makes the options plain client literals — the
   same shape as `ScanDpiField`'s inline options — and nothing crosses the wire.")

(def options
  "Offered languages as [code label], in menu order. `nil` code = auto-detect.
   Codes are ISO-639-1, which is what the Whisper API accepts.

   Auto-detect is first and is the default: it is right on most material. It is
   overridable because when it is wrong it does not degrade gracefully — the
   model commits to the language it guessed and writes fluent text in it."
  [[nil  "Auto-detect"]
   ["en" "English"]
   ["ar" "Arabic"]
   ["fr" "French"]
   ["de" "German"]
   ["es" "Spanish"]
   ["it" "Italian"]
   ["pt" "Portuguese"]
   ["ru" "Russian"]
   ["tr" "Turkish"]
   ["zh" "Chinese"]
   ["ja" "Japanese"]
   ["ko" "Korean"]
   ["hi" "Hindi"]])

(def codes
  "The non-nil codes, for validating a stored or submitted value."
  (into #{} (keep first) options))

(defn code?
  "Whether `v` is an offered language code. Pre: v is a string or nil.
   Post: false for nil, blank, and anything not in `codes`."
  [v]
  (contains? codes v))
