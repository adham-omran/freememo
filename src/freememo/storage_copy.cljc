(ns freememo.storage-copy
  "Storage explanations shared by the configured Settings panels.")

(defn credit-spend [rate-label]
  (if rate-label
    (str "Credits pay for hosted AI features and stored library files. Storage costs " rate-label ".")
    "Credits pay for hosted AI features."))

(defn storage-usage [rate-label]
  (str "Uploaded PDFs, EPUBs, audio files, and videos use storage. "
    (when rate-label "Stored library files are billed at this rate while you keep them. ")
    "Delete documents from the Library to free space."))
