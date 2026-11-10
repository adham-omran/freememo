(ns freememo.local-files
  "File helpers of the Local Install launcher."
  (:import [java.io File]
           [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

(defn write-private!
  "Post: `f` holds `text` and only its owner can read it (0600 where the OS has it).
         The write goes to a temporary file first and moves over `f` in one
         step, so a crash leaves the old file or the new one, never half."
  [^File f text]
  (let [path (.toPath f)
        tmp (.resolveSibling path (str (.getFileName path) ".tmp"))]
    (Files/deleteIfExists tmp)
    (try
      (Files/createFile tmp (into-array FileAttribute
                              [(PosixFilePermissions/asFileAttribute (PosixFilePermissions/fromString "rw-------"))]))
      (catch UnsupportedOperationException _
        (Files/createFile tmp (make-array FileAttribute 0))))
    (spit (.toFile tmp) text)
    (try
      (Files/move tmp path (into-array java.nio.file.CopyOption
                             [StandardCopyOption/ATOMIC_MOVE StandardCopyOption/REPLACE_EXISTING]))
      (catch java.nio.file.AtomicMoveNotSupportedException _
        (Files/move tmp path (into-array java.nio.file.CopyOption [StandardCopyOption/REPLACE_EXISTING]))))))
