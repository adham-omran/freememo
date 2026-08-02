(ns freememo.archive
  "Safely explode an uploaded archive to a temp directory.

   Two import flows need this — code repositories and SuperMemo collections —
   and the interesting part is the guards, not the copying: path-traversal
   containment, entry count, per-entry bytes, total uncompressed bytes. A
   second copy of those guards is a second place for them to drift, so they
   live here and both flows call in.

   Two containers are supported. ZIP is handled by java.util.zip; 7z by
   commons-compress, because SuperMemo collections are commonly distributed
   as .7z and java.util.zip cannot read them. Both go through the same guards
   and the same traversal check — the container is a detail behind
   `extract-to-temp-dir!`.

   NEVER evaluates, parses or interprets an extracted file."
  (:require [clojure.java.io :as io])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream File]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipEntry ZipInputStream]
           [org.apache.commons.compress.archivers.sevenz SevenZArchiveEntry SevenZFile]
           [org.apache.commons.compress.utils SeekableInMemoryByteChannel]))

(def default-limits
  "Caps that make a hostile archive fail fast instead of filling the disk.
   :max-total-bytes is the one worth overriding — a code repo is small, a
   SuperMemo collection is routinely over 150 MB uncompressed."
  {:max-entries 50000
   :max-entry-bytes (* 20 1024 1024)
   :max-total-bytes (* 300 1024 1024)})

(defn delete-dir!
  "Recursively delete `dir`. Best-effort; never throws."
  [^File dir]
  (when (and dir (.exists dir))
    (doseq [^File f (reverse (file-seq dir))]
      (try (.delete f) (catch Exception _ nil)))))

;; ── Container detection ────────────────────────────────────────────

(defn- magic?
  "Compare leading bytes against `sig`, unsigned. JVM bytes are signed, so
   the array side is masked rather than the signature coerced — 0xBC in the
   7z signature is not a valid signed byte."
  [^bytes b sig]
  (and b (>= (alength b) (count sig))
    (every? (fn [[i v]] (= (long v) (bit-and (aget b (int i)) 0xFF)))
      (map-indexed vector sig))))

(defn archive-kind
  "Post: :zip, :sevenz, or nil when the bytes are neither."
  [^bytes b]
  (cond
    (magic? b [0x50 0x4B 0x03 0x04]) :zip
    (magic? b [0x37 0x7A 0xBC 0xAF 0x27 0x1C]) :sevenz
    :else nil))

;; ── Entry listing (no decompression) ───────────────────────────────

(defn entry-names
  "Names of every file entry, without extracting or decompressing anything.
   Pre : `bytes` is an archive; callers that care should check `archive-kind`.
   Post: a vector of entry names; empty when the bytes are unreadable or the
         container is unrecognised. Never throws — callers use this to
         classify an upload, where an unreadable archive is a routine answer
         (\"not this flow\") rather than an error."
  [^bytes bytes]
  (try
    (case (archive-kind bytes)
      :zip (with-open [zin (ZipInputStream. (ByteArrayInputStream. bytes))]
             (loop [acc []]
               (if-let [^ZipEntry e (.getNextEntry zin)]
                 (recur (if (.isDirectory e) acc (conj acc (.getName e))))
                 acc)))
      :sevenz (with-open [sz (-> (SevenZFile/builder)
                               (.setSeekableByteChannel (SeekableInMemoryByteChannel. bytes))
                               (.get))]
                (loop [acc []]
                  (if-let [^SevenZArchiveEntry e (.getNextEntry sz)]
                    (recur (if (.isDirectory e) acc (conj acc (.getName e))))
                    acc)))
      [])
    (catch Exception _ [])))

;; ── Extraction ─────────────────────────────────────────────────────

(defn- safe-target
  "File for `entry-name` under `dir`, or nil if it escapes `dir` (path
   traversal / absolute path). Canonical-path containment is the real guard.
   Pre: dir a File. Post: a File strictly under dir, or nil."
  [^File dir ^String entry-name]
  (let [f (File. dir entry-name)
        base (str (.getCanonicalPath dir) File/separator)]
    (when (.startsWith (.getCanonicalPath f) base) f)))

(defn- copy-entry!
  "Drain the current entry into `target`, enforcing the per-entry cap.
   Pre : `read-chunk!` fills the supplied buffer from the CURRENT entry and
         returns the byte count, or -1 at the entry's end; `target`'s parents
         may not exist yet.
   Post: returns the byte count written.
   Throws ex-info {:type ::archive-too-large} past `max-entry-bytes`, before
   the bytes reach disk."
  [read-chunk! ^File target ^long max-entry-bytes entry-name]
  (let [baos (ByteArrayOutputStream.)
        buf (byte-array 8192)
        written (loop [w 0]
                  (let [r (long (read-chunk! buf))]
                    (if (pos? r)
                      (let [w2 (+ w r)]
                        (when (> w2 max-entry-bytes)
                          (throw (ex-info "Archive entry too large"
                                   {:type ::archive-too-large :entry entry-name})))
                        (.write baos buf 0 r)
                        (recur w2))
                      w)))]
    (io/make-parents target)
    (io/copy (.toByteArray baos) target)
    written))

(defn- skip-entry!
  "Drain the current entry without writing it. Required for 7z: both
   containers read entries from one sequential stream, so an entry that is
   filtered out must still be consumed before advancing."
  [read-chunk!]
  (let [buf (byte-array 8192)]
    (loop [] (when (pos? (long (read-chunk! buf))) (recur)))))

(defn- extract-entries!
  "Shared guard loop over an archive's entries.
   Pre : `next-entry!` returns [name directory?] or nil at end;
         `read-chunk!` reads from the entry `next-entry!` last returned.
   Post: returns `dir`.
   Invariant: entries are read strictly in stream order. 7z archives from
         SuperMemo are solid LZMA2 — random access via getInputStream would
         re-decompress the whole block per entry, turning a 26 MB archive
         into minutes of CPU."
  [^File dir {:keys [keep? max-entries max-entry-bytes max-total-bytes]} next-entry! read-chunk!]
  (loop [n-entries 0, total 0]
    (when (> n-entries (long max-entries))
      (throw (ex-info "Archive has too many entries" {:type ::archive-too-large})))
    (if-let [[nm directory?] (next-entry!)]
      (let [target (when-not directory? (safe-target dir nm))]
        (if (or (nil? target) (not (keep? nm)))
          (do (when-not directory? (skip-entry! read-chunk!))
              (recur (inc n-entries) total))
          (let [written (copy-entry! read-chunk! target max-entry-bytes nm)
                total2 (+ total written)]
            (when (> total2 (long max-total-bytes))
              (throw (ex-info "Archive too large uncompressed" {:type ::archive-too-large})))
            (recur (inc n-entries) total2))))
      dir)))

(defn extract-to-temp-dir!
  "Extract `archive-bytes` (ZIP or 7z) into a fresh temp directory.
   Pre : `archive-bytes` is a ZIP or 7z archive. `opts` may carry :prefix
         (temp dir name prefix), :keep? (predicate on the entry name — only
         matching entries are written; default: everything), and any of
         `default-limits`.
   Post: a temp dir File holding the extracted tree. On ANY failure the temp
         dir is removed and the error rethrown, so a caller never inherits a
         half-extracted directory.
   Invariant: the caller owns the returned dir and MUST delete it — this fn
         cannot know when the caller has finished reading from it.
   Throws ex-info {:type ::archive-too-large} when a cap is exceeded, and
   {:type ::unsupported-archive} when the bytes are neither ZIP nor 7z."
  [^bytes archive-bytes opts]
  (let [{:keys [prefix] :as cfg}
        (merge default-limits {:prefix "archive" :keep? (constantly true)} opts)
        kind (archive-kind archive-bytes)
        _ (when-not kind
            (throw (ex-info "Unsupported archive format (expected ZIP or 7z)"
                     {:type ::unsupported-archive})))
        dir (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (try
      (case kind
        :zip
        (with-open [zin (ZipInputStream. (ByteArrayInputStream. archive-bytes))]
          (extract-entries! dir cfg
            (fn [] (when-let [^ZipEntry e (.getNextEntry zin)]
                     [(.getName e) (.isDirectory e)]))
            (fn [^bytes buf] (.read zin buf))))

        :sevenz
        (with-open [sz (-> (SevenZFile/builder)
                         (.setSeekableByteChannel (SeekableInMemoryByteChannel. archive-bytes))
                         (.get))]
          (extract-entries! dir cfg
            (fn [] (when-let [^SevenZArchiveEntry e (.getNextEntry sz)]
                     [(.getName e) (.isDirectory e)]))
            (fn [^bytes buf] (.read sz buf)))))
      dir
      (catch Throwable t
        (delete-dir! dir)
        (throw t)))))
