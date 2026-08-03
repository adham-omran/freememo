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

   **Source is a byte[] OR a File.** Every public fn takes either. The File
   arities exist because a chunked upload lands multi-gigabyte archives that
   cannot be represented as a byte[] at all: a JVM array is int-indexed, so
   2 GiB is the hard ceiling, and the prod heap (2.1 GiB) puts the practical
   limit far below that. `read-source` is the single place that branch lives.

   NEVER evaluates, parses or interprets an extracted file."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io BufferedOutputStream ByteArrayInputStream File FileInputStream
            FileOutputStream InputStream OutputStream]
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

(def ^:const copy-buffer-bytes
  "Transfer granularity when draining one entry to disk. Matches
   `largeobj/io-buffer-bytes` so a large-object round trip and an extraction
   move bytes in the same size steps."
  262144)

(defn delete-dir!
  "Recursively delete `dir`. Best-effort; never throws."
  [^File dir]
  (when (and dir (.exists dir))
    (doseq [^File f (reverse (file-seq dir))]
      (try (.delete f) (catch Exception _ nil)))))

;; ── Source handling ────────────────────────────────────────────────
;; A source is a byte[] or a File. Nothing else. Keeping the branch in these
;; two fns is what lets every public fn below be source-agnostic.

(defn- magic-prefix
  "Bytes for a magic-number test: at least `n` of them, or fewer if that is all
   there is. nil when `src` is unreadable.
   Pre : src is a byte[] or a File; n > 0.
   Post: for a byte[], the array itself — every caller reads only a prefix, so
         copying would be waste. For a File, a fresh array of at most `n` bytes;
         the read touches only those bytes, because classifying a 7.5 GB
         archive must not read 7.5 GB."
  ^bytes [src ^long n]
  (cond
    (bytes? src)
    src

    (instance? File src)
    (try
      (with-open [in (FileInputStream. ^File src)]
        (let [buf (byte-array n)
              read (.read ^InputStream in buf 0 (int n))]
          (if (pos? read)
            (if (= read n) buf (java.util.Arrays/copyOf buf read))
            (byte-array 0))))
      (catch Exception _ nil))

    :else nil))

(defn- magic?
  "Compare leading bytes against `sig`, unsigned. JVM bytes are signed, so
   the array side is masked rather than the signature coerced — 0xBC in the
   7z signature is not a valid signed byte."
  [^bytes b sig]
  (and b (>= (alength b) (count sig))
    (every? (fn [[i v]] (= (long v) (bit-and (aget b (int i)) 0xFF)))
      (map-indexed vector sig))))

(defn archive-kind
  "Post: :zip, :sevenz, or nil when the source is neither.
   Pre : `src` is a byte[] or a File."
  [src]
  (let [b (magic-prefix src 6)]
    (cond
      (magic? b [0x50 0x4B 0x03 0x04]) :zip
      (magic? b [0x37 0x7A 0xBC 0xAF 0x27 0x1C]) :sevenz
      :else nil)))

(defn- read-source
  "Call `f` with [next-entry! read-chunk!] over `src`, then close the reader.

   Pre : `src` is a byte[] or a File holding a ZIP or 7z archive; `kind` is
         `archive-kind`'s answer for it.
   Post: `f`'s return value. `next-entry!` returns [name directory?] or nil at
         the end; `read-chunk!` fills a caller-supplied buffer from the entry
         `next-entry!` last returned, and returns -1 at that entry's end.
   Invariant: entries are read strictly in stream order for BOTH containers.
         SuperMemo 7z archives are solid LZMA2, where
         `SevenZFile.getInputStream` per entry re-decompresses the whole block
         — that turned a 26 MB archive into more than ten minutes.
   Invariant: a File source is never materialized into heap. ZIP reads through
         a FileInputStream; 7z through `SevenZFile$Builder.setFile`, which
         seeks on the file rather than on an in-memory channel."
  [src kind f]
  (case kind
    :zip
    (with-open [in ^InputStream (if (bytes? src)
                                  (ByteArrayInputStream. ^bytes src)
                                  (FileInputStream. ^File src))
                zin (ZipInputStream. in)]
      (f (fn [] (when-let [^ZipEntry e (.getNextEntry zin)]
                  [(.getName e) (.isDirectory e)]))
        (fn [^bytes buf] (.read zin buf))))

    :sevenz
    (with-open [sz ^SevenZFile (cond-> (SevenZFile/builder)
                                 (bytes? src)
                                 (.setSeekableByteChannel
                                   (SeekableInMemoryByteChannel. ^bytes src))

                                 (instance? File src)
                                 (.setFile ^File src)

                                 :always (.get))]
      (f (fn [] (when-let [^SevenZArchiveEntry e (.getNextEntry sz)]
                  [(.getName e) (.isDirectory e)]))
        (fn [^bytes buf] (.read sz buf))))))

;; ── Entry listing (no decompression) ───────────────────────────────

(defn entry-names
  "Names of every file entry, without extracting or decompressing anything.
   Pre : `src` is a byte[] or a File; callers that care should check
         `archive-kind`.
   Post: a vector of entry names; empty when the source is unreadable or the
         container is unrecognised. Never throws — callers use this to
         classify an upload, where an unreadable archive is a routine answer
         (\"not this flow\") rather than an error."
  [src]
  (try
    (if-let [kind (archive-kind src)]
      (read-source src kind
        (fn [next-entry! _read-chunk!]
          (loop [acc []]
            (if-let [[nm directory?] (next-entry!)]
              (recur (if directory? acc (conj acc nm)))
              acc))))
      [])
    (catch Exception _ [])))

(defn every-entry-name?
  "Whether every predicate in `preds` is satisfied by some file entry name.

   Exists so classification does not pay for a full entry walk when it does
   not have to: the walk stops as soon as the last outstanding predicate is
   satisfied. On a SuperMemo collection whose `info/` entries sit early in the
   archive this returns after a handful of headers instead of all of them.

   Pre : `src` is a byte[] or a File; `preds` is a non-empty collection of
         predicates on a lower-cased entry name.
   Post: true iff each predicate matched at least one entry name. False when
         the source is unreadable or the container is unrecognised.
   Invariant: never throws; never decompresses entry data."
  [src preds]
  (try
    (if-let [kind (archive-kind src)]
      (read-source src kind
        (fn [next-entry! _read-chunk!]
          (loop [outstanding (set preds)]
            (if (empty? outstanding)
              true
              (if-let [[nm directory?] (next-entry!)]
                (recur (if directory?
                         outstanding
                         (let [lower (str/lower-case nm)]
                           (into #{} (remove #(% lower)) outstanding))))
                false)))))
      false)
    (catch Exception _ false)))

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
  "Drain the current entry straight into `target`, enforcing the per-entry cap.

   Pre : `read-chunk!` fills the supplied buffer from the CURRENT entry and
         returns the byte count, or -1 at the entry's end; `target`'s parents
         may not exist yet.
   Post: returns the byte count written.
   Throws ex-info {:type ::archive-too-large} past `max-entry-bytes`, having
   written at most that many bytes. The caller deletes the whole temp dir on
   any throw, so a partial file is not observable.
   Invariant: heap high-water is one `copy-buffer-bytes` buffer, independent
   of entry size. Buffering the entry first (which this used to do) made the
   per-entry cap the only thing standing between a large member and the heap."
  [read-chunk! ^File target ^long max-entry-bytes entry-name]
  (io/make-parents target)
  (with-open [out (BufferedOutputStream. (FileOutputStream. target))]
    (let [buf (byte-array copy-buffer-bytes)]
      (loop [w 0]
        (let [r (long (read-chunk! buf))]
          (if (pos? r)
            (let [w2 (+ w r)]
              (when (> w2 max-entry-bytes)
                (throw (ex-info "Archive entry too large"
                         {:type ::archive-too-large :entry entry-name})))
              (.write ^OutputStream out buf 0 (int r))
              (recur w2))
            (do (.flush ^OutputStream out) w)))))))

(defn- skip-entry!
  "Drain the current entry without writing it. Required for 7z: both
   containers read entries from one sequential stream, so an entry that is
   filtered out must still be consumed before advancing."
  [read-chunk!]
  (let [buf (byte-array copy-buffer-bytes)]
    (loop [] (when (pos? (long (read-chunk! buf))) (recur)))))

(defn- extract-entries!
  "Shared guard loop over an archive's entries.
   Pre : `next-entry!` returns [name directory?] or nil at end;
         `read-chunk!` reads from the entry `next-entry!` last returned.
   Post: returns `dir`."
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
  "Extract `src` (ZIP or 7z, byte[] or File) into a fresh temp directory.
   Pre : `src` is a ZIP or 7z archive. `opts` may carry :prefix (temp dir name
         prefix), :keep? (predicate on the entry name — only matching entries
         are written; default: everything), and any of `default-limits`.
   Post: a temp dir File holding the extracted tree. On ANY failure the temp
         dir is removed and the error rethrown, so a caller never inherits a
         half-extracted directory.
   Invariant: the caller owns the returned dir and MUST delete it — this fn
         cannot know when the caller has finished reading from it.
   Invariant: heap high-water is one copy buffer, whatever `src` is.
   Throws ex-info {:type ::archive-too-large} when a cap is exceeded, and
   {:type ::unsupported-archive} when the source is neither ZIP nor 7z."
  [src opts]
  (let [{:keys [prefix] :as cfg}
        (merge default-limits {:prefix "archive" :keep? (constantly true)} opts)
        kind (archive-kind src)
        _ (when-not kind
            (throw (ex-info "Unsupported archive format (expected ZIP or 7z)"
                     {:type ::unsupported-archive})))
        dir (.toFile (Files/createTempDirectory prefix (make-array FileAttribute 0)))]
    (try
      (read-source src kind
        (fn [next-entry! read-chunk!]
          (extract-entries! dir cfg next-entry! read-chunk!)))
      dir
      (catch Throwable t
        (delete-dir! dir)
        (throw t)))))
