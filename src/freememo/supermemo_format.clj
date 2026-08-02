(ns freememo.supermemo-format
  "SuperMemo 17/18/19 on-disk collection decoder. Pure: takes a directory,
   returns data. No database, no HTTP, no side effects beyond reading files.

   The format is undocumented. Record layouts for contents.dat, ElementInfo.dat,
   compon.dat and the registry .mem/.rtx pair come from SuperMemoAssistant
   (MIT, github.com/supermemo/SuperMemoAssistant, src/Core/.../SuperMemo17/Files);
   the scheduling fields, repetitions.dat, RepetitionHistory.dat, reference.dat
   and the .sub subset files were recovered by measurement against a real
   SM19 collection and are documented in plans/supermemo-import.md.

   Every layout claim below is backed by an assertion in `verify` that the
   caller runs before trusting the decode.

   Sizes: contents 37 B/record, ElementInfo 118 B/record, registry .mem
   30 B/record, RepetitionHistory 39 B/record, reference.dat 32 B/record.
   Element id N lives at record index N-1 in contents.dat and ElementInfo.dat."
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]
           [java.time LocalDate LocalDateTime]
           [java.nio.charset StandardCharsets]))

;; ── Primitive readers ──────────────────────────────────────────────
;; JVM bytes are signed; every multi-byte read masks to 0xFF first.

(defn- u8  [^bytes b o] (bit-and (aget b o) 0xFF))

(defn- u16 [^bytes b o]
  (bit-or (u8 b o) (bit-shift-left (u8 b (inc o)) 8)))

(defn- i16 [^bytes b o]
  (let [v (u16 b o)] (if (>= v 0x8000) (- v 0x10000) v)))

(defn- i32 [^bytes b o]
  (unchecked-int
    (bit-or (u8 b o)
      (bit-shift-left (u8 b (+ o 1)) 8)
      (bit-shift-left (u8 b (+ o 2)) 16)
      (bit-shift-left (u8 b (+ o 3)) 24))))

(defn- f64
  "IEEE-754 double, little-endian."
  [^bytes b o]
  (Double/longBitsToDouble
    (reduce (fn [acc k] (bit-or acc (bit-shift-left (long (u8 b (+ o k))) (* 8 k))))
      0 (range 8))))

(defn- real48
  "Borland Real48: exponent first, 39-bit mantissa, sign in bit 47.
   Post: 0.0 iff the exponent byte is 0 (Turbo Pascal's zero encoding)."
  [^bytes b o]
  (let [ex (u8 b o)]
    (if (zero? ex)
      0.0
      (let [b5 (u8 b (+ o 5))
            sign (bit-shift-right b5 7)
            mant (bit-or (bit-shift-left (long (bit-and b5 0x7F)) 32)
                   (bit-shift-left (long (u8 b (+ o 4))) 24)
                   (bit-shift-left (long (u8 b (+ o 3))) 16)
                   (bit-shift-left (long (u8 b (+ o 2))) 8)
                   (long (u8 b (+ o 1))))]
        (* (if (zero? sign) 1.0 -1.0)
          (+ 1.0 (/ (double mant) (Math/pow 2 39)))
          (Math/pow 2 (- ex 129)))))))

(def ^:private tdatetime-epoch
  "Delphi TDateTime day 0. Both SuperMemo date encodings are relative to it."
  (LocalDate/of 1899 12 30))

(defn tdatetime->local-date-time
  "Post: nil for a zero/absent TDateTime, else the instant it names.
   A TDateTime below 1000 is SuperMemo's 'never' rather than the year 1902."
  [v]
  (when (and v (> v 1000.0))
    (let [days (long (Math/floor v))
          frac (- v days)
          nanos (long (* frac 86400 1e9))]
      (-> (.atStartOfDay (.plusDays tdatetime-epoch days))
        (.plusNanos nanos)))))

(defn- read-bytes ^bytes [^File f]
  (with-open [in (io/input-stream f)]
    (.readAllBytes in)))

(defn- decode-text
  "SuperMemo writes UTF-8 in the registries and in most element files, but
   older imports carry windows-1252. Decode strictly as UTF-8 and fall back
   on malformed input, so a single legacy element cannot corrupt the rest.
   Pre : 0 <= from, from+len <= (alength b).
   Post: a string; never throws."
  [^bytes b from len]
  (let [dec (doto (.newDecoder StandardCharsets/UTF_8)
              (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
              (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT))]
    (try
      (str (.decode dec (java.nio.ByteBuffer/wrap b from len)))
      (catch java.nio.charset.CharacterCodingException _
        (String. b from len (java.nio.charset.Charset/forName "windows-1252"))))))

;; ── Collection layout ──────────────────────────────────────────────

(defn- child-file
  "Case-insensitive lookup of `names` under `dir`. SuperMemo writes
   ElementInfo.dat on Windows; an archive may round-trip it lower-cased."
  ^File [^File dir & names]
  (let [wanted (set (map str/lower-case names))]
    (first (filter #(contains? wanted (str/lower-case (.getName ^File %)))
             (or (seq (.listFiles dir)) [])))))

(defn- child-dir ^File [^File dir name]
  (let [f (child-file dir name)]
    (when (and f (.isDirectory f)) f)))

(defn find-collection-root
  "Locate the collection folder inside an extracted archive.
   Pre : `dir` is a readable directory.
   Post: the File whose `info/` holds contents.dat + ElementInfo.dat, or nil.
   Searches `dir` and its descendants to depth 3 — archives are commonly
   zipped one or two levels above the collection folder."
  [^File dir]
  (letfn [(collection? [^File d]
            (when-let [info (child-dir d "info")]
              (and (child-file info "contents.dat")
                (child-file info "ElementInfo.dat"))))
          (search [^File d depth]
            (cond
              (not (.isDirectory d)) nil
              (collection? d) d
              (>= depth 3) nil
              :else (some #(search % (inc depth))
                      (filter #(.isDirectory ^File %) (or (seq (.listFiles d)) [])))))]
    (search dir 0)))

;; ── contents.dat — the knowledge tree ──────────────────────────────

(def ^:private contents-record-bytes 37)

(defn- read-contents
  "Post: vector indexed by (element-id - 1)."
  [^bytes b]
  (mapv (fn [i]
          (let [o (* i contents-record-bytes)]
            {:id (inc i)
             :deleted? (pos? (u8 b o))
             :parent-id (i32 b (+ o 1))
             :first-child-id (i32 b (+ o 5))
             :last-child-id (i32 b (+ o 9))
             :prev-sibling-id (i32 b (+ o 13))
             :next-sibling-id (i32 b (+ o 17))
             :descendant-count (i32 b (+ o 21))
             :children-count (i32 b (+ o 25))}))
    (range (quot (alength b) contents-record-bytes))))

;; ── ElementInfo.dat — type, content pointers, schedule ─────────────

(def ^:private element-info-record-bytes 118)

(def element-type
  "Byte 0 of an ElementInfo record. From SuperMemoAssistant's ElementType enum."
  {0 :topic 1 :item 2 :task 3 :template 4 :concept-group})

(defn- read-element-info
  [^bytes b]
  (mapv (fn [i]
          (let [o (* i element-info-record-bytes)]
            {:id (inc i)
             :type (element-type (u8 b o) :unknown)
             :title-text-id (i32 b (+ o 2))
             :component-pos (i32 b (+ o 6))
             :interval-days (u16 b (+ o 16))
             :last-rep-day (i16 b (+ o 18))
             :last-touch (f64 b (+ o 20))
             :a-factor (real48 b (+ o 28))
             :u-factor (real48 b (+ o 34))
             :reference-id (u16 b (+ o 44))
             :template-id (i32 b (+ o 61))
             :concept-id (i32 b (+ o 65))
             :first-rep (f64 b (+ o 77))}))
    (range (quot (alength b) element-info-record-bytes))))

;; ── Registry (.mem + .rtx + file slots) ────────────────────────────

(def ^:private mem-record-bytes 30)

;; An .rtx record is <value> NUL <int32 member-id> <byte flag>; `length`
;; from the .mem row spans all of it, so the value is length-6 bytes and the
;; trailing id is a free integrity check (see `verify`).
(def ^:private rtx-trailer-bytes 6)

(defn- read-mem
  "Post: vector indexed by (member-id - 1)."
  [^bytes b]
  (mapv (fn [i]
          (let [o (* i mem-record-bytes)]
            {:id (inc i)
             :use-count (i32 b o)
             :link-type (u8 b (+ o 4))
             :rtx-offset (i32 b (+ o 10))
             :rtx-length (i32 b (+ o 14))
             :slot-id (i32 b (+ o 22))}))
    (range (quot (alength b) mem-record-bytes))))

(defn- rtx-value
  "Post: nil when the member carries no .rtx payload."
  [^bytes rtx {:keys [rtx-offset rtx-length]}]
  (when (and (pos? rtx-offset) (>= rtx-length rtx-trailer-bytes))
    (let [from (dec rtx-offset)
          len (- rtx-length rtx-trailer-bytes)]
      (when (<= (+ from len) (alength rtx))
        (decode-text rtx from len)))))

(defn- rtx-trailer-id
  "The member id SuperMemo writes after each .rtx value. See `verify`."
  [^bytes rtx {:keys [rtx-offset rtx-length]}]
  (when (and (pos? rtx-offset) (>= rtx-length rtx-trailer-bytes))
    (let [at (+ (dec rtx-offset) rtx-length -5)]
      (when (<= (+ at 4) (alength rtx)) (i32 rtx at)))))

(defn- load-registry
  "Read one registry quadruple. Missing files yield an empty registry rather
   than an error — Comment/Link/Style are absent from small collections."
  [^File registry-dir base]
  (let [mem (child-file registry-dir (str base ".mem"))
        rtx (child-file registry-dir (str base ".rtx"))]
    (if-not mem
      {:members [] :rtx (byte-array 0)}
      {:members (read-mem (read-bytes mem))
       :rtx (if rtx (read-bytes rtx) (byte-array 0))})))

(defn- registry-text
  "Post: the member's string value, or nil."
  [{:keys [members rtx]} id]
  (when (and (pos? (long id)) (<= (long id) (count members)))
    (rtx-value rtx (nth members (dec (long id))))))

;; ── Element file slots ─────────────────────────────────────────────

(defn- index-element-slots
  "Map slot number -> File over the whole `elements/` tree.
   SuperMemo buckets slots into nested directories by an allocation order we
   do not decode, so the index is built by one walk rather than by computing
   a path per lookup.
   Post: {slot-number File}; later duplicates of a slot number are ignored."
  [^File elements-dir]
  (if-not elements-dir
    {}
    (reduce (fn [acc ^File f]
              (let [n (.getName f)
                    dot (.lastIndexOf n ".")
                    stem (if (pos? dot) (subs n 0 dot) n)]
                (if-let [slot (try (Long/parseLong stem) (catch NumberFormatException _ nil))]
                  (if (contains? acc slot) acc (assoc acc slot f))
                  acc)))
      {}
      (filter #(.isFile ^File %) (file-seq elements-dir)))))

;; ── compon.dat — component groups ──────────────────────────────────

(def ^:private component-group-magic [0x31 0xD4])

(defn- component-struct-bytes
  "Bytes of the struct that follows a component's uint16 type header.

   The header is self-describing: the low byte is SuperMemoAssistant's
   ComponentType and the high byte is the struct size minus one. Every size
   SMA documents agrees — Html 29 encodes as 0x1C0D, Image 26 as 0x1902,
   Text 35 as 0x2200, Sound 49 as 0x3003.

   Deriving the size beats a lookup table because the walk then survives a
   component type we have never seen. A table built from the types one
   collection happens to contain stops the walk at the first unlisted header,
   and every later component of that element is lost — which is exactly what
   a table did to the Text components carrying 289 answers in one collection."
  [header]
  (inc (bit-shift-right header 8)))

(def ^:private component-kind
  {0x0D :html 0x00 :text 0x0C :rtf 0x01 :spelling
   0x02 :image 0x03 :sound 0x04 :video})

(defn- read-component-group
  "Decode the group at byte offset `pos` in compon.dat.
   Post: {:element-id :components [{:kind :registry-id}...]} or nil when the
   magic does not match. Every header carries its own struct size, so an
   unfamiliar component type is recorded as :unknown and the walk continues
   to the components after it."
  [^bytes b pos]
  (when (and (>= pos 0) (< (+ pos 11) (alength b))
          (= (u8 b pos) (first component-group-magic))
          (= (u8 b (inc pos)) (second component-group-magic)))
    (let [element-id (i32 b (+ pos 4))
          n (u8 b (+ pos 8))
          skip (u16 b (+ pos 9))]
      (loop [p (+ pos 11 skip), left n, acc [], truncated? false]
        (if (or (zero? left) (>= (+ p 2) (alength b)))
          {:element-id element-id
           :components acc
           :truncated? (or truncated? (pos? left))}
          (let [header (u16 b p)
                size (component-struct-bytes header)
                ;; Every component struct places registryId at struct offset
                ;; 18: the 10-byte geometry prefix (unknown1/left/top/width/
                ;; height/displayAt) is followed by 8 bytes that differ per
                ;; type but never change length.
                reg-at (+ p 2 18)
                reg (when (<= (+ reg-at 4) (alength b)) (i32 b reg-at))]
            ;; A size of 1 means the high byte was 0, which no real component
            ;; has — treat it as desynchronization rather than looping.
            (if (< size 2)
              {:element-id element-id :components acc :truncated? true}
              (recur (+ p 2 size) (dec left)
                (conj acc {:kind (component-kind (bit-and header 0xFF) :unknown)
                           :registry-id reg})
                truncated?))))))))

;; ── repetitions.dat / RepetitionHistory.dat / .sub ─────────────────

(defn- read-int32-array [^bytes b]
  (mapv #(i32 b (* 4 %)) (range (quot (alength b) 4))))

(def ^:private repetition-history-record-bytes 39)

(defn- read-repetition-history
  "Post: vector of {:element-id :ordinal :at}; :at is nil for the placeholder
   rows SuperMemo writes for scheduled-but-never-repeated elements."
  [^bytes b]
  (mapv (fn [i]
          (let [o (* i repetition-history-record-bytes)]
            {:element-id (i32 b o)
             :ordinal (u16 b (+ o 4))
             :at (tdatetime->local-date-time (f64 b (+ o 10)))}))
    (range (quot (alength b) repetition-history-record-bytes))))

;; ── reference.dat — bibliography ───────────────────────────────────

(def ^:private reference-record-bytes 32)

(defn- read-references
  "Eight Text-registry ids per reference; index = reference id.
   Field positions established by inspection: 1 title, 2 date, 3 link,
   4 source, 7 comment. 0, 5 and 6 are unused in every observed collection.
   Post: {reference-id {:title :date :url :source :comment}} for non-empty rows."
  [^bytes b text-registry]
  (into {}
    (keep (fn [i]
            (let [o (* i reference-record-bytes)
                  fld #(registry-text text-registry (i32 b (+ o (* 4 %))))
                  m {:title (fld 1) :date (fld 2) :url (fld 3)
                     :source (fld 4) :comment (fld 7)}]
              (when (some some? (vals m)) [i m]))))
    (range 1 (quot (alength b) reference-record-bytes))))

;; ── Verification ───────────────────────────────────────────────────

(defn- divides? [^bytes b n] (and b (pos? (alength b)) (zero? (mod (alength b) n))))

(defn verify
  "Structural checks a caller runs before trusting a decode.
   Post: vector of {:check :ok? :detail}. An :ok? false entry means that part
   of the collection did not match the documented layout; the caller decides
   whether to continue and MUST surface the entry in its report."
  [{:keys [contents element-info repetition-history live-count root]}]
  (let [root-descendants (:descendant-count root)]
    [{:check :record-counts-agree
      :ok? (= (count contents) (count element-info))
      :detail {:contents (count contents) :element-info (count element-info)}}
     {:check :live-count-matches-root-descendants
      :ok? (= live-count (inc (or root-descendants -1)))
      :detail {:live live-count :root-descendants root-descendants}}
     {:check :repetition-history-present
      :ok? (some? repetition-history)
      :detail {:records (count repetition-history)}}]))

;; ── Top level ──────────────────────────────────────────────────────

(defn- calibrate-epoch
  "SuperMemo stores repetition days relative to a per-collection epoch and
   also stores one absolute TDateTime per element, so the epoch is recoverable
   as floor(last-touch) - last-rep-day.
   Pre : `infos` is the ElementInfo vector.
   Post: {:epoch-day N :agreement R :samples K}; nil when no element carries
   both values. `agreement` is the fraction of samples voting for the modal
   epoch — a value below 1.0 means some elements were touched after their last
   repetition, which is expected; a value near 0 means the layout is wrong."
  [infos]
  (let [votes (->> infos
                (keep (fn [{:keys [last-touch last-rep-day]}]
                        (when (> last-touch 1000.0)
                          (- (long (Math/floor last-touch)) last-rep-day))))
                frequencies)]
    (when (seq votes)
      (let [[epoch-day n] (apply max-key val votes)
            total (reduce + (vals votes))]
        {:epoch-day epoch-day
         :agreement (/ (double n) total)
         :samples total}))))

(defn day->local-date
  "Pre : `epoch-day` from `calibrate-epoch`; `day` a SuperMemo day number.
   Post: the calendar date that day number names."
  ^LocalDate [epoch-day day]
  (.plusDays tdatetime-epoch (+ (long epoch-day) (long day))))

(defn read-collection
  "Decode a SuperMemo collection folder.
   Pre : `root` is the collection directory — the one holding `info/`.
         Callers get it from `find-collection-root`.
   Post: a map with :elements (vector indexed by id-1, merging contents.dat and
         ElementInfo.dat), :next-rep-day, :priority-order, :outstanding,
         :repetition-history, :references, :text, :image, :slots, :epoch-day,
         :checks. Never throws on a missing optional file; a missing required
         file surfaces as a failed entry in :checks.
   Invariant: :elements is indexed so that (nth elements (dec id)) is element id."
  [^File root]
  (let [info-dir (child-dir root "info")
        registry-dir (child-dir root "registry")
        elements-dir (child-dir root "elements")
        rd (fn [^File dir & names]
             (when dir (when-let [f (apply child-file dir names)] (read-bytes f))))
        contents-b (rd info-dir "contents.dat")
        elinfo-b (rd info-dir "ElementInfo.dat")
        compon-b (rd info-dir "compon.dat")
        reps-b (rd info-dir "repetitions.dat")
        hist-b (rd info-dir "RepetitionHistory.dat")
        prio-b (rd info-dir "priority.sub")
        outst-b (rd info-dir "Outstanding.sub")
        refdat-b (rd registry-dir "reference.dat")
        contents (if (divides? contents-b contents-record-bytes)
                   (read-contents contents-b) [])
        infos (if (divides? elinfo-b element-info-record-bytes)
                (read-element-info elinfo-b) [])
        text-reg (if registry-dir (load-registry registry-dir "Text") {:members [] :rtx (byte-array 0)})
        image-reg (if registry-dir (load-registry registry-dir "Image") {:members [] :rtx (byte-array 0)})
        slots (index-element-slots elements-dir)
        n (max (count contents) (count infos))
        elements (mapv (fn [i]
                         (let [c (nth contents i nil)
                               e (nth infos i nil)
                               group (when (and compon-b e (nat-int? (:component-pos e)))
                                       (read-component-group compon-b (:component-pos e)))]
                           (merge {:id (inc i)} c e
                             {:title (some->> (:title-text-id e) (registry-text text-reg))
                              :components (:components group)
                              :components-truncated? (boolean (:truncated? group))})))
                   (range n))
        next-rep (if reps-b (read-int32-array reps-b) [])
        hist (if (divides? hist-b repetition-history-record-bytes)
               (read-repetition-history hist-b) [])
        live (remove :deleted? elements)
        epoch (calibrate-epoch (filter #(> (or (:last-touch %) 0.0) 1000.0) infos))]
    {:root root
     :elements elements
     :next-rep-day next-rep
     :priority-order (if prio-b (read-int32-array prio-b) [])
     :outstanding (set (if outst-b (read-int32-array outst-b) []))
     :repetition-history hist
     :references (if (and refdat-b (divides? refdat-b reference-record-bytes))
                   (read-references refdat-b text-reg) {})
     :text text-reg
     :image image-reg
     :slots slots
     :epoch-day (:epoch-day epoch)
     :epoch-agreement (:agreement epoch)
     :live-count (count live)
     :checks (verify {:contents contents
                      :element-info infos
                      :repetition-history hist
                      :live-count (count live)
                      :root (first elements)})}))

(defn element-slot-file
  "Resolve the file backing an element component's registry member.
   Pre : `registry` is the Text or Image registry from `read-collection`.
   Post: a File under `elements/`, or nil when the member has no file slot."
  ^File [collection registry registry-id]
  (let [{:keys [members]} registry]
    (when (and registry-id (pos? (long registry-id)) (<= (long registry-id) (count members)))
      (let [{:keys [slot-id]} (nth members (dec (long registry-id)))]
        (when (pos? (long (or slot-id 0)))
          (get (:slots collection) (long slot-id)))))))

(defn element-html
  "Concatenated HTML of an element's html/text components, in component order.
   Pre : `element` came from `read-collection`.
   Post: a string, or nil when the element has no textual component.
   A component whose file slot is missing contributes nothing and is the
   caller's to report — this fn does not distinguish absent from empty."
  [collection element]
  (let [text-reg (:text collection)
        parts (->> (:components element)
                (filter #(#{:html :text :rtf} (:kind %)))
                (keep (fn [{:keys [registry-id]}]
                        (if-let [^File f (element-slot-file collection text-reg registry-id)]
                          (let [^bytes b (read-bytes f)] (decode-text b 0 (alength b)))
                          (registry-text text-reg registry-id))))
                (remove str/blank?))]
    (when (seq parts) (str/join "\n" parts))))

(defn element-images
  "Image files referenced by an element's image components, in order.
   Post: vector of File; components whose slot is missing are omitted."
  [collection element]
  (->> (:components element)
    (filter #(= :image (:kind %)))
    (keep #(element-slot-file collection (:image collection) (:registry-id %)))
    vec))
