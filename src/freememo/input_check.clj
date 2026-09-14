(ns freememo.input-check
  "Guards on user-supplied strings at the boundaries that cannot defend
   themselves: length caps on what reaches a column, control-character
   stripping on what reaches a title, and quoting on what reaches a
   Content-Disposition header.

   Requires nothing that reaches `freememo.db`, so `input_check_test` runs
   with PostgreSQL down."
  (:require [clojure.string :as str])
  (:import (java.text Normalizer Normalizer$Form)))

(def title-max 500)
(def card-max 10000)
(def prompt-max 5000)
(def label-max 500)   ; one occlusion label's text, newlines included

(defn check-length!
  "Throw ex-info ::length-exceeded when s exceeds max-chars.
   nil and blank pass. field is a keyword used in the error message."
  [field s max-chars]
  (when (and s (> (count s) max-chars))
    (throw (ex-info (str (name field) " exceeds " max-chars " character limit")
             {:type ::length-exceeded
              :field field
              :max max-chars
              :actual (count s)}))))

(defn length-error?
  "Predicate for catching :type ::length-exceeded ex-data."
  [data]
  (= ::length-exceeded (:type data)))

;; ── Titles ─────────────────────────────────────────────────────────

(def ^:private control-chars
  "C0 controls and DEL. A title is one line of display text: a NUL truncates it
   in every C-backed layer below us, and a CR or LF forges a header line
   wherever a title reaches one. No other character in this class has a
   legitimate reading in a title."
  #"[\x00-\x1F\x7F]")

(defn sanitize-title
  "Strip control characters from a user-supplied title and trim it.

   Pre : `s` is a string or nil.
   Post: a non-blank string holding no character in `control-chars`, with no
         leading or trailing whitespace. Inner whitespace is preserved. nil,
         blank, or fully-stripped input yields \"Untitled\".
   Inv : path separators, quotation marks and backslashes SURVIVE. A title is
         display text and nothing else reads it as a path — `content-disposition`
         owns the filename a topic downloads under, and `clean-html` owns the
         markup. Stripping `/` and `\\` here cost `\\(TeX\\)` its delimiters and
         turned \"Foo / Bar\" into \"Foo Bar\", for no reader's benefit: no route,
         no IRI, no archive path and no Anki field is built from a title, and
         four title columns (EPUB chapters, bulk import, `sources.title`,
         `curricula.title`) never passed through here at all."
  [s]
  (if (nil? s)
    "Untitled"
    (let [stripped (-> s (str/replace control-chars "") str/trim)]
      (if (str/blank? stripped) "Untitled" stripped))))

(defn prettify-title
  "Canonicalize an import-time title for display:
   - Replace underscores with spaces.
   - Strip trailing .pdf (case-insensitive).
   - Collapse whitespace runs and trim.
   Idempotent. nil/blank returns the input unchanged so callers can guard."
  [s]
  (if (or (nil? s) (str/blank? s))
    s
    (-> s
      (str/replace #"_" " ")
      (str/replace #"(?i)\.pdf$" "")
      (str/replace #"\s+" " ")
      str/trim)))

;; ── Content-Disposition ────────────────────────────────────────────

(def ^:private filename-unsafe
  "What no filesystem and no Content-Disposition parser takes verbatim: the
   Windows reserved set plus control characters. Deliberately the same class
   `extensions/chrome/src/background.js` strips client-side, so a captured page
   and a renamed topic download under one rule."
  #"[\x00-\x1F\x7F\\/:*?\"<>|]+")

(def ^:private attr-char
  "RFC 8187 §3.2.1 attr-char — what may stand unencoded in a `filename*`."
  (set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$&+-.^_`|~"))

(defn- ascii-fold
  "Pre : `s` is a string.
   Post: `s` with combining marks removed and every remaining character outside
         printable ASCII dropped. \"episódio\" yields \"episodio\" rather than
         \"episdio\", which is the whole reason to decompose first."
  [s]
  (-> (Normalizer/normalize ^String s Normalizer$Form/NFD)
    (str/replace #"\p{M}+" "")
    (str/replace #"[^\x20-\x7E]+" "")))

(defn- pct-encode
  "Pre : `s` is a string.
   Post: `s` as UTF-8 bytes, percent-escaped to an RFC 8187 ext-value. Holds no
         character outside `attr-char` and `%`, so it cannot end a header line."
  [s]
  (let [sb (StringBuilder.)]
    (doseq [b (.getBytes ^String s "UTF-8")]
      (let [n (bit-and (long b) 0xFF)
            c (char n)]
        (if (attr-char c)
          (.append sb c)
          (.append sb (format "%%%02X" n)))))
    (.toString sb)))

(defn- ensure-ext
  "Pre : `s` is a non-blank string. `ext` is a string, \"\" for none.
   Post: `s`, with `ext` appended unless `s` already ends with it, compared
         case-insensitively. A \"\" ext appends nothing."
  [s ext]
  (if (str/ends-with? (str/lower-case s) (str/lower-case ext))
    s
    (str s ext)))

(defn content-disposition
  "A complete Content-Disposition header value naming `title` as the file.

   Pre : `disposition` is \"inline\" or \"attachment\". `title` is a string or
         nil. `extension` is a string starting with \".\", or nil when the
         caller knows no extension.
   Post: a value carrying BOTH parameters RFC 6266 §4.3 asks for — a quoted
         ASCII `filename` for parsers that read only that, and an RFC 8187
         `filename*` carrying the title's own characters. Neither parameter
         holds a quotation mark, a path separator, or a control character, so
         the result is one well-formed header line for ANY title.
   Post: `extension` is appended unless `title` already ends with it, compared
         case-insensitively. A nil extension appends nothing.
   Inv : every current browser prefers `filename*`, so the ASCII parameter is a
         lossy fallback — but never a BARE EXTENSION, which reads as a hidden
         dotfile. A title whose stem folds to nothing falls back to
         \"download\", so `.pdf` is impossible and `download.pdf` is what a
         fully non-Latin title yields there.
   Blame: interpolating a title straight into a header is the caller bug this
         removes. Production holds titles bearing `\"`, and `sanitize-title`
         deliberately keeps them."
  [disposition title extension]
  (let [ext  (or extension "")
        base (-> (or title "")
               (str/replace filename-unsafe " ")
               (str/replace #"\s+" " ")
               str/trim)
        base (if (str/blank? base) "download" base)
        ;; The fallback is chosen from the STEM, before the extension is
        ;; appended: folding "مرحبا.pdf" and testing the result yields ".pdf",
        ;; which is not blank and would ship as a hidden dotfile.
        stem (let [a (str/trim (ascii-fold base))]
               (if (str/blank? a) "download" a))]
    (str disposition
      "; filename=\"" (ensure-ext stem ext) "\""
      "; filename*=UTF-8''" (pct-encode (ensure-ext base ext)))))
