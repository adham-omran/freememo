(ns freememo.text
  "Plain-text extraction for indexing (search) and analysis."
  (:require [clojure.string :as str]
            [taoensso.telemere :as tel])
  (:import [org.jsoup Jsoup]))

;; PUA codepoints used by Linux Libertine / Linux Biolinum to encode
;; ligatures, small caps, and stylistic alternates that have no real
;; Unicode codepoint. Derived from the font's source SFD glyph table
;; (linuxlibertine.sf.net 5.3.0).
(def ^:private linux-libertine-pua
  {;; Discretionary ligatures
   0xE030 "fb"     ;; f_b
   0xE032 "ffh"    ;; f_f_h
   0xE033 "ffj"    ;; f_f_j
   0xE034 "ffk"    ;; f_f_k
   0xE035 "fft"    ;; f_f_t
   0xE036 "fh"     ;; f_h
   0xE037 "fj"     ;; f_j
   0xE038 "fk"     ;; f_k
   0xE039 "ft"     ;; f_t
   0xE03A "ck"     ;; c_k
   0xE03B "ch"     ;; c_h
   0xE03C "tt"     ;; t_t
   0xE03D "ct"     ;; c_t
   0xE048 "Qu"     ;; Q_u
   0xE049 "Th"     ;; T_h
   0xE04A "tz"     ;; t_z
   ;; Long-s ligatures (archaic) — fold to plain s
   0xE03E "si"     ;; longs_i
   0xE03F "ss"     ;; longs_longs
   0xE043 "sl"     ;; longs_l
   0xE044 "ssi"    ;; longs_longs_i
   0xE045 "ss"     ;; longs_s
   0xE047 "sh"     ;; longs_h
   ;; Stylistic alternates
   0xE050 "&"      ;; ampersand.alt
   0xE0E0 "f"      ;; f.short
   ;; Small caps a–z (Linux Libertine renders as small uppercase; semantic intent is uppercase)
   0xE051 "A" 0xE052 "B" 0xE053 "C" 0xE054 "D" 0xE055 "E"
   0xE056 "F" 0xE057 "G" 0xE058 "H" 0xE059 "I" 0xE05A "J"
   0xE05B "K" 0xE05C "L" 0xE05D "M" 0xE05E "N" 0xE05F "O"
   0xE060 "P" 0xE061 "Q" 0xE062 "R" 0xE063 "S" 0xE064 "T"
   0xE065 "U" 0xE066 "V" 0xE067 "W" 0xE068 "X" 0xE069 "Y"
   0xE06A "Z"
   0xE06D "-"      ;; hyphen.sc
   ;; Oldstyle / lining figures (proportional)
   0xE020 "0" 0xE021 "1" 0xE022 "2" 0xE023 "3" 0xE024 "4"
   0xE025 "5" 0xE026 "6" 0xE027 "7" 0xE028 "8" 0xE029 "9"
   ;; Tabular oldstyle figures
   0xE118 "0" 0xE119 "1" 0xE11A "2" 0xE11B "3" 0xE11C "4"
   0xE11D "5" 0xE11E "6" 0xE11F "7" 0xE120 "8" 0xE121 "9"})

;; Standard Unicode ligatures decomposed to ASCII so search / Anki sync /
;; grep-style consumers see plain letters.
(def ^:private unicode-ligatures
  {0xFB00 "ff"
   0xFB01 "fi"
   0xFB02 "fl"
   0xFB03 "ffi"
   0xFB04 "ffl"})

(def ^:private translation-table
  (merge linux-libertine-pua unicode-ligatures))

(defn- pua? [^long cp]
  (or (and (>= cp 0xE000) (<= cp 0xF8FF))
    (and (>= cp 0xF0000) (<= cp 0xFFFFD))
    (and (>= cp 0x100000) (<= cp 0x10FFFD))))

(defn normalize-extracted-text
  "Translate Linux Libertine PUA ligature/small-cap codepoints back to
   readable text and decompose standard Unicode ligatures (U+FB00–FB04)
   to ASCII pairs. Logs at :debug if any unmapped PUA codepoints remain
   after translation. Returns the input unchanged for nil/blank text."
  [text]
  (if (or (nil? text) (str/blank? text))
    text
    (let [sb (StringBuilder. (count text))
          n (count text)]
      (loop [i 0]
        (when (< i n)
          (let [cp (int (.codePointAt ^String text i))
                advance (Character/charCount cp)]
            (if-let [replacement (translation-table cp)]
              (.append sb ^String replacement)
              (.appendCodePoint sb cp))
            (recur (+ i advance)))))
      (let [out (.toString sb)
            out-len (count out)
            remaining (loop [i 0 c 0]
                        (if (>= i out-len)
                          c
                          (let [cp (int (.codePointAt ^String out i))]
                            (recur (+ i (Character/charCount cp))
                              (if (pua? cp) (inc c) c)))))]
        (when (pos? remaining)
          (tel/log! {:level :debug :id ::unmapped-pua
                     :data {:count remaining
                            :sample (subs out 0 (min 80 out-len))}}
            "Unmapped PUA codepoints in extracted text"))
        out))))

(defn strip-html
  "Convert HTML to plain text via Jsoup. Returns empty string for nil/blank input.
   Decodes entities and collapses whitespace."
  [html]
  (if (or (nil? html) (and (string? html) (str/blank? html)))
    ""
    (-> (Jsoup/parseBodyFragment html) .body .text)))

(defn escape-html
  "Escape the four HTML-significant characters."
  [s]
  (-> s
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")))

(defn strip-control-chars
  "Remove C0 control characters that are invalid in HTML/XML text — including
   NUL (0x00), which Postgres rejects in a UTF8 column. Preserves tab (0x09),
   newline (0x0A), and carriage return (0x0D)."
  [s]
  (str/replace s #"[\x00-\x08\x0B\x0C\x0E-\x1F]" ""))

(defn text->paragraph-html
  "Convert plain text (e.g., from PDFBox or PDF.js) into paragraph HTML.
   Strips HTML-invalid control chars (incl. NUL), splits on blank lines,
   escapes HTML, wraps each non-empty block in <p>. Returns an empty string
   when no non-blank blocks remain."
  [text]
  (if (or (nil? text) (str/blank? text))
    ""
    (let [normalized (-> text
                       strip-control-chars
                       (str/replace "\r\n" "\n")
                       (str/replace "\r" "\n"))
          blocks (->> (str/split normalized #"\n[ \t]*\n+")
                   (map str/trim)
                   (remove str/blank?))]
      (->> blocks
        (map (fn [block]
               (str "<p>" (escape-html block) "</p>")))
        (str/join "\n")))))
