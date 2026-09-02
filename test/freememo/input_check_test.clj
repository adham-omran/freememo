(ns freememo.input-check-test
  "The title/filename split: what `sanitize-title` keeps, and what
   `content-disposition` is therefore obliged to quote.

   The two contracts are complementary and must be read together. A title is
   display text and keeps `/`, `\\` and `\"`; the header builder is the only
   thing that has to make those safe. Testing either alone would let the pair
   drift into a title nobody can download.

   Needs no database: freememo.input-check requires only clojure.string."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [freememo.input-check :as input]))

;; ── sanitize-title ─────────────────────────────────────────────────

(deftest keeps-every-character-a-title-may-hold
  (testing "path separators survive — no route, IRI or archive path reads a title"
    (is (= "Foo / Bar" (input/sanitize-title "Foo / Bar")))
    (is (= "src/freememo/db.clj" (input/sanitize-title "src/freememo/db.clj")))
    (is (= "C:\\Users\\x" (input/sanitize-title "C:\\Users\\x"))))
  (testing "quotation marks survive — production holds three such titles"
    (is (= "The \"Best of Both Worlds\""
          (input/sanitize-title "The \"Best of Both Worlds\""))))
  (testing "TeX delimiters survive — the backslash strip used to destroy them"
    (is (= "Bounds on \\(O(n \\log n)\\)"
          (input/sanitize-title "Bounds on \\(O(n \\log n)\\)")))))

(deftest strips-control-characters
  (is (= "ab" (input/sanitize-title "a\u0000b")))
  (is (= "a b" (input/sanitize-title "a\r\n b")) "CR and LF go; the space stays")
  (is (= "ab" (input/sanitize-title "a\u007Fb")) "DEL")
  (is (not-any? #(< (int %) 0x20)
        (input/sanitize-title (apply str (map char (range 0 0x80)))))))

(deftest trims-outside-and-preserves-inside
  (is (= "a  b" (input/sanitize-title "   a  b   "))))

(deftest untitled-is-the-only-blank-result
  (is (= "Untitled" (input/sanitize-title nil)))
  (is (= "Untitled" (input/sanitize-title "")))
  (is (= "Untitled" (input/sanitize-title "   ")))
  (is (= "Untitled" (input/sanitize-title "\u0000\r\n")))
  (testing "a title of separators is now itself, not Untitled — they are content"
    (is (= "///" (input/sanitize-title "///")))))

;; ── content-disposition ────────────────────────────────────────────

(def ^:private hostile
  "Every title shape that has broken, or could break, a header line."
  ["The \"Best of Both Worlds\""
   "src/freememo/db.clj"
   "C:\\Users\\x"
   "a\r\nX-Injected: yes"
   "episódio 3/4"
   "مرحبا"
   "日本語のタイトル"
   "///"
   "   "
   ""
   nil
   "Report.PDF"
   "tune.mp3"
   "a*b?c<d>e|f:g"])

(defn- params
  "The header value split into its `filename` and `filename*` parameters."
  [v]
  {:ascii (second (re-find #"filename=\"([^\"]*)\"" v))
   :ext   (second (re-find #"filename\*=UTF-8''(\S*)$" v))})

(deftest every-title-yields-one-well-formed-header-line
  (doseq [t hostile]
    (let [v (input/content-disposition "inline" (when t (input/sanitize-title t)) ".pdf")
          {:keys [ascii ext]} (params v)]
      (testing (pr-str t)
        (is (not (re-find #"[\r\n]" v)) "a header value is one line")
        (is (some? ascii) "the quoted filename parameter parses")
        (is (some? ext) "the extended filename parameter parses")
        (is (not (str/includes? ascii "\"")) "no quote can close the token early")
        (is (not (re-find #"[\\/]" ascii)) "no path separator reaches a filesystem")
        (is (re-matches #"[A-Za-z0-9!#$&+\-.^_`|~%]*" ext)
          "filename* holds only attr-char and %")))))

(deftest the-ascii-fallback-is-never-a-bare-extension
  (testing "a stem with no ASCII left falls back rather than shipping .pdf"
    (is (= "download.pdf" (:ascii (params (input/content-disposition "inline" "مرحبا" ".pdf")))))
    (is (= "download.pdf" (:ascii (params (input/content-disposition "inline" nil ".pdf")))))
    (is (= "download.pdf" (:ascii (params (input/content-disposition "inline" "///" ".pdf")))))))

(deftest diacritics-fold-rather-than-vanish
  (is (= "episodio 3 4.pdf"
        (:ascii (params (input/content-disposition "inline" "episódio 3/4" ".pdf")))))
  (testing "the extended parameter carries the real characters"
    (is (str/includes?
          (:ext (params (input/content-disposition "inline" "episódio" ".pdf")))
          "%C3%B3"))))

(deftest extension-is-appended-once-and-case-insensitively
  (is (= "Report.PDF" (:ascii (params (input/content-disposition "inline" "Report.PDF" ".pdf")))))
  (is (= "Report.pdf" (:ascii (params (input/content-disposition "inline" "Report" ".pdf")))))
  (testing "a nil extension appends nothing — the audio route knows no suffix"
    (is (= "tune.mp3" (:ascii (params (input/content-disposition "inline" "tune.mp3" nil)))))))

(deftest disposition-type-is-the-caller-s
  (is (str/starts-with? (input/content-disposition "attachment" "x" ".zip") "attachment; "))
  (is (str/starts-with? (input/content-disposition "inline" "x" ".pdf") "inline; ")))

;; ── prettify-title, unchanged by the split ─────────────────────────

(deftest prettify-title-is-idempotent-and-leaves-separators-alone
  (is (= "My Paper" (input/prettify-title "My_Paper.pdf")))
  (is (= "My Paper" (input/prettify-title (input/prettify-title "My_Paper.pdf"))))
  (testing "the double space the old strip left behind no longer arises"
    (is (= "Foo / Bar" (input/prettify-title (input/sanitize-title "Foo / Bar"))))))

;; ── check-length!, unchanged ───────────────────────────────────────

(deftest check-length-rejects-rather-than-truncates
  (is (nil? (input/check-length! :title (apply str (repeat input/title-max "a")) input/title-max)))
  (is (thrown? clojure.lang.ExceptionInfo
        (input/check-length! :title (apply str (repeat (inc input/title-max) "a")) input/title-max)))
  (is (input/length-error?
        (try (input/check-length! :title (apply str (repeat 501 "a")) 500)
          (catch clojure.lang.ExceptionInfo e (ex-data e))))))
