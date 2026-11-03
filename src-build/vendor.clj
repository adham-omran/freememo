(ns vendor
  "Fetch, hash-verify, and audit the vendored third-party client assets.

   The tree under resources/public/freememo/vendor/ is NOT in git — it is built
   here, during `docker build` and once per dev clone, and every byte is checked
   against the committed `vendor-lock.sha256` before it is accepted. That
   lockfile is what keeps a build-time download from reopening the arbitrary-
   code-execution vector that dropping the CDNs closed: a compromised or drifted
   upstream fails the build instead of shipping.

   This namespace is the single source of truth for which versions we ship. The
   HTML files and freememo.vendor-libs reference the paths it creates; nothing
   else names a version.

   Layout: vendor/<lib>@<version>/<upstream-relative-path>. Upstream directory
   structure is preserved, because several stylesheets carry relative url()
   references (katex -> fonts/, pdf.js -> images/).

   Tasks (JDK only — no curl, no python, no node):
     clj -X:build vendor/fetch!       download + verify against the lockfile
     clj -X:build vendor/write-lock!  download + REWRITE the lockfile (version bump)
     clj -X:build vendor/verify!      hash the on-disk tree against the lockfile
     clj -X:build vendor/check-refs!  every path named in HTML/CLJS exists on disk"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URI)
           (java.net.http HttpClient HttpClient$Redirect HttpRequest HttpResponse$BodyHandlers)
           (java.nio.file Files)
           (java.security MessageDigest)
           (java.time Duration)))

;; ---------------------------------------------------------------------------
;; Manifest
;; ---------------------------------------------------------------------------

(def ^:private versions
  {:pdf.js "3.11.174", :highlight.js "11.9.0", :quill "2.0.3",
   :quill-resize-module "2.1.3", :konva "9.3.16", :graphology "0.26.0",
   :sigma "3.0.2", :wavesurfer.js "7.8.6", :lamejs "1.2.1", :katex "0.16.11",
   :highlightjs-zig "1.0.2", :amiri "5.3.0", :noto-naskh-arabic "2.021",
   ;; @fontsource-variable/inter and …/noto-sans-arabic. Fontsource versions
   ;; its packages, not the fonts, so these name the package release.
   :inter "5.3.0", :noto-sans-arabic "5.3.0", :fira-code "5.3.0",
   ;; @floating-ui/core and @floating-ui/dom are released together and share a
   ;; version number, so one string covers both packages below.
   :floating-ui "1.8.0"})

(defn- v [lib] (get versions lib))
(def ^:private cdnjs "https://cdnjs.cloudflare.com/ajax/libs")
(def ^:private jsd "https://cdn.jsdelivr.net/npm")
;; google/fonts publishes no release tags, so a commit pins the font files.
(def ^:private google-fonts
  "https://raw.githubusercontent.com/google/fonts/ad2e6647e3d97ad5a8b6b5f95042f99161af5915")

(def ^:private assets
  "[local-path-under-vendor  url] pairs.

   pdf_viewer.min.css also references images/altText_add.svg and
   images/altText_done.svg. pdf.js 3.11.174 ships neither — absent from cdnjs
   AND from the pdfjs-dist npm package — so those two url()s already 404 in
   production. Nothing to vendor; the behaviour is unchanged."
  (let [pdf (v :pdf.js), hl (v :highlight.js), q (v :quill), rs (v :quill-resize-module)
        kv (v :konva), gr (v :graphology), sg (v :sigma), ws (v :wavesurfer.js)
        lm (v :lamejs), kx (v :katex), hz (v :highlightjs-zig), am (v :amiri)
        fl (v :floating-ui), nn (v :noto-naskh-arabic)]
    [[(str "pdf.js@" pdf "/pdf.min.js")                (str cdnjs "/pdf.js/" pdf "/pdf.min.js")]
     [(str "pdf.js@" pdf "/pdf_viewer.min.js")         (str cdnjs "/pdf.js/" pdf "/pdf_viewer.min.js")]
     [(str "pdf.js@" pdf "/pdf_viewer.min.css")        (str cdnjs "/pdf.js/" pdf "/pdf_viewer.min.css")]
     [(str "pdf.js@" pdf "/pdf.worker.min.js")         (str cdnjs "/pdf.js/" pdf "/pdf.worker.min.js")]
     ;; cdnjs does not host the viewer's images/ dir; pdfjs-dist does.
     [(str "pdf.js@" pdf "/images/loading-icon.gif")   (str jsd "/pdfjs-dist@" pdf "/web/images/loading-icon.gif")]
     [(str "highlight.js@" hl "/highlight.min.js")     (str cdnjs "/highlight.js/" hl "/highlight.min.js")]
     [(str "highlight.js@" hl "/languages/clojure.min.js") (str cdnjs "/highlight.js/" hl "/languages/clojure.min.js")]
     ;; Zig is in NO highlight.js release — the core project lists it as a
     ;; third-party grammar. This package's dist is the only build that
     ;; self-registers on window.hljs the way the clojure pack does; the repo
     ;; highlight.js's SUPPORTED_LANGUAGES.md links to (fwx5618177) ships a
     ;; CommonJS bundle that throws `module is not defined` in a script tag.
     ;; Its own directory, not highlight.js@…/languages/, because it is a
     ;; different package with its own version.
     [(str "highlightjs-zig@" hz "/zig.min.js")        (str jsd "/highlightjs-zig@" hz "/dist/zig.min.js")]
     [(str "quill@" q "/quill.js")                     (str jsd "/quill@" q "/dist/quill.js")]
     [(str "quill@" q "/quill.snow.css")               (str jsd "/quill@" q "/dist/quill.snow.css")]
     [(str "quill@" q "/quill.bubble.css")             (str jsd "/quill@" q "/dist/quill.bubble.css")]
     [(str "quill-resize-module@" rs "/resize.js")     (str jsd "/quill-resize-module@" rs "/dist/resize.js")]
     [(str "konva@" kv "/konva.min.js")                (str jsd "/konva@" kv "/konva.min.js")]
     [(str "graphology@" gr "/graphology.umd.min.js")  (str jsd "/graphology@" gr "/dist/graphology.umd.min.js")]
     [(str "sigma@" sg "/sigma.min.js")                (str jsd "/sigma@" sg "/dist/sigma.min.js")]
     [(str "wavesurfer.js@" ws "/wavesurfer.min.js")   (str jsd "/wavesurfer.js@" ws "/dist/wavesurfer.min.js")]
     [(str "wavesurfer.js@" ws "/plugins/regions.min.js") (str jsd "/wavesurfer.js@" ws "/dist/plugins/regions.min.js")]
     [(str "lamejs@" lm "/lame.min.js")                (str jsd "/lamejs@" lm "/lame.min.js")]
     [(str "katex@" kx "/katex.min.js")                (str jsd "/katex@" kx "/dist/katex.min.js")]
     [(str "katex@" kx "/katex.min.css")               (str jsd "/katex@" kx "/dist/katex.min.css")]
     [(str "katex@" kx "/contrib/auto-render.min.js")  (str jsd "/katex@" kx "/dist/contrib/auto-render.min.js")]
     ;; Amiri — the Naskh face the poem reader sets (spec row 3.4). Two files,
     ;; not twenty-four: only the `arabic` subset at weights 400 and 700 is
     ;; reachable, because `.poem-reader` is the one scope that names the family
     ;; and the text under it is Arabic. A Latin glyph there (a verse number)
     ;; falls through to the next family in the stack, which is what the
     ;; latin/latin-ext subsets would otherwise cost 8 more files to serve.
     ;;
     ;; The version is in the url path, so the stylesheet
     ;; (resources/public/freememo/amiri.css) is committed with local url()s.
     ;; inter.css and noto-sans-arabic.css follow the same shape.
     [(str "amiri@" am "/files/amiri-arabic-400-normal.woff2")
      (str jsd "/@fontsource/amiri@" am "/files/amiri-arabic-400-normal.woff2")]
     [(str "amiri@" am "/files/amiri-arabic-700-normal.woff2")
      (str jsd "/@fontsource/amiri@" am "/files/amiri-arabic-700-normal.woff2")]
     ;; The OFL text, so the vendored tree carries its own licence.
     [(str "amiri@" am "/LICENSE") (str jsd "/@fontsource/amiri@" am "/LICENSE")]
     ;; Noto Naskh Arabic — the Arabic face of the Typst answer sheet
     ;; (freememo.answer-sheet). Typst reads TTF and OTF but not woff2, so
     ;; Amiri above cannot serve it. The browser never loads this file. The
     ;; server copies it out of the jar and passes its directory to Typst.
     ;; freememo.answer-sheet names this path, so a version bump edits it too.
     [(str "noto-naskh-arabic@" nn "/NotoNaskhArabic-wght.ttf")
      (str google-fonts "/ofl/notonaskharabic/NotoNaskhArabic%5Bwght%5D.ttf")]
     [(str "noto-naskh-arabic@" nn "/OFL.txt") (str google-fonts "/ofl/notonaskharabic/OFL.txt")]
     ;; Floating UI — core must execute before dom, which reads window.FloatingUICore
     ;; and defines window.FloatingUIDOM. freememo.spotlight is the consumer.
     [(str "floating-ui@" fl "/floating-ui.core.umd.min.js")
      (str jsd "/@floating-ui/core@" fl "/dist/floating-ui.core.umd.min.js")]
     [(str "floating-ui@" fl "/floating-ui.dom.umd.min.js")
      (str jsd "/@floating-ui/dom@" fl "/dist/floating-ui.dom.umd.min.js")]
     ;; Noto Sans Arabic, the Arabic face of the app UI (resources/public/freememo
     ;; /noto-sans-arabic.css). Only the `arabic` subset: Inter comes first in
     ;; --font-sans and owns every Latin glyph, so this file loads only for a
     ;; page with Arabic text on it.
     [(str "noto-sans-arabic@" (v :noto-sans-arabic) "/files/noto-sans-arabic-arabic-wght-normal.woff2")
      (str jsd "/@fontsource-variable/noto-sans-arabic@" (v :noto-sans-arabic)
        "/files/noto-sans-arabic-arabic-wght-normal.woff2")]
     [(str "noto-sans-arabic@" (v :noto-sans-arabic) "/LICENSE")
      (str jsd "/@fontsource-variable/noto-sans-arabic@" (v :noto-sans-arabic) "/LICENSE")]
     [(str "inter@" (v :inter) "/LICENSE")
      (str jsd "/@fontsource-variable/inter@" (v :inter) "/LICENSE")]]))

(def ^:private inter-assets
  "[local-path-under-vendor  url] pairs for Inter, the app's UI and reading face
   (resources/public/freememo/inter.css).

   The `opsz` build carries the weight AND the optical-size axis, so a heading
   draws tighter than body text with no letter-spacing rule. Every subset at
   both styles: unicode-range means a browser fetches only the subsets a page
   uses, and the italic is what Quill's italic button shows."
  (for [subset ["latin" "latin-ext" "cyrillic" "cyrillic-ext" "greek" "greek-ext" "vietnamese"]
        style ["normal" "italic"]
        :let [file (str "inter-" subset "-opsz-" style ".woff2")]]
    [(str "inter@" (v :inter) "/files/" file)
     (str jsd "/@fontsource-variable/inter@" (v :inter) "/files/" file)]))

(def ^:private fira-code-assets
  "[local-path-under-vendor  url] pairs for Fira Code, the app's monospace face
   (resources/public/freememo/fira-code.css). One style, because Fira Code has
   no italic. Every subset, for the same unicode-range reason as Inter."
  (let [fc (v :fira-code)]
    (concat
      (for [subset ["latin" "latin-ext" "cyrillic" "cyrillic-ext" "greek" "greek-ext" "symbols2"]
            :let [file (str "fira-code-" subset "-wght-normal.woff2")]]
        [(str "fira-code@" fc "/files/" file)
         (str jsd "/@fontsource-variable/fira-code@" fc "/files/" file)])
      [[(str "fira-code@" fc "/LICENSE") (str jsd "/@fontsource-variable/fira-code@" fc "/LICENSE")]])))

(def ^:private vendor-root "resources/public/freememo/vendor")
(def ^:private lock-file "vendor-lock.sha256")

;; ---------------------------------------------------------------------------
;; Primitives
;; ---------------------------------------------------------------------------

(def ^:private http
  (delay (-> (HttpClient/newBuilder)
           (.connectTimeout (Duration/ofSeconds 20))
           (.followRedirects HttpClient$Redirect/NORMAL)
           (.build))))

(defn- GET
  "Fetch `url` as bytes.
   Post: returns a byte-array on HTTP 200; throws ex-info {:type ::network} on
         any other status or on an IO failure — the caller distinguishes this
         from a hash mismatch."
  ^bytes [url]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
              (.timeout (Duration/ofSeconds 120))
              ;; jsDelivr and cdnjs both 403 an empty user-agent from some hosts.
              (.header "User-Agent" "freememo-vendor-fetch")
              (.GET) (.build))
        resp (try (.send @http req (HttpResponse$BodyHandlers/ofByteArray))
                  (catch Exception e
                    (throw (ex-info (str "network failure fetching " url)
                             {:type ::network :url url} e))))]
    (when-not (= 200 (.statusCode resp))
      (throw (ex-info (str "HTTP " (.statusCode resp) " fetching " url)
               {:type ::network :url url :status (.statusCode resp)})))
    (.body resp)))

(defn- sha256 ^String [^bytes bs]
  (let [d (.digest (MessageDigest/getInstance "SHA-256") bs)]
    (apply str (map #(format "%02x" %) d))))

(defn- write-bytes! [rel ^bytes bs]
  (let [f (io/file vendor-root rel)]
    (io/make-parents f)
    (with-open [o (io/output-stream f)] (.write o bs))
    f))

(defn- read-lock
  "Post: {rel-path sha} from the committed lockfile, or nil when it is absent."
  []
  (let [f (io/file lock-file)]
    (when (.exists f)
      (into {} (for [line (str/split-lines (slurp f))
                     :let [line (str/trim line)]
                     :when (and (not (str/blank? line))
                             (not (str/starts-with? line "#")))
                     :let [[sha rel] (str/split line #"\s+" 2)]]
                 [rel sha])))))

(defn- write-lock-file! [sha-by-rel]
  (spit lock-file
    (str "# SHA256 of every file `clj -X:build vendor/fetch!` downloads into\n"
      "# resources/public/freememo/vendor/ (which is gitignored).\n"
      "# Regenerate with: clj -X:build vendor/write-lock!\n"
      "# Verify with:     clj -X:build vendor/verify!   (or sha256sum -c from the vendor root)\n"
      (str/join "\n" (for [[rel sha] (sort-by key sha-by-rel)] (str sha "  " rel)))
      "\n")))

;; ---------------------------------------------------------------------------
;; Tasks
;; ---------------------------------------------------------------------------

(defn- katex-font-assets
  "Post: [local-path url] for every woff2 katex.min.css names relatively.
   Pre:  katex.min.css is already on disk (phase 1 fetched it).

   Only woff2 is vendored: the woff/ttf entries in each @font-face src are never
   requested once woff2 resolves, and this app already requires a browser with
   CSS light-dark() and WebGL.

   The css is parsed before it has been hash-verified, so in principle it could
   name unexpected paths — but any file that lands here without a lockfile entry
   is reported as UNLOCKED and fails the build, and the url is always built from
   jsDelivr's katex package root."
  []
  (let [kx (v :katex)
        css (slurp (io/file vendor-root (str "katex@" kx "/katex.min.css")))
        rels (distinct (map second (re-seq #"url\((fonts/[A-Za-z0-9_-]+\.woff2)\)" css)))]
    (for [rel rels]
      [(str "katex@" kx "/" rel) (str jsd "/katex@" kx "/dist/" rel)])))

(defn- fetch-pairs!
  "Download each [rel url], hashing as we go.
   Post: {rel sha} for every file written.
   Inv:  a file is written only after its bytes are in hand, so a failed run
         never leaves a truncated file."
  [pairs]
  (reduce (fn [acc [rel url]]
            (let [bs (GET url)
                  sha (sha256 bs)]
              (write-bytes! rel bs)
              (printf "%9d  %s\n" (alength bs) rel)
              (flush)
              (assoc acc rel sha)))
    {} pairs))

(defn- fetch-all!
  "Download every asset in two phases.
   Pre:  network reachable.
   Post: the vendor tree is complete and {rel sha} covers every file written.

   Phase 2 exists because katex's font list lives inside a stylesheet phase 1
   fetches — there is no manifest of it upstream."
  []
  (let [phase1 (fetch-pairs! (concat assets inter-assets fira-code-assets))
        phase2 (fetch-pairs! (katex-font-assets))]
    (merge phase1 phase2)))

(defn- report-mismatches!
  "Pre:  `actual` and `expected` are {rel sha}.
   Post: returns nil when they agree; throws ex-info {:type ::integrity} naming
         every offending path, with both hashes, otherwise. A mismatch means
         either an upstream re-publish or a compromise — the operator must be
         able to tell them apart, so both hashes are printed."
  [actual expected urls-by-rel]
  (let [missing (sort (remove (set (keys actual)) (keys expected)))
        extra (sort (remove (set (keys expected)) (keys actual)))
        changed (sort (for [[rel sha] actual
                            :let [want (get expected rel)]
                            :when (and want (not= want sha))]
                        rel))]
    (when (or (seq missing) (seq extra) (seq changed))
      (doseq [rel changed]
        (printf "MISMATCH %s\n  expected %s\n  actual   %s\n  url      %s\n"
          rel (get expected rel) (get actual rel) (get urls-by-rel rel "?")))
      (doseq [rel missing] (printf "MISSING  %s (in lockfile, not fetched)\n" rel))
      (doseq [rel extra] (printf "UNLOCKED %s (fetched, not in lockfile)\n" rel))
      (flush)
      (throw (ex-info (str "vendor integrity check failed: "
                        (count changed) " changed, " (count missing) " missing, "
                        (count extra) " unlocked")
               {:type ::integrity :changed changed :missing missing :extra extra})))))

(defn fetch!
  "Download every vendored asset and verify it against the committed lockfile.
   Pre:  vendor-lock.sha256 exists (run write-lock! to create it).
   Post: the vendor tree is complete and every byte matches the lockfile, or the
         process exits non-zero having named the offending paths."
  [_]
  (let [expected (or (read-lock)
                   (throw (ex-info (str lock-file " not found — run: clj -X:build vendor/write-lock!")
                            {:type ::manifest})))
        actual (fetch-all!)]
    (report-mismatches! actual expected
      (into {} (concat assets inter-assets fira-code-assets (katex-font-assets))))
    (println (format "vendor: %d files verified against %s" (count actual) lock-file))))

(defn write-lock!
  "Download every vendored asset and (re)write the lockfile from what arrived.
   Use ONLY when intentionally bumping a version — it accepts whatever upstream
   serves, which is exactly what fetch! exists to prevent."
  [_]
  (let [actual (fetch-all!)]
    (write-lock-file! actual)
    (println (format "vendor: wrote %s with %d entries" lock-file (count actual)))))

(defn verify!
  "Hash the on-disk tree against the lockfile without downloading anything.
   Post: silent success, or ex-info {:type ::integrity} naming what differs."
  [_]
  (let [expected (or (read-lock) (throw (ex-info (str lock-file " not found") {:type ::manifest})))
        actual (into {} (for [rel (keys expected)
                              :let [f (io/file vendor-root rel)]
                              :when (.exists f)]
                          [rel (sha256 (Files/readAllBytes (.toPath f)))]))]
    (report-mismatches! actual expected {})
    (println (format "vendor: %d on-disk files match %s" (count actual) lock-file))))

(def ^:private font-stylesheets
  "The committed @font-face files. Their url()s are relative to the file, so
   `vendor/…` names a path under vendor-root."
  ["resources/public/freememo/amiri.css"
   "resources/public/freememo/inter.css"
   "resources/public/freememo/noto-sans-arabic.css"
   "resources/public/freememo/fira-code.css"])

(defn check-refs!
  "Every /freememo/vendor/… path named by the HTML, by freememo.vendor-libs or
   by a committed font stylesheet must exist on disk.
   Post: silent success, or ex-info {:type ::dangling-ref} naming each missing
         path. Guards the hand-written paths in those files."
  [_]
  (let [html (mapcat #(re-seq #"/freememo/vendor/[A-Za-z0-9@._/-]+" (slurp %))
               ["resources/public/freememo/index.prod.html"
                "resources/public/freememo/index.dev.html"])
        fonts (mapcat #(map second (re-seq #"url\(vendor/([A-Za-z0-9@._/-]+)\)" (slurp %)))
                font-stylesheets)
        cljs (map #(str/replace % "\"" "")
               (re-seq #"\"[A-Za-z0-9._-]+@[0-9][A-Za-z0-9._/-]+\""
                 (slurp "src/freememo/vendor_libs.cljs")))
        refs (->> (concat (map #(str/replace % "/freememo/vendor/" "") html) cljs fonts)
               (filter #(re-find #"\.(js|css|svg|gif|woff2)$" %))
               distinct sort)
        missing (remove #(.exists (io/file vendor-root %)) refs)]
    (when (seq missing)
      (doseq [m missing] (printf "DANGLING %s\n" m))
      (flush)
      (throw (ex-info (str (count missing) " referenced vendor path(s) missing")
               {:type ::dangling-ref :missing missing})))
    (println (format "vendor: all %d referenced paths present" (count refs)))))
