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
   :sigma "3.0.2", :wavesurfer.js "7.8.6", :lamejs "1.2.1", :katex "0.16.11"})

(defn- v [lib] (get versions lib))
(def ^:private cdnjs "https://cdnjs.cloudflare.com/ajax/libs")
(def ^:private jsd "https://cdn.jsdelivr.net/npm")

(def ^:private assets
  "[local-path-under-vendor  url] pairs.

   pdf_viewer.min.css also references images/altText_add.svg and
   images/altText_done.svg. pdf.js 3.11.174 ships neither — absent from cdnjs
   AND from the pdfjs-dist npm package — so those two url()s already 404 in
   production. Nothing to vendor; the behaviour is unchanged."
  (let [pdf (v :pdf.js), hl (v :highlight.js), q (v :quill), rs (v :quill-resize-module)
        kv (v :konva), gr (v :graphology), sg (v :sigma), ws (v :wavesurfer.js)
        lm (v :lamejs), kx (v :katex)]
    [[(str "pdf.js@" pdf "/pdf.min.js")                (str cdnjs "/pdf.js/" pdf "/pdf.min.js")]
     [(str "pdf.js@" pdf "/pdf_viewer.min.js")         (str cdnjs "/pdf.js/" pdf "/pdf_viewer.min.js")]
     [(str "pdf.js@" pdf "/pdf_viewer.min.css")        (str cdnjs "/pdf.js/" pdf "/pdf_viewer.min.css")]
     [(str "pdf.js@" pdf "/pdf.worker.min.js")         (str cdnjs "/pdf.js/" pdf "/pdf.worker.min.js")]
     ;; cdnjs does not host the viewer's images/ dir; pdfjs-dist does.
     [(str "pdf.js@" pdf "/images/loading-icon.gif")   (str jsd "/pdfjs-dist@" pdf "/web/images/loading-icon.gif")]
     [(str "highlight.js@" hl "/highlight.min.js")     (str cdnjs "/highlight.js/" hl "/highlight.min.js")]
     [(str "highlight.js@" hl "/languages/clojure.min.js") (str cdnjs "/highlight.js/" hl "/languages/clojure.min.js")]
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
     [(str "katex@" kx "/contrib/auto-render.min.js")  (str jsd "/katex@" kx "/dist/contrib/auto-render.min.js")]]))

(def ^:private vendor-root "resources/public/freememo/vendor")
(def ^:private lock-file "vendor-lock.sha256")

;; Open Sans is the one asset whose upstream urls are release-hashed, so its
;; stylesheet is COMMITTED as text at resources/public/freememo/open-sans.css —
;; outside the gitignored vendor tree, because git cannot un-ignore a file inside
;; an ignored directory.
;;
;; That file carries BOTH halves of the identity: its url()s are local
;; (`vendor/open-sans/files/<hash>.woff2`, what the browser fetches) and its
;; `@upstream-prefix` header records the fonts.gstatic.com directory those
;; basenames came from (what fetch! downloads). A Google font release therefore
;; cannot drift our urls — it would need a new prefix, which is a visible commit —
;; and a byte change at a frozen url still fails the hash check.
(def ^:private open-sans-css "resources/public/freememo/open-sans.css")
(def ^:private open-sans-dir "open-sans/files/")
(def ^:private google-css2-url
  "https://fonts.googleapis.com/css2?family=Open+Sans:wght@400;500;600;700;800&display=swap")
;; css2 serves woff2 urls only to a modern UA; with a bare agent it returns ttf.
(def ^:private modern-ua
  (str "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36"))

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
  (^bytes [url] (GET url "freememo-vendor-fetch"))
  (^bytes [url user-agent]
  (let [req (-> (HttpRequest/newBuilder (URI/create url))
              (.timeout (Duration/ofSeconds 120))
              ;; jsDelivr and cdnjs both 403 an empty user-agent from some hosts.
              (.header "User-Agent" user-agent)
              (.GET) (.build))
        resp (try (.send @http req (HttpResponse$BodyHandlers/ofByteArray))
                  (catch Exception e
                    (throw (ex-info (str "network failure fetching " url)
                             {:type ::network :url url} e))))]
    (when-not (= 200 (.statusCode resp))
      (throw (ex-info (str "HTTP " (.statusCode resp) " fetching " url)
               {:type ::network :url url :status (.statusCode resp)})))
    (.body resp))))

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

(defn- font-assets
  "Post: [local-path url] for every woff2 the committed open-sans.css names.
   Pre:  open-sans.css exists, carries an `@upstream-prefix <url>` header, and
         its url()s are local `vendor/open-sans/files/<basename>` paths.
         A violation is a bad commit of that file, not a fetch failure."
  []
  (let [css (slurp open-sans-css)
        prefix (second (re-find #"@upstream-prefix\s+(\S+)" css))
        bases (map second (re-seq (re-pattern (str "url\\(vendor/" open-sans-dir "([^)]*\\.woff2)\\)")) css))]
    (when-not prefix
      (throw (ex-info (str open-sans-css " has no @upstream-prefix header — regenerate with"
                        " clj -X:build vendor/write-open-sans-css!")
               {:type ::manifest})))
    (when (empty? bases)
      (throw (ex-info (str open-sans-css " names no local woff2 urls") {:type ::manifest})))
    ;; Open Sans is a variable font: one file per subset, referenced once per
    ;; weight. 50 url()s, 10 distinct files — dedupe or fetch each five times.
    (distinct (for [b bases] [(str open-sans-dir b) (str prefix b)]))))

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
        rels (distinct (map second (re-seq #"url\((fonts/[A-Za-z_-]+\.woff2)\)" css)))]
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
   Pre:  network reachable; open-sans.css committed with its @upstream-prefix.
   Post: the vendor tree is complete and {rel sha} covers every file written.

   Phase 2 exists because katex's font list lives inside a stylesheet phase 1
   fetches — there is no manifest of it upstream."
  []
  (let [phase1 (fetch-pairs! (concat assets (font-assets)))
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
      (into {} (concat assets (font-assets) (katex-font-assets))))
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

(defn write-open-sans-css!
  "Regenerate the committed resources/public/freememo/open-sans.css from Google's
   css2 endpoint. Run ONLY when deliberately updating the font — its whole point
   is to freeze both the basenames and the upstream directory in git.
   Post: the file's url()s are local, its @upstream-prefix header names the
         gstatic directory, and it references at least one woff2.
   Throws when css2 returns urls from more than one directory, which would make
   a single prefix wrong."
  [_]
  (let [css (String. (GET google-css2-url modern-ua) "UTF-8")
        urls (map second (re-seq #"url\((https://fonts\.gstatic\.com/[^)]*\.woff2)\)" css))
        dirs (distinct (map #(subs % 0 (inc (str/last-index-of % "/"))) urls))]
    (when (empty? urls)
      (throw (ex-info "css2 returned no woff2 urls (user-agent too old?)" {:type ::manifest})))
    (when (< 1 (count dirs))
      (throw (ex-info (str "css2 urls span " (count dirs) " directories; one @upstream-prefix cannot cover them")
               {:type ::manifest :dirs dirs})))
    (let [prefix (first dirs)
          local (str/replace css
                  #"url\(https://fonts\.gstatic\.com/[^)]*/([^/)]+\.woff2)\)"
                  (str "url(vendor/" open-sans-dir "$1)"))]
      (spit open-sans-css
        (str "/* Open Sans, self-hosted. Generated by: clj -X:build vendor/write-open-sans-css!\n"
          "   Do not edit by hand.\n\n"
          "   url()s are local; the woff2 files live in the gitignored vendor tree and are\n"
          "   downloaded by vendor/fetch! from the directory below. Freezing both halves here\n"
          "   means a Google font release cannot silently change what we serve.\n\n"
          "   @upstream-prefix " prefix "\n*/\n" local))
      (println (format "wrote %s — %d woff2 files, prefix %s"
                 open-sans-css (count urls) prefix)))))

(defn check-refs!
  "Every /freememo/vendor/… path named by the HTML or by freememo.vendor-libs
   must exist on disk.
   Post: silent success, or ex-info {:type ::dangling-ref} naming each missing
         path. Guards the ~20 hand-written paths in those three files."
  [_]
  (let [html (mapcat #(re-seq #"/freememo/vendor/[A-Za-z0-9@._/-]+" (slurp %))
               ["resources/public/freememo/index.prod.html"
                "resources/public/freememo/index.dev.html"])
        cljs (map #(str/replace % "\"" "")
               (re-seq #"\"[A-Za-z0-9._-]+@[0-9][A-Za-z0-9._/-]+\""
                 (slurp "src/freememo/vendor_libs.cljs")))
        refs (->> (concat (map #(str/replace % "/freememo/vendor/" "") html) cljs)
               (filter #(re-find #"\.(js|css|svg|gif|woff2)$" %))
               distinct sort)
        missing (remove #(.exists (io/file vendor-root %)) refs)]
    (when (seq missing)
      (doseq [m missing] (printf "DANGLING %s\n" m))
      (flush)
      (throw (ex-info (str (count missing) " referenced vendor path(s) missing")
               {:type ::dangling-ref :missing missing})))
    (println (format "vendor: all %d referenced paths present" (count refs)))))
