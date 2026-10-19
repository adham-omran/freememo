(ns build
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.tools.build.api :as b]
            [clojure.tools.logging :as log]
            [shadow.cljs.devtools.api :as shadow-api]
            [shadow.cljs.devtools.server :as shadow-server]))

(def electric-user-version (b/git-process {:git-args "describe --tags --long --always --dirty"}))

;; Deployed short commit SHA, baked into electric-manifest.edn for the About
;; readout. nil under a Docker build (context has no .git) — the deploy's
;; :git-commit arg supplies it there, same as :version.
(def default-git-commit (b/git-process {:git-args "rev-parse --short HEAD"}))

(defn build-client "
invoke like: clj -X:build:prod build-client`
Note: do not use `clj -T`, because Electric shadow compilation requires
application classpath to be available"
  [{:keys [optimize debug verbose version shadow-build git-commit]
    :or {optimize true, debug false, verbose false, version electric-user-version, shadow-build :prod, git-commit default-git-commit}
    :as config}]
  (log/info 'build-client (pr-str config #_argmap))
  (b/delete {:path "resources/public/freememo/js"})
  (b/delete {:path "resources/electric-manifest.edn"})

  ; bake electric-user-version into artifact, cljs and clj
  (b/write-file {:path "resources/electric-manifest.edn" :content (-> config (dissoc :version :shadow-build) (assoc :hyperfiddle/electric-user-version version :git-commit git-commit))})

  ; "java.lang.NoClassDefFoundError: com/google/common/collect/Streams" is fixed by
  ; adding com.google.guava/guava {:mvn/version "31.1-jre"} to deps,
  ; see https://hf-inc.slack.com/archives/C04TBSDFAM6/p1692636958361199
  (shadow-server/start!)
  (as->
      (shadow-api/release shadow-build
        {:debug   debug,
         :verbose verbose,
         :config-merge
         [{:compiler-options {:optimizations (if optimize :advanced :simple)}
           :closure-defines  {'hyperfiddle.electric-client3/ELECTRIC_USER_VERSION version}}]})
      shadow-status (assert (= shadow-status :done) "shadow-api/release error")) ; fail build on error
  (shadow-server/stop!)
  (log/info "client built"))

(def class-dir "target/classes")

(defn- case-sensitive?
  "Whether the filesystem holding `dir` tells `a` from `A`."
  [dir]
  (let [probe (io/file dir "case-probe")]
    (io/make-parents probe)
    (spit probe "")
    (try (not (.exists (io/file dir "CASE-PROBE")))
         (finally (.delete probe)))))

(defn- sh! [& args]
  (let [{:keys [exit err]} (apply shell/sh args)]
    (when-not (zero? exit)
      (throw (ex-info (str (first args) " failed: " err) {:args args})))))

(defn- with-class-dir
  "Run `(f dir)` with a class directory on a case-sensitive filesystem.

   AOT compiling Electric writes classes whose names differ only in case,
   such as `electric3$Apply` and `electric3$apply`. On a case-insensitive
   disk, the default on macOS, the second file overwrites the first, and the
   jar fails at launch with NoClassDefFoundError (wrong name). A jar entry
   keeps its case, so only the class directory needs a case-sensitive disk.
   On macOS that is a sparse disk image, attached for this build only.
   Post: the image is detached, even when `f` throws."
  [f]
  (if (case-sensitive? "target")
    (f class-dir)
    (let [image (.getAbsolutePath (io/file "target/classes.sparseimage"))
          mount (.getAbsolutePath (io/file "target/classes-cs"))]
      (log/info "aot: this disk ignores case; compiling on a case-sensitive image")
      (sh! "hdiutil" "create" "-size" "8g" "-type" "SPARSE" "-fs" "Case-sensitive APFS"
        "-volname" "freememo-classes" image)
      (.mkdirs (io/file mount))
      (sh! "hdiutil" "attach" "-nobrowse" "-mountpoint" mount image)
      (try (f mount)
           (finally (sh! "hdiutil" "detach" "-force" mount))))))

(defn uberjar
  [{:keys [optimize debug verbose ::jar-name, ::skip-client, ::aot, version, aliases, shadow-build]
    :or {optimize true, debug false, verbose false, skip-client false, aot false, version electric-user-version, aliases [:prod], shadow-build :prod}
    :as args}]
  ; careful, shell quote escaping combines poorly with clj -X arg parsing, strings read as symbols
  (log/info 'uberjar (pr-str args))
  (b/delete {:path "target"})

  (when-not skip-client
    (build-client (select-keys args [:optimize :debug :verbose :version :shadow-build :git-commit])))

  ((if aot with-class-dir #(% class-dir))
   (fn [class-dir]
     (b/copy-dir {:target-dir class-dir :src-dirs ["src" "src-prod" "resources"]})
     (let [jar-name (or (some-> jar-name str) ; override for Dockerfile builds to avoid needing to reconstruct the name
                      (format "freememo-%s.jar" version))
           basis (b/create-basis {:project "deps.edn" :aliases aliases})]
       ;; The desktop jar starts on the user's computer at every launch, so it
       ;; is AOT compiled: from source, `prod` and its dependencies compile for
       ;; about 70 s before the port opens. The server deploy keeps source
       ;; loading. Compile after copy-dir, so each class is newer than its
       ;; source and Clojure loads the class.
       (when aot
         (log/info "aot: compiling prod")
         (b/compile-clj {:basis basis
                         :src-dirs ["src" "src-prod"]
                         :class-dir class-dir
                         :ns-compile '[prod]
                         :java-opts ["-Xss4m"]}))
       (b/uber {:class-dir class-dir
                :uber-file (str "target/" jar-name)
                :basis     basis})
       (log/info jar-name)))))
