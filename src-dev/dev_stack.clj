(ns dev-stack
  "Dev-only: run compilation on a thread with an explicitly sized stack.

   Electric's macroexpander (`hyperfiddle.electric.impl.lang3/-expand-all`)
   recurses once per node of the expanded form, and Clojure's `load` nests a
   compiler frame stack per `require` level. Expanding this app's namespaces
   needs more than the JVM's 1 MB default `ThreadStackSize`, so the dev boot
   overflows intermittently (measured: 1 failure in 5 cold starts). The `e/defn`
   named in the crash moves with the stack size — 512k blames
   `home_page.cljc:39`, 640k `ai_features_section.cljc:336`, 768k
   `knowledge_tree.cljc:457` — so there is no single form to shrink.

   `:prod` and `:build` buy headroom with `-Xss4m`; the `:dev` alias carries no
   `:jvm-opts` by choice, so the dev boot sizes its own stack instead. A
   `Thread` built with a `stackSize` argument overrides the JVM default without
   any launcher flag.

   Not covered: nREPL session threads (`nrepl.SessionThread` calls
   `Thread.<init>()`, no factory knob) and shadow's build workers
   (`shadow.cljs.devtools.server.util/thread` → `(Thread. f)`). Reload a large
   namespace from the editor via `dev/reload!` rather than a bare `require`.

   NOT callable during RT initialization — that means `user.clj` is off limits.
   `clojure.lang.RT/doInit` is `synchronized` on `RT.class` and loads `user.clj`
   before setting `INITIALIZED`, so a loader thread that touches any Java class
   whose static initializer calls `RT.init()` blocks on that monitor while the
   main thread blocks in `join`. Observed deadlock: telemere-slf4j's
   `TelemereLogger.init` → `clojure.java.api.Clojure.<clinit>` → `RT.init()`,
   reached from `hyperfiddle.log/get-logger*` while loading the Electric runtime.
   `dev/-main` runs after RT init, which is why the app load lives there.

   See plans/dev-jvm-stack-overflow-loader-thread.md.")

(def ^:private default-stack-mb
  "16 MB is the confirmed-working figure: the whole app loads on a 16 MB thread
   even when the JVM default is -Xss512k."
  16)

(defn- stack-bytes
  "Loader-thread stack size in bytes.

   Pre: `DEV_LOADER_STACK_MB`, if set, is a positive integer.
   Post: > 0. Malformed override throws rather than silently falling back."
  ^long []
  (if-some [configured (System/getenv "DEV_LOADER_STACK_MB")]
    (let [mb (parse-long (.trim ^String configured))]
      (when-not (and mb (pos? mb))
        (throw (IllegalArgumentException.
                 (str "DEV_LOADER_STACK_MB must be a positive integer, got: "
                   (pr-str configured)))))
      (* mb 1024 1024))
    (* default-stack-mb 1024 1024)))

(defn call-on-big-stack
  "Invoke `thunk` on a fresh thread whose stack is `DEV_LOADER_STACK_MB` MB
   (default 16), and return its value.

   Pre: `thunk` is a 0-arg fn, and RT initialization has finished (see the ns
   docstring — calling this from `user.clj` deadlocks).
   Post: `thunk` has run to completion before this returns — the loader thread
   is joined, so callers may rely on required namespaces being fully loaded.
   A throwable from `thunk` is rethrown here, on the caller's thread, with the
   loader thread's stack trace intact.
   Blame: a `StackOverflowError` escaping this fn means `thunk` needs more than
   the configured stack, not that the mechanism failed."
  [thunk]
  (let [!value (volatile! nil)
        !error (volatile! nil)
        loader (Thread. nil
                 ^Runnable (fn [] (try (vreset! !value (thunk))
                                       (catch Throwable e (vreset! !error e))))
                 "dev-stack-loader"
                 (stack-bytes))]
    (.start loader)
    (.join loader)
    (if-some [e @!error]
      (throw e)
      @!value)))
