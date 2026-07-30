(ns freememo.vendor-libs
  "On-demand loading of the vendored third-party libraries that are NOT in
   index*.html.

   All third-party JS lives under /freememo/vendor/<lib>@<version>/ and is served
   with an immutable cache header (src-prod/prod.cljc). Quill, quill-resize,
   highlight.js and KaTeX stay as <script> tags in the HTML: Quill's syntax
   module reads window.hljs at editor construction, and quill_field's
   namespace-load `defonce` patches Quill's icon registry before any guard runs.
   The five groups here have no tag at all — nothing fetches them until a feature
   asks, which keeps ~305 KB (gzip) of JS off the boot path.

   ensure!
     Pre  — `group` is a key of `specs`.
     Post — the returned promise resolves with true once every file in the group
            has executed AND every global named in :probes is defined; it rejects
            if a file fails to load or a probe is still missing afterwards.
     Inv  — one <script> element per file for the lifetime of the page, and one
            shared promise per group however many callers race.

   Plain defns over a `defonce` cache, never `e/defn`: Electric re-evaluates
   reactive bodies unpredictably, so injection guards must live outside the
   reactive graph (CLAUDE.md, 'JS Library Init Side Effects in e/defn').

   The vendor tree is NOT in git — `clj -X:build vendor/fetch!` downloads it and
   hash-verifies it against the committed vendor-lock.sha256 (src-build/vendor.clj
   owns the versions). Version strings appear both there and here;
   `clj -X:build vendor/check-refs!` fails when a path named here or in the HTML
   is absent from the tree."
  (:require [goog.object :as gobj]
            [freememo.logging :as log]))

(def ^:private root "/freememo/vendor/")

(def ^:private specs
  {:pdf-js
   {:files ["pdf.js@3.11.174/pdf.min.js"
            "pdf.js@3.11.174/pdf_viewer.min.js"]
    :probes ["pdfjsLib" "pdfjsViewer"]
    ;; workerSrc is versioned state, so it belongs next to the version — not
    ;; duplicated at each pdf.js call site (it was an inline <script> before).
    :after (fn []
             (set! (.. js/window -pdfjsLib -GlobalWorkerOptions -workerSrc)
               (str root "pdf.js@3.11.174/pdf.worker.min.js")))}

   :konva
   {:files ["konva@9.3.16/konva.min.js"]
    :probes ["Konva"]}

   ;; graphology MUST execute before sigma — sigma's Graph model comes from it.
   :graph
   {:files ["graphology@0.26.0/graphology.umd.min.js"
            "sigma@3.0.2/sigma.min.js"]
    :probes ["graphology" "Sigma"]}

   ;; regions.min.js attaches window.WaveSurfer.Regions, so core comes first.
   :wavesurfer
   {:files ["wavesurfer.js@7.8.6/wavesurfer.min.js"
            "wavesurfer.js@7.8.6/plugins/regions.min.js"]
    :probes ["WaveSurfer"]}

   :lamejs
   {:files ["lamejs@1.2.1/lame.min.js"]
    :probes ["lamejs"]}

   ;; KaTeX is prefetched at boot rather than tagged in the HTML. It was two
   ;; `defer` tags plus an inline promise and an onload= attribute; routing it
   ;; through ensure! deletes all three inline snippets (the precondition for a
   ;; `script-src 'self'` CSP) while keeping the same non-blocking arrival.
   ;; auto-render must follow the core, which defines window.katex.
   :katex
   {:files ["katex@0.16.11/katex.min.js"
            "katex@0.16.11/contrib/auto-render.min.js"]
    :probes ["katex" "renderMathInElement"]}})

(defonce ^:private !pending (atom {}))

(defn- inject!
  "Append one <script src> and resolve when it executes.
   Post: rejects with an Error naming `path` if the browser reports an error."
  [path]
  (js/Promise.
    (fn [resolve reject]
      (let [^js el (.createElement js/document "script")]
        (.addEventListener el "load" (fn [_] (resolve true)) #js {:once true})
        (.addEventListener el "error"
          (fn [_] (reject (js/Error. (str "vendor script failed to load: " path))))
          #js {:once true})
        (set! (.-src el) (str root path))
        (.appendChild (.-head js/document) el)))))

(defn- probes-present?
  "Globals named in `probes` that are still undefined. Reads by string key so
   :advanced never renames them."
  [probes]
  (remove #(some? (gobj/get js/window %)) probes))

(defn loaded?
  "True when every global of `group` is already defined. For call sites that
   must stay synchronous; prefer awaiting ensure!."
  [group]
  (boolean (when-let [{:keys [probes]} (get specs group)]
             (empty? (probes-present? probes)))))

(defn ensure!
  "Load `group` if needed; returns the group's shared promise.
   Files are injected one at a time because every multi-file group here has an
   execution-order dependency."
  [group]
  (or
    (get @!pending group)
    (if-let [{:keys [files probes after]} (get specs group)]
      ;; The miss-then-store below cannot interleave: JS is single-threaded and
      ;; nothing awaits between the `get` above and the `swap!` below.
      (let [p (-> (reduce (fn [chain path] (.then chain (fn [_] (inject! path))))
                    (js/Promise.resolve true)
                    files)
                (.then (fn [_]
                         (let [missing (probes-present? probes)]
                           (when (seq missing)
                             (throw (js/Error. (str "vendor group " group
                                                 " loaded but globals missing: "
                                                 (pr-str (vec missing)))))))
                         (when after (after))
                         true))
                (.catch (fn [^js err]
                          (log/log-warn (str "vendor group " group " failed: "
                                          (.-message err)))
                          ;; Drop the cache entry so a later attempt can retry.
                          (swap! !pending dissoc group)
                          (throw err))))]
        (swap! !pending assoc group p)
        p)
      (js/Promise.reject (js/Error. (str "unknown vendor group: " group))))))

(defn with!
  "Run `f` once `group` is loaded — the shape imperative mount callbacks want.
     Pre  — f is side-effecting and safe to run one tick later than the caller.
     Post — f runs at most once, after the group's globals exist; if the group
            fails to load, f never runs and ensure! has logged the failure.
   Returns nil, so a caller cannot mistake it for the handle f produces."
  [group f]
  (.then (ensure! group) (fn [_] (f)))
  nil)

(defn prefetch!
  "Start loading `group` without awaiting it; ensure! logs any failure.
   Returns nil so callers cannot accidentally treat it as the promise."
  [group]
  (.catch (ensure! group) (fn [_] nil))
  nil)
