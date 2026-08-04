(ns freememo.shell
  "Serves the production HTML shell — `index.prod.html` with its per-request
   substitutions.

   Both production entrypoints (`prod`, `prod-jetty9`) mount `wrap-prod-index-page`
   from here. They used to carry byte-identical copies of it plus `template` and
   the manifest reader, which meant a change to the shell had two homes and one of
   them was routinely forgotten.

   Two substitutions, and they differ in lifetime:
     - the compiled javascript module paths, read once from the shadow-cljs
       manifest and fingerprinted for cache busting;
     - this deployment's head contribution, resolved per request because it may
       carry a one-shot event (see `freememo.deployment-hook`). It is \"\" on a
       deployment that adds nothing, which is the default."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [freememo.deployment-hook :as hook]
   [ring.util.response :as ring-response]))

(defn template
  "In string template `\"<div>$:foo/bar$</div>\"`, replace all instances of $key$
   with target specified by map `m`. Target values are coerced to string with `str`.
   E.g. (template \"<div>$:foo$</div>\" {:foo 1}) => \"<div>1</div>\".

   Post: every `$key$` present in `t` for a key in `m` is replaced. A key in `m`
   with no placeholder in `t` is silently dropped, and a placeholder with no key
   in `m` is left in the output verbatim — so a caller MUST supply every key its
   template names, including the empty string.
   String matches replace literally, so `$` in a replacement value is safe."
  [t m]
  (reduce-kv (fn [acc k v] (str/replace acc (str "$" k "$") (str v))) t m))

(defn compiled-javascript-modules
  "Read the shadow-cljs build manifest into template keys.
   Post: {:hyperfiddle.client.module/<name> \"<folder>/<output-name>\"}, or nil
   when the manifest is absent from the classpath (no client build)."
  [manifest-path]
  (when-let [manifest (io/resource manifest-path)]
    (let [manifest-folder (when-let [folder-name (second (rseq (str/split manifest-path #"\/")))]
                            (str folder-name "/"))]
      (->> (slurp manifest)
        (edn/read-string)
        (reduce (fn [r module]
                  (assoc r (keyword "hyperfiddle.client.module" (name (:name module)))
                    (str manifest-folder (:output-name module))))
          {})))))

(defn wrap-prod-index-page
  "Serves `index.prod.html` with injected javascript modules from `manifest.edn`.
   `manifest.edn` is generated at client build time and contains javascript modules
   information (e.g. file location and file hash).

   Pre: `config` carries string `:resources-path` and `:manifest-path`.
   Post: 200 text/html `no-store` for any request that reaches this handler — it
   inspects neither method nor path, so it is the catch-all for everything no API
   route and no static resource matched. Falls through to `next-handler` only when
   the shell itself is missing from the classpath.
   `:session` is attached only when `hook/shell-head` returns one; attaching
   nil would DELETE the session, because ring rewrites the cookie whenever the
   response map contains the key."
  [next-handler config]
  (fn [ring-req]
    (assert (string? (:resources-path config)))
    (assert (string? (:manifest-path config)))
    (if-let [response (ring-response/resource-response
                        (str (:resources-path config) "/freememo/index.prod.html"))]
      (if-let [module (compiled-javascript-modules (:manifest-path config))]
        (let [{:keys [html session]} (hook/shell-head ring-req)]
          (cond-> (-> (ring-response/response
                        (template (slurp (:body response))
                          (merge config module {:freememo/deployment-head html})))
                    (ring-response/content-type "text/html")
                    ;; never cache – this is dynamically generated content.
                    (ring-response/header "Cache-Control" "no-store"))
            session (assoc :session session)))
        (-> (ring-response/not-found (pr-str ::missing-shadow-build-manifest))
          (ring-response/content-type "text/plain")))
      ;; else – index.prod.html wasn't found on classpath
      (next-handler ring-req))))
