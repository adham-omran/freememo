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
   [freememo.crawler-page :as crawler-page]
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

(def text-only-paths
  "Paths where an HTML response is a lie, so this handler answers 404 instead.

   Every one of these is fetched by a machine that expects `text/plain` or XML and
   treats markup as a malformed file.

   `/ads.txt` is a member again. A route in `freememo.api` owned it while this
   deployment published an authorized-sellers body; that route went with the ad
   path on 2026-08-06, and without an owner the path would fall to the shell and
   answer `200 text/html` — the exact defect
   plans/crawler-rendering-and-well-known-files.md fixed. A deployment that
   publishes such a file again must route it, and drop it from this set in the
   same commit.

   `/robots.txt` is absent on purpose: a static resource owns it and is matched
   before this handler runs, so listing it here would give one path two owners."
  #{"/ads.txt" "/security.txt" "/sitemap.xml"})

(defn- text-only-path?
  "Whether `uri` must never receive HTML.
   Post: true for a member of `text-only-paths` and for anything under
   `/.well-known/`, which is reserved by RFC 8615 for machine-read files."
  [uri]
  (boolean (and uri
                (or (contains? text-only-paths uri)
                    (str/starts-with? uri "/.well-known/")))))

(defn- crawler-request?
  "Whether this request comes from a crawler that cannot render the app.
   Pre: `ring-req` carries lower-cased header keys, as Ring guarantees.
   Post: true only for a GET whose user-agent `freememo.crawler-page` recognizes.
   A non-GET is excluded because the crawler document answers no other method,
   and reading the session is deliberately avoided — a crawler carries none."
  [ring-req]
  (and (= :get (:request-method ring-req))
       (crawler-page/crawler-user-agent? (get-in ring-req [:headers "user-agent"]))))

(defn- crawler-response
  "The response a recognized crawler gets for `uri`.

   Post: the static landing document for \"/\", and 404 for every other path.
   One crawlable URL is the point: this handler is the catch-all, so serving the
   document everywhere would put identical text on unbounded URLs, which search
   engines discount as duplicate content.
   A future public page needs an entry here, or it stays invisible to crawlers."
  [uri]
  (if (= "/" uri)
    (-> (ring-response/response (crawler-page/landing-document))
      (ring-response/content-type "text/html; charset=utf-8")
      ;; Static per deploy, unlike the shell's no-store: the document is the same
      ;; for every crawler until the copy or the build changes.
      (ring-response/header "Cache-Control" "public, max-age=3600"))
    (-> (ring-response/not-found "Not found")
      (ring-response/content-type "text/plain; charset=utf-8"))))

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
    (cond
      ;; Machine-read files must never get markup. Answered before the shell,
      ;; because the shell is what would otherwise answer them with HTML.
      (text-only-path? (:uri ring-req))
      (-> (ring-response/not-found "Not found")
        (ring-response/content-type "text/plain; charset=utf-8"))

      ;; A crawler cannot run the app at all: Googlebot's renderer has no
      ;; WebSocket support and this shell ships an empty body, so without this
      ;; branch every crawler read 0 words at every URL.
      (crawler-request? ring-req)
      (crawler-response (:uri ring-req))

      :else
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
        (next-handler ring-req)))))
