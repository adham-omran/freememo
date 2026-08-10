(ns bundle-check
  "Assertions about the `:advanced` client bundle that a successful build does
   not make. JDK-only, like `vendor` — no node, no python.

   Closure's renaming and dead-code elimination can silently delete code the
   build reports as compiled. A `<script>`-loaded library that reads one of our
   properties by its literal name is the exposed case: Closure sees a write
   nothing reads and removes it, the build prints 0 warnings, and the feature is
   absent only in production.

   Run after `build-client`:

     clj -X:build:prod build-client
     clj -X:build bundle-check/verify!"
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io File]))

(def ^:private js-dir "resources/public/freememo/js")

;; `main.<32 hex>.js` is the :prod module — `:module-hash-names true` in
;; shadow-cljs.edn. A bare `main.js` is the :dev build's output in the same
;; directory, and asserting against it would prove nothing about :advanced.
(def ^:private prod-bundle-re #"^main\.[0-9A-F]{16,}\.js$")

(defn- prod-bundle
  "The `:advanced` bundle.
   Post: a File. Throws ex-info when none exists, or when more than one does —
         `build-client` deletes the directory first, so two means a stale bundle
         is present and a check could silently read the wrong one."
  ^File []
  (let [found (->> (.listFiles (io/file js-dir))
                (filter #(re-matches prod-bundle-re (.getName ^File %)))
                vec)]
    (case (count found)
      1 (first found)
      0 (throw (ex-info (str "no :prod bundle in " js-dir
                          " — run `clj -X:build:prod build-client` first")
                 {:type ::no-bundle :dir js-dir}))
      (throw (ex-info (str (count found) " :prod bundles in " js-dir
                        " — delete the directory and rebuild")
               {:type ::ambiguous-bundle
                :found (mapv #(.getName ^File %) found)})))))

;; The divider blot is registered by a namespace-load `defonce` in
;; freememo.quill-field, and Parchment reads `blotName` off the class by that
;; literal name. `blotName` appears in no Closure extern, so a
;; `(set! (.-blotName Divider) …)` write is one Closure believes nothing reads —
;; it DELETED the assignment and shipped `x.tagName="HR";Quill.register(x)`,
;; which registers nothing and drops every `<hr>` in production while dev works.
;; `(aset Divider "blotName" …)` is what prevents that. This checks the outcome
;; rather than the source, because the source form is easy to change back.
(def ^:private divider-anchor "Quill.import(\"blots/block/embed\")")

(def ^:private divider-requirements
  "[label regex] pairs the emitted registration must satisfy. Closure emits an
   unquoted property write when it keeps the name, so match on the name and its
   value rather than on a quoted key."
  [["blotName survives, keyed \"divider\"" #"\.blotName\s*=\s*\"divider\""]
   ["tagName survives, keyed \"HR\""       #"\.tagName\s*=\s*\"HR\""]
   ["Reflect.construct bridges the ES6 superclass" #"Reflect\.construct"]
   ["Quill.register is called"             #"Quill\.register"]])

(defn- divider-findings
  "What the divider registration in `source` fails to satisfy.
   Pre : `source` is the bundle's text.
   Post: a vector of failure strings, empty when every requirement holds."
  [^String source]
  (let [at (.indexOf source divider-anchor)]
    (if (neg? at)
      ["the divider registration is absent from the bundle — the defonce was eliminated"]
      ;; The registration is one emitted IIFE; 400 chars covers it with room.
      (let [block (subs source at (min (count source) (+ at 400)))]
        (into [] (keep (fn [[label re]]
                         (when-not (re-find re block)
                           (str label " — NOT FOUND"))))
          divider-requirements)))))

(defn verify!
  "Assert the `:advanced` bundle kept what a successful build does not guarantee.
   Post: silent success, or ex-info {:type ::bundle-check} naming each failure.
   Inv : reads only the built artifact, so it cannot pass by reading the source
         it is meant to police."
  [_]
  (let [bundle (prod-bundle)
        source (slurp bundle)
        findings (divider-findings source)]
    (when (seq findings)
      (throw (ex-info (str "bundle-check: " (.getName bundle) " failed "
                        (count findings) " assertion(s):\n  "
                        (str/join "\n  " findings))
               {:type ::bundle-check :bundle (.getName bundle) :findings findings})))
    (println (format "bundle-check: %s satisfies %d divider-blot assertions"
               (.getName bundle) (count divider-requirements)))))
