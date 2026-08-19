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

;; The `id` attributor is the second registration with the same exposure. Quill
;; models no `id`, so without it every in-document anchor target dies in the
;; Delta on the first edit — and `freememo.html-cleaner` would be storing an
;; attribute the editor cannot keep. Its own `aget` reads are string-keyed for
;; the reason above; these assertions prove the string keys reached the bundle.
(def ^:private attributor-anchor "Quill.import(\"parchment\")")

;; Closure canonicalises `x["Attributor"]` to `x.Attributor` and, for a property
;; read on a value from an unknown global, leaves the NAME alone — measured:
;; `new Bjb.Attributor("id","id",{scope:Bjb.Scope.BLOCK})`. So these match the
;; dot form, as the divider requirements above do. Asserting the bracket form
;; fails on a bundle that is perfectly correct.
(def ^:private attributor-requirements
  [["the Attributor constructor survives by name" #"\.Attributor\("]
   ["Scope.BLOCK survives by name"                #"\.Scope\.BLOCK"]
   ["the attribute is named \"id\" twice"         #"\"id\"\s*,\s*\"id\""]
   ["Quill.register is called"                    #"Quill\.register"]])

(def ^:private max-block-chars
  "Each registration is one emitted IIFE; this covers it with room."
  400)

(defn- registration-findings
  "What the registration anchored at `anchor` fails to satisfy.

   Pre : `source` is the bundle's text; `requirements` is [label regex] pairs;
         `others` names every OTHER registration's anchor.
   Post: a vector of failure strings, empty when every requirement holds.
   Inv : the window stops at the next registration. Closure emitted the divider
         and the attributor adjacent, putting TWO `Quill.register` calls inside
         a fixed 400-char window — so a deleted divider registration would have
         passed on its neighbour's call, which is the exact elimination this
         file exists to catch.
   Inv : an anchor that occurs more than once is an error, not a first-match.
         Both anchors resolve by position, so a second occurrence anywhere
         earlier would silently retarget the whole check."
  [^String source what anchor others requirements]
  (let [at (.indexOf source ^String anchor)]
    (cond
      (neg? at)
      [(str "the " what " registration is absent from the bundle"
         " — the defonce was eliminated")]

      (not (neg? (.indexOf source ^String anchor (inc at))))
      [(str what ": the anchor " (pr-str anchor) " occurs more than once —"
         " the check cannot tell which registration it is reading")]

      :else
      (let [next-at (->> others
                      (map #(.indexOf source ^String % (inc at)))
                      (remove neg?)
                      (reduce min (count source)))
            block (subs source at (min next-at (+ at max-block-chars)))]
        (into [] (keep (fn [[label re]]
                         (when-not (re-find re block)
                           (str what ": " label " — NOT FOUND"))))
          requirements)))))

(defn verify!
  "Assert the `:advanced` bundle kept what a successful build does not guarantee.
   Post: silent success, or ex-info {:type ::bundle-check} naming each failure.
   Inv : reads only the built artifact, so it cannot pass by reading the source
         it is meant to police."
  [_]
  (let [bundle (prod-bundle)
        source (slurp bundle)
        findings (into (registration-findings source "divider blot"
                         divider-anchor [attributor-anchor] divider-requirements)
                   (registration-findings source "id attributor"
                     attributor-anchor [divider-anchor] attributor-requirements))]
    (when (seq findings)
      (throw (ex-info (str "bundle-check: " (.getName bundle) " failed "
                        (count findings) " assertion(s):\n  "
                        (str/join "\n  " findings))
               {:type ::bundle-check :bundle (.getName bundle) :findings findings})))
    (println (format "bundle-check: %s satisfies %d assertions"
               (.getName bundle)
               (+ (count divider-requirements) (count attributor-requirements))))))
