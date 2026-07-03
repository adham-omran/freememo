(ns freememo.commands-test
  "Guards the command-architecture invariants (README \"Command architecture\"):
   1. The registry is well-formed — ids valid, :views ⊆ known channels,
      :bind collision-free, :class valid.
   2. Single bump authority — no source file outside freememo.commands
      increments an invalidation channel atom by hand."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.commands :as commands]))

(deftest registry-is-valid
  (is (empty? (commands/validate-registry))
    (str "registry violations: " (vec (commands/validate-registry)))))

(deftest every-entry-has-label-and-class
  (doseq [[id entry] commands/registry]
    (testing (str id)
      (is (string? (:label entry)) "missing :label")
      (is (#{:mutation :query :nav} (:class entry)) "invalid :class"))))

;; ── Single bump authority ───────────────────────────────────────────────────

(def ^:private bump-pattern
  ;; A hand-written channel bump: swap! on a user-state atom for one of the
  ;; invalidation channels. Status atoms (:pending-cards, :card-gen-status, …)
  ;; are NOT channels and stay free to swap!.
  (re-pattern
    (str "swap!\\s+\\(us/get-atom\\s+\\S+\\s+:("
      (str/join "|" (map name commands/invalidation-channels))
      ")\\)\\s+inc")))

(defn- clj-sources []
  (->> (file-seq (io/file "src"))
    (filter #(and (.isFile %) (re-find #"\.clj[cs]?$" (.getName %))))))

(deftest single-bump-authority
  (doseq [f (clj-sources)
          :when (not (str/ends-with? (.getPath f) "freememo/commands.cljc"))]
    (testing (.getPath f)
      (is (nil? (re-find bump-pattern (slurp f)))
        "hand-written channel bump — declare :views in freememo.commands and call bump!/bump-channels! instead"))))
