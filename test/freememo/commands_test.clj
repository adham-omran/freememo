(ns freememo.commands-test
  "Guards the command-architecture invariants (README \"Command architecture\"):
   1. The registry is well-formed — ids valid, :views ⊆ known channels,
      :bind collision-free, :class valid.
   2. Single bump authority — no source file outside freememo.commands
      increments an invalidation channel atom by hand."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.commands :as commands]
            [freememo.user-state :as user-state]))

(deftest registry-is-valid
  (is (empty? (commands/validate-registry))
    (str "registry violations: " (vec (commands/validate-registry)))))

(deftest every-invalidation-channel-has-a-counter
  (doseq [channel commands/invalidation-channels]
    (testing (str channel)
      (is (instance? clojure.lang.Atom (user-state/get-atom -1 channel))))))

(deftest every-entry-has-label-and-class
  (doseq [[id entry] commands/registry]
    (testing (str id)
      (is (string? (:label entry)) "missing :label")
      (is (#{:mutation :query :nav} (:class entry)) "invalid :class"))))

(deftest anki-review-commands-run-only-in-the-viewer
  (doseq [id [:find-next-marked-card
              :find-next-suspended-card
              :find-next-remarked-card]]
    (let [entry (commands/command id)]
      (is (= :electric (:exec entry)))
      (is (= #{:viewer} (:when entry)))
      (is (not (:palette-hidden entry))))))

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

;; ── Page-flip chords ────────────────────────────────────────────────────────
;; `effective-bind` and `display-chord` are pure, so the substitution and the
;; two key-name glyphs are testable without a browser. `util/mac-platform?`
;; returns false on the JVM, so these assert the NON-macOS branch — the one
;; that carries the substitution and is therefore the one that can be wrong.

(deftest page-flip-chords-avoid-the-reserved-tab-accelerator
  (testing "off macOS the two page-flip chords take alt, not ctrl"
    ;; ctrl+pg-up / ctrl+pg-down are Chrome's previous-tab and next-tab
    ;; (IsReservedCommandOrKey), so the page never receives them.
    (is (= "alt+pg-up" (commands/effective-bind :prev-page "meta+pg-up")))
    (is (= "alt+pg-down" (commands/effective-bind :next-page "meta+pg-down"))))
  (testing "every other bound command keeps the meta→ctrl substitution"
    (doseq [[id bind] (commands/bindings)
            :when (not (contains? #{:prev-page :next-page :toggle-palette} id))]
      (testing (str id)
        (is (not (str/includes? (commands/effective-bind id bind) "alt+pg"))
          "an unrelated command picked up the page-flip substitution")))))

(deftest display-chord-names-the-page-keys
  (testing "goog's pg-up / pg-down do not reach a label as PG-UP / PG-DOWN"
    (is (= "Alt+PgUp" (commands/display-chord :prev-page)))
    (is (= "Alt+PgDn" (commands/display-chord :next-page))))
  (testing "no bound command renders a raw goog key name"
    (doseq [[id _] (commands/bindings)]
      (testing (str id)
        (is (not (str/includes? (commands/display-chord id) "PG-"))
          "display-chord fell through to its upper-case fallback")))))
