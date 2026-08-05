(ns freememo.pre-prompt-mode
  "How literally card generation reads the pre-prompt typed in Generate with
   Prompt. Three modes, most permissive first:

     \"guide\"    — the pre-prompt is a PATTERN for the questions the model
                    writes. The behaviour that shipped before this namespace
                    existed, and the default.
     \"clarify\"  — the pre-prompt IS the question, rephrased only for clarity.
     \"verbatim\" — the pre-prompt IS the question, word for word.

   Stored and transported as a STRING, matching the other enum-valued settings
   (reasoning, verbosity). `normalize` is the single gate: every consumer runs
   its input through it, so an absent row, an old value after a rename, or a
   nil from a caller that never sets the key all read as \"guide\" rather than
   selecting no wrapper at all.

   NO reader conditionals here, deliberately. Both peers must see plain
   literals: a CLJ-only def read from client scope compiles with no warning,
   emits nothing on the CLJS peer, and kills the WebSocket at mount — see
   freememo.transcribe-language, whose docstring records that crash."
  (:require [clojure.string :as str]))

(def guide "guide")
(def clarify "clarify")
(def verbatim "verbatim")

(def default guide)

(def modes
  "Selectable modes in control order — most permissive first, strictest last."
  [guide clarify verbatim])

(def ^:private labels
  {guide "Guide" clarify "Clarify" verbatim "Verbatim"})

(def ^:private descriptions
  {guide "The prompt guides the questions the model writes."
   clarify "The prompt is the question, rephrased only for clarity."
   verbatim "The prompt is the question, word for word."})

(defn normalize
  "Pre: anything. Post: one of `modes` — an unrecognized or absent value
   becomes `default`."
  [mode]
  (if (contains? labels mode) mode default))

(defn label [mode] (labels (normalize mode)))

(defn description [mode] (descriptions (normalize mode)))

(defn question-kind?
  "True for card kinds whose schema carries a question the pre-prompt can
   become. Basic has :q, overlapping has :question and must end with \"?\".
   Cloze has neither — system.md forbids question marks in cloze — so cloze can
   only be bounded to the prompt's topic, never made to ask it."
  [card-type]
  (or (= "basic" card-type) (= "overlapping" card-type)))

(defn single-card?
  "True when this generation must produce exactly one card: a non-blank
   pre-prompt, a mode past \"guide\", and a question-bearing kind.

   Single owner on purpose. The server forces the card count from this
   predicate and the dialog discloses \"Generates 1 card\" from the same one, so
   the promise the user reads and the count the model is given cannot drift."
  [mode card-type pre-prompt]
  (boolean (and pre-prompt
             (not (str/blank? pre-prompt))
             (not= guide (normalize mode))
             (question-kind? card-type))))
