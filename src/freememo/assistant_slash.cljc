(ns freememo.assistant-slash
  "The assistant composer's slash commands, parsed in one place.

   The server dispatches on `parse` (`assistant/submit-chat-input!`), and the
   browser reads the same result to decide what the optimistic echo shows
   (`assistant-submit-effect`). Before this namespace each side matched the
   literal text on its own. No other namespace names a command's text
   (plans/assistant-chat-forks.md §4.3.1)."
  (:require [clojure.string :as str]))

(def compact-command
  "Summarizes the open chat's own conversation into a checkpoint."
  "/compact")

(def fork-command
  "Forks the open chat at its latest message, with the question after it."
  "/fork")

(def fork-usage-error
  "The error a `/fork` with no question gets."
  (str "Type a question after " fork-command "."))

(defn parse
  "The command that composer text names.

   Pre:  `text` is the draft, or nil.
   Post: nil for an ordinary message. {:command :compact} for `/compact`.
         {:command :fork :question q} for `/fork`, with `q` the trimmed text
         after it, which is blank when no question follows. `/compact` must
         be the whole draft, and `/fork` must end the draft or be followed by
         whitespace: `/forked` and `/compact now` are ordinary messages."
  [text]
  (let [draft (str/trim (or text ""))]
    (cond
      (= compact-command draft)
      {:command :compact}

      (= fork-command draft)
      {:command :fork :question ""}

      (and (str/starts-with? draft fork-command)
        (re-matches #"(?s)\s.*" (subs draft (count fork-command))))
      {:command :fork :question (str/trim (subs draft (count fork-command)))})))
