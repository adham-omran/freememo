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

(def commands
  "Every command, in the order the composer's menu lists them. `:completion`
   is the text a pick writes into the draft: a command that takes an argument
   ends in a space, so the learner types the argument next."
  [{:text compact-command
    :usage compact-command
    :completion compact-command
    :description "Summarize this chat's conversation"}
   {:text fork-command
    :usage (str fork-command " <question>")
    :completion (str fork-command " ")
    :description "Fork this chat and ask the question in the fork"}])

(defn suggestions
  "The commands the composer's menu offers for `draft`.
   Pre:  `draft` is the composer text as typed, or nil.
   Post: nil unless `draft` starts with `/` and holds no whitespace, which is
         while the learner is still typing a command's name. Otherwise the
         commands whose text starts with `draft`, possibly none."
  [draft]
  (when (and (string? draft)
          (str/starts-with? draft "/")
          (not (re-find #"\s" draft)))
    (filterv #(str/starts-with? (:text %) draft) commands)))

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
