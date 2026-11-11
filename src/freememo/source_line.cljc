(ns freememo.source-line
  "What the Source Document line shows: above a topic in the Topic View, and on
   line 1 of a Learn queue row (plans/topic-view-source-line.md,
   plans/learn-queue-source-lines.md). The terms are in CONTEXT.md, § Topics.

   The line names the Source Document, then the topic title after \" › \".
   It drops the title when the title only repeats the first words of the body,
   which is the title that an extract gets at creation. A renamed topic keeps
   its title in the line."
  (:require [clojure.string :as str]))

(def separator " › ")

(defn- squash
  "Lower case, with no tags, no entities and no white space, so that a title
   cut from a selection compares equal to the stripped body text."
  [s]
  (-> (or s "")
      (str/replace #"<[^>]*>" "")
      (str/replace #"&[a-zA-Z]+;|&#\d+;" "")
      (str/replace #"\s+" "")
      (str/replace #"(\.\.\.|…)$" "")
      str/lower-case))

(defn title-from-body?
  "True when `title` is the first words of `body-text`.
   Pre : both are strings or nil. `body-text` can be a prefix of the body.
   Post: false for a blank title."
  [title body-text]
  (let [t (squash title)]
    (and (seq t) (str/starts-with? (squash body-text) t))))

(defn topic-line
  "The parts of the line for one topic.
   Pre : `:root?` is true for a topic with no parent. `:source-document` is the
         title of the topic's Source Document, or nil. `:title` is the topic's
         own title. `:body-text` is the start of its body as plain text.
   Post: `{:source-document s :title t}`. `s` is never nil. `t` is nil when the
         line shows only the Source Document."
  [{:keys [root? source-document title body-text]}]
  {:source-document (or (not-empty source-document) (or title ""))
   :title (when-not (or root?
                        (str/blank? title)
                        (title-from-body? title body-text))
            title)})

(defn full-text
  "The whole line as one string, for a tooltip or an accessible label."
  [{:keys [source-document title]}]
  (if title (str source-document separator title) source-document))
