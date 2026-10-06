(ns freememo.walkthrough-name
  "A walkthrough's name (plans/repository-home.md §4.3): the one the reader
   wrote, or else one derived from its steps.

   NO requires, so both peers read it: the server derives names for the
   repository home and the tab title, and the rename dialog checks a name
   before it is sent.")

(def max-name-chars
  "The longest name a reader may give a walkthrough."
  120)

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

(defn- unticked
  "`s` without the backticks of Markdown inline code."
  [s]
  (apply str (remove #{\`} (str s))))

(defn derived-name
  "A name from `steps`, each {:label :noise?} in order: the first step that is
   neither Noise nor \"Other changes\", plus \" and N more\" for the other such
   steps. With no such step, the first step's label. With no step at all, the
   range from `sha-from` to `sha-b`.
   Post: a non-blank string."
  [steps sha-from sha-b]
  (let [taught (remove #(or (:noise? %) (= "Other changes" (:label %))) steps)
        lead (or (first taught) (first steps))
        more (max 0 (dec (count taught)))]
    (if lead
      (str (unticked (:label lead)) (when (pos? more) (str " and " more " more")))
      (str (short-sha sha-from) " → " (short-sha sha-b)))))

(defn display-name
  "`name` when the reader set one, else `derived-name`."
  [name steps sha-from sha-b]
  (if (seq name) name (derived-name steps sha-from sha-b)))

(defn normalize-name
  "What a typed name stores.
   Post: {:name s} for a trimmed name of 1 to `max-name-chars` characters,
         {:name nil} for a blank one (the derived name shows again), or
         {:error s} for one that is too long."
  [typed]
  (let [s (.trim (str typed))]
    (cond
      (= "" s) {:name nil}
      (> (count s) max-name-chars) {:error (str "A name holds at most " max-name-chars " characters.")}
      :else {:name s})))
