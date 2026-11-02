(ns freememo.undo-scope
  "Which undo entries the current view shows. Cmd-Z undoes the newest entry
   whose subject the view shows, and nothing else (plans/remove-toasts.md 1.8.1).

   A SUBJECT is a plain value that names a place in the app:
     [:topic id]  the viewer of that topic
     :library-cards, :library-documents, :settings, :curriculum, :assistant,
     :knowledge-questions, :knowledge-facts

   The client builds the set of shown subjects (shown-undo-subjects) from the
   route and the open panels. The server maps each undo entry to the set of
   subjects that show it (entry-subjects). An entry is a match when the two sets
   share a member. This ns has no requires, so both peers and tests load it."
  (:require [clojure.string :as str]))

(defn- parse-id [s]
  #?(:clj (try (Long/parseLong s) (catch Exception _ nil))
     :cljs (let [n (js/parseInt s 10)] (when-not (js/isNaN n) n))))

(defn path-subjects
  "The subjects a route path shows.
   Pre: path is a URL path such as \"/viewer/topic/12/3\", or nil.
   Post: a set. A path that shows nothing undoable gives #{}."
  [path]
  (let [[a b c] (remove str/blank? (str/split (or path "") #"/"))]
    (case a
      "viewer"     (if (and (= b "topic") (parse-id (or c "")))
                     #{[:topic (parse-id c)]}
                     #{})
      "library"    (if (= b "cards") #{:library-cards} #{:library-documents})
      "settings"   #{:settings}
      "curriculum" #{:curriculum}
      "knowledge"  (case b
                     "questions" #{:knowledge-questions}
                     "facts"     #{:knowledge-facts}
                     #{})
      #{})))

(defn shown-subjects
  "Pure core of shown-undo-subjects.
   Pre: path is the route path, topic-ids are extra topics the viewer shows
   (a PDF root beside its page), assistant-open? is true when the assistant
   panel is mounted.
   Post: a set of subjects."
  [path topic-ids assistant-open?]
  (cond-> (path-subjects path)
    (seq topic-ids) (into (map (fn [id] [:topic id])) (remove nil? topic-ids))
    assistant-open? (conj :assistant)))

#?(:cljs
   (defn shown-undo-subjects
     "The subjects the current view shows, as a vector for the wire.
      `ctx` is the command-bus context. Reads the URL and the DOM, so it runs
      only on the client, at dispatch time."
     [ctx]
     (vec (shown-subjects (.. js/window -location -pathname)
            [(:export-topic-id ctx)]
            (some? (.querySelector js/document ".assistant-panel"))))))

(defn entry-subjects
  "The subjects that show undo entry `entry`.
   Pre: entry is a parsed undo_log row. `parent-of` maps a topic id to its
   parent id or nil. It is called only for the entry types that need it.
   Post: a set of subjects. An unknown action_type gives #{}, so no view
   shows it."
  [entry parent-of]
  (let [snap (:snapshot entry)
        refs (:entity_refs entry)
        topics (fn [ids] (into #{} (comp (remove nil?) (map (fn [id] [:topic id]))) ids))]
    (case (:action_type entry)
      ("delete-card" "bulk-delete-cards")
      (let [cards (if (map? snap) (:cards snap) snap)]
        (conj (topics (map #(or (:topic_id %) (:topic-id %)) cards)) :library-cards))
      "remove-pin"      (topics (map #(or (:topic_id %) (:topic-id %)) snap))
      "reset-prompt"    #{:settings}
      "delete-document" (into #{:library-documents} (topics (map parent-of refs)))
      "move-topic"      (into #{:library-documents}
                          (topics [(:old-parent-id snap)
                                   (parent-of (:topic-id snap))]))
      "split-article"   (topics [(:source-topic-id snap)])
      "delete-curriculum" #{:curriculum}
      "delete-assistant-chat" #{:assistant}
      ("reject-question" "bulk-reject-questions") #{:knowledge-questions}
      ("reject-fact" "bulk-reject-facts" "mark-duplicate-facts") #{:knowledge-facts}
      #{})))

(defn entry-subject
  "Alias of entry-subjects for the plan's name. One entry can be shown by
   several views, so the value is a set."
  [entry parent-of]
  (entry-subjects entry parent-of))

(defn shown?
  "True when entry's subjects meet the `shown` collection."
  [entry shown parent-of]
  (boolean (some (set shown) (entry-subjects entry parent-of))))
