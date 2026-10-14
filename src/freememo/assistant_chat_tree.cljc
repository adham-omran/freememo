(ns freememo.assistant-chat-tree
  "The fork tree of assistant chats, as pure functions over chat maps
   (plans/assistant-chat-forks.md §4.2.1, §4.4.1, §4.6).

   A fork is a chat with a parent chat and a fork message. Every chat has at
   most one parent, so the chats of one document form a forest. A child
   inherits its parent's transcript up to the fork message, and a parent reads
   the part of each included descendant after its fork point.

   Message ids come from one global serial, and a chat's own messages are
   written after it exists, so every own message of a child has an id greater
   than its fork message. The path arithmetic below depends on that.

   The browser and the server both read this namespace, so it has no reader
   conditionals and no requires that one of them lacks.

   A chat map here carries :id, :parent-id and :fork-message-id. The branch and
   history functions also read :title, :excluded? and :label."
  (:require [clojure.string :as str]))

(defn- path-to
  "The (chat id, last message id) pairs that give `chat-id`'s transcript up to
   message `through`, root first. A chat contributes a pair only when it can
   hold an own message at or before `through`."
  [chats chat-id through]
  (let [{:keys [parent-id fork-message-id]} (get chats chat-id)
        earlier (if parent-id
                  (path-to chats parent-id (min through fork-message-id))
                  [])]
    (cond-> earlier
      (or (nil? parent-id) (> through fork-message-id))
      (conj [chat-id through]))))

(defn inherited-path
  "The transcript a chat inherits, as [chat-id last-message-id] pairs from the
   root to the fork point.

   Pre:  `chats` maps an id to a chat map for `chat-id` and every ancestor.
   Post: [] for a root chat. Otherwise the pairs, oldest first. The messages
         they name, read in order, ascend by id. A fork at a message the
         parent inherited names no pair for the parent."
  [chats chat-id]
  (let [{:keys [parent-id fork-message-id]} (get chats chat-id)]
    (if parent-id
      (path-to chats parent-id fork-message-id)
      [])))

(defn fork-order
  "Sort chats by fork message id, then by chat id. This is the order of
   branches in a request and of children in the history view (§4.4.2)."
  [chats]
  (sort-by (juxt :fork-message-id :id) chats))

(defn children-index
  "parent id → that parent's children in `fork-order`."
  [chats]
  (into {}
    (map (fn [[parent-id children]] [parent-id (vec (fork-order children))]))
    (group-by :parent-id (filter :parent-id chats))))

(defn included-branches
  "The descendants of `chat-id` whose transcripts its next answer reads.

   Pre:  `chats` holds `chat-id`'s live descendants, each with :title and
         :excluded?. Chats outside that subtree are ignored.
   Post: a vector in depth-first order, siblings in `fork-order`. Each entry is
         {:chat-id :fork-message-id :titles}, where :titles names the path from
         the direct child of `chat-id` down to the branch. An excluded chat and
         everything under it is absent, and so is every sibling or ancestor of
         `chat-id`."
  [chats chat-id]
  (let [children (children-index chats)
        walk (fn walk [parent-id titles]
               (into []
                 (comp (remove :excluded?)
                   (mapcat (fn [child]
                             (let [path (conj titles (:title child))]
                               (into [{:chat-id (:id child)
                                       :fork-message-id (:fork-message-id child)
                                       :titles path}]
                                 (walk (:id child) path))))))
                 (get children parent-id)))]
    (walk chat-id [])))

(defn lineage
  "The ancestors of `chat-id`, root first, without the chat itself.
   Pre:  `chats-by-id` maps an id to a chat map. An ancestor missing from it
         ends the walk, so a chat whose parent is not listed has none.
   Post: a vector of chat maps."
  [chats-by-id chat-id]
  (loop [ancestors () id (:parent-id (get chats-by-id chat-id))]
    (if-let [chat (and id (get chats-by-id id))]
      (recur (conj ancestors chat) (:parent-id chat))
      (vec ancestors))))

(defn with-descendants
  "Each chat of `roots` followed by its descendants, depth first and in
   `fork-order`, every row with :depth (0 for a root) and :guides
   (plans/assistant-history-fork-guides.md §3.1).

   :guides holds one entry per level, :depth entries in all, that say what the
   history row draws in that level's column. The last entry is the row's own
   level: :corner for the last child of its parent, :pass for any other child.
   Each earlier entry is an ancestor's level: :pass while that ancestor has a
   later sibling, so its line runs on past this row, and :blank when it has
   none.

   Pre:  `children` is a `children-index` over the chats that may be shown, so
         \"last child\" means last among the shown children."
  [roots children]
  (let [walk (fn walk [chat guides ancestor-guides]
               (let [kids (get children (:id chat))
                     last-index (dec (count kids))]
                 (into [(assoc chat :depth (count guides) :guides guides)]
                   (comp (map-indexed
                           (fn [i kid]
                             (let [last? (= i last-index)]
                               (walk kid
                                 (conj ancestor-guides (if last? :corner :pass))
                                 (conj ancestor-guides (if last? :blank :pass))))))
                     cat)
                   kids)))]
    (into [] (mapcat #(walk % [] [])) roots)))

(defn excerpt
  "`text` on one line, cut to `limit` characters with an ellipsis."
  [text limit]
  (let [line (str/trim (str/replace (str text) #"\s+" " "))]
    (if (> (count line) limit)
      (str (subs line 0 limit) "…")
      line)))
