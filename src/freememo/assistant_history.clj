(ns freememo.assistant-history
  "Bounded reads for the assistant transcript, history picker, and mentions."
  (:require [clojure.string :as str]
            [freememo.db :as db]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def page-size 50)
(def ^:private read-options {:builder-fn rs/as-unqualified-maps})

(defn- literal-pattern [query]
  (str "%" (str/replace (str/lower-case (str/trim (or query "")))
             #"[\\%_]" #(str "\\" %)) "%"))

(defn document-options
  "Return at most 50 owned document titles matching literal query text."
  [user-id current-root-id query]
  (jdbc/execute! db/ds
    ["SELECT id, coalesce(title, 'Untitled') AS title FROM topics
       WHERE user_id = ? AND parent_id IS NULL AND staged_delete_id IS NULL
         AND id <> ? AND kind NOT IN ('repo', 'score')
         AND lower(coalesce(title, 'Untitled')) LIKE ?
       ORDER BY created_at DESC, id DESC LIMIT ?"
     user-id current-root-id (literal-pattern query) page-size]
    read-options))

(defn chat-option [row]
  (when row
    {:id (:id row) :label (or (:title row) "Untitled")
     :mode (:mode row) :web-search? (:web_search row)
     :activity-ms (:activity_ms row) :pinned-ms (:pinned_ms row)
     :parent-id (:parent_chat_id row) :fork-message-id (:fork_message_id row)
     :excluded? (boolean (:excluded row))}))

(def ^:private chat-columns
  "id, title, mode, web_search, parent_chat_id, fork_message_id,
   excluded_at IS NOT NULL AS excluded,
   (extract(epoch FROM (coalesce(updated_at, created_at) AT TIME ZONE current_setting('TimeZone'))) * 1000)::bigint AS activity_ms,
   (extract(epoch FROM (pinned_at AT TIME ZONE current_setting('TimeZone'))) * 1000)::bigint AS pinned_ms")

(defn active-chat
  "Read active metadata independently from any history result page."
  [user-id root-topic-id chat-id]
  (when chat-id
    (chat-option
      (jdbc/execute-one! db/ds
        [(str "SELECT " chat-columns " FROM assistant_chats
                WHERE user_id = ? AND root_topic_id = ? AND id = ?
                  AND staged_delete_id IS NULL")
         user-id root-topic-id chat-id] read-options))))

(defn chat-page
  "Read one bounded search page. Extra metadata establishes the next-page flag."
  [user-id root-topic-id query offset]
  (let [rows (jdbc/execute! db/ds
               [(str "SELECT " chat-columns " FROM assistant_chats
                       WHERE user_id = ? AND root_topic_id = ? AND staged_delete_id IS NULL
                         AND lower(coalesce(title, 'Untitled')) LIKE ?
                       ORDER BY pinned_at DESC NULLS LAST, coalesce(updated_at, created_at) DESC, id DESC
                       LIMIT ? OFFSET ?")
                user-id root-topic-id (literal-pattern query) (inc page-size) (max 0 (long offset))]
               read-options)]
    {:query query :offset offset
     :rows (mapv chat-option (take page-size rows)) :more? (> (count rows) page-size)}))

(defn branch-options
  "Read immediate branch navigation with a fixed bound. History search reaches all other chats."
  [user-id root-topic-id chat-id]
  (if-not chat-id
    []
    (mapv chat-option
      (jdbc/execute! db/ds
        [(str "SELECT " chat-columns " FROM assistant_chats
                WHERE user_id = ? AND root_topic_id = ? AND staged_delete_id IS NULL
                  AND (id = ? OR id = (SELECT parent_chat_id FROM assistant_chats WHERE id = ? AND user_id = ?)
                       OR parent_chat_id = ?)
                ORDER BY (id = ?) DESC,
                         (id = (SELECT parent_chat_id FROM assistant_chats WHERE id = ? AND user_id = ?)) DESC,
                         id DESC LIMIT ?")
         user-id root-topic-id chat-id chat-id user-id chat-id chat-id chat-id user-id page-size]
        read-options))))

(def ^:private timeline-sql
  "WITH RECURSIVE line AS (
     SELECT id, parent_chat_id, fork_message_id, title, user_id,
            9223372036854775807::bigint AS through_id, true AS own
       FROM assistant_chats WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL
     UNION ALL
     SELECT c.id, c.parent_chat_id, c.fork_message_id, c.title, c.user_id,
            least(line.through_id, line.fork_message_id), false
       FROM assistant_chats c JOIN line ON c.id = line.parent_chat_id
      WHERE c.user_id = line.user_id),
   events AS (
     SELECT CASE WHEN l.own THEN 2 ELSE 0 END AS section, m.id * 2 AS position,
            0 AS kind, m.id AS event_id, m.id AS message_id, NULL::bigint AS checkpoint_id,
            NOT l.own AS inherited, NULL::text AS chat_title
       FROM assistant_messages m JOIN line l ON l.id = m.chat_id
      WHERE m.id <= l.through_id
     UNION ALL
     SELECT CASE WHEN cp.scope = 'inherited' THEN 0 ELSE 2 END,
            coalesce(cp.covered_through_message_id, 0) * 2 + 1, 1, cp.id,
            NULL::bigint, cp.id, false, NULL::text
       FROM assistant_checkpoints cp JOIN line l ON l.id = cp.chat_id AND l.own
     UNION ALL
     SELECT 2, coalesce((SELECT max(m.id) FROM assistant_messages m
                        WHERE m.chat_id = cp.for_chat_id AND m.created_at <= cp.created_at), 0) * 2 + 1,
            1, cp.id, NULL::bigint, cp.id, false, src.title
       FROM assistant_checkpoints cp JOIN line l ON l.id = cp.for_chat_id AND l.own
       JOIN assistant_chats src ON src.id = cp.chat_id AND src.user_id = l.user_id
      WHERE cp.chat_id <> cp.for_chat_id
     UNION ALL
     SELECT 1, 0, 2, l.id, NULL::bigint, NULL::bigint, false, p.title
       FROM line l JOIN assistant_chats p ON p.id = l.parent_chat_id AND p.user_id = l.user_id
      WHERE l.own)
   SELECT * FROM events
    WHERE (?::bigint IS NULL OR (section, position, kind, event_id) < (?, ?, ?, ?))
    ORDER BY section DESC, position DESC, kind DESC, event_id DESC LIMIT ?")

(defn transcript-page
  "Read at most 50 events. Cursor includes all tie breakers, including checkpoint identity."
  [user-id chat-id before]
  (let [[section position kind id] before
        candidates (if chat-id
                     (jdbc/execute! db/ds
                       [timeline-sql chat-id user-id section section position kind id (inc page-size)]
                       read-options)
                     [])
        events (vec (reverse (take page-size candidates)))
        cursor (some-> events first ((juxt :section :position :kind :event_id)))]
    {:events events :more? (> (count candidates) page-size) :cursor cursor}))

(defn message-rows
  "Read bodies only for the requested owned message IDs."
  [user-id ids]
  (if (seq ids)
    (jdbc/execute! db/ds
      ["SELECT m.id, m.role, m.content, m.client_id,
               r.run_uuid, r.status, r.error_text, r.retry_of_run_uuid,
               r.response_text
         FROM assistant_messages m JOIN assistant_chats c ON c.id = m.chat_id
         LEFT JOIN assistant_runs r ON r.assistant_message_id = m.id
        WHERE c.user_id = ? AND m.id = ANY(?)"
       user-id (long-array ids)] read-options)
    []))

(defn cited-evidence
  "Read cited evidence for a bounded set of answer messages."
  [user-id ids]
  (if (seq ids)
    (jdbc/execute! db/ds
      ["SELECT e.*, r.run_uuid FROM assistant_evidence e
         JOIN assistant_runs r ON r.id = e.run_id
         JOIN assistant_chats c ON c.id = r.chat_id
        WHERE c.user_id = ? AND r.assistant_message_id = ANY(?)
          AND strpos(r.response_text, e.citation_key) > 0
          AND r.response_text ~ (? || e.citation_key || ?)
        ORDER BY r.id, e.id"
       user-id (long-array ids)
       "\\[evidence:[^]]*(?<![A-Za-z0-9_-])" "(?![A-Za-z0-9_-])[^]]*\\]"]
      read-options)
    []))

(defn checkpoints
  "Read checkpoint bodies only for the bounded event page."
  [user-id ids]
  (if (seq ids)
    (jdbc/execute! db/ds
      ["SELECT cp.*, r.run_uuid FROM assistant_checkpoints cp
         JOIN assistant_runs r ON r.id = cp.run_id
         JOIN assistant_chats c ON c.id = cp.chat_id
        WHERE c.user_id = ? AND cp.id = ANY(?)"
       user-id (long-array ids)] read-options)
    []))

(defn active-message-id
  "Read only the active answer ID on a stream publication."
  [user-id chat-id]
  (:assistant_message_id
    (jdbc/execute-one! db/ds
      ["SELECT r.assistant_message_id FROM assistant_runs r
         JOIN assistant_chats c ON c.id = r.chat_id
        WHERE c.user_id = ? AND c.id = ? AND c.staged_delete_id IS NULL
          AND r.status IN ('queued', 'streaming', 'stop-requested')
        ORDER BY r.id DESC LIMIT 1"
       user-id chat-id] read-options)))
