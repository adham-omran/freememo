(ns freememo.article-split
  "Preview and save article sections.
   Contract: session user IDs authorize every operation.
   Preview never writes. Creation checks the locked source version.
   A transaction saves the entire batch, its references, and its undo entry."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [freememo.article-sections :as article-sections]
            [freememo.article-split-identity :as identity]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.html-cleaner :as cleaner]
            [freememo.input-check :as input]
            [freememo.logging :as log]
            [freememo.text :as text]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(def ^:private row-options {:builder-fn rs/as-unqualified-maps})
(def ^:private modes #{:headings :lines :splitmark :blocks})

(defn- normalized-options [options]
  (let [{:keys [mode splitmark]} options]
    (when-not (modes mode)
      (throw (ex-info "Choose a boundary mode." {:invalid? true})))
    (when (and (= :splitmark mode) (or (not (string? splitmark)) (str/blank? splitmark)))
      (throw (ex-info "Enter a splitmark." {:invalid? true})))
    (cond-> {:mode mode}
      (= :splitmark mode) (assoc :splitmark (str/trim splitmark)))))

(defn- source-topic [conn user-id topic-id lock?]
  (when (and (integer? user-id) (integer? topic-id))
    (jdbc/execute-one! conn
      [(str "WITH RECURSIVE ancestors AS (
               SELECT id, parent_id, user_id, staged_delete_id FROM topics WHERE id = ?
               UNION ALL
               SELECT t.id, t.parent_id, t.user_id, t.staged_delete_id
               FROM topics t JOIN ancestors a ON t.id = a.parent_id)
             SELECT t.* FROM topics t WHERE t.id = ?
               AND EXISTS (SELECT 1 FROM ancestors WHERE parent_id IS NULL AND user_id = ?)
               AND NOT EXISTS (SELECT 1 FROM ancestors WHERE staged_delete_id IS NOT NULL)"
         (when lock? " FOR UPDATE OF t"))
       topic-id topic-id user-id]
      row-options)))

(defn- proposed-sections [html options]
  (mapv #(update % :content cleaner/clean-html-fragment)
    (article-sections/sections (or html "") options)))

(defn preview
  "Pre: user-id comes from the session. Post: sanitized sections or a readable error.
   Invariant: preview leaves the database unchanged."
  [user-id topic-id options]
  (try
    (let [options (normalized-options options)]
      (if-let [source (source-topic db/ds user-id topic-id false)]
        {:success true :source-version (identity/source-version (:content source))
         :sections (proposed-sections (:content source) options)}
        {:success false :error "Article not found."}))
    (catch Exception e
      (if (:invalid? (ex-data e))
        {:success false :error (.getMessage e)}
        (do (log/log-error (str "Article preview failed: " (.getMessage e)))
            {:success false :error "Could not preview the article."})))))

(defn- effective-source-id [conn topic-id]
  (:source_id
   (jdbc/execute-one! conn
     ["WITH RECURSIVE ancestors AS (
          SELECT id, parent_id, source_id, 0 AS depth FROM topics WHERE id = ?
          UNION ALL
          SELECT t.id, t.parent_id, t.source_id, a.depth + 1
          FROM topics t JOIN ancestors a ON t.id = a.parent_id)
        SELECT source_id FROM ancestors WHERE source_id IS NOT NULL ORDER BY depth LIMIT 1"
      topic-id] row-options)))

(defn- clone-source! [conn user-id source-id]
  (when source-id
    (:id (jdbc/execute-one! conn
           ["INSERT INTO sources (user_id, csl_type, csl, url, title, container_title)
             SELECT ?, csl_type, csl, url, title, container_title FROM sources WHERE id = ?
             RETURNING id" user-id source-id] row-options))))

(defn- selected-parent-id [section sections selected topic-ids source-id]
  (loop [parent (:parent-index section)]
    (cond
      (nil? parent) source-id
      (contains? selected parent) (get topic-ids parent source-id)
      :else (recur (:parent-index (nth sections parent))))))

(defn- save-batch! [conn user-id topic-id {:keys [source-version options selected-indices]}]
  (if-let [source (source-topic conn user-id topic-id true)]
    (let [version (identity/source-version (:content source))]
      (if (not= source-version version)
        {:success false :stale? true :error "The article changed. Preview its sections again."}
        (let [sections (proposed-sections (:content source) options)
              selected (set selected-indices)
              valid-indices (set (map :index sections))]
          (when-not (and (sequential? selected-indices) (seq selected)
                     (every? integer? selected) (every? valid-indices selected))
            (throw (ex-info "Select at least one valid section." {:invalid? true})))
          (let [bibliography-id (effective-source-id conn topic-id)
                existing (into {} (map (juxt :article_split_key :id))
                           (jdbc/execute! conn
                             ["SELECT id, article_split_key FROM topics
                               WHERE article_split_source_id = ?" topic-id] row-options))
                batch (reduce
                        (fn [{:keys [topic-ids] :as batch} section]
                          (if-not (selected (:index section))
                            batch
                            (let [provenance (identity/provenance version options (:index section))
                                  key (identity/section-key provenance)]
                              (if-let [existing-id (get existing key)]
                                (-> batch (assoc-in [:topic-ids (:index section)] existing-id)
                                  (update :skipped inc))
                                (let [source-id (clone-source! conn user-id bibliography-id)
                                      parent-id (selected-parent-id section sections selected topic-ids topic-id)
                                      title (input/prettify-title (input/sanitize-title (:title section)))
                                      title (subs title 0 (min input/title-max (count title)))
                                      content (db/sanitize-utf8 (:content section))
                                      id (:id (jdbc/execute-one! conn
                                                ["INSERT INTO topics
                                                  (user_id, parent_id, kind, title, content, content_text,
                                                   is_extract, source_id, article_split_source_id, article_split_key,
                                                   article_split_provenance)
                                                  VALUES (?, ?, 'basic', ?, ?, ?, true, ?, ?, ?, ?) RETURNING id"
                                                 user-id parent-id title content (text/strip-html content)
                                                 source-id topic-id key (json/generate-string provenance)] row-options))]
                                  (jdbc/execute! conn
                                    ["INSERT INTO topic_pins (topic_id, media_id, placement, ord)
                                      SELECT ?, media_id, placement, ord FROM topic_pins WHERE topic_id = ?"
                                     id topic-id])
                                  (-> batch (assoc-in [:topic-ids (:index section)] id)
                                    (update :created conj id)
                                    (update :sources into (if source-id [source-id] []))))))))
                        {:topic-ids {} :created [] :sources [] :skipped 0} sections)
                created (:created batch)
                undo-id (when (seq created)
                          (db/insert-undo-entry-raw! conn user-id "split-article" "document" created
                            {:topic-ids created :source-ids (:sources batch) :source-topic-id topic-id}))]
            {:success true :created (count created) :skipped (:skipped batch) :undo-id undo-id}))))
    {:success false :error "Article not found."}))

(defn create-topics!
  "Pre: caller supplies a preview version and selected section indices.
   Post: every selected new section exists, or the transaction changes nothing.
   Invariants: source content and existing generated topics stay unchanged."
  [user-id topic-id request]
  (try
    (let [request (update request :options normalized-options)
          result (jdbc/with-transaction [tx db/ds]
                   (save-batch! tx user-id topic-id request))]
      (when (:undo-id result)
        (try
          (db/prune-undo-log! db/ds user-id)
          (catch Exception e
            (log/log-error (str "Article split undo cleanup failed: " (.getMessage e))))))
      (when (and (:success result) (pos? (:created result)))
        (commands/bump-channels! user-id #{:tree-mutations :undo-mutations :pin-mutations}))
      result)
    (catch Exception e
      (if (:invalid? (ex-data e))
        {:success false :error (.getMessage e)}
        (do (log/log-error (str "Article split failed: " (.getMessage e)))
            {:success false :error "Could not create the topics. No topics were saved."})))))

(defn- retain-descendant-cards!
  "Reassign cards on retained topics before their old document root is deleted.
   Pre: unrelated children have moved outside the batch.
   Post: those cards and their version history reference their surviving roots."
  [conn deleting-ids]
  (let [cards (jdbc/execute! conn
                ["WITH RECURSIVE ancestors AS (
                    SELECT c.id AS card_id, c.occlusion_group_id, c.score_group_id,
                           t.id AS topic_id, t.parent_id
                    FROM flashcards c JOIN topics t ON t.id = c.topic_id
                    WHERE c.root_topic_id = ANY(?) AND NOT (c.topic_id = ANY(?))
                    UNION ALL
                    SELECT a.card_id, a.occlusion_group_id, a.score_group_id, t.id, t.parent_id
                    FROM ancestors a JOIN topics t ON t.id = a.parent_id)
                  SELECT card_id, occlusion_group_id, score_group_id, topic_id AS root_id
                  FROM ancestors WHERE parent_id IS NULL"
                 deleting-ids deleting-ids] row-options)]
    (doseq [[root-id root-cards] (group-by :root_id cards)]
      (let [card-ids (long-array (map :card_id root-cards))
            occlusion-ids (long-array (keep :occlusion_group_id root-cards))
            score-ids (long-array (keep :score_group_id root-cards))]
        (jdbc/execute! conn
          ["UPDATE flashcards SET root_topic_id = ? WHERE id = ANY(?)" root-id card-ids])
        (jdbc/execute! conn
          ["UPDATE card_versions SET root_topic_id = ? WHERE root_topic_id = ANY(?)
            AND ((scope_type = 'card' AND scope_id = ANY(?))
              OR (scope_type = 'occlusion_group' AND scope_id = ANY(?))
              OR (scope_type = 'score_group' AND scope_id = ANY(?)))"
           root-id deleting-ids card-ids occlusion-ids score-ids])))))

(defn undo-batch!
  "Pre: entry is the user's split undo entry.
   Post: only its created topics and unused bibliography copies are removed.
   Invariant: unrelated child topics move to the nearest surviving ancestor."
  [user-id entry]
  (jdbc/with-transaction [tx db/ds]
    (let [locked-entry (jdbc/execute-one! tx
                         ["SELECT * FROM undo_log WHERE id = ? AND user_id = ? FOR UPDATE"
                          (:id entry) user-id] row-options)]
      (when (and locked-entry (nil? (:undone_at locked-entry)))
        (let [snapshot (json/parse-string (str (:snapshot locked-entry)) true)
              ids (long-array (:topic-ids snapshot))
              rows (jdbc/execute! tx
                     ["SELECT id, parent_id FROM topics WHERE id = ANY(?) AND user_id = ? FOR UPDATE"
                      ids user-id] row-options)
              parents (into {} (map (juxt :id :parent_id)) rows)
              deleting (set (keys parents))
              children (jdbc/execute! tx
                         ["SELECT id, parent_id FROM topics WHERE parent_id = ANY(?) FOR UPDATE" ids]
                         row-options)]
          (doseq [child children :when (not (deleting (:id child)))]
            (let [parent-id (loop [id (:parent_id child)]
                              (if (deleting id) (recur (get parents id)) id))]
              (jdbc/execute! tx
                ["UPDATE topics SET parent_id = ?, user_id = COALESCE(user_id, ?) WHERE id = ?"
                 parent-id user-id (:id child)])))
          (retain-descendant-cards! tx ids)
          (let [cards (jdbc/execute! tx
                        ["SELECT * FROM flashcards WHERE topic_id = ANY(?) OR root_topic_id = ANY(?)"
                         ids ids] row-options)]
            (db/record-anki-tombstones! tx user-id cards))
          (jdbc/execute! tx ["DELETE FROM topics WHERE id = ANY(?) AND user_id = ?" ids user-id])
          (jdbc/execute! tx
            ["DELETE FROM sources s WHERE s.id = ANY(?) AND s.user_id = ?
              AND NOT EXISTS (SELECT 1 FROM topics t WHERE t.source_id = s.id)"
             (long-array (:source-ids snapshot)) user-id])
          (jdbc/execute! tx ["UPDATE undo_log SET undone_at = now() WHERE id = ?" (:id entry)])))))
  nil)
