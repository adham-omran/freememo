(ns freememo.media-migration
  "On-startup idempotent migration: scans all topic content HTML and re-hosts any
   <img> that isn't already /api/media/<id> via `image-rehost/rehost-html-images!`.

   Entry point: `migrate-images-to-media-table!` (not wired into startup)."
  (:require
   [freememo.db :as db]
   [freememo.image-rehost :as image-rehost]
   [taoensso.telemere :as tel]
   [next.jdbc :as jdbc]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql]))

;; ---------------------------------------------------------------------------
;; Per-topic rewrite
;; ---------------------------------------------------------------------------

(defn- rewrite-topic-images!
  "Re-host all non-media <img> srcs in one topic's content.
   Existing content already carries absolute srcs, so base-url is empty.
   Returns [n-uploaded n-not-uploaded] for that topic."
  [{:keys [topics/id topics/user_id topics/content]}]
  (when (and content (seq content))
    (let [{:keys [html uploaded hotlinked skipped]}
          (image-rehost/rehost-html-images!
            {:html content :base-url "" :user-id user_id})]
      (when (pos? uploaded)
        (jdbc/execute-one! db/ds
          (sql/format {:update :topics
                       :set {:content html}
                       :where [:= :id id]})))
      [uploaded (+ hotlinked skipped)])))

;; ---------------------------------------------------------------------------
;; Main entry point
;; ---------------------------------------------------------------------------

(defn migrate-images-to-media-table!
  "Scan every topic with non-null content. For each <img> that is NOT already
   /api/media/<id>: store bytes in `media` table and rewrite src.
   Idempotent — a second run is a no-op.
   Failures on individual images are warned and skipped (does not abort)."
  []
  (tel/log! :info "Starting image-to-media migration")
  (try
    ;; Fetch all topics that have content (no user filter — this is a global migration)
    (let [topics (jdbc/execute! db/ds
                   (sql/format {:select [:id :user_id :content]
                                :from   [:topics]
                                :where  [:and
                                         [:is-not :content nil]
                                         [:<> :content ""]]})
                   {:builder-fn rs/as-unqualified-lower-maps})
          topic-count (count topics)
          ;; Rough image count estimate by scanning for <img in content
          est-imgs (reduce (fn [acc {:keys [content]}]
                             (+ acc (count (re-seq #"<img" (or content "")))))
                           0 topics)]
      (tel/log! {:level :info :id ::migration-start
                 :data {:topic-count topic-count :est-image-count est-imgs}}
        (str "Migration: " topic-count " topics, ~" est-imgs " images to inspect"))

      (let [total-uploaded (atom 0)
            total-skipped  (atom 0)
            processed      (atom 0)]
        (doseq [topic topics]
          (try
            (when-let [[up sk] (rewrite-topic-images! topic)]
              (swap! total-uploaded + up)
              (swap! total-skipped  + sk)
              (let [done (swap! processed inc)]
                (when (zero? (mod done 25))
                  (tel/log! {:level :info :id ::migration-progress
                             :data {:processed done :uploaded @total-uploaded :skipped @total-skipped}}
                    (str "Migration progress: " done "/" topic-count " topics")))))
            (catch Exception e
              (tel/log! {:level :warn :id ::migration-topic-failed
                         :data {:topic-id (:id topic) :error (.getMessage e)}}
                "Migration: topic processing failed — skipping"))))

        (tel/log! {:level :info :id ::migration-complete
                   :data {:uploaded @total-uploaded :skipped @total-skipped :topics topic-count}}
          (str "Migration complete: " @total-uploaded " images uploaded, "
               @total-skipped " skipped"))))
    (catch Exception e
      (tel/log! {:level :error :id ::migration-failed :data {:error (.getMessage e)}}
        "Image migration failed"))))
