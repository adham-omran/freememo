(ns freememo.score
  "Score card kind — server-side group orchestration and the optimistic-queue
   command methods (:add-score-group / :update-score-group).

   Data shape: one score_groups row (audio segment + page-qualified rect
   geometry + clip media ref) fans out to one 'score' flashcards row per
   direction ('audio-front' / 'sheet-front'; 'Both' = two rows = two Anki
   notes sharing the same media). The clip MP3 and per-rect PNG crops are
   pre-materialized media rows, cut/cropped client-side at card creation —
   the Anki push only fetches stored bytes (headless Quick Sync safe)."
  (:require
   [freememo.db :as db]
   [freememo.optimistic :as opt]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]))

(def directions #{"audio-front" "sheet-front"})

(defn- validate-segment
  [start-ms end-ms]
  (when-not (and (int? start-ms) (int? end-ms) (>= start-ms 0) (> end-ms start-ms))
    (throw (ex-info "Malformed audio segment" {:start-ms start-ms :end-ms end-ms}))))

(defn- validate-geometry
  "Throw on malformed geometry; return it unchanged otherwise.
   Pre (caller): geometry = {:pages [{:page :width :height
   :rects [{:x :y :w :h :media-id (:ordinal)}]} ...]} in snapshot pixels,
   at least one page with at least one rect, every rect carrying its crop's
   media id."
  [{:keys [pages] :as geometry}]
  (when (empty? pages)
    (throw (ex-info "Score card needs at least one notation rectangle" {})))
  (doseq [{:keys [page width height rects]} pages]
    (when-not (and (int? page) (pos? page)
                (number? width) (pos? width) (number? height) (pos? height))
      (throw (ex-info "Malformed score page snapshot" {:geometry geometry})))
    (when (empty? rects)
      (throw (ex-info "Score page entry without rectangles" {:geometry geometry})))
    (doseq [{:keys [x y w h media-id]} rects]
      (when-not (and (number? x) (number? y) (number? w) (number? h)
                  (pos? w) (pos? h) (some? media-id))
        (throw (ex-info "Malformed score rectangle" {:geometry geometry})))))
  geometry)

(defn create-group!
  "Create a group + its per-direction cards.
   payload = {:topic-id :root-topic-id :start-ms :end-ms :clip-media-id
              :geometry :directions}
   Post: {:success true :group-id id :ids [card-id ...]} or {:success false :error}."
  [payload]
  (try
    (let [dirs (vec (:directions payload))]
      (when (or (empty? dirs) (not-every? directions dirs))
        (throw (ex-info "Invalid card direction" {:directions dirs})))
      (validate-segment (:start-ms payload) (:end-ms payload))
      (let [result (db/insert-score-group!
                     (-> payload
                       (select-keys [:topic-id :root-topic-id :start-ms :end-ms
                                     :clip-media-id])
                       (assoc :geometry (validate-geometry (:geometry payload))
                         :directions dirs)))]
        {:success true :group-id (:group-id result) :ids (:ids result)}))
    (catch Exception e
      (tel/error! {:id ::create-group!} e)
      {:success false :error (.getMessage e)})))

(defn update-group!
  "Replace a group's segment, clip, and geometry (see db/update-score-group!).
   payload = {:group-id :start-ms :end-ms :clip-media-id :geometry}
   Post: {:success true :group-id id} or {:success false :error}."
  [payload]
  (try
    (validate-segment (:start-ms payload) (:end-ms payload))
    (let [result (db/update-score-group!
                   (assoc (select-keys payload [:group-id :start-ms :end-ms
                                                :clip-media-id])
                     :geometry (validate-geometry (:geometry payload))))]
      (assoc result :success true))
    (catch Exception e
      (tel/error! {:id ::update-group! :data {:group-id (:group-id payload)}} e)
      {:success false :error (.getMessage e)})))

(defn get-group-for-edit
  "Everything the score editor needs to reopen a group.
   Post: {:success true :group {:group-id :start-ms :end-ms :clip-media-id
                                :geometry :directions}}."
  [group-id]
  (try
    (if-let [group (db/get-score-group group-id)]
      {:success true
       :group {:group-id group-id
               :start-ms (:score_groups/start_ms group)
               :end-ms (:score_groups/end_ms group)
               :clip-media-id (:score_groups/clip_media_id group)
               :geometry (:score_groups/geometry group)
               :directions (mapv :flashcards/score_direction
                             (db/get-score-cards group-id))}}
      {:success false :error "Score group not found"})
    (catch Exception e
      (tel/error! {:id ::get-group-for-edit :data {:group-id group-id}} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Optimistic-queue commands. Contract (see freememo.optimistic ns docstring):
;; methods own the effect + toast; optimistic/execute! bumps the registry
;; :views and removes the command from the queue.
;; ---------------------------------------------------------------------------

(defmethod opt/run-command! :add-score-group [user-id {:keys [id payload]}]
  (let [result (create-group! payload)]
    (if (:success result)
      (do (swap! (us/get-atom user-id :pending-cards) update id merge
            {:status :confirmed :real-ids (:ids result)})
        (toasts/push! user-id {:level :success
                               :message (let [n (count (:ids result))]
                                          (str n " score card" (when (not= 1 n) "s") " added"))}))
      (do (swap! (us/get-atom user-id :pending-cards) update id merge
            {:status :error :error (:error result)})
        (toasts/push! user-id {:level :error
                               :message (or (:error result) "Failed to add score cards")})))
    :done))

(defmethod opt/run-command! :update-score-group [user-id {:keys [payload]}]
  (let [result (update-group! payload)]
    (if (:success result)
      (toasts/push! user-id {:level :success :message "Score card updated"})
      (toasts/push! user-id {:level :error
                             :message (or (:error result) "Failed to update score card")}))
    :done))
