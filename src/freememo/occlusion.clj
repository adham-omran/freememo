(ns freememo.occlusion
  "Image-occlusion card kind — server-side group orchestration and the
   optimistic-queue command methods (:add-occlusion / :update-occlusion).

   Data shape: one occlusion_groups row (image + mode + geometry) fans out to
   one 'occlusion' flashcards row per mask. Geometry writes go through the
   transactional, ordinal-safe fns in freememo.db; this ns adds validation,
   sanitization, toasts, and the :pending-cards overlay bookkeeping.

   Mask SVGs are NOT stored — they are generated client-side at push time
   from the geometry here (freememo.occlusion-svg / freememo.occlusion-anki)."
  (:require
   [freememo.db :as db]
   [freememo.html-cleaner :as cleaner]
   [freememo.input-check :as input]
   [freememo.optimistic :as opt]
   [freememo.toasts :as toasts]
   [freememo.user-state :as us]
   [taoensso.telemere :as tel]))

(def io-field-keys
  "The six user-editable IO FreeMemo text fields, as io_fields JSONB keys.
   Order matches the Anki model's field order (Header, Footer, Remarks,
   Sources, Extra 1, Extra 2)."
  [:header :footer :remarks :sources :extra1 :extra2])

(defn- sanitize-io-fields
  "clean-html every text field; drop unknown keys; length-capped like other
   card fields. Sanitization happens here — the single server entry point —
   rather than per-field on the client."
  [io-fields]
  (into {}
    (map (fn [k]
           (let [v (get io-fields k)]
             (input/check-length! k v input/card-max)
             [k (or (some-> v cleaner/clean-html) "")])))
    io-field-keys))

(defn- validate-geometry
  "Throw on malformed geometry; return it unchanged otherwise.
   Pre (caller): geometry = {:width :height :rects [{:x :y :w :h (:ordinal)}]}
   in natural-image pixels, at least one rect."
  [{:keys [width height rects] :as geometry}]
  (when-not (and (number? width) (pos? width) (number? height) (pos? height))
    (throw (ex-info "Occlusion geometry is missing image dimensions" {:geometry geometry})))
  (when (empty? rects)
    (throw (ex-info "Occlusion needs at least one mask" {})))
  (doseq [{:keys [x y w h]} rects]
    (when-not (and (number? x) (number? y) (number? w) (number? h) (pos? w) (pos? h))
      (throw (ex-info "Malformed occlusion mask rectangle" {:geometry geometry}))))
  geometry)

(defn create-group!
  "Create a group + its per-mask cards.
   payload = {:topic-id :root-topic-id :image-media-id :mode :geometry :io-fields}
   Post: {:success true :group-id id :ids [card-id ...]} or {:success false :error}."
  [payload]
  (try
    (let [result (db/insert-occlusion-group!
                   (assoc payload
                     :geometry (validate-geometry (:geometry payload))
                     :io-fields (sanitize-io-fields (:io-fields payload))))]
      {:success true :group-id (:group-id result) :ids (:ids result)})
    (catch Exception e
      (tel/error! {:id ::create-group!} e)
      {:success false :error (.getMessage e)})))

(defn update-group!
  "Full reconcile of a group edit (see db/reconcile-occlusion-group!).
   payload = {:group-id :mode :geometry :io-fields}
   Post: {:success true :added-ids [..] :removed [{:id :anki-note-id} ..]}."
  [payload]
  (try
    (let [result (db/reconcile-occlusion-group!
                   {:group-id (:group-id payload)
                    :mode (:mode payload)
                    :geometry (validate-geometry (:geometry payload))
                    :io-fields (sanitize-io-fields (:io-fields payload))})]
      (assoc result :success true))
    (catch Exception e
      (tel/error! {:id ::update-group! :data {:group-id (:group-id payload)}} e)
      {:success false :error (.getMessage e)})))

(defn get-group-for-edit
  "Everything the occlusion modal needs to reopen a group.
   Post: {:success true :group {:group-id :image-media-id :mode :geometry
                                :io-fields :note-ids-by-ordinal}}
   io-fields come from the lowest-ordinal row — the modal edits fields
   group-wide, so per-row divergence (Anki-side pulls) is squashed on the
   next group save."
  [group-id]
  (try
    (if-let [group (db/get-occlusion-group group-id)]
      (let [cards (db/get-occlusion-cards group-id)]
        {:success true
         :group {:group-id group-id
                 :image-media-id (:occlusion_groups/image_media_id group)
                 :mode (:occlusion_groups/mode group)
                 :geometry (:occlusion_groups/geometry group)
                 :io-fields (or (:flashcards/io_fields (first cards)) {})
                 :note-ids-by-ordinal
                 (into {}
                   (keep (fn [c]
                           (when-let [nid (:flashcards/anki_note_id c)]
                             [(:flashcards/mask_ordinal c) nid])))
                   cards)}})
      {:success false :error "Occlusion group not found"})
    (catch Exception e
      (tel/error! {:id ::get-group-for-edit :data {:group-id group-id}} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Optimistic-queue commands. Contract (see freememo.optimistic ns docstring):
;; methods own the effect + toast; optimistic/execute! bumps the registry
;; :views and removes the command from the queue.
;; ---------------------------------------------------------------------------

(defmethod opt/run-command! :add-occlusion [user-id {:keys [id payload]}]
  (let [result (create-group! payload)]
    (if (:success result)
      (do (swap! (us/get-atom user-id :pending-cards) update id merge
            {:status :confirmed :real-ids (:ids result)})
        (toasts/push! user-id {:level :success
                               :message (let [n (count (:ids result))]
                                          (str n " occlusion card" (when (not= 1 n) "s") " added"))}))
      (do (swap! (us/get-atom user-id :pending-cards) update id merge
            {:status :error :error (:error result)})
        (toasts/push! user-id {:level :error
                               :message (or (:error result) "Failed to add occlusion cards")})))
    :done))

(defmethod opt/run-command! :update-occlusion [user-id {:keys [payload]}]
  (let [result (update-group! payload)]
    (if (:success result)
      (toasts/push! user-id {:level :success :message "Occlusion updated"})
      (toasts/push! user-id {:level :error
                             :message (or (:error result) "Failed to update occlusion")}))
    :done))
