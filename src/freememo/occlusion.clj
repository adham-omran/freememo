(ns freememo.occlusion
  "Image-occlusion card kind — server-side group orchestration and the
   optimistic-queue command methods (:add-occlusion / :update-occlusion).

   Data shape: one occlusion_groups row (image + mode + geometry) fans out to
   one 'occlusion' flashcards row per MASK GROUP — rects sharing a mask ordinal
   are one card (freememo.occlusion-ordinals). Geometry writes go through the
   transactional, ordinal-safe fns in freememo.db; this ns adds validation,
   sanitization, toasts, and the :pending-cards overlay bookkeeping.

   Mask SVGs are NOT stored — they are generated client-side at push time
   from the geometry here (freememo.occlusion-svg / freememo.occlusion-anki)."
  (:require
   [clojure.string :as str]
   [freememo.db :as db]
   [freememo.html-cleaner :as cleaner]
   [freememo.input-check :as input]
   [freememo.optimistic :as opt]
   [freememo.toasts :as toasts]
   [taoensso.telemere :as tel]))

(def io-field-keys
  "The six user-editable FreeMemo IO text fields, as io_fields JSONB keys.
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

(defn- checked-geometry
  "Throw on malformed geometry; otherwise return it with mask ordinals coerced
   to long — the one place incoming geometry is normalized, so no reader below
   has to.
   Pre (caller): geometry = {:width :height :rects [{:x :y :w :h} ...]} in
   natural-image pixels, at least one rect, each rect carrying at most one
   membership key (:ordinal for an existing mask group, :gid for a new one —
   see freememo.occlusion-ordinals).
   Post: same shape; every present :ordinal is a positive long."
  [{:keys [width height rects] :as geometry}]
  (when-not (and (number? width) (pos? width) (number? height) (pos? height))
    (throw (ex-info "Occlusion geometry is missing image dimensions" {:geometry geometry})))
  (when (empty? rects)
    (throw (ex-info "Occlusion needs at least one mask" {})))
  (doseq [{:keys [x y w h ordinal gid] :as rect} rects]
    (when-not (and (number? x) (number? y) (number? w) (number? h) (pos? w) (pos? h))
      (throw (ex-info "Malformed occlusion mask rectangle" {:geometry geometry})))
    (when (and (some? ordinal) (some? gid))
      (throw (ex-info "Occlusion mask carries both a mask ordinal and a group tag"
               {:rect rect})))
    (when (and (some? ordinal)
            (not (and (number? ordinal) (pos? ordinal) (== ordinal (long ordinal)))))
      (throw (ex-info "Malformed occlusion mask ordinal" {:rect rect})))
    (when (and (some? gid) (or (not (string? gid)) (str/blank? gid)))
      (throw (ex-info "Malformed occlusion mask group tag" {:rect rect}))))
  (update geometry :rects
    (fn [rects]
      (mapv (fn [rect]
              (cond-> rect
                (some? (:ordinal rect)) (update :ordinal long)))
        rects))))

(defn create-group!
  "Create a group + its per-mask cards.
   payload = {:topic-id :root-topic-id :image-media-id :mode :geometry :io-fields}
   Post: {:success true :group-id id :ids [card-id ...]} or {:success false :error}."
  [user-id payload]
  (try
    (when-not (db/owns-topic? user-id (:root-topic-id payload))
      (throw (ex-info "Not authorized for this topic" {:type ::not-owner})))
    (let [result (db/insert-occlusion-group!
                   (assoc payload
                     :geometry (checked-geometry (:geometry payload))
                     :io-fields (sanitize-io-fields (:io-fields payload))))]
      {:success true :group-id (:group-id result) :ids (:ids result)})
    (catch Exception e
      (tel/error! {:id ::create-group!} e)
      {:success false :error (.getMessage e)})))

(defn update-group!
  "Full reconcile of a group edit (see db/reconcile-occlusion-group!).
   payload = {:group-id :mode :geometry :io-fields}
   Post: {:success true :added-ids [..] :removed [{:id :anki-note-id} ..]}."
  [user-id payload]
  (try
    (when-not (= user-id (db/occlusion-group-owner (:group-id payload)))
      (throw (ex-info "Not authorized for this group" {:type ::not-owner})))
    (let [result (db/reconcile-occlusion-group!
                   {:group-id (:group-id payload)
                    :mode (:mode payload)
                    :geometry (checked-geometry (:geometry payload))
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
  [user-id group-id]
  (try
    (if (not= user-id (db/occlusion-group-owner group-id))
      {:success false :error "Occlusion group not found"}
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
      {:success false :error "Occlusion group not found"}))
    (catch Exception e
      (tel/error! {:id ::get-group-for-edit :data {:group-id group-id}} e)
      {:success false :error (.getMessage e)})))

;; ---------------------------------------------------------------------------
;; Optimistic-queue commands. Contract (see freememo.optimistic ns docstring):
;; methods own the effect + toast; optimistic/execute! bumps the registry
;; :views and removes the command from the queue.
;; ---------------------------------------------------------------------------

(defmethod opt/run-command! :add-occlusion [user-id {:keys [payload]}]
  (let [result (create-group! user-id payload)]
    (if (:success result)
      (do (toasts/push! user-id {:level :success
                                 :message (let [n (count (:ids result))]
                                            (str n " occlusion card" (when (not= 1 n) "s") " added"))})
          {:ok? true :real-ids (:ids result)})
      (do (toasts/push! user-id {:level :error
                                 :message (or (:error result) "Failed to add occlusion cards")})
          {:ok? false :error (:error result)}))))

(defmethod opt/run-command! :update-occlusion [user-id {:keys [payload]}]
  (let [result (update-group! user-id payload)]
    (if (:success result)
      (do (toasts/push! user-id {:level :success :message "Occlusion updated"})
          {:ok? true})
      (do (toasts/push! user-id {:level :error
                                 :message (or (:error result) "Failed to update occlusion")})
          {:ok? false :error (:error result)}))))
