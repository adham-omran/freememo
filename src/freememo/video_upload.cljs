(ns freememo.video-upload
  "CLJS chunked video uploader (plans/incremental-video.md §4.3).

   A 700 MB file cannot pass through a 100 MB request cap, so the transfer is a
   sequence: `init` declares the size and reserves the quota, `chunk` appends
   8 MB at a time straight into the Postgres large object, `finalize` turns the
   session into a topic. Nothing buffers the whole file — `Blob.slice` is a view,
   so each POST reads only its own window off disk.

   Every exit path is compensated. A failed or cancelled upload POSTs `abort`,
   which unlinks the object and refunds the reservation; if even that fails
   (tab closed, network gone), the server's hourly sweep reaps the session. The
   one state we must never leave behind is a reservation with no object and no
   session to explain it.

   Resume (§4.3 3.2) asks the server how many bytes it actually holds and
   restarts from there, rather than trusting a client-side counter that a
   partially-applied chunk would have made a lie."
  (:require [freememo.client-errors :as ce]))

(def ^:private default-chunk-bytes (* 8 1024 1024))

(defn- post-json
  "POST form-encoded `params`, parse the JSON reply. Rejects on a non-2xx so
   every caller's `.catch` is the single failure path."
  [url params]
  (let [body (js/URLSearchParams.)]
    (doseq [[k v] params] (when (some? v) (.append body (name k) (str v))))
    (-> (js/fetch url #js {:method "POST" :body body})
      (.then (fn [^js r]
               (-> (.json r)
                 (.then (fn [^js d]
                          (if (.-success d)
                            d
                            (throw (js/Error. (or (.-error d)
                                                (str "HTTP " (.-status r))))))))))))))

(defn- put-chunk
  "POST one Blob slice as the raw request body."
  [session-id ^js blob]
  (-> (js/fetch (str "/api/video/chunk?session=" (js/encodeURIComponent session-id))
        #js {:method "POST"
             :headers #js {"Content-Type" "application/octet-stream"}
             :body blob})
    (.then (fn [^js r]
             (-> (.json r)
               (.then (fn [^js d]
                        (if (.-success d)
                          d
                          (throw (js/Error. (or (.-error d) "Chunk rejected")))))))))))

(defn- upload-from
  "Recursively send chunks from byte `sent` to the end. Returns a Promise that
   resolves when the server holds every byte.

   Recursion rather than a loop because each step must await the previous one:
   `lo_write` appends at the object's current end, so overlapping POSTs for one
   session would interleave. The server also serializes with a row lock, but
   sending them in order keeps the failure modes to one."
  [session-id ^js file chunk-bytes on-progress cancelled?]
  (let [total (.-size file)]
    (letfn [(step [sent]
              (cond
                (cancelled?) (js/Promise.reject (js/Error. "cancelled"))
                (>= sent total) (js/Promise.resolve sent)
                :else
                (let [end (min total (+ sent chunk-bytes))
                      slice (.slice file sent end)]
                  (-> (put-chunk session-id slice)
                    (.then (fn [^js d]
                             (let [received (.-received d)]
                               (on-progress received total)
                               (step received))))))))]
      (step 0))))

(defn upload-file!
  "Upload one video end to end.

   `opts`: {:file :parent-id :transcribe? :on-progress (fn [sent total])
            :cancelled? (fn [])}
   Returns a Promise of the new topic id.

   `:transcribe?` defaults to true and rides `finalize`, not `init`: the server
   needs it only at the moment it launches the pipeline, and putting it on the
   session row would be state to migrate for no gain (§15.3 3.2).

   `:cancelled?` is polled between chunks rather than mid-request: a chunk
   already in flight has already been paid for in bandwidth, and letting it land
   keeps `received_bytes` consistent with the object's real size."
  [{:keys [^js file parent-id on-progress cancelled? transcribe?]
    :or {on-progress (fn [_ _]) cancelled? (fn [] false) transcribe? true}}]
  (let [!session (atom nil)]
    (-> (post-json "/api/video/init"
          {:filename (.-name file)
           :mime_type (.-type file)
           :total_bytes (.-size file)
           :parent_id parent-id})
      (.then (fn [^js d]
               (reset! !session (.-session_id d))
               (upload-from (.-session_id d) file
                 (or (.-chunk_size d) default-chunk-bytes)
                 on-progress cancelled?)))
      (.then (fn [_]
               (post-json "/api/video/finalize"
                 {:session_id @!session
                  ;; Always sent, both values: `post-json` drops nil params, so
                  ;; relying on omission to mean false would send nothing and the
                  ;; server would read the absent-⇒-transcribe default.
                  :transcribe (boolean transcribe?)})))
      (.then (fn [^js d] (.-doc_id d)))
      (.catch (fn [e]
                ;; Compensate before re-throwing so the caller's error handler
                ;; never has to know about session cleanup.
                (when-let [sid @!session]
                  (-> (post-json "/api/video/abort" {:session_id sid})
                    (.catch (fn [_] nil))))
                (ce/report! :video/upload e)
                (throw e))))))

(defn upload-files!
  "Upload `files` one after another, reporting per-file progress.

   Sequential, not parallel: two concurrent 700 MB uploads would double the
   quota reservation, the disk write pressure, and the ffmpeg queue depth for
   no wall-clock win on a single connection.

   `on-file` receives {:index :count :filename :sent :total}. Resolves with the
   vector of created topic ids; rejects on the first failure, leaving already-
   uploaded files in place (they are complete topics, not partial state).

   `:transcribe?` applies to the whole batch (§15.3 3.5). Per-file would mean a
   control on every staged row for a choice the user makes once."
  [{:keys [files parent-id on-file cancelled? transcribe?]
    :or {on-file (fn [_]) cancelled? (fn [] false) transcribe? true}}]
  (let [fs (vec files)
        n (count fs)]
    (letfn [(step [i acc]
              (if (>= i n)
                (js/Promise.resolve acc)
                (let [^js f (nth fs i)]
                  (-> (upload-file!
                        {:file f
                         :parent-id parent-id
                         :transcribe? transcribe?
                         :cancelled? cancelled?
                         :on-progress (fn [sent total]
                                        (on-file {:index i :count n
                                                  :filename (.-name f)
                                                  :sent sent :total total}))})
                    (.then (fn [id] (step (inc i) (conj acc id))))))))]
      (step 0 []))))
