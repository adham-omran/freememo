(ns freememo.chunked-upload
  "CLJS chunked upload transport, shared by the video and archive flows
   (plans/incremental-video.md §4.3, plans/supermemo-import-large-archives.md
   §6.1).

   A file larger than the request cap cannot arrive as one POST, so the transfer
   is a sequence: `init` declares the size and reserves the quota, `chunk`
   appends 8 MB at a time straight into the Postgres large object, `finalize`
   turns the session into whatever that flow makes of it. Nothing buffers the
   whole file — `Blob.slice` is a view, so each POST reads only its own window
   off disk.

   `base` is the only difference between flows here: \"/api/video\" or
   \"/api/archive\". What `finalize` returns is the caller's to interpret, so
   this namespace does not look inside it.

   Every exit path is compensated. A failed or cancelled upload POSTs `abort`,
   which unlinks the object and refunds the reservation; if even that fails
   (tab closed, network gone), the server's hourly sweep reaps the session. The
   one state we must never leave behind is a reservation with no object and no
   session to explain it."
  (:require [freememo.client-errors :as ce]))

(def default-chunk-bytes (* 8 1024 1024))

(defn post-json
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
  [base session-id ^js blob]
  (-> (js/fetch (str base "/chunk?session=" (js/encodeURIComponent session-id))
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
  "Recursively send chunks from byte 0 to the end. Returns a Promise that
   resolves when the server holds every byte.

   Recursion rather than a loop because each step must await the previous one:
   `lo_write` appends at the object's current end, so overlapping POSTs for one
   session would interleave. The server also serializes with a row lock, but
   sending them in order keeps the failure modes to one.

   `:cancelled?` is polled between chunks rather than mid-request: a chunk
   already in flight has been paid for in bandwidth, and letting it land keeps
   `received_bytes` consistent with the object's real size."
  [base session-id ^js file chunk-bytes on-progress cancelled?]
  (let [total (.-size file)]
    (letfn [(step [sent]
              (cond
                (cancelled?) (js/Promise.reject (js/Error. "cancelled"))
                (>= sent total) (js/Promise.resolve sent)
                :else
                (let [end (min total (+ sent chunk-bytes))
                      slice (.slice file sent end)]
                  (-> (put-chunk base session-id slice)
                    (.then (fn [^js d]
                             (let [received (.-received d)]
                               (on-progress received total)
                               (step received))))))))]
      (step 0))))

(defn upload!
  "Run one file through init → chunk* → finalize against `base`.

   `opts`: {:file :base :error-id
            :init-params    map merged into the init POST (`total_bytes` is
                            always sent from the file itself)
            :finalize-params map merged into the finalize POST
            :on-progress    (fn [sent total])
            :cancelled?     (fn [])}

   Pre : `:file` is a Blob/File; `:base` is a route prefix whose server side
         implements init, chunk, finalize and abort.
   Post: resolves with finalize's parsed JSON reply. Rejects with the first
         failure, having POSTed `abort` for the session so no reservation and no
         large object survive.
   Invariant: heap on both sides stays at one chunk — the client slices, the
         server appends."
  [{:keys [^js file base error-id init-params finalize-params on-progress cancelled?]
    :or {on-progress (fn [_ _]) cancelled? (fn [] false)}}]
  (let [!session (atom nil)]
    (-> (post-json (str base "/init")
          (assoc init-params :total_bytes (.-size file)))
      (.then (fn [^js d]
               (reset! !session (.-session_id d))
               (upload-from base (.-session_id d) file
                 (or (.-chunk_size d) default-chunk-bytes)
                 on-progress cancelled?)))
      (.then (fn [_]
               (post-json (str base "/finalize")
                 (assoc finalize-params :session_id @!session))))
      (.catch (fn [e]
                ;; Compensate before re-throwing so the caller's error handler
                ;; never has to know about session cleanup.
                (when-let [sid @!session]
                  (-> (post-json (str base "/abort") {:session_id sid})
                    (.catch (fn [_] nil))))
                (ce/report! (or error-id :upload/chunked) e)
                (throw e))))))
