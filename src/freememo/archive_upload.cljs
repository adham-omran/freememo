(ns freememo.archive-upload
  "CLJS archive upload — the archive half of the chunked transport
   (plans/supermemo-import-large-archives.md §6.1).

   `/api/upload-file` reads its whole multipart body into a byte[] before
   classifying it, so it cannot carry a 7.5 GB SuperMemo collection at any
   configuration setting. This route chunks instead, exactly as video does, and
   shares the transport in `freememo.chunked-upload`.

   What differs from video is what the caller gets back. Video's finalize hands
   over a playable topic. An archive's finalize only says which import flow the
   archive holds — the import that follows is an Electric service call, because
   extracting and importing 7.5 GB runs for minutes and has no business on an
   HTTP request."
  (:require [freememo.chunked-upload :as cu]))

(defn archive-file?
  "Whether `file` should take the chunked route rather than /api/upload-file.

   Extension alone, deliberately: the client cannot read magic bytes without
   loading the file, and the server re-derives the real flow from the archive's
   entry names at finalize. A `.zip` that turns out to hold neither a collection
   nor a repo is rejected there, not here.

   Pre : `file` is a File. Post: true for .zip and .7z, false otherwise."
  [^js file]
  (let [n (some-> (.-name file) .toLowerCase)]
    (boolean (and n (or (.endsWith n ".zip") (.endsWith n ".7z"))))))

(defn upload-archive!
  "Upload one archive end to end and resolve with the flow the server found.

   `opts`: {:file :on-progress (fn [sent total]) :cancelled? (fn [])}

   Pre : `:file` is a `.zip` or `.7z` File — see `archive-file?`.
   Post: resolves with {:session-id S :flow \"supermemo\"|\"repo\" :filename S}.
         The session survives, holding the uploaded bytes, until an import
         confirms it or the reap sweep collects it.
   Post: rejects when the upload fails or the archive holds no importable flow,
         having released the session either way — so a rejected promise leaves
         no reservation behind."
  [{:keys [^js file on-progress cancelled?]
    :or {on-progress (fn [_ _]) cancelled? (fn [] false)}}]
  (-> (cu/upload!
        {:file file
         :base "/api/archive"
         :error-id :archive/upload
         :init-params {:filename (.-name file)}
         :finalize-params {}
         :on-progress on-progress
         :cancelled? cancelled?})
    (.then (fn [^js d]
             {:session-id (.-session_id d)
              :flow (.-flow d)
              :filename (.-filename d)}))))
