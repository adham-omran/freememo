(ns freememo.neoreader-picker
  "Browser half of the NeoReader sync: pick a Drive file, then ask the server to
   sync against it.

   Google's Picker is the THIRD third-party script this app loads, and the only
   one outside `deployment-hook-impl`. It cannot be vendored: the Picker is a
   Google-hosted widget that talks to Google, exactly like `gtag.js` and
   `adsbygoogle.js`. Two things keep it inside the spirit of the no-CDN rule —
   it carries NO `<script>` tag in either HTML shell, and it loads only when a
   user clicks Sync. A page nobody syncs from makes no request to Google, and a
   self-hosted deployment that never uses this feature never contacts Google at
   all.

   The Picker needs an OAuth token, and the SERVER vends it from the stored
   refresh token. The browser therefore runs no OAuth flow of its own, so there
   is one grant, one consent screen, and one credential behind both the Picker
   and the server-side download.

   A folder pick would grant nothing about the folder's contents under
   `drive.file`, so this picks a FILE. The user navigates to
   `onyx/<DEVICE>/Read/` themselves, and the view is filtered to HTML so the
   folder shows only exports."
  (:require [clojure.string :as str]))

(def ^:private picker-api-src "https://apis.google.com/js/api.js")

(defonce ^:private !loaded (atom nil))

(defn- load-picker-api!
  "Load Google's Picker API once.

   Pre : none.
   Post: a promise resolving when `google.picker` is defined, rejecting on a
         load or timeout failure. One shared promise per page — repeated clicks
         reuse it rather than injecting a second script."
  []
  (or @!loaded
    (reset! !loaded
      (js/Promise.
        (fn [resolve reject]
          (let [done (fn []
                       (if (and (exists? js/gapi) (.-load js/gapi))
                         (.load js/gapi "picker"
                           #js {:callback (fn []
                                            (if (and (exists? js/google) (.-picker js/google))
                                              (resolve true)
                                              (reject (js/Error. "Picker API loaded but google.picker is missing"))))
                                :onerror (fn [] (reject (js/Error. "Could not load the Google Picker")))})
                         (reject (js/Error. "Google API script loaded but gapi is missing"))))]
            (if (and (exists? js/gapi) (.-load js/gapi))
              (done)
              (let [el (.createElement js/document "script")]
                (set! (.-src el) picker-api-src)
                (set! (.-async el) true)
                (set! (.-onload el) done)
                (set! (.-onerror el) (fn [] (reject (js/Error. "Could not reach Google to load the Picker"))))
                (.appendChild (.-head js/document) el)))))))))

(defn- form-body
  "Pre : `m` is a map of string keys to values.
   Post: a URL-encoded body string, matching how every other handler in
         `freememo.api` reads its params."
  [m]
  (str/join "&" (map (fn [[k v]] (str (js/encodeURIComponent k) "=" (js/encodeURIComponent (str v)))) m)))

(defn- post-json
  "Pre : `path` is an app route; `params` a map of form params.
   Post: a promise of the parsed JSON body as a CLJS map with keyword keys."
  [path params]
  (-> (js/fetch path
        #js {:method "POST"
             :credentials "same-origin"
             :headers #js {"Content-Type" "application/x-www-form-urlencoded"}
             :body (form-body params)})
    (.then (fn [r] (.json r)))
    (.then (fn [j] (js->clj j :keywordize-keys true)))))

(defn- picker-token
  "Pre : the user is signed in.
   Post: a promise of {:success bool :token s :client_id s :reauthorize bool}."
  []
  (-> (js/fetch "/api/neoreader/picker-token" #js {:credentials "same-origin"})
    (.then (fn [r] (.json r)))
    (.then (fn [j] (js->clj j :keywordize-keys true)))))

(defn- start-drive-authorization!
  "Send the browser through the Drive consent flow.

   Pre : `return-to` is the path to come back to.
   Post: navigates away. Nothing after this call runs."
  [return-to]
  (set! (.-href js/location)
    (str "/auth/google/drive?return_to=" (js/encodeURIComponent return-to))))

(defn- open-picker!
  "Show the Picker and resolve with the chosen file id, or nil if cancelled.

   Pre : the Picker API is loaded, `token` is a live `drive.file` token, and
         `app-id` is the Cloud PROJECT NUMBER.
   Post: a promise of a file-id string, or nil when the user cancelled. The
         picked file is granted to this app, which is what makes the later
         server-side read possible.
   Inv : `setAppId` is REQUIRED under `drive.file`. Without it the pick succeeds,
         the file id looks fine, and every server read of it answers 404 — the
         error names the file and never the missing app id. Do not remove it.
   Note: no `setSelectFolderEnabled` — a folder pick grants nothing about its
         contents, so offering it would hand the user a choice that cannot work."
  [token app-id]
  (js/Promise.
    (fn [resolve _reject]
      (let [picker-ns (.-picker js/google)
            View (.-DocsView picker-ns)
            view (doto (new View (.. picker-ns -ViewId -DOCS))
                   (.setMimeTypes "text/html")
                   (.setIncludeFolders true)
                   (.setSelectFolderEnabled false))
            Builder (.-PickerBuilder picker-ns)
            Action (.-Action picker-ns)
            builder (doto (new Builder)
                      (.setTitle "Choose your NeoReader export (onyx / DEVICE / Read)")
                      (.setAppId app-id)
                      (.setOAuthToken token)
                      (.addView view)
                      (.setCallback
                        (fn [data]
                          (let [action (aget data (.. picker-ns -Response -ACTION))]
                            (cond
                              (= action (.-PICKED Action))
                              (let [docs (aget data (.. picker-ns -Response -DOCUMENTS))
                                    doc (aget docs 0)]
                                (resolve (aget doc (.. picker-ns -Document -ID))))

                              (= action (.-CANCEL Action))
                              (resolve nil)

                              :else nil)))))]
        (.setVisible (.build builder) true)))))

(defn- pick-then-sync!
  "Open the Picker, bind what the user chose, and sync.

   Pre : `token` is a live `drive.file` token and `app-id` is the project number.
   Post: a promise of the sync response map, or nil when the user cancelled or
         the bind failed. A cancelled Picker binds nothing and syncs nothing."
  [topic-id token app-id on-status]
  (on-status {:phase :picking})
  (-> (load-picker-api!)
    (.then (fn [_] (open-picker! token app-id)))
    (.then (fn [file-id]
             (if (nil? file-id)
               (do (on-status {:phase :cancelled}) nil)
               (-> (post-json "/api/neoreader/bind" {"topic_id" topic-id "file_id" file-id})
                 (.then (fn [{:keys [success error]}]
                          (if success
                            (do (on-status {:phase :syncing})
                                (post-json "/api/neoreader/sync" {"topic_id" topic-id}))
                            (do (on-status {:phase :error
                                            :message (or error "Could not link that file")})
                                nil))))))))))

(defn sync-document!
  "Run one NeoReader sync for `topic-id`, picking a file first if none is bound.

   Pre : the user is signed in and `topic-id` is an EPUB document they own.
         `return-to` is the path to come back to after Drive consent.
         `on-status` takes one map and is called at every step:
           {:phase :syncing|:picking|:authorizing|:cancelled|:done|:error …}
         An :error map carries :message.
   Post: a promise that always resolves — every failure arrives through
         `on-status`, so a caller needs no catch.
   Inv : the server decides whether a file is bound, so the browser tracks no
         binding state and one click covers both the first sync and every later
         one.
   Inv : the Drive consent flow NAVIGATES AWAY. After :authorizing nothing else
         in this call runs.
   Note: a :done status needs no message — the server pushes the toast, beside
         the counts that produced it. `on-status` exists for the phases the
         server never sees: a cancelled Picker, and a Picker that would not load."
  [topic-id return-to on-status]
  (on-status {:phase :syncing})
  (-> (post-json "/api/neoreader/sync" {"topic_id" topic-id})
    (.then (fn [{:keys [success needs_pick reauthorize error] :as result}]
             (cond
               success (do (on-status {:phase :done}) nil)

               reauthorize (do (on-status {:phase :authorizing})
                               (start-drive-authorization! return-to)
                               nil)

               needs_pick
               (-> (picker-token)
                 (.then (fn [{:keys [success token app_id reauthorize error]}]
                          (cond
                            reauthorize (do (on-status {:phase :authorizing})
                                            (start-drive-authorization! return-to)
                                            nil)
                            (not success) (do (on-status {:phase :error
                                                          :message (or error "Could not reach Google Drive")})
                                              nil)
                            ;; Refuse rather than open a Picker whose picks grant
                            ;; nothing. The alternative failure is a 404 at sync
                            ;; time that names the file, not the missing app id.
                            (str/blank? app_id)
                            (do (on-status {:phase :error
                                            :message "Google Picker is misconfigured — no app id"})
                                nil)
                            :else (pick-then-sync! topic-id token app_id on-status)))))

               :else (do (on-status {:phase :error :message (or error "Sync failed")})
                         result))))
    (.then (fn [result]
             ;; The second sync, the one after a fresh bind, reports here.
             (when (and (map? result) (contains? result :success))
               (if (:success result)
                 (on-status {:phase :done})
                 (on-status {:phase :error
                             :message (or (:error result) "Sync failed")})))
             nil))
    (.catch (fn [e]
              (on-status {:phase :error
                          :message (or (.-message e) "Sync failed")})
              nil))))
