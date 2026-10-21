(ns freememo.neoreader-picker
  "Browser half of the NeoReader sync: pick a Drive file, then ask the server to
   sync against it.

   The Picker itself, and why it loads from Google, is `freememo.google-picker`.

   The Picker needs an OAuth token, and the SERVER vends it from the stored
   refresh token. The browser therefore runs no OAuth flow of its own, so there
   is one grant, one consent screen, and one credential behind both the Picker
   and the server-side download.

   A folder pick would grant nothing about the folder's contents under
   `drive.file`, so this picks a FILE. The user navigates to
   `onyx/<DEVICE>/Read/` themselves, and the view is filtered to HTML so the
   folder shows only exports."
  (:require [clojure.string :as str]
            [freememo.google-picker :as picker]))

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

(defn- pick-then-sync!
  "Open the Picker, bind what the user chose, and sync.

   Pre : `token` is a live `drive.file` token and `app-id` is the project number.
   Post: a promise of the sync response map, or nil when the user cancelled or
         the bind failed. A cancelled Picker binds nothing and syncs nothing."
  [topic-id token app-id on-status]
  (on-status {:phase :picking})
  (-> (picker/load!)
    (.then (fn [_] (picker/pick-file!
                     {:token token :app-id app-id :mime-types "text/html"
                      :title "Choose your NeoReader export (onyx / DEVICE / Read)"})))
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
