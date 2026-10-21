(ns freememo.gdoc-import
  "Browser half of a Google Doc import (plans/google-docs-pull.md, row 4.2):
   get a token for one connected account, let the user pick a Doc in that
   account's Drive, then ask the server to import it.

   The server vends the token, so the browser runs no OAuth flow. The Picker
   shows the Drive of the token's account, which is what lets a browser signed
   in to a personal Google account browse a work account's Drive."
  (:require [freememo.google-picker :as picker]))

(def ^:private doc-mime-type "application/vnd.google-apps.document")

(defn- json-of [response]
  (-> (.json response) (.then #(js->clj % :keywordize-keys true))))

(defn- picker-token
  "Post: a promise of {:success bool :token s :app_id s :error s :reconnect bool}."
  [account-sub]
  (-> (js/fetch (str "/api/gdoc/picker-token?account=" (js/encodeURIComponent account-sub))
        #js {:credentials "same-origin"})
    (.then json-of)))

(defn- post-import
  "Post: a promise of {:success bool :topic_id n :error s}."
  [account-sub file-id]
  (-> (js/fetch "/api/gdoc/import"
        #js {:method "POST" :credentials "same-origin"
             :body (js/URLSearchParams. #js {"account" account-sub "file_id" file-id})})
    (.then json-of)))

(defn import-doc!
  "Pick a Doc in connected account `account-sub`'s Drive and import it.

   Pre : the user is signed in, and `account-sub` is one of their connected
         accounts. `on-status` takes one map and is called at every step:
           {:phase :picking|:importing|:cancelled|:done|:error …}
         :done carries :topic-id, the Doc's root. :error carries :message, and
         :topic-id when the Doc was already imported.
   Post: a promise that always resolves. Every outcome arrives through
         `on-status`, so a caller needs no catch."
  [account-sub on-status]
  (on-status {:phase :picking})
  (-> (picker-token account-sub)
    (.then (fn [{:keys [success token app_id error]}]
             (if-not success
               (on-status {:phase :error :message (or error "Could not reach Google Drive.")})
               (-> (picker/load!)
                 (.then (fn [_] (picker/pick-file! {:token token :app-id app_id
                                                    :mime-types doc-mime-type
                                                    :title "Choose a Google Doc"})))
                 (.then (fn [file-id]
                          (if (nil? file-id)
                            (on-status {:phase :cancelled})
                            (do (on-status {:phase :importing})
                                (-> (post-import account-sub file-id)
                                  (.then (fn [{:keys [success topic_id error]}]
                                           (if success
                                             (on-status {:phase :done :topic-id topic_id})
                                             (on-status (cond-> {:phase :error
                                                                 :message (or error "The import failed.")}
                                                          topic_id (assoc :topic-id topic_id)))))))))))))))
    (.catch (fn [e]
              (on-status {:phase :error :message (or (.-message e) "The import failed.")})))))
