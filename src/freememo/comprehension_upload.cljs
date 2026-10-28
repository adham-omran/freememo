(ns freememo.comprehension-upload
  "Upload one original answer scan while Forms5 owns the edit lifecycle.
   Cancellation stops the browser request. A saved original can remain on the server.")

(defn upload! [cycle-id file]
  (fn [success failure]
    (let [controller (js/AbortController.)
          body (js/FormData.)]
      (.append body "cycle_id" (str cycle-id))
      (.append body "file" file)
      (-> (js/fetch "/api/comprehension/scan"
            #js {:method "POST" :body body :signal (.-signal controller)})
        (.then (fn [response]
                 (-> (.json response)
                   (.then #(success (js->clj % :keywordize-keys true))))))
        (.catch failure))
      #(.abort controller))))
