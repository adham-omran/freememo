(ns freememo.zotero-client
  "Browser-side client for the FreeMemo-for-Zotero plugin's HTTP API.

   Talks to http://127.0.0.1:23119/freememo/* — endpoints registered by the
   plugin (see freememo-zotero-plugin/ at the repo root). All calls are
   `fetch`-based and return Promises; results are Clojure maps with
   :keywordize-keys true.

   Pre across all fns: the user has Zotero running, the FreeMemo plugin
   installed, and 'Allow other applications…' enabled in Zotero Settings →
   Advanced. Pre violations surface as transport errors with :ok? false.

   Result shape:
     {:ok? bool :status int :data <map|nil> :error <string|nil>}
   Transport failure ⇒ :ok? false, :status 0, :error <message>."
  (:require
   [hyperfiddle.electric3 :as e]))

(def ^:private base-url "http://127.0.0.1:23119/freememo")

#?(:cljs
   (defn- json-fetch!
     "GET a JSON endpoint. Pre: url is the plugin's loopback URL. Post:
      resolves to the result shape documented at the top of the ns; never
      rejects. Transport failure → :ok? false, :status 0, :error <message>;
      caller-visible error always lives in :error so UI doesn't have to
      sniff JS vs CLJS shapes."
     [url]
     (-> (js/fetch url (clj->js {:method "GET"}))
         (.then (fn [resp]
                  (-> (.json resp)
                      (.then (fn [body]
                               {:ok?    (.-ok resp)
                                :status (.-status resp)
                                :data   (js->clj body :keywordize-keys true)
                                :error  nil})))))
         (.catch (fn [e]
                   (js/console.warn "FreeMemo zotero-client fetch failed:" e)
                   {:ok?    false
                    :status 0
                    :data   nil
                    :error  (or (.-message e) "Fetch failed")})))))

#?(:cljs
   (defn probe!
     "GET /freememo/probe. Post: resolves to result with :data
      {:ok :plugin_version :zotero_version :library_id} on 200."
     []
     (json-fetch! (str base-url "/probe"))))

#?(:cljs
   (defn list-items!
     "GET /freememo/items/top?limit=&start=. Returns one page."
     [{:keys [limit start] :or {limit 200 start 0}}]
     (json-fetch! (str base-url "/items/top?limit=" limit "&start=" start))))

#?(:cljs
   (defn list-all-items!
     "Drain all pages of /freememo/items/top.

      Pre: caps the drain at `:max-items` (default 10000) to keep the
      browser from pulling unbounded libraries. Post: resolves to
      {:ok? :data {:items [...]}}. On any page failure, short-circuits
      with that page's failure result."
     ([] (list-all-items! {:max-items 10000 :page-size 500}))
     ([{:keys [max-items page-size]
        :or {max-items 10000 page-size 500}}]
      (js/Promise.
        (fn [resolve _reject]
          (let [acc (atom [])]
            (letfn [(step [start]
                      (-> (list-items! {:limit page-size :start start})
                          (.then
                            (fn [result]
                              (cond
                                (not (:ok? result))
                                (resolve result)

                                :else
                                (let [items (get-in result [:data :items] [])
                                      next-start (get-in result [:data :next_start])]
                                  (swap! acc into items)
                                  (cond
                                    (>= (count @acc) max-items)
                                    (resolve {:ok? true :status 200
                                              :data {:items (vec (take max-items @acc))}
                                              :error nil})

                                    (nil? next-start)
                                    (resolve {:ok? true :status 200
                                              :data {:items @acc}
                                              :error nil})

                                    :else
                                    (step next-start))))))))]
              (step 0))))))))

#?(:cljs
   (defn get-csljson!
     "GET /freememo/items/{key}/csljson. Post: :data {:csl <map|nil>}.
      nil CSL is allowed (attachment-only items)."
     [item-key]
     (json-fetch! (str base-url "/items/" (js/encodeURIComponent item-key) "/csljson"))))

#?(:cljs
   (defn list-pdf-attachments!
     "GET /freememo/items/{key}/children/pdfs. Post: :data {:pdfs [...]}.
      Empty :pdfs is allowed (item has no PDF children)."
     [item-key]
     (json-fetch! (str base-url "/items/" (js/encodeURIComponent item-key) "/children/pdfs"))))

#?(:cljs
   (defn- base64->uint8array
     "Decode a base64 string to Uint8Array. Pre: b64 is a valid base64
      string (no whitespace assumed). Post: Uint8Array of decoded bytes."
     [b64]
     (let [binary (js/atob b64)
           n      (.-length binary)
           out    (js/Uint8Array. n)]
       (dotimes [i n]
         (aset out i (.charCodeAt binary i)))
       out)))

#?(:cljs
   (defn fetch-attachment-bytes!
     "GET /freememo/items/{key}/file. Post: :data {:filename :content_type
      :size :bytes} where :bytes is a Uint8Array decoded from the
      base64 payload. Pre: attachment-key is a PDF attachment under the
      user library. 413 too-large and 404 not-found surface as ordinary
      result maps (:ok? false, :status set, :data carries the server's
      error body)."
     [attachment-key]
     (-> (json-fetch! (str base-url "/items/" (js/encodeURIComponent attachment-key) "/file"))
         (.then (fn [result]
                  (if (and (:ok? result) (get-in result [:data :base64]))
                    (let [{:keys [filename content_type size base64]} (:data result)]
                      (assoc result :data
                        {:filename     filename
                         :content_type content_type
                         :size         size
                         :bytes        (base64->uint8array base64)}))
                    result))))))
