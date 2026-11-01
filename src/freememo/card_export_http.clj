(ns freememo.card-export-http
  "GET and POST /api/cards/export: one user's cards as a file another
   flashcard app imports. See plans/card-export-csv-tsv.md.

   The route resolves a card set and streams `freememo.card-export`'s file.
   It owns nothing about the format.

   | scope    | cards                                        | method |
   |----------|----------------------------------------------|--------|
   | `topic`  | the topic and all its descendants            | GET    |
   | `all`    | every card of the user                       | GET    |
   | `filter` | what Library > Cards lists for posted `opts` | POST   |

   The session names the user. Nothing in the request can name another
   account: a topic outside it answers 404, and the filter query is scoped by
   the session's user id."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [freememo.card-export :as export]
            [freememo.db :as db]
            [freememo.user-state :as us]
            [ring.core.protocols :as ring-protocols]
            [taoensso.telemere :as tel])
  (:import [java.io BufferedWriter OutputStream OutputStreamWriter Writer]
           [java.nio.charset StandardCharsets]
           [java.util.zip ZipEntry ZipOutputStream]))

;; ── Card sets ──────────────────────────────────────────────────────

(defn- with-root-titles
  "Attach `:root_title` from one title query over the cards' roots."
  [cards]
  (let [titles (db/get-topic-titles (into [] (comp (keep :flashcards/root_topic_id) (distinct)) cards))]
    (mapv #(assoc % :root_title (get titles (:flashcards/root_topic_id %))) cards)))

(defn- library-filter-cards
  "The cards Library > Cards lists for `opts`, in its order.

   Calls the view's own query fn, so the export cannot disagree with the
   rows on screen. Resolved at call time because `freememo.library-cards` is an
   Electric namespace: `freememo.main` loads it before the first request, and a
   static require from this handler namespace would put its compilation on
   whichever thread loads `freememo.api`.
   Post: a vector of flashcard rows, or nil when the query failed."
  [user-id opts]
  (let [query (requiring-resolve 'freememo.library-cards/query-user-cards*)
        result (query nil user-id opts)]
    (when (:success result)
      (into [] (mapcat :members) (:cards result)))))

(defn card-set
  "The cards and the download base name for one request's scope.

   Pre : `user-id` comes from the session.
   Post: {:cards [row] :base s}, or nil when the topic is not the user's or the
         filter query failed. Every row carries `:root_title`."
  [user-id {:keys [scope topic-id opts]}]
  (case scope
    :topic  (when (and topic-id (db/owns-topic? user-id topic-id))
              {:cards (with-root-titles (db/get-flashcards-for-subtree topic-id))
               :base (get (db/get-topic-titles [topic-id]) topic-id)})
    :all    {:cards (db/get-user-flashcards user-id) :base "freememo-cards"}
    :filter (when-let [cards (library-filter-cards user-id opts)]
              {:cards cards :base "freememo-cards"})
    nil))

;; ── Request parsing ────────────────────────────────────────────────

(def ^:private opts-keys #{:text :kinds :origins :flags :overlay-ids :sort-col :sort-dir})

(defn parse-opts
  "The Library filter opts posted as EDN, or nil when they are not a map of
   the keys `query-user-cards*` reads. EDN carries no code, so reading a
   client value is safe. Unknown keys are dropped."
  [s]
  (try
    (let [v (edn/read-string {:default (fn [_ _] nil)} (or s ""))]
      (when (map? v) (select-keys v opts-keys)))
    (catch Exception _ nil)))

(defn parse-request
  "Pre: `params` is Ring's string-keyed params map.
   Post: {:scope kw :topic-id long|nil :opts map|nil :preset map}, or
         {:error s} naming the first bad parameter."
  [method params]
  (let [scope (keyword (get params "scope"))
        preset (export/resolve-preset (get params "preset") (= "1" (get params "images")))
        topic-id (some-> (get params "topic-id") parse-long)
        opts (when (= scope :filter) (parse-opts (get params "opts")))]
    (cond
      (nil? preset) {:error "Unknown preset"}
      (not (contains? #{:topic :all :filter} scope)) {:error "Unknown scope"}
      (and (= scope :filter) (not= method :post)) {:error "The filter scope needs POST"}
      (and (= scope :filter) (nil? opts)) {:error "Bad filter options"}
      (and (= scope :topic) (nil? topic-id)) {:error "Missing topic-id"}
      :else {:scope scope :topic-id topic-id :opts opts :preset preset})))

;; ── Response body ──────────────────────────────────────────────────

(defn- utf8-writer ^Writer [^OutputStream out]
  (BufferedWriter. (OutputStreamWriter. out StandardCharsets/UTF_8)))

(defn- write-media!
  "Write each named media file into `zos` under media/, one at a time.
   Pre: `names` holds only ids `user-id` owns (`media-mime-types-for-user`)."
  [^ZipOutputStream zos user-id names]
  (doseq [[id file] names]
    (when-let [row (db/get-media id)]
      (when (= user-id (:media/user_id row))
        (.putNextEntry zos (ZipEntry. (str "media/" file)))
        (.write zos ^bytes (:media/bytes row))
        (.closeEntry zos)))))

(defn write-export!
  "Write the export of `cards` to `out`: the text file, or for a preset with
   images a zip of `cards.txt` and `media/`.
   Post: `out` is finished but not closed. Returns the summary."
  [^OutputStream out user-id preset cards]
  (if (:images? preset)
    (let [ids (export/cards-media-ids cards)
          mimes (db/media-mime-types-for-user user-id ids)
          names (into (sorted-map)
                  (keep (fn [id] (when-let [m (get mimes id)]
                                   [id (export/media-filename id m)])))
                  ids)
          zos (ZipOutputStream. out)
          w (utf8-writer zos)]
      (.putNextEntry zos (ZipEntry. "cards.txt"))
      (let [summary (export/write-cards! w preset cards names)]
        (.flush w)
        (.closeEntry zos)
        (write-media! zos user-id names)
        (.finish zos)
        summary))
    (let [w (utf8-writer out)
          summary (export/write-cards! w preset cards nil)]
      (.flush w)
      summary)))

(defrecord CardExportBody [user-id preset cards])

(extend-protocol ring-protocols/StreamableResponseBody
  CardExportBody
  (write-body-to-stream [this _response ^OutputStream out]
    ;; Membership is what the Activity monitor sees, as in account export.
    ;; `finally` inside with-in-flight: a client that leaves mid-download
    ;; breaks the pipe here, and the row must still close.
    (let [{:keys [user-id preset cards]} this]
      (us/with-in-flight user-id :exporting-cards "cards"
        (let [summary (write-export! out user-id preset cards)]
          (tel/log! {:level :info :id ::cards-exported
                     :data (assoc summary :user-id user-id :preset (:id preset))}
            "Card export written")
          (.close out))))))

(defn- skipped-header
  "`occlusion=3,score=1`, or `none`."
  [skipped]
  (if (empty? skipped)
    "none"
    (str/join "," (map (fn [[k n]] (str k "=" n)) (sort skipped)))))

(defn export-handler
  "GET or POST /api/cards/export.

   Pre : `request` carries a Ring session and parsed params.
   Post: 200 streaming the file, 400 for a bad parameter, 401 signed out, or
         404 for a topic the user does not own. The counts go in headers, so a
         client can read them without parsing the body.
   Blame: a throw inside the body is a truncated download, because the status
         and headers are already sent."
  [request]
  (if-let [user-id (get-in request [:session :user-id])]
    (let [parsed (parse-request (:request-method request) (:params request))]
      (if-let [error (:error parsed)]
        {:status 400 :body error}
        (if-let [{:keys [cards base]} (card-set user-id parsed)]
          (let [preset (:preset parsed)
                summary (export/summarize preset cards)]
            {:status 200
             :headers {"Content-Type" (if (:images? preset)
                                        "application/zip"
                                        (if (= "csv" (:ext preset))
                                          "text/csv; charset=utf-8"
                                          "text/plain; charset=utf-8"))
                       "Content-Disposition" (str "attachment; filename=\""
                                               (export/download-filename base preset) "\"")
                       "Cache-Control" "no-store"
                       "X-Export-Cards" (str (:cards summary))
                       "X-Export-Rows" (str (:rows summary))
                       "X-Export-Dropped" (str (:dropped summary))
                       "X-Export-Skipped" (skipped-header (:skipped summary))}
             :body (->CardExportBody user-id preset cards)})
          {:status 404 :body "Not found"})))
    {:status 401 :body "Not authenticated"}))
