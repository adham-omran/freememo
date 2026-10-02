(ns freememo.reading-position-http
  "POST /api/reading-position (plans/reading-position-memory.md §4.1).

   An HTTP route rather than an Electric server call for the reason
   `video-http/position-handler` gives: the client sends on leave, and only a
   `sendBeacon` survives the teardown that leaving causes. An Electric effect
   in flight is cancelled with its frame."
  (:require
   [freememo.db :as db]
   [freememo.reading-anchor :as ra]
   [freememo.upload-http :refer [json-response require-auth]]
   [taoensso.telemere :as tel]))

(defn- parse-number [s]
  (when (string? s)
    (let [x (parse-double s)]
      (when (and x (Double/isFinite x)) x))))

(defn position-body
  "The position a form-encoded beacon carries, or nil when it is not one.
   Pre : `params` has string keys and string values, as Ring's form parser
         gives them.
   Post: {:topic-id :surface :anchor {:i :fp :frac} :fraction}, every value
         inside the bounds `reading-anchor/valid-anchor?` checks (§4 1.6)."
  [params]
  (let [topic-id (some-> (get params "topic_id") str parse-long)
        surface (get params "surface")
        anchor {:i (some-> (get params "i") str parse-long)
                :fp (get params "fp")
                :frac (parse-number (get params "frac"))}
        fraction (parse-number (get params "fraction"))]
    (when (and topic-id (contains? ra/surfaces surface)
            (ra/valid-anchor? anchor fraction))
      {:topic-id topic-id :surface surface :anchor anchor :fraction fraction})))

(defn position-handler
  "Pre : form-encoded {topic_id, surface, i, fp, frac, fraction}.
   Post: 204 with no body for any signed-in request. A beacon has no error
         channel, so a body that is not a position is dropped and logged, and
         a forged topic id writes nothing (`db/save-reading-position!`)."
  [request]
  (if-let [user-id (require-auth request)]
    (try
      (if-let [{:keys [topic-id surface anchor fraction]} (position-body (:params request))]
        (db/save-reading-position! user-id topic-id surface anchor fraction)
        (tel/log! {:level :warn :id ::invalid-position
                   :data {:user-id user-id :params (:params request)}}
          "Reading position body rejected"))
      {:status 204 :headers {} :body ""}
      (catch Exception e
        (tel/error! {:id ::save-position} e)
        {:status 204 :headers {} :body ""}))
    (json-response 401 {:success false :error "Not authenticated"})))
