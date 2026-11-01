(ns freememo.youtube-import
  "Import a YouTube link as a `youtube` topic (plans/youtube-kind.md §2).

   No bytes are fetched. The only network call is YouTube's oEmbed endpoint,
   which gives the title and the channel without an API key. The import never
   depends on it: any oEmbed failure still creates the topic."
  (:require [cheshire.core :as json]
            [clj-http.client :as http]
            [freememo.commands :as commands]
            [freememo.db :as db]
            [freememo.desktop-link :as link]
            [freememo.youtube-url :as youtube-url]
            [taoensso.telemere :as tel]))

(def ^:private oembed-timeout-ms 5000)

(def embedding-warning
  "The toast text for an import whose oEmbed answered 401. A 401 is a probable
   sign that the owner disabled embedding; the player's error 101/150 is the
   certain one, so this warns and never refuses."
  "Imported. The owner may not allow embedding, so it may not play here.")

(defn fetch-oembed
  "YouTube's oEmbed answer for `video-id`.

   Post: {:status :ok :title S :channel S-or-nil}, {:status :unauthorized} for
         a 401, or {:status :unavailable} for anything else: offline, a
         timeout, another status, or a body with no title. Never throws."
  [video-id]
  (if (link/offline-message "YouTube lookup")
    {:status :unavailable}
    (try
      (let [resp (http/get "https://www.youtube.com/oembed"
                   {:query-params {"url" (youtube-url/watch-url video-id) "format" "json"}
                    :cookie-policy :none
                    :throw-exceptions false
                    :socket-timeout oembed-timeout-ms
                    :connection-timeout oembed-timeout-ms
                    :max-body 65536})
            status (:status resp)]
        (cond
          (= 401 status) {:status :unauthorized}
          (= 200 status)
          (let [body (json/parse-string (:body resp))
                title (get body "title")]
            (if (string? title)
              {:status :ok :title title :channel (get body "author_name")}
              {:status :unavailable}))
          :else {:status :unavailable}))
      (catch Exception e
        (tel/log! {:level :warn :id ::oembed :data {:video-id video-id}} (.getMessage e))
        {:status :unavailable}))))

(defn existing-topic
  "The user's root topic for the video `url` names, or nil when `url` names no
   YouTube video or the user has none. Deduplicates by video id, so a
   `youtu.be` link and a watch link to one video find one topic.

   Post: {:topic-id N :title S} or nil."
  [user-id url]
  (when-let [{:keys [video-id]} (youtube-url/parse url)]
    (db/find-youtube-root user-id video-id)))

(defn import!
  "Create the topic for the YouTube video that `parsed` names.

   Pre:  `parsed` is a non-nil `youtube-url/parse` answer.
   Post: {:ok true :flow :already-exists :topic-id N :title S}, or
         {:ok true :flow :imported :topic-id N :title S}, with
         :warning S when oEmbed answered 401."
  [user-id {:keys [video-id start-ms]}]
  (if-let [existing (db/find-youtube-root user-id video-id)]
    (assoc existing :ok true :flow :already-exists)
    (let [{:keys [status title channel]} (fetch-oembed video-id)
          created (db/create-youtube-topic! user-id
                    {:video-id video-id
                     :title (or title (str "YouTube video " video-id))
                     :channel channel
                     :start-ms start-ms})]
      (commands/bump! user-id :import-document)
      (cond-> (assoc created :ok true :flow :imported)
        (= :unauthorized status) (assoc :warning embedding-warning)))))
