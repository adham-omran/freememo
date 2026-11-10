(ns freememo.local-gate
  "The HTTP gate of a Local Install (docs/adr/0001-jvm-launcher-replaces-electron.md).

   The user's own browser is the UI, and any web page or local program can send
   a request to 127.0.0.1. The gate lets a request through only when:

   1. its Host header is `127.0.0.1:<port>` or `localhost:<port>`, so DNS rebinding fails;
   2. a state-changing request, or a WebSocket upgrade, carries an Origin header
      that is one of those two origins, so a foreign page cannot act;
   3. it carries the `fm_local` cookie with the install's secret. The launcher
      may name requests that need no cookie (`:public?`). Ticket 04 names the
      sign-in callback, which freememo.net's redirect sends without the
      SameSite=Strict cookie. Rules 1 and 2 still apply to it.

   The launcher opens the browser at a URL with a one-time `fm_token`. A GET
   with a valid token gets the cookie (HttpOnly, SameSite=Strict, ten years)
   and a redirect to the same URL without the token. A token works once, and
   only for five minutes.

   NO db or app requires: this namespace is a pure Ring middleware, so tests
   load it with PostgreSQL stopped."
  (:require [clojure.string :as str]
            [freememo.local-http :as http])
  (:import [java.net URLEncoder]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.util Base64]
           [java.util.concurrent ConcurrentHashMap]))

(def cookie-name "fm_local")
(def token-param "fm_token")
(def token-ttl-ms (* 5 60 1000))

(def ^:private cookie-max-age (* 10 365 24 60 60))

(defn- same-secret? [^String a ^String b]
  (and a b (MessageDigest/isEqual (.getBytes a StandardCharsets/UTF_8)
             (.getBytes b StandardCharsets/UTF_8))))

(defn- cookie-value [request]
  (some->> (get-in request [:headers "cookie"])
    (re-find (re-pattern (str "(?:^|;\\s*)" cookie-name "=([^;]+)")))
    second))

(defn- deny [status text]
  {:status status :headers {"Content-Type" "text/plain"} :body text})

(defn- mark-title
  "Post: an HTML shell that reads `<title>FreeMemo Local …`, so the mark shows
   before the client boots. Any other response is unchanged."
  [response]
  (let [content-type (some-> response (get-in [:headers "Content-Type"]))]
    (if (and (string? (:body response)) content-type (str/starts-with? content-type "text/html"))
      (update response :body str/replace "<title>FreeMemo" "<title>FreeMemo Local")
      response)))

(defn new-token
  "Post: 256 random bits as base64url text."
  []
  (let [bytes (byte-array 32)]
    (.nextBytes (SecureRandom.) bytes)
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bytes)))

(defn make-gate
  "Pre:  `origin` is \"http://127.0.0.1:<port>\". `secret` is the cookie value,
         which the launcher keeps in the data directory so a cookie lasts
         across restarts. `user-id` is the one account of this install, or a
         `(fn [])` that answers it (the account can change after a removal).
         `local-route` is `(fn [request])` for the shell endpoints, nil for none.
         `public?` is `(fn [request])`: true for a request that needs no cookie.
   Post: `{:wrap f, :issue-token! f}`. `(issue-token!)` returns a new one-time
         token. `(wrap handler)` returns the gated handler. It sits inside
         `wrap-session`, which it needs for the :session user id."
  [{:keys [origin secret user-id local-route public? now-ms]
    :or {now-ms #(System/currentTimeMillis)}}]
  (let [host (str/replace origin #"^https?://" "")
        port (second (re-find #":(\d+)$" host))
        hosts #{host (str "localhost:" port)}
        origins (into #{origin} (map #(str "http://" %)) hosts)
        tokens (ConcurrentHashMap.)
        issue-token! (fn []
                       (let [t (new-token)]
                         (.put tokens t (+ (now-ms) token-ttl-ms))
                         t))
        take-token! (fn [t]
                      ;; `remove` is atomic: two requests cannot both win.
                      (let [expiry (.remove tokens t)]
                        (boolean (and expiry (< (now-ms) expiry)))))
        cookie-ok? (fn [request] (same-secret? (cookie-value request) secret))
        stripped-location (fn [request]
                            (let [rest-pairs (remove (fn [[k _]] (= k token-param))
                                               (http/query-pairs (:query-string request)))
                                  enc #(URLEncoder/encode ^String % "UTF-8")]
                              (str (:uri request)
                                (when (seq rest-pairs)
                                  (str "?" (str/join "&" (map (fn [[k v]] (if v (str (enc k) "=" (enc v)) (enc k)))
                                                           rest-pairs)))))))
        wrap (fn [handler]
               (fn [request]
                 (let [headers (:headers request)
                       origin-header (get headers "origin")
                       method (:request-method request)
                       websocket? (some-> (get headers "upgrade") str/lower-case (= "websocket"))
                       token (some (fn [[k v]] (when (= k token-param) v)) (http/query-pairs (:query-string request)))]
                   (cond
                     (not (contains? hosts (get headers "host")))
                     (deny 403 "Forbidden: wrong Host")

                     (and (or websocket? (not (#{:get :head :options} method)))
                       (not (contains? origins origin-header)))
                     (deny 403 "Forbidden: wrong Origin")

                     (and token (= :get method))
                     (cond
                       (take-token! token)
                       {:status 302
                        :headers {"Location" (stripped-location request)
                                  "Cache-Control" "no-store"
                                  "Set-Cookie" (str cookie-name "=" secret "; Path=/; HttpOnly; SameSite=Strict; Max-Age=" cookie-max-age)}
                        :body ""}
                       (cookie-ok? request)
                       {:status 302 :headers {"Location" (stripped-location request)} :body ""}
                       :else (deny 401 "Unauthorized"))

                     (and public? (public? request))
                     (mark-title (handler request))

                     (not (cookie-ok? request))
                     (deny 401 "Unauthorized")

                     :else
                     (let [request (assoc-in request [:session :user-id] (if (fn? user-id) (user-id) user-id))]
                       (or (when local-route (local-route request))
                         (mark-title (handler request))))))))]
    {:wrap wrap :issue-token! issue-token!}))
