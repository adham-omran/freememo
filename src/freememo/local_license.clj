(ns freememo.local-license
  "The Sync License reminder of a Local Install (plans/local-install.md,
   \"New: screens as pages\"). Electron showed it in its own window. Here it is
   a page of the local server.

   After the trial, with no Sync License (or a license that does not cover this
   version), the first page of each launch is this reminder. \"Continue\" opens
   the library. The page shows once per launch: `wrap-reminder` keeps that
   flag in memory, so a restart shows it again. Local use never depends on the
   license, so Continue always works.

   The license comes from the cache of the last sync (`sync-client/status`),
   so an offline launch shows the reminder too.

   NO db or app requires: the launcher passes `license-fn`, so tests load this
   namespace with PostgreSQL stopped."
  (:require [freememo.local-http :as http]))

(def nag-states
  "The license states that start a launch with the reminder."
  #{"unlicensed" "expired" "grace"})

(def continue-path "/license-reminder/continue")

(defn purchase-url
  "Post: the existing checkout, in Settings on freememo.net."
  [origin]
  (str origin "/settings#settings-desktop"))

(defn reminder-text
  "Post: the sentence for `license` (a map with a \"state\" key)."
  [license]
  (if (= "grace" (get license "state"))
    (str "Your Sync License does not cover this version. Sync downloads only"
      (if-let [end (get license "grace-ends")]
        (str ", until " (subs (str end) 0 (min 10 (count (str end)))) ".")
        ", for now."))
    (str "Your FreeMemo trial has ended. Your library stays on this computer and keeps working. "
      "Sync with freememo.net needs a Sync License.")))

(defn reminder-page
  "Post: a Ring response with the reminder. The page has its own title, which
         the gate marks as Local. It links to the checkout only when `buy?`."
  [origin license buy?]
  (http/page
    (str "<h1>Sync License</h1><p id=\"text\">" (http/esc (reminder-text license)) "</p>"
      (when buy?
        (str "<p><a class=\"buy\" id=\"buy\" href=\"" (http/esc (purchase-url origin))
          "\" target=\"_blank\" rel=\"noopener\">Buy a Sync License</a></p>"))
      "<form method=\"post\" action=\"" continue-path "\">"
      "<button type=\"submit\">Continue to my library</button></form>")
    {:title "FreeMemo Sync License"}))

(defn wrap-reminder
  "Pre:  `license-fn` is `(fn [])`: the cached license map (string keys) or nil.
         `origin` is freememo.net's origin. `buy?` is `(fn [])`: false while no
         license is for sale (Settings follows the same rule, `buy-url*`).
   Post: until the student continues, GET / answers the reminder when the
         license state is in `nag-states`. POST to `continue-path` ends the
         reminder for this launch and redirects to /. Everything else goes to
         `handler`."
  [handler {:keys [origin license-fn buy?] :or {buy? (constantly true)}}]
  (let [!seen (atom false)]
    (fn [request]
      (cond
        (and (= :post (:request-method request)) (= continue-path (:uri request)))
        (do (reset! !seen true)
            {:status 303 :headers {"Location" "/"} :body ""})

        (and (not @!seen) (= :get (:request-method request)) (= "/" (:uri request)))
        (let [license (try (license-fn) (catch Throwable _ nil))]
          (if (contains? nag-states (get license "state"))
            (reminder-page origin license (boolean (buy?)))
            (handler request)))

        :else (handler request)))))
