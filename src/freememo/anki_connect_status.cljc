(ns freememo.anki-connect-status
  "What one press of Connect found out about AnkiConnect
   (plans/getting-started-checklist.md 8.3). No requires, so `clj -M:test`
   reaches it with no browser.

   The browser cannot tell Anki closed from a CORS block or a local-network
   block: all three fail with the same TypeError. That is why they share one
   outcome, `:unreachable`, and the guide lists every cause.")

(def connect-timeout-ms
  "How long Connect waits for AnkiConnect (8.2). A request with no answer is
   most often App Nap on macOS, which stalls Anki while it is in the back."
  5000)

(def api-version
  "The AnkiConnect API version `anki-call!` speaks."
  6)

(defn classify
  "The outcome of one `requestPermission` call.

   Pre : `outcome` is {:result map} when AnkiConnect answered, with the map
         `anki-call!` returns, or {:failure :timeout} or
         {:failure :unreachable} when it did not.
   Post: one of :granted :denied :needs-key :unreachable :timeout.
         :needs-key wins over :granted, because FreeMemo sends no API key.
         The flag is read under both spellings: the README says
         `requireApiKey`, and the add-on's source sends `requireApikey`."
  [{:keys [result failure]}]
  (cond
    (= failure :timeout) :timeout
    (some? failure) :unreachable
    (not= "granted" (:permission result)) :denied
    (or (true? (:requireApiKey result)) (true? (:requireApikey result))) :needs-key
    :else :granted))

(defn outdated?
  "True when a granted AnkiConnect reports an API version older than the one
   FreeMemo speaks (8.3.1). A missing version is not proof of age."
  [{:keys [version]}]
  (and (number? version) (< version api-version)))
