(ns freememo.youtube-api
  "Loads YouTube's IFrame Player API on demand (plans/youtube-kind.md 3.1).

   This is the second runtime third-party script, after `gtag.js`. It cannot
   be vendored: `iframe_api` is a small loader that fetches a versioned widget
   script from YouTube, so a pinned copy would still load an unpinned one. It
   is not a `vendor-libs` group for the same reason: those groups are files
   that `vendor-lock.sha256` hashes.

   Nothing requests a YouTube host until a YouTube topic mounts a player.

   The wait is capped, like `math/on-katex-ready!`. A stalled script would
   otherwise leave the promise pending forever and the pane blank with no
   message. After a failure the next call tries again, so a reload of the
   topic is a retry.")

(def ^:private api-url "https://www.youtube.com/iframe_api")

(def wait-ms
  "How long a player waits for the API before it shows the load error."
  10000)

(defonce ^:private !ready (atom nil))

(defn- loaded-yt []
  (let [yt (.-YT js/window)]
    (when (and yt (.-Player yt)) yt)))

(defn- inject-script! []
  (when-not (.querySelector js/document (str "script[src=\"" api-url "\"]"))
    (let [s (.createElement js/document "script")]
      (set! (.-src s) api-url)
      (set! (.-async s) true)
      (.appendChild (.-head js/document) s))))

(defn- load! []
  (js/Promise.
    (fn [resolve reject]
      (if-let [yt (loaded-yt)]
        (resolve yt)
        (let [prev (.-onYouTubeIframeAPIReady js/window)
              timer (js/setTimeout #(reject (js/Error. "YouTube player API timed out")) wait-ms)]
          ;; The API calls this global once when it is ready. Chain any handler
          ;; already there rather than replace it.
          (set! (.-onYouTubeIframeAPIReady js/window)
                (fn []
                  (js/clearTimeout timer)
                  (when (fn? prev) (prev))
                  (resolve (.-YT js/window))))
          (inject-script!))))))

(defn ensure!
  "A promise of `window.YT` with `YT.Player` defined. One shared promise while
   a load is in flight or done; a rejected load is forgotten so the next call
   retries.

   Post: resolves to `window.YT`, or rejects after `wait-ms`."
  []
  (or @!ready
      (reset! !ready
        (.catch (load!)
                (fn [e]
                  (reset! !ready nil)
                  (js/Promise.reject e))))))
