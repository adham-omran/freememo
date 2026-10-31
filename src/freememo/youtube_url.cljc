(ns freememo.youtube-url
  "Which addresses are YouTube, so web import can refuse them.

   A YouTube page is a JavaScript shell. Fetched as an article, its only
   server-rendered text is the site footer, in whatever language YouTube picks
   for the server's IP. Video import takes files only
   (plans/incremental-video.md 11.1), so the honest answer is a refusal that
   names the path that works. See plans/feedback-activity-biblio-youtube.md.

   One definition, read by the server boundary (`wikipedia/fetch-url`) and the
   Import modal's fast path. Requires only `clojure.string`, so both peers load
   it with nothing else."
  (:require [clojure.string :as str]))

(def rejection-message
  "YouTube links can't be imported. Use Import → Video with a downloaded file.")

(def ^:private host-re
  ;; Optional "scheme://" or a bare "//", optional userinfo, then the host up
  ;; to a port, path, query or fragment. A scheme only counts with its "//":
  ;; "youtube.com:443/x" is a host and a port, not the scheme "youtube.com".
  #"^(?:(?:[A-Za-z][A-Za-z0-9+.\-]*:)?//)?(?:[^/?#@]*@)?([^/?#:]+)")

(def ^:private youtube-domains
  ["youtube.com" "youtu.be" "youtube-nocookie.com"])

(defn- host-of [url]
  (some-> (re-find host-re (str/trim url))
    second
    str/lower-case
    (str/replace #"\.$" "")))

(defn youtube-url?
  "True when `url`'s host is a YouTube domain or one of its subdomains, for any
   path. Pre: `url` is a string or nil. Post: a boolean; nil and blank are false."
  [url]
  (boolean
    (when-some [host (when (string? url) (host-of url))]
      (some (fn [domain]
              (or (= host domain) (str/ends-with? host (str "." domain))))
        youtube-domains))))
