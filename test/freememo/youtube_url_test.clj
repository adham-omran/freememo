(ns freememo.youtube-url-test
  "Which addresses are YouTube, which video one names, and which ones web
   import refuses (plans/youtube-kind.md 2.2). Pure, no database."
  (:require [clojure.test :refer [deftest is testing are]]
            [freememo.import-modal :as import-modal]
            [freememo.youtube-url :as yt]))

(def ^:private id "dQw4w9WgXcQ")

(deftest every-accepted-form-gives-its-video-id
  (are [url] (= id (:video-id (yt/parse url)))
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
    "https://youtube.com/watch?v=dQw4w9WgXcQ"
    "http://m.youtube.com/watch?v=dQw4w9WgXcQ"
    "https://music.youtube.com/watch?v=dQw4w9WgXcQ"
    "www.youtube.com/watch?v=dQw4w9WgXcQ"
    "https://www.youtube.com/watch?feature=share&v=dQw4w9WgXcQ"
    "https://youtu.be/dQw4w9WgXcQ"
    "https://youtu.be/dQw4w9WgXcQ?si=abc"
    "https://www.youtube.com/shorts/dQw4w9WgXcQ"
    "https://www.youtube.com/embed/dQw4w9WgXcQ"
    "https://www.youtube-nocookie.com/embed/dQw4w9WgXcQ"
    "  https://WWW.YOUTUBE.COM/watch?v=dQw4w9WgXcQ  "))

(deftest a-playlist-url-is-its-video-or-nothing
  (testing "list= with v= imports the video alone"
    (is (= id (:video-id (yt/parse "https://www.youtube.com/watch?v=dQw4w9WgXcQ&list=PL123")))))
  (testing "list= alone names no video"
    (is (nil? (yt/parse "https://www.youtube.com/playlist?list=PL123")))))

(deftest a-url-that-names-no-video-is-nil
  (are [url] (nil? (yt/parse url))
    nil
    ""
    "https://en.wikipedia.org/wiki/YouTube"
    "https://www.youtube.com/"
    "https://www.youtube.com/@channel"
    "https://www.youtube.com/watch?v=short"
    "https://www.youtube.com/watch?v=dQw4w9WgXcQextra"
    "https://notyoutube.com/watch?v=dQw4w9WgXcQ"
    "https://youtu.be/"
    "https://www.youtube.com/live/dQw4w9WgXcQ"))

(deftest the-start-time-is-read-in-ms
  (are [url ms] (= ms (:start-ms (yt/parse url)))
    "https://youtu.be/dQw4w9WgXcQ?t=90" 90000
    "https://youtu.be/dQw4w9WgXcQ?t=90s" 90000
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=1m30s" 90000
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=1h2m3s" 3723000
    "https://www.youtube.com/embed/dQw4w9WgXcQ?start=42" 42000
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ#t=15" 15000
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=0" nil
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=soon" nil
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ" nil))

(deftest the-watch-url-is-canonical
  (is (= "https://www.youtube.com/watch?v=dQw4w9WgXcQ" (yt/watch-url id)))
  (testing "a start rounds down to whole seconds, which is what t= takes"
    (is (= "https://www.youtube.com/watch?v=dQw4w9WgXcQ&t=12s" (yt/watch-url id 12999))))
  (is (= "https://www.youtube.com/watch?v=dQw4w9WgXcQ" (yt/watch-url id 0))))

(deftest youtube-hosts-match-for-any-path
  (doseq [url ["https://www.youtube.com/watch?v=dQw4w9WgXcQ"
               "https://youtube.com/shorts/abc"
               "https://m.youtube.com/watch?v=abc"
               "https://music.youtube.com/playlist?list=x"
               "https://youtu.be/dQw4w9WgXcQ"
               "https://www.youtube-nocookie.com/embed/abc"
               "HTTPS://WWW.YouTube.COM/watch?v=abc"
               "http://youtube.com:443/watch?v=abc"
               "www.youtube.com/watch?v=abc"
               "  https://youtu.be/abc  "
               "https://user@youtube.com/watch?v=abc"
               "https://www.youtube.com./watch?v=abc"
               "youtube.com"
               "//youtu.be/abc"]]
    (testing url
      (is (true? (yt/youtube-url? url))))))

(deftest other-hosts-do-not-match
  (doseq [url ["https://notyoutube.com/watch?v=abc"
               "https://youtube.com.evil.net/watch?v=abc"
               "https://en.wikipedia.org/wiki/YouTube"
               "https://example.com/?next=https://youtube.com"
               "https://evil.net/youtube.com"
               ""
               "   "
               nil]]
    (testing (pr-str url)
      (is (false? (yt/youtube-url? url))))))

(deftest only-a-youtube-page-that-names-no-video-is-refused
  (are [url] (true? (yt/non-video-youtube-url? url))
    "https://www.youtube.com/"
    "https://www.youtube.com/@channel"
    "https://www.youtube.com/playlist?list=PL123"
    "https://youtu.be/abc")
  (are [url] (false? (yt/non-video-youtube-url? url))
    "https://www.youtube.com/watch?v=dQw4w9WgXcQ"
    "https://youtu.be/dQw4w9WgXcQ?t=90"
    "https://en.wikipedia.org/wiki/YouTube"
    nil))

(deftest the-import-modal-refuses-locally-in-the-server-shape
  (is (= {:ok false :error yt/rejection-message}
        (import-modal/refused-url-result "https://www.youtube.com/@channel")))
  (testing "a video link goes to the server, which imports it"
    (is (nil? (import-modal/refused-url-result "https://youtu.be/dQw4w9WgXcQ"))))
  (is (nil? (import-modal/refused-url-result "https://en.wikipedia.org/wiki/YouTube"))))
