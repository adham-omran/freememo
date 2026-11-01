(ns freememo.youtube-url-test
  "The YouTube URL parser (plans/youtube-kind.md 2.2). Pure, no database."
  (:require [clojure.test :refer [deftest is testing are]]
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
