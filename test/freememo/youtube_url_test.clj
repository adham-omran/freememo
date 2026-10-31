(ns freememo.youtube-url-test
  "Which addresses web import refuses as YouTube, and the Import modal's
   local refusal. Needs no database."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.import-modal :as import-modal]
            [freememo.youtube-url :as yt]))

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

(deftest the-import-modal-refuses-locally-in-the-server-shape
  (is (= {:ok false :error yt/rejection-message}
        (import-modal/refused-url-result "https://youtu.be/abc")))
  (is (nil? (import-modal/refused-url-result "https://en.wikipedia.org/wiki/YouTube"))))
