(ns freememo.audio-clip-test
  "Asserts the two Anki boundaries `freememo.audio-clip` owns.

   The invariant worth the most here is the last one: a clip that nobody edited
   must mask to the same string on both sides. Without it, push rewrites the
   clip's spelling — `<audio>` becomes `[sound:…]` — and every reconcile after
   that reads the difference as an Anki-side edit, forever, on every card
   holding a clip. `anki-fields/mask-img-tags` exists for the weaker version of
   the same problem, where only the src changes.

   Pure: no database, no Quill, no missionary. `rewrite-media-srcs` is spelled
   out here as one `str/replace` rather than required from
   `freememo.anki-sync-helpers`, so this suite stays loadable on its own."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.audio-clip :as clip]))

(def ^:private uploaded
  "What push does to a src before `stored->anki-html` sees it: the media id
   becomes the filename `fetch-and-store-media!` stored in Anki."
  (fn [html] (str/replace html #"/api/media/(\d+)" "$1.mp3")))

(deftest reads-one-clip
  (testing "no clip"
    (is (= [] (clip/clip-tags "<p>a</p>")))
    (is (= [] (clip/clip-tags nil)))
    (is (nil? (clip/media-id nil)))
    (is (nil? (clip/clip-src "<p>a</p>"))))
  (testing "one clip, either quoting style and either closing form"
    (is (= ["<audio controls src=\"/api/media/7\"></audio>"]
          (clip/clip-tags "<p>a</p><audio controls src=\"/api/media/7\"></audio>")))
    (is (= "/api/media/7" (clip/clip-src "<audio src=\"/api/media/7\"></audio>")))
    (is (= "/api/media/7" (clip/clip-src "<audio src='/api/media/7'>")))
    (is (= "7" (clip/media-id "<audio controls loop src=\"/api/media/7\"></audio>"))))
  (testing "two clips keep document order"
    (is (= ["<audio src=\"/api/media/1\"></audio>" "<audio src=\"/api/media/2\"></audio>"]
          (clip/clip-tags (str "<audio src=\"/api/media/1\"></audio>x"
                            "<audio src=\"/api/media/2\"></audio>"))))))

(deftest push-writes-sound-tokens
  (testing "a field with no clip is untouched"
    (is (= "<p>a</p>" (clip/stored->anki-html "<p>a</p>")))
    (is (= "" (clip/stored->anki-html "")))
    (is (nil? (clip/stored->anki-html nil))))
  (testing "one clip becomes one token, and the loop attribute does not survive"
    (is (= "<p>a</p>[sound:7.mp3]"
          (clip/stored->anki-html (uploaded "<p>a</p><audio controls loop src=\"/api/media/7\"></audio>")))))
  (testing "two clips become two tokens"
    (is (= "[sound:1.mp3]x[sound:2.mp3]"
          (clip/stored->anki-html
            (uploaded "<audio src=\"/api/media/1\"></audio>x<audio src=\"/api/media/2\"></audio>")))))
  (testing "a failed upload leaves /api/media in the src, and falls back to <id>.mp3"
    (is (= "[sound:7.mp3]"
          (clip/stored->anki-html "<audio controls src=\"/api/media/7\"></audio>"))))
  (testing "a clip with no src is dropped, never written as an empty token"
    (is (= "<p>a</p>" (clip/stored->anki-html "<p>a</p><audio controls></audio>")))))

(deftest mask-covers-both-spellings
  (testing "the local element and the Anki token mask to the same string"
    (let [local "<p>a</p><audio controls loop src=\"/api/media/7\"></audio>"
          anki (clip/stored->anki-html (uploaded local))]
      (is (= (clip/mask-clips local) (clip/mask-clips anki)))))
  (testing "a real edit beside the clip still shows"
    (let [local "<p>a</p><audio src=\"/api/media/7\"></audio>"
          edited "<p>a and b</p>[sound:7.mp3]"]
      (is (not= (clip/mask-clips local) (clip/mask-clips edited)))))
  (testing "the loop setting is invisible to the comparison, because Anki cannot hold it"
    (is (= (clip/mask-clips "<audio controls src=\"/api/media/7\"></audio>")
          (clip/mask-clips "<audio controls loop src=\"/api/media/7\"></audio>"))))
  (testing "no clip means no change"
    (is (= "<p>a</p>" (clip/mask-clips "<p>a</p>")))))

(deftest restore-keeps-local-clips
  (testing "the local element replaces the Anki token"
    (is (= "<p>b</p><audio controls loop src=\"/api/media/7\"></audio>"
          (clip/restore-clip-tags
            "<p>b</p>[sound:7.mp3]"
            "<p>a</p><audio controls loop src=\"/api/media/7\"></audio>"))))
  (testing "positional, so two clips keep their order"
    (is (= "<audio src=\"/api/media/1\"></audio>|<audio src=\"/api/media/2\"></audio>"
          (clip/restore-clip-tags
            "[sound:1.mp3]|[sound:2.mp3]"
            "<audio src=\"/api/media/1\"></audio>x<audio src=\"/api/media/2\"></audio>"))))
  (testing "a token past the local count keeps its Anki form — only Anki can resolve it"
    (is (= "<audio src=\"/api/media/1\"></audio>|[sound:9.mp3]"
          (clip/restore-clip-tags
            "[sound:1.mp3]|[sound:9.mp3]"
            "<audio src=\"/api/media/1\"></audio>"))))
  (testing "a local clip past the Anki count is dropped — that deletion is the pull"
    (is (= "<audio src=\"/api/media/1\"></audio>"
          (clip/restore-clip-tags
            "[sound:1.mp3]"
            "<audio src=\"/api/media/1\"></audio><audio src=\"/api/media/2\"></audio>"))))
  (testing "no clip on either side returns the Anki text unchanged"
    (is (= "<p>b</p>" (clip/restore-clip-tags "<p>b</p>" "<p>a</p>")))
    (is (= "" (clip/restore-clip-tags nil nil)))))

(deftest round-trip-of-an-unedited-card
  (testing "push, then pull: the write is byte-identical to what was stored"
    (let [local "<p>Maqsum</p><audio controls loop src=\"/api/media/412\"></audio>"
          anki (clip/stored->anki-html (uploaded local))]
      (is (= "<p>Maqsum</p>[sound:412.mp3]" anki))
      (is (= (clip/mask-clips anki) (clip/mask-clips local))
        "masked equal, so the reconcile reports no change")
      (is (= local (clip/restore-clip-tags anki local))
        "and if it were written anyway, the local clip survives it"))))
