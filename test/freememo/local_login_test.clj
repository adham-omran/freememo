(ns freememo.local-login-test
  "Start at login as pure data: the plist, the Run entry, the reasons."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [freememo.local-install :as install]
            [freememo.local-login :as login]
            [freememo.local-tray :as tray]))

(deftest the-launch-agent-starts-the-installed-app-with-no-tab
  (let [xml (login/launch-agent-xml "/Applications/FreeMemo Local.app/Contents/MacOS/FreeMemo Local")]
    (is (str/includes? xml "<string>/Applications/FreeMemo Local.app/Contents/MacOS/FreeMemo Local</string>\n    <string>--no-tab</string>"))
    (is (str/includes? xml "<key>RunAtLoad</key>\n  <true/>"))
    (is (str/includes? xml "<string>net.freememo.local</string>")))
  (is (str/includes? (login/launch-agent-xml "/a&b/<x>") "/a&amp;b/&lt;x&gt;"))
  (is (= "/Users/x/Library/LaunchAgents/net.freememo.local.plist"
        (str (login/launch-agent-path "/Users/x")))))

(deftest the-run-entry-quotes-the-program-and-adds-no-tab
  (is (= "\"C:\\Program Files\\FreeMemo Local\\FreeMemo Local.exe\" --no-tab"
        (login/run-command "C:\\Program Files\\FreeMemo Local\\FreeMemo Local.exe")))
  (is (= ["reg" "add" "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run" "/v" "FreeMemoLocal"
          "/t" "REG_SZ" "/d" "\"C:\\a b\\x.exe\" --no-tab" "/f"]
        (login/reg-add-args "C:\\a b\\x.exe"))))

(deftest the-item-is-disabled-with-a-reason-when-it-cannot-work
  (is (= :mac (login/os-kind "Mac OS X")))
  (is (= :windows (login/os-kind "Windows 11")))
  (is (= :other (login/os-kind "Linux")))
  (is (= "Start at login (installed app only)" (login/unavailable-reason :mac nil)))
  (is (nil? (login/unavailable-reason :windows "C:\\x.exe")))
  (is (some? (login/unavailable-reason :other "x"))))

(deftest linux-has-no-login-item-and-no-error
  (when (= :other (login/os-kind))
    (is (nil? (login/login-item)))))

(deftest the-template-image-keeps-the-shape-and-drops-the-white
  (let [img (java.awt.image.BufferedImage. 2 1 java.awt.image.BufferedImage/TYPE_INT_ARGB)]
    (.setRGB img 0 0 (unchecked-int 0xff2563eb))
    (.setRGB img 1 0 (unchecked-int 0xffffffff))
    (let [t (tray/template-image img)
          alpha #(bit-and (unsigned-bit-shift-right (.getRGB t % 0) 24) 0xff)]
      (is (> (alpha 0) 200))
      (is (zero? (alpha 1)))
      (is (zero? (bit-and (.getRGB t 0 0) 0xffffff))))))

(deftest the-no-tab-argument-is-parsed-and-the-login-programs-pass-it
  (is (true? (:no-tab (#'install/parse-args ["--no-tab"]))))
  (is (nil? (:no-tab (#'install/parse-args ["--port" "5"]))))
  (is (= login/no-tab-arg "--no-tab")))
