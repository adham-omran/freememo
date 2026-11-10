(ns freememo.local-tray
  "The tray icon and the browser opener of a Local Install
   (plans/local-install.md, \"The tray\").

   The menu holds, in order: \"Update available\" (only while the update feed
   shows a newer version), \"Open FreeMemo\", a disabled line with the server
   state, and \"Quit\". \"Start at login\" is ticket 09."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.local-log :refer [log]]))

(defn open-browser! [url]
  (try
    (if (and (java.awt.Desktop/isDesktopSupported)
          (.isSupported (java.awt.Desktop/getDesktop) java.awt.Desktop$Action/BROWSE))
      (.browse (java.awt.Desktop/getDesktop) (java.net.URI. url))
      (let [os (str/lower-case (System/getProperty "os.name" ""))
            cmd (cond (str/includes? os "mac") "open" (str/includes? os "win") "explorer" :else "xdg-open")]
        (.start (ProcessBuilder. ^java.util.List [cmd url]))))
    (catch Exception e (log "cannot open the browser: " (ex-message e)))))

(defn headless? []
  (or (= "true" (System/getenv "FREEMEMO_HEADLESS"))
      (java.awt.GraphicsEnvironment/isHeadless)
      (not (java.awt.SystemTray/isSupported))))

(defn server-label
  "Post: the text of the disabled server-state line of the tray menu."
  [port sync-state]
  (str "Running on port " port
    (case sync-state
      :offline ", offline"
      :syncing ", syncing"
      :signed-out ", signed out"
      :error ", sync failed"
      nil)))

(defn install-tray!
  "Post: the tray icon is in the system tray. Returns its `PopupMenu`.
         `open!` and `quit!` are `(fn [])`. `label` is the server-state text."
  [open! quit! label]
  (let [image (javax.imageio.ImageIO/read (io/resource "public/freememo/icon-192.png"))
        menu (java.awt.PopupMenu.)
        listener (fn [f] (reify java.awt.event.ActionListener
                           (actionPerformed [_ _] (f))))
        item (fn [label f] (doto (java.awt.MenuItem. label) (.addActionListener (listener f))))]
    (.add menu (item "Open FreeMemo" open!))
    (.add menu (doto (java.awt.MenuItem. label) (.setEnabled false)))
    (.add menu (item "Quit" quit!))
    (.add (java.awt.SystemTray/getSystemTray)
      (doto (java.awt.TrayIcon. image "FreeMemo Local" menu)
        (.setImageAutoSize true)
        (.addActionListener (listener open!))))
    menu))

(defn show-update!
  "Post: the tray menu has an \"Update available\" item first when `available`
   is `{:url}`, and none when it is nil. A click opens the installer URL."
  [^java.awt.PopupMenu menu available]
  (let [label "Update available"]
    (doseq [i (reverse (range (.getItemCount menu)))
            :when (= label (.getLabel (.getItem menu i)))]
      (.remove menu (int i)))
    (when-let [url (:url available)]
      (.insert menu
        (doto (java.awt.MenuItem. label)
          (.addActionListener (reify java.awt.event.ActionListener
                                (actionPerformed [_ _] (open-browser! url)))))
        0))))
