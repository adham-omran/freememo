(ns freememo.local-log
  "The one-line log of the Local Install launcher namespaces. It goes to
   stdout, as before. The launcher markers that tests read
   (`FREEMEMO_READY`, `FREEMEMO_OPEN_URL`) are plain `println`s in
   `freememo.local-install`, not here.")

(defn log [& parts]
  (println (apply str "[local-install] " parts))
  (flush))
