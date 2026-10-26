(ns freememo.peer
  "Which side of row sync this JVM is (plans/electron-local-db-sync.md).

   The Electron desktop app starts its bundled JVM with FREEMEMO_PEER=desktop.
   Every other process, freememo.net and self-host alike, is the server peer.
   The role is fixed for the life of the process, so it is read once.")

(def role
  "`:desktop` or `:server`."
  (if (= "desktop" (some-> (System/getenv "FREEMEMO_PEER") .trim)) :desktop :server))

(defn desktop?
  "True in the JVM that the desktop app runs against its local database."
  []
  (= role :desktop))
