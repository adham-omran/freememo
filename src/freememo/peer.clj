(ns freememo.peer
  "Which side of row sync this JVM is (plans/electron-local-db-sync.md).

   The Electron desktop app starts its bundled JVM with FREEMEMO_PEER=desktop.
   Every other process, freememo.net and self-host alike, is the server peer.
   The role is fixed for the life of the process, so it is read once.

   The Local Install is a desktop peer that its own launcher starts
   (docs/adr/0001-jvm-launcher-replaces-electron.md). The launcher sets
   FREEMEMO_LOCAL_INSTALL=true through `freememo.launch-env`."
  (:require [freememo.launch-env :as launch-env]))

(def role
  "`:desktop` or `:server`."
  (if (= "desktop" (some-> (launch-env/getenv "FREEMEMO_PEER") .trim)) :desktop :server))

(def ^:private local-install
  (= "true" (some-> (launch-env/getenv "FREEMEMO_LOCAL_INSTALL") .trim)))

(defn desktop?
  "True in the JVM that the desktop app runs against its local database."
  []
  (= role :desktop))

(defn local-install?
  "True in the JVM that the Local Install launcher started. It shows the
   \"Local\" mark in the tab title and in the nav rail."
  []
  (and local-install (desktop?)))
