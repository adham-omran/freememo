(ns freememo.local-login
  "Start at login for a Local Install (plans/local-install/issues/21).

   macOS: a LaunchAgent plist in `~/Library/LaunchAgents`. Windows: a Run
   entry under `HKCU`. Both start the installed app (the jpackage launcher,
   found through the `jpackage.app-path` system property) with `--no-tab`, so
   a login opens no browser tab. Linux has no item.

   The data builders (`launch-agent-xml`, `run-command`, `reg-add-args`) are
   pure. `enable!`, `disable!` and `enabled?` write the file or the registry."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [freememo.local-log :refer [log]]))

(def no-tab-arg "--no-tab")
(def agent-label "net.freememo.local")
(def run-value-name "FreeMemoLocal")
(def run-key "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Run")

(defn os-kind
  "Post: `:mac`, `:windows` or `:other`."
  ([] (os-kind (System/getProperty "os.name" "")))
  ([os-name]
   (let [os (str/lower-case os-name)]
     (cond (str/includes? os "mac") :mac
           (str/includes? os "win") :windows
           :else :other))))

(defn installed-executable
  "Post: the path of the installed launcher, or nil in dev and tests.
         jpackage sets `jpackage.app-path` in the launcher it generates."
  []
  (let [p (System/getProperty "jpackage.app-path")]
    (when (and p (not (str/blank? p)) (.isFile (io/file p))) p)))

(defn unavailable-reason
  "Post: nil when the item can work, else a short reason for the disabled item."
  [kind exe]
  (cond (= :other kind) "Start at login (macOS and Windows only)"
        (nil? exe) "Start at login (installed app only)"))

(defn- xml-escape [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;") (str/replace ">" "&gt;")))

(defn launch-agent-path
  "Post: the plist file under the home directory `home`."
  [home]
  (io/file home "Library" "LaunchAgents" (str agent-label ".plist")))

(defn launch-agent-xml
  "Post: the LaunchAgent plist text that starts `exe` with the no-tab argument
         when the user logs in."
  [exe]
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
    "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">\n"
    "<plist version=\"1.0\">\n<dict>\n"
    "  <key>Label</key>\n  <string>" agent-label "</string>\n"
    "  <key>ProgramArguments</key>\n  <array>\n"
    "    <string>" (xml-escape exe) "</string>\n"
    "    <string>" no-tab-arg "</string>\n"
    "  </array>\n"
    "  <key>RunAtLoad</key>\n  <true/>\n"
    "</dict>\n</plist>\n"))

(defn run-command
  "Post: the Run entry text: the quoted program, then the no-tab argument."
  [exe]
  (str "\"" exe "\" " no-tab-arg))

(defn reg-add-args [exe]
  ["reg" "add" run-key "/v" run-value-name "/t" "REG_SZ" "/d" (run-command exe) "/f"])

(def reg-delete-args ["reg" "delete" run-key "/v" run-value-name "/f"])
(def reg-query-args ["reg" "query" run-key "/v" run-value-name])

(defn- sh-ok? [args]
  (let [p (.start (doto (ProcessBuilder. ^java.util.List args) (.redirectErrorStream true)))]
    (.readAllBytes (.getInputStream p))
    (zero? (.waitFor p))))

(defn enabled? [kind]
  (try
    (case kind
      :mac (.isFile (launch-agent-path (System/getProperty "user.home")))
      :windows (sh-ok? reg-query-args)
      false)
    (catch Exception _ false)))

(defn enable! [kind exe]
  (case kind
    :mac (let [f (launch-agent-path (System/getProperty "user.home"))]
           (io/make-parents f)
           (spit f (launch-agent-xml exe)))
    :windows (when-not (sh-ok? (reg-add-args exe)) (throw (ex-info "cannot write the Run entry" {})))))

(defn disable! [kind]
  (case kind
    :mac (io/delete-file (launch-agent-path (System/getProperty "user.home")) true)
    :windows (sh-ok? reg-delete-args)))

(defn login-item
  "Post: `{:label :enabled? :checked? :toggle!}` for the tray, or nil on Linux.
         With no reason to disable, `toggle!` flips the setting. Never throws."
  []
  (let [kind (os-kind)
        exe (installed-executable)
        reason (unavailable-reason kind exe)]
    (cond
      (= :other kind) nil
      reason {:label reason :enabled? false :checked? false :toggle! (fn [])}
      :else {:label "Start at login" :enabled? true :checked? (enabled? kind)
             :toggle! (fn []
                        (try (if (enabled? kind) (disable! kind) (enable! kind exe))
                             (catch Exception e (log "start at login: " (ex-message e)))))})))
