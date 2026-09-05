(ns freememo.assistant-diagnostics
  "Browser-local assistant state for fatal client reports.

   The state contains identifiers and enums only. It never contains message or
   evidence text. The HTTP failure beacon can read it after Electric stops."
  (:require [freememo.client-state :as client-state]))

(defonce ^:private !state
  (atom {}))

(defn replace-state!
  "Replace the current assistant diagnostic state.

   Pre: state contains identifiers and enums only.
   Post: snapshot returns state on the client. The server stays unchanged.
   Invariant: this function never stores user content."
  [state]
  #?(:cljs (reset! !state state)
     :clj nil)
  nil)

(defn snapshot
  "Return the current client diagnostic state with its page-load session ID."
  []
  #?(:cljs (assoc @!state :session-id client-state/session-id)
     :clj {}))
