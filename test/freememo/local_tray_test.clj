(ns freememo.local-tray-test
  "The tray text of a Local Install. The menu itself needs a desktop session,
   so the Seam 1 suite does not open it."
  (:require [clojure.test :refer [deftest is]]
            [freememo.local-tray :as tray]))

(deftest the-server-line-names-the-port-and-a-sync-trouble
  (is (= "Running on port 47077" (tray/server-label 47077 nil)))
  (is (= "Running on port 47077" (tray/server-label 47077 :idle)))
  (is (= "Running on port 47077, offline" (tray/server-label 47077 :offline)))
  (is (= "Running on port 47077, signed out" (tray/server-label 47077 :signed-out))))
