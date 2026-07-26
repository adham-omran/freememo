(ns user) ; Under :dev alias, automatically load 'dev so the REPL is ready to go with zero interaction

;; NOTE: this file runs inside `clojure.lang.RT/doInit`, which holds the RT.class
;; monitor — no other thread can run Clojure here (see src-dev/dev_stack.clj).
;; The app's Electric namespaces are therefore NOT loaded here; `dev/-main`
;; loads them on a big-stack thread. At a bare REPL, call `(dev/load-app-namespaces!)`.

(print "[user] loading dev... ") (flush)
(require 'dev) ; jetty 10+ – the default
;; (require '[dev-jetty9 :as dev]) ; require :jetty9 alias
(println "Ready.")