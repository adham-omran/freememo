(ns freememo.local-install-harness-test
  "The harness never starts a JVM from stale classes: the compiled classes live
   in a directory named by the hash of the sources (`source-key`)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [freememo.local-install-harness :as h]))

(deftest a-source-change-gives-another-class-directory
  (let [dir (h/temp-dir "li-key")
        src (io/file dir "src")]
    (try
      (.mkdirs src)
      (spit (io/file src "a.clj") "(ns a)")
      (let [k0 (h/source-key [(.getPath src)])]
        (is (= k0 (h/source-key [(.getPath src)])) "the same sources give the same key")
        (spit (io/file src "a.clj") "(ns a) 1")
        (let [k1 (h/source-key [(.getPath src)])]
          (is (not= k0 k1) "an edit changes the key")
          (spit (io/file src "b.clj") "(ns b)")
          (is (not= k1 (h/source-key [(.getPath src)])) "a new file changes the key")))
      (finally (h/delete-tree! dir)))))
