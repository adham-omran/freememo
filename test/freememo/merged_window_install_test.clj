(ns freememo.merged-window-install-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [freememo.merged-window :as window]))

(def ^:dynamic *editor-state*)
(def ^:dynamic *dirty*)
(def ^:dynamic *selection*)
(def ^:dynamic *installs*)

(defn replace-body! [html topic force?]
  (swap! *installs* conj [html topic force?]))

(defn cljs-form [form]
  (walk/postwalk
    (fn [node]
      (if (reader-conditional? node)
        (:cljs (apply hash-map (:form node)))
        node))
    form))

(defn actual-installer []
  (with-open [reader (java.io.PushbackReader. (io/reader "src/freememo/rich_text_editor.cljc"))]
    (binding [*read-eval* false *data-readers* (assoc *data-readers* 'js identity)]
      (loop []
        (let [form (read {:read-cond :preserve :eof nil} reader)]
          (cond
            (nil? form) (throw (ex-info "The editor body installer was not found." {}))
            (and (seq? form) (= 'defn (first form)) (= 'install-body! (second form)))
            (binding [*ns* (the-ns 'freememo.merged-window-install-test)]
              (eval (walk/postwalk-replace
                      {'!editor-state '*editor-state* '!dirty-html '*dirty*
                       '!last-selection '*selection* 'set-content! 'replace-body!
                       'merged-window/install-allowed? 'window/install-allowed?}
                      (cljs-form (cons 'fn (drop 3 form))))))
            :else (recur)))))))

(defmacro with-editor [dirty & body]
  `(binding [*editor-state* (atom {:save-scope {:body-key :first}})
             *dirty* (atom ~dirty) *selection* (atom {:index 10 :length 5})
             *installs* (atom [])]
     ~@body))

(deftest an-unsaved-body-keeps-its-content-and-selection
  (with-editor {:edit-id "pending"}
    ((actual-installer) "new body" 20 {:body-key :next} "older")
    (is (= [] @*installs*))
    (is (= {:index 10 :length 5} @*selection*))
    (is (= {:edit-id "pending"} @*dirty*))))

(deftest an-acknowledged-range-replacement-clears-old-selection-indices
  (with-editor {:edit-id "saved"}
    ((actual-installer) "new body" 20 {:body-key :next} "saved")
    (is (= [["new body" 20 true]] @*installs*))
    (is (= {:body-key :next} (:save-scope @*editor-state*)))
    (is (nil? @*selection*))
    (is (nil? @*dirty*))))

(deftest the-same-saved-body-preserves-its-selection
  (with-editor {:edit-id "saved"}
    ((actual-installer) "old server snapshot" 20 {:body-key :first} "saved")
    (is (= [] @*installs*))
    (is (= {:index 10 :length 5} @*selection*))
    (is (= {:edit-id "saved"} @*dirty*))))
