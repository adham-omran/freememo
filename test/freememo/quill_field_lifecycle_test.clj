(ns freememo.quill-field-lifecycle-test
  "Exercise deferred editor initialization with a controlled clock. No database is required.
   Read the client function and replace browser operations with test doubles."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]))

(def ^:dynamic *clock*)
(def ^:dynamic *ready*)
(def ^:dynamic *events*)

(defn set-timeout! [callback _delay]
  (reset! *clock* callback)
  ::timer)

(defn clear-timeout! [_timer]
  (reset! *clock* nil))

(defn on-ready! [callback]
  (reset! *ready* callback))

(defn init-editor! [container & _]
  (swap! *events* conj :init)
  {:editor {:container container}})

(defn destroy-editor! [state]
  (when (:editor state) (swap! *events* conj :destroy)))

(defn focus! [editor]
  (swap! *events* conj
    (if @(:connected (:container editor)) :focus :detached-focus)))

(defn connected? [container]
  @(:connected container))

(defn- schedule-function []
  (with-open [reader (java.io.PushbackReader. (io/reader "src/freememo/quill_field.cljc"))]
    (binding [*read-eval* false
              *data-readers* (assoc *data-readers* 'js identity)]
      (loop []
        (let [form (read {:read-cond :allow :features #{:cljs} :eof nil} reader)]
          (cond
            (nil? form) (throw (ex-info "Editor scheduler was not found" {}))
            (and (seq? form) (= 'defn (first form)) (= 'schedule-quill-init! (second form)))
            (binding [*ns* (the-ns 'freememo.quill-field-lifecycle-test)]
              (eval
                (walk/postwalk-replace
                  {'js/setTimeout 'set-timeout!
                   'js/clearTimeout 'clear-timeout!
                   'math/on-katex-ready! 'on-ready!
                   'init-quill-field! 'init-editor!
                   'destroy-quill-field! 'destroy-editor!
                   '.focus 'focus!
                   '.-isConnected 'connected?}
                  (walk/postwalk
                    #(if (= 'js (:tag (meta %)))
                       (with-meta % (dissoc (meta %) :tag)) %)
                    (cons 'fn (drop 3 form))))))
            :else (recur)))))))

(defn- run-clock! []
  (when-let [callback @*clock*]
    (reset! *clock* nil)
    (callback)))

(defn- resolve-ready! []
  (when-let [callback @*ready*]
    (reset! *ready* nil)
    (callback true)))

(defmacro with-clock [& body]
  `(binding [*clock* (atom nil) *ready* (atom nil) *events* (atom [])]
     ~@body))

(deftest cancelled-field-never-initializes-or-focuses
  (doseq [after-tick? [false true]]
    (testing (str "Unmount " (if after-tick? "during KaTeX load" "before the timer"))
      (with-clock
        (let [container {:connected (atom true)}
              state (atom nil)
              editor (atom nil)
              stop! ((schedule-function) state container "" "" nil editor false true)]
          (when after-tick? (run-clock!))
          ;; The old scheduler returns a timer, with no cancellation callback.
          (when (fn? stop!) (stop!))
          (reset! (:connected container) false)
          (run-clock!)
          (resolve-ready!)
          (is (= [] @*events*))
          (is (nil? @state))
          (is (nil? @editor)))))))

(deftest detached-field-never-initializes
  (with-clock
    (let [container {:connected (atom false)}]
      ((schedule-function) (atom nil) container "" "" nil (atom nil) false true)
      (run-clock!)
      (resolve-ready!)
      (is (= [] @*events*)))))

(deftest cancelled-field-ignores-ready-before-dom-removal
  (with-clock
    (let [container {:connected (atom true)}
          stop! ((schedule-function) (atom nil) container "" "" nil (atom nil) false true)]
      (run-clock!)
      (stop!)
      (resolve-ready!)
      (is (= [] @*events*)))))

(deftest mounted-field-clears-its-editor-once
  (with-clock
    (let [container {:connected (atom true)}
          editor (atom nil)
          stop! ((schedule-function) (atom nil) container "" "" nil editor false true)]
      (run-clock!)
      (resolve-ready!)
      (stop!)
      (stop!)
      (is (= [:init :focus :destroy] @*events*))
      (is (nil? @editor)))))

(deftest mounted-field-cleans-up-without-clearing-its-replacement
  (with-clock
    (let [container {:connected (atom true)}
          state (atom nil)
          editor (atom nil)
          stop! ((schedule-function) state container "" "" nil editor false true)]
      (run-clock!)
      (resolve-ready!)
      (is (= [:init :focus] @*events*))
      (reset! editor ::replacement)
      (when (fn? stop!) (stop!))
      (is (= [:init :focus :destroy] @*events*))
      (is (nil? @state))
      (is (= ::replacement @editor)))))
