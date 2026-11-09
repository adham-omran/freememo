(ns freememo.merged-window-install-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [freememo.merged-window :as window]))

(def ^:dynamic *editor-state*)
(def ^:dynamic *dirty*)
(def ^:dynamic *selection*)
(def ^:dynamic *installs*)
(def ^:dynamic *held*)

(def *blocked-range* (atom nil))
(def ^:dynamic *patch-result* nil)
(defn patch-body! [& _] *patch-result*)

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
                       'merged-window/install-allowed? 'window/install-allowed?
                       'merged-window/snapshot-current? 'window/snapshot-current?
                       'patch-overlapping-body! 'patch-body!
                       'merged-window/!blocked-range '*blocked-range*
                       '!held-topic-id '*held*}
                      (cljs-form (cons 'fn (drop 3 form))))))
            :else (recur)))))))

(defmacro with-editor [dirty & body]
  `(binding [*editor-state* (atom {:save-scope {:body-key :first}})
             *dirty* (atom ~dirty) *selection* (atom {:index 10 :length 5})
             *installs* (atom []) *held* (atom nil)]
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

(deftest an-overlap-patch-keeps-the-transformed-selection
  (with-editor {:edit-id "saved"}
    (binding [*patch-result* true]
      (reset! *editor-state* {:save-scope {:body-key :first :merged? true :pdf-root-id 9
                                          :page-numbers (vec (range 1 21))}})
      (let [scope {:body-key :second :merged? true :pdf-root-id 9
                   :page-numbers (vec (range 11 31))}]
        ((actual-installer) "next body" 9 scope "saved")
        (is (= [] @*installs*))
        (is (= scope (:save-scope @*editor-state*)))
        (is (= {:index 10 :length 5} @*selection*))
        (is (nil? @*dirty*))))))

(deftest an-overlap-patch-waits-for-composition-or-upload
  (with-editor {:edit-id "saved"}
    (reset! *editor-state* {:save-scope {:body-key :first :merged? true :pdf-root-id 9
                                        :page-numbers (vec (range 1 21))}})
    ((actual-installer) "next body" 9 {:body-key :second :merged? true :pdf-root-id 9
                                     :page-numbers (vec (range 11 31))} "saved")
    (is (= [] @*installs*))
    (is (= :first (get-in @*editor-state* [:save-scope :body-key])))
    (is (= {:edit-id "saved"} @*dirty*))))

(deftest settlement-retries-do-not-reinstall-an-installed-snapshot
  (with-editor nil
    (reset! *editor-state* {:save-scope {:body-key :first}
                            :installed-body-html "current server snapshot"})
    ((actual-installer) "current server snapshot" 9 {:body-key :first} nil)
    (is (= [] @*installs*))
    (is (= {:index 10 :length 5} @*selection*))))

(deftest a-new-server-snapshot-still-installs-in-the-same-range
  (with-editor nil
    (reset! *editor-state* {:save-scope {:body-key :first}
                            :installed-body-html "old server snapshot"})
    ((actual-installer) "new server snapshot" 9 {:body-key :first} nil)
    (is (= [["new server snapshot" 9 true]] @*installs*))
    (is (= "new server snapshot" (:installed-body-html @*editor-state*)))))

(deftest an-explicit-jump-outside-the-installed-window-reloads-saved-content
  (with-editor {:edit-id "saved"}
    (binding [*patch-result* true]
      (reset! *editor-state* {:save-scope {:body-key :first :merged? true :pdf-root-id 9
                                          :page-numbers (vec (range 1 21))}})
      ((actual-installer) "next body" 9 {:body-key :second :merged? true :pdf-root-id 9
                                        :page-numbers (vec (range 11 31)) :requested-page 27} "saved")
      (is (= [["next body" 9 true]] @*installs*))
      (is (nil? @*selection*)))))

(deftest a-window-snapshot-from-before-the-save-fence-does-not-install
  (with-editor {:edit-id "B"}
    ((actual-installer) "stale body" 9 {:body-key :second :merged? true
                                       :read-request {:fence "A"} :expected-read-request {:fence "B"}} "B")
    (is (= [] @*installs*))
    (is (= :first (get-in @*editor-state* [:save-scope :body-key])))))

;; plans/learn-next-speed/issues/02-editor-stays-mounted-across-next.md: the
;; editor's topic and its save scope change together. A Next keeps the editor
;; mounted, so the old body stays in it until the new one installs, and an edit
;; or a highlight in that window must stay on the old topic.
(deftest an-installed-body-moves-the-editor-topic-with-its-scope
  (with-editor nil
    (reset! *editor-state* {:topic-id 10 :save-scope {:body-key :first :editor-doc-id 10}})
    ((actual-installer) "next body" 20 {:body-key :next :editor-doc-id 20} nil)
    (is (= [["next body" 20 true]] @*installs*))
    (is (= 20 (:topic-id @*editor-state*)))
    (is (= 20 (get-in @*editor-state* [:save-scope :editor-doc-id])))))

(deftest a-refused-body-keeps-the-editor-topic-with-its-scope
  (with-editor {:edit-id "pending"}
    (reset! *editor-state* {:topic-id 10 :save-scope {:body-key :first :editor-doc-id 10}})
    ((actual-installer) "next body" 20 {:body-key :next :editor-doc-id 20} "older")
    (is (= [] @*installs*))
    (is (= 10 (:topic-id @*editor-state*)))
    (is (= 10 (get-in @*editor-state* [:save-scope :editor-doc-id])))))

;; plans/learn-next-speed/issues/10-edit-during-save-window.md: Learn's tap on
;; Next holds the editor read-only for topic 10 until the next body is in.
(deftest the-next-body-ends-the-hold-of-the-learn-tap
  (with-editor nil
    (reset! *editor-state* {:topic-id 10 :save-scope {:body-key :first :editor-doc-id 10}})
    (reset! *held* 10)
    ((actual-installer) "next body" 20 {:body-key :next :editor-doc-id 20} nil)
    (is (= 20 (:topic-id @*editor-state*)))
    (is (nil? @*held*))))

(deftest a-refused-body-keeps-the-hold-of-the-learn-tap
  (with-editor {:edit-id "pending"}
    (reset! *editor-state* {:topic-id 10 :save-scope {:body-key :first :editor-doc-id 10}})
    (reset! *held* 10)
    ((actual-installer) "next body" 20 {:body-key :next :editor-doc-id 20} "older")
    (is (= 10 @*held*))))
