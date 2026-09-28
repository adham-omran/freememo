(ns freememo.saved-prompt-test
  "Saved prompts in `freememo.settings`: the base stamp, the state the Advanced
   view marks, and the length cap. See plans/ocr-math-html-to-tex.md §5.

   Needs no database: the two writes are redefined to record their calls."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.db :as db]
            [freememo.input-check :as input]
            [freememo.settings :as settings]))

(def ^:private digest #'settings/prompt-digest)

(deftest override-state-covers-every-case
  (let [default "Extract all text."]
    (is (= :default (settings/override-state nil nil default)))
    (is (= :edited (settings/override-state "mine" (digest default) default)))
    (is (= :stale (settings/override-state "mine" (digest "an older default") default)))
    (is (= :unknown-base (settings/override-state "mine" nil default)))))

(deftest the-stamp-ignores-line-endings
  (is (= (digest "a\nb") (digest "a\r\nb"))))

(deftest every-default-fits-the-cap
  (doseq [[kind text] {:system (settings/get-default-system-prompt)
                       :ocr (settings/get-default-ocr-prompt)}]
    (is (some? text) (name kind))
    (is (<= (count text) input/prompt-max) (name kind))))

(defn- record-writes
  "Call `f` with the settings writes redefined. Returns the recorded calls."
  [f]
  (let [!calls (atom [])]
    (with-redefs [db/delete-setting (fn [_ k] (swap! !calls conj [:delete k]))
                  db/set-settings! (fn [_ kvs] (swap! !calls conj [:set kvs]))]
      (f))
    @!calls))

(deftest saving-the-default-deletes-the-row
  (let [default (settings/get-default-system-prompt)]
    (is (= [[:delete settings/PROMPT_SYSTEM] [:delete settings/PROMPT_SYSTEM_BASE]]
           (record-writes #(is (= {:success true}
                                  (settings/save-system-prompt 1 default))))))))

(deftest saving-an-edit-stamps-the-current-default
  (testing "an edit of the default system prompt fits the cap (§5.9)"
    (let [default (settings/get-default-system-prompt)
          edited (str default " Be brief.")]
      (is (= [[:set {settings/PROMPT_SYSTEM edited
                     settings/PROMPT_SYSTEM_BASE (digest default)}]]
             (record-writes #(is (= {:success true}
                                    (settings/save-system-prompt 1 edited)))))))))

(deftest an-oversized-prompt-is-refused
  (let [calls (record-writes
                #(is (not (:success (settings/save-ocr-prompt 1 (apply str (repeat (inc input/prompt-max) "x")))))))]
    (is (empty? calls))))
