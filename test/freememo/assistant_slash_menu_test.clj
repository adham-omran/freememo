(ns freememo.assistant-slash-menu-test
  "The composer's slash-command menu without a browser
   (plans/assistant-context-meter-and-slash-menu.md §2)."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.assistant-slash :as slash]
            [freememo.assistant-slash-menu :as slash-menu]))

(defn- texts [commands] (mapv :text commands))

(deftest suggestions-follow-the-command-name-being-typed
  (is (= ["/compact" "/fork"] (texts (slash/suggestions "/"))))
  (is (= ["/fork"] (texts (slash/suggestions "/f"))))
  (is (= ["/compact"] (texts (slash/suggestions "/compact"))))
  (testing "no match is an empty list, so the menu can tell it from no menu"
    (is (= [] (slash/suggestions "/x"))))
  (testing "no menu once the name is done, or away from the start"
    (is (nil? (slash/suggestions "/fork ")))
    (is (nil? (slash/suggestions "/fork what")))
    (is (nil? (slash/suggestions " /f")))
    (is (nil? (slash/suggestions "a/f")))
    (is (nil? (slash/suggestions "")))
    (is (nil? (slash/suggestions nil)))))

(deftest every-listed-command-is-one-parse-accepts
  (doseq [{:keys [text completion]} slash/commands]
    (testing text
      (is (some? (slash/parse (str completion (when (= completion (str text " ")) "why?")))))
      (is (= text (subs completion 0 (count text)))))))

(deftest a-pick-of-fork-leaves-room-for-the-question
  (is (= "/fork " (:completion (first (slash/suggestions "/fo")))))
  (testing "and closes the menu, because the draft now holds whitespace"
    (is (nil? (slash/suggestions "/fork ")))))

(deftest menu-state
  (testing "open while a command name is being typed"
    (is (= {:open? true :matches (slash/suggestions "/") :selected 0}
          (slash-menu/menu-state "/" nil 0 false))))
  (testing "the selection wraps both ways"
    (is (= 1 (:selected (slash-menu/menu-state "/" nil -1 false))))
    (is (= 0 (:selected (slash-menu/menu-state "/" nil 2 false)))))
  (testing "Escape closes it for that draft only"
    (is (false? (:open? (slash-menu/menu-state "/f" "/f" 0 false))))
    (is (true? (:open? (slash-menu/menu-state "/fo" "/f" 0 false)))))
  (testing "never together with the @ menu"
    (is (false? (:open? (slash-menu/menu-state "/" nil 0 true)))))
  (testing "closed, with nothing selected, on no match"
    (is (= {:open? false :matches [] :selected nil}
          (slash-menu/menu-state "/x" nil 0 false)))
    (is (= {:open? false :matches [] :selected nil}
          (slash-menu/menu-state "hello" nil 0 false)))))

(deftest enter-sends-once-the-draft-is-the-completion
  (let [[compact fork] slash/commands]
    (is (true? (slash-menu/enter-picks? "/c" compact)))
    (is (false? (slash-menu/enter-picks? "/compact" compact)))
    (testing "/fork alone still completes, because it needs a question"
      (is (true? (slash-menu/enter-picks? "/fork" fork))))))
