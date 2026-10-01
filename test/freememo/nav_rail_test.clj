(ns freememo.nav-rail-test
  "The nav rail's pure rules (plans/nav-stage-rail.md): which row a route
   marks, which route Continue remembers, and what Keep open does at each
   width. The pointer and focus rules need a browser; section-0-arrival
   covers them."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.commands :as commands]
   [freememo.nav-rail :as rail]
   [freememo.viewport :as viewport]))

(def routed-tabs
  "Every top-level tab `Main` dispatches, except :credits."
  [:home :viewer :library :search :import :knowledge :graph :quiz :help
   :settings :learn :schedule :curriculum])

(def rail-tabs
  (set (concat [(:tab rail/import-item)]
               (map :tab (apply concat rail/destinations))
               (map :tab rail/utilities))))

(deftest every-route-marks-exactly-its-row
  (testing "1.7.1: each routed tab marks a row the rail draws"
    (doseq [tab routed-tabs]
      (is (contains? rail-tabs (rail/active-item (list (symbol (name tab)))))
        (str tab " marks no row"))))
  (testing "the root route is Home"
    (is (= :home (rail/active-item nil)))
    (is (= :home (rail/active-item '()))))
  (testing "a topic and a bare viewer both mark Continue"
    (is (= :viewer (rail/active-item '(viewer topic 42))))
    (is (= :viewer (rail/active-item '(viewer)))))
  (testing "the learn session and subset review mark Learn"
    (is (= :learn (rail/active-item '(viewer learn-session))))
    (is (= :learn (rail/active-item '(viewer subset-review 7)))))
  (testing "Credits marks no row"
    (is (nil? (rail/active-item '(credits))))))

(deftest continue-remembers-only-a-topic-route
  (testing "2.1.1: a /viewer/topic/<id> route gives its id, with or without a page"
    (is (= 42 (rail/viewer-topic-id '(viewer topic 42))))
    (is (= 42 (rail/viewer-topic-id '(viewer topic 42 3)))))
  (testing "D8: a learn session and a subset review never change Continue"
    (is (nil? (rail/viewer-topic-id '(viewer learn-session))))
    (is (nil? (rail/viewer-topic-id '(viewer subset-review 42)))))
  (testing "other routes give nil"
    (is (nil? (rail/viewer-topic-id '(viewer))))
    (is (nil? (rail/viewer-topic-id '(library documents))))
    (is (nil? (rail/viewer-topic-id nil)))))

(deftest keep-open-docks-only-above-900px
  (testing "above 900px a kept rail is docked, whatever the transient open"
    (is (= "kept" (rail/rail-state true false false)))
    (is (= "kept" (rail/rail-state true true false))))
  (testing "D16: at 900px and below Keep open acts like a hover-open rail"
    (is (= "open" (rail/rail-state true true true)))
    (is (= "closed" (rail/rail-state true false true))))
  (testing "without Keep open only the transient open shows the rail"
    (is (= "open" (rail/rail-state false true false)))
    (is (= "closed" (rail/rail-state false false false)))))

(deftest toggling-keep-open
  (let [compact-before @viewport/!compact?]
    (try
      (testing "above 900px: Keep open on docks the rail and clears any hover open"
        (reset! viewport/!compact? false)
        (let [!kept (atom false) !save (atom nil) !open (atom true) !via (atom :hover)]
          (rail/toggle-kept! !kept !save !open !via)
          (is (true? @!kept))
          (is (true? @!save) "the new value is staged for the save Token")
          (is (false? @!open))
          (is (true? (rail/docked? @!kept)))))
      (testing "at 900px and below: Keep open on opens the rail over the page"
        (reset! viewport/!compact? true)
        (let [!kept (atom false) !save (atom nil) !open (atom false) !via (atom nil)]
          (rail/toggle-kept! !kept !save !open !via)
          (is (true? @!kept) "the saved preference still applies to wider windows")
          (is (true? @!open))
          (is (= :toggle @!via) "only an outside press, a navigation or Esc closes it")
          (is (false? (rail/docked? @!kept)))
          (rail/toggle-kept! !kept !save !open !via)
          (is (false? @!kept))
          (is (false? @!open))))
      (finally
        (reset! viewport/!compact? compact-before)))))

(deftest keep-open-names-its-chord
  (testing "1.4.3: the label reads the chord from the registry"
    ;; util/mac-platform? is false on the JVM, so this is the non-macOS form.
    (is (= "Ctrl+." (commands/display-chord :toggle-nav-rail)))))

(deftest help-for-this-page-opens-a-modal-or-falls-back-to-help
  (testing "a page with a Help section shows it over the page"
    (let [!slug (atom nil)
          navigations (atom [])]
      (rail/open-page-help! #(swap! navigations conj %&) "import-file" !slug)
      (is (= "import-file" @!slug))
      (is (empty? @navigations) "the page stays where it is")))
  (testing "a page with no section opens all of Help"
    (let [!slug (atom nil)
          navigations (atom [])]
      (rail/open-page-help! #(swap! navigations conj %&) nil !slug)
      (is (nil? @!slug))
      (is (= [[:help]] @navigations)))))
