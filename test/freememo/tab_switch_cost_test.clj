(ns freememo.tab-switch-cost-test
  "Contracts for the cost of a tab switch on a slow tablet
   (plans/low-end-tablet-performance/issues/14-electric-mount-cost.md).

   The cost needs a slow device to measure. The tablet probe is
   `playwright/tablet-rig-tabs.mjs`. These tests keep the code shapes the fix
   depends on: Main keeps the Learn page mounted after its first visit, a
   hidden Learn page reads no live counter, and Main's navigate! is one value
   for the life of the shell."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [freememo.learn-page :as learn-page]
   [freememo.navigation :as nav]))

(deftest path->tab-reads-the-first-path-segment
  (is (= :library (nav/path->tab "/library/documents")))
  (is (= :learn (nav/path->tab "/learn")))
  (is (= :viewer (nav/path->tab "/viewer/topic/42/3")))
  (is (= :help (nav/path->tab "/help/keyboard-shortcuts")))
  (testing "the root and an absent path are Home, as in Main's active-tab"
    (is (= :home (nav/path->tab "/")))
    (is (= :home (nav/path->tab "")))
    (is (= :home (nav/path->tab nil)))))

(defn- navigate-fn-source
  "The text of Main's navigate! fn, from its binding to the end of its last arity."
  []
  (let [source (slurp "src/freememo/main.cljc")
        start (str/index-of source "navigate! (fn")
        end (str/index-of source "(reset! !nav-cmd {:route (nav->route tab nav)" start)]
    (assert (and start end) "Main no longer binds navigate! as (fn …)")
    (subs source start end)))

(deftest navigate!-closes-over-no-reactive-value
  (testing "a closure over active-tab was a new value on every switch, so every
            always-mounted consumer of navigate! ran again"
    (let [body (navigate-fn-source)]
      (is (not (str/includes? body "active-tab")))
      (is (not (re-find #"\br/route\b" body)))
      (is (str/includes? body "(nav/current-tab)")))))

(deftest keep-tab!-remembers-only-kept-tabs
  (let [!kept (atom #{})]
    (is (= #{} (nav/keep-tab! !kept :library)))
    (is (= #{:learn} (nav/keep-tab! !kept :learn)))
    (testing "leaving a kept tab keeps it"
      (is (= #{:learn} (nav/keep-tab! !kept :help)))
      (is (= #{:learn} @!kept)))))

(deftest main-keeps-the-learn-page-mounted
  (let [source (slurp "src/freememo/main.cljc")]
    (testing "a case around dom/main remounts it, and the kept page with it, on every switch"
      (is (not (str/includes? source "(case (route-timing/mark-shell!")))
      (is (str/includes? source "(route-timing/mark-shell! active-tab)))")))
    (testing "Learn mounts under the kept set, not under the active tab"
      (is (str/includes? source "(when (contains? kept :learn)"))
      (is (str/includes? source "(LearnPage user-id navigate! nil shown?)"))
      (is (not (str/includes? source "(when (= active-tab :learn)"))))))

(deftest a-hidden-learn-page-reads-no-live-counter
  (testing "each region reads the held refresh, so a session's reviews do not
            query the six regions behind the session"
    (let [source (slurp "src/freememo/learn_page.cljc")]
      (is (= 2 (count (re-seq #"us/get-atom" source)))
          "only LearnOverview's live sum reads the counters")
      (is (str/includes? source "(held-revision shown? held live)")
          "an Electric `if` here re-emits refresh on every hide and show")))
  (testing "no region e/fn closes over shown?: an e/fn over a changing value
            remounts the region's whole loaded body on every hide and show"
    (let [source (slurp "src/freememo/learn_page.cljc")]
      (is (str/includes? source "(e/defn QueueTable [revision refresh user-id navigate!]"))
      (is (not (str/includes? (subs source 0 (str/index-of source "(defn held-revision"))
                                "shown?"))
          "every region is defined above held-revision and LearnOverview, so none may read shown?"))))

(deftest held-revision-holds-while-hidden
  (is (= 7 (learn-page/held-revision true nil 7)))
  (is (= 7 (learn-page/held-revision false nil 7)) "nothing held yet: read live")
  (is (= 9 (learn-page/held-revision true 7 9)))
  (is (= 7 (learn-page/held-revision false 7 9)) "hidden: keep the last value"))

(deftest a-hidden-kept-page-releases-its-focus-target
  (let [main (slurp "src/freememo/main.cljc")
        surface (slurp "src/freememo/context_menu_surface.cljc")
        start (str/index-of main "(when (contains? kept :learn)")
        end (str/index-of main "(when (= active-tab :schedule)" start)
        body (subs main start end)]
    (testing "the hide releases focus from the page's own node, as the last form"
      (is (str/includes? body "(menu-surface/release-focus-within! dom/node)"))
      (is (< (str/index-of body "(LearnPage") (str/index-of body "release-focus-within!"))))
    (testing "the release is a plain fn in the surface ns, so a second kept page reuses it"
      (is (str/includes? surface "(defn release-focus-within!"))
      (is (str/includes? surface "bus/retract-focus-target-where!")))))
