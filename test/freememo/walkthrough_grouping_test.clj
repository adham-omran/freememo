(ns freememo.walkthrough-grouping-test
  "The grouping checks and assembly (plans/commit-range-walkthrough.md §6.5).
   No model. `group!` runs through the real `kg-llm/chat!`, with the HTTP call
   under it stubbed, so its contract — the value under the root key — is the
   one production enforces."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.kg-llm :as llm]
            [freememo.openrouter :as openrouter]
            [freememo.walkthrough-grouping :as g]
            [freememo.walkthrough-model-stub :as stub]
            [freememo.walkthrough-range :as wr]))

(def ^:private a (apply str "aaaaaaa" (repeat 33 "0")))
(def ^:private b (apply str "bbbbbbb" (repeat 33 "0")))

(defn- hunk [sha path index & {:as more}]
  (merge {:commit-sha sha :old-path path :new-path path :index index
          :header "@@ -1 +1 @@" :lines ["+x\n"] :added 1 :removed 0}
    more))

(deftest a-concept-list-must-name-every-commit
  (is (nil? (g/concept-problem {:concepts [{:label "Split hold" :commits ["aaaaaaa"]}
                                           {:label "Rename" :commits ["bbbbbbb"]}]}
              [a b])))
  (is (re-find #"in no concept: bbbbbbb"
        (g/concept-problem {:concepts [{:label "Split hold" :commits ["aaaaaaa"]}]} [a b])))
  (is (re-find #"not in the range"
        (g/concept-problem {:concepts [{:label "X" :commits ["ccccccc" "aaaaaaa" "bbbbbbb"]}]} [a b])))
  (is (re-find #"share a label"
        (g/concept-problem {:concepts [{:label "X" :commits ["aaaaaaa"]} {:label "x " :commits ["bbbbbbb"]}]}
          [a b])))
  (is (re-find #"reserved"
        (g/concept-problem {:concepts [{:label "Noise" :commits ["aaaaaaa" "bbbbbbb"]}]} [a b]))))

(deftest a-concept-follows-what-it-depends-on
  (is (= ["split hold" "rename cycle to loop"]
        (map :label (g/ordered [{:label "rename cycle to loop" :depends_on ["split hold"]}
                                {:label "split hold"}]))))
  (testing "a cycle keeps the model's order"
    (is (= ["x" "y"] (map :label (g/ordered [{:label "x" :depends_on ["y"]}
                                             {:label "y" :depends_on ["x"]}]))))))

(deftest every-hunk-gets-exactly-one-concept
  (let [hunks [(hunk a "ir.zig" 0) (hunk a "ir.zig" 1) (hunk b "vendor/x.c" 0) (hunk b "doc.md" 0)]
        k #(wr/hunk-key (:commit-sha %) (:old-path %) (:new-path %) (:index %))
        noise #{(k (nth hunks 2))}
        {:keys [concepts] placed :hunks}
        (g/assemble [{:label "Split hold" :commits [a]} {:label "Unused" :commits [b]}]
          hunks noise {(k (nth hunks 0)) "Split hold" (k (nth hunks 1)) nil (k (nth hunks 3)) "Split hold"})]
    (is (= 4 (count placed)))
    (is (every? :concept placed))
    (is (= ["Split hold" "Other changes" "Noise"] (map :label concepts))
      "an empty concept is dropped; other and noise come last")
    (is (:noise? (last concepts)))))

(deftest the-passes-run-through-chat-and-bill-once-each
  (let [commits [{:sha a :subject "feat: split hold" :body "WP2.2 of plans/x.md" :parents ["p"]}
                 {:sha b :subject "refactor: rename cycle to loop" :body "" :parents [a]}]
        hunks [(hunk a "plans/x.md" 0) (hunk a "ir.zig" 0) (hunk b "ir.zig" 0)]
        !calls (atom [])
        !bills (atom [])
        r (with-redefs [openrouter/chat-completion!
                        (fn [k body & more]
                          (swap! !calls conj (:content (second (:messages body))))
                          (apply stub/chat-completion! k body more))]
            (g/group! {:chat (fn [system user root-key root-type]
                               (llm/chat! "key" {} system user root-key root-type))
                       :commits commits :hunks hunks :noise #{}
                       :check-cancelled! (fn []) :set-phase! (fn [_])
                       :on-pass (fn [pass cost _] (swap! !bills conj [pass cost]))}))]
    (is (= ["split hold" "rename cycle to loop"] (map :label (:concepts r))) "§6.5.3")
    (is (= [:concepts :classify] (map first @!bills)) "§6.5.6: one charge per pass")
    (is (str/includes? (first @!calls) "Work packages: WP2.2"))
    (is (str/includes? (first @!calls) "Plan diff:"))
    (is (= {"split hold" 2 "rename cycle to loop" 1} (frequencies (map :concept (:hunks r)))))))

(deftest a-cancel-stops-before-the-next-call
  (let [!calls (atom 0)]
    (is (thrown-with-msg? Exception #"cancelled"
          (g/group! {:chat (fn [& _] (swap! !calls inc)
                             {:parsed [{:label "x" :commits ["aaaaaaa"]}] :cost 0.0 :tokens []})
                     :commits [{:sha a :subject "s" :body ""}] :hunks [(hunk a "x" 0)] :noise #{}
                     :check-cancelled! (fn [] (when (pos? @!calls) (throw (ex-info "cancelled" {}))))
                     :set-phase! (fn [_]) :on-pass (fn [& _])})))
    (is (= 1 @!calls))))
