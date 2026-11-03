(ns freememo.reply-schema-test
  "Every structured AI call states a reply schema that strict mode accepts.
   See plans/structured-reply-schemas.md."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assessment :as assessment]
            [freememo.cards :as cards]
            [freememo.cards-from-facts :as cards-from-facts]
            [freememo.comprehension-ocr :as comprehension-ocr]
            [freememo.kg-extract :as kg-extract]
            [freememo.kg-grade :as kg-grade]
            [freememo.kg-llm :as llm]
            [freememo.kg-questions :as kg-questions]
            [freememo.openrouter :as openrouter]
            [freememo.reply-schema :as rs]
            [freememo.walkthrough-grouping :as grouping]))

(def contracts
  "Every reply contract in src, by the call site that sends it (§3 of the plan).
   A new structured call adds its contract here."
  {"assessment blueprint" assessment/blueprint-reply
   "assessment questions" assessment/questions-reply
   "assessment grade" assessment/grade-reply
   "kg-extract triples" kg-extract/triples-reply
   "kg-extract link" kg-extract/link-reply
   "kg-grade question" kg-grade/question-grade-reply
   "kg-grade card" kg-grade/card-grade-reply
   "kg-questions atomic and synthesis" kg-questions/questions-reply
   "cards-from-facts" cards-from-facts/fact-ids-reply
   "comprehension-ocr" comprehension-ocr/transcript-reply
   "walkthrough concepts" (grouping/replies :concepts)
   "walkthrough summaries" (grouping/replies :summaries)
   "walkthrough assignments" (grouping/replies :assignments)
   "cards basic" (cards/card-replies "basic")
   "cards cloze" (cards/card-replies "cloze")
   "cards overlapping" (cards/card-replies "overlapping")})

(deftest every-contract-is-one-strict-mode-accepts
  ;; The plan's 12 call sites: the card loop has one contract per card type,
  ;; and kg-grade's two calls have one each.
  (is (= 16 (count contracts)))
  (doseq [[site reply] contracts]
    (testing site
      (is (= [] (rs/strict-problems (:schema reply))))
      (is (= [] (rs/strict-problems (rs/wire-schema reply))))
      (is (= reply (rs/require-contract! reply))))))

(deftest every-contract-name-is-distinct
  (is (= (count contracts) (count (set (map :name (vals contracts)))))))

(deftest the-strict-check-names-each-problem
  (is (= ["$: every property must be required"]
         (rs/strict-problems {:type "object" :properties {:a rs/json-string}
                              :required [] :additionalProperties false})))
  (is (= ["$: additionalProperties must be false"]
         (rs/strict-problems {:type "object" :properties {:a rs/json-string} :required ["a"]})))
  (is (= ["$.m: an object without fixed properties is an open map"]
         (rs/strict-problems (rs/strict-object {:m {:type "object"}}))))
  (is (= ["$.xs[]: an object without fixed properties is an open map"]
         (rs/strict-problems (rs/strict-object {:xs (rs/array-of {:type ["object" "null"]})}))))
  (is (= [] (rs/strict-problems (rs/strict-object {:n (rs/nullable (rs/array-of rs/json-integer))})))))

(deftest a-contract-that-strict-mode-would-refuse-fails-when-defined
  (is (= ::rs/invalid-contract
         (:type (ex-data (try (rs/contract "bad" :root {:type "object" :properties {:a rs/json-string}})
                              (catch clojure.lang.ExceptionInfo e e))))))
  (is (thrown? clojure.lang.ExceptionInfo (rs/contract "has space" :root (rs/array-of rs/json-string))))
  (is (thrown? clojure.lang.ExceptionInfo (rs/contract "scalar" :root rs/json-string))))

(def ^:private resolved
  {:api-key "key" :entry {:id "m" :supported-parameters #{:response_format :structured_outputs}}
   :model-fields {:model "m" :response_format {:type "json_object"}}})

(defn- chat-capturing
  "Run kg-llm/chat! with `reply` against a provider that answers `content`.
   Post: [result-or-exception request-bodies]."
  [resolved reply content]
  (let [bodies (atom [])
        result (with-redefs [openrouter/chat-completion!
                             (fn [_ body & _]
                               (swap! bodies conj body)
                               {:choices [{:message {:content content} :finish_reason "stop"}]
                                :usage {:cost 0.01}})]
                 (try (llm/chat! resolved "system" "user" reply)
                      (catch clojure.lang.ExceptionInfo e e)))]
    [result @bodies]))

(deftest chat-sends-the-schema-only-to-a-model-with-structured-outputs
  (let [reply kg-grade/card-grade-reply
        content "{\"grade\":{\"verdict\":\"correct\",\"explanation\":\"Yes.\"}}"]
    (testing "structured outputs"
      (let [[_ [body]] (chat-capturing resolved reply content)]
        (is (= {:type "json_schema"
                :json_schema {:name "card_grade" :strict true :schema (rs/wire-schema reply)}}
               (:response_format body)))))
    (testing "JSON mode only"
      (let [[_ [body]] (chat-capturing (assoc-in resolved [:entry :supported-parameters] #{:response_format})
                         reply content)]
        (is (= {:type "json_object"} (:response_format body)))))))

(deftest chat-reads-a-null-field-as-absent
  (let [[result] (chat-capturing resolved kg-extract/triples-reply
                   "{\"triples\":[{\"s\":\"heart\",\"p\":\"pumps\",\"o\":\"blood\",\"lit\":null}]}")]
    (is (= [{:s "heart" :p "pumps" :o "blood"}] (:parsed result)))))

(deftest chat-refuses-a-call-without-a-contract-before-any-request
  (doseq [reply [nil {:root :grade} {:name "x" :root :grade :schema {:type "object"}}]]
    (let [[result bodies] (chat-capturing resolved reply "{}")]
      (is (= ::rs/invalid-contract (:type (ex-data result))) (pr-str reply))
      (is (empty? bodies))))
  (testing "the old root-key and root-type form no longer exists"
    (is (thrown? clojure.lang.ArityException
          (llm/chat! "key" {} "system" "user" :grade :map)))))

(deftest model-chosen-keys-come-back-as-the-map-each-feature-reads
  (is (= {"aorta" 42 "atrium" "new"}
         (#'kg-extract/link-decisions->map [{:label "aorta" :entity_id 42} {:label "atrium"}])))
  (is (= {"H1" "split hold" "H2" "other"}
         (grouping/assignment-entries->map [{:hunk_id "H1" :label "split hold"}
                                            {:hunk_id "H2" :label "other"}]))))

;; ── Who may call the transport (§6.2) ───────────────────────────────────────

(def ^:private transport-owners
  "The files that may build structured-call fields or call the transport, and why.
   Everything else goes through kg-llm/chat!, which needs a contract."
  {"chat-completion!" #{"kg_llm.clj"    ; the seam
                        "cards.clj"}    ; the card loop sends card-replies through reply-schema/request-fields
   "structured-chat-completion-fields" #{"kg_llm.clj"           ; resolve-model+gate!
                                         "cards.clj"            ; wrapped in request-fields
                                         "comprehension_ocr.clj"}}) ; its fields go to chat!

(defn transport-violations
  "Pre: files is a seq of [file-name text]. Post: [file-name call] for every
   call of openrouter/<call> in a file that transport-owners does not list."
  [files]
  (for [[file text] files
        [call owners] transport-owners
        :when (and (str/includes? text (str "openrouter/" call)) (not (owners file)))]
    [file call]))

(deftest only-the-seam-and-the-card-loop-call-the-transport
  (let [files (for [f (file-seq (io/file "src"))
                    :when (and (.isFile f) (re-find #"\.clj[cs]?$" (.getName f))
                               (not= "openrouter.clj" (.getName f)))]
                [(.getName f) (slurp f)])]
    (is (= [] (vec (transport-violations files)))))
  (testing "a planted call fails the check"
    (is (= [["new_feature.clj" "chat-completion!"]]
           (vec (transport-violations [["new_feature.clj" "(openrouter/chat-completion! k body)"]]))))))
