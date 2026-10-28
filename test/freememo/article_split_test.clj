(ns freememo.article-split-test
  "Article batch contracts. Requires PostgreSQL with the current schema."
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [freememo.account-import :as account-import]
            [freememo.article-split :as article-split]
            [freememo.article-split-identity :as split-identity]
            [freememo.db :as db]
            [freememo.text :as text]
            [freememo.undo :as undo]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [org.jsoup Jsoup]))

(def ^:private row-options {:builder-fn rs/as-unqualified-maps})
(def ^:private headings {:mode :headings})
(def ^:private article "<p>Introduction</p><h2>Parent</h2><p>Own text</p><h3>Child</h3><p>Child text</p>")

(defn- user! []
  (:id (jdbc/execute-one! db/ds
         ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
          (str "article-split-" (random-uuid)) (str (random-uuid) "@example.test")]
         row-options)))

(defn- topic! [user-id parent-id content]
  (:topics/id (db/create-topic! {:user-id user-id :parent-id parent-id
                                :title "Article" :content content})))

(defn- topics [user-id]
  (jdbc/execute! db/ds ["SELECT * FROM topics WHERE user_id = ? ORDER BY id" user-id] row-options))

(defmacro with-user [[user-id] & body]
  `(let [~user-id (user!)]
     (try ~@body
       (finally
         (jdbc/execute! db/ds ["DELETE FROM topics WHERE user_id = ?" ~user-id])
         (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" ~user-id])))))

(defn- request [preview indices]
  {:source-version (:source-version preview) :options headings :selected-indices indices})

(defn- highlighted-request [preview indices]
  (assoc (request preview indices) :highlight-source? true))

(defn- source-content [source-id]
  (:topics/content (db/get-topic source-id)))

(defn- blue-text [html]
  (mapv #(.text %)
    (.select (.body (Jsoup/parseBodyFragment html)) "span[style*=#44C2FF]")))

(deftest preview-and-creation-preserve-source-and-hierarchy
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          bibliography-id (:sources/id (db/create-source!
                                        {:user-id user-id :csl-type "webpage"
                                         :csl {:title "Citation"} :title "Citation"}))
          media-id (:id (jdbc/execute-one! db/ds
                         ["INSERT INTO media (user_id, kind, bytes, mime_type, sha256, byte_size)
                           VALUES (?, 'image', ?, 'image/png', ?, 1) RETURNING id"
                          user-id (byte-array [1]) (apply str (repeat 64 "a"))] row-options))]
      (jdbc/execute! db/ds ["UPDATE topics SET source_id = ? WHERE id = ?" bibliography-id source-id])
      (jdbc/execute! db/ds
        ["INSERT INTO topic_pins (topic_id, media_id, placement) VALUES (?, ?, 'front')" source-id media-id])
      (let [before (db/get-topic source-id)
            preview (article-split/preview user-id source-id headings)
            result (article-split/create-topics! user-id source-id (request preview [0 1 2]))
            generated (remove #(= source-id (:id %)) (topics user-id))
            [_ parent child] generated]
        (is (:success preview))
        (is (= 3 (count (:sections preview))))
        (is (= {:success true :created 3 :skipped 0} (dissoc result :undo-id)))
        (is (= source-id (:parent_id parent)))
        (is (= (:id parent) (:parent_id child)))
        (is (every? :is_extract generated))
        (is (every? #(= "basic" (:kind %)) generated))
        (is (= before (db/get-topic source-id)))
        (is (= 3 (count (set (map :source_id generated)))))
        (is (not-any? #(= bibliography-id (:source_id %)) generated))
        (is (= 4 (:n (jdbc/execute-one! db/ds
                         ["SELECT count(*) AS n FROM topic_pins WHERE topic_id = ANY(?)"
                          (long-array (map :id (topics user-id)))] row-options))))
        (jdbc/execute! db/ds ["UPDATE topics SET content = '<p>Edited</p>' WHERE id = ?" (:id child)])
        (is (= {:success true :created 0 :skipped 3 :undo-id nil}
              (article-split/create-topics! user-id source-id (request preview [0 1 2]))))
        (is (= "<p>Edited</p>" (:topics/content (db/get-topic (:id child)))))
        (let [outside-id (topic! user-id (:id child) "<p>Unrelated</p>")]
          (is (:success (undo/undo-entry! user-id (:undo-id result))))
          (is (= #{source-id outside-id} (set (map :id (topics user-id)))))
          (is (= source-id (:topics/parent_id (db/get-topic outside-id))))
          (is (= article (:topics/content (db/get-topic source-id))))
          (is (= 1 (:n (jdbc/execute-one! db/ds
                          ["SELECT count(*) AS n FROM sources WHERE user_id = ?" user-id] row-options))))
          (is (:nothing? (undo/undo-entry! user-id (:undo-id result)))))))))

(deftest selected-descendants-use-nearest-selected-ancestor
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          child-result (article-split/create-topics! user-id source-id (request preview [2]))
          child (last (topics user-id))]
      (is (= 1 (:created child-result)))
      (is (= source-id (:parent_id child)))
      (let [result (article-split/create-topics! user-id source-id (request preview [1 2]))]
        (is (= 1 (:created result)))
        (is (= 1 (:skipped result)))
        (is (= source-id (:topics/parent_id (db/get-topic (:id child))))
          "A later split preserves the existing topic's placement.")))))

(deftest stale-invalid-and-unowned-requests-write-nothing
  (with-user [owner]
    (with-user [stranger]
      (let [source-id (topic! owner nil article)
            preview (article-split/preview owner source-id headings)]
        (is (false? (:success (article-split/preview stranger source-id headings))))
        (is (false? (:success (article-split/create-topics! stranger source-id (request preview [1])))))
        (doseq [indices [[] [99] [nil] ["1"]]]
          (is (false? (:success (article-split/create-topics! owner source-id (request preview indices))))))
        (jdbc/execute! db/ds ["UPDATE topics SET content = '<p>Changed</p>' WHERE id = ?" source-id])
        (is (:stale? (article-split/create-topics! owner source-id (request preview [1]))))
        (is (= 1 (count (topics owner))))
        (is (empty? (db/get-undo-entries owner)))))))

(deftest a-failed-batch-rolls-back-topics-and-references
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          source (:sources/id (db/create-source! {:user-id user-id :csl-type "webpage" :csl {:title "Citation"}}))]
      (jdbc/execute! db/ds ["UPDATE topics SET source_id = ? WHERE id = ?" source source-id])
      (let [preview (article-split/preview user-id source-id headings)
            result (with-redefs [db/insert-undo-entry-raw! (fn [& _] (throw (ex-info "Forced failure" {})))]
                     (article-split/create-topics! user-id source-id (request preview [0 1 2])))]
        (is (false? (:success result)))
        (is (= 1 (count (topics user-id))))
        (is (= 1 (:n (jdbc/execute-one! db/ds
                        ["SELECT count(*) AS n FROM sources WHERE user_id = ?" user-id] row-options))))
        (is (empty? (db/get-undo-entries user-id)))))))

(deftest imported-split-provenance-translates-topic-ids
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          copy-id (topic! user-id source-id "<p>Copy</p>")]
      (#'account-import/link-imported-splits! db/ds
        [{:id 101 :article_split_source_id 100}] {100 source-id 101 copy-id})
      (is (= source-id (:topics/article_split_source_id (db/get-topic copy-id)))))))

(defn- exported-topics [user-id]
  (mapv #(json/parse-string (:row %) true)
    (jdbc/execute! db/ds
      ["SELECT to_jsonb(t)::text AS row FROM topics t WHERE user_id = ?" user-id] row-options)))

(defn- import-topic-rows! [user-id rows media-map]
  (jdbc/with-transaction [tx db/ds]
    (let [result (#'account-import/import-table! tx :topics
                   (#'account-import/ordered-topic-rows rows)
                   (#'account-import/table-columns tx :topics) {} media-map user-id nil)]
      (#'account-import/link-imported-splits! tx rows (:map result))
      (:map result))))

(deftest imported-media-references-preserve-split-identity-in-every-mode
  (doseq [options [{:mode :headings} {:mode :lines} {:mode :blocks}
                   {:mode :splitmark :splitmark "  ~~~[x]  "}]]
    (testing (str "Imported identity for " (:mode options))
      (with-user [owner]
        (with-user [recipient]
          (let [html (str "<p>Intro<img src=\"/api/media/12\"></p><h2>First</h2>"
                      "<p>One</p><p>Two</p><hr><p>~~~[x]</p><h3>Second</h3>"
                      "<p>Three</p><p>Four</p><p>Five</p>")
                source-id (topic! owner nil html)
                preview (article-split/preview owner source-id options)
                indices (mapv :index (:sections preview))
                split (article-split/create-topics! owner source-id
                        {:source-version (:source-version preview) :options options :selected-indices indices})
                edited-id (:id (last (topics owner)))
                edited-html "<p>Edited copy<img src=\"/api/media/12\"></p>"]
            (is (= (count indices) (:created split)))
            (is (every? #(string? (:article_split_provenance %))
                  (filter :article_split_source_id (topics owner))))
            (jdbc/execute! db/ds ["UPDATE topics SET content = ? WHERE id = ?" edited-html edited-id])
            (let [archive (exported-topics owner)
                  topic-map (import-topic-rows! recipient archive {12 9012})
                  imported-id (get topic-map source-id)
                  imported-preview (article-split/preview recipient imported-id options)
                  repeated (article-split/create-topics! recipient imported-id
                             {:source-version (:source-version imported-preview)
                              :options options :selected-indices indices})]
              (is (not= (:source-version preview) (:source-version imported-preview)))
              (is (= {:success true :created 0 :skipped (count indices) :undo-id nil} repeated))
              (is (= "<p>Edited copy<img src=\"/api/media/9012\"></p>"
                    (:topics/content (db/get-topic (get topic-map edited-id)))))
              (is (every? #(= 64 (count (:article_split_key %)))
                    (filter :article_split_source_id (topics recipient)))))))))))

(deftest account-import-keeps-historical-splits-distinct-from-current-source
  (with-user [owner]
    (with-user [recipient]
      (let [source-id (topic! owner nil (str article "<p><img src=\"/api/media/12\"></p>"))
            original-preview (article-split/preview owner source-id headings)
            original-split (article-split/create-topics! owner source-id (request original-preview [0 1 2]))]
        (is (= 3 (:created original-split)))
        (jdbc/execute! db/ds ["UPDATE topics SET content = content || '<p>Source edit</p>' WHERE id = ?" source-id])
        (let [archive (exported-topics owner)
              topic-map (import-topic-rows! recipient archive {12 9012})
              imported-id (get topic-map source-id)
              preview (article-split/preview recipient imported-id headings)
              created (article-split/create-topics! recipient imported-id (request preview [0 1 2]))]
          (is (= 3 (:created created)))
          (is (zero? (:skipped created)))
          (is (= {:success true :created 0 :skipped 3 :undo-id nil}
                (article-split/create-topics! recipient imported-id (request preview [0 1 2]))))
          (doseq [archived-topic (filter :article_split_source_id archive)]
            (let [imported (db/get-topic (get topic-map (:id archived-topic)))]
              (is (= (:article_split_key archived-topic) (:topics/article_split_key imported)))
              (is (= (split-identity/read-provenance (:article_split_provenance archived-topic))
                    (split-identity/read-provenance (:topics/article_split_provenance imported)))))))))))

(deftest split-keys-stay-bounded-through-json-and-reference-rewrites
  (let [version (split-identity/source-version "<p>Stored HTML</p>")
        provenance (split-identity/provenance version
                     {:mode :splitmark :splitmark (apply str (repeat 10000 "x"))} 2)
        roundtrip (split-identity/read-provenance (json/generate-string provenance))
        rebased (split-identity/rebase-source-version roundtrip version "new-version")]
    (is (= 64 (count (split-identity/section-key provenance))))
    (is (= (split-identity/section-key provenance) (split-identity/section-key roundtrip)))
    (is (not= (split-identity/section-key provenance) (split-identity/section-key rebased)))
    (is (= provenance (split-identity/rebase-source-version provenance "other-version" "new-version")))))

(deftest repeated-account-transfers-rebase-only-the-current-split-version
  (with-user [owner]
    (with-user [first-recipient]
      (with-user [second-recipient]
        (let [source-id (topic! owner nil (str article "<p><img src=\"/api/media/12\"></p>"))
              original-preview (article-split/preview owner source-id headings)]
          (is (= 3 (:created (article-split/create-topics! owner source-id (request original-preview [0 1 2])))))
          (jdbc/execute! db/ds ["UPDATE topics SET content = content || '<p>Source edit</p>' WHERE id = ?" source-id])
          (let [current-preview (article-split/preview owner source-id headings)]
            (is (= 3 (:created (article-split/create-topics! owner source-id (request current-preview [0 1 2]))))))
          (let [first-map (import-topic-rows! first-recipient (exported-topics owner) {12 9012})
                first-source-id (get first-map source-id)
                second-map (import-topic-rows! second-recipient (exported-topics first-recipient) {9012 7777})
                second-source-id (get second-map first-source-id)
                preview (article-split/preview second-recipient second-source-id headings)
                historical-version (:source-version original-preview)]
            (is (= {:success true :created 0 :skipped 3 :undo-id nil}
                  (article-split/create-topics! second-recipient second-source-id (request preview [0 1 2]))))
            (is (= 7 (count (topics second-recipient))))
            (is (= {historical-version 3 (:source-version preview) 3}
                  (frequencies
                    (map #(-> (:article_split_provenance %) str (json/parse-string true) :source-version)
                      (filter :article_split_source_id (topics second-recipient))))))))))))

(deftest successful-creation-prunes-undo-after-commit
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          calls (atom [])
          prune db/prune-undo-log!
          result (with-redefs [db/prune-undo-log!
                              (fn [conn owner]
                                (swap! calls conj {:owner owner
                                                   :committed? (= 2 (count (topics owner)))})
                                (prune conn owner))]
                   (article-split/create-topics! user-id source-id (request preview [1])))]
      (is (:success result))
      (is (= 1 (:created result)))
      (is (= [{:owner user-id :committed? true}] @calls))
      (is (some? (db/get-undo-entry (:undo-id result)))))))

(deftest failed-undo-pruning-does-not-change-creation-success
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          result (with-redefs [db/prune-undo-log! (fn [& _] (throw (ex-info "Forced prune failure" {})))]
                   (article-split/create-topics! user-id source-id (request preview [1])))]
      (is (:success result))
      (is (= 1 (:created result)))
      (is (= 2 (count (topics user-id))))
      (is (some? (db/get-undo-entry (:undo-id result)))))))

(deftest concurrent-creation-saves-each-section-once
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          start (promise)
          create (fn [] @start (article-split/create-topics! user-id source-id (request preview [1 2])))
          first-call (future (create))
          second-call (future (create))]
      (deliver start true)
      (let [results [@first-call @second-call]]
        (is (every? :success results))
        (is (= [0 2] (sort (map :created results))))
        (is (= [0 2] (sort (map :skipped results))))
        (is (= 3 (count (topics user-id))))
        (is (= 1 (count (db/get-undo-entries user-id))))))))

(deftest undo-leaves-an-existing-generated-parent
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          first-batch (article-split/create-topics! user-id source-id (request preview [1]))
          parent (last (topics user-id))
          second-batch (article-split/create-topics! user-id source-id (request preview [1 2]))
          child (last (topics user-id))]
      (is (= (:id parent) (:parent_id child)))
      (is (= 1 (:created second-batch)))
      (is (= 1 (:skipped second-batch)))
      (is (:success (undo/undo-entry! user-id (:undo-id second-batch))))
      (is (= #{source-id (:id parent)} (set (map :id (topics user-id)))))
      (is (nil? (:undone_at (db/get-undo-entry (:undo-id first-batch))))))))

(deftest undo-preserves-inherited-ownership-when-a-batch-topic-became-a-root
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          result (article-split/create-topics! user-id source-id (request preview [1]))
          generated-id (:id (last (topics user-id)))
          existing-id (topic! user-id generated-id "<p>Keep</p>")
          grandchild-id (topic! user-id existing-id "<p>Keep this too</p>")
          cards (mapv (fn [topic-id]
                        (jdbc/execute-one! db/ds
                          ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, answer, anki_note_id)
                            VALUES (?, ?, 'basic', ?, 'Keep answer', ?) RETURNING *"
                           topic-id generated-id (str "Keep question " topic-id) (+ 1000000000 topic-id)]
                          row-options))
                  [existing-id grandchild-id])
          version-ids (mapv (fn [card]
                             (:id (jdbc/execute-one! db/ds
                                    ["INSERT INTO card_versions
                                      (root_topic_id, scope_type, scope_id, kind, question, answer)
                                      VALUES (?, 'card', ?, 'basic', 'Old question', 'Old answer') RETURNING id"
                                     generated-id (:id card)] row-options))) cards)
          deleted-card (jdbc/execute-one! db/ds
                         ["INSERT INTO flashcards (topic_id, root_topic_id, kind, question, anki_note_id)
                           VALUES (?, ?, 'basic', 'Batch question', ?) RETURNING *"
                          generated-id generated-id (+ 1000000000 generated-id)] row-options)
          topic-version-id (:id (jdbc/execute-one! db/ds
                                  ["INSERT INTO topic_versions (topic_id, title, content)
                                    VALUES (?, 'Old title', '<p>Old content</p>') RETURNING id"
                                   grandchild-id] row-options))]
      (jdbc/execute! db/ds ["UPDATE topics SET parent_id = NULL WHERE id = ?" generated-id])
      (jdbc/execute! db/ds ["UPDATE topics SET user_id = NULL WHERE id = ?" existing-id])
      (is (:success (undo/undo-entry! user-id (:undo-id result))))
      (is (nil? (:topics/parent_id (db/get-topic existing-id))))
      (is (db/owns-topic? user-id existing-id))
      (is (= "<p>Keep</p>" (:topics/content (db/get-topic existing-id))))
      (is (= "<p>Keep this too</p>" (:topics/content (db/get-topic grandchild-id))))
      (doseq [card cards]
        (let [retained (jdbc/execute-one! db/ds ["SELECT * FROM flashcards WHERE id = ?" (:id card)] row-options)]
          (is (= (:question card) (:question retained)))
          (is (= (:answer card) (:answer retained)))
          (is (= existing-id (:root_topic_id retained)))
          (is (= (:anki_note_id card) (:anki_note_id retained)))))
      (is (= [existing-id existing-id]
            (mapv :root_topic_id (jdbc/execute! db/ds
                                  ["SELECT root_topic_id FROM card_versions WHERE id = ANY(?) ORDER BY id"
                                   (long-array version-ids)] row-options))))
      (is (= "<p>Old content</p>"
            (:content (jdbc/execute-one! db/ds ["SELECT content FROM topic_versions WHERE id = ?" topic-version-id]
                        row-options))))
      (is (nil? (jdbc/execute-one! db/ds ["SELECT id FROM flashcards WHERE id = ?" (:id deleted-card)])))
      (is (= [(:anki_note_id deleted-card)]
            (mapv :anki_note_id (jdbc/execute! db/ds
                                 ["SELECT anki_note_id FROM anki_note_tombstones WHERE user_id = ?" user-id]
                                 row-options)))))))

(deftest optional-source-highlighting-preserves-excluded-sections-and-topic-copies
  (with-user [user-id]
    (let [html (str "<div class='original'>" article "</div>")
          source-id (topic! user-id nil html)
          preview (article-split/preview user-id source-id headings)
          result (article-split/create-topics! user-id source-id (highlighted-request preview [1]))
          generated (last (topics user-id))
          highlighted (source-content source-id)]
      (is (:success result))
      (is (= 1 (:created result)))
      (is (= ["Parent" "Own text"] (blue-text highlighted)))
      (is (= (:content (nth (:sections preview) 1)) (:content generated)))
      (is (empty? (blue-text (:content generated))))
      (is (= html (:source-before-content result)))
      (is (= highlighted (:source-content result)))
      (is (= source-id (:source-topic-id result)))
      (is (= (text/strip-html html)
             (:topics/content_text (db/get-topic source-id))))
      (let [undone (undo/undo-entry! user-id (:undo-id result))]
        (is (:success undone))
        (is (not (:source-highlights-kept? undone)))
        (is (= html (source-content source-id)))
        (is (= [source-id] (mapv :id (topics user-id))))))))

(deftest explicitly-disabled-source-highlighting-keeps-exact-source
  (with-user [user-id]
    (let [html (str "<article class='original'>" article "</article>")
          source-id (topic! user-id nil html)
          before (db/get-topic source-id)
          preview (article-split/preview user-id source-id headings)
          result (article-split/create-topics! user-id source-id
                   (assoc (request preview [1]) :highlight-source? false))]
      (is (= 1 (:created result)))
      (is (= before (db/get-topic source-id)))
      (is (not (contains? result :source-content)))
      (is (not (contains? result :source-before-content)))
      (is (:success (undo/undo-entry! user-id (:undo-id result))))
      (is (= html (source-content source-id))))))

(deftest source-highlighting-marks-only-new-sections-and-repeat-does-not-write
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          initial (article-split/create-topics! user-id source-id (request preview [1]))
          parent-id (:id (last (topics user-id)))
          edited "<p>Edited existing topic</p>"]
      (is (= 1 (:created initial)))
      (jdbc/execute! db/ds ["UPDATE topics SET content = ? WHERE id = ?" edited parent-id])
      (let [result (article-split/create-topics! user-id source-id (highlighted-request preview [1 2]))
            source-after (db/get-topic source-id)
            undo-after (db/get-undo-entries user-id)
            fresh-preview (article-split/preview user-id source-id headings)
            repeated (article-split/create-topics! user-id source-id
                       (highlighted-request fresh-preview [1 2]))]
        (is (= 1 (:created result)))
        (is (= 1 (:skipped result)))
        (is (= ["Child" "Child text"] (blue-text (:topics/content source-after))))
        (is (= edited (:topics/content (db/get-topic parent-id))))
        (is (= {:success true :created 0 :skipped 2 :undo-id nil} repeated))
        (is (= source-after (db/get-topic source-id)))
        (is (= undo-after (db/get-undo-entries user-id)))
        (is (= 3 (count (topics user-id))))))))

(deftest source-highlight-and-undo-rebase-surviving-identities-across-all-modes
  (with-user [user-id]
    (let [html (str "<p>Introduction</p><h2>Parent</h2><p>Own text</p><hr>"
                    "<p>CUT</p><h3>Child</h3><p>Child text</p><p>Last text</p>")
          source-id (topic! user-id nil html)
          options-list [headings {:mode :lines} {:mode :blocks}
                        {:mode :splitmark :splitmark "CUT"}]]
      (doseq [options options-list]
        (let [preview (article-split/preview user-id source-id options)]
          (is (= 1 (:created (article-split/create-topics! user-id source-id
                               (assoc (request preview [0]) :options options)))))))
      (let [existing (filterv :article_split_source_id (topics user-id))
            source-version (split-identity/source-version html)
            edited-id (:id (first existing))
            edited "<p>Keep this edited copy</p>"]
        (jdbc/execute! db/ds ["UPDATE topics SET content = ? WHERE id = ?" edited edited-id])
        (let [preview (article-split/preview user-id source-id headings)
              highlighted (article-split/create-topics! user-id source-id
                            (highlighted-request preview [1]))
              highlighted-version (split-identity/source-version (source-content source-id))]
          (is (= 1 (:created highlighted)))
          (is (not= source-version highlighted-version))
          (doseq [topic existing]
            (let [updated (db/get-topic (:id topic))]
              (is (= highlighted-version
                     (:source-version (split-identity/read-provenance
                                        (:topics/article_split_provenance updated)))))
              (is (not= (:article_split_key topic) (:topics/article_split_key updated)))))
          (doseq [options options-list]
            (let [fresh-preview (article-split/preview user-id source-id options)]
              (is (= {:success true :created 0 :skipped 1 :undo-id nil}
                     (article-split/create-topics! user-id source-id
                       (assoc (highlighted-request fresh-preview [0]) :options options))))))
          (is (:success (undo/undo-entry! user-id (:undo-id highlighted))))
          (is (= html (source-content source-id)))
          (is (= edited (:topics/content (db/get-topic edited-id))))
          (doseq [topic existing]
            (let [restored (db/get-topic (:id topic))]
              (is (= (:article_split_key topic) (:topics/article_split_key restored)))
              (is (= (split-identity/read-provenance (:article_split_provenance topic))
                     (split-identity/read-provenance (:topics/article_split_provenance restored))))))
          (doseq [options options-list]
            (let [restored-preview (article-split/preview user-id source-id options)]
              (is (= {:success true :created 0 :skipped 1 :undo-id nil}
                     (article-split/create-topics! user-id source-id
                       (assoc (request restored-preview [0]) :options options)))))))))))

(deftest source-highlight-undo-preserves-later-source-edits-and-warns
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          preview (article-split/preview user-id source-id headings)
          result (article-split/create-topics! user-id source-id (highlighted-request preview [1]))
          edited (str (source-content source-id) "<p>User added text</p>")]
      (is (= 1 (:created result)))
      (jdbc/execute! db/ds ["UPDATE topics SET content = ? WHERE id = ?" edited source-id])
      (let [undone (undo/undo-entry! user-id (:undo-id result))]
        (is (:success undone))
        (is (true? (:source-highlights-kept? undone)))
        (is (true? (:warning? undone)))
        (is (= edited (source-content source-id)))
        (is (= [source-id] (mapv :id (topics user-id))))))))

(deftest source-highlighting-rejects-read-only-topics-without-partial-writes
  (doseq [kind ["gdoc-snapshot" "poem"]]
    (testing kind
      (with-user [user-id]
        (let [source-id (topic! user-id nil article)]
          (jdbc/execute! db/ds ["UPDATE topics SET kind = ? WHERE id = ?" kind source-id])
          (let [before (db/get-topic source-id)
                preview (article-split/preview user-id source-id headings)
                result (article-split/create-topics! user-id source-id (highlighted-request preview [1]))]
            (is (:success preview))
            (is (false? (:success result)))
            (is (string? (:error result)))
            (is (= before (db/get-topic source-id)))
            (is (= [source-id] (mapv :id (topics user-id))))
            (is (empty? (db/get-undo-entries user-id)))
            (let [without-highlights (article-split/create-topics! user-id source-id (request preview [1]))]
              (is (= 1 (:created without-highlights)))
              (is (= before (db/get-topic source-id))))))))))

(deftest failed-highlighted-batch-rolls-back-source-topics-identities-and-references
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          bibliography-id (:sources/id (db/create-source!
                                         {:user-id user-id :csl-type "webpage"
                                          :csl {:title "Citation"}}))]
      (jdbc/execute! db/ds ["UPDATE topics SET source_id = ? WHERE id = ?" bibliography-id source-id])
      (let [preview (article-split/preview user-id source-id headings)
            initial (article-split/create-topics! user-id source-id (request preview [0]))
            before-source (db/get-topic source-id)
            before-topics (topics user-id)
            before-undo (db/get-undo-entries user-id)
            reached-undo? (atom false)
            result (with-redefs [db/insert-undo-entry-raw!
                                (fn [& _]
                                  (reset! reached-undo? true)
                                  (throw (ex-info "Forced failure" {})))]
                     (article-split/create-topics! user-id source-id (highlighted-request preview [1 2])))]
        (is (= 1 (:created initial)))
        (is @reached-undo?)
        (is (false? (:success result)))
        (is (= before-source (db/get-topic source-id)))
        (is (= before-topics (topics user-id)))
        (is (= before-undo (db/get-undo-entries user-id)))
        (is (= 2 (:n (jdbc/execute-one! db/ds
                         ["SELECT count(*) AS n FROM sources WHERE user_id = ?" user-id] row-options))))))))

(deftest source-highlighting-keeps-historical-source-identities-distinct
  (with-user [user-id]
    (let [source-id (topic! user-id nil article)
          historical-preview (article-split/preview user-id source-id headings)
          historical-result (article-split/create-topics! user-id source-id (request historical-preview [1]))
          historical-topic (last (topics user-id))
          current-html (str article "<p>Later source edit</p>")]
      (is (= 1 (:created historical-result)))
      (jdbc/execute! db/ds ["UPDATE topics SET content = ? WHERE id = ?" current-html source-id])
      (let [current-preview (article-split/preview user-id source-id headings)
            current-result (article-split/create-topics! user-id source-id (request current-preview [0]))
            highlighted-result (article-split/create-topics! user-id source-id
                                 (highlighted-request current-preview [1]))]
        (is (= 1 (:created current-result)))
        (is (= 1 (:created highlighted-result)))
        (is (= (:article_split_key historical-topic)
               (:topics/article_split_key (db/get-topic (:id historical-topic)))))
        (is (= (split-identity/read-provenance (:article_split_provenance historical-topic))
               (split-identity/read-provenance
                 (:topics/article_split_provenance (db/get-topic (:id historical-topic))))))
        (is (:success (undo/undo-entry! user-id (:undo-id highlighted-result))))
        (is (= current-html (source-content source-id)))
        (is (= (:article_split_key historical-topic)
               (:topics/article_split_key (db/get-topic (:id historical-topic)))))
        (let [restored-preview (article-split/preview user-id source-id headings)]
          (is (= {:success true :created 0 :skipped 1 :undo-id nil}
                 (article-split/create-topics! user-id source-id (request restored-preview [0]))))
          (is (= 1 (:created (article-split/create-topics! user-id source-id
                               (request restored-preview [1]))))))))))

(deftest source-highlighting-without-visible-text-keeps-exact-source
  (with-user [user-id]
    (let [html "<img src='/api/media/12' alt='Example'>"
          source-id (topic! user-id nil html)
          before (db/get-topic source-id)
          preview (article-split/preview user-id source-id headings)
          result (article-split/create-topics! user-id source-id (highlighted-request preview [0]))]
      (is (= 1 (:created result)))
      (is (= before (db/get-topic source-id)))
      (is (not (contains? result :source-content)))
      (is (:success (undo/undo-entry! user-id (:undo-id result))))
      (is (= html (source-content source-id))))))
