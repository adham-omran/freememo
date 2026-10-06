(ns freememo.walkthrough-test
  "The walkthrough service against PostgreSQL and a fixture repository
   (plans/commit-range-walkthrough.md §6.4–§6.8). The model is stubbed at its
   HTTP call (`walkthrough-model-stub`), so the real JSON parsing runs. Every
   test skips when the database is down."
  (:require [clojure.java.io :as io]
            [freememo.account-export :as account-export]
            [freememo.account-import :as account-import]
            [freememo.archive :as archive]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [freememo.assistant :as assistant]
            [freememo.db :as db]
            [freememo.document-ref :as docref]
            [freememo.question-map :as qm]
            [freememo.git-clone :as git]
            [freememo.git-clone-test :as fixture]
            [freememo.kg-llm :as llm]
            [freememo.openrouter :as openrouter]
            [freememo.walkthrough-model-stub :as stub]
            [freememo.repo-commits :as repo-commits]
            [freememo.repo-import :as repo-import]
            [freememo.user-state :as us]
            [freememo.walkthrough :as wt]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]))

(defn- db-up? []
  (try (jdbc/execute-one! db/ds ["SELECT 1"]) true (catch Exception _ false)))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})

(def commits
  [{"docs/ir-spec.md" "# IR\n\nA cycle op repeats.\n\nEnd.\n"
    "src/ir.zig" "const Op = enum { cycle };\n" :message "start"}
   {"docs/ir-spec.md" "# IR\n\nA cycle op repeats.\n\nA hold op splits.\n\nEnd.\n"
    "src/ir.zig" "const Op = enum { cycle, hold };\n" :message "feat: split hold\n\nWP2.2"}
   {"docs/ir-spec.md" "# IR\n\nA loop op repeats.\n\nA hold op splits.\n\nEnd.\n"
    "src/ir.zig" "const Op = enum { loop, hold };\n" :message "refactor: rename cycle to loop"}])

(defmacro with-walkthrough
  "Bind `user`, `repo-id` and `wid` to a prepared walkthrough over `commits`."
  [[user repo-id wid shas] & body]
  `(if-not (db-up?)
     (is true "PostgreSQL is unavailable; integration assertion skipped")
     (let [{repo# :dir ~shas :shas} (fixture/fixture-repo! commits)
           ~user (:id (jdbc/execute-one! db/ds
                        ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                         (str "walkthrough-test-" (random-uuid)) (str (random-uuid) "@example.test")]
                        opts))
           root# (fixture/temp-dir "clone-root")]
       (try
         (with-redefs [git/protocols #{"https" "file"}
                       git/clone-root (fn [] root#)
                       git/remote-url (fn [_# _#] (str "file://" repo#))
                       llm/resolve-model+gate! (fn [& _#] {:api-key "k" :entry {:id "stub"}
                                                           :model-fields {}})
                       openrouter/chat-completion! stub/chat-completion!]
           (let [staged# (repo-import/stage-repository! ~user {:forge "gitlab" :host "gitlab.example.com"
                                                               :repository "owner/name"})
                 r# (repo-import/import-staged! ~user (:stage-id staged#)
                      {:dirs {"" true} :extensions #{".md" ".zig"}})
                 ~repo-id (:id (jdbc/execute-one! db/ds ["SELECT parent_id AS id FROM topics WHERE id = ?"
                                                         (:topic-id r#)] opts))
                 ~wid (do (#'db/setup-schema-walkthroughs!)
                          (:walkthrough-id (wt/start! ~user ~repo-id (second ~shas) (last ~shas))))]
             (deref (future (loop [n# 0]
                              (when (and (< n# 200)
                                      (= "preparing" (:status (db/get-walkthrough ~user ~wid))))
                                (Thread/sleep 50) (recur (inc n#))))))
             ~@body))
         (finally
           (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" ~user]))))))

(deftest a-walkthrough-is-prepared-into-ordered-steps
  (with-walkthrough [user repo-id wid shas]
    (let [o (wt/overview user wid)]
      (is (= "ready" (:status o)) (str (:error o)))
      (testing "§6.5.3: the split comes before the rename it changes"
        (is (= ["split hold" "rename cycle to loop"] (map :label (:concepts o)))))
      (testing "§6.4.3: every hunk has exactly one row"
        (is (= 4 (count (db/walkthrough-hunks user wid)))))
      (testing "§6.6.3: the main document is the Markdown file"
        (is (= ["docs/ir-spec.md" "src/ir.zig"] (map :path (wt/step-files user wid 2))))))))

(deftest the-canvas-colors-this-steps-changes
  (with-walkthrough [user repo-id wid shas]
    (let [{:keys [html markdown? attributed? ref-spec]} (wt/canvas user wid 2 "docs/ir-spec.md" :previous)]
      (is markdown?)
      (is attributed?)
      (is (= (last shas) (:sha ref-spec)))
      (is (re-find #"<p data-wt=\"wt-c\">A loop op repeats" html) "the rename is colored")
      (is (re-find #"<p data-wt=\"wt-o\">A hold op splits" html) "the split is another step's")
      (is (re-find #"<h1[^>]*>IR</h1>" html) "an unchanged block has no mark"))
    (testing "§6.6.2: Since A colors the earlier step's change too"
      (is (re-find #"data-wt=\"wt-e\">A hold op splits"
            (:html (wt/canvas user wid 2 "docs/ir-spec.md" :start)))))
    (testing "code shows one span per line, its text the source exactly"
      (let [{:keys [html]} (wt/canvas user wid 2 "src/ir.zig" :previous)]
        (is (str/includes? html "<span class=\"wt-line wt-c\">const Op = enum { loop, hold };</span>"))))))

(deftest steps-move-and-the-grouping-can-be-fixed
  (with-walkthrough [user repo-id wid shas]
    (is (= {:ok 1} (wt/understood! user wid)) "the opener moves to step 1")
    (is (= {:ok 2} (wt/understood! user wid)))
    (is (= 2 (:step (wt/overview user wid))) "§6.6.5: the step survives a reload")
    (let [[split rename] (:concepts (wt/overview user wid))
          zig-hunk (:id (first (:hunks (second (wt/step-files user wid 2)))))]
      (testing "§6.5.7.1: a moved hunk shows in its new step"
        (is (:ok (wt/move-hunk! user zig-hunk (:id split))))
        (is (some #(= zig-hunk (:id %)) (mapcat :hunks (wt/step-files user wid 1)))))
      (testing "§6.5.7.2: rename"
        (is (:ok (wt/rename-concept! user (:id rename) "loop")))
        (is (:error (wt/rename-concept! user (:id rename) "split hold")) "a taken name is refused"))
      (testing "§6.5.7.3: merge"
        (is (:ok (wt/merge-concepts! user (:id rename) (:id split))))
        (is (= ["split hold"] (map :label (:concepts (wt/overview user wid)))))
        (is (= 4 (reduce + (map :hunk-count (:concepts (wt/overview user wid))))))))))

(deftest the-tutor-sees-the-step-and-marks-carry-a-version
  (with-walkthrough [user repo-id wid shas]
    (testing "no tail without an open screen"
      (is (nil? (wt/request-tail user repo-id 1))))
    (wt/set-focus! user repo-id {:walkthrough-id wid :position 0})
    (testing "§6.7.1: the opener tests the state at A"
      (let [{:keys [content run]} (wt/request-tail user repo-id 1)]
        (is (str/includes? content "This is the opener"))
        (is (= (first shas) (:as-of-sha run)))
        (with-redefs [freememo.question-map/link-labels! (fn [& _] {})]
          (wt/apply-reply! user run "```freememo-map\n{\"known\": [\"cycle op\"]}\n```\n\nGood."))
        (let [[mark] (db/known-marks-on-repo user repo-id)]
          (is (= ["cycle op" "tutor" (first shas)] ((juxt :label :source :as_of_sha) mark))
            "one mark, source tutor, as of A"))
        (with-redefs [freememo.question-map/link-labels! (fn [& _] {})]
          (wt/apply-reply! user run "```freememo-map\n{\"known\": [\"Cycle op\"]}\n```\n\nAgain."))
        (is (= 1 (count (db/known-marks-on-repo user repo-id)))
          "a second answer about the same idea reuses its entity")))
    (wt/set-focus! user repo-id {:walkthrough-id wid :position 2 :path "docs/ir-spec.md"
                                 :sha (last shas)})
    (testing "§6.7.2: a step's sources are its hunks and its file"
      (let [{:keys [content evidence]} (wt/request-tail user repo-id 5)]
        (is (= "E5" (:citation-key (first evidence))))
        (is (every? #(= wid (:walkthrough-id %)) evidence))
        (is (every? #(= repo-id (:document-topic-id %)) evidence))
        (is (some #(= "docs/ir-spec.md" (:document-title %)) evidence))
        (is (str/includes? content "rename cycle to loop"))
        (is (str/includes? content "cycle op") "the reader's marks are listed")))
    (testing "the focused ref is the canvas file at its commit"
      (is (= {:repo-id repo-id :path "docs/ir-spec.md" :sha (last shas)}
            (select-keys (wt/focused-ref user repo-id) [:repo-id :path :sha]))))
    (wt/clear-focus! user repo-id wid 0)
    (is (some? (wt/focused-ref user repo-id)) "another view's focus stays")
    (wt/clear-focus! user repo-id wid 2)
    (is (nil? (wt/focused-ref user repo-id)))))

(deftest a-cancel-leaves-the-walkthrough-failed-and-empty
  (if-not (db-up?)
    (is true "PostgreSQL is unavailable; integration assertion skipped")
    (let [user (:id (jdbc/execute-one! db/ds
                      ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                       (str "walkthrough-test-" (random-uuid)) (str (random-uuid) "@example.test")]
                      opts))
          repo (:id (jdbc/execute-one! db/ds ["INSERT INTO topics (user_id, kind, title) VALUES (?, 'repo', 'r')
                                               RETURNING id" user] opts))]
      (try
        (let [wid (db/create-walkthrough! user repo "a" "b" [])
              entered (promise)
              release (promise)]
          (with-redefs [wt/group-range! (fn [& _]
                                          (deliver entered true)
                                          @release
                                          {:concepts [{:label "x"}] :hunks []})]
            (let [f (future (wt/prepare! user wid repo))]
              @entered
              ((get @(us/get-atom user :walkthrough-cancellers) wid))
              (deliver release true)
              @f))
          (let [w (db/get-walkthrough user wid)]
            (is (= ["failed" "Cancelled."] ((juxt :status :error) w)))
            (is (empty? (db/walkthrough-concepts user wid)) "§6.5.5: nothing stored")))
        (finally (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" user]))))))

(deftest an-ask-in-a-step-reads-the-canvas-files-pins
  (with-walkthrough [user repo-id wid shas]
    (let [canvas-ref {:repo-id repo-id :path "docs/ir-spec.md" :sha (second shas)}]
      (is (:ok (qm/create-pin! user canvas-ref {:quote "A hold op splits." :question "Why split?"})))
      (is (:ok (qm/create-pin! user canvas-ref {:quote "A cycle op repeats."})))
      (wt/set-focus! user repo-id {:walkthrough-id wid :position 1 :path "docs/ir-spec.md"
                                   :sha (second shas)})
      (testing "§6.6.6: the run's file is the canvas file, so one Ask carries both pins"
        (let [ref (assistant/document-ref-for user repo-id)
              {:keys [text error]} (qm/open-pins-message user ref)]
          (is (nil? error))
          (is (str/includes? text "[pin 1]"))
          (is (str/includes? text "[pin 2]"))))
      (wt/clear-focus! user repo-id wid 1))))

(deftest one-ref-gives-the-same-bytes-through-both-sources
  (with-walkthrough [user repo-id wid shas]
    (let [bare {:repo-id repo-id :path "src/ir.zig" :sha (last shas)}
          topic-ref (docref/resolve-spec user bare)]
      (is (some? (:topic-id topic-ref)) "commit B was imported, so the file is a topic")
      (testing "§6.3.1: topic_code and the clone agree"
        (is (= (:source (docref/content user topic-ref))
              (:source (docref/content user (dissoc topic-ref :topic-id))))))
      (testing "a commit that was never imported reads from the clone"
        (is (= "const Op = enum { cycle, hold };\n"
              (:source (docref/content user {:repo-id repo-id :path "src/ir.zig" :sha (second shas)}))))))))

(deftest a-failed-walkthrough-can-be-prepared-again
  (with-walkthrough [user repo-id wid shas]
    (is (:error (wt/restart! user wid)) "a ready walkthrough is not restarted")
    (jdbc/execute! db/ds ["UPDATE walkthroughs SET status = 'failed', error = 'boom' WHERE id = ?" wid])
    (is (= {:ok true} (wt/restart! user wid)))
    (deref (future (loop [n 0]
                     (when (and (< n 200) (not= "ready" (:status (db/get-walkthrough user wid))))
                       (Thread/sleep 50) (recur (inc n))))))
    (let [o (wt/overview user wid)]
      (is (= ["ready" nil 0] ((juxt :status :error :step) o)))
      (is (= ["split hold" "rename cycle to loop"] (map :label (:concepts o)))
        "the old grouping is gone and the new one is whole"))))

(deftest a-walkthrough-is-named-listed-and-deleted
  (with-walkthrough [user repo-id wid shas]
    (let [row (fn [] (first (wt/list-for-repo user repo-id)))]
      (testing "plans/repository-home.md §4.2.2.2: the home row"
        (is (= {:id wid :name nil :status "ready" :display-name "split hold and 1 more"
                :progress "0 / 2"}
              (select-keys (row) [:id :name :status :display-name :progress])))
        (is (= (str (subs (second shas) 0 7) " → " (subs (last shas) 0 7) " · 2 commits")
              (:range (row))))
        (is (string? (:progressed (row)))))
      (testing "§4.3.3: a name is trimmed, shows everywhere, and clears when blank"
        (is (= {:ok true} (wt/rename! user wid "  Hold and loop ")))
        (is (= "Hold and loop" (:display-name (row))))
        (is (= ["Hold and loop" "Hold and loop"] ((juxt :name :display-name) (wt/overview user wid))))
        (is (:error (wt/rename! user wid (apply str (repeat 121 "x")))))
        (is (= "Hold and loop" (:name (row))) "a refused name changes nothing")
        (is (:error (wt/rename! (inc user) wid "Theirs")) "another user's walkthrough")
        (is (= {:ok true} (wt/rename! user wid "")))
        (is (= "split hold and 1 more" (:display-name (row)))))
      (testing "§4.2.2.7: delete takes the steps with it"
        (is (:error (wt/delete! (inc user) wid)))
        (is (= {:ok true} (wt/delete! user wid)))
        (is (empty? (wt/list-for-repo user repo-id)))
        (is (empty? (db/walkthrough-concepts user wid)))))))

(deftest the-home-and-the-commit-card-read-the-clone
  (with-walkthrough [user repo-id wid shas]
    (let [commit-id (:id (jdbc/execute-one! db/ds ["SELECT id FROM topics WHERE parent_id = ? AND kind = 'commit'"
                                                   repo-id] opts))]
      (testing "§4.2.3: the imported snapshot, its files and its subject"
        (is (= [[(last shas) 2]] (map (juxt :commit_sha :files) (db/repo-snapshots user repo-id))))
        (is (= {:ok {(last shas) "refactor: rename cycle to loop"}}
              (repo-commits/snapshot-subjects user repo-id))))
      (testing "§4.4.1: the commit card"
        (let [card (repo-commits/commit-card user commit-id)]
          (is (= {:repo-id repo-id :sha (last shas) :subject "refactor: rename cycle to loop"
                  :changed-files 2 :refusal nil}
                (select-keys card [:repo-id :sha :subject :changed-files :refusal])))
          (is (seq (:author card)))))
      (testing "§4.4.2: a lost clone is made again"
        (git/delete-clone! repo-id)
        (is (= (last shas) (:sha (repo-commits/commit-card user commit-id)))))
      (testing "§4.4.4: the repository's first commit cannot be walked through alone"
        (jdbc/execute! db/ds ["UPDATE topics SET commit_sha = ? WHERE id = ?" (first shas) commit-id])
        (is (re-find #"first commit" (str (:refusal (repo-commits/commit-card user commit-id))))))
      (testing "not a commit snapshot"
        (is (nil? (repo-commits/commit-card user repo-id)))))))

(deftest a-walkthrough-name-survives-an-account-transfer
  (with-walkthrough [user repo-id wid shas]
    (is (= {:ok true} (wt/rename! user wid "Hold and loop")))
    (let [zip (java.io.File/createTempFile "walkthrough-transfer" ".zip")
          other (:id (jdbc/execute-one! db/ds
                       ["INSERT INTO users (username, email) VALUES (?, ?) RETURNING id"
                        (str "walkthrough-transfer-" (random-uuid)) (str (random-uuid) "@example.test")]
                       opts))]
      (try
        (with-open [out (io/output-stream zip)]
          (account-export/write-archive! db/ds user out "http://test.example"))
        (let [dir (archive/extract-to-temp-dir! zip {})]
          (try
            (is (:ok (account-import/import-archive! db/ds other dir nil {})))
            (testing "plans/repository-home.md §4.3.4: the name crosses with the walkthrough"
              (is (= ["Hold and loop"]
                    (map :name (jdbc/execute! db/ds ["SELECT name FROM walkthroughs WHERE user_id = ?" other]
                                 opts)))))
            (finally (git/delete-dir! dir))))
        (finally
          (.delete zip)
          (jdbc/execute! db/ds ["DELETE FROM users WHERE id = ?" other]))))))
