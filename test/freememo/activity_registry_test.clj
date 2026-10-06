(ns freememo.activity-registry-test
  "Discovery's two halves must agree for every server-tier type.
   plans/activity-monitor.md §4, §5.

   `live-work` builds a work_key from each member of an owner's atom, through
   the entry's `:owner-key->args` and `:work-key`. `read-state` parses that key
   back through `:key-fn` and looks for it in the same atom. An entry whose
   fields do not fit the member shape fails only when discovery runs. And
   `activity-reporter/open-rows!` catches each type's failure, so that type
   never gets a row, and one log line per tick is the only trace.

   `:url-import` shipped that way: `(constantly [])` called `identity` with no
   arguments, and no link import ever reached the Activity monitor.

   Needs no database. The registry requires only `freememo.user-state`."
  (:require
   [clojure.test :refer [deftest is testing]]
   [freememo.activity-registry :as registry]
   [freememo.user-state :as us]))

(def ^:private uid
  "A user id that no real session holds. `user-state` is one registry for the
   whole process, so a real id would share its atoms with this test."
  -4711)

(def ^:private samples
  "One member per server-tier type, in the shape its owner publishes.

   `:entry` is the value for an owner whose atom is a map. `:subject-id` is what
   discovery must derive, and is absent for a type with `:subject :none`."
  {"card-gen"             {:member 42 :entry {:active-id 1 :pending 1} :subject-id 42}
   "page-ocr"             {:member [7 3] :subject-id 7}
   "transcribe"           {:member 42 :subject-id 42}
   "video-process"        {:member 42 :subject-id 42}
   "distill-doc"          {:member 42 :subject-id 42}
   "distill-repo"         {:member 42 :subject-id 42}
   "generate-questions"   {:member 42 :subject-id 42}
   "exam-form-generation" {:member "material/2"}
   "exam-grade"           {:member 42 :entry [1 3]}
   "account-export"       {:member "account"}
   "account-import"       {:member "account"}
   "url-import"           {:member "https://example.org/"}
   "repo-import"          {:member "github.com/owner/name" :entry "downloading"}
   "walkthrough-prepare"  {:member 42 :entry "reading diffs"}})

(defn- publish! [a {:keys [member entry]}]
  (if (map? @a)
    (swap! a assoc member entry)
    (swap! a conj member)))

(defn- discover
  "`live-work`, realized here. A throw becomes a value that an assertion can
   name, instead of an error that loses its `testing` context."
  [type]
  (try (vec (registry/live-work type uid))
    (catch Throwable t t)))

(deftest every-server-tier-type-has-a-sample
  (is (= (registry/server-tier-types) (set (keys samples)))
    "a new server-tier type needs a member in `samples`"))

(deftest discovery-round-trips-for-every-server-tier-type
  (doseq [type (sort (registry/server-tier-types))
          :let [sample (samples type)
                a (us/get-atom uid (:state-key (registry/activity-type type)))]
          :when sample]
    (testing type
      (try
        (publish! a sample)
        (let [work (discover type)]
          (is (vector? work) (str "live-work threw: " work))
          (when (vector? work)
            (is (= 1 (count work)))
            (doseq [{:keys [work-key subject-id]} work]
              (is (string? work-key) "activities.work_key is a TEXT column")
              (is (some? (registry/read-state type uid work-key))
                "read-state must find the member that live-work named")
              (is (= (:subject-id sample) subject-id)))))
        (finally (reset! a (empty @a)))))))
