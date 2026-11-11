(ns freememo.release-scripts-test
  "The release scripts in tools/release, run against a fake GitHub API (no
   network). The fake keeps releases and tags in memory."
  (:require [cheshire.core :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]])
  (:import [com.sun.net.httpserver HttpExchange HttpHandler HttpServer]
           [java.net InetSocketAddress]
           [java.nio.file Files]))

(defn- respond [^HttpExchange ex status body]
  (let [bs (.getBytes (if (string? body) ^String body ^String (json/generate-string body)) "UTF-8")]
    (.add (.getResponseHeaders ex) "Content-Type" "application/json")
    (.sendResponseHeaders ex status (if (zero? (alength bs)) -1 (alength bs)))
    (when (pos? (alength bs))
      (with-open [o (.getResponseBody ex)] (.write o bs)))))

(defn- page-of [coll query]
  (let [m (into {} (map #(str/split % #"=" 2)) (remove str/blank? (str/split (or query "") #"&")))
        per (parse-long (get m "per_page" "30"))
        n (parse-long (get m "page" "1"))]
    (vec (take per (drop (* per (dec n)) coll)))))

(defn- fake-github
  "Start a fake GitHub. state is {:releases [...] :tags [...]}; :log gets one
   string per request."
  [state*]
  (let [log (atom [])
        server (HttpServer/create (InetSocketAddress. "127.0.0.1" 0) 0)
        handler (reify HttpHandler
                  (handle [_ ex]
                    (try
                      (let [m (.getRequestMethod ex)
                            path (.getPath (.getRequestURI ex))
                            query (.getRawQuery (.getRequestURI ex))
                            auth (.getFirst (.getRequestHeaders ex) "Authorization")
                            body (slurp (.getRequestBody ex))
                            p (str/replace path #"^/repos/o/r" "")]
                        (swap! log conj (str m " " p))
                        (cond
                          (not= "Bearer tok" auth) (respond ex 401 {:message "bad credentials"})
                          (and (= m "GET") (= p "/releases/latest"))
                          (if-let [r (first (remove #(or (:draft %) (:prerelease %)) (:releases @state*)))]
                            (respond ex 200 r)
                            (respond ex 404 {:message "Not Found"}))
                          (and (= m "GET") (= p "/releases"))
                          (respond ex 200 (page-of (:releases @state*) query))
                          (and (= m "POST") (= p "/releases"))
                          (let [r (json/parse-string body true)
                                r (assoc r :id (+ 1000 (count (:releases @state*))))]
                            (swap! state* update :releases #(into [r] %))
                            (respond ex 201 r))
                          (and (= m "DELETE") (re-matches #"/releases/(\d+)" p))
                          (let [id (parse-long (second (re-matches #"/releases/(\d+)" p)))]
                            (swap! state* update :releases #(vec (remove (fn [r] (= id (:id r))) %)))
                            (respond ex 204 ""))
                          (and (= m "GET") (= p "/tags"))
                          (respond ex 200 (page-of (map (fn [t] {:name t}) (:tags @state*)) query))
                          (and (= m "DELETE") (str/starts-with? p "/git/refs/tags/"))
                          (let [t (subs p (count "/git/refs/tags/"))]
                            (swap! state* update :tags #(vec (remove #{t} %)))
                            (respond ex 204 ""))
                          :else (respond ex 404 {:message (str "no route " m " " p)})))
                      (catch Throwable t
                        (respond ex 500 {:message (str t)})))))]
    (.createContext server "/" handler)
    (.start server)
    {:server server :log log :url (str "http://127.0.0.1:" (.getPort (.getAddress server)))}))

(defn- sh-run
  "Run tools/release/<script> with the fake GitHub. Returns {:exit :out :err}."
  [fake script & args]
  (let [pb (ProcessBuilder. ^java.util.List (into ["sh" (str "tools/release/" script)] args))
        env (.environment pb)]
    (.put env "GH_API" (or (:url fake) "http://127.0.0.1:1"))
    (.put env "GH_REPO" "o/r")
    (.put env "TMPDIR" "/home/adham/.cache/fm-seam1-tmp")
    (.put env "GH_TOKEN" "tok")
    (let [p (.start pb)
          out (future (slurp (.getInputStream p)))
          err (future (slurp (.getErrorStream p)))]
      {:exit (.waitFor p) :out (str/trim @out) :err (str/trim @err)})))

(defmacro ^:private with-fake [[fake state] & body]
  `(let [state# ~state
         ~fake (fake-github state#)]
     (try ~@body
          (finally (.stop ^HttpServer (:server ~fake) 0)))))

(defn- rel [tag & {:as m}] (merge {:id (hash tag) :tag_name tag :draft false :prerelease false} m))

(deftest guard-compares-with-the-latest-published-release
  (let [state (atom {:releases [(rel "v26.11.14336" :id 1)
                                (rel "v26.12.1" :id 2 :draft true)]})]
    (with-fake [fake state]
      (testing "a larger version passes, and a draft does not count"
        (is (zero? (:exit (sh-run fake "guard.sh" "26.11.14337"))))
        (is (zero? (:exit (sh-run fake "guard.sh" "26.12.0")))))
      (testing "numeric per part"
        (is (zero? (:exit (sh-run fake "guard.sh" "26.11.20000")))))
      (testing "the same version stops"
        (let [r (sh-run fake "guard.sh" "26.11.14336")]
          (is (= 1 (:exit r)))
          (is (str/includes? (:err r) "not larger"))))
      (testing "a smaller version stops"
        (is (= 1 (:exit (sh-run fake "guard.sh" "26.10.44639"))))))))

(deftest guard-passes-with-no-release
  (with-fake [fake (atom {:releases []})]
    (is (zero? (:exit (sh-run fake "guard.sh" "26.11.1"))))))

(deftest create-draft-makes-a-draft-and-replaces-an-old-one
  (let [state (atom {:releases [(rel "v26.11.5" :id 7 :draft true)
                                (rel "v26.11.1" :id 3)]})
        notes (Files/createTempFile (.toPath (io/file "/home/adham/.cache/fm-seam1-tmp")) "notes" ".md"
                                    (make-array java.nio.file.attribute.FileAttribute 0))]
    (try
      (spit (.toFile notes) "- First\n- Second\n")
      (with-fake [fake state]
        (let [r (sh-run fake "create-draft.sh" "26.11.5" "abc123" (str notes))]
          (is (zero? (:exit r)) (:err r))
          (let [rs (:releases @state)
                made (first (filter #(= "v26.11.5" (:tag_name %)) rs))]
            (is (= 1 (count (filter #(= "v26.11.5" (:tag_name %)) rs))) "the old draft is gone")
            (is (true? (:draft made)))
            (is (= "abc123" (:target_commitish made)))
            (is (= "- First\n- Second\n" (:body made)))
            (is (some #(= "v26.11.1" (:tag_name %)) rs) "the published release stays"))))
      (finally (Files/deleteIfExists notes)))))

(deftest prune-keeps-only-the-published-release
  (let [state (atom {:releases (into [(rel "v26.12.9" :id 900)]
                                     (map #(rel (str "v26.11." %) :id %) (range 1 120)))
                     :tags (into ["v26.12.9" "keepme-not-a-release"]
                                 (map #(str "v26.11." %) (range 1 120)))})]
    (with-fake [fake state]
      (let [r (sh-run fake "prune.sh" "26.12.9")]
        (is (zero? (:exit r)) (:err r))
        (is (= ["v26.12.9"] (map :tag_name (:releases @state))) "119 older releases, two pages")
        (is (= ["v26.12.9" "keepme-not-a-release"] (:tags @state))
            "every older v tag goes, and a tag without the v prefix stays")))))

(deftest prune-refuses-when-the-release-to-keep-is-a-draft-or-missing
  (doseq [rs [[(rel "v26.12.9" :id 900 :draft true) (rel "v26.11.1" :id 1)]
              [(rel "v26.11.1" :id 1)]]]
    (let [state (atom {:releases rs :tags ["v26.11.1" "v26.12.9"]})]
      (with-fake [fake state]
        (let [r (sh-run fake "prune.sh" "26.12.9")]
          (is (= 1 (:exit r)))
          (is (= (count rs) (count (:releases @state))) "nothing deleted")
          (is (= 2 (count (:tags @state)))))))))

(deftest notes-list-the-subjects-since-the-previous-tag
  (let [dir (Files/createTempDirectory (.toPath (io/file "/home/adham/.cache/fm-seam1-tmp")) "notes-repo"
                                       (make-array java.nio.file.attribute.FileAttribute 0))
        d (str dir)
        git (fn [& args]
              (let [p (.start (doto (ProcessBuilder. ^java.util.List (into ["git" "-C" d "-c" "user.name=t" "-c" "user.email=t@t"] args))
                                (.redirectErrorStream true)))
                    out (slurp (.getInputStream p))]
                (.waitFor p) (str/trim out)))
        notes #(:out (sh-run nil "notes.sh" d %1 %2))]
    (try
      (git "init" "-q")
      (doseq [s ["One" "Two" "Three"]] (git "commit" "-q" "--allow-empty" "-m" s))
      (git "tag" "v1" "HEAD~1")
      (doseq [s ["Four"]] (git "commit" "-q" "--allow-empty" "-m" s))
      (testing "since the tag"
        (is (= "- Four\n- Three" (notes "v1" "HEAD"))))
      (testing "no previous tag lists the history"
        (is (= "- Four\n- Three\n- Two\n- One" (notes "" "HEAD")))
        (is (= "- Four\n- Three\n- Two\n- One" (notes "v-gone" "HEAD"))))
      (testing "nothing new"
        (is (= "No public changes." (notes "HEAD" "HEAD"))))
      (finally
        (doseq [f (reverse (file-seq (io/file d)))] (.delete f))))))
