(ns freememo.local-postgres-test
  "The embedded PostgreSQL of a Local Install runs the production major (ADR
   0003), and a data directory of another major is moved aside, never opened.
   Run under `:test:local-install`: it starts the zonky PostgreSQL 18.6."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [freememo.local-postgres :as pg])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "fm-pg-test" (make-array FileAttribute 0))))

(deftest an-old-major-data-directory-moves-aside-and-is-not-deleted
  (let [root (temp-dir) pgdata (io/file root "pgdata")]
    (.mkdirs pgdata)
    (spit (io/file pgdata "PG_VERSION") "16\n")
    (spit (io/file pgdata "marker") "library")
    (let [aside (pg/retire-old-major! pgdata)]
      (is (not (.exists pgdata)))
      (is (re-find #"^pgdata\.pg16-\d{8}$" (.getName aside)))
      (is (= "library" (slurp (io/file aside "marker")))))
    ;; A second old directory on the same day gets its own name.
    (.mkdirs pgdata)
    (spit (io/file pgdata "PG_VERSION") "16\n")
    (is (re-find #"-1$" (.getName (pg/retire-old-major! pgdata))))))

(deftest the-current-major-and-an-empty-directory-stay-in-place
  (let [root (temp-dir) pgdata (io/file root "pgdata")]
    (is (nil? (pg/retire-old-major! pgdata)))
    (.mkdirs pgdata)
    (spit (io/file pgdata "PG_VERSION") (str pg/pg-major "\n"))
    (is (nil? (pg/retire-old-major! pgdata)))
    (is (.exists pgdata))))

(deftest postgresql-18-starts-after-an-old-major-and-has-the-extensions
  (let [root (temp-dir) pgdata (io/file root "pgdata")]
    (.mkdirs pgdata)
    (spit (io/file pgdata "PG_VERSION") "16\n")
    (let [pg (pg/start-postgres! pgdata root "pw1234")]
      (try
        (with-open [c (java.sql.DriverManager/getConnection
                        (str "jdbc:postgresql://127.0.0.1:" (.getPort pg) "/postgres") "postgres" "pw1234")
                    s (.createStatement c)]
          (let [rs (.executeQuery s "SHOW server_version")]
            (.next rs)
            (is (.startsWith (.getString rs 1) "18.6")))
          (let [rs (.executeQuery s "SELECT string_agg(name, ',' ORDER BY name) FROM pg_available_extensions
                                     WHERE name IN ('pgcrypto','pg_trgm','fuzzystrmatch','unaccent')")]
            (.next rs)
            (is (= "fuzzystrmatch,pg_trgm,pgcrypto,unaccent" (.getString rs 1)))))
        (finally (.close pg)))
      (is (= "18" (clojure.string/trim (slurp (io/file pgdata "PG_VERSION")))))
      (is (some #(re-find #"^pgdata\.pg16-" (.getName %)) (.listFiles root))))))
