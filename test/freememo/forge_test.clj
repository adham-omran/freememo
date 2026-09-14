(ns freememo.forge-test
  "Reading what a user types into the import form, and the address gate
   (plans/git-forge-integration.md §4.2). No network is required: the address
   gate resolves only literal IP addresses here."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.forge :as forge]))

(deftest parse-origin-reads-a-host-the-way-it-is-typed
  (is (= {:scheme "https" :host "gitlab.com"} (forge/parse-origin "gitlab.com")))
  (is (= {:scheme "https" :host "git.example.org"} (forge/parse-origin " https://Git.Example.org/ ")))
  (is (= {:scheme "http" :host "10.0.0.5:3000"} (forge/parse-origin "http://10.0.0.5:3000")))
  (is (nil? (forge/parse-origin "")))
  (is (nil? (forge/parse-origin "ftp://example.org")))
  (is (nil? (forge/parse-origin "https://user:secret@example.org"))
    "a URL carrying credentials is refused, so they cannot reach a log line"))

(deftest parse-repository-input-reads-every-shape-a-user-pastes
  (doseq [[input path host]
          [["octocat/Hello-World" "octocat/Hello-World" nil]
           ["https://github.com/octocat/Hello-World" "octocat/Hello-World" "github.com"]
           ["https://github.com/octocat/Hello-World.git" "octocat/Hello-World" "github.com"]
           ["https://github.com/octocat/Hello-World/tree/main/src" "octocat/Hello-World" "github.com"]
           ["https://gitlab.com/group/sub/project/-/tree/main" "group/sub/project" "gitlab.com"]
           ["https://codeberg.org/forgejo/forgejo/src/branch/forgejo" "forgejo/forgejo" "codeberg.org"]
           ["git@github.com:octocat/Hello-World.git" "octocat/Hello-World" "github.com"]]]
    (testing input
      (let [r (forge/parse-repository-input input)]
        (is (= path (:path r)))
        (is (= host (:host (:origin r)))))))
  (is (nil? (forge/parse-repository-input "   "))))

(deftest a-repository-path-is-validated-per-forge
  (is (forge/valid-repository-path? "github" "octocat/Hello-World"))
  (is (not (forge/valid-repository-path? "github" "group/sub/project"))
    "GitHub and Forgejo have no nested groups")
  (is (forge/valid-repository-path? "gitlab" "group/sub/project"))
  (doseq [bad ["octocat" "../etc" "a/.." "a b/c" "a//b" "/a/b"]]
    (is (not (forge/valid-repository-path? "gitlab" bad)) bad)))

(deftest the-address-gate-refuses-private-hosts-by-default
  ;; FORGE_ALLOW_PRIVATE_HOSTS is unset in a test JVM, and config.edn does not
  ;; set the key — §4.2 2.6's default.
  (is (false? (forge/address-permitted? "http://127.0.0.1:3000/api/v1")))
  (is (false? (forge/address-permitted? "http://192.168.1.10/api/v4")))
  (is (false? (forge/address-permitted? "file:///etc/passwd"))))
