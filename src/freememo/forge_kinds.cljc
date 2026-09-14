(ns freememo.forge-kinds
  "The git forges a repository can be imported from — the single vocabulary.

   NO REQUIRES and NO reader conditional, for the reason
   `freememo.code-languages` records: the import modal and the Settings section
   name this list from client scope, and `freememo.forge` reads it on the JVM.
   A CLJ-only def read from client scope closes the WebSocket at mount.

   `:id` is the string stored in `repo_selections.forge` and inside the token
   setting key, so it MUST NOT change once a row holds it.")

(def forges
  "Display order. `:default-host` prefills the host field; a self-hosted
   instance replaces it."
  [{:id "github"  :label "GitHub"  :default-host "github.com"}
   {:id "gitlab"  :label "GitLab"  :default-host "gitlab.com"}
   {:id "forgejo" :label "Forgejo" :default-host "codeberg.org"}])

(def forge-ids
  "`forges` as a set of `:id` strings — the answer to \"may this string be
   stored as a forge\"."
  (into #{} (map :id) forges))

(defn forge-label
  "Post: the display label for `forge-id`, or the id itself when unknown."
  [forge-id]
  (or (some #(when (= forge-id (:id %)) (:label %)) forges) (str forge-id)))

(defn default-host
  "Post: the public host for `forge-id`, or nil when unknown."
  [forge-id]
  (some #(when (= forge-id (:id %)) (:default-host %)) forges))
