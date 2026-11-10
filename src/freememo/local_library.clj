(ns freememo.local-library
  "The database side of `freememo.local-account`: what a Local Install does to
   its library and its sync link when the account changes
   (plans/local-install/issues/05-one-library-one-account.md).

   Needs the app namespaces, so the launcher loads it after it sets the
   database settings."
  (:require [next.jdbc :as jdbc]
            [freememo.account-transfer-format :as fmt]
            [freememo.db :as db]
            [freememo.desktop :as desktop]
            [freememo.desktop-link :as link]
            [freememo.sync-client :as sync-client]))

(defn unsynced-count
  "Post: the number of rows and deletions that the server has not received."
  []
  (+ (reduce + (map #(:n (jdbc/execute-one! db/ds [(str "SELECT count(*) AS n FROM " (name %) " WHERE usn = -1")]
                           {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))
                 fmt/synced-tables))
    (:n (jdbc/execute-one! db/ds ["SELECT count(*) AS n FROM sync_graves WHERE usn = -1"]
          {:builder-fn next.jdbc.result-set/as-unqualified-lower-maps}))))

(defn revoked?
  "Post: true after a sync got 401, until a sync starts again."
  []
  (= :signed-out (:state (sync-client/status))))

(defn library-ready? [] (boolean (:library-ready (sync-client/status))))

(defn wipe!
  "Post: no sync runs, no row, table or large object of the library remains,
         the schema is new and empty, and the sync link holds no token.
         `adopt!` starts the sync scheduler again after the next sign-in."
  []
  (sync-client/stop-scheduler!)
  (link/configure! {:origin (link/origin) :token nil :user-id nil})
  (jdbc/execute! db/ds ["SELECT lo_unlink(oid) FROM pg_largeobject_metadata"])
  (jdbc/execute! db/ds ["DROP SCHEMA public CASCADE"])
  (jdbc/execute! db/ds ["CREATE SCHEMA public"])
  (db/setup-schema))

(defn adopt!
  "Pre:  `wipe!` ran. Post: `account` owns the empty library and syncs."
  [account]
  (desktop/set-launch-config! account)
  (desktop/start!)
  (sync-client/resume-after-sign-in!))

(defn renew!
  "Post: the link holds the token of `account`, the same account as before,
         and a sync starts."
  [account]
  (desktop/set-launch-config! account)
  (link/configure! {:origin (get account "origin") :token (get account "token")
                    :user-id (get account "user_id")})
  (sync-client/resume-after-sign-in!))
