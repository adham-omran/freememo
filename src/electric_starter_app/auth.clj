(ns electric-starter-app.auth
  "User authentication with pgcrypto password hashing."
  (:require
    [electric-starter-app.db :as db]
    [next.jdbc :as jdbc]))

(defn create-user [username password]
  (try
    (let [result (jdbc/execute-one! db/ds
                   ["INSERT INTO users (username, password_hash) VALUES (?, crypt(?, gen_salt('bf'))) RETURNING id"
                    username password])]
      {:success true :user-id (:users/id result)})
    (catch org.postgresql.util.PSQLException e
      (if (.contains (.getMessage e) "duplicate key")
        {:success false :error "Username already taken"}
        (do (println "ERROR [create-user]:" (.getMessage e))
            {:success false :error "Failed to create account"})))))

(defn authenticate [username password]
  (try
    (let [result (jdbc/execute-one! db/ds
                   ["SELECT id, username FROM users WHERE username = ? AND password_hash = crypt(?, password_hash)"
                    username password])]
      (if result
        {:success true :user-id (:users/id result) :username (:users/username result)}
        {:success false :error "Invalid username or password"}))
    (catch Exception e
      (println "ERROR [authenticate]:" (.getMessage e))
      {:success false :error "Authentication failed"})))
