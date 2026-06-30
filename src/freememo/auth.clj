(ns freememo.auth
  "User authentication with pgcrypto password hashing."
  (:require
    [freememo.db :as db]
    [taoensso.telemere :as tel]
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
        (do (tel/error! {:id ::create-user} e)
            {:success false :error "Failed to create account"})))))

(defn ensure-admin-user!
  "Seed an admin account from env at boot, only if absent — never resets an
   existing password (Q4: seed-if-absent).
   Pre: username/password non-blank (caller checks env presence).
   Post: a users row with `username` exists. Returns :created | :exists | :error."
  [username password]
  (if (db/get-user-by-username username)
    :exists
    (let [r (create-user username password)]
      (if (:success r)
        :created
        (do (tel/error! {:id ::ensure-admin-user} (:error r))
            :error)))))

(defn authenticate [username password]
  (try
    (let [result (jdbc/execute-one! db/ds
                   ["SELECT id, username FROM users WHERE username = ? AND password_hash = crypt(?, password_hash)"
                    username password])]
      (if result
        {:success true :user-id (:users/id result) :username (:users/username result)}
        {:success false :error "Invalid username or password"}))
    (catch Exception e
      (tel/error! {:id ::authenticate} e)
      {:success false :error "Authentication failed"})))
