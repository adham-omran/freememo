(ns freememo.launch-env
  "Where the server reads the settings that the Local Install launcher owns
   (docs/adr/0001-jvm-launcher-replaces-electron.md).

   A JVM cannot set its own environment. The Local Install launcher runs the
   server in its own JVM, so it cannot give `DB_*`, `FREEMEMO_PEER`,
   `ENC_KEY_SECRET` and the like through env vars. It sets a system property
   with `set!` before it loads any server namespace. `getenv` reads the
   env var first and the property second, so a server, a self-host and
   Electron (which all set env vars) behave as before.

   NO requires: this namespace loads first.")

(defn property-name
  "Post: the system property that stands for env var `k`."
  [k]
  (str "freememo.env." k))

(defn getenv
  "Post: the env var `k`, else the system property `freememo.env.<k>`, else nil."
  [k]
  (or (System/getenv k)
      (System/getProperty (property-name k))))

(defn set!
  "Pre:  call it before the namespace that reads `k` loads. Most of them read
         at load time.
   Post: `getenv` returns `v` for `k`, unless the env has `k`."
  [k v]
  (System/setProperty (property-name k) (str v))
  nil)
