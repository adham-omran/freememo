(ns freememo.desktop-version
  "The desktop app's declared versions (plans/electron-local-db-sync.md 13.3).

   `desktop-major` is what a license covers (13.2). The maintainer bumps it by
   hand. It is independent of the installer's release version, YYYY.M.N
   (electron/scripts/version.mjs).

   `sync-protocol` is what `meta` compares (2.3). It changes only together with
   `desktop-major` (13.3.1). Inside one major, schema changes stay additive,
   because an apply sets only the columns a payload carries (2.4.3).")

(def desktop-major 1)

(def sync-protocol
  "Encoded as major × 1000 + revision, so a protocol change that forgot the
   major bump is a test failure (`desktop_version_test`)."
  1000)

(defn protocol-major
  "The desktop major a sync protocol number belongs to."
  [protocol]
  (quot protocol 1000))
