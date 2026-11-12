(ns freememo.desktop-version
  "What a Local Install and freememo.net must agree on (CONTEXT.md, Sync
   Protocol; docs/adr/0002).

   The Sync Protocol is one plain number. A breaking change to sync, sign-in
   or the AI proxy raises it by one. Nothing else in FreeMemo has a major:
   a Sync License covers every Release.

   Inside one Sync Protocol, schema changes stay additive, because an apply
   sets only the columns a payload carries (2.4.3).")

(def sync-protocol 1)

(def runtime
  "The Runtime of this Release: one number. It rises by one when a change needs
   something that an App Update cannot replace (the launcher, the JRE, bundled
   tools, PostgreSQL). The release workflow writes it into
   `freememo-<rv>-release.json`; ticket 20 adds the gate.
   `desktop-version-test` fails when an installer input changes, until the
   change records whether it raises this number.
   2: the app child JVM shows as FreeMemo, not as Java (2026-11-12)."
  2)

(def protocol-header
  "The request header with which a Local Install sends its Sync Protocol on
   every sync, sign-in and AI proxy call."
  "X-FreeMemo-Sync-Protocol")

(defn protocol-error
  "Compare the Sync Protocol that a request sent with this server's.
   Post: nil when they are equal, `\"update-required\"` when the client is
         older, `\"server-outdated\"` when it is newer or sent no number."
  [sent]
  (cond
    (= sent sync-protocol) nil
    (and (number? sent) (< sent sync-protocol)) "update-required"
    :else "server-outdated"))
