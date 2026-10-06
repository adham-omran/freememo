(ns freememo.db
  "Database connection and schema management for PostgreSQL.
   Unified topics model — all entities (documents, pages, extracts) are topics
   in a parent_id tree."
  (:require
   [next.jdbc :as jdbc]
   [next.jdbc.connection :as connection]
   [next.jdbc.result-set :as rs]
   [honey.sql :as sql]
   [cheshire.core :as json]
   [taoensso.telemere :as tel]
   [clojure.string :as str]
   [freememo.ai-tiers :as ai-tiers]
   [freememo.anki-digest :as digest]
   [freememo.cloze :as cloze]
   [freememo.csl-util :as csl]
   [freememo.html-cleaner :as cleaner]
   [freememo.input-check :as input]
   [freememo.largeobj :as lo]
   [freememo.quota :as quota]
   [freememo.config :as config]
   [freememo.fsrs :as fsrs]
   [freememo.logging :as log]
   [freememo.occlusion-ordinals :as ord]
   [freememo.page-merge :as page-merge]
   [freememo.priority :as priority]
   [freememo.text :as text])
  (:import [com.zaxxer.hikari HikariDataSource]
           [org.postgresql.util PGobject]))

(declare sanitize-utf8)
(declare migrate-to-topics!)
(declare backfill-content-text!)
(declare backfill-local-card-digests!)
(declare backfill-sources!)
(declare backfill-pdf-sources!)
(declare run-grandfather-migration!)
(declare run-once!)
(declare move-everyone-to-best-level!)
(declare normalize-inline-card-images!)
(declare record-anki-tombstones!)
(declare start-purge-scheduler!)
(declare unsynced-sql)
(declare ->jsonb)
(declare pgobject->clj)
;; Defined in the Incremental Video section; called from complete-credit-order!,
;; which sits far above it.
(declare promote-to-paid-storage-tier!)

;; ---------------------------------------------------------------------------
;; Connection configuration
;; ---------------------------------------------------------------------------

(def db-config
  {:dbtype "postgresql"
   :host (or (System/getenv "DB_HOST") "localhost")
   :port (Integer/parseInt (or (System/getenv "DB_PORT") "5432"))
   :dbname (or (System/getenv "DB_NAME") "cardmaker")
   ;; HikariCP setter is setUsername → key must be :username, NOT :user
   ;; (next.jdbc's ->pool passes leftover keys to the pool's Java setters;
   ;; :user maps to no setter and is silently dropped → driver would fall
   ;; back to the OS user and auth would fail).
   :username (or (System/getenv "DB_USER") "cardmaker")
   :password (or (System/getenv "DB_PASSWORD") "dev")
   :maximumPoolSize 10})

;; HikariCP connection pool. Loading this namespace does not connect.
;; `->pool` builds the data source through `clojure.java.data/to-java` and its
;; setters, and Hikari starts that pool at the first connection. So a down or
;; misconfigured database surfaces at the first query, not at load.
;;
;; Measured on 2026-09-14 with `clj -M:test`. The require returned in about 3 s
;; with `(.isRunning ds)` false. With the port refusing connections, the first
;; query threw `PSQLException` after about 1 s. Against a port that accepts TCP
;; and never answers, the first query blocked with no output.
(defonce ds (connection/->pool HikariDataSource db-config))

;; ---------------------------------------------------------------------------
;; Schema setup
;; ---------------------------------------------------------------------------

(defn- old-tables-exist?
  "Check if the legacy documents table exists (migration indicator)."
  []
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM information_schema.tables
             WHERE table_name = 'documents' AND table_schema = 'public'"])))

(defn- topics-table-exists?
  []
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM information_schema.tables
             WHERE table_name = 'topics' AND table_schema = 'public'"])))

(defn- setup-schema-core!
  "Extensions, the users table (+ OAuth + storage-quota columns), and the
   per-user settings table.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Enable pgcrypto for password hashing
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS pgcrypto"])

  ;; Enable pg_trgm for trigram-based substring and fuzzy search
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS pg_trgm"])

  ;; Groundwork for the planned embedding tier of freememo.kg-dedup — the
  ;; paraphrase residual h3 cannot reach (`hfql lacks-knowledge-of business
  ;; domain` vs `hfql is-not omniscient` scores trigram 0.11, containment 0.25).
  ;; NEITHER is an input to h3 today, and that is a measurement, not an
  ;; oversight: over 234 hand-labelled fact pairs, normalized Levenshtein scored
  ;; F1 0.52 against trigram's 0.78, and `max(trigram, levenshtein)` scored 0.55
  ;; — WORSE than trigram alone. Levenshtein rewards shared structure regardless
  ;; of meaning (`identity | must-be | injective` vs `resolver | must-be |
  ;; injective` scores 0.73 and is not a duplicate). unaccent is untested on this
  ;; corpus; it normalizes Latin diacritics and the observed residual is Arabic.
  ;; See plans/kg-duplicate-fact-sweep.md.
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS fuzzystrmatch"])
  (jdbc/execute! ds ["CREATE EXTENSION IF NOT EXISTS unaccent"])

  ;; Create users table
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS users (
      id SERIAL PRIMARY KEY,
      username TEXT NOT NULL UNIQUE,
      password_hash TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Google OAuth migrations
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS google_id TEXT UNIQUE"])
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS email TEXT"])
  (jdbc/execute! ds ["ALTER TABLE users ALTER COLUMN password_hash DROP NOT NULL"])
  ;; Human-readable name for addressing the user in outgoing mail. Filled from
  ;; Google's `name` claim at login, but only when NULL — see
  ;; `set-display-name-if-absent!`, which must never clobber a hand-set value.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS display_name TEXT"])

  ;; Storage quota: denormalized usage counter + optional per-user override.
  ;; ALTER + backfill run in one tx so partial failure rolls back; otherwise
  ;; columns could exist with default 0 and `had-usage?` becomes true forever.
  (let [had-usage? (some? (jdbc/execute-one! ds
                            ["SELECT 1 FROM information_schema.columns
                              WHERE table_name = 'users' AND column_name = 'usage_bytes'"]))
        topic-files-exists? (some? (jdbc/execute-one! ds
                                     ["SELECT 1 FROM information_schema.tables
                                       WHERE table_name = 'topic_files'"]))]
    (jdbc/with-transaction [tx ds]
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS usage_bytes BIGINT NOT NULL DEFAULT 0"])
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS quota_bytes BIGINT"])
      (jdbc/execute! tx ["ALTER TABLE users ADD COLUMN IF NOT EXISTS upload_max_bytes BIGINT"])
      (when (and (not had-usage?) topic-files-exists?)
        (tel/log! :info "Backfilling users.usage_bytes from topic_files.file_size")
        (jdbc/execute! tx
          ["UPDATE users SET usage_bytes = COALESCE(
              (SELECT SUM(tf.file_size)
               FROM topic_files tf JOIN topics t ON tf.topic_id = t.id
               WHERE t.user_id = users.id), 0)"]))))

  ;; Settings table (per-user key-value)
  (let [has-user-id (jdbc/execute-one! ds
                      ["SELECT column_name FROM information_schema.columns
                        WHERE table_name = 'settings' AND column_name = 'user_id'"])]
    (when-not has-user-id
      (tel/log! :info "Migrating settings table to per-user")
      (jdbc/execute! ds ["DROP TABLE IF EXISTS settings"])
      (jdbc/execute! ds ["
        CREATE TABLE IF NOT EXISTS settings (
          user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
          key TEXT NOT NULL,
          value TEXT NOT NULL,
          updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
          PRIMARY KEY (user_id, key)
        )"])))

  ;; One row per one-time data migration this database has run (run-once!).
  ;; Global, not per user: a per-user marker would re-run a migration for a user
  ;; who signed up after it, and overwrite what that user chose since.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS app_migrations (
      id TEXT PRIMARY KEY,
      applied_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )"])
)

(defn- setup-schema-topics!
  "The topics tree (+ repetition log, is_live/dismissed/content_text flags),
   topic_files, and flashcards — plus the legacy-table migration and its
   post-migration column/constraint cleanup. Order matters: flashcards'
   topic_id/root_topic_id FKs need topics first, and the migration needs
   both tables in place before it can move rows into them.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Topics table (unified: documents + pages + content_items)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topics (
      id SERIAL PRIMARY KEY,
      user_id INTEGER REFERENCES users(id) ON DELETE SET NULL,
      parent_id INTEGER REFERENCES topics(id) ON DELETE CASCADE,
      kind TEXT NOT NULL DEFAULT 'basic',
      title TEXT NOT NULL,
      content TEXT,
      page_number INTEGER,
      source_url TEXT,
      status TEXT DEFAULT 'active',
      priority DOUBLE PRECISION DEFAULT 50,
      interval_days REAL DEFAULT 1.0,
      a_factor REAL DEFAULT 2.0,
      next_review_at TIMESTAMP,
      last_review_at TIMESTAMP,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; review_count scalar superseded by topic_repetitions append-only log
  ;; (count derivable as COUNT(*) WHERE event_type IN ('advance','touch')).
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS review_count"])

  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_parent ON topics(parent_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_user ON topics(user_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_next_review ON topics(next_review_at)"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_topics_page
                      ON topics(parent_id, page_number) WHERE page_number IS NOT NULL"])

  ;; Topic repetition log — append-only history of session-driven mutations.
  ;; Each row captures pre-mutation snapshot of the six SR-relevant topic
  ;; fields so a SuperMemo-style history view can reconstruct what changed.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_repetitions (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      event_type TEXT NOT NULL
        CHECK (event_type IN ('advance','touch','postpone','done','restore','priority-change','dismiss','undismiss')),
      event_at TIMESTAMP NOT NULL DEFAULT NOW(),
      status_before TEXT NOT NULL,
      priority_before DOUBLE PRECISION,
      interval_days_before REAL NOT NULL,
      a_factor_before REAL NOT NULL,
      next_review_at_before TIMESTAMP,
      last_review_at_before TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_repetitions_topic_time
                      ON topic_repetitions(topic_id, event_at DESC)"])
  ;; The Learn streak query walks backward one calendar day at a time. This
  ;; expression index makes each day-presence check bounded by the streak
  ;; length instead of reading the user's complete repetition history.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_repetitions_user_event_day
                      ON topic_repetitions(user_id, event_type, (DATE(event_at)))"])
  ;; Widen event_type for Dismiss/Undismiss on installs created before them
  ;; (the inline CHECK above only applies to a freshly-created table).
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions DROP CONSTRAINT IF EXISTS topic_repetitions_event_type_check"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ADD CONSTRAINT topic_repetitions_event_type_check
                      CHECK (event_type IN ('advance','touch','postpone','done','restore','priority-change',
                                            'dismiss','undismiss','import'))"])

  ;; 'import' rows come from a SuperMemo collection, which records WHEN each
  ;; repetition happened but not the interval, A-factor or status in force at
  ;; the time. Those three columns become nullable so an imported repetition
  ;; reads as "unknown" rather than carrying an invented value; every native
  ;; writer still supplies them (see log-topic-repetition!).
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN status_before DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN interval_days_before DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE topic_repetitions ALTER COLUMN a_factor_before DROP NOT NULL"])

  ;; Priority carries three decimal places (freememo.priority). Both columns
  ;; started as INTEGER, so widen them on installs created before that.
  ;; The data_type guard is load-bearing: ALTER COLUMN ... TYPE rewrites the
  ;; whole table, and an unguarded statement would rewrite it on every boot.
  (doseq [[table column] [["topics" "priority"] ["topic_repetitions" "priority_before"]]]
    (when (some? (jdbc/execute-one! ds
                   ["SELECT 1 FROM information_schema.columns
                     WHERE table_name = ? AND column_name = ? AND data_type = 'integer'"
                    table column]))
      (tel/log! {:level :info :id ::widen-priority :data {:table table :column column}}
        "Widening priority column to DOUBLE PRECISION")
      (jdbc/execute! ds [(str "ALTER TABLE " table " ALTER COLUMN " column " TYPE DOUBLE PRECISION")])))
  (jdbc/execute! ds ["ALTER TABLE topics ALTER COLUMN priority SET DEFAULT 50"])

  ;; Range backstop for the insert paths that never reach
  ;; freememo.priority/normalize (create-topic!, insert-topic-rows!, the
  ;; migration inserts). PostgreSQL orders NaN above every non-NaN value, so
  ;; `<= 100` rejects NaN and no separate NaN clause is needed — a NaN priority
  ;; would otherwise sort last forever and bury the topic at the foot of the
  ;; learning queue. NULL passes, as it does for every CHECK, so the column
  ;; stays nullable.
  ;;
  ;; PostgreSQL has no ADD CONSTRAINT IF NOT EXISTS, so presence is tested by
  ;; name. Add-when-absent, NOT the DROP-then-ADD used for event_type above:
  ;; ADD CONSTRAINT takes ACCESS EXCLUSIVE and scans the whole table, and
  ;; `topics` is read on every page. Paying that per boot is what the guard on
  ;; the type change above exists to avoid. Changing the RANGE later therefore
  ;; needs an explicit DROP — comparing definition text does not work, because
  ;; PostgreSQL rewrites it (`priority >= (0)::double precision`).
  (when-not (some? (jdbc/execute-one! ds
                     ["SELECT 1 FROM pg_constraint WHERE conname = 'topics_priority_range_check'"]))
    (tel/log! {:level :info :id ::add-priority-range-check} "Adding topics priority range CHECK")
    (jdbc/execute! ds ["ALTER TABLE topics ADD CONSTRAINT topics_priority_range_check
                        CHECK (priority >= 0 AND priority <= 100)"]))

  ;; SuperMemo import provenance. sm_rank is the topic's exact position in the
  ;; source collection's priority order; `priority` holds the same ordering
  ;; rounded to 0-100, and sm_rank restores the precision that rounding loses
  ;; (get-learning-queue orders by it before the daily hash). sm_element_id is
  ;; the element id it came from. Both NULL for natively created topics.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS sm_rank INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS sm_element_id INTEGER"])

  ;; Git forge import provenance (plans/git-forge-integration.md §4.1).
  ;;
  ;; repo_path is the repository-relative path of a 'commit', 'dir' or 'code'
  ;; node: '' on the commit node, 'src/db' on a folder, 'src/db/pool.zig' on a
  ;; file. It is the node's identity under its commit, which is why every one of
  ;; these kinds is in `structural-kinds` and cannot be re-parented.
  ;;
  ;; commit_sha lives on the 'commit' node and repo_url on the 'repo' node, so
  ;; neither repeats across the thousands of rows beneath them. All three are
  ;; NULL for every other kind, and no index exists on them yet: nothing queries
  ;; by path today, and an unused index on a table this size is pure write cost.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS repo_path TEXT"])
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS commit_sha TEXT"])
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS repo_url TEXT"])

  ;; Live Document flag — a kind='pdf' root that the user keeps appending
  ;; camera/upload image pages to. Stays a first-class PDF everywhere; this
  ;; boolean only drives the viewer's add-photos affordance and empty-state.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS is_live BOOLEAN NOT NULL DEFAULT false"])

  ;; Dismiss flag — removes a topic AND its whole subtree from the Learning
  ;; Queue while keeping it in the collection (SuperMemo Dismiss). Orthogonal to
  ;; status: a topic may be both done and dismissed (4 positions). Reversible.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS dismissed BOOLEAN NOT NULL DEFAULT false"])

  ;; A topic the user made with Extract — text or a video range. The
  ;; getting-started checklist reads it (plans/getting-started-checklist.md 1.2).
  ;; `kind` cannot answer this: an EPUB chapter is also a `basic` child.
  ;;
  ;; Only `create-topic!` with `:extract? true` writes it. Deliberately NOT
  ;; backfilled (Q6), so an extract made before this column reads false.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS is_extract BOOLEAN NOT NULL DEFAULT false"])

  ;; WHEN a topic was marked done. `status` alone cannot answer "how many pages
  ;; did I cover today", which is what the Schedule tab's daily quota counts
  ;; down (plans/study-plan-daily-quota.md §5.1).
  ;;
  ;; Deliberately NOT backfilled. NULL reads as "not today", which is the right
  ;; answer for every page marked before this column existed.
  ;;
  ;; Every writer of `status` owns this too: set on the way into 'done', NULL on
  ;; the way out. A row that is done with no timestamp, or timestamped and not
  ;; done, is a writer that forgot.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS done_at TIMESTAMPTZ"])

  ;; Search: plain-text column derived from HTML content, trigram GIN index
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS content_text text"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_content_text_trgm
                      ON topics USING GIN (content_text gin_trgm_ops)"])

  ;; Learn-queue preview. GENERATED, not written by the app, for the reason
  ;; `neoreader_highlights.quote_hash` is: the derivation belongs beside the
  ;; column it reads, where the two cannot drift.
  ;;
  ;; `|| ''` is load-bearing. `left(col, n)` on a TOASTed value asks PostgreSQL
  ;; for a byte slice, and that slice can end in the middle of a multi-byte
  ;; character — which raises `invalid byte sequence for encoding "UTF8"` and
  ;; aborts the WHOLE statement, not the one row. 133 production rows do that
  ;; at n=400. Concatenation forces a full detoast, so no slice is taken.
  ;; Measured on production: this expression runs over all 42379 content-bearing
  ;; rows in 649 ms.
  ;;
  ;; Reads content_text (already stripped) rather than content: 400 characters
  ;; of plain text cost 14 MB across the table, where 4000 characters of HTML
  ;; would cost up to 193 MB, and the consumer takes 80 characters either way.
  ;; See plans/learn-queue-toast-slice-defect.md.
  ;;
  ;; ADD COLUMN of a STORED generated column rewrites the table under an
  ;; ACCESS EXCLUSIVE lock. `IF NOT EXISTS` makes that a one-time cost.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS content_preview TEXT
                      GENERATED ALWAYS AS (left(content_text || '', 400)) STORED"])

  ;; Topic files (binary storage, split from old documents.file_data)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_files (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      file_data BYTEA NOT NULL,
      file_size INTEGER,
      mime_type TEXT
    )"])

  ;; Score topics own TWO blobs (role='main' PDF + role='audio' recording);
  ;; every other kind keeps a single 'main' row. The historical
  ;; UNIQUE(topic_id) relaxes to UNIQUE(topic_id, role) — the DROP CONSTRAINT
  ;; migrates existing installs, the index covers both old and new.
  (jdbc/execute! ds ["ALTER TABLE topic_files ADD COLUMN IF NOT EXISTS role TEXT NOT NULL DEFAULT 'main'"])
  (jdbc/execute! ds ["ALTER TABLE topic_files DROP CONSTRAINT IF EXISTS topic_files_topic_id_key"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_topic_files_topic_role
                      ON topic_files(topic_id, role)"])

  ;; Flashcards table — create if not exists, then add new columns
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS flashcards (
      id SERIAL PRIMARY KEY,
      document_id INTEGER,
      page_number INTEGER,
      kind TEXT NOT NULL,
      question TEXT,
      answer TEXT,
      cloze TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Add all flashcard columns (idempotent)
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS content_item_id INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_note_id BIGINT DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_synced_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT NULL"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS topic_id INTEGER REFERENCES topics(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS root_topic_id INTEGER REFERENCES topics(id) ON DELETE CASCADE"])

  ;; Reconcile baselines — what each SIDE held at the last successful sync.
  ;;
  ;; Two columns, not one, and the pairing is the whole mechanism. `stripped=`
  ;; compares Anki's field against the local field, which reports THAT the two
  ;; sides differ and never WHICH side moved — so a card edited only in FreeMemo
  ;; satisfies both halves of `anki_sync_server/conflict?` and both bulk buttons
  ;; skip it. Comparing each side against its own baseline makes the two
  ;; questions independent.
  ;;
  ;; Neither digest is ever compared with the other. `anki_baseline_digest` is
  ;; always produced from bytes READ BACK from Anki (never from the payload sent,
  ;; which Anki may normalise on write); `local_baseline_digest` is always
  ;; produced from the flashcards row. `freememo.anki-digest` owns both.
  ;; A consequence worth keeping: score and overlapping need no Anki→local
  ;; inverse to participate, because their two digests never meet.
  ;;
  ;; CHAR(64) is hex SHA-256, matching `media.sha256`.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_baseline_digest CHAR(64)"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS local_baseline_digest CHAR(64)"])

  ;; Which generation of the digest functions wrote the two baselines above.
  ;; `anki-digest/digest-version` is the current value and carries the bump rule.
  ;;
  ;; Without this column a change to either digest function moves every card's
  ;; digest without any card changing, and `classify` cannot tell that from real
  ;; edits on both sides — so it reports the whole collection as conflicts and
  ;; nothing can push. That is not hypothetical: version 2 shipped as a bare code
  ;; change and did exactly that to 1459 of one account's 1528 cards.
  ;;
  ;; NULL means version 1 or earlier, which shipped before the column existed.
  ;; `classify` reads any mismatch as `:needs-baseline`, so the recovery is one
  ;; ordinary run: it re-baselines from the current state of both sides, writes
  ;; nothing to Anki, and reports no card as changed.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS baseline_digest_version INTEGER"])

  ;; `anki-digest/local-digest` of the row AS IT STANDS. The baseline above says
  ;; what the last sync agreed on; this says what is true now, and the pair is THE
  ;; definition of "unsynced":
  ;;
  ;;   local_current_digest IS DISTINCT FROM local_baseline_digest
  ;;     OR anki_note_id IS NULL
  ;;
  ;; It exists because that predicate has to be expressible in SQL. Three of the
  ;; five staleness surfaces are aggregates (`get-unsynced-card-count` by topic and
  ;; by root, and `synced_cards` behind the document tree's Unsynced filter), and a
  ;; kind-aware Clojure digest is not a WHERE clause. Before this column they ran a
  ;; timestamp test instead — `updated_at > anki_synced_at` — while the reconcile
  ;; ran the digest. The two disagreed on exactly the edits that move one and not
  ;; the other, and a card caught between them could never be cleared: the count
  ;; said work existed and the runner said none did.
  ;;
  ;; Every content write restamps it, through `restamp-card-digests!` — see
  ;; `update-flashcard!`. A writer that skips that helper leaves a stale digest,
  ;; which is the one failure mode this column has.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS local_current_digest CHAR(64)"])

  ;; The push settings the card's Anki note embeds, snapshotted for the digest.
  ;;
  ;; NOT the source of truth. Push resolves the header through
  ;; `settings/get-anki-header-for-topic` and the bibliography through the
  ;; effective-source walk; these columns record what it resolved.
  ;;
  ;; They exist because a settings edit has to be able to dirty a card, and it had
  ;; no way to. The header lives in two `settings` rows per root topic and the
  ;; bibliography in `sources.csl`, so changing either wrote no flashcards row —
  ;; invisible to the timestamp test AND to the digest.
  ;;
  ;; `write-anki-settings-snapshot!` is the ONLY writer, and it owns the ownership
  ;; clause, the transaction and the batching. Three public fns reach it:
  ;; `set-anki-header-snapshot!` and `set-anki-bibliography-snapshot!` at edit
  ;; time, and `refresh-anki-settings-snapshot!` from the push path just before it
  ;; takes baselines.
  ;;
  ;; NULL header means header off, which digests the same as "" — correct, because
  ;; `prepend-header` writes nothing for a blank header.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_header_text TEXT"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_bibliography_html TEXT"])

  ;; Anki's per-note `mod` at the last sync. Written, read by nothing.
  ;; Measured on production: 58 of 1477 cards carried a `mod` newer than
  ;; `anki_synced_at` and NONE of them had a content difference — tag writes move
  ;; it. It is recorded so a future `notesModTime` fast path is a data question
  ;; rather than a migration, and it must not become a change signal.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_baseline_mod BIGINT"])

  ;; When reconcile last classified this card as a conflict, NULL when it is not
  ;; one. A timestamp rather than a boolean, so the Library row can say how long
  ;; the card has been stuck. Server-side state on purpose: a conflict must
  ;; survive a reload without re-running a whole reconcile to rediscover it.
  ;; Cleared by resolving the conflict, and by any run that finds the card no
  ;; longer conflicted.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS anki_conflict_at TIMESTAMP"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_anki_conflict
                      ON flashcards(root_topic_id) WHERE anki_conflict_at IS NOT NULL"])

  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_topic ON flashcards(topic_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_root_topic ON flashcards(root_topic_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_anki_note_id ON flashcards(anki_note_id) WHERE anki_note_id IS NOT NULL"])

  ;; Anki notes whose local card is gone — the durable half of a deletion.
  ;;
  ;; `card-components/try-delete-anki-notes!` already deletes the notes from the
  ;; client on every local-delete path, and it is fire-and-forget: with Anki
  ;; closed, the note survives with nothing left pointing at it. Ten such orphans
  ;; were counted in production, three of them under a topic that no longer
  ;; exists. This table is what lets the next reconcile finish the job.
  ;;
  ;; `anki_baseline_digest` is snapshotted from the card BEFORE it goes, because
  ;; reconcile must still ask "did Anki's copy change since we last synced?" —
  ;; and by then there is no row left to ask. Without it every tombstone would
  ;; resolve to a conflict and no deletion would ever propagate.
  ;;
  ;; UNIQUE(user_id, anki_note_id) with ON CONFLICT DO NOTHING makes recording
  ;; idempotent: deleting a card twice, or a retry after a failed run, adds
  ;; nothing. user_id is the scope — a note id means nothing outside the
  ;; collection it belongs to.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS anki_note_tombstones (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      anki_note_id BIGINT NOT NULL,
      anki_baseline_digest CHAR(64),
      deleted_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_anki_tombstones_user_note
                      ON anki_note_tombstones(user_id, anki_note_id)"])

  ;; Run migration if old tables exist
  (when (old-tables-exist?)
    (migrate-to-topics!))

  ;; After migration: drop legacy columns if they still exist
  (when (and (topics-table-exists?)
          (not (old-tables-exist?)))
    ;; Drop legacy flashcard columns (safe — migration already moved data)
    (try
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS document_id"])
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS page_number"])
      (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS content_item_id"])
      (catch Exception _ nil)))
  ;; The flashcard uniqueness indexes are NOT created here. Removing the
  ;; duplicates that block them re-points card_schedules, kg_reviews and
  ;; anki_card_states, and on a fresh database those tables do not exist yet.
  ;; See setup-schema-flashcard-uniqueness!, the last step of setup-schema.

  ;; Fill `local_current_digest` for rows that predate the column (idempotent).
  ;; Baselines are deliberately left alone — see the fn's docstring for why the
  ;; first reconcile after this deploy re-pushes the collection once.
  (let [n (backfill-local-card-digests!)]
    (when (pos? n)
      (tel/log! {:level :info :id ::backfill-local-card-digests :data {:count n}}
        (str "Backfilled " n " card digest(s)"))))
)

(defn- setup-schema-activity!
  "User events, the media registry, and topic pins (pins FK into media).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; User events (activity tracking)
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS user_events (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      event_type TEXT NOT NULL,
      metadata JSONB,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_user_events_user_type
                      ON user_events (user_id, event_type, created_at DESC)"])

  ;; Media registry — per-user blobs (images today; audio/video/file reserved).
  ;; Served via /api/media/:id with per-user auth. Deduped by (user_id, sha256).
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS media (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('image','audio','video','file')),
      bytes BYTEA NOT NULL,
      mime_type TEXT NOT NULL,
      sha256 CHAR(64) NOT NULL,
      byte_size INTEGER NOT NULL,
      source_url TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_media_user_sha
                      ON media(user_id, sha256)"])

  ;; Topic pins — up to 2 image references per topic with front/back placement.
  ;; K1 cap enforced in the insert layer (set-pin!). EC-snapshot: extract creation
  ;; copies parent's rows into child via copy-pins-to-child!.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_pins (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      placement TEXT NOT NULL CHECK (placement IN ('front','back')),
      ord SMALLINT NOT NULL DEFAULT 0
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_pins_topic ON topic_pins(topic_id)"])
)

(defn delete-messageless-assistant-chats!
  "Delete every assistant chat that holds no message. Returns the count.

   A chat now comes into being with its first message, so a chat with none is
   a legacy \"＋ New\" row or a first send that failed before its cleanup
   existed. Both are noise in the history view. Runs on every boot and is
   idempotent: the colour still serving during a blue/green deploy can create
   one more such row after this boot's pass."
  []
  (or (::jdbc/update-count
        (jdbc/execute-one! ds ["DELETE FROM assistant_chats c
                                 WHERE NOT EXISTS (SELECT 1 FROM assistant_messages m
                                                    WHERE m.chat_id = c.id)"]))
      0))

(defn- setup-schema-assistant!
  "AI assistant chats, message transcripts, durable runs, and evidence.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Socratic AI assistant — per-document chats + their message transcripts.
  ;; A chat is scoped to (user_id, root_topic_id): the document being read.
  ;; The transcript holds learner/assistant turns only; the learner's live
  ;; reading context is retained with each run, not stored as transcript rows.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_chats (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      title TEXT,
      web_search BOOLEAN NOT NULL DEFAULT FALSE,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_chats_user_topic
                      ON assistant_chats(user_id, root_topic_id)"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_messages (
      id SERIAL PRIMARY KEY,
      chat_id INTEGER NOT NULL REFERENCES assistant_chats(id) ON DELETE CASCADE,
      role TEXT NOT NULL CHECK (role IN ('user','assistant')),
      content TEXT NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; Client-generated id for the optimistic echo of a user turn. The client
  ;; renders the sent turn immediately and retires that echo once the persisted
  ;; row bearing this id appears in the transcript — a globally-unique key that
  ;; survives the round trip. NULL on assistant/server-originated rows.
  (jdbc/execute! ds ["ALTER TABLE assistant_messages ADD COLUMN IF NOT EXISTS client_id TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_messages_chat
                      ON assistant_messages(chat_id, id)"])
  (delete-messageless-assistant-chats!)
  ;; Persona for this chat, an :id from freememo.assistant-modes/registry. Mode
  ;; is per chat so a transcript is one persona end to end. NULL on every chat
  ;; that predates the column, which resolve-mode maps to the Socratic default —
  ;; correct, since Socratic was the only persona that ever existed before it.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS mode TEXT"])
  ;; Web search is a per-chat preference. Existing chats retain the safe default:
  ;; no external lookup unless the learner turns it on.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS web_search BOOLEAN NOT NULL DEFAULT FALSE"])
  ;; An opaque chat identity gives OpenRouter a stable sticky-routing key. It is
  ;; not derived from a user id, chat id, or document id.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS provider_session_id TEXT"])
  (jdbc/execute! ds ["UPDATE assistant_chats
                         SET provider_session_id = md5(random()::text || clock_timestamp()::text || id::text)
                       WHERE provider_session_id IS NULL"])
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ALTER COLUMN provider_session_id SET NOT NULL"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_assistant_chats_provider_session
                      ON assistant_chats(provider_session_id)"])
  ;; History view (plans/assistant-chat-history.md §1). TIMESTAMP, matching the
  ;; table's other two columns; the list query converts all of them to epoch
  ;; milliseconds at one boundary. last_opened_at is a column rather than a
  ;; settings key because it moves through an account import with no id remap.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS pinned_at TIMESTAMP"])
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS last_opened_at TIMESTAMP"])

  ;; A run is the authority for one answer or one /compact operation. Response
  ;; text is append-only while active. Terminal transitions close that boundary.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_runs (
      id BIGSERIAL PRIMARY KEY,
      run_uuid TEXT NOT NULL UNIQUE,
      chat_id INTEGER NOT NULL REFERENCES assistant_chats(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('answer','compact')),
      status TEXT NOT NULL CHECK (status IN
        ('queued','streaming','stop-requested','stopped','completed','failed')),
      user_message_id INTEGER REFERENCES assistant_messages(id) ON DELETE CASCADE,
      assistant_message_id INTEGER REFERENCES assistant_messages(id) ON DELETE CASCADE,
      retry_of_run_uuid TEXT,
      provider_session_id TEXT NOT NULL,
      requested_model TEXT,
      resolved_model TEXT,
      provider_name TEXT,
      generation_id TEXT,
      request_snapshot JSONB NOT NULL DEFAULT '{}'::jsonb,
      source_snapshot JSONB NOT NULL DEFAULT '[]'::jsonb,
      response_text TEXT NOT NULL DEFAULT '',
      error_text TEXT,
      input_tokens INTEGER,
      output_tokens INTEGER,
      reasoning_tokens INTEGER,
      cached_tokens INTEGER,
      cache_write_tokens INTEGER,
      cost_usd NUMERIC(16,8),
      provider_latency_ms BIGINT,
      generation_ms BIGINT,
      first_token_ms BIGINT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      started_at TIMESTAMP,
      first_token_at TIMESTAMP,
      stopped_at TIMESTAMP,
      completed_at TIMESTAMP,
      failed_at TIMESTAMP,
      CHECK ((kind = 'answer' AND user_message_id IS NOT NULL AND assistant_message_id IS NOT NULL)
          OR (kind = 'compact' AND user_message_id IS NULL AND assistant_message_id IS NULL))
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_runs_chat
                      ON assistant_runs(chat_id, id)"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_assistant_runs_one_active
                      ON assistant_runs(chat_id)
                      WHERE status IN ('queued','streaming','stop-requested')"])

  ;; A checkpoint is a completed compaction event. Covered messages remain in
  ;; the transcript. Future requests replace that range with this summary.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_checkpoints (
      id BIGSERIAL PRIMARY KEY,
      chat_id INTEGER NOT NULL REFERENCES assistant_chats(id) ON DELETE CASCADE,
      run_id BIGINT NOT NULL UNIQUE REFERENCES assistant_runs(id) ON DELETE CASCADE,
      summary TEXT NOT NULL,
      covered_from_message_id INTEGER NOT NULL REFERENCES assistant_messages(id) ON DELETE CASCADE,
      covered_through_message_id INTEGER NOT NULL REFERENCES assistant_messages(id) ON DELETE CASCADE,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      CHECK (covered_from_message_id <= covered_through_message_id)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_checkpoints_chat
                      ON assistant_checkpoints(chat_id, id)"])

  ;; Evidence is separate from rendered answer text. The model cites citation_key
  ;; values. Navigation uses retained targets and degrades when live content moves.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS assistant_evidence (
      id BIGSERIAL PRIMARY KEY,
      run_id BIGINT NOT NULL REFERENCES assistant_runs(id) ON DELETE CASCADE,
      citation_key TEXT NOT NULL,
      document_topic_id INTEGER,
      page_topic_id INTEGER,
      page_number INTEGER,
      anchor TEXT,
      quoted_text TEXT,
      quoted_html TEXT,
      document_title TEXT NOT NULL,
      display_label TEXT NOT NULL,
      source_revision TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      UNIQUE(run_id, citation_key)
    )"])
  (jdbc/execute! ds ["ALTER TABLE assistant_evidence
                      ADD COLUMN IF NOT EXISTS quoted_html TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_evidence_run
                      ON assistant_evidence(run_id, citation_key)"])
  ;; No provider worker survives a JVM restart. Close runs that the previous
  ;; process owned, preserve their last stored text, and make them retryable.
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx
      [(str "UPDATE assistant_messages m SET content = r.response_text "
         "FROM assistant_runs r WHERE m.id = r.assistant_message_id "
         "AND r.status IN ('queued','streaming','stop-requested')")])
    (jdbc/execute! tx
      [(str "UPDATE assistant_runs SET status = 'failed', "
         "error_text = 'Interrupted by server restart.', failed_at = CURRENT_TIMESTAMP "
         "WHERE status IN ('queued','streaming','stop-requested')")]))
)

(defn- setup-schema-annotations!
  "Image-occlusion groups, audio/score groups, and the overlapping-cloze
   column — the three non-basic flashcard kinds' supporting tables/columns.
   Both groups FK into media, so this runs after setup-schema-activity!.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Occlusion groups — one row per image-occlusion authoring session.
  ;; geometry JSONB = {:width :height :rects [{:x :y :w :h :ordinal} ...]}
  ;; in natural-image pixels. An ordinal identifies a MASK GROUP, so several
  ;; rects may share one and sharing it is the grouping. next_ordinal is the
  ;; single ordinal authority: ordinals are assigned from it and NEVER reused,
  ;; because they bind the per-mask-group flashcard row ↔ Anki note ↔ SVG rect
  ;; id. anki_key names the group's Anki media files (fm-<anki_key>-…) and note IDs.
  ;; Placed after media (image FK) and before the flashcards ALTERs below.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS occlusion_groups (
      id SERIAL PRIMARY KEY,
      anki_key TEXT NOT NULL,
      image_media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      mode TEXT NOT NULL CHECK (mode IN ('hide-all','hide-one')),
      geometry JSONB NOT NULL,
      next_ordinal INTEGER NOT NULL DEFAULT 1,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS occlusion_group_id INTEGER REFERENCES occlusion_groups(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS mask_ordinal INTEGER"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS io_fields JSONB"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_occlusion_group
                      ON flashcards(occlusion_group_id) WHERE occlusion_group_id IS NOT NULL"])
  ;; One row per mask ordinal — the grouping invariant, enforced rather than
  ;; conventional. Guarded like the other unique indexes above: a legacy
  ;; duplicate must not stop the app from booting.
  (try
    (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_flashcards_mask_ordinal
                        ON flashcards(occlusion_group_id, mask_ordinal)
                        WHERE occlusion_group_id IS NOT NULL"])
    (catch Exception e
      (tel/log! {:level :warn :id ::mask-ordinal-index :data {:error (.getMessage e)}}
        "Could not create uq_flashcards_mask_ordinal — duplicate mask ordinals exist")))

  ;; Score groups — one row per (audio segment × notation rects) pair on a
  ;; kind='score' topic. geometry JSONB = {:pages [{:page :width :height
  ;; :rects [{:x :y :w :h :ordinal :media-id} ...]} ...]} in snapshot-pixel
  ;; space of the recorded page render. Clip + crops are materialized media
  ;; rows (cut/cropped client-side at card creation, NOT at push time).
  ;; anki_key names the group's Anki media files (fm-score-<anki_key>-…).
  ;; One flashcard row per direction ('audio-front'/'sheet-front'); 'Both'
  ;; fans out to two notes sharing the same media.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS score_groups (
      id SERIAL PRIMARY KEY,
      anki_key TEXT NOT NULL,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      clip_media_id BIGINT NOT NULL REFERENCES media(id) ON DELETE CASCADE,
      geometry JSONB NOT NULL,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT NULL
    )"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS score_group_id INTEGER REFERENCES score_groups(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS score_direction TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_flashcards_score_group
                      ON flashcards(score_group_id) WHERE score_group_id IS NOT NULL"])

  ;; Overlapping cloze — one row per authored list. The overlapping JSONB is the
  ;; whole card: {:question :items [...] :settings {...} :fields {:Original :Full
  ;; :Text1 ...}}. :items+:settings are the source edits write to; :fields is
  ;; the materialized Anki-field expansion (freememo.overlapping/expand),
  ;; re-derived on every save. question/answer/cloze stay NULL for this kind.
  (jdbc/execute! ds ["ALTER TABLE flashcards ADD COLUMN IF NOT EXISTS overlapping JSONB"])
)

(defn- setup-schema-sources!
  "Bibliography: the sources table, topics.source_id, the source_url/
   source_reference backfill + cleanup, and legacy title canonicalization.
   Must run after setup-schema-topics! (backfills read/write topics rows).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Drop deprecated source_reference columns — title is the single source of truth.
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS source_reference"])
  (jdbc/execute! ds ["ALTER TABLE flashcards DROP COLUMN IF EXISTS source_reference"])

  ;; Sources — bibliography records, decoupled from the FM topic tree.
  ;; One row per origin (wiki article, PDF, book). Many topics may cite the
  ;; same source via topics.source_id. csl JSONB stores the full CSL-JSON
  ;; record; url + title denormalize the hot fields for indexing.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS sources (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      csl_type TEXT NOT NULL,
      csl JSONB NOT NULL DEFAULT '{}'::jsonb,
      url TEXT,
      title TEXT,
      container_title TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; Idempotent column add for installs that created the table without container_title.
  (jdbc/execute! ds ["ALTER TABLE sources ADD COLUMN IF NOT EXISTS container_title TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_sources_user_url ON sources(user_id, url)"])

  ;; topics.source_id — FK into sources. Nullable: a topic may have no
  ;; bibliography. ON DELETE SET NULL preserves topic content if a source row
  ;; is deleted (sources are reference data; topics are user content).
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS
                       source_id INTEGER REFERENCES sources(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_source ON topics(source_id)"])

  ;; Capture dedupe. Index the DIGEST, never the text — a btree row cannot
  ;; exceed about 2704 bytes and `content_text` routinely does, which is the
  ;; same reason `uq_flashcards_question` indexes `md5(question)`.
  ;;
  ;; Scoped by source_id rather than user_id: the question a capture asks is
  ;; "does this user already hold this page's content under this URL", and
  ;; `sources` is where the URL lives. That also keeps the index selective —
  ;; one source has a handful of topics, one user has tens of thousands.
  ;;
  ;; It lives here, not beside `content_text` in setup-schema-topics!, because
  ;; the ALTER just above adds `source_id`. setup-schema-topics! runs before
  ;; this fn, so on a fresh database the index failed there with
  ;; `column "source_id" does not exist`.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_source_content_digest
                      ON topics (source_id, md5(content_text))
                      WHERE source_id IS NOT NULL AND content_text IS NOT NULL"])

  ;; Backfill sources from legacy topics.source_url (idempotent — only runs
  ;; on rows where source_url IS NOT NULL AND source_id IS NULL).
  (backfill-sources!)

  ;; Backfill document sources for legacy PDF root topics with no source row
  ;; (idempotent — only touches kind='pdf' rows where source_id IS NULL).
  (backfill-pdf-sources!)

  ;; Drop the legacy topics.source_url column. All bibliography lives in
  ;; `sources` now; topics reference via source_id. Runs after backfill so
  ;; existing URL data is migrated, not lost.
  (jdbc/execute! ds ["ALTER TABLE topics DROP COLUMN IF EXISTS source_url"])

  ;; Canonicalize legacy titles: replace _ with space, strip trailing .pdf,
  ;; collapse whitespace, trim. Idempotent — clean rows are skipped by WHERE.
  (let [pretty-sql "btrim(regexp_replace(
                            regexp_replace(replace(title, '_', ' '), '\\.pdf$', '', 'i'),
                            '\\s+', ' ', 'g'))"
        result (jdbc/execute-one! ds
                 [(str "WITH updated AS (
                          UPDATE topics SET title = " pretty-sql "
                          WHERE title <> " pretty-sql "
                          RETURNING 1
                        )
                        SELECT COUNT(*) AS n FROM updated")])
        n (or (:n result) 0)]
    (when (pos? n)
      (tel/log! {:level :info :id ::prettify-titles :data {:count n}}
        (str "Prettified " n " legacy title(s)"))))

  ;; Backfill content_text for existing rows (idempotent)
  (backfill-content-text!)
)

(defn- setup-schema-credits!
  "Pass-through AI billing: per-user balance/markup columns, credit_orders,
   credit_transactions.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Credits — pass-through AI billing (official deployment only).
  ;; Per-user IQD balance (denormalized, mirrors usage_bytes) + append-only
  ;; ledger (source of truth: SUM(amount_iqd) per user == credit_balance_iqd)
  ;; + Wayl order tracking (reference_id UNIQUE = webhook idempotency).
  ;; See plans/credits-wayl-payment-system.md §5.1.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS credit_balance_iqd BIGINT NOT NULL DEFAULT 0"])
  ;; Per-user markup override (nullable). NULL = use the config default markup;
  ;; a value replaces it for this user's billing (see freememo.credits/resolve-markup).
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS markup_override DECIMAL"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS credit_orders (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      reference_id TEXT NOT NULL UNIQUE,
      amount_iqd BIGINT NOT NULL,
      status TEXT NOT NULL DEFAULT 'pending'
        CHECK (status IN ('pending','complete','failed')),
      wayl_code TEXT,
      wayl_link_id TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      completed_at TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_credit_orders_user
                      ON credit_orders(user_id, created_at DESC)"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS credit_transactions (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('purchase','debit','grant','adjustment')),
      amount_iqd BIGINT NOT NULL,
      balance_after BIGINT NOT NULL,
      endpoint TEXT,
      model TEXT,
      input_tokens INTEGER,
      cached_tokens INTEGER,
      output_tokens INTEGER,
      reasoning_tokens INTEGER,
      attempts INTEGER,
      order_reference_id TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_credit_transactions_user
                      ON credit_transactions(user_id, created_at DESC)"])
)

(defn- setup-schema-card-versions!
  "Card edit history — superseded card/group renditions. Must run after
   setup-schema-topics! (FKs topics). See plans/card-edit-history.md.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Write-behind version log: a row holds the rendition a save REPLACED, so
  ;; the current rendition is never duplicated here and the pre-edit state of
  ;; an LLM-generated card is captured on its first edit for free.
  ;; superseded_at therefore means "stopped being current at"; rendition k's
  ;; validity interval is [row(k-1).superseded_at, row(k).superseded_at), with
  ;; flashcards.created_at opening the first.
  ;;
  ;; Scope is (scope_type, scope_id), NOT an FK. Two reasons:
  ;; reconcile-occlusion-group! deletes flashcard rows whose mask ordinal
  ;; disappears, so an FK to flashcards would erase a mask's history during an
  ;; ordinary EDIT; and one scope column pair needs one index where three
  ;; nullable id columns would need three partial ones (~105 vs ~138 B/row).
  ;; root_topic_id is the only FK — history is reclaimed with the document.
  ;;
  ;; Payload columns mirror their source columns and are sparse: scope_type
  ;; selects which are non-null. geometry is shared by both group scopes
  ;; (occlusion rects vs score pages — scope_type disambiguates the shape).
  ;; occlusion_image_media_id is copied, not joined: remove-occlusion-mask!
  ;; deletes the group row once its last mask card is gone, and the version
  ;; must still render after that.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS card_versions (
      id BIGSERIAL PRIMARY KEY,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      scope_type TEXT NOT NULL CHECK (scope_type IN ('card','occlusion_group','score_group')),
      scope_id BIGINT NOT NULL,
      kind TEXT NOT NULL,
      superseded_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      question TEXT,
      answer TEXT,
      cloze TEXT,
      overlapping JSONB,
      io_fields JSONB,
      geometry JSONB,
      occlusion_mode TEXT,
      occlusion_image_media_id BIGINT,
      score_direction TEXT,
      score_start_ms INTEGER,
      score_end_ms INTEGER,
      score_clip_media_id BIGINT
    )"])
  ;; Reads are always "newest first, within one scope" — the composite index
  ;; serves both the ordering and the scope filter. id DESC breaks ties when
  ;; two versions share a microsecond (no sequence counter needed).
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_versions_scope
                      ON card_versions(scope_type, scope_id, superseded_at DESC, id DESC)"])
  ;; Unindexed FK referencing columns make ON DELETE CASCADE table-scan.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_versions_root_topic
                      ON card_versions(root_topic_id)"])
)

(defn- setup-schema-undo!
  "The undo log and topics.staged_delete_id (which FKs into it) — must run
   after setup-schema-topics! (topics must already exist).
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Undo log — per-user action history for the Undo feature.
  ;; entity_refs/snapshot are JSONB; occurred_at drives the 12h window,
  ;; undone_at IS NULL marks an entry still reversible. Ordinary rows are
  ;; capped at 100; staged deletions stay until their root is purged.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS undo_log (
      id BIGSERIAL PRIMARY KEY,
      user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      action_type TEXT NOT NULL CHECK (action_type IN
        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document','delete-curriculum','move-topic',
         'reject-question','reject-fact','bulk-reject-facts','mark-duplicate-facts',
         'delete-assistant-chat')),
      entity_type TEXT NOT NULL CHECK (entity_type IN ('flashcard','pin','setting','document','curriculum','question','fact',
                    'assistant-chat')),
      entity_refs JSONB NOT NULL,
      snapshot JSONB NOT NULL,
      occurred_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      undone_at TIMESTAMPTZ
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_undo_log_user
                      ON undo_log(user_id, occurred_at DESC)"])
  ;; Widen the CHECK lists on installs created before 'delete-document'/'document'
  ;; and before 'mark-duplicate-facts' (CREATE TABLE IF NOT EXISTS won't alter a
  ;; live constraint). Without the widening the duplicate sweep's undo insert
  ;; throws INSIDE its transaction, rolling the whole sweep back — an unmarkable
  ;; document with nothing in the log to explain it.
  (jdbc/execute! ds ["ALTER TABLE undo_log DROP CONSTRAINT IF EXISTS undo_log_action_type_check"])
  (jdbc/execute! ds ["ALTER TABLE undo_log ADD CONSTRAINT undo_log_action_type_check
                      CHECK (action_type IN
                        ('delete-card','bulk-delete-cards','remove-pin','reset-prompt','delete-document','delete-curriculum','move-topic',
                         'reject-question','reject-fact','bulk-reject-facts','mark-duplicate-facts',
         'delete-assistant-chat'))"])
  (jdbc/execute! ds ["ALTER TABLE undo_log DROP CONSTRAINT IF EXISTS undo_log_entity_type_check"])
  (jdbc/execute! ds ["ALTER TABLE undo_log ADD CONSTRAINT undo_log_entity_type_check
                      CHECK (entity_type IN ('flashcard','pin','setting','document','curriculum','question','fact',
                    'assistant-chat'))"])

  ;; Staged-delete marker: a topic (and its whole subtree) hidden pending the
  ;; 12h undo window points at the undo_log entry that staged it. ON DELETE
  ;; SET NULL — see prune/purge: only the time-based purge removes a
  ;; delete-document entry, and it hard-deletes the topics first.
  (jdbc/execute! ds ["ALTER TABLE topics ADD COLUMN IF NOT EXISTS staged_delete_id BIGINT
                      REFERENCES undo_log(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topics_staged_delete
                      ON topics(staged_delete_id) WHERE staged_delete_id IS NOT NULL"])
  ;; The same marker on assistant chats. Added here rather than in
  ;; setup-schema-assistant!, which runs before undo_log exists.
  (jdbc/execute! ds ["ALTER TABLE assistant_chats ADD COLUMN IF NOT EXISTS staged_delete_id BIGINT
                      REFERENCES undo_log(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_assistant_chats_staged_delete
                      ON assistant_chats(staged_delete_id) WHERE staged_delete_id IS NOT NULL"])
)

(defn- setup-schema-kg!
  "Knowledge graph: entities, predicates, facts, question bank (+ FSRS
   columns), question↔fact links, quiz/exam sessions, answers, review log,
   and the graph-layout cache. See plans/knowledge-graph-quizzes.md.
   Split out of setup-schema (see 7.4); called from it in order."
  []
  ;; Knowledge graph — LLM-distilled facts with human review (see
  ;; plans/knowledge-graph-quizzes.md). Physical store is annotated rows;
  ;; the Ontop service (ontop/) publishes approved rows as orthodox RDF.
  ;; IRIs are DERIVED, never stored: urn:freememo:{user_id}:entity/{id},
  ;; :predicate/{slug}, urn:freememo:fact/{id} — the OBDA mapping owns the
  ;; derivation, so rows and RDF cannot drift.

  ;; Entities — graph nodes. label/aliases double as the quiz keyword lexicon.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_entities (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      label TEXT NOT NULL,
      aliases TEXT[] NOT NULL DEFAULT '{}',
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_entities_user ON kg_entities(user_id)"])
  ;; Trigram index feeds the entity-linking shortlist during extraction.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_entities_label_trgm
                      ON kg_entities USING gin (label gin_trgm_ops)"])

  ;; Predicates — per-user vocabulary, born empty; extraction proposes, the
  ;; review queue approves. slug CHECK enforces IRI-safety at the boundary so
  ;; no insert site has to.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_predicates (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      slug TEXT NOT NULL CHECK (slug ~ '^[a-z0-9]+(-[a-z0-9]+)*$'),
      label TEXT NOT NULL,
      description TEXT,
      status TEXT NOT NULL DEFAULT 'proposed'
        CHECK (status IN ('proposed','approved','rejected')),
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      reviewed_at TIMESTAMP,
      UNIQUE (user_id, slug)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_predicates_user_status
                      ON kg_predicates(user_id, status)"])

  ;; Facts — one row per statement. Invariants: exactly one of
  ;; object_entity_id / object_literal (CHECK); no duplicate s/p/o within one
  ;; document (partial unique indexes below — writers must ON CONFLICT DO
  ;; NOTHING). graph_topic_id = root document topic (the named graph);
  ;; page_number = provenance. Only status='approved' rows are published via
  ;; SPARQL, feed question generation, or feed grading.
  ;;
  ;; status 'duplicate' is written ONLY by freememo.kg-dedup's sweep: a NEAR
  ;; duplicate the two partial unique indexes below cannot see, because they key
  ;; on entity ids and a predicate id and the model rewords all three between
  ;; runs. Measured on production: those indexes caught 4 of 49 labelled
  ;; duplicates (recall 0.08). It is deliberately NOT 'rejected' — a human
  ;; judgement and a 0.90-precision detector must stay tellable apart, and the
  ;; Facts view filters on exactly this distinction.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_facts (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      subject_entity_id INTEGER NOT NULL REFERENCES kg_entities(id) ON DELETE CASCADE,
      predicate_id INTEGER NOT NULL REFERENCES kg_predicates(id),
      object_entity_id INTEGER REFERENCES kg_entities(id) ON DELETE CASCADE,
      object_literal TEXT,
      object_datatype TEXT,
      graph_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      page_number INTEGER,
      status TEXT NOT NULL DEFAULT 'proposed'
        CHECK (status IN ('proposed','approved','rejected','duplicate')),
      source_model TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      reviewed_at TIMESTAMP,
      CHECK ((object_entity_id IS NULL) <> (object_literal IS NULL))
    )"])
  ;; Widen the status CHECK on tables that predate 'duplicate'. CREATE TABLE IF
  ;; NOT EXISTS above is a no-op on an existing table, so its inline CHECK never
  ;; reaches one — without this DROP/ADD the sweep's UPDATE fails with a
  ;; constraint violation on every database created before this line. Inline
  ;; CHECKs are auto-named <table>_<column>_check.
  (jdbc/execute! ds ["ALTER TABLE kg_facts DROP CONSTRAINT IF EXISTS kg_facts_status_check"])
  (jdbc/execute! ds ["ALTER TABLE kg_facts ADD CONSTRAINT kg_facts_status_check
                      CHECK (status IN ('proposed','approved','rejected','duplicate'))"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_graph_status
                      ON kg_facts(graph_topic_id, status)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_user_status
                      ON kg_facts(user_id, status)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_subject
                      ON kg_facts(subject_entity_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_facts_object_entity
                      ON kg_facts(object_entity_id) WHERE object_entity_id IS NOT NULL"])
  ;; Dedup within a document; the same s/p/o from a second document is a new
  ;; row (distinct provenance). md5() keeps long literals under btree limits.
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_kg_facts_spo_entity
                      ON kg_facts(subject_entity_id, predicate_id, object_entity_id, graph_topic_id)
                      WHERE object_entity_id IS NOT NULL"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_kg_facts_spo_literal
                      ON kg_facts(subject_entity_id, predicate_id, md5(object_literal), graph_topic_id)
                      WHERE object_literal IS NOT NULL"])

  ;; Per-page extraction log — what makes a re-distill skip pages it already did,
  ;; and what makes a cancelled or timed-out run resumable instead of total loss.
  ;; See plans/kg-page-skip-extraction.md.
  ;;
  ;; `page_topic_id` is UNIQUE on its own: a page topic has exactly one parent, so
  ;; it belongs to exactly one document. The wiki/web documents that have no page
  ;; children point it at the root topic itself, which is why this table has no
  ;; nullable page_number and no `IS NULL` special case anywhere.
  ;;
  ;; `fact_count IS NULL` means "extracted, not yet linked" — the fail-open marker
  ;; the whole design rests on. A row in that state is only skippable while its
  ;; staged triples still exist (see pages-needing-extraction), so an account
  ;; import, which drops kg_staged_triples as in-flight state, arrives with those
  ;; pages eligible again rather than marked done with no facts. Its VALUE counts
  ;; the triples linking built for the page and overcounts rows the fact indexes
  ;; dropped, so it is the marker and NOT a display figure — the Knowledge views
  ;; read fact counts from kg_facts.
  ;;
  ;; `attempt_count` is scoped to `content_digest`: a failure records the digest it
  ;; failed on so that editing the page resets the count (see
  ;; record-page-extraction-failure!). A failure row is therefore digest-matching
  ;; AND unlinked AND unstaged, which the selector reads as "retry, until the cap".
  ;;
  ;; `prompt_version`, `extract_model_id` and the predicate vocabulary size are
  ;; RECORDED, never compared. Gating the skip on the vocabulary would make it
  ;; almost never fire, because the vocabulary grows on every curation.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_page_extractions (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      graph_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      page_topic_id INTEGER NOT NULL UNIQUE REFERENCES topics(id) ON DELETE CASCADE,
      content_digest TEXT NOT NULL,
      digest_version INTEGER NOT NULL,
      fact_count INTEGER,
      attempt_count INTEGER NOT NULL DEFAULT 0,
      last_error TEXT,
      prompt_version TEXT,
      extract_model_id TEXT,
      extracted_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_page_extractions_graph
                      ON kg_page_extractions(graph_topic_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_page_extractions_user
                      ON kg_page_extractions(user_id)"])

  ;; Raw pass-1 output, awaiting pass-2 entity linking. Labels only — no entity
  ;; ids, because linking has not run yet.
  ;;
  ;; This table exists so extraction can persist per PAGE while linking stays ONE
  ;; batched call per invocation. Linking per page would multiply LLM calls by page
  ;; count, and would match each page's labels against a nearly empty kg_entities,
  ;; which under-merges and creates duplicate entities.
  ;;
  ;; The XOR CHECK mirrors kg_facts': it enforces kg-extract/valid-triple?'s shape
  ;; at the boundary, so no reader of a staged row has to re-check it.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_staged_triples (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      page_extraction_id INTEGER NOT NULL
        REFERENCES kg_page_extractions(id) ON DELETE CASCADE,
      subject_label TEXT NOT NULL,
      predicate_label TEXT NOT NULL,
      object_label TEXT,
      object_literal TEXT,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      CHECK ((object_label IS NULL) <> (object_literal IS NULL))
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_staged_triples_extraction
                      ON kg_staged_triples(page_extraction_id)"])

  ;; Question bank — generated from approved facts, curated by exception
  ;; (edit/reject); 'retired' marks questions whose underlying fact was
  ;; rejected. kind: 'atomic' covers one fact, 'synthesis' spans an entity's
  ;; neighborhood. Links live in kg_question_facts; a hard fact delete
  ;; (entity-merge dedup) cascades the link rows only.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_questions (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('atomic','synthesis')),
      question TEXT NOT NULL,
      reference_answer TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'approved'
        CHECK (status IN ('approved','rejected','retired')),
      source_model TEXT,
      created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_user_status
                      ON kg_questions(user_id, status)"])
  ;; FSRS-6 per-question scheduling state (see freememo.fsrs). fsrs_state:
  ;; 1=Learning 2=Review 3=Relearning. fsrs_due NULL = never introduced — a
  ;; "new" card, eligible only through the daily new-card cap. Cold-start (no
  ;; backfill): every existing question keeps NULL memory + NULL due and so is
  ;; treated as new. The Review quiz is the only writer; custom quiz/exam skip it.
  (doseq [col ["fsrs_stability REAL"
               "fsrs_difficulty REAL"
               "fsrs_due TIMESTAMPTZ"
               "fsrs_state SMALLINT NOT NULL DEFAULT 1"
               "fsrs_step SMALLINT DEFAULT 0"
               "fsrs_reps INTEGER NOT NULL DEFAULT 0"
               "fsrs_lapses INTEGER NOT NULL DEFAULT 0"
               "fsrs_last_review TIMESTAMPTZ"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_questions ADD COLUMN IF NOT EXISTS " col)]))
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_fsrs_due
                      ON kg_questions(user_id, fsrs_due)"])
  ;; Curation flags — orthogonal to status, both reversible (mirrors
  ;; topics.dismissed). flagged = "revisit this question later"; it never filters a
  ;; draw. suspended = withheld from every draw until cleared. Deliberately NOT
  ;; folded into status: the fact reject/restore path rewrites status
  ;; ('retired'/'approved'), which would silently clobber a suspension stored there.
  (doseq [col ["flagged BOOLEAN NOT NULL DEFAULT false"
               "suspended BOOLEAN NOT NULL DEFAULT false"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_questions ADD COLUMN IF NOT EXISTS " col)]))
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_questions_suspended
                      ON kg_questions(user_id) WHERE suspended"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_question_facts (
      question_id INTEGER NOT NULL REFERENCES kg_questions(id) ON DELETE CASCADE,
      fact_id INTEGER NOT NULL REFERENCES kg_facts(id) ON DELETE CASCADE,
      PRIMARY KEY (question_id, fact_id)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_question_facts_fact
                      ON kg_question_facts(fact_id)"])

  ;; Quiz/exam sessions — question_ids freezes the draw at start (reload-safe,
  ;; and an exam's paper must not shift mid-sitting). time_limit_seconds NULL =
  ;; quiz (untimed, instant feedback); set = exam. finished_at NULL = active.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_sessions (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      kind TEXT NOT NULL CHECK (kind IN ('quiz','exam')),
      scope_topic_ids INTEGER[] NOT NULL,
      question_ids INTEGER[] NOT NULL,
      time_limit_seconds INTEGER,
      started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      finished_at TIMESTAMPTZ
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_sessions_user_active
                      ON kg_sessions(user_id, kind) WHERE finished_at IS NULL"])

  ;; One answer row per (session, question); verdict NULL until graded.
  ;; missed_fact_ids ⊆ the question's linked facts; matched_keywords ⊆ the
  ;; linked entities' labels/aliases — both enforced by the grading fn, which
  ;; is the only writer of graded fields.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_answers (
      id SERIAL PRIMARY KEY,
      session_id INTEGER NOT NULL REFERENCES kg_sessions(id) ON DELETE CASCADE,
      question_id INTEGER NOT NULL REFERENCES kg_questions(id),
      position INTEGER NOT NULL,
      user_answer TEXT NOT NULL,
      verdict TEXT CHECK (verdict IN ('correct','partial','incorrect')),
      explanation TEXT,
      missed_fact_ids INTEGER[] NOT NULL DEFAULT '{}',
      matched_keywords TEXT[] NOT NULL DEFAULT '{}',
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      graded_at TIMESTAMPTZ,
      UNIQUE (session_id, question_id)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_answers_session
                      ON kg_answers(session_id, position)"])
  ;; The question's wording as first asked. History LISTS render this snapshot so a
  ;; later edit never rewrites the record; history DETAIL reads the live question by
  ;; id and marks the divergence. NULL on rows written before this column existed —
  ;; "not recorded", which is not the same as "unchanged".
  (jdbc/execute! ds ["ALTER TABLE kg_answers ADD COLUMN IF NOT EXISTS question_text TEXT"])

  ;; FSRS review log — append-only, one row per graded Review-quiz answer. Source
  ;; of truth for scheduling history + daily caps (mirrors topic_repetitions).
  ;; Custom quiz and exam never write here (practice / no-FSRS). reps_before = the
  ;; card's fsrs_reps at review time, so reps_before = 0 marks a card's first-ever
  ;; review — i.e. a "new" introduction — for the new-per-day cap.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_reviews (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      question_id INTEGER NOT NULL REFERENCES kg_questions(id) ON DELETE CASCADE,
      rating SMALLINT NOT NULL CHECK (rating IN (1,2,3,4)),
      verdict TEXT CHECK (verdict IN ('correct','partial','incorrect')),
      state_before SMALLINT NOT NULL,
      state_after SMALLINT NOT NULL,
      reps_before INTEGER NOT NULL,
      stability_after REAL,
      difficulty_after REAL,
      elapsed_days INTEGER,
      scheduled_days INTEGER,
      reviewed_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_user_time
                      ON kg_reviews(user_id, reviewed_at DESC)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_question
                      ON kg_reviews(question_id, reviewed_at DESC)"])
  ;; Answer record. The Review flow writes no kg_answers row, so these columns ARE
  ;; its answer history — one row per graded review. A grading failure writes no row
  ;; at all (the FSRS schedule is untouched), so a failed grade leaves no history and
  ;; the answer text is lost; the session flows persist before grading instead.
  ;; question_text = the wording at grade time (see kg_answers above).
  ;;
  ;; duration_ms = how long the turn took, client-measured from turn mount to
  ;; submit-or-rating. NULL means NOT RECORDED, never zero: every row written
  ;; before the column existed is NULL and there is no backfill, so a stats
  ;; average must state its own denominator rather than treat NULL as 0. The
  ;; value is client-supplied and therefore untrusted — clamp-duration-ms is the
  ;; single bound, applied in apply-fsrs-review!.
  (doseq [col ["user_answer TEXT"
               "explanation TEXT"
               "question_text TEXT"
               "missed_fact_ids INTEGER[] NOT NULL DEFAULT '{}'"
               "duration_ms INTEGER"]]
    (jdbc/execute! ds [(str "ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS " col)]))

  ;; Graph-view layout cache — one row per (user, scope). payload is the
  ;; positioned, wire-ready render payload {:nodes :edges :predicates :docs};
  ;; version = the :kg-mutations counter at compute time, so a stale row is
  ;; detected by a cheap integer compare on read (freememo.kg-graph). Positions
  ;; are recomputed lazily on the next Graph-tab open after any KG mutation.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS kg_graph_layout (
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      scope TEXT NOT NULL,
      version BIGINT NOT NULL,
      payload JSONB NOT NULL,
      computed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (user_id, scope)
    )"])
)

(defn- setup-schema-card-quiz!
  "Flashcards as Quiz items: per-deletion FSRS state, and kg_reviews widened to log
   a card review. See plans/cards-in-quiz-queue.md §1. Runs after
   setup-schema-kg! — it alters kg_reviews."
  []
  ;; One row per QUIZ ITEM, not per card. A cloze card yields one item per
  ;; deletion (ord = the N in {{cN::…}}); a basic card yields exactly one (ord 0).
  ;; Rows are created lazily on first review, so an ABSENT row means "never
  ;; introduced" — the same signal `fsrs_due IS NULL` carries for a question.
  ;; Columns mirror kg_questions' FSRS set; freememo.fsrs owns their meaning.
  ;; No user_id: a card is scoped through flashcards.root_topic_id → topics.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS card_schedules (
      flashcard_id INTEGER NOT NULL REFERENCES flashcards(id) ON DELETE CASCADE,
      ord SMALLINT NOT NULL,
      state SMALLINT NOT NULL DEFAULT 1,
      step SMALLINT DEFAULT 0,
      stability REAL,
      difficulty REAL,
      due TIMESTAMPTZ,
      reps INTEGER NOT NULL DEFAULT 0,
      lapses INTEGER NOT NULL DEFAULT 0,
      last_review TIMESTAMPTZ,
      PRIMARY KEY (flashcard_id, ord)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_card_schedules_due
                      ON card_schedules(due) WHERE due IS NOT NULL"])

  ;; kg_reviews logs BOTH item types: exactly one of question_id / flashcard_id is
  ;; set, and a card row also carries its ord. One log table is what keeps
  ;; fsrs-daily-counts — and so the shared daily cap — a single query.
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ALTER COLUMN question_id DROP NOT NULL"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS flashcard_id INTEGER
                      REFERENCES flashcards(id) ON DELETE CASCADE"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS ord SMALLINT"])
  ;; Which arm produced the rating. Without it a self-graded Good (rating 3, NULL
  ;; verdict) cannot be told apart from a row written before this column existed.
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD COLUMN IF NOT EXISTS grade_source TEXT
                      NOT NULL DEFAULT 'ai'"])
  ;; Drop-then-add is the idempotent shape for a named CHECK (ADD CONSTRAINT has no
  ;; IF NOT EXISTS).
  (jdbc/execute! ds ["ALTER TABLE kg_reviews DROP CONSTRAINT IF EXISTS kg_reviews_grade_source"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD CONSTRAINT kg_reviews_grade_source
                      CHECK (grade_source IN ('ai','self'))"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews DROP CONSTRAINT IF EXISTS kg_reviews_one_item"])
  (jdbc/execute! ds ["ALTER TABLE kg_reviews ADD CONSTRAINT kg_reviews_one_item
                      CHECK ((question_id IS NULL) <> (flashcard_id IS NULL)
                             AND (flashcard_id IS NULL OR ord IS NOT NULL))"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_kg_reviews_card
                      ON kg_reviews(flashcard_id, ord, reviewed_at DESC)
                      WHERE flashcard_id IS NOT NULL"]))

(defn- setup-schema-video!
  "Incremental Video (plans/incremental-video.md §4.1): the large-object anchor,
   the reference transcript, extract ranges, chunked-upload sessions, and the
   two storage-metering columns on users.

   Video bytes live in a Postgres large object, NOT in topic_files — `bytea`
   caps at 1 GB and cannot be range-read. topic_files still holds the
   ffmpeg-extracted MP3 under role='audio', which is small and read whole."
  []
  ;; One row per kind='video' topic. lo_oid is the large object; usage_bytes
  ;; counts byte_size, so the two must be written and cleared together.
  ;; last_pos_ms is the resume position (§4.1 Q2) — a column rather than a
  ;; settings key so it lives and dies with the video.
  (let [video-table-existed?
        (some? (jdbc/execute-one! ds
                 ["SELECT 1 FROM information_schema.tables
                   WHERE table_name = 'topic_videos' AND table_schema = 'public'"]))]
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS topic_videos (
        topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
        lo_oid OID NOT NULL,
        byte_size BIGINT NOT NULL,
        mime_type TEXT NOT NULL,
        duration_ms INTEGER,
        last_pos_ms INTEGER NOT NULL DEFAULT 0,
        created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      )"])
    (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_videos_oid ON topic_videos(lo_oid)"])

    ;; One row per chunk-uploaded kind='epub' topic, for the same reason
    ;; topic_videos exists: `topic_files.file_data` is BYTEA, which caps at 1 GB
    ;; and materializes whole in heap on every read, so a large book could not be
    ;; stored there at all. A separate table rather than a nullable `lo_oid` on
    ;; topic_files, because that column would make `file_data` nullable under
    ;; eleven existing readers that do not branch on it — a silent nil rather
    ;; than a compile error. See plans/large-epub-import.md §7.4.
    ;;
    ;; EPUB topics created before this table, and any still arriving through
    ;; multipart, keep their bytes in topic_files. Both shapes are live: read
    ;; through `with-epub-source`, which is the only thing that knows the
    ;; difference.
    (jdbc/execute! ds ["
      CREATE TABLE IF NOT EXISTS topic_epubs (
        topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
        lo_oid OID NOT NULL,
        byte_size BIGINT NOT NULL,
        created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
      )"])
    (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_epubs_oid ON topic_epubs(lo_oid)"])

    ;; §12.4 3.1 — TRUE while the stored bytes are still the ones the browser
    ;; uploaded and the pipeline may yet replace them with a remuxed MP4. The
    ;; playback handler reads it to decide whether the response may be cached:
    ;; `get-video-handler`'s 24-hour `max-age` rests on "the bytes behind a
    ;; topic id never change", which is true only once this clears.
    (jdbc/execute! ds ["ALTER TABLE topic_videos
                        ADD COLUMN IF NOT EXISTS remux_pending BOOLEAN NOT NULL DEFAULT FALSE"])

    ;; §4.2 2.1 — usage_bytes is now SUM(topic_files.file_size) +
    ;; SUM(topic_videos.byte_size). Recomputed once, at the moment the table is
    ;; introduced: the video term is provably 0 then, so this only repairs
    ;; pre-existing drift, and every later mutation maintains the invariant.
    (when-not video-table-existed?
      (tel/log! :info "Recomputing users.usage_bytes over topic_files + topic_videos")
      (jdbc/execute! ds
        ["UPDATE users SET usage_bytes = COALESCE(
            (SELECT SUM(tf.file_size)
             FROM topic_files tf JOIN topics t ON tf.topic_id = t.id
             WHERE t.user_id = users.id), 0)
          + COALESCE(
            (SELECT SUM(tv.byte_size)
             FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
             WHERE t.user_id = users.id), 0)
          + COALESCE(
            (SELECT SUM(te.byte_size)
             FROM topic_epubs te JOIN topics t ON te.topic_id = t.id
             WHERE t.user_id = users.id), 0)"])))

  ;; Reference transcript — one row per Whisper segment (§4.1 1.2 / decision B1).
  ;; `ord` is the position within the whole video after chunk offsets are folded
  ;; in, so (topic_id, ord) is the natural read order and the overlap query's
  ;; index. Word-level timestamps would be a second table; the shape admits it.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS video_transcripts (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      ord INTEGER NOT NULL,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      text TEXT NOT NULL
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_video_transcripts_topic_ord
                      ON video_transcripts(topic_id, ord)"])

  ;; Extract ranges (§4.1 1.3 / decision C3). topic_id is the EXTRACT topic, not
  ;; the video — one range per extract. The range is provenance: the extract's
  ;; text was copied in at creation and is editable thereafter, so the range
  ;; never rewrites content (decision "derivation is initial-only").
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS video_segments (
      id BIGSERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      start_ms INTEGER NOT NULL,
      end_ms INTEGER NOT NULL,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_video_segments_topic
                      ON video_segments(topic_id)"])

  ;; Chunked-upload sessions (§4.3). The large object is created — and the
  ;; quota reserved — at init, so an abandoned session leaks BOTH until the
  ;; sweep reaps it (§4.3 3.3). received_bytes is the resume cursor (§4.3 3.2).
  ;;
  ;; Was `video_upload_sessions`. Renamed because the archive import flow needs
  ;; the same three-call transport for the same reason video did — a 7.5 GB
  ;; SuperMemo collection cannot go through a 100 MB request cap — and a second
  ;; table would be a second place for the reap sweep, the orphan sweep and the
  ;; compensation path to drift. `flow` discriminates what finalize does; the
  ;; transport above it is identical.
  (let [table? (fn [nm] (some? (jdbc/execute-one! ds
                                 ["SELECT 1 FROM information_schema.tables
                                   WHERE table_schema = 'public' AND table_name = ?" nm])))]
    (when (and (table? "video_upload_sessions") (not (table? "upload_sessions")))
      (tel/log! :info "Renaming video_upload_sessions to upload_sessions")
      (jdbc/execute! ds ["ALTER TABLE video_upload_sessions RENAME TO upload_sessions"])))
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS upload_sessions (
      id TEXT PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      lo_oid OID NOT NULL,
      total_bytes BIGINT NOT NULL,
      received_bytes BIGINT NOT NULL DEFAULT 0,
      filename TEXT NOT NULL,
      mime_type TEXT NOT NULL,
      parent_id INTEGER REFERENCES topics(id) ON DELETE CASCADE,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
      updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; 'video' is the default so the rename above backfills in-flight sessions
  ;; correctly — every row that predates this column IS a video upload.
  (jdbc/execute! ds ["ALTER TABLE upload_sessions
                      ADD COLUMN IF NOT EXISTS flow TEXT NOT NULL DEFAULT 'video'"])
  ;; Path of the archive materialized out of the large object at finalize. A
  ;; CACHE, never state: the large object stays authoritative, and the import
  ;; re-materializes when this path is absent or stale (a blue-green deploy
  ;; flip lands the import in a different container than the upload).
  (jdbc/execute! ds ["ALTER TABLE upload_sessions ADD COLUMN IF NOT EXISTS archive_path TEXT"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_upload_sessions_updated
                      ON upload_sessions(updated_at)"])
  (jdbc/execute! ds ["DROP INDEX IF EXISTS idx_video_upload_sessions_updated"])

  ;; §4.1 1.4 — storage metering. last_metered_at seeds to now() for existing
  ;; users so the first accrual bills from migration forward, never retroactively.
  ;; storage_grace_started_at is NULL except while a zero-balance user's bytes
  ;; are counting down to reclamation.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS last_metered_at TIMESTAMP"])
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS storage_grace_started_at TIMESTAMP"])
  ;; Sub-unit carry, in millionths of an IQD. Credits are whole IQD, but one
  ;; hourly tick of a small library prices below 1 IQD; without a carry the
  ;; charge would round to zero every hour forever and the storage meter would
  ;; collect nothing from exactly the users it was written for.
  (jdbc/execute! ds ["ALTER TABLE users ADD COLUMN IF NOT EXISTS
                      storage_debt_micro BIGINT NOT NULL DEFAULT 0"])
  (jdbc/execute! ds ["UPDATE users SET last_metered_at = now() WHERE last_metered_at IS NULL"])
)

(defn- setup-schema-neoreader!
  "Boox NeoReader highlights — one row per highlight of one document's export
   (plans/boox-neoreader-google-drive-sync.md §5). Must run after
   setup-schema-topics! — two columns FK topics.

   `highlighted_at` keeps the DEVICE's own `yyyy-MM-dd HH:mm:ss` string and is
   not a TIMESTAMP. The export names no time zone, so parsing it would invent
   one, and the string is part of the row's identity — a later change of format
   would orphan every row written before it.

   `quote_hash` is GENERATED, not written by the app. It exists because identity
   needs the quote and a B-tree index row cannot hold a long TEXT column (see
   the flashcard unique indexes above). Generating it in Postgres puts the digest
   beside the constraint that uses it, where the two cannot drift.

   Identity is (document, device timestamp, quote digest). Position in the export
   cannot serve: deleting one highlight on the device shifts every later one, and
   a re-sync would then rewrite every row after the deletion."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS neoreader_highlights (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      chapter_topic_id INTEGER REFERENCES topics(id) ON DELETE SET NULL,
      chapter_label TEXT,
      highlighted_at TEXT NOT NULL,
      quote TEXT NOT NULL,
      quote_hash TEXT GENERATED ALWAYS AS (md5(quote)) STORED,
      note TEXT,
      ordinal INTEGER,
      outcome TEXT NOT NULL,
      synced_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])
  ;; Unguarded, unlike the flashcard indexes: this table is new, so no legacy
  ;; duplicate can exist. It must also succeed — `replace-neoreader-highlights!`
  ;; names it in an ON CONFLICT clause, which has no meaning without it.
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_neoreader_highlights
                      ON neoreader_highlights(root_topic_id, highlighted_at, quote_hash)"]))

(defn- setup-schema-account-transfer!
  "Import bookkeeping for account transfer (plans/user-data-export-import.md
   §5.5). Must run after setup-schema-topics! — `parent_topic_id` FKs topics.

   Two tables rather than one. `account_imports` is one row per import RUN, and
   `account_import_ids` is the source-id to new-id map for that run. Folding the
   run header into the map table would need a nullable source_kind and
   source_id, which the primary key cannot carry.

   The map is what makes three things possible from one place: the
   already-imported warning, the resume that skips rows already created, and
   blob-gap detection. It is target-instance state and is never exported.

   `copy_ordinal` is what separates a RESUME from a deliberate second copy. A
   rerun of the same export resumes the newest row for it; the explicit override
   inserts a fresh row at the next ordinal, so both copies keep their own map and
   neither can be mistaken for the other.

   `metadata_done_at` and `blobs_done_at` are the two phases. Metadata commits
   in one transaction, so its stamp is written inside that transaction and a
   crash leaves the whole run absent. Blobs commit one at a time, so their stamp
   lands only when every blob is present — an unstamped run with a stamped
   metadata phase is exactly what a resume looks for."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS account_imports (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      export_id TEXT NOT NULL,
      copy_ordinal INTEGER NOT NULL DEFAULT 0,
      format_version INTEGER NOT NULL,
      source_instance TEXT,
      exported_at TEXT,
      parent_topic_id INTEGER REFERENCES topics(id) ON DELETE SET NULL,
      started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      metadata_done_at TIMESTAMPTZ,
      blobs_done_at TIMESTAMPTZ,
      report JSONB,
      UNIQUE (user_id, export_id, copy_ordinal)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_account_imports_user
                      ON account_imports(user_id, started_at DESC)"])

  ;; source_kind is a table name, not an enum: the exported table set lives in
  ;; freememo.account-transfer-format and a CHECK here would be a second copy of
  ;; it, free to drift. source_id is BIGINT because media and card_versions are
  ;; BIGSERIAL on the source side.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS account_import_ids (
      import_id BIGINT NOT NULL REFERENCES account_imports(id) ON DELETE CASCADE,
      source_kind TEXT NOT NULL,
      source_id BIGINT NOT NULL,
      new_id BIGINT NOT NULL,
      PRIMARY KEY (import_id, source_kind, source_id)
    )"])
  ;; Unindexed FK referencing columns make ON DELETE CASCADE table-scan.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_account_import_ids_import
                      ON account_import_ids(import_id)"]))

(defn- setup-schema-study-scheduler!
  "The Study Scheduler (plans/study-scheduler-spec.md §3). Must run after
   setup-schema-topics! — both tables FK topics or flashcards.

   Two tables, and the split is the point.

   `study_schedules` is the user's INTENT: this document, this page range, by
   this date. It is durable, exported, and nothing else derives it.

   `anki_card_states` is DERIVED (§3.5): FSRS memory state replayed out of
   Anki's revlog. Anki is the source of truth and this is a cache in front of
   it, so it is excluded from account export the way `kg_graph_layout` is.
   Dropping the whole table degrades the Schedule tab to 'Anki unreachable'
   and breaks nothing else, which is §3.1's own done-condition.

   The scheduler writes NO column of `card_schedules` (§1.4). That table is the
   Learn queue's per-deletion FSRS state and this feature builds no queue.

   `config_fingerprint` covers Anki's weights, retention target and revlog
   cutoff (§3.3). Readers filter on it, so a config change discards the rows it
   no longer describes.

   `high_water_revlog_id` is the newest revlog id folded into the row. It once
   selected an incremental `cardReviews` pull; that pull is gone, because
   AnkiConnect scopes the action by a card's CURRENT deck and returned nothing
   for cards gathered into a filtered deck. The column is still written as the
   per-card anchor of the replay, and read by nobody.

   Page numbers, never topic ids, in `first_page`, `last_page` and
   `excluded_pages`. An id inside an array is the account-transfer failure
   §3.4 has no guard for — `fk-columns` translates scalars, so an array of
   topic ids would arrive on the target pointing at another user's rows.
   A page number needs no translation."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS study_schedules (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      root_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      target_date DATE NOT NULL,
      first_page INTEGER,
      last_page INTEGER,
      daily_capacity INTEGER NOT NULL DEFAULT 60,
      theta REAL,
      excluded_pages INTEGER[] NOT NULL DEFAULT '{}',
      start_known INTEGER NOT NULL DEFAULT 0,
      start_covered INTEGER NOT NULL DEFAULT 0,
      covered_baseline_at TIMESTAMPTZ,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      archived_at TIMESTAMPTZ
    )"])
  ;; §1.3 sums the daily load across every ACTIVE schedule, so that is the
  ;; query shape the index serves.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_study_schedules_active
                      ON study_schedules(user_id, target_date)
                      WHERE archived_at IS NULL"])
  ;; θ is a probability. I4 (§6.6.3) lets a schedule lower it; nothing lets a
  ;; schedule leave the range where retrievability has a meaning.
  (jdbc/execute! ds ["ALTER TABLE study_schedules DROP CONSTRAINT IF EXISTS study_schedules_theta_range"])
  (jdbc/execute! ds ["ALTER TABLE study_schedules ADD CONSTRAINT study_schedules_theta_range
                      CHECK (theta IS NULL OR (theta > 0 AND theta < 1))"])
  (jdbc/execute! ds ["ALTER TABLE study_schedules DROP CONSTRAINT IF EXISTS study_schedules_capacity_positive"])
  (jdbc/execute! ds ["ALTER TABLE study_schedules ADD CONSTRAINT study_schedules_capacity_positive
                      CHECK (daily_capacity > 0)"])

  ;; The COVERAGE baseline. The pace figure measures covered pages against a
  ;; straight line, and a line needs the count the user had when they made the
  ;; plan. `start_known` cannot serve: known is at or below covered by the
  ;; maturation gate, so reusing it understates the baseline and reports every
  ;; schedule as further ahead than it is.
  ;;
  ;; `covered_baseline_at` is the line's ORIGIN, and it exists because coverage
  ;; at creation time is unrecoverable. `topics.status` is a TEXT column
  ;; toggled in place with no timestamp, and `undo_log.action_type` carries no
  ;; done toggle, so nothing anywhere records when a page was marked done. A
  ;; schedule that predates this column therefore takes TODAY's covered count
  ;; against TODAY as its origin: exact going forward, and it forgives whatever
  ;; lag the old line had measured. Reusing `created_at` with today's count
  ;; would instead make a half-elapsed schedule jump several pages behind
  ;; overnight, which is a worse lie than a forgiven lag.
  (jdbc/execute! ds ["ALTER TABLE study_schedules ADD COLUMN IF NOT EXISTS start_covered
                      INTEGER NOT NULL DEFAULT 0"])
  (jdbc/execute! ds ["ALTER TABLE study_schedules ADD COLUMN IF NOT EXISTS covered_baseline_at
                      TIMESTAMPTZ"])
  ;; Guarded on the origin being absent, so this runs ONCE per row. Without the
  ;; guard every boot would re-baseline every live schedule to `on plan`, and
  ;; the pace figure would never report a lag again.
  ;;
  ;; The count is `document-progress`' own rule: pages of the root, inside the
  ;; range, not staged for deletion, marked done. Excluded pages are NOT
  ;; filtered — lever I5 removes a page's CARDS, never the page, and
  ;; `schedule-inputs` keeps it in the denominator.
  (let [n (::jdbc/update-count
           (jdbc/execute-one! ds
             ["UPDATE study_schedules s
                  SET start_covered = (SELECT COUNT(*) FROM topics t
                                        WHERE t.parent_id = s.root_topic_id
                                          AND t.kind = 'page'
                                          AND t.user_id = s.user_id
                                          AND t.staged_delete_id IS NULL
                                          AND t.status = 'done'
                                          AND (s.first_page IS NULL OR t.page_number >= s.first_page)
                                          AND (s.last_page IS NULL OR t.page_number <= s.last_page)),
                      covered_baseline_at = now()
                WHERE s.covered_baseline_at IS NULL"])
           0)]
    (when (pos? n)
      (tel/log! {:level :info :id ::schedule-coverage-baseline :data {:rows n}}
        "Backfilled the study-schedule coverage baseline")))

  ;; Keyed on (user, Anki card id): an Anki card id is unique inside ONE
  ;; collection, and two users on this instance sync two different ones.
  ;; `flashcard_id` is nullable because a note can yield more Anki cards than
  ;; FreeMemo knows about — a cloze note is one FreeMemo card and N Anki cards,
  ;; and every one of them carries memory state the page mean of §5.1 reads.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS anki_card_states (
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      anki_card_id BIGINT NOT NULL,
      anki_note_id BIGINT,
      flashcard_id INTEGER REFERENCES flashcards(id) ON DELETE CASCADE,
      deck TEXT,
      state SMALLINT NOT NULL DEFAULT 1,
      step SMALLINT,
      stability REAL,
      difficulty REAL,
      interval_days REAL,
      last_review_at TIMESTAMPTZ,
      due_at TIMESTAMPTZ,
      reps INTEGER NOT NULL DEFAULT 0,
      config_fingerprint TEXT NOT NULL,
      high_water_revlog_id BIGINT NOT NULL DEFAULT 0,
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (user_id, anki_card_id)
    )"])
  ;; The page model reads every state of one document at once, through the
  ;; note ids its flashcards carry.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_anki_card_states_note
                      ON anki_card_states(user_id, anki_note_id)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_anki_card_states_flashcard
                      ON anki_card_states(flashcard_id) WHERE flashcard_id IS NOT NULL"])

  ;; The deck config, cached beside the state it governs. §8.2 requires a
  ;; forecast to still render when Anki is unreachable, and a forecast needs
  ;; the WEIGHTS: `anki_card_states` stores a fingerprint OVER the config, not
  ;; the config, and reconstructing parameters out of a fingerprint would make
  ;; an identity string load-bearing as a data format.
  ;;
  ;; One row per (user, deck). A cache like its sibling, so it is excluded from
  ;; account export for the same reason.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS anki_deck_configs (
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      deck TEXT NOT NULL,
      config JSONB NOT NULL,
      fingerprint TEXT NOT NULL,
      fetched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (user_id, deck)
    )"]))

(defn- setup-schema-curriculum!
  "Personal curricula and evidence-bound long-form exams.

   The order of these tables is also their account-import order. A form belongs
   to one material or one block. Five questions are frozen at approval. Grade
   revisions are append-only, and block completion is a monotonic timestamp."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curricula (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      title TEXT NOT NULL CHECK (length(trim(title)) > 0),
      target_date DATE,
      daily_minutes INTEGER CHECK (daily_minutes > 0),
      study_weekdays SMALLINT[] NOT NULL DEFAULT '{}',
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      CHECK (target_date IS NOT NULL OR daily_minutes IS NOT NULL
             OR cardinality(study_weekdays) > 0),
      CHECK (study_weekdays <@ ARRAY[1,2,3,4,5,6,7]::SMALLINT[])
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_curricula_user
                      ON curricula(user_id, updated_at DESC)"])
  ;; A staged curriculum stays intact until its Undo window ends.
  ;; Purge deletes the root before the entry so the FK never resurfaces it.
  (jdbc/execute! ds ["ALTER TABLE curricula ADD COLUMN IF NOT EXISTS staged_delete_id BIGINT
                      REFERENCES undo_log(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_curricula_staged_delete
                      ON curricula(staged_delete_id) WHERE staged_delete_id IS NOT NULL"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_blocks (
      id SERIAL PRIMARY KEY,
      curriculum_id INTEGER NOT NULL REFERENCES curricula(id) ON DELETE CASCADE,
      position INTEGER NOT NULL CHECK (position >= 0),
      title TEXT NOT NULL CHECK (length(trim(title)) > 0),
      unlocked_at TIMESTAMPTZ,
      focused_at TIMESTAMPTZ,
      completed_at TIMESTAMPTZ,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (curriculum_id, position)
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_curriculum_blocks_focus
                      ON curriculum_blocks(curriculum_id) WHERE focused_at IS NOT NULL"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_materials (
      id SERIAL PRIMARY KEY,
      block_id INTEGER NOT NULL REFERENCES curriculum_blocks(id) ON DELETE CASCADE,
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      position INTEGER NOT NULL CHECK (position >= 0),
      estimated_minutes INTEGER CHECK (estimated_minutes > 0),
      studied_at TIMESTAMPTZ,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (block_id, position),
      UNIQUE (block_id, topic_id)
    )"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_exam_forms (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      block_id INTEGER REFERENCES curriculum_blocks(id) ON DELETE CASCADE,
      material_id INTEGER REFERENCES curriculum_materials(id) ON DELETE CASCADE,
      version INTEGER NOT NULL CHECK (version > 0),
      form_kind TEXT NOT NULL DEFAULT 'standard'
        CHECK (form_kind IN ('standard','targeted-retry')),
      status TEXT NOT NULL DEFAULT 'draft'
        CHECK (status IN ('draft','approved')),
      threshold INTEGER NOT NULL DEFAULT 70 CHECK (threshold BETWEEN 0 AND 100),
      blueprint JSONB NOT NULL,
      questions JSONB NOT NULL CHECK (jsonb_typeof(questions) = 'array'
                                     AND jsonb_array_length(questions) = 5),
      source_versions JSONB NOT NULL,
      accepted_source_versions JSONB,
      generation_cost_credits NUMERIC(12,4),
      approved_at TIMESTAMPTZ,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      CHECK ((block_id IS NULL) <> (material_id IS NULL)),
      CHECK ((status = 'approved') = (approved_at IS NOT NULL))
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_curriculum_material_form_version
                      ON curriculum_exam_forms(material_id, version)
                      WHERE material_id IS NOT NULL"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_curriculum_block_form_version
                      ON curriculum_exam_forms(block_id, version)
                      WHERE block_id IS NOT NULL"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_exam_sittings (
      id SERIAL PRIMARY KEY,
      form_id INTEGER NOT NULL REFERENCES curriculum_exam_forms(id) ON DELETE CASCADE,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      submitted_at TIMESTAMPTZ,
      answers JSONB,
      self_correction_score INTEGER CHECK (self_correction_score BETWEEN 0 AND 100),
      self_correction_reason TEXT
    )"])
  ;; Answer drafts autosave here while a sitting is open. `answers` stays the
  ;; frozen submission, so a resumed sitting reads `draft_answers` and a graded
  ;; one reads `answers`. Finalize copies one column to the other unchanged.
  (jdbc/execute! ds ["ALTER TABLE curriculum_exam_sittings
                      ADD COLUMN IF NOT EXISTS draft_answers JSONB"])
  ;; A sitting the learner abandons. `cancelled_at` closes it without freezing
  ;; answers, so an accidental start costs nothing and no grade can follow.
  ;; `archived_at` then takes it off the list. Both are nullable event stamps,
  ;; matching started_at and submitted_at — a sitting is a sequence of events,
  ;; not a status column.
  (jdbc/execute! ds ["ALTER TABLE curriculum_exam_sittings
                      ADD COLUMN IF NOT EXISTS cancelled_at TIMESTAMPTZ"])
  (jdbc/execute! ds ["ALTER TABLE curriculum_exam_sittings
                      ADD COLUMN IF NOT EXISTS archived_at TIMESTAMPTZ"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_grade_revisions (
      id SERIAL PRIMARY KEY,
      sitting_id INTEGER NOT NULL REFERENCES curriculum_exam_sittings(id) ON DELETE CASCADE,
      revision INTEGER NOT NULL CHECK (revision > 0),
      raw_score INTEGER NOT NULL CHECK (raw_score BETWEEN 0 AND 100),
      passed BOOLEAN NOT NULL,
      criteria JSONB NOT NULL,
      explanation TEXT NOT NULL,
      grading_cost_credits NUMERIC(12,4),
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (sitting_id, revision)
    )"])

  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS curriculum_unlock_overrides (
      id SERIAL PRIMARY KEY,
      curriculum_id INTEGER NOT NULL REFERENCES curricula(id) ON DELETE CASCADE,
      from_block_id INTEGER NOT NULL REFERENCES curriculum_blocks(id) ON DELETE CASCADE,
      to_block_id INTEGER NOT NULL REFERENCES curriculum_blocks(id) ON DELETE CASCADE,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (from_block_id, to_block_id),
      CHECK (from_block_id <> to_block_id)
    )"]))

(defn- setup-schema-activity-monitor!
  "One row per long-running operation a user started — the Activity monitor's
   durable record (plans/activity-monitor.md). Must run after
   setup-schema-topics!: `subject_id` FKs topics.

   NOT `setup-schema-activity!`. That one owns `user_events`, `media` and
   `topic_pins` — 'activity' there means analytics. A plural near-twin of its
   name would put two unrelated fns one character apart.

   This table is a SNAPSHOT, never the source of truth. The per-user atoms in
   freememo.user-state stay authoritative for live state; `freememo.activity`
   writes this behind them on a timer. That is what makes a missed write
   self-healing rather than permanent — the next tick corrects it. Same
   discipline as `flashcards.anki_header_text`.

   Identity is `work_key`, NOT `subject_id`. The owners key their in-flight
   state five different ways — a topic id (card gen), a `[doc page]` pair (OCR),
   a root topic id (distill), an upload session id (TEXT), and nothing at all
   (an Anki reconcile has no scope to narrow, per quick-sync's docstring). One
   integer column cannot carry all five, so each owner supplies a `work_key`
   string it composes itself, and OCR's is `\"<doc>/<page>\"`.

   `subject_id` exists ONLY so the popover can navigate somewhere, and it is
   nullable because two types have nowhere to go: an Anki reconcile spans the
   collection, and an upload has no topic until it lands. What a `subject_id`
   MEANS is declared once per type in the activity registry, which is why there
   is no `subject_kind` column duplicating that per row.

   The unique index is PARTIAL on `terminated_at IS NULL`. Absolute uniqueness
   would forbid ever re-running the same card generation; the contract is only
   that one run of one piece of work is in flight at a time — which is what the
   owners already enforce (`:processing-videos` dedupes on (user, topic);
   `reconcile/start!` returns :busy).

   `type` is plain TEXT with no CHECK, matching topic kinds and card kinds: the
   vocabulary lives in code, in the activity registry."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS activities (
      id BIGSERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      type TEXT NOT NULL,
      work_key TEXT NOT NULL,
      subject_id INTEGER REFERENCES topics(id) ON DELETE SET NULL,
      phase TEXT NOT NULL,
      -- BIGINT, not INTEGER: an upload reports BYTES, and a 3 GB video would
      -- overflow int4 silently at the driver rather than loudly here.
      done BIGINT,
      total BIGINT,
      cancellable BOOLEAN NOT NULL DEFAULT FALSE,
      unrecoverable BOOLEAN NOT NULL DEFAULT FALSE,
      outcome TEXT CHECK (outcome IN ('success','failure','cancelled','interrupted')),
      error TEXT,
      started_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      terminated_at TIMESTAMPTZ,
      CONSTRAINT activities_terminal_agrees
        CHECK ((terminated_at IS NULL) = (outcome IS NULL)),
      CONSTRAINT activities_total_needs_done
        CHECK (total IS NULL OR done IS NOT NULL)
    )"])
  ;; Widen an activities table created before bytes were a progress unit.
  (jdbc/execute! ds ["ALTER TABLE activities ALTER COLUMN done TYPE BIGINT"])
  (jdbc/execute! ds ["ALTER TABLE activities ALTER COLUMN total TYPE BIGINT"])
  ;; The idempotency contract of `activity/start!`: at most one in-flight run per
  ;; (user, type, work_key). Partial, so a completed run does not block a retry.
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS idx_activities_inflight
                      ON activities(user_id, type, work_key)
                      WHERE terminated_at IS NULL"])
  ;; The popover's query: this user's activities, newest first.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_activities_user_started
                      ON activities(user_id, started_at DESC)"])
  ;; The staleness sweep reads only in-flight rows, by how long they have been quiet.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_activities_stale
                      ON activities(updated_at) WHERE terminated_at IS NULL"])
  ;; The prune sweep keeps failures longer than successes, so it reads by both.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_activities_prune
                      ON activities(outcome, terminated_at)
                      WHERE terminated_at IS NOT NULL"]))

(defn- setup-schema-reading-positions!
  "Where a reader left each scrollable surface of a topic
   (plans/reading-position-memory.md §4.1). Must run after setup-schema-topics!.

   No `user_id`: the topic's owner is the row's owner, and a second copy of that
   fact could disagree with it. `ON DELETE CASCADE` takes the rows with their
   topic on a hard delete and on the staged-delete purge, which a `settings` key
   per topic never did.

   `anchor` is `{i, fp, frac}` from `freememo.reading-anchor`. `fraction` is the
   progress through the surface, kept for a progress display that does not
   exist yet (§4 0.3)."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS reading_positions (
      topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      surface TEXT NOT NULL
        CHECK (surface IN ('editor', 'poem', 'code-rendered', 'code-source')),
      anchor JSONB NOT NULL,
      fraction REAL NOT NULL CHECK (fraction BETWEEN 0 AND 1),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (topic_id, surface)
    )"]))

(defn- setup-schema-question-pins!
  "Anchored questions on rendered Markdown (plans/anchored-questions.md §6).
   Must run after setup-schema-kg! and setup-schema-code!.

   A question map is one per document key: a repository node plus a
   `repo_path`. Each GitLab update writes a new snapshot topic for every file
   under the same repository node, so keying by the snapshot would strand pins
   on the old file. The map row also owns the label counter, so a label is
   never reused after a delete (§6.2.3).

   A pin stores its lifecycle only. Whether its quote matches is computed per
   snapshot when one renders (§6.1.3), so no column says `stale`.

   A root links to a `kg_entities` row with ON DELETE SET NULL and keeps its
   own label, so deleting an entity leaves every pin with a root (§6.4.2.4).
   The partial unique index holds the one-`next`-per-map invariant (§6.3.8).

   A known mark has no `user_id`: the entity's owner is the mark's owner, and
   a second copy of that fact could disagree with it (§6.4.5)."
  []
  (jdbc/execute! ds ["ALTER TABLE kg_entities
                      ADD COLUMN IF NOT EXISTS from_diagnosis BOOLEAN NOT NULL DEFAULT false"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS question_maps (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      repo_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      repo_path TEXT NOT NULL,
      next_label INTEGER NOT NULL DEFAULT 1 CHECK (next_label >= 1),
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (repo_topic_id, repo_path)
    )"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS question_roots (
      id SERIAL PRIMARY KEY,
      map_id INTEGER NOT NULL REFERENCES question_maps(id) ON DELETE CASCADE,
      label TEXT NOT NULL,
      entity_id INTEGER REFERENCES kg_entities(id) ON DELETE SET NULL,
      in_document BOOLEAN NOT NULL,
      citations JSONB NOT NULL DEFAULT '[]',
      status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'next', 'learned')),
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (map_id, label)
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_question_roots_one_next
                      ON question_roots(map_id) WHERE status = 'next'"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS question_pins (
      id SERIAL PRIMARY KEY,
      map_id INTEGER NOT NULL REFERENCES question_maps(id) ON DELETE CASCADE,
      label INTEGER NOT NULL CHECK (label >= 1),
      snapshot_topic_id INTEGER REFERENCES topics(id) ON DELETE SET NULL,
      quote TEXT NOT NULL CHECK (quote <> ''),
      heading_anchor TEXT,
      question TEXT NOT NULL DEFAULT '',
      status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open', 'closed')),
      root_id INTEGER REFERENCES question_roots(id) ON DELETE SET NULL,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      UNIQUE (map_id, label)
    )"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS known_marks (
      entity_id INTEGER NOT NULL REFERENCES kg_entities(id) ON DELETE CASCADE,
      source TEXT NOT NULL CHECK (source IN ('self', 'tutor')),
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      PRIMARY KEY (entity_id, source)
    )"]))

(defn- setup-schema-walkthroughs!
  "Commit-range walkthroughs (plans/commit-range-walkthrough.md §6.4, §6.8),
   and the columns they add to feature 1's tables. Must run after
   setup-schema-question-pins! and setup-schema-assistant!.

   A walkthrough is one reader's path from sha A to sha B of one repository
   node. `commits` holds the range as `git log` gave it, oldest first, so the
   screen never asks git for it again. `step` 0 is the opener (§6.7.1); step n
   is the concept at position n.

   A hunk row is the key of one diff hunk, never its text: the clone holds the
   text and gives it back on demand. Its concept is NOT NULL, so no hunk ends
   with no concept (§6.5.2), and the key is unique per walkthrough, so none
   ends with two. A concept is deleted only by a merge, which moves its hunks
   first.

   Known marks gain `as_of`, a repository node and a sha (K2, §6.8.1). The
   key stays (entity_id, source): the version is metadata about when the
   reader learned the idea, never a second mark.

   Pins gain the sha they were made at and, on code, an offset hint.

   Evidence gains `walkthrough_id`, so a citation of a file at a commit that
   is not a topic opens the walkthrough that showed it (§6.3.5)."
  []
  (jdbc/execute! ds ["ALTER TABLE known_marks
                      ADD COLUMN IF NOT EXISTS as_of_repo_topic_id INTEGER
                        REFERENCES topics(id) ON DELETE SET NULL"])
  (jdbc/execute! ds ["ALTER TABLE known_marks ADD COLUMN IF NOT EXISTS as_of_sha TEXT"])
  (jdbc/execute! ds ["ALTER TABLE question_pins ADD COLUMN IF NOT EXISTS snapshot_sha TEXT"])
  ;; A pin on code anchors by its exact quote and a character offset into the
  ;; whole source, the hint that picks among repeated quotes (§6.3.7).
  (jdbc/execute! ds ["ALTER TABLE question_pins ADD COLUMN IF NOT EXISTS offset_hint INTEGER"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS walkthroughs (
      id SERIAL PRIMARY KEY,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      repo_topic_id INTEGER NOT NULL REFERENCES topics(id) ON DELETE CASCADE,
      sha_a TEXT NOT NULL,
      sha_b TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'preparing'
        CHECK (status IN ('preparing', 'ready', 'failed')),
      error TEXT,
      commits JSONB NOT NULL DEFAULT '[]',
      step INTEGER NOT NULL DEFAULT 0 CHECK (step >= 0),
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_walkthroughs_repo
                      ON walkthroughs(repo_topic_id, id)"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS walkthrough_concepts (
      id SERIAL PRIMARY KEY,
      walkthrough_id INTEGER NOT NULL REFERENCES walkthroughs(id) ON DELETE CASCADE,
      position INTEGER NOT NULL CHECK (position >= 1),
      label TEXT NOT NULL CHECK (label <> ''),
      summary TEXT NOT NULL DEFAULT '',
      commits JSONB NOT NULL DEFAULT '[]',
      noise BOOLEAN NOT NULL DEFAULT false,
      understood_at TIMESTAMPTZ,
      UNIQUE (walkthrough_id, label)
    )"])
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS walkthrough_hunks (
      id SERIAL PRIMARY KEY,
      walkthrough_id INTEGER NOT NULL REFERENCES walkthroughs(id) ON DELETE CASCADE,
      concept_id INTEGER NOT NULL REFERENCES walkthrough_concepts(id) ON DELETE CASCADE,
      commit_sha TEXT NOT NULL,
      old_path TEXT,
      new_path TEXT,
      header TEXT NOT NULL,
      hunk_index INTEGER NOT NULL CHECK (hunk_index >= 0),
      added INTEGER NOT NULL DEFAULT 0,
      removed INTEGER NOT NULL DEFAULT 0,
      CHECK (old_path IS NOT NULL OR new_path IS NOT NULL)
    )"])
  (jdbc/execute! ds ["CREATE UNIQUE INDEX IF NOT EXISTS uq_walkthrough_hunks_key
                      ON walkthrough_hunks(walkthrough_id, commit_sha,
                        coalesce(old_path, ''), coalesce(new_path, ''), hunk_index)"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_walkthrough_hunks_concept
                      ON walkthrough_hunks(concept_id)"])
  (jdbc/execute! ds ["ALTER TABLE assistant_evidence
                      ADD COLUMN IF NOT EXISTS walkthrough_id INTEGER
                        REFERENCES walkthroughs(id) ON DELETE SET NULL"]))

(defn- setup-schema-document-summaries!
  "Install the maintained root-document progress read model.

   Preconditions: topics and flashcards exist with their final columns.
   Postconditions: each current root has one summary row, and statement triggers
   refresh every root affected by later topic or flashcard writes.
   Invariant: summary counts use the same item and sync definitions as
   get-document-status. A trigger defect is an implementation bug."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS document_summaries (
      root_topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      total_items BIGINT NOT NULL DEFAULT 0,
      done_items BIGINT NOT NULL DEFAULT 0,
      total_cards BIGINT NOT NULL DEFAULT 0,
      synced_cards BIGINT NOT NULL DEFAULT 0,
      updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
      CHECK (done_items <= total_items),
      CHECK (synced_cards <= total_cards)
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_document_summaries_user
                      ON document_summaries(user_id)"])

  ;; Resolve a topic to its current root. Deletes pass the nearest surviving
  ;; parent, so this function never needs deleted rows to reconstruct ancestry.
  (jdbc/execute! ds ["
    CREATE OR REPLACE FUNCTION freememo_root_topic_id(p_topic_id INTEGER)
    RETURNS INTEGER
    LANGUAGE SQL
    STABLE
    AS $$
      WITH RECURSIVE ancestors AS (
        SELECT id, parent_id
        FROM topics
        WHERE id = p_topic_id
        UNION ALL
        SELECT parent.id, parent.parent_id
        FROM topics parent
        JOIN ancestors child ON parent.id = child.parent_id
      )
      SELECT id
      FROM ancestors
      WHERE parent_id IS NULL
      LIMIT 1
    $$"])

  ;; Recompute only the named roots. This is deliberately a recomputation, not
  ;; delta arithmetic: moves, cascades and sync-state changes then share one
  ;; correctness path. Statement triggers keep bulk imports at one refresh per
  ;; affected root rather than one refresh per inserted row.
  (jdbc/execute! ds [(str "
    CREATE OR REPLACE FUNCTION freememo_refresh_document_summaries(p_root_ids INTEGER[])
    RETURNS VOID
    LANGUAGE plpgsql
    AS $$
    BEGIN
      IF p_root_ids IS NULL OR cardinality(p_root_ids) = 0 THEN
        RETURN;
      END IF;

      DELETE FROM document_summaries summary
      WHERE summary.root_topic_id = ANY(p_root_ids)
        AND NOT EXISTS (
          SELECT 1 FROM topics root
          WHERE root.id = summary.root_topic_id
            AND root.parent_id IS NULL
            AND root.user_id IS NOT NULL
        );

      INSERT INTO document_summaries
        (root_topic_id, user_id, total_items, done_items,
         total_cards, synced_cards, updated_at)
      SELECT root.id,
             root.user_id,
             CASE WHEN root.kind = 'pdf'
                  THEN COALESCE(page_stats.total_items, 0)
                  ELSE COALESCE(descendant_stats.total_items, 0)
             END,
             CASE WHEN root.kind = 'pdf'
                  THEN COALESCE(page_stats.done_items, 0)
                  ELSE COALESCE(descendant_stats.done_items, 0)
             END,
             COALESCE(card_stats.total_cards, 0),
             COALESCE(card_stats.synced_cards, 0),
             NOW()
      FROM topics root
      LEFT JOIN LATERAL (
        SELECT COUNT(*) AS total_items,
               COUNT(*) FILTER (WHERE page.status = 'done') AS done_items
        FROM topics page
        WHERE page.parent_id = root.id AND page.kind = 'page'
      ) page_stats ON TRUE
      LEFT JOIN LATERAL (
        SELECT COUNT(*) AS total_items,
               COUNT(*) FILTER (WHERE descendant.status = 'done') AS done_items
        FROM (
          WITH RECURSIVE descendants AS (
            SELECT root.id, root.status
            UNION ALL
            SELECT child.id, child.status
            FROM topics child
            JOIN descendants parent ON child.parent_id = parent.id
          )
          SELECT id, status FROM descendants
        ) descendant
      ) descendant_stats ON TRUE
      LEFT JOIN LATERAL (
        SELECT COUNT(*) AS total_cards,
               COUNT(*) FILTER (WHERE NOT " (unsynced-sql "card.") ") AS synced_cards
        FROM flashcards card
        WHERE card.root_topic_id = root.id
      ) card_stats ON TRUE
      WHERE root.id = ANY(p_root_ids)
        AND root.parent_id IS NULL
        AND root.user_id IS NOT NULL
      ON CONFLICT (root_topic_id) DO UPDATE
      SET user_id = EXCLUDED.user_id,
          total_items = EXCLUDED.total_items,
          done_items = EXCLUDED.done_items,
          total_cards = EXCLUDED.total_cards,
          synced_cards = EXCLUDED.synced_cards,
          updated_at = EXCLUDED.updated_at;
    END
    $$")])

  (jdbc/execute! ds ["
    CREATE OR REPLACE FUNCTION freememo_refresh_summaries_after_topic_insert()
    RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    DECLARE affected INTEGER[];
    BEGIN
      SELECT array_agg(DISTINCT freememo_root_topic_id(inserted.id))
      INTO affected
      FROM inserted;
      PERFORM freememo_refresh_document_summaries(affected);
      RETURN NULL;
    END
    $$"])
  (jdbc/execute! ds ["
    CREATE OR REPLACE FUNCTION freememo_refresh_summaries_after_topic_update()
    RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    DECLARE affected INTEGER[];
    BEGIN
      WITH RECURSIVE changed AS (
        SELECT old_rows.id, old_rows.parent_id
        FROM old_rows
        JOIN new_rows USING (id)
        WHERE new_rows.parent_id IS DISTINCT FROM old_rows.parent_id
           OR new_rows.kind IS DISTINCT FROM old_rows.kind
           OR new_rows.status IS DISTINCT FROM old_rows.status
           OR new_rows.user_id IS DISTINCT FROM old_rows.user_id
      ),
      old_ancestors(changed_id, id, parent_id) AS (
        SELECT id, id, parent_id
        FROM changed
        UNION ALL
        SELECT ancestor.changed_id,
               parent.id,
               CASE WHEN old_parent.id IS NOT NULL
                    THEN old_parent.parent_id
                    ELSE parent.parent_id
               END
        FROM old_ancestors ancestor
        JOIN topics parent ON parent.id = ancestor.parent_id
        LEFT JOIN old_rows old_parent ON old_parent.id = parent.id
      ),
      old_roots AS (
        SELECT DISTINCT id AS root_id
        FROM old_ancestors
        WHERE parent_id IS NULL
      ),
      new_roots AS (
        SELECT DISTINCT freememo_root_topic_id(new_rows.id) AS root_id
        FROM new_rows
        JOIN changed USING (id)
      )
      SELECT array_agg(DISTINCT root_id)
      INTO affected
      FROM (
        SELECT root_id FROM new_roots
        UNION
        SELECT root_id FROM old_roots
      ) roots
      WHERE root_id IS NOT NULL;
      PERFORM freememo_refresh_document_summaries(affected);
      RETURN NULL;
    END
    $$"])
  (jdbc/execute! ds ["
    CREATE OR REPLACE FUNCTION freememo_refresh_summaries_after_topic_delete()
    RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    DECLARE affected INTEGER[];
    BEGIN
      SELECT array_agg(DISTINCT root_id)
      INTO affected
      FROM (
        SELECT freememo_root_topic_id(deleted.parent_id) AS root_id
        FROM deleted
        WHERE deleted.parent_id IS NOT NULL
        UNION
        SELECT deleted.id
        FROM deleted
        WHERE deleted.parent_id IS NULL
      ) roots
      WHERE root_id IS NOT NULL;
      PERFORM freememo_refresh_document_summaries(affected);
      RETURN NULL;
    END
    $$"])
  (jdbc/execute! ds ["
    CREATE OR REPLACE FUNCTION freememo_refresh_summaries_after_card_write()
    RETURNS TRIGGER
    LANGUAGE plpgsql
    AS $$
    DECLARE affected INTEGER[];
    BEGIN
      IF TG_OP = 'INSERT' THEN
        SELECT array_agg(DISTINCT root_topic_id) INTO affected FROM inserted;
      ELSIF TG_OP = 'DELETE' THEN
        SELECT array_agg(DISTINCT root_topic_id) INTO affected FROM deleted;
      ELSE
        SELECT array_agg(DISTINCT root_topic_id)
        INTO affected
        FROM (
          SELECT root_topic_id FROM inserted
          UNION
          SELECT root_topic_id FROM deleted
        ) roots;
      END IF;
      PERFORM freememo_refresh_document_summaries(affected);
      RETURN NULL;
    END
    $$"])

  (doseq [trigger ["freememo_document_summary_topic_insert"
                   "freememo_document_summary_topic_update"
                   "freememo_document_summary_topic_delete"]]
    (jdbc/execute! ds [(str "DROP TRIGGER IF EXISTS " trigger " ON topics")]))
  (doseq [trigger ["freememo_document_summary_card_insert"
                   "freememo_document_summary_card_update"
                   "freememo_document_summary_card_delete"]]
    (jdbc/execute! ds [(str "DROP TRIGGER IF EXISTS " trigger " ON flashcards")]))
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_topic_insert
    AFTER INSERT ON topics
    REFERENCING NEW TABLE AS inserted
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_topic_insert()"])
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_topic_update
    AFTER UPDATE ON topics
    REFERENCING OLD TABLE AS old_rows NEW TABLE AS new_rows
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_topic_update()"])
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_topic_delete
    AFTER DELETE ON topics
    REFERENCING OLD TABLE AS deleted
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_topic_delete()"])
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_card_insert
    AFTER INSERT ON flashcards
    REFERENCING NEW TABLE AS inserted
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_card_write()"])
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_card_update
    AFTER UPDATE ON flashcards
    REFERENCING OLD TABLE AS deleted NEW TABLE AS inserted
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_card_write()"])
  (jdbc/execute! ds ["
    CREATE TRIGGER freememo_document_summary_card_delete
    AFTER DELETE ON flashcards
    REFERENCING OLD TABLE AS deleted
    FOR EACH STATEMENT
    EXECUTE FUNCTION freememo_refresh_summaries_after_card_write()"])

  ;; Rebuild once at boot. This repairs an interrupted install and applies a
  ;; changed summary definition to rows that an older trigger maintained.
  ;; Navigation reads remain constant-time because this work is not on them.
  (jdbc/execute! ds ["
    SELECT freememo_refresh_document_summaries(array_agg(root.id))
    FROM topics root
    WHERE root.parent_id IS NULL
      AND root.user_id IS NOT NULL"]))

(defn- setup-schema-poem!
  "Install `poem_packages`, the structured body of a kind `poem` topic.

   Preconditions: `setup-schema-topics!` has run — `topic_id` FKs topics — and
   `setup-schema-core!` has run, because `user_id` FKs users.
   Postconditions: one row per poem topic, holding the four package documents
   as JSONB exactly as `plans/poem-package-format.md` §7 states.

   Why a table rather than `topics.content`: the reader addresses a verse by
   number and a note by `(verse, word)`, and HTML with `data-*` attributes
   cannot be queried or validated. `topics.content` still holds a rendered
   fallback, so search and `content_preview` work (§7); the reader never reads
   it.

   `topic_id` is UNIQUE, so a topic owns at most one package — the reader's
   `get-poem-package` returns one row or nil, with no ordering to choose.
   Both foreign keys cascade: a deleted topic and a deleted account each take
   the package with them, which is what keeps this off every delete path."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS poem_packages (
      id SERIAL PRIMARY KEY,
      topic_id INTEGER NOT NULL UNIQUE REFERENCES topics(id) ON DELETE CASCADE,
      user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
      manifest JSONB NOT NULL DEFAULT '{}'::jsonb,
      verses JSONB NOT NULL DEFAULT '[]'::jsonb,
      notes JSONB NOT NULL DEFAULT '[]'::jsonb,
      sources JSONB NOT NULL DEFAULT '[]'::jsonb,
      created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
    )"])
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_poem_packages_user
                      ON poem_packages(user_id)"]))

(defn- setup-schema-code!
  "Source text and import selection for git-forge repositories.

   `topic_code` holds one file's text for a kind='code' topic. The text lives
   here rather than in `topics.content` because the editor beside the code pane
   holds the user's NOTES, and one column cannot be both. Same split as a
   video's transcript, for the same reason (plans/git-forge-integration.md D3).

   Split out of setup-schema (see 7.4); called from it in order."
  []
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS topic_code (
      topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
      source TEXT NOT NULL,
      byte_size INTEGER NOT NULL,
      lang TEXT,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; Search reads this column, so it carries the trigram GIN index
  ;; `topics.content_text` carries. NEVER a btree index here: a btree row cannot
  ;; exceed about 2704 bytes and a source file routinely does, which is why
  ;; idx_topics_source_content_digest indexes a digest instead of the text.
  (jdbc/execute! ds ["CREATE INDEX IF NOT EXISTS idx_topic_code_source_trgm
                      ON topic_code USING GIN (source gin_trgm_ops)"])

  ;; `|| ''` is load-bearing, for the reason topics.content_preview records:
  ;; left(col, n) over a TOASTed value takes a byte slice that can end mid
  ;; character, and the resulting invalid-byte-sequence error aborts the whole
  ;; statement rather than the one row.
  (jdbc/execute! ds ["ALTER TABLE topic_code ADD COLUMN IF NOT EXISTS source_preview TEXT
                      GENERATED ALWAYS AS (left(source || '', 400)) STORED"])

  ;; One row per kind='repo' topic: what an update needs to repeat the first
  ;; import with no picker. `forge` says which API the URL speaks — a
  ;; self-hosted host name cannot tell GitLab from Forgejo. `ref` is the branch,
  ;; tag or sha the user asked for, resolved again on every update. `selection`
  ;; is the directory decisions and the extension set (freememo.repo-tree).
  ;; It hangs off the repo node rather than a commit because every commit under
  ;; the repository reuses it.
  (jdbc/execute! ds ["
    CREATE TABLE IF NOT EXISTS repo_selections (
      topic_id INTEGER PRIMARY KEY REFERENCES topics(id) ON DELETE CASCADE,
      forge TEXT NOT NULL,
      ref TEXT NOT NULL,
      selection JSONB NOT NULL,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )"])

  ;; The commit a staged forge tree came from ({:forge :origin-url :repo-url
  ;; :path :ref :sha}). On the session row, not in process memory, so a picker
  ;; confirmed after a restart still knows what it holds. upload_sessions is
  ;; excluded from account transfer, so this column needs no transfer decision.
  (jdbc/execute! ds ["ALTER TABLE upload_sessions ADD COLUMN IF NOT EXISTS forge_source JSONB"]))

;; ---------------------------------------------------------------------------
;; Flashcard uniqueness — plans/flashcard-dedupe-index-repair.md
;; ---------------------------------------------------------------------------

(def ^:private flashcard-unique-index-ddl
  "[name ddl] for the two indexes that make `insert-flashcards!`'s arbiter-less
   ON CONFLICT DO NOTHING a dedupe.

   Two partial indexes, not one, because a B-tree index treats NULL as distinct:
   one index over both nullable columns could not stop a duplicate. The same
   rule is why occlusion, score and overlapping cards never meet here — they
   carry NULL in both question and cloze.

   They index md5(question) / md5(cloze), NOT the text itself. A B-tree index
   row cannot exceed 1/3 of an 8 KB page (~2704 bytes), while input-check/
   card-max permits 10 000 characters — so indexing the text made any card
   past ~2704 bytes UNINSERTABLE, with the driver reporting \"index row size
   N exceeds btree version 4 maximum 2704\" and no hint that two layers
   disagreed. A SuperMemo item carries full HTML plus appended <img> tags and
   clears that ceiling routinely; it was the first path to expose it.

   The digest preserves the constraint exactly — a collision is what would
   weaken it, and md5 collisions on same-topic card text are not a practical
   concern — while a `left(question, N)` prefix would NOT: two cards
   differing only past N would collide and `insert-flashcards!`'s
   ON CONFLICT DO NOTHING would silently drop one."
  [["idx_flashcards_unique_basic_digest"
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_flashcards_unique_basic_digest
     ON flashcards(topic_id, kind, md5(question)) WHERE cloze IS NULL"]
   ["idx_flashcards_unique_cloze_digest"
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_flashcards_unique_cloze_digest
     ON flashcards(topic_id, kind, md5(cloze)) WHERE question IS NULL"]])

(defn- flashcard-unique-indexes-valid?
  "True when both uniqueness indexes exist on flashcards and are valid. That is
   the steady state, and in it setup-schema-flashcard-uniqueness! takes no lock."
  [conn]
  (let [names (mapv first flashcard-unique-index-ddl)]
    (= (count names)
      (:n (jdbc/execute-one! conn
            (into [(str "SELECT count(*) AS n FROM pg_index i
                          JOIN pg_class c ON c.oid = i.indexrelid
                         WHERE i.indrelid = 'flashcards'::regclass AND i.indisvalid
                           AND c.relname IN (" (str/join ", " (repeat (count names) "?")) ")")]
              names)
            {:builder-fn rs/as-unqualified-maps})))))

(def ^:private flashcard-dedupe-pairs-sql
  "Build flashcard_dedupe(loser_id, survivor_id): one row per flashcard that
   shares a key of `flashcard-unique-index-ddl` with a row that ranks higher.

   The keys and predicates MUST stay the index's. Two filters are added on
   top, because GROUP BY and PARTITION BY put all NULL keys together while the
   index keeps each NULL distinct: a NULL topic_id or a NULL text never
   conflicts, so it must never be grouped.

   The survivor is the member with the most that cannot be moved. In order:
     1. an Anki link — deleting the linked row would orphan its note;
     2. the most Anki reviews, read through the note id, because nothing
        writes anki_card_states.flashcard_id;
     3. the most local reviews — card_schedules reps plus kg_reviews rows;
     4. the most edit history;
     5. the latest edit, then the lowest id.
   `plans/flashcard-dedupe-audit.sql` repeats this ranking so its preview
   matches what this migration deletes. A change here is a change there."
  "CREATE TEMP TABLE flashcard_dedupe ON COMMIT DROP AS
   WITH keyed AS (
     SELECT id, 1 AS idx, topic_id, kind, md5(question) AS digest
       FROM flashcards
      WHERE cloze IS NULL AND question IS NOT NULL AND topic_id IS NOT NULL
     UNION ALL
     SELECT id, 2, topic_id, kind, md5(cloze)
       FROM flashcards
      WHERE question IS NULL AND cloze IS NOT NULL AND topic_id IS NOT NULL
   ), dup AS (
     SELECT * FROM (SELECT k.*, count(*) OVER (PARTITION BY idx, topic_id, kind, digest) AS n
                      FROM keyed k) x
      WHERE n > 1
   ), valued AS (
     SELECT d.id, d.idx, d.topic_id, d.kind, d.digest, f.anki_note_id, f.updated_at,
            COALESCE((SELECT sum(s.reps) FROM anki_card_states s
                       WHERE s.user_id = t.user_id
                         AND s.anki_note_id = f.anki_note_id), 0) AS anki_reps,
            COALESCE((SELECT sum(c.reps) FROM card_schedules c
                       WHERE c.flashcard_id = d.id), 0)
            + (SELECT count(*) FROM kg_reviews r WHERE r.flashcard_id = d.id) AS local_reviews,
            (SELECT count(*) FROM card_versions v
              WHERE v.scope_type = 'card' AND v.scope_id = d.id) AS versions
       FROM dup d
       JOIN flashcards f ON f.id = d.id
       LEFT JOIN topics t ON t.id = d.topic_id
   ), ranked AS (
     SELECT id, idx, topic_id, kind, digest,
            row_number() OVER (PARTITION BY idx, topic_id, kind, digest
                               ORDER BY (anki_note_id IS NOT NULL) DESC, anki_reps DESC,
                                        local_reviews DESC, versions DESC,
                                        updated_at DESC NULLS LAST, id ASC) AS rank
       FROM valued
   )
   SELECT l.id AS loser_id, s.id AS survivor_id
     FROM ranked l
     JOIN ranked s ON s.idx = l.idx AND s.topic_id = l.topic_id AND s.kind = l.kind
                  AND s.digest = l.digest AND s.rank = 1
    WHERE l.rank > 1")

(defn- dedupe-flashcards-for-unique-indexes!
  "Delete every flashcard that duplicates another under a key of
   `flashcard-unique-index-ddl`. One survivor stays per key, and what each
   deleted row carried moves onto it:
     - its content, as a card_versions row on the survivor when it differs, so
       the History modal can restore it;
     - its card_versions history, re-pointed;
     - its card_schedules, one per ord: the most reps wins, then the latest
       review, then the survivor's own row;
     - its kg_reviews log, re-pointed;
     - its anki_card_states, re-pointed when they describe the survivor's note;
     - its Anki note, as a tombstone when no remaining card holds that note,
       as every hard-delete path owes (see `record-anki-tombstones!`).

   Pre : `tx` is an open transaction that holds the locks
         setup-schema-flashcard-uniqueness! takes, and card_versions,
         card_schedules, kg_reviews and anki_card_states exist.
   Post: no two flashcards share a key of either index, so `tx` can build both.
         Returns {:deleted n …counts :pairs [[loser survivor] …]}.
   Idempotent: with no duplicates it writes nothing and returns {:deleted 0}."
  [tx]
  (let [written #(or (:next.jdbc/update-count %) 0)
        exec! (fn [stmt] (written (jdbc/execute-one! tx [stmt])))
        plain {:builder-fn rs/as-unqualified-maps}]
    ;; ON COMMIT DROP clears the table at commit only, so a second call inside
    ;; one transaction would find it still there.
    (jdbc/execute-one! tx ["DROP TABLE IF EXISTS pg_temp.flashcard_dedupe"])
    (jdbc/execute-one! tx [flashcard-dedupe-pairs-sql])
    (let [pairs (jdbc/execute! tx
                  ["SELECT loser_id, survivor_id FROM flashcard_dedupe
                    ORDER BY survivor_id, loser_id"] plain)]
      (if (empty? pairs)
        {:deleted 0}
        (let [versions-written
              (exec! "INSERT INTO card_versions (root_topic_id, scope_type, scope_id, kind,
                                                question, answer, cloze, overlapping)
                     SELECT DISTINCT ON (d.survivor_id, l.question, l.answer, l.cloze, l.overlapping)
                            COALESCE(s.root_topic_id, l.root_topic_id), 'card', d.survivor_id,
                            l.kind, l.question, l.answer, l.cloze, l.overlapping
                       FROM flashcard_dedupe d
                       JOIN flashcards l ON l.id = d.loser_id
                       JOIN flashcards s ON s.id = d.survivor_id
                      WHERE COALESCE(s.root_topic_id, l.root_topic_id) IS NOT NULL
                        AND (l.question    IS DISTINCT FROM s.question
                          OR l.answer      IS DISTINCT FROM s.answer
                          OR l.cloze       IS DISTINCT FROM s.cloze
                          OR l.overlapping IS DISTINCT FROM s.overlapping)")
              versions-moved
              (exec! "UPDATE card_versions v SET scope_id = d.survivor_id
                       FROM flashcard_dedupe d
                      WHERE v.scope_type = 'card' AND v.scope_id = d.loser_id")
              ;; card_schedules is keyed (flashcard_id, ord), so a survivor can
              ;; hold one schedule per ord. Drop every losing schedule first,
              ;; which leaves at most one per (survivor, ord) and lets the move
              ;; below touch the primary key without a collision.
              schedules-dropped
              (exec! "DELETE FROM card_schedules c
                      USING (SELECT cs.flashcard_id, cs.ord,
                                    row_number() OVER (
                                      PARTITION BY m.survivor_id, cs.ord
                                      ORDER BY cs.reps DESC, cs.last_review DESC NULLS LAST,
                                               (cs.flashcard_id = m.survivor_id) DESC,
                                               cs.flashcard_id) AS rank
                               FROM (SELECT loser_id AS member_id, survivor_id FROM flashcard_dedupe
                                     UNION
                                     SELECT survivor_id, survivor_id FROM flashcard_dedupe) m
                               JOIN card_schedules cs ON cs.flashcard_id = m.member_id) r
                     WHERE c.flashcard_id = r.flashcard_id AND c.ord = r.ord AND r.rank > 1")
              schedules-moved
              (exec! "UPDATE card_schedules c SET flashcard_id = d.survivor_id
                       FROM flashcard_dedupe d WHERE c.flashcard_id = d.loser_id")
              reviews-moved
              (exec! "UPDATE kg_reviews r SET flashcard_id = d.survivor_id
                       FROM flashcard_dedupe d WHERE r.flashcard_id = d.loser_id")
              ;; A state of any other note belongs to a note the tombstone below
              ;; retires, so the delete's cascade removes it, as a user delete would.
              states-moved
              (exec! "UPDATE anki_card_states a SET flashcard_id = d.survivor_id
                       FROM flashcard_dedupe d JOIN flashcards s ON s.id = d.survivor_id
                      WHERE a.flashcard_id = d.loser_id AND a.anki_note_id = s.anki_note_id")
              ;; Read BEFORE the delete: afterwards the rows are gone. A note that
              ;; a remaining card still holds is NOT orphaned — a tombstone for it
              ;; would make the next reconcile delete a live card's note.
              orphaned-notes
              (jdbc/execute! tx
                ["SELECT DISTINCT t.user_id, l.anki_note_id, l.anki_baseline_digest
                    FROM flashcard_dedupe d
                    JOIN flashcards l ON l.id = d.loser_id
                    JOIN topics t ON t.id = l.topic_id
                   WHERE l.anki_note_id IS NOT NULL
                     AND NOT EXISTS (SELECT 1 FROM flashcards k
                                      WHERE k.anki_note_id = l.anki_note_id
                                        AND NOT EXISTS (SELECT 1 FROM flashcard_dedupe x
                                                         WHERE x.loser_id = k.id))"]
                plain)
              deleted
              (exec! "DELETE FROM flashcards f USING flashcard_dedupe d WHERE f.id = d.loser_id")
              tombstones
              (reduce-kv (fn [n user-id rows] (+ n (record-anki-tombstones! tx user-id rows)))
                0 (group-by :user_id orphaned-notes))]
          {:deleted deleted
           :survivors (count (distinct (map :survivor_id pairs)))
           :versions-written versions-written
           :versions-moved versions-moved
           :schedules-dropped schedules-dropped
           :schedules-moved schedules-moved
           :reviews-moved reviews-moved
           :states-moved states-moved
           :tombstones tombstones
           :pairs (mapv (juxt :loser_id :survivor_id) pairs)})))))

(defn- setup-schema-flashcard-uniqueness!
  "Remove duplicate flashcards and install `flashcard-unique-index-ddl`, in ONE
   transaction, so no duplicate can arrive between the cleanup and the build.

   The last step of setup-schema, because the cleanup re-points tables that
   earlier steps create. Idempotent: once both indexes are valid it only reads
   pg_index, and takes no lock.

   The lock blocks writers on the five tables the cleanup touches, for the
   seconds the repair takes. It matters under blue/green: the old color keeps
   writing cards while this one boots. The order puts card_versions first,
   because update-flashcard-versioned! writes it before flashcards, and a
   cascade from flashcards reaches the other three after flashcards. A lock
   wait past lock_timeout aborts the repair, which the next boot retries.

   Never silent on failure: without these indexes `insert-flashcards!` still
   runs (a bare ON CONFLICT DO NOTHING needs no arbiter) but stops deduping,
   and duplicates collect. From 2026-08-03 to this fix, production logged
   ::flashcard-unique-index-migration on every boot for exactly that reason.
   See plans/flashcard-dedupe-index-repair.md."
  []
  (when (and (topics-table-exists?) (not (old-tables-exist?))
          (not (flashcard-unique-indexes-valid? ds)))
    (try
      (let [result (jdbc/with-transaction [tx ds]
                     (jdbc/execute! tx ["SET LOCAL lock_timeout = '15s'"])
                     (jdbc/execute! tx ["LOCK TABLE card_versions, flashcards, card_schedules,
                                                   kg_reviews, anki_card_states
                                         IN SHARE ROW EXCLUSIVE MODE"])
                     (let [result (dedupe-flashcards-for-unique-indexes! tx)]
                       (jdbc/execute! tx ["DROP INDEX IF EXISTS idx_flashcards_unique_topic"])
                       (doseq [[_ ddl] flashcard-unique-index-ddl]
                         (jdbc/execute! tx [ddl]))
                       ;; The text-keyed predecessors. Only after the replacements
                       ;; exist, so the table never goes without uniqueness.
                       (jdbc/execute! tx ["DROP INDEX IF EXISTS idx_flashcards_unique_basic"])
                       (jdbc/execute! tx ["DROP INDEX IF EXISTS idx_flashcards_unique_cloze"])
                       result))]
        (when (pos? (:deleted result))
          (tel/log! {:level :warn :id ::flashcard-dedupe
                     :data (-> result
                             (assoc :pair-count (count (:pairs result)))
                             (update :pairs #(vec (take 200 %))))}
            (str "Deleted " (:deleted result) " duplicate flashcard(s) so the uniqueness "
              "indexes could be built"))))
      (catch Exception e
        (tel/log! {:level :error :id ::flashcard-unique-index-migration
                   :data {:error (.getMessage e)}}
          "Could not install the flashcard uniqueness indexes — card dedupe is OFF")))))

(defn setup-schema []
  (tel/log! :info "Setting up database schema")

  (setup-schema-core!)
  (setup-schema-topics!)
  (setup-schema-poem!)
  (setup-schema-card-versions!)
  (setup-schema-activity!)
  (setup-schema-assistant!)
  (setup-schema-annotations!)
  (setup-schema-sources!)
  (setup-schema-credits!)
  (setup-schema-undo!)
  (setup-schema-kg!)
  (setup-schema-card-quiz!)
  (setup-schema-video!)
  (setup-schema-code!)
  (setup-schema-neoreader!)
  (setup-schema-account-transfer!)
  (setup-schema-study-scheduler!)
  (setup-schema-curriculum!)
  (setup-schema-activity-monitor!)
  (setup-schema-document-summaries!)
  (setup-schema-reading-positions!)
  (setup-schema-question-pins!)
  (setup-schema-walkthroughs!)
  ;; Last: it re-points tables that the steps above create.
  (setup-schema-flashcard-uniqueness!)

  ;; One-time grandfather credit grant (idempotent; no-op when credits disabled)
  (run-grandfather-migration!)

  ;; AI levels: every existing user moves to Best, once per database.
  (run-once! "2026-09-23-ai-levels-best" move-everyone-to-best-level!)

  ;; Card-history precondition: inline base64 images out of card fields, so no
  ;; version row can ever duplicate an image payload. Idempotent; must run
  ;; before any save can version (plans/card-edit-history.md §3).
  (normalize-inline-card-images!)

  ;; Hourly purge of staged documents whose undo window has elapsed.
  (start-purge-scheduler!)

  (tel/log! :info "Database ready"))

;; ---------------------------------------------------------------------------
;; Data migration (old tables → topics)
;; ---------------------------------------------------------------------------

(defn migrate-to-topics!
  "Migrate documents, pages, content_items → unified topics table.
   Preserves document IDs. Pages and content_items get new IDs."
  []
  (tel/log! :info "Starting migration to unified topics model")
  (jdbc/with-transaction [tx ds]
    ;; 1. Migrate documents → topics (preserve IDs)
    (let [doc-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM documents"]))]
      (tel/log! {:level :info :id ::migrate-documents :data {:count doc-count}} "Migrating documents")
      (jdbc/execute! tx
        ["INSERT INTO topics (id, user_id, parent_id, kind, title, content, source_url,
                              status, priority, interval_days, a_factor, next_review_at, last_review_at,
                              created_at)
          SELECT id, user_id, NULL,
                 CASE source_type WHEN 'topic' THEN 'basic' WHEN 'pdf' THEN 'pdf'
                                  WHEN 'epub' THEN 'epub' WHEN 'web' THEN 'web'
                                  ELSE COALESCE(source_type, 'basic') END,
                 COALESCE(filename, 'Untitled'), html_content, source_url,
                 COALESCE(status, 'active'), COALESCE(priority, 50),
                 COALESCE(interval_days, 1.0), COALESCE(a_factor, 2.0),
                 next_review_at, last_review_at, uploaded_at
          FROM documents
          ON CONFLICT (id) DO NOTHING"]))

    ;; Reset sequence past max document ID to avoid conflicts with auto-generated IDs
    (jdbc/execute! tx ["SELECT setval('topics_id_seq', (SELECT COALESCE(MAX(id), 0) FROM topics))"])

    ;; 2. Migrate document files → topic_files
    (let [file-count (:count (jdbc/execute-one! tx
                               ["SELECT COUNT(*) AS count FROM documents WHERE file_data IS NOT NULL"]))]
      (tel/log! {:level :info :id ::migrate-files :data {:count file-count}} "Migrating document files")
      (jdbc/execute! tx
        ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
          SELECT id, file_data, file_size, mime_type
          FROM documents WHERE file_data IS NOT NULL
          ON CONFLICT (topic_id) DO NOTHING"]))

    ;; 3. Migrate pages → topics (new IDs, build mapping)
    (let [page-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM pages"]))]
      (tel/log! {:level :info :id ::migrate-pages :data {:count page-count}} "Migrating pages")
      ;; Create temp mapping table
      (jdbc/execute! tx ["CREATE TEMP TABLE page_id_map (old_page_id INTEGER PRIMARY KEY, new_topic_id INTEGER)"])
      ;; Insert pages as topics one by one and build mapping
      (let [pages (jdbc/execute! tx
                    ["SELECT p.id AS page_id, p.document_id, p.page_number, p.text, p.is_done,
                              p.priority, p.interval_days, p.a_factor, p.next_review_at,
                              p.last_review_at, p.created_at, d.user_id
                       FROM pages p JOIN documents d ON p.document_id = d.id
                       ORDER BY p.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [p pages]
          (let [new-topic (jdbc/execute-one! tx
                            ["INSERT INTO topics (user_id, parent_id, kind, title, content, page_number,
                                                  status, priority, interval_days, a_factor,
                                                  next_review_at, last_review_at, created_at)
                              VALUES (?, ?, 'page', ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                              ON CONFLICT (parent_id, page_number) WHERE page_number IS NOT NULL
                              DO NOTHING
                              RETURNING id"
                             (:user_id p) (:document_id p) (str "Page " (:page_number p))
                             (:text p) (:page_number p)
                             (if (true? (:is_done p)) "done" "active")
                             (or (:priority p) priority/priority-default)
                             (or (:interval_days p) 1.0)
                             (or (:a_factor p) 2.0)
                             (:next_review_at p) (:last_review_at p)
                             (:created_at p)]
                            {:builder-fn rs/as-unqualified-maps})]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO page_id_map (old_page_id, new_topic_id) VALUES (?, ?)
                    ON CONFLICT DO NOTHING"
                 (:page_id p) new-id]))))))

    ;; 4. Migrate content_items → topics (two passes)
    (let [ci-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM content_items"]))]
      (tel/log! {:level :info :id ::migrate-content-items :data {:count ci-count}} "Migrating content items")
      (jdbc/execute! tx ["CREATE TEMP TABLE ci_id_map (old_ci_id INTEGER PRIMARY KEY, new_topic_id INTEGER)"])

      ;; Pass 1: content_items without parent_content_item_id
      (let [items (jdbc/execute! tx
                    ["SELECT ci.id, ci.document_id, ci.page_number, ci.kind, ci.content,
                            ci.status, ci.priority, ci.interval_days, ci.a_factor,
                            ci.next_review_at, ci.last_review_at,
                            ci.created_at, d.user_id
                     FROM content_items ci JOIN documents d ON ci.document_id = d.id
                     WHERE ci.parent_content_item_id IS NULL
                     ORDER BY ci.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [ci items]
          ;; Find parent: if page_number is set, look up the page topic; otherwise use document_id
          (let [parent-id (if-let [pn (:page_number ci)]
                            (or (:new_topic_id
                                 (jdbc/execute-one! tx
                                   ["SELECT new_topic_id FROM page_id_map pm
                                      JOIN pages p ON pm.old_page_id = p.id
                                      WHERE p.document_id = ? AND p.page_number = ?"
                                    (:document_id ci) pn]
                                   {:builder-fn rs/as-unqualified-maps}))
                                ;; Fallback: look up directly in topics
                              (:id
                               (jdbc/execute-one! tx
                                 ["SELECT id FROM topics WHERE parent_id = ? AND page_number = ?"
                                  (:document_id ci) pn]
                                 {:builder-fn rs/as-unqualified-maps}))
                                ;; Last resort: use document as parent
                              (:document_id ci))
                            (:document_id ci))
                ci-kind (or (:kind ci) "html")
                title (let [raw (or (:content ci) "")]
                        (-> raw
                          (str/replace #"<[^>]+>" "")
                          (subs 0 (min 80 (count (str/replace raw #"<[^>]+>" ""))))
                          str/trim
                          (#(if (str/blank? %) "Extract" %))))
                new-topic (jdbc/execute-one! tx
                            (sql/format {:insert-into :topics
                                         :values [{:user_id (:user_id ci)
                                                   :parent_id parent-id
                                                   :kind "basic"
                                                   :title title
                                                   :content (sanitize-utf8 (:content ci))
                                                   :status (or (:status ci) "active")
                                                   :priority (or (:priority ci) priority/priority-default)
                                                   :interval_days (or (:interval_days ci) 1.0)
                                                   :a_factor (or (:a_factor ci) 2.0)
                                                   :next_review_at (:next_review_at ci)
                                                   :last_review_at (:last_review_at ci)
                                                   :created_at (:created_at ci)}]
                                         :returning [:id]}
                              {:builder-fn rs/as-unqualified-maps}))]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO ci_id_map (old_ci_id, new_topic_id) VALUES (?, ?)
                  ON CONFLICT DO NOTHING"
                 (:id ci) new-id])))))

      ;; Pass 2: content_items with parent_content_item_id
      (let [items (jdbc/execute! tx
                    ["SELECT ci.id, ci.document_id, ci.page_number, ci.kind, ci.content,
                            ci.parent_content_item_id, ci.status, ci.priority,
                            ci.interval_days, ci.a_factor, ci.next_review_at,
                            ci.last_review_at, ci.created_at, d.user_id
                     FROM content_items ci JOIN documents d ON ci.document_id = d.id
                     WHERE ci.parent_content_item_id IS NOT NULL
                     ORDER BY ci.id"]
                    {:builder-fn rs/as-unqualified-maps})]
        (doseq [ci items]
          (let [parent-id (or (:new_topic_id
                               (jdbc/execute-one! tx
                                 ["SELECT new_topic_id FROM ci_id_map WHERE old_ci_id = ?"
                                  (:parent_content_item_id ci)]
                                 {:builder-fn rs/as-unqualified-maps}))
                            (:document_id ci))
                title (let [raw (or (:content ci) "")]
                        (-> raw
                          (str/replace #"<[^>]+>" "")
                          (subs 0 (min 80 (count (str/replace raw #"<[^>]+>" ""))))
                          str/trim
                          (#(if (str/blank? %) "Extract" %))))
                new-topic (jdbc/execute-one! tx
                            (sql/format {:insert-into :topics
                                         :values [{:user_id (:user_id ci)
                                                   :parent_id parent-id
                                                   :kind "basic"
                                                   :title title
                                                   :content (sanitize-utf8 (:content ci))
                                                   :status (or (:status ci) "active")
                                                   :priority (or (:priority ci) priority/priority-default)
                                                   :interval_days (or (:interval_days ci) 1.0)
                                                   :a_factor (or (:a_factor ci) 2.0)
                                                   :next_review_at (:next_review_at ci)
                                                   :last_review_at (:last_review_at ci)
                                                   :created_at (:created_at ci)}]
                                         :returning [:id]}
                              {:builder-fn rs/as-unqualified-maps}))]
            (when-let [new-id (:id new-topic)]
              (jdbc/execute! tx
                ["INSERT INTO ci_id_map (old_ci_id, new_topic_id) VALUES (?, ?)
                  ON CONFLICT DO NOTHING"
                 (:id ci) new-id]))))))

    ;; 5. Migrate flashcards — set topic_id and root_topic_id
    (tel/log! :info "Migrating flashcards")
    ;; Cards linked to content_items
    (jdbc/execute! tx
      ["UPDATE flashcards f SET
          topic_id = m.new_topic_id,
          root_topic_id = f.document_id
        FROM ci_id_map m
        WHERE f.content_item_id = m.old_ci_id
          AND f.topic_id IS NULL"])
    ;; Cards linked to pages (no content_item_id)
    (jdbc/execute! tx
      ["UPDATE flashcards f SET
          topic_id = pm.new_topic_id,
          root_topic_id = f.document_id
        FROM page_id_map pm
        JOIN pages p ON pm.old_page_id = p.id
        WHERE f.content_item_id IS NULL
          AND f.document_id = p.document_id
          AND f.page_number = p.page_number
          AND f.topic_id IS NULL"])
    ;; Any remaining cards without topic_id — set root_topic_id at least
    (jdbc/execute! tx
      ["UPDATE flashcards SET root_topic_id = document_id
        WHERE root_topic_id IS NULL AND document_id IS NOT NULL"])
    ;; Backfill: cards with null topic_id get topic_id = root_topic_id
    (jdbc/execute! tx
      ["UPDATE flashcards SET topic_id = root_topic_id
        WHERE topic_id IS NULL AND root_topic_id IS NOT NULL"])

    ;; 5b. Fix web/wikipedia articles: reparent extracts from orphan "Page 1" topics to root
    ;; Old web/wikipedia imports created dummy page rows; migration turned them into page topics
    (let [reparented (jdbc/execute-one! tx
                       ["UPDATE topics SET parent_id = page_topics.parent_id
                         FROM (
                           SELECT p.id as page_id, p.parent_id
                           FROM topics p
                           JOIN topics r ON p.parent_id = r.id
                           WHERE p.kind = 'page' AND r.kind IN ('web', 'wikipedia')
                         ) AS page_topics
                         WHERE topics.parent_id = page_topics.page_id"])]
      (when (pos? (or (:next.jdbc/update-count reparented) 0))
        (tel/log! {:level :info :id ::reparent-extracts :data {:count (:next.jdbc/update-count reparented)}}
          "Reparented extracts from orphan page topics")))
    (jdbc/execute! tx
      ["DELETE FROM topics
        WHERE kind = 'page'
          AND parent_id IN (SELECT id FROM topics WHERE kind IN ('web', 'wikipedia'))"])

    ;; 6. Reset sequence
    (jdbc/execute! tx
      ["SELECT setval('topics_id_seq', GREATEST((SELECT COALESCE(MAX(id), 1) FROM topics), 1))"])

    ;; 7. Rename old tables
    (tel/log! :info "Renaming old tables")
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS content_items RENAME TO content_items_old"])
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS pages RENAME TO pages_old"])
    (jdbc/execute! tx ["ALTER TABLE IF EXISTS documents RENAME TO documents_old"])

    ;; Clean up temp tables
    (jdbc/execute! tx ["DROP TABLE IF EXISTS page_id_map"])
    (jdbc/execute! tx ["DROP TABLE IF EXISTS ci_id_map"])

    ;; Print stats
    (let [topic-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM topics"]))
          file-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM topic_files"]))
          card-count (:count (jdbc/execute-one! tx ["SELECT COUNT(*) AS count FROM flashcards WHERE topic_id IS NOT NULL"]))]
      (tel/log! {:level :info :id ::migration-complete
                 :data {:topics topic-count :files file-count :flashcards card-count}}
        "Migration complete"))))

;; ---------------------------------------------------------------------------
;; Settings (unchanged)
;; ---------------------------------------------------------------------------

(defn get-setting [user-id key]
  (-> (jdbc/execute-one! ds
        (sql/format {:select [:value]
                     :from [:settings]
                     :where [:and
                             [:= :user_id user-id]
                             [:= :key key]]}))
    :settings/value))

(defn set-setting
  "Upsert a per-user setting. Audits every change here (§3.4) — the single
   choke point for all settings/save-* callers, so no per-caller log is needed."
  [user-id key value]
  (let [r (jdbc/execute! ds
            (sql/format {:insert-into :settings
                         :values [{:user_id user-id :key key :value value}]
                         :on-conflict [:user_id :key]
                         :do-update-set {:value value
                                         :updated_at [:now]}}))]
    (log/audit! {:id ::setting-changed :user-id user-id :action :update
                 :entity :setting :entity-id key})
    r))

(defn set-settings!
  "Upsert several per-user settings in ONE transaction, so a reader never sees
   some of them written and the rest not. Audits each key, as set-setting does.
   Pre: `kvs` is a {key value} map of strings. Post: nil."
  [user-id kvs]
  (jdbc/with-transaction [tx ds]
    (doseq [[k v] kvs]
      (jdbc/execute! tx
        (sql/format {:insert-into :settings
                     :values [{:user_id user-id :key k :value v}]
                     :on-conflict [:user_id :key]
                     :do-update-set {:value v
                                     :updated_at [:now]}}))))
  (doseq [k (keys kvs)]
    (log/audit! {:id ::setting-changed :user-id user-id :action :update
                 :entity :setting :entity-id k}))
  nil)

(defn delete-setting
  "Delete a per-user setting (settings reset path). Audits here (§3.4)."
  [user-id key]
  (let [r (jdbc/execute! ds
            (sql/format {:delete-from :settings
                         :where [:and
                                 [:= :user_id user-id]
                                 [:= :key key]]}))]
    (log/audit! {:id ::setting-deleted :user-id user-id :action :delete
                 :entity :setting :entity-id key})
    r))

(defn get-settings
  "Batch variant of get-setting: {key value} for all of `ks` in one query.
   Keys with no row are absent from the result map. [] ks → {} (no DB hit)."
  [user-id ks]
  (if (empty? ks)
    {}
    (into {}
      (map (juxt :settings/key :settings/value))
      (jdbc/execute! ds
        (sql/format {:select [:key :value]
                     :from [:settings]
                     :where [:and
                             [:= :user_id user-id]
                             [:in :key ks]]})))))

(defn setting-keys-with-prefix
  "Post: every setting key `user-id` holds that starts with `prefix`, sorted.
   `starts_with` rather than LIKE, because `_` in a prefix is a LIKE wildcard."
  [user-id prefix]
  (mapv :key
    (jdbc/execute! ds
      ["SELECT key FROM settings WHERE user_id = ? AND starts_with(key, ?) ORDER BY key"
       user-id prefix]
      {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; Assistant chats (Socratic AI assistant)
;; ---------------------------------------------------------------------------

(defn create-assistant-chat!
  "Insert a chat for (user-id, root-topic-id) with `title` and `mode`.
   Pre:  `mode` is an assistant-modes :id — the caller validates, this fn does
         not (freememo.assistant/start-chat! resolves it first).
   Post: the row's mode = `mode`. Returns new id."
  ([user-id root-topic-id title mode]
   (create-assistant-chat! user-id root-topic-id title mode false))
  ([user-id root-topic-id title mode web-search?]
   (-> (jdbc/execute-one! ds
         (sql/format {:insert-into :assistant_chats
                      :values [{:user_id user-id
                                :root_topic_id root-topic-id
                                :title title
                                :mode mode
                                :web_search (boolean web-search?)
                                :provider_session_id (str (random-uuid))}]
                      :returning [:id]}))
     :assistant_chats/id)))

(defn get-assistant-chat
  "Chat row for `chat-id` IFF owned by `user-id` and not staged for deletion,
   else nil (ownership gate). Every chat read and write that takes a chat id
   from the client passes through here, so a staged chat is invisible to all of
   them.
   Post: {:assistant_chats/id … :assistant_chats/title … :assistant_chats/mode …}
   or nil. :mode may be NULL on a chat that predates the column — callers
   resolve it through assistant-modes/resolve-mode, never read it raw."
  [user-id chat-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :root_topic_id :title :mode :web_search :provider_session_id]
                 :from [:assistant_chats]
                 :where [:and [:= :id chat-id] [:= :user_id user-id]
                         [:is :staged_delete_id nil]]})))

(def ^:private local-epoch-ms-sql
  "Epoch milliseconds of a TIMESTAMP column, read as the server's local time.
   CURRENT_TIMESTAMP and now() fill these columns in the session time zone, so
   the same zone converts them back."
  "(extract(epoch FROM (%s AT TIME ZONE current_setting('TimeZone'))) * 1000)::bigint")

(defn get-assistant-chats
  "Live chats for (user-id, root-topic-id), newest activity first, without bodies.
   :mode and :web_search ride along so the panel reads the active chat's
   settings out of this already-watched list instead of a second query.
   Post: each row carries :activity_ms and :pinned_ms (nil when unpinned) as
   epoch milliseconds. No TIMESTAMP value is returned, so none crosses the wire."
  [user-id root-topic-id]
  (jdbc/execute! ds
    [(str "SELECT id, title, mode, web_search, "
       (format local-epoch-ms-sql "coalesce(updated_at, created_at)") " AS activity_ms, "
       (format local-epoch-ms-sql "pinned_at") " AS pinned_ms "
       "FROM assistant_chats "
       "WHERE user_id = ? AND root_topic_id = ? AND staged_delete_id IS NULL "
       "ORDER BY coalesce(updated_at, created_at) DESC, id DESC")
     user-id root-topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-last-opened-assistant-chat-id
  "The live chat the user opened most recently on this document, or nil.
   A chat never opened since the column existed has a NULL last_opened_at and
   is never chosen."
  [user-id root-topic-id]
  (:id (jdbc/execute-one! ds
         ["SELECT id FROM assistant_chats
            WHERE user_id = ? AND root_topic_id = ? AND staged_delete_id IS NULL
              AND last_opened_at IS NOT NULL
            ORDER BY last_opened_at DESC, id DESC LIMIT 1"
          user-id root-topic-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn insert-assistant-message!
  "Append a message and touch the parent chat's updated_at. Returns new id.
   Pre: role ∈ {\"user\",\"assistant\"} (enforced by table CHECK); `client-id`
   is the client's echo-correlation id for a user turn, nil for assistant rows.
   Post: the row's client_id = `client-id`."
  [chat-id role content client-id]
  (let [id (-> (jdbc/execute-one! ds
                 (sql/format {:insert-into :assistant_messages
                              :values [{:chat_id chat-id :role role :content content
                                        :client_id client-id}]
                              :returning [:id]}))
             :assistant_messages/id)]
    (jdbc/execute! ds
      (sql/format {:update :assistant_chats
                   :set {:updated_at [:now]}
                   :where [:= :id chat-id]}))
    id))

(defn get-assistant-messages
  "Transcript for `chat-id`, oldest first (learner/assistant turns only —
   reading context is injected transiently per send, not stored here)."
  [chat-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :role :content :client_id :created_at]
                 :from [:assistant_messages]
                 :where [:= :chat_id chat-id]
                 :order-by [[:id :asc]]})))

(defn get-assistant-message
  "One message IFF its chat belongs to user-id, else nil."
  [user-id message-id]
  (jdbc/execute-one! ds
    [(str "SELECT m.* FROM assistant_messages m "
       "JOIN assistant_chats c ON c.id = m.chat_id "
       "WHERE c.user_id = ? AND m.id = ?")
     user-id message-id]
    {:builder-fn rs/as-unqualified-maps}))

(def ^:private active-assistant-run-statuses
  ["queued" "streaming" "stop-requested"])

(defn- insert-assistant-run-evidence!
  "Insert the evidence records for one new run inside its creation transaction.
   Pre: run-id belongs to the transaction's new run. Each record has a unique
   :citation-key. Post: every record points to run-id. Empty evidence is a no-op."
  [tx run-id evidence]
  (when (seq evidence)
    (jdbc/execute! tx
      (sql/format
        {:insert-into :assistant_evidence
         :values (mapv (fn [{:keys [citation-key document-topic-id page-topic-id
                                    page-number anchor quoted-text quoted-html document-title
                                    display-label source-revision walkthrough-id]}]
                         {:run_id run-id
                          :citation_key citation-key
                          :document_topic_id document-topic-id
                          :page_topic_id page-topic-id
                          :page_number page-number
                          :anchor anchor
                          :quoted_text quoted-text
                          :quoted_html quoted-html
                          :document_title document-title
                          :display_label display-label
                          :source_revision source-revision
                          :walkthrough_id walkthrough-id})
                   evidence)}))))

(defn create-assistant-answer-run!
  "Create one user turn, its assistant placeholder, and its durable run.

   Pre: chat-id exists and the caller already passed the ownership and credit
   gates. `run` contains :run-uuid, snapshots, provider session, and model.
   Post: returns {:created true :run-id :run-uuid :user-message-id
   :assistant-message-id}, {:created false :active-run-uuid}, or
   {:created false :chat-missing? true} when the chat is gone or staged.
   Invariant: one chat has at most one active answer or compaction run. This
   transaction owns the chat lock that enforces the invariant."
  [chat-id user-text client-id
   {:keys [run-uuid retry-of-run-uuid provider-session-id requested-model
           request-snapshot source-snapshot evidence]}]
  (jdbc/with-transaction [tx ds]
    (if-not (jdbc/execute-one! tx [(str "SELECT id FROM assistant_chats WHERE id = ? "
                                       "AND staged_delete_id IS NULL FOR UPDATE")
                                  chat-id])
      {:created false :chat-missing? true}
      (if-let [active (jdbc/execute-one! tx
                        [(str "SELECT run_uuid FROM assistant_runs WHERE chat_id = ? "
                           "AND status IN ('queued','streaming','stop-requested') LIMIT 1")
                         chat-id]
                        {:builder-fn rs/as-unqualified-maps})]
        {:created false :active-run-uuid (:run_uuid active)}
        (let [user-message-id
              (:assistant_messages/id
                (jdbc/execute-one! tx
                  (sql/format {:insert-into :assistant_messages
                               :values [{:chat_id chat-id :role "user" :content user-text
                                         :client_id client-id}]
                               :returning [:id]})))
              assistant-message-id
              (:assistant_messages/id
                (jdbc/execute-one! tx
                  (sql/format {:insert-into :assistant_messages
                               :values [{:chat_id chat-id :role "assistant" :content ""}]
                               :returning [:id]})))
              run-id
              (:assistant_runs/id
                (jdbc/execute-one! tx
                  (sql/format {:insert-into :assistant_runs
                               :values [{:run_uuid run-uuid
                                         :chat_id chat-id
                                         :kind "answer"
                                         :status "queued"
                                         :user_message_id user-message-id
                                         :assistant_message_id assistant-message-id
                                         :retry_of_run_uuid retry-of-run-uuid
                                         :provider_session_id provider-session-id
                                         :requested_model requested-model
                                         :request_snapshot (->jsonb request-snapshot)
                                         :source_snapshot (->jsonb (or source-snapshot []))}]
                               :returning [:id]})))]
          (insert-assistant-run-evidence! tx run-id evidence)
          (jdbc/execute! tx ["UPDATE assistant_chats SET updated_at = CURRENT_TIMESTAMP WHERE id = ?"
                             chat-id])
          {:created true :run-id run-id :run-uuid run-uuid
           :user-message-id user-message-id
           :assistant-message-id assistant-message-id})))))

(defn create-assistant-compact-run!
  "Create a compaction run without adding a learner or assistant message.
   Pre: chat-id belongs to the caller and request-snapshot names a non-empty
   covered range. Post: the same creation result shape as answer runs,
   including {:created false :chat-missing? true}.
   Invariant: the chat lock rejects concurrent answer or compact operations."
  [chat-id {:keys [run-uuid provider-session-id requested-model request-snapshot
                   source-snapshot]}]
  (jdbc/with-transaction [tx ds]
    (if-not (jdbc/execute-one! tx [(str "SELECT id FROM assistant_chats WHERE id = ? "
                                       "AND staged_delete_id IS NULL FOR UPDATE")
                                  chat-id])
      {:created false :chat-missing? true}
      (if-let [active (jdbc/execute-one! tx
                        [(str "SELECT run_uuid FROM assistant_runs WHERE chat_id = ? "
                           "AND status IN ('queued','streaming','stop-requested') LIMIT 1")
                         chat-id]
                        {:builder-fn rs/as-unqualified-maps})]
        {:created false :active-run-uuid (:run_uuid active)}
        (let [run-id
              (:assistant_runs/id
                (jdbc/execute-one! tx
                  (sql/format {:insert-into :assistant_runs
                               :values [{:run_uuid run-uuid :chat_id chat-id
                                         :kind "compact" :status "queued"
                                         :provider_session_id provider-session-id
                                         :requested_model requested-model
                                         :request_snapshot (->jsonb request-snapshot)
                                         :source_snapshot (->jsonb (or source-snapshot []))}]
                               :returning [:id]})))]
          {:created true :run-id run-id :run-uuid run-uuid})))))

(defn append-assistant-run-text!
  "Append one provider delta while the run is active.
   Pre: delta is a non-empty string. Post: returns the complete stored text, or
   nil when Stop or another terminal transition already closed the boundary.
   Invariant: text never changes after a terminal state. The conditional UPDATE
   owns this invariant."
  [run-uuid delta]
  (some-> (jdbc/execute-one! ds
            [(str "UPDATE assistant_runs SET response_text = response_text || ?, "
               "status = 'streaming', started_at = COALESCE(started_at, CURRENT_TIMESTAMP), "
               "first_token_at = COALESCE(first_token_at, CURRENT_TIMESTAMP) "
               "WHERE run_uuid = ? AND status IN ('queued','streaming') "
               "RETURNING response_text")
             delta run-uuid]
            {:builder-fn rs/as-unqualified-maps})
    :response_text))

(defn replace-assistant-run-text!
  "Replace an active run's stored text before it completes. Used once, after
   the stream ends, to drop a question-map block the reply started with
   (plans/anchored-questions.md §6.3.2), so the transcript stores the prose.
   Pre: `text` is the prose of the run's own stored text.
   Post: true when the run was still active; a stopped or failed run keeps its
         text, under the same invariant as `append-assistant-run-text!`."
  [run-uuid text]
  (some? (jdbc/execute-one! ds
           ["UPDATE assistant_runs SET response_text = ?
              WHERE run_uuid = ? AND status IN ('queued','streaming')
             RETURNING run_uuid"
            text run-uuid])))

(defn update-assistant-run-metadata!
  "Merge provider metadata without changing status or response text.
   Pre: attrs contains only known provider fields. Post: known non-nil values
   replace prior values in every run state, including stopped."
  [run-uuid attrs]
  (let [allowed {:resolved-model :resolved_model
                 :provider-name :provider_name
                 :generation-id :generation_id
                 :input-tokens :input_tokens
                 :output-tokens :output_tokens
                 :reasoning-tokens :reasoning_tokens
                 :cached-tokens :cached_tokens
                 :cache-write-tokens :cache_write_tokens
                 :cost-usd :cost_usd
                 :provider-latency-ms :provider_latency_ms
                 :generation-ms :generation_ms
                 :first-token-ms :first_token_ms}
        fields (into {} (keep (fn [[k column]]
                                (when-some [v (get attrs k)] [column v]))) allowed)]
    (when (seq fields)
      (jdbc/execute! ds
        (sql/format {:update :assistant_runs :set fields
                     :where [:= :run_uuid run-uuid]})))))

(defn complete-assistant-answer-run!
  "Complete an active answer and copy its durable text to the transcript row.
   Post: true when this call won the terminal transition. False means Stop or
   another terminal transition won first. Late text never reaches the message."
  [run-uuid]
  (jdbc/with-transaction [tx ds]
    (if-let [run (jdbc/execute-one! tx
                    [(str "UPDATE assistant_runs SET status = 'completed', "
                       "completed_at = CURRENT_TIMESTAMP "
                       "WHERE run_uuid = ? AND status IN ('queued','streaming') "
                       "RETURNING assistant_message_id, response_text")
                     run-uuid]
                    {:builder-fn rs/as-unqualified-maps})]
      (do
        (jdbc/execute! tx ["UPDATE assistant_messages SET content = ? WHERE id = ?"
                           (:response_text run) (:assistant_message_id run)])
        true)
      false)))

(defn complete-assistant-compact-run!
  "Complete an active compaction run and publish its checkpoint atomically.
   Pre: the covered ids belong to the run's chat. Post: true when one checkpoint
   was inserted, false when the run was already terminal."
  [run-uuid summary covered-from-message-id covered-through-message-id]
  (jdbc/with-transaction [tx ds]
    (if-let [run (jdbc/execute-one! tx
                    [(str "UPDATE assistant_runs SET status = 'completed', response_text = ?, "
                       "completed_at = CURRENT_TIMESTAMP "
                       "WHERE run_uuid = ? AND status IN ('queued','streaming') "
                       "RETURNING id, chat_id")
                     summary run-uuid]
                    {:builder-fn rs/as-unqualified-maps})]
      (do
        (jdbc/execute! tx
          (sql/format {:insert-into :assistant_checkpoints
                       :values [{:chat_id (:chat_id run) :run_id (:id run)
                                 :summary summary
                                 :covered_from_message_id covered-from-message-id
                                 :covered_through_message_id covered-through-message-id}]}))
        true)
      false)))

(defn fail-assistant-run!
  "Fail an active run and preserve any partial answer in its transcript row.
   Post: true when this call won the terminal transition. A stopped run remains
   stopped and receives no late error text."
  [run-uuid error-text]
  (jdbc/with-transaction [tx ds]
    (if-let [run (jdbc/execute-one! tx
                    [(str "UPDATE assistant_runs SET status = 'failed', error_text = ?, "
                       "failed_at = CURRENT_TIMESTAMP "
                       "WHERE run_uuid = ? AND status IN ('queued','streaming','stop-requested') "
                       "RETURNING assistant_message_id, response_text")
                     error-text run-uuid]
                    {:builder-fn rs/as-unqualified-maps})]
      (do
        (when-let [message-id (:assistant_message_id run)]
          (jdbc/execute! tx ["UPDATE assistant_messages SET content = ? WHERE id = ?"
                             (:response_text run) message-id]))
        true)
      false)))

(defn stop-active-run-in!
  "Stop the active run in an owned chat at the last durable text boundary, on
   the caller's connectable, so a caller can stop and then act in one
   transaction.
   Post: {:stopped true :run-uuid} when a transition occurred, else
   {:stopped false}. The assistant transcript receives exactly response_text.
   Every later provider write checks for an active status, so no text reaches
   the message after this commits."
  [tx user-id chat-id]
  (if-let [run (jdbc/execute-one! tx
                  [(str "UPDATE assistant_runs r SET status = 'stopped', "
                     "stopped_at = CURRENT_TIMESTAMP "
                     "FROM assistant_chats c WHERE r.chat_id = c.id "
                     "AND c.user_id = ? AND c.id = ? "
                     "AND r.status IN ('queued','streaming','stop-requested') "
                     "RETURNING r.run_uuid, r.assistant_message_id, r.response_text")
                   user-id chat-id]
                  {:builder-fn rs/as-unqualified-maps})]
    (do
      (when-let [message-id (:assistant_message_id run)]
        (jdbc/execute! tx ["UPDATE assistant_messages SET content = ? WHERE id = ?"
                           (:response_text run) message-id]))
      {:stopped true :run-uuid (:run_uuid run)})
    {:stopped false}))

(defn stop-assistant-run!
  "`stop-active-run-in!` in a transaction of its own."
  [user-id chat-id]
  (jdbc/with-transaction [tx ds]
    (stop-active-run-in! tx user-id chat-id)))

(defn get-active-assistant-run
  "The active run for an owned chat, or nil."
  [user-id chat-id]
  (jdbc/execute-one! ds
    [(str "SELECT r.* FROM assistant_runs r JOIN assistant_chats c ON c.id = r.chat_id "
       "WHERE c.user_id = ? AND c.id = ? "
       "AND r.status IN ('queued','streaming','stop-requested') LIMIT 1")
     user-id chat-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-assistant-run
  "One run with parsed snapshots IFF its chat belongs to user-id."
  [user-id run-uuid]
  (some-> (jdbc/execute-one! ds
            [(str "SELECT r.* FROM assistant_runs r JOIN assistant_chats c ON c.id = r.chat_id "
               "WHERE c.user_id = ? AND r.run_uuid = ?")
             user-id run-uuid]
            {:builder-fn rs/as-unqualified-maps})
    (update :request_snapshot pgobject->clj)
    (update :source_snapshot pgobject->clj)))

(defn get-assistant-run-evidence
  "Evidence for an owned run, ordered by stored citation key."
  [user-id run-uuid]
  (jdbc/execute! ds
    [(str "SELECT e.* FROM assistant_evidence e "
       "JOIN assistant_runs r ON r.id = e.run_id "
       "JOIN assistant_chats c ON c.id = r.chat_id "
       "WHERE c.user_id = ? AND r.run_uuid = ? ORDER BY e.id")
     user-id run-uuid]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-cited-assistant-evidence-for-chat
  "The retained evidence of an owned chat that its answers cite, ordered by run
   and insertion. A row is cited when its key is in an evidence token of its
   run's response text, such as `[evidence:E1]` or `[evidence:E1, E2]`. These
   are the forms `assistant/evidence-token-pattern` matches. A `strpos` for
   `[evidence:<key>]` missed every key of a grouped token, so those citations
   rendered as raw text.

   A run retains every block it showed the model, and for a repo file that is
   the whole file: about 3000 rows and 700 KB for a 240 KB Markdown file. An
   answer links a handful. The transcript re-reads this on every stream update,
   so reading every row cost a full chat's evidence per update. A filtered row's
   quote columns are never read."
  [user-id chat-id]
  (jdbc/execute! ds
    [(str "SELECT e.*, r.run_uuid FROM assistant_evidence e "
       "JOIN assistant_runs r ON r.id = e.run_id "
       "JOIN assistant_chats c ON c.id = r.chat_id "
       "WHERE c.user_id = ? AND c.id = ? "
       "AND strpos(r.response_text, e.citation_key) > 0 "
       "AND r.response_text ~ (? || e.citation_key || ?) "
       "ORDER BY r.id, e.id")
     user-id chat-id
     "\\[evidence:[^]]*(?<![A-Za-z0-9_-])"
     "(?![A-Za-z0-9_-])[^]]*\\]"]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-assistant-runs-for-chat
  "Runs for an owned chat, oldest first, with parsed snapshots."
  [user-id chat-id]
  (mapv #(-> %
           (update :request_snapshot pgobject->clj)
           (update :source_snapshot pgobject->clj))
    (jdbc/execute! ds
      [(str "SELECT r.* FROM assistant_runs r JOIN assistant_chats c ON c.id = r.chat_id "
         "WHERE c.user_id = ? AND c.id = ? ORDER BY r.id")
       user-id chat-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn get-assistant-run-summaries-for-chat
  "Run fields needed by the transcript. Large snapshots stay server-side until
   the owner opens the details modal."
  [user-id chat-id]
  (jdbc/execute! ds
    [(str "SELECT r.id, r.run_uuid, r.chat_id, r.kind, r.status, "
       "r.user_message_id, r.assistant_message_id, r.retry_of_run_uuid, "
       "r.response_text, r.error_text, r.created_at, r.started_at, "
       "r.first_token_at, r.stopped_at, r.completed_at, r.failed_at "
       "FROM assistant_runs r JOIN assistant_chats c ON c.id = r.chat_id "
       "WHERE c.user_id = ? AND c.id = ? ORDER BY r.id")
     user-id chat-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-assistant-checkpoints
  "Completed checkpoints for an owned chat, oldest first."
  [user-id chat-id]
  (jdbc/execute! ds
    [(str "SELECT cp.*, r.run_uuid FROM assistant_checkpoints cp "
       "JOIN assistant_runs r ON r.id = cp.run_id "
       "JOIN assistant_chats c ON c.id = cp.chat_id "
       "WHERE c.user_id = ? AND c.id = ? ORDER BY cp.id")
     user-id chat-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn latest-assistant-checkpoint
  "The newest completed checkpoint for an owned chat, or nil."
  [user-id chat-id]
  (jdbc/execute-one! ds
    [(str "SELECT cp.*, r.run_uuid FROM assistant_checkpoints cp "
       "JOIN assistant_runs r ON r.id = cp.run_id "
       "JOIN assistant_chats c ON c.id = cp.chat_id "
       "WHERE c.user_id = ? AND c.id = ? ORDER BY cp.id DESC LIMIT 1")
     user-id chat-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- update-live-chat!
  "Apply `changes` to one live chat IFF owned by `user-id`.
   Post: true when a row changed. A foreign or staged chat changes nothing."
  [user-id chat-id changes]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            (sql/format {:update :assistant_chats
                         :set changes
                         :where [:and [:= :id chat-id] [:= :user_id user-id]
                                 [:is :staged_delete_id nil]]})))))

(defn set-assistant-chat-title!
  "Rename a live owned chat. Pre: `title` is trimmed, non-blank and capped by
   the caller (freememo.assistant/rename-chat!). Post: true when renamed. Does
   not touch updated_at: a rename is not activity in the chat."
  [user-id chat-id title]
  (update-live-chat! user-id chat-id {:title title}))

(defn set-assistant-chat-pinned!
  "Pin a live owned chat (pinned_at = now) or unpin it (NULL).
   Post: true when the row changed. updated_at is untouched, so pinning does not
   move a chat within its date group."
  [user-id chat-id pinned?]
  (update-live-chat! user-id chat-id
    {:pinned_at (if pinned? [:raw "CURRENT_TIMESTAMP"] nil)}))

(defn mark-assistant-chat-opened!
  "Record that the user opened a live owned chat. Post: true when recorded."
  [user-id chat-id]
  (update-live-chat! user-id chat-id {:last_opened_at [:raw "CURRENT_TIMESTAMP"]}))

(defn delete-empty-assistant-chat!
  "Delete an owned chat IFF it holds no messages.
   The cleanup for a first send that failed after creating its chat
   (freememo.assistant/ensure-and-start-answer-run!). The message test makes it
   safe against a chat that did receive a turn. Post: true when deleted."
  [user-id chat-id]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["DELETE FROM assistant_chats c
               WHERE c.id = ? AND c.user_id = ?
                 AND NOT EXISTS (SELECT 1 FROM assistant_messages m WHERE m.chat_id = c.id)"
             chat-id user-id]))))

;; Defined with the undo log further down.
(declare insert-undo-entry-raw! prune-undo-log!)

(defn stage-assistant-chat-for-deletion!
  "Hide a live owned chat for the 12-hour undo window, stopping its active run.

   Pre: none — a foreign, missing or already staged chat returns nil.
   Post: {:entry-id :title :stopped?}. In one transaction: the chat row is
   locked, its active run (if any) is stopped, one delete-assistant-chat undo
   entry is written and the chat's marker points at it, and the log is pruned.
   Invariant: the chat lock is the same lock run creation takes, so no run can
   start between the stop and the stage."
  [user-id chat-id]
  (jdbc/with-transaction [tx ds]
    (when-let [chat (jdbc/execute-one! tx
                      ["SELECT title FROM assistant_chats
                         WHERE id = ? AND user_id = ? AND staged_delete_id IS NULL
                         FOR UPDATE"
                       chat-id user-id]
                      {:builder-fn rs/as-unqualified-maps})]
      (let [stopped (stop-active-run-in! tx user-id chat-id)
            entry-id (insert-undo-entry-raw! tx user-id "delete-assistant-chat"
                       "assistant-chat" [chat-id] {:title (:title chat)})]
        (jdbc/execute! tx ["UPDATE assistant_chats SET staged_delete_id = ? WHERE id = ?"
                           entry-id chat-id])
        (prune-undo-log! tx user-id)
        {:entry-id entry-id :title (:title chat) :stopped? (:stopped stopped)}))))

(defn restore-staged-assistant-chat!
  "Clear the marker that `entry` set on its chat.

   Pre: entry is a live delete-assistant-chat entry for user-id.
   Post: the chat, its messages and its runs are visible again, unchanged. A
   stopped run stays stopped.
   Throws when no row matches: the chat's document was purged and the cascade
   removed the chat. undo-entry! catches that, reports failure and leaves the
   entry live. Clearing only the matching marker means an older entry can never
   undo a newer deletion."
  [user-id entry]
  (let [chat-id (first (:entity_refs entry))
        row (jdbc/execute-one! ds
              ["UPDATE assistant_chats SET staged_delete_id = NULL
                 WHERE id = ? AND user_id = ? AND staged_delete_id = ?
                 RETURNING id"
               chat-id user-id (:id entry)]
              {:builder-fn rs/as-unqualified-maps})]
    (or row
      (throw (ex-info "Assistant chat not found"
               {:type ::assistant-chat-not-found :chat-id chat-id})))))

(defn set-assistant-chat-mode!
  "Switch a chat's persona IFF owned by `user-id`.
   Pre:  `mode` is an assistant-modes :id — validated by the caller
         (freememo.assistant/set-chat-mode!).
   Post: the row's mode = `mode` when owned; nothing changes otherwise. Does NOT
   touch updated_at — switching a persona is not activity in the chat, and the
   picker orders on that column."
  [user-id chat-id mode]
  (jdbc/execute! ds
    (sql/format {:update :assistant_chats
                 :set {:mode mode}
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn set-assistant-chat-web-search!
  "Set the web-search preference for an owned chat.
   Pre: `web-search?` is boolean. Post: only the owned chat changes. This does
   not touch updated_at because changing a control is not chat activity."
  [user-id chat-id web-search?]
  (jdbc/execute! ds
    (sql/format {:update :assistant_chats
                 :set {:web_search (boolean web-search?)}
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn delete-assistant-chat!
  "Delete a chat (cascades to messages) IFF owned by `user-id`."
  [user-id chat-id]
  (jdbc/execute! ds
    (sql/format {:delete-from :assistant_chats
                 :where [:and [:= :id chat-id] [:= :user_id user-id]]})))

(defn list-email-update-recipients
  "Email addresses of users who opted into email updates (settings key
   'email_updates' = 'true') and have a non-blank email.
   Post: vector of distinct email strings (possibly empty)."
  []
  (->> (jdbc/execute! ds
         (sql/format {:select-distinct [:users.email]
                      :from [:users]
                      :join [:settings [:= :settings.user_id :users.id]]
                      :where [:and
                              [:= :settings.key "email_updates"]
                              [:= :settings.value "true"]
                              [:<> :users.email nil]
                              [:<> [:trim :users.email] ""]]}))
    (map :users/email)
    (remove str/blank?)
    vec))

;; ---------------------------------------------------------------------------
;; Credits — pass-through AI billing (see plans/credits-wayl-payment-system.md §5)
;; ---------------------------------------------------------------------------

(defn get-credit-balance
  "Current IQD credit balance for a user. 0 for unknown users.
   `connectable` defaults to the pool; pass a tx for transactional reads."
  ([user-id] (get-credit-balance ds user-id))
  ([connectable user-id]
   (or (:users/credit_balance_iqd
         (jdbc/execute-one! connectable
           ["SELECT credit_balance_iqd FROM users WHERE id = ?" user-id]))
     0)))

(defn list-credit-transactions
  "User's credit ledger newest-first, for the Settings → Usage 'AI costs' view.
   Optional filters (nil/blank = no constraint): `kind`/`endpoint`/`model`
   exact-match; `since-days` keeps rows newer than N days; `min-amount` keeps
   rows whose magnitude |amount_iqd| >= min-amount (e.g. 500 finds 500+ debits
   or credits); `q` case-insensitive substring over endpoint OR model. Selects
   only display columns (no reasoning_tokens/attempts/order ref — out of scope).
   Pre:  user-id identifies a user; since-days/min-amount nil or non-negative.
   Post: ≤ 5000 unqualified-key maps ordered created_at DESC, id DESC (stable
         window tiebreak for same-timestamp rows). Over-cap ledgers are
         truncated with a :warn log.
   Note: the since-days boundary uses the app JVM clock, which is colocated with
         Postgres in this deployment; a cross-tz split would shift it by hours."
  [user-id {:keys [kind endpoint model since-days min-amount q]}]
  (let [cap 5000
        like (when-not (str/blank? q) (str "%" (str/trim q) "%"))
        cutoff (when since-days
                 (java.sql.Timestamp/valueOf
                   (.minusDays (java.time.LocalDateTime/now) (long since-days))))
        where (cond-> [:and [:= :user_id user-id]]
                (not (str/blank? kind))     (conj [:= :kind kind])
                (not (str/blank? endpoint)) (conj [:= :endpoint endpoint])
                (not (str/blank? model))    (conj [:= :model model])
                cutoff                      (conj [:>= :created_at cutoff])
                (and min-amount (pos? min-amount)) (conj [:>= [:abs :amount_iqd] min-amount])
                like                        (conj [:or [:ilike :endpoint like]
                                                   [:ilike :model like]]))
        rows (jdbc/execute! ds
               (sql/format {:select [:id :kind :amount_iqd :balance_after
                                     :endpoint :model :input_tokens :cached_tokens
                                     :output_tokens :created_at]
                            :from [:credit_transactions]
                            :where where
                            :order-by [[:created_at :desc] [:id :desc]]
                            :limit (inc cap)})
               {:builder-fn rs/as-unqualified-maps})]
    (when (> (count rows) cap)
      (tel/log! {:level :warn :id ::credit-history-truncated
                 :data {:user-id user-id :cap cap}}
        "Credit history exceeded display cap; truncated"))
    (vec (take cap rows))))

(defn credit-transaction-facets
  "Distinct non-empty endpoints and models across a user's whole ledger — the
   'AI costs' filter dropdown options. Filter-independent by design so options
   never vanish as the user narrows.
   Pre:  user-id. Post: {:endpoints [str…] :models [str…]}, each sorted asc,
         no nils/blanks."
  [user-id]
  (let [distinct-col (fn [col]
                       (->> (jdbc/execute! ds
                              (sql/format {:select-distinct [col]
                                           :from [:credit_transactions]
                                           :where [:and [:= :user_id user-id]
                                                   [:<> col nil] [:<> col ""]]
                                           :order-by [[col :asc]]})
                              {:builder-fn rs/as-unqualified-maps})
                         (map col)
                         (remove str/blank?)
                         vec))]
    {:endpoints (distinct-col :endpoint)
     :models    (distinct-col :model)}))

(defn get-user-markup
  "Per-user markup override, or nil when unset (caller falls back to config).
   Returns a BigDecimal (Postgres DECIMAL)."
  [user-id]
  (:users/markup_override
    (jdbc/execute-one! ds
      ["SELECT markup_override FROM users WHERE id = ?" user-id])))

(defn set-user-markup!
  "Set or clear a user's markup override (REPL/SQL admin path — no UI yet).
   Pass nil to clear and fall back to the config default."
  [user-id markup]
  (jdbc/execute! ds
    ["UPDATE users SET markup_override = ? WHERE id = ?" markup user-id]))

(defn- sum-tokens
  "Sum of `k` over the token maps that report it, or nil when none does."
  [maps k]
  (when-some [vs (seq (keep k maps))]
    (reduce + vs)))

(defn debit-token-columns
  "The token columns of one debit ledger row.
   Pre:  attempts is a seq of token maps keyed as openrouter/token-keys, one
         per billed completion, or nil when the caller knows none.
   Post: {:input_tokens :cached_tokens :output_tokens :reasoning_tokens
          :attempts}. A count no attempt reported is nil (NULL), never 0 —
         zero is a real count. :attempts is nil when attempts is empty."
  [attempts]
  {:input_tokens (sum-tokens attempts :input-tokens)
   :cached_tokens (sum-tokens attempts :cached-tokens)
   :output_tokens (sum-tokens attempts :output-tokens)
   :reasoning_tokens (sum-tokens attempts :reasoning-tokens)
   :attempts (when (seq attempts) (count attempts))})

(defn debit-credits!
  "Atomic debit for a completed AI action. Locks the balance row, subtracts
   `cost-iqd`, appends a 'debit' ledger row with summed token detail.
   Pre:  cost-iqd >= 0; attempts is a seq of token maps (see
         debit-token-columns), or nil.
   Post: balance decreased by cost-iqd (may go negative — the gate allows one
         overshoot); ledger row written. Returns the new balance.
   Invariant: between SELECT FOR UPDATE and UPDATE no other tx touches the row."
  [user-id cost-iqd {:keys [endpoint model attempts]}]
  (jdbc/with-transaction [tx ds]
    (let [locked (jdbc/execute-one! tx
                   ["SELECT credit_balance_iqd FROM users WHERE id = ? FOR UPDATE" user-id])
          bal (or (:users/credit_balance_iqd locked) 0)
          new-bal (- bal cost-iqd)]
      (jdbc/execute! tx
        ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
      (jdbc/execute! tx
        (sql/format {:insert-into :credit_transactions
                     :values [(merge {:user_id user-id
                                      :kind "debit"
                                      :amount_iqd (- cost-iqd)
                                      :balance_after new-bal
                                      :endpoint endpoint
                                      :model model}
                               (debit-token-columns attempts))]}))
      new-bal)))

(defn credit-account!
  "Add `amount-iqd` to a user's balance + append a credit ledger row.
   `kind` is \"purchase\" | \"grant\" | \"adjustment\". `connectable` MUST be a
   live transaction (the SELECT FOR UPDATE requires it) — webhook crediting
   reuses the order-completion tx; grants wrap their own.
   Pre: amount-iqd > 0. Post: balance increased; ledger row written.
   Returns the new balance."
  [connectable user-id amount-iqd kind {:keys [order-reference-id]}]
  (let [locked (jdbc/execute-one! connectable
                 ["SELECT credit_balance_iqd FROM users WHERE id = ? FOR UPDATE" user-id])
        bal (or (:users/credit_balance_iqd locked) 0)
        new-bal (+ bal amount-iqd)]
    (jdbc/execute! connectable
      ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
    (jdbc/execute! connectable
      (sql/format {:insert-into :credit_transactions
                   :values [{:user_id user-id :kind kind :amount_iqd amount-iqd
                             :balance_after new-bal :order_reference_id order-reference-id}]}))
    new-bal))

(defn insert-credit-order!
  "Create a pending order. Returns the row."
  [user-id reference-id amount-iqd]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :credit_orders
                 :values [{:user_id user-id :reference_id reference-id
                           :amount_iqd amount-iqd :status "pending"}]
                 :returning [:*]})))

(defn set-order-wayl-fields!
  "Record Wayl's code/link-id on a pending order after link creation."
  [reference-id wayl-code wayl-link-id]
  (jdbc/execute! ds
    ["UPDATE credit_orders SET wayl_code = ?, wayl_link_id = ? WHERE reference_id = ?"
     wayl-code wayl-link-id reference-id]))

(defn get-credit-order [reference-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*] :from [:credit_orders]
                 :where [:= :reference_id reference-id]})))

(defn complete-credit-order!
  "Idempotently mark an order complete and credit the buyer, in one tx. Locks
   the order row; a redelivered webhook (already complete) is a no-op.
   Pre:  reference-id from a verified webhook.
   Post: on first completion the order is 'complete' and balance += amount.
   Returns {:credited true :amount n :user-id u} or {:credited false :reason
            :already|:unknown}."
  [reference-id]
  (jdbc/with-transaction [tx ds]
    (let [order (jdbc/execute-one! tx
                  ["SELECT * FROM credit_orders WHERE reference_id = ? FOR UPDATE" reference-id]
                  {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? order) {:credited false :reason :unknown}
        (= "complete" (:status order)) {:credited false :reason :already}
        :else
        (let [user-id (:user_id order)
              amount (:amount_iqd order)]
          (jdbc/execute! tx
            ["UPDATE credit_orders SET status = 'complete', completed_at = CURRENT_TIMESTAMP
              WHERE reference_id = ?" reference-id])
          (credit-account! tx user-id amount "purchase" {:order-reference-id reference-id})
          ;; §4.2 2.4 — first completed order promotes the storage cap from the
          ;; free tier to the paid tier (decision R3: the tier caps how much may
          ;; be stored; the meter prices how long). Guarded on quota_bytes IS
          ;; NULL inside the fn, so repeat purchases and admin overrides are
          ;; both no-ops. Same tx as the credit, so a rollback undoes both.
          (promote-to-paid-storage-tier! tx user-id quota/paid-quota-bytes)
          {:credited true :amount amount :user-id user-id})))))

(defn fail-credit-order!
  "Mark a pending order failed (provider error / Cancelled / Rejected / Returned).
   No-op once the order is complete."
  [reference-id]
  (jdbc/execute! ds
    ["UPDATE credit_orders SET status = 'failed'
      WHERE reference_id = ? AND status = 'pending'" reference-id]))

(defn- user-has-grant? [connectable user-id]
  (some? (jdbc/execute-one! connectable
           ["SELECT 1 FROM credit_transactions WHERE user_id = ? AND kind = 'grant' LIMIT 1"
            user-id])))

(defn grant-credits!
  "Grant `amount-iqd` to a user (own tx). Returns the new balance."
  [user-id amount-iqd]
  (jdbc/with-transaction [tx ds]
    (credit-account! tx user-id amount-iqd "grant" {})))

(defn grant-signup-credits!
  "One-time signup grant (idempotent per user). No-op when credits are disabled,
   the configured amount is 0, or the user already holds a grant."
  [user-id]
  (let [amount (config/signup-grant)]
    (when (and (config/credits-enabled?) (pos? amount)
               (not (user-has-grant? ds user-id)))
      (grant-credits! user-id amount)
      (tel/log! {:level :info :id ::signup-grant :data {:user-id user-id :amount amount}}
        "Signup credit grant"))))

(defn run-once!
  "Run data migration `f` at most once per database, under the name `id`.
   `f` receives the transaction. The marker row and `f`'s writes commit together,
   so a failure leaves no marker and the next boot tries again. During a deploy
   both colors boot: the second one's INSERT waits on the first's key, then
   conflicts, and skips.
   Post: true when `f` ran now, nil when it had already run."
  [id f]
  (jdbc/with-transaction [tx ds]
    (when (jdbc/execute-one! tx
            ["INSERT INTO app_migrations (id) VALUES (?) ON CONFLICT DO NOTHING RETURNING id" id])
      (f tx)
      true)))

(defn move-everyone-to-best-level!
  "Set every AI task of every existing user to the Best level (product decision,
   2026-09-23). The rows a user chose before stay in the table, unread while the
   task is on a named level. Users who sign up later read Best as the default
   (ai-tiers/default-level). Run through run-once!, never directly.
   Post: every user has an ai_tier_<task> row holding \"best\" for each task."
  [tx]
  (let [ks (mapv ai-tiers/level-key ai-tiers/tasks)
        r (jdbc/execute-one! tx
            (into [(str "INSERT INTO settings (user_id, key, value)
                         SELECT u.id, k.key, ? FROM users u
                         CROSS JOIN (VALUES " (str/join ", " (repeat (count ks) "(?)")) ") AS k(key)
                         ON CONFLICT (user_id, key)
                         DO UPDATE SET value = EXCLUDED.value, updated_at = now()")
                   "best"]
              ks))]
    (tel/log! {:level :info :id ::moved-everyone-to-best-level
               :data {:rows (:next.jdbc/update-count r)}}
      "Moved every user's AI tasks to the Best level")))

(defn run-grandfather-migration!
  "One-time grant to every pre-existing user without a grant. Idempotent (the
   grant-row check excludes already-granted users). No-op when credits are
   disabled or the configured amount is 0. Called at boot from setup-schema."
  []
  (let [amount (config/grandfather-grant)]
    (when (and (config/credits-enabled?) (pos? amount))
      (let [users (jdbc/execute! ds
                    ["SELECT u.id FROM users u
                      WHERE NOT EXISTS (SELECT 1 FROM credit_transactions t
                                        WHERE t.user_id = u.id AND t.kind = 'grant')"]
                    {:builder-fn rs/as-unqualified-maps})]
        (when (seq users)
          (doseq [{:keys [id]} users]
            (grant-credits! id amount))
          (tel/log! {:level :info :id ::grandfather-grant
                     :data {:count (count users) :amount amount}}
            "Grandfather credit grant"))))))

;; ---------------------------------------------------------------------------
;; Google OAuth user queries (unchanged)
;; ---------------------------------------------------------------------------

(defn get-user-by-id [user-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username :google_id]
                 :from [:users]
                 :where [:= :id user-id]})))

(defn get-user-by-google-id [google-id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username :email]
                 :from [:users]
                 :where [:= :google_id google-id]})))

(defn get-user-by-username [username]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :username]
                 :from [:users]
                 :where [:= :username username]})))

(defn upsert-google-user [google-id email username]
  (jdbc/execute-one! ds
    (sql/format {:insert-into :users
                 :values [{:google_id google-id :email email :username username}]
                 :on-conflict [:google_id]
                 :do-update-set {:email email}
                 :returning [:id :username]})))

(defn set-display-name-if-absent!
  "Fill users.display_name for `user-id`, but only when it is currently NULL.
   Pre:  user-id identifies an existing users row; display-name is a string or nil.
   Post: a NULL display_name becomes the trimmed display-name; an existing
         non-NULL value is left untouched. No-op for nil/blank input.
   Invariant: a hand-curated display_name is never clobbered by a later login."
  [user-id display-name]
  (when-not (str/blank? display-name)
    (jdbc/execute! ds
      ["UPDATE users SET display_name = ? WHERE id = ? AND display_name IS NULL"
       (str/trim display-name) user-id])))

(defn get-mail-addressees
  "Rows needed to address outgoing mail to specific users.
   Pre:  user-ids is a non-empty collection of ints.
   Post: vector of {:users/id :users/email :users/display_name}, one per
         existing id, in ascending id order; ids with no row are absent, so
         callers MUST compare the count against what they asked for."
  [user-ids]
  (when (seq user-ids)
    (jdbc/execute! ds
      (sql/format {:select [:id :email :display_name]
                   :from [:users]
                   :where [:in :id (vec user-ids)]
                   :order-by [[:id :asc]]}))))

(defn user-ids-with-event
  "Set of user ids that already have at least one `event-type` row.
   Post: set of ints (possibly empty). Used for idempotent one-off sends."
  [event-type]
  (into #{}
    (map :user_events/user_id)
    (jdbc/execute! ds
      (sql/format {:select-distinct [:user_id]
                   :from [:user_events]
                   :where [:= :event_type event-type]}))))

(defn user-has-event?
  "Whether `user-id` already has at least one `event-type` row.
   Post: boolean. Advisory only — `user_events` has no unique constraint, so a
   concurrent check-then-insert pair can both see false."
  [user-id event-type]
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM user_events WHERE user_id = ? AND event_type = ? LIMIT 1"
            user-id event-type])))

(defn insert-user-event!
  "Record a user event. The 3-arity stores `metadata` as jsonb.
   Post: exactly one user_events row inserted."
  ([user-id event-type] (insert-user-event! user-id event-type nil))
  ([user-id event-type metadata]
   (jdbc/execute! ds
     (sql/format {:insert-into :user_events
                  :values [(cond-> {:user_id user-id :event_type event-type}
                             (some? metadata)
                             (assoc :metadata
                               [:cast (json/generate-string metadata) :jsonb]))]}))))

;; ---------------------------------------------------------------------------
;; Utility
;; ---------------------------------------------------------------------------

(defn sanitize-utf8
  "Re-encode string through UTF-8 to strip invalid byte sequences."
  [s]
  (when s
    (String. (.getBytes (str s) "UTF-8") "UTF-8")))

(defn backfill-content-text!
  "Populate topics.content_text for rows where it is NULL but content exists.
   Strips HTML via Jsoup. Batched to avoid loading all rows at once."
  []
  (let [batch-size 200]
    (loop [total 0]
      (let [rows (jdbc/execute! ds
                   ["SELECT id, content FROM topics
                      WHERE content IS NOT NULL AND content_text IS NULL
                      LIMIT ?" batch-size]
                   {:builder-fn rs/as-unqualified-maps})]
        (if (empty? rows)
          (when (pos? total)
            (tel/log! {:level :info :id ::backfill-content-text :data {:count total}}
              "Backfilled content_text"))
          (do
            (doseq [{:keys [id content]} rows]
              (jdbc/execute! ds
                ["UPDATE topics SET content_text = ? WHERE id = ?"
                 (text/strip-html content) id]))
            (recur (+ total (count rows)))))))))

;; ---------------------------------------------------------------------------
;; Topic CRUD
;; ---------------------------------------------------------------------------

(defn- audit-doc-created!
  "Audit a document (root/child topic) creation.
   Pre: user-id the owner (nil for owner-less system topics — skipped); topic-id
   the new row's id. Post: one :create audit signal (§3.2). Returns nil."
  [user-id topic-id]
  (when user-id
    (log/audit! {:id ::topic-created :user-id user-id :action :create
                 :entity :document :entity-id topic-id})))

(defn create-topic!
  "Create a topic. attrs is a map with keys:
   :user-id :kind :title :parent-id :content :page-number
   :source-id :status :priority :extract?
   :source-id attaches an existing `sources` row (bibliography).
   :extract? true marks a topic the user made with Extract (`is_extract`).
   Returns the created row with :topics/id."
  [attrs]
  (let [clean-title (input/prettify-title (input/sanitize-title (:title attrs)))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (when (:content attrs) (sanitize-utf8 (:content attrs)))
        row (cond-> {:kind (or (:kind attrs) "basic")
                     :title clean-title
                     :status (or (:status attrs) "active")
                     :priority (or (:priority attrs) priority/priority-default)}
              (:user-id attrs) (assoc :user_id (:user-id attrs))
              (:parent-id attrs) (assoc :parent_id (:parent-id attrs))
              sanitized (assoc :content sanitized
                          :content_text (text/strip-html sanitized))
              (:page-number attrs) (assoc :page_number (:page-number attrs))
              (:source-id attrs) (assoc :source_id (:source-id attrs))
              (:extract? attrs) (assoc :is_extract true))
        created (jdbc/execute-one! ds
                  (sql/format {:insert-into :topics
                               :values [row]
                               :returning [:*]}))]
    (audit-doc-created! (:user-id attrs) (:topics/id created))
    created))

(defn available-import-title
  "Return an unused root-document title for a user.
   Hidden topics reserve their titles until the undo window expires."
  ([user-id requested-title]
   (available-import-title ds user-id requested-title))
  ([conn user-id requested-title]
   (let [base (input/prettify-title (input/sanitize-title requested-title))
         escaped (-> base
                   (str/replace "\\" "\\\\")
                   (str/replace "%" "\\%")
                   (str/replace "_" "\\_"))
         rows (jdbc/execute! conn
                ["SELECT title FROM topics
                  WHERE user_id = ? AND parent_id IS NULL
                    AND lower(title) LIKE lower(?) ESCAPE '\\'"
                 user-id (str escaped "%")]
                {:builder-fn rs/as-unqualified-maps})
         pattern (re-pattern (str "(?i)^" (java.util.regex.Pattern/quote base)
                               "(?: \\(([0-9]+)\\))?$"))
         used (into #{}
                (keep (fn [{:keys [title]}]
                        (when-let [[_ n] (re-matches pattern title)]
                          (if n
                            (try (Long/parseLong n)
                                 (catch NumberFormatException _ nil))
                            1))))
                rows)]
     (if-not (contains? used 1)
       base
       (let [n (first (remove used (iterate inc 2)))
             suffix (str " (" n ")")
             room (max 0 (- input/title-max (count suffix)))]
         (str (subs base 0 (min room (count base))) suffix))))))

(defn get-topic
  "Get a topic by ID. Excludes topics staged for deletion (hidden)."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:is :staged_delete_id nil]]})))

(defn get-topic-priority
  "Current review priority for a topic (0=highest…100=lowest, three decimal
   places). Returns nil for a missing or staged-for-deletion topic; callers
   default to priority/priority-default."
  [id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:priority]
                         :from [:topics]
                         :where [:and [:= :id id] [:is :staged_delete_id nil]]}))
    :topics/priority))

(defn get-topic-titles
  "Map {topic-id title} for `ids`, INCLUDING staged-for-deletion topics so
   undo-log sources still resolve. Missing/hard-deleted ids are simply absent."
  [ids]
  (if (empty? ids)
    {}
    (into {}
      (map (juxt :id :title))
      (jdbc/execute! ds
        (sql/format {:select [:id :title] :from [:topics] :where [:in :id (vec ids)]})
        {:builder-fn rs/as-unqualified-maps}))))

(defn get-topic-for-user
  "Get a topic by ID, scoped to a user. Excludes topics staged for deletion."
  [user-id id]
  (jdbc/execute-one! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :id id] [:= :user_id user-id] [:is :staged_delete_id nil]]})))

(defn get-topic-title-for-user
  "The title of one topic the user owns, or nil.

   Pre: id is an integer.
   Post: nil for a missing id, another user's topic, and a topic staged for
   deletion — the scope `get-topic-for-user` checks, without the content
   columns its `SELECT *` fetches. Read on every rename by the tab title."
  [user-id id]
  (:topics/title
    (jdbc/execute-one! ds
      (sql/format {:select [:title]
                   :from [:topics]
                   :where [:and [:= :id id] [:= :user_id user-id] [:is :staged_delete_id nil]]}))))

(def ^:private topic-file-sizes
  "Derived table summing STORED BYTES per topic. Join THIS instead of
   topic_files directly in list queries: score topics own TWO file rows
   (PDF + audio), so a bare row join fans a topic out into duplicates.

   Unions in topic_videos so a video topic reports its real footprint. A video's
   topic_files row holds only the extracted MP3 (a few MB); the video itself is
   a large object, and without this term Library would show a 700 MB video as
   5 MB — disagreeing with the same user's storage bar.

   The cast is load-bearing: `topic_videos.byte_size` is BIGINT, and Postgres'
   SUM(bigint) is NUMERIC, which arrives as a BigDecimal. Every consumer of
   :file_size was written against the Long that SUM(integer) used to return."
  {:select [:topic_id [[:cast [:sum :file_size] :bigint] :file_size]]
   :from [[{:union-all [{:select [:topic_id :file_size] :from [:topic_files]}
                        {:select [:topic_id [:byte_size :file_size]] :from [:topic_videos]}]}
           :stored_bytes]]
   :group-by [:topic_id]})

(defn get-root-topics
  "Get all root topics for a user (parent_id IS NULL). Replaces get-documents.
   Includes file_size (summed across a topic's file rows) or content length,
   plus bibliography fields joined from `sources` (NULL when topic has no
   source_id)."
  [user-id]
  (let [topics (jdbc/execute! ds
                 (sql/format {:select [:t/id :t/title :t/kind
                                       :s/url :s/title :s/csl_type :s/container_title
                                       :t/status :t/priority :t/created_at :t/content
                                       [:tf/file_size :file_size]]
                              :from [[:topics :t]]
                              :left-join [[topic-file-sizes :tf] [:= :t/id :tf/topic_id]
                                          [:sources :s] [:= :t/source_id :s/id]]
                              :where [:and [:= :t/user_id user-id] [:= :t/parent_id nil]
                                      [:is :t/staged_delete_id nil]]
                              :order-by [[:t/created_at :desc]]}))]
    (mapv (fn [t]
            (let [size (or (:file_size t)
                         (when-let [c (:topics/content t)]
                           (count (.getBytes c "UTF-8"))))]
              (-> t
                (assoc :topics/file_size size)
                (dissoc :topics/content :file_size))))
      topics)))

(defn get-children
  "Get direct children of a topic, ordered by page_number then created_at."
  [parent-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :parent_id parent-id] [:is :staged_delete_id nil]]
                 :order-by [[:page_number :asc-nulls-last] [:created_at :asc]]})))

(defn stored-content
  "The stored form of editor HTML — the ONE filter the document-content path
   applies, so `update-topic-content!` and `save-merged-pages!` cannot drift.

   Strips `<span class=\"ql-token …\">` wrappers — Quill 2.0.3's
   `clipboard.convert` misreads them as `code-token: true`, corrupting render on
   reload. The browser's `syntax` module re-applies tokens on each load.

   `clean-html` deliberately does NOT run here. The document-content path has
   never sanitized, and adding it would newly strip formats the editor keeps."
  [content]
  (sanitize-utf8 (cleaner/strip-ql-tokens content)))

(defn update-topic-content!
  "Update the content of a topic. Filter: `stored-content`.

   Pre : callers do not call this for a kind `poem` topic. A poem is read-only
         material (`plans/poem-reader-spec.md` row 3.2) and mounts no editor,
         so every caller named in §6 of that spec addresses another kind.
   Post: the row's `content` and `content_text` hold the filtered HTML — EXCEPT
         for a poem, where nothing is written and nil is returned.

   The poem guard sits here rather than at the four call sites because this is
   the one function every content writer that can reach a poem goes through, and
   a guard per caller is four places for it to be forgotten. A poem's `content`
   is a rendered fallback derived from `poem_packages`; a write would replace it
   with editor output that the package no longer agrees with, and nothing reads
   `content` to notice.

   It refuses rather than throws: the precondition says this cannot happen, so
   a call that arrives is a defect to report, not a failure the caller has a
   branch for. The `:warn` names the topic so the defect is findable."
  [id content]
  (if (= "poem" (:topics/kind (jdbc/execute-one! ds
                                (sql/format {:select [:kind] :from :topics
                                             :where [:= :id id]}))))
    (do (tel/log! {:level :warn :id ::poem-content-write-refused :data {:topic-id id}}
          "Refused a content write to a poem topic")
        nil)
    (let [sanitized (stored-content content)]
      (jdbc/execute-one! ds
        (sql/format {:update :topics
                     :set {:content sanitized
                           :content_text (text/strip-html sanitized)}
                     :where [:= :id id]})))))

;; ---------------------------------------------------------------------------
;; Merged page reading (plans/merged-page-reading.md)
;; ---------------------------------------------------------------------------

(defn page-rows-for-merge
  "The page rows a merged reading surface is built from.

   Pre : `root-id` is a PDF root topic id.
   Post: `{:id :page_number :content}` per page, ascending, staged deletes
         excluded — the same exclusion `count-pages` applies, so the merged
         document and the toolbar's page count agree.

   Selects three columns rather than `*`: a merged 883-page document is
   megabytes, and every other column would ride along for nothing."
  [root-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :page_number :content]
                 :from [:topics]
                 :where [:and
                         [:= :parent_id root-id]
                         [:= :kind "page"]
                         [:is :staged_delete_id nil]
                         [:is-not :page_number nil]]
                 :order-by [[:page_number :asc]]})))

(defn merged-page-document
  "One document from `root-id`'s page rows, with a boundary marker per page.

   Pre : `root-id` is a PDF root topic id.
   Post: `{:html <merged> :page-numbers [ints]}`. The page-number list is what
         `save-merged-pages!` needs to split the document back, because the
         editor cannot report a page whose marker the reader deleted."
  [root-id]
  (let [rows (page-rows-for-merge root-id)]
    {:html (page-merge/merge-pages rows)
     :page-numbers (mapv :topics/page_number rows)}))

(defn save-merged-pages!
  "Split a merged document back into `root-id`'s page rows.

   Pre : `root-id` is a PDF root topic id. `merged` is HTML read out of the
         editor. `page-numbers` is the list the merge reported.
   Post: every page whose slice differs from its stored content is updated.
         Unchanged pages are not written. Returns
         `{:written <count> :pages [page-numbers]}`.
   Invariant: no row is inserted and no row is deleted. Only `content` and
         `content_text` change — `status`, `page_number`, `priority` and every
         scheduling column are untouched, so Done marks, the \"N / M pages done\"
         figure and a schedule's progress survive a merged save.

   ONE UPDATE PER CHANGED PAGE, NOT PER PAGE. The editor's auto-save fires on
   every edit whose HTML differs, and the largest real PDF in this collection
   holds 883 pages — an unconditional split would be 883 statements per
   keystroke.

   The read and the writes share one transaction, so a failure part-way leaves
   no page row written."
  [root-id merged page-numbers]
  (jdbc/with-transaction [tx ds]
    (let [rows (jdbc/execute! tx
                 (sql/format {:select [:id :page_number :content]
                              :from [:topics]
                              :where [:and
                                      [:= :parent_id root-id]
                                      [:= :kind "page"]
                                      [:is :staged_delete_id nil]
                                      [:is-not :page_number nil]]}))
          by-page (into {} (map (juxt :topics/page_number identity)) rows)
          ;; The caller's list is authoritative for WHICH pages to split into.
          ;; Intersecting it with the live rows keeps a page deleted in another
          ;; tab out of the write set.
          wanted (filterv #(contains? by-page %) (remove nil? page-numbers))
          slices (page-merge/split-merged merged wanted)
          changed (for [[page slice] slices
                        :let [row (get by-page page)
                              sanitized (stored-content slice)]
                        :when (not= sanitized (or (:topics/content row) ""))]
                    [page (:topics/id row) sanitized])]
      (doseq [[_ id sanitized] changed]
        (jdbc/execute-one! tx
          (sql/format {:update :topics
                       :set {:content sanitized
                             :content_text (text/strip-html sanitized)}
                       :where [:= :id id]})))
      {:written (count changed)
       :pages (mapv first changed)})))

(defn rename-topic!
  "Rename a topic. Sanitizes and length-checks the new title.
   Throws ::blank-title when input is blank.
   Bumps updated_at on flashcards whose root_topic_id matches so Anki push
   detects the title change. For non-root ids the bump is a no-op."
  [id new-title]
  (when (or (nil? new-title) (str/blank? new-title))
    (throw (ex-info "title must not be blank"
             {:type ::blank-title :id id})))
  (let [clean-title (input/sanitize-title new-title)]
    (input/check-length! :title clean-title input/title-max)
    (jdbc/execute-one! ds
      (sql/format {:update :topics
                   :set {:title clean-title}
                   :where [:= :id id]}))
    (jdbc/execute-one! ds
      ["UPDATE flashcards SET updated_at = CURRENT_TIMESTAMP WHERE root_topic_id = ?" id])))

(defn get-root-topic-id
  "Traverse parent_id upward to find the root topic (parent_id IS NULL).
   Returns the root topic's id, or the input id if it is already a root."
  [topic-id]
  (when topic-id
    (:id
     (jdbc/execute-one! ds
       ["WITH RECURSIVE ancestors AS (
           SELECT id, parent_id FROM topics WHERE id = ?
           UNION ALL
           SELECT t.id, t.parent_id FROM topics t
           JOIN ancestors a ON t.id = a.parent_id
         )
         SELECT id FROM ancestors WHERE parent_id IS NULL"
        topic-id]))))

(defn- bump-user-usage!
  "Adjust users.usage_bytes by delta (signed). Clamped at 0.
   Pass tx for transactional callers, ds otherwise."
  [connectable user-id delta]
  (jdbc/execute! connectable
    ["UPDATE users SET usage_bytes = GREATEST(0, usage_bytes + ?) WHERE id = ?"
     (long delta) user-id]))

(defn- subtree-file-bytes
  "Sum of stored bytes across the topic subtree rooted at id (including id).
   Returns 0 when nothing is found.

   Counts BOTH storage tables, matching the `usage_bytes` definition
   (plans/incremental-video.md §4.2 2.1): `topic_files.file_size` plus
   `topic_videos.byte_size`. This function is the deletion decrement, so
   omitting the video term would free the wrong amount and leave the counter
   permanently high by the size of every deleted video."
  [connectable id]
  (or (:sum (jdbc/execute-one! connectable
              ["WITH RECURSIVE subtree AS (
                  SELECT id FROM topics WHERE id = ?
                  UNION ALL
                  SELECT c.id FROM topics c
                  JOIN subtree s ON c.parent_id = s.id
                )
                SELECT CAST(
                  COALESCE((SELECT SUM(tf.file_size) FROM topic_files tf
                             JOIN subtree s ON tf.topic_id = s.id), 0)
                + COALESCE((SELECT SUM(tv.byte_size) FROM topic_videos tv
                             JOIN subtree s ON tv.topic_id = s.id), 0)
                + COALESCE((SELECT SUM(te.byte_size) FROM topic_epubs te
                             JOIN subtree s ON te.topic_id = s.id), 0)
                  AS bigint) AS sum"
               id]))
    0))

(defn- subtree-large-object-oids
  "Large-object OIDs owned by the topic subtree rooted at id (including id).
   The list a hard-delete path must `lo_unlink` — Postgres will not do it when
   the owning row cascades away.

   Named for large objects rather than for video: two tables hold them now, and
   a name that says `video` is how the epub half would be forgotten. Every table
   with an `lo_oid` MUST have a branch here, or its bytes survive the topic
   forever with nothing pointing at them."
  [connectable id]
  (mapv :lo_oid
    (jdbc/execute! connectable
      ["WITH RECURSIVE subtree AS (
          SELECT id FROM topics WHERE id = ?
          UNION ALL
          SELECT c.id FROM topics c JOIN subtree s ON c.parent_id = s.id
        )
        SELECT tv.lo_oid FROM topic_videos tv JOIN subtree s ON tv.topic_id = s.id
        UNION ALL
        SELECT te.lo_oid FROM topic_epubs te JOIN subtree s ON te.topic_id = s.id"
       id]
      {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; Anki note tombstones — see the table comment in setup-schema-core!
;; ---------------------------------------------------------------------------

(defn record-anki-tombstones!
  "Record that `deleted-rows`' Anki notes have lost their local cards.

   pre : `conn` is a connectable, and MUST be the same transaction that removed
         the rows — a tombstone committed without its deletion would delete a
         live note. `deleted-rows` are flashcards rows (namespaced or plain
         keys); rows with no `anki_note_id` are skipped, since an unpushed card
         owns no note.
   post: one row per distinct synced note id, carrying the card's
         `anki_baseline_digest`. Idempotent — a repeat is a no-op.

   EVERY path that hard-deletes a synced card owes this call. No database trigger
   does it instead: a `BEFORE DELETE` trigger on `flashcards` cannot read
   `topics.user_id` when the delete arrives as a cascade from a deleted topic,
   because the parent row is already gone by then. The test
   `t30-every-flashcard-delete-path-tombstones` is what keeps the list honest."
  [conn user-id deleted-rows]
  (let [rows (into []
               (comp
                 (keep (fn [r]
                         (when-let [nid (or (:flashcards/anki_note_id r) (:anki_note_id r))]
                           {:user_id user-id
                            :anki_note_id nid
                            :anki_baseline_digest (or (:flashcards/anki_baseline_digest r)
                                                    (:anki_baseline_digest r))})))
                 (distinct))
               deleted-rows)]
    (when (seq rows)
      (jdbc/execute! conn
        (sql/format {:insert-into :anki_note_tombstones
                     :values rows
                     :on-conflict [:user_id :anki_note_id]
                     :do-nothing true})))
    (count rows)))

(defn subtree-synced-cards
  "The synced flashcards under the topic subtree rooted at `topic-id`.

   Read BEFORE a cascading delete, for the reason `subtree-large-object-oids`
   is: afterwards there is nothing left to read. Returns rows carrying
   `anki_note_id` and `anki_baseline_digest`."
  [conn topic-id]
  (jdbc/execute! conn
    ["WITH RECURSIVE subtree AS (
        SELECT id FROM topics WHERE id = ?
        UNION ALL
        SELECT c.id FROM topics c JOIN subtree s ON c.parent_id = s.id
      )
      SELECT DISTINCT f.anki_note_id, f.anki_baseline_digest
      FROM flashcards f
      WHERE f.anki_note_id IS NOT NULL
        AND (f.root_topic_id IN (SELECT id FROM subtree)
             OR f.topic_id IN (SELECT id FROM subtree))"
     topic-id]))

(defn- delete-repo-clones!
  "Remove the git clones of the repository nodes `repo-ids`
   (plans/commit-range-walkthrough.md §6.1.5). Runs AFTER the transaction that
   deleted the rows commits, so a rollback keeps the clone its row still
   needs. Resolved at call time: `freememo.git-clone` is a filesystem concern
   this namespace does not otherwise load."
  [repo-ids]
  (when (seq repo-ids)
    (let [delete! (requiring-resolve 'freememo.git-clone/delete-clone!)]
      (doseq [id repo-ids] (delete! id)))))

(defn delete-topic-for-user!
  "Delete a topic scoped to user. Decrements usage_bytes by the subtree's
   total stored bytes, unlinks its large objects, and tombstones the Anki notes
   of every synced card it destroys — all atomically. A deleted repository
   node's git clone is removed once that transaction commits.

   §4.7 7.2: this is the hard-delete path (the staged path is
   stage-topic-for-deletion! + purge-staged-documents!). Unlinking here is safe
   precisely because the row disappears in the same transaction — there is no
   undo window to preserve."
  [user-id id]
  (let [[result repo-ids]
        (jdbc/with-transaction [tx ds]
    (let [repo-ids (mapv :id (jdbc/execute! tx
                               ["SELECT id FROM topics WHERE id = ? AND user_id = ? AND kind = 'repo'"
                                id user-id]
                               {:builder-fn rs/as-unqualified-maps}))
          freed (subtree-file-bytes tx id)
          oids (subtree-large-object-oids tx id)
          ;; Both reads happen BEFORE the delete — the cards cascade away with
          ;; the topics.
          synced-cards (subtree-synced-cards tx id)
          result (jdbc/execute! tx
                   (sql/format {:delete-from :topics
                                :where [:and [:= :id id] [:= :user_id user-id]]}))]
      (record-anki-tombstones! tx user-id synced-cards)
      (doseq [oid oids] (lo/unlink! tx oid))
      (when (pos? freed)
        (bump-user-usage! tx user-id (- freed)))
      (log/audit! {:id ::topic-deleted :user-id user-id :action :delete
                   :entity :document :entity-id id})
      [result repo-ids]))]
    (delete-repo-clones! repo-ids)
    result))

;; ---------------------------------------------------------------------------
;; File operations
;; ---------------------------------------------------------------------------

(defn get-topic-file
  "Get the binary file data for a topic (PDF/EPUB/audio). 1-arity reads the
   'main' blob — the only one non-score kinds have. Score topics carry a
   second 'audio' role."
  ([topic-id] (get-topic-file topic-id "main"))
  ([topic-id role]
   (jdbc/execute-one! ds
     (sql/format {:select [:file_data :file_size :mime_type]
                  :from [:topic_files]
                  :where [:and [:= :topic_id topic-id] [:= :role role]]}))))

(defn topic-file-exists?
  "True iff a topic has a stored binary file blob for the role ('main' default)."
  ([topic-id] (topic-file-exists? topic-id "main"))
  ([topic-id role]
   (some? (jdbc/execute-one! ds
            (sql/format {:select [[[:inline 1] :x]]
                         :from [:topic_files]
                         :where [:and [:= :topic_id topic-id] [:= :role role]]})))))

(defn count-pages
  "Number of kind='page' child topics under `parent-id` (excludes staged
   deletes). Drives the live-doc viewer's reload trigger."
  [parent-id]
  (or (:c (jdbc/execute-one! ds
            (sql/format {:select [[[:count :*] :c]]
                         :from [:topics]
                         :where [:and [:= :parent_id parent-id] [:= :kind "page"]
                                 [:is :staged_delete_id nil]]})
            {:builder-fn rs/as-unqualified-maps}))
    0))

(defn save-topic-file!
  "Store binary file for a topic. Upserts."
  [topic-id file-bytes file-size mime-type]
  (jdbc/execute! ds
    ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
      VALUES (?, ?, ?, ?)
      ON CONFLICT (topic_id)
      DO UPDATE SET file_data = excluded.file_data, file_size = excluded.file_size, mime_type = excluded.mime_type"
     topic-id file-bytes file-size mime-type]))

;; ---------------------------------------------------------------------------
;; Sources — bibliography records (CSL-JSON shaped)
;; ---------------------------------------------------------------------------

(defn- ->jsonb
  "Wrap a Clojure value as a PostgreSQL JSONB parameter."
  [v]
  (doto (PGobject.)
    (.setType "jsonb")
    (.setValue (json/generate-string (or v {})))))

(defn- pgobject->clj
  "Parse a PGobject (jsonb) back to a Clojure value. Keywordizes keys.
   nil → nil; non-PGobject inputs pass through."
  [v]
  (cond
    (nil? v) nil
    (instance? PGobject v) (json/parse-string (.getValue ^PGobject v) true)
    :else v))

;; ---------------------------------------------------------------------------
;; Reading positions — plans/reading-position-memory.md §4.1
;; ---------------------------------------------------------------------------
;; Below `->jsonb` and `pgobject->clj` for the same reason as the poem package
;; fns: no caller ever holds a PGobject (§4 1.5).

(defn save-reading-position!
  "Upsert where the reader left `surface` of `topic-id`.

   Pre : `anchor` is `{:i :fp :frac}` and `fraction` is in [0, 1] — the HTTP
         handler validates both; `surface` is one of `reading-anchor/surfaces`.
   Post: one row for (topic-id, surface) when `user-id` owns the topic, else no
         change. Returns the update count.
   Inv : ownership is a clause of the statement, so a forged topic id writes
         nothing. No audit line: a scroll pause is not an action a user takes,
         and it repeats every few seconds of reading."
  [user-id topic-id surface anchor fraction]
  (:next.jdbc/update-count
   (jdbc/execute-one! ds
     ["INSERT INTO reading_positions (topic_id, surface, anchor, fraction, updated_at)
       SELECT t.id, ?, ?, ?, now() FROM topics t WHERE t.id = ? AND t.user_id = ?
       ON CONFLICT (topic_id, surface) DO UPDATE
         SET anchor = excluded.anchor, fraction = excluded.fraction,
             updated_at = excluded.updated_at"
      surface (->jsonb anchor) (double fraction) topic-id user-id])))

(defn get-reading-position
  "Where the reader left `surface` of `topic-id`.
   Post: {:anchor {:i :fp :frac} :fraction} when `user-id` owns the topic and a
         row exists, else nil. `:anchor` is a Clojure map, never a PGobject,
         because the value crosses the Electric wire."
  [user-id topic-id surface]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT r.anchor, r.fraction FROM reading_positions r
                     JOIN topics t ON t.id = r.topic_id
                     WHERE r.topic_id = ? AND r.surface = ? AND t.user_id = ?"
                    topic-id surface user-id])]
    {:anchor (pgobject->clj (:reading_positions/anchor row))
     :fraction (some-> (:reading_positions/fraction row) double)}))

;; ---------------------------------------------------------------------------
;; Poem packages — the structured body of a kind `poem` topic
;; ---------------------------------------------------------------------------
;; plans/poem-package-format.md §7 and §8. Both fns live here, below `->jsonb`
;; and `pgobject->clj`, so no caller ever holds a PGobject — the same ownership
;; rule `insert-source-rows!` applies to `:csl`.

(defn insert-poem-package!
  "Insert the one `poem_packages` row of a poem topic.

   Pre : `tx` is an open transaction in which `topic-id` was already inserted.
         `pkg` is `{:manifest M :verses [..] :notes [..] :sources [..]}` of plain
         Clojure values — `freememo.poem-import` has already validated it.
   Post: the new row's id. The row is written inside `tx`, so the topic and its
         package commit together or neither does (spec row 2.2).
   Invariant: the four documents are stored VERBATIM. Nothing here reshapes the
         package, because `get-poem-package`'s contract is that the reader sees
         what the build script wrote."
  [tx user-id topic-id {:keys [manifest verses notes sources]}]
  (:poem_packages/id
   (jdbc/execute-one! tx
     (sql/format {:insert-into :poem_packages
                  :values [{:topic_id topic-id
                            :user_id user-id
                            :manifest (->jsonb (or manifest {}))
                            :verses (->jsonb (or verses []))
                            :notes (->jsonb (or notes []))
                            :sources (->jsonb (or sources []))}]
                  :returning [:id]}))))

(defn get-poem-package
  "The poem package of `topic-id`, or nil when the topic has none.

   Pre : `topic-id` is a topic id, or nil.
   Post: `{:manifest {…} :verses [{…}] :notes [{…}] :sources [{…}]}` with
         keyword keys and string values unchanged (format doc §8). `:type` stays
         a string and `:words` stays a vector of maps with `:i`, `:half`,
         `:text`, because `pgobject->clj` keywordizes KEYS only.
   Invariant: reads no other column. A caller wanting the title reads the topic
         row — this one answers for the package alone."
  [topic-id]
  (when topic-id
    (when-let [row (jdbc/execute-one! ds
                     (sql/format {:select [:manifest :verses :notes :sources]
                                  :from :poem_packages
                                  :where [:= :topic_id topic-id]}))]
      {:manifest (pgobject->clj (:poem_packages/manifest row))
       :verses (pgobject->clj (:poem_packages/verses row))
       :notes (pgobject->clj (:poem_packages/notes row))
       :sources (pgobject->clj (:poem_packages/sources row))})))

(defn create-source!
  "Insert a sources row. attrs = {:user-id :csl-type :csl :url :title}.
   :csl is a CSL-JSON map (may be nil → {}). Returns full row with :sources/id.
   container_title is denormalized from csl.container-title."
  [{:keys [user-id csl-type csl url title] :as attrs}]
  (when (or (nil? user-id) (str/blank? csl-type))
    (throw (ex-info "create-source! requires :user-id and :csl-type"
             {:type ::invalid-source-attrs :attrs attrs})))
  (let [container (when (map? csl) (:container-title csl))
        row (jdbc/execute-one! ds
              (sql/format {:insert-into :sources
                           :values [{:user_id user-id
                                     :csl_type csl-type
                                     :csl (->jsonb (or csl {}))
                                     :url url
                                     :title title
                                     :container_title container}]
                           :returning [:*]}))]
    (log/audit! {:id ::source-created :user-id user-id :action :create
                 :entity :source :entity-id (:sources/id row)})
    (update row :sources/csl pgobject->clj)))

(defn get-source
  "Get a source by id. Parses csl JSONB to a Clojure map. Returns nil for nil id."
  [id]
  (when id
    (when-let [row (jdbc/execute-one! ds
                     (sql/format {:select [:*]
                                  :from [:sources]
                                  :where [:= :id id]}))]
      (update row :sources/csl pgobject->clj))))

(defn find-source-by-url
  "Look up a sources row for (user-id, url). Returns row or nil.
   Used at import to dedupe — same wiki article → one sources row, many topics.

   Ordered, for the reason `find-web-topic-by-source-id` gives about its own
   `LIMIT 1`: `idx_sources_user_url` is NOT unique and one URL really does hold
   several rows. `sphinx-import` writes its build's source through
   `insert-source-rows!` rather than `ensure-source-for-url!`, so every
   re-import of a build adds another — production carries eight rows for one
   Blender manual URL and six for another, with only one of each set attached
   to a topic. Unordered, this returned whichever row the planner reached
   first, so a dedupe could look past the attached one and import the page
   again. Oldest wins, matching that sibling."
  [user-id url]
  (when (and user-id (not (str/blank? url)))
    (when-let [row (jdbc/execute-one! ds
                     (sql/format {:select [:*]
                                  :from [:sources]
                                  :where [:and [:= :user_id user-id] [:= :url url]]
                                  :order-by [[:id :asc]]
                                  :limit 1}))]
      (update row :sources/csl pgobject->clj))))

(defn update-source!
  "Update a sources row. attrs = {:id :csl-type :csl :url :title}. id required;
   other keys updated only when present. Bumps updated_at. container_title
   is re-denormalized when csl is provided.

   CALLER OBLIGATION: a citation change must reach the cards that cite it.
   Follow this with `set-anki-bibliography-snapshot!`, passing
   `anki-sync-helpers/format-bibliography-html` of the new CSL — otherwise the
   push keeps sending the new citation while every staleness surface reports the
   card as synced, so nothing ever sends it.

   The call is not folded in here on purpose: rendering the citation is
   `anki-sync-helpers`' job, and requiring that namespace from this one would put
   missionary and the CLJS push pipeline under every namespace that touches the
   database. Three call sites carry it instead —
   `biblio-import/…` twice and `bibliography-form/save-source-for-topic!*`."
  [{:keys [id csl-type csl url title]}]
  (when (nil? id) (throw (ex-info "update-source! requires :id" {:type ::missing-id})))
  (let [set-map (cond-> {:updated_at (java.sql.Timestamp. (System/currentTimeMillis))}
                  csl-type (assoc :csl_type csl-type)
                  (some? csl) (assoc :csl (->jsonb csl)
                                :container_title (when (map? csl) (:container-title csl)))
                  (some? url) (assoc :url url)
                  (some? title) (assoc :title title))
        row (jdbc/execute-one! ds
              (sql/format {:update :sources
                           :set set-map
                           :where [:= :id id]
                           :returning [:*]}))]
    (when row
      (log/audit! {:id ::source-updated :user-id (:sources/user_id row) :action :update
                   :entity :source :entity-id id}))
    (some-> row (update :sources/csl pgobject->clj))))

(defn attach-source-to-topic!
  "Set topics.source_id for a topic owned by user-id. Returns update count."
  [user-id topic-id source-id]
  (jdbc/execute-one! ds
    ["UPDATE topics SET source_id = ? WHERE id = ? AND user_id = ?"
     source-id topic-id user-id]))

(defn resolve-effective-source-id
  "Source id a topic should cite: its own source_id when set, else the nearest
   ancestor's (walking parent_id upward), else nil.
   Pre:  topic-id may be nil (→ nil).
   Post: returns a sources.id drawn from topic-id's ancestor chain (self
         included), preferring the shallowest; nil when no topic in the chain
         has a source. Legacy source-less extracts thus resolve to the root."
  [topic-id]
  (when topic-id
    (:source_id
     (jdbc/execute-one! ds
       ["WITH RECURSIVE anc(id, parent_id, source_id, depth) AS (
           SELECT id, parent_id, source_id, 0 FROM topics WHERE id = ?
           UNION ALL
           SELECT t.id, t.parent_id, t.source_id, a.depth + 1
             FROM topics t JOIN anc a ON t.id = a.parent_id)
         SELECT source_id FROM anc
          WHERE source_id IS NOT NULL
          ORDER BY depth
          LIMIT 1"
        topic-id]
       {:builder-fn rs/as-unqualified-maps}))))

(defn get-ancestor-ids
  "Topic ids from `topic-id` up to its root, nearest-first (self at depth 0).
   Pre:  topic-id may be nil.
   Post: [self parent … root] ordered by depth ascending; [] when topic-id nil
         or unknown. Used to resolve per-item settings that inherit up the tree."
  [topic-id]
  (if (nil? topic-id)
    []
    (mapv :id
      (jdbc/execute! ds
        ["WITH RECURSIVE anc(id, parent_id, depth) AS (
            SELECT id, parent_id, 0 FROM topics WHERE id = ?
            UNION ALL
            SELECT t.id, t.parent_id, a.depth + 1
              FROM topics t JOIN anc a ON t.id = a.parent_id)
          SELECT id FROM anc ORDER BY depth"
         topic-id]
        {:builder-fn rs/as-unqualified-maps}))))

(defn resolve-pdf-page-numbers
  "PDF page number for each id in `topic-ids`: the page_number of the nearest
   kind='page' ancestor-or-self whose OWN parent is kind='pdf'.

   The parent-kind test is the discriminator, not page_number alone. EPUB
   chapters store a 1-based chapter index in the same topics.page_number column
   (create-epub-topic!'s chapter rows), so a nil-check would report chapter 3 as
   page 3. It also excludes a score topic's pages: a score topic drives the PDF
   viewer as its own root but is kind='score'. Page stubs only ever land under
   the topic owning the PDF (insert-page-stubs!), so parent kind IS the test.

   Ancestor depth is unbounded — an extract nested under an extract under a page
   resolves to that page. The walk does NOT stop at an intermediate topic that
   carries its own source_id: every extract gets a cloned source at creation
   (content-toolbar-helpers/create-extract-topic-safe!), so that test would
   block the nested case it exists to serve.

   The seed list binds as ONE int4[] parameter through `= ANY(?)`, not one
   placeholder per id. `topics.id` is SERIAL (int4), so every real id fits. An
   IN-list would have put the whole card set under the 65 535 bound-parameter
   ceiling this file documents for bulk inserts.

   Pre:  topic-ids is a collection of topic ids. nils and duplicates are dropped.
         Every remaining id fits in an int4 — a wider value is a caller bug and
         throws here rather than silently missing.
   Post: {topic-id page-number}, holding ONLY the ids that resolve — an id with
         no PDF page ancestor is absent, never mapped to nil. Exactly one
         statement with exactly one parameter, whatever the collection size."
  [topic-ids]
  (let [ids (into [] (comp (remove nil?) (distinct) (map int)) topic-ids)]
    (if (empty? ids)
      {}
      (into {}
        (map (juxt :seed :page_number))
        (jdbc/execute! ds
          ["WITH RECURSIVE anc(seed, id, parent_id, kind, page_number, depth) AS (
              SELECT t.id, t.id, t.parent_id, t.kind, t.page_number, 0
                FROM topics t WHERE t.id = ANY(?)
              UNION ALL
              SELECT a.seed, t.id, t.parent_id, t.kind, t.page_number, a.depth + 1
                FROM topics t JOIN anc a ON t.id = a.parent_id)
            SELECT DISTINCT ON (a.seed) a.seed, a.page_number
              FROM anc a JOIN topics p ON a.parent_id = p.id
             WHERE a.kind = 'page'
               AND a.page_number IS NOT NULL
               AND p.kind = 'pdf'
             ORDER BY a.seed, a.depth"
           (int-array ids)]
          {:builder-fn rs/as-unqualified-maps})))))

(defn clone-source!
  "Duplicate sources row `source-id` into a NEW row owned by user-id, so later
   edits to either row stay independent. Copies csl/csl_type/url/title.
   Pre:  source-id may be nil (→ nil).
   Post: returns the new sources.id, or nil when source-id is nil or the row
         is missing."
  [user-id source-id]
  (when source-id
    (when-let [src (get-source source-id)]
      (:sources/id
       (create-source! {:user-id  user-id
                        :csl-type (:sources/csl_type src)
                        :csl      (:sources/csl src)
                        :url      (:sources/url src)
                        :title    (:sources/title src)})))))

(defn- kind->csl-type
  "Map legacy topics.kind values to CSL-JSON type tokens."
  [kind]
  (case kind
    "pdf" "document"
    "epub" "book"
    "audio" "song"
    "webpage"))

(defn- ts->date-parts
  "Convert a java.sql.Timestamp to a CSL date-parts vector [[year month day]]."
  [^java.sql.Timestamp ts]
  (let [ldt (.toLocalDateTime ts)]
    [[(.getYear ldt) (.getMonthValue ldt) (.getDayOfMonth ldt)]]))

(defn- now-date-parts
  "CSL date-parts for the current instant. Convenience for accessed-at."
  []
  (ts->date-parts (java.sql.Timestamp. (System/currentTimeMillis))))

(defn backfill-sources!
  "Idempotent migration: build a sources row for every distinct
   (user_id, source_url) on topics where source_id IS NULL. Links the
   topics back via source_id.

   Mapping for the legacy topics.kind on the earliest-created row in each group:
     pdf       → csl_type='document'
     epub      → csl_type='book'
     wikipedia → csl_type='webpage' + csl.container-title='Wikipedia'
     web/other → csl_type='webpage'

   csl.URL ← source_url, csl.title ← topics.title, csl.accessed ←
   topics.created_at (approx — accessed-at ≈ import time)."
  []
  (let [groups (try
                 (jdbc/execute! ds
                   ["SELECT user_id, source_url,
                            MIN(created_at) AS first_seen,
                            (ARRAY_AGG(kind ORDER BY created_at))[1] AS canonical_kind,
                            (ARRAY_AGG(title ORDER BY created_at))[1] AS canonical_title
                     FROM topics
                     WHERE source_url IS NOT NULL AND source_id IS NULL
                       AND user_id IS NOT NULL
                     GROUP BY user_id, source_url"]
                   {:builder-fn rs/as-unqualified-maps})
                 ;; If the column was already dropped (post-T4 reboot), this
                 ;; query errors; treat as "nothing to backfill".
                 (catch Exception _ []))]
    (when (seq groups)
      (jdbc/with-transaction [tx ds]
        (doseq [{:keys [user_id source_url first_seen canonical_kind canonical_title]} groups]
          (let [csl-type (kind->csl-type canonical_kind)
                csl (cond-> {:type csl-type
                             :title canonical_title
                             :URL source_url
                             :accessed {:date-parts (ts->date-parts first_seen)}}
                      (= canonical_kind "wikipedia")
                      (assoc :container-title "Wikipedia"))
                src (jdbc/execute-one! tx
                      (sql/format {:insert-into :sources
                                   :values [{:user_id user_id
                                             :csl_type csl-type
                                             :csl (->jsonb csl)
                                             :url source_url
                                             :title canonical_title
                                             :container_title (:container-title csl)
                                             :created_at first_seen}]
                                   :returning [:id]}))
                sid (:sources/id src)]
            (jdbc/execute! tx
              ["UPDATE topics SET source_id = ?
                WHERE user_id = ? AND source_url = ? AND source_id IS NULL"
               sid user_id source_url])))
        (tel/log! {:level :info :id ::backfill-sources :data {:count (count groups)}}
          (str "Backfilled " (count groups) " source(s) from topics.source_url"))))))

(defn backfill-pdf-sources!
  "Legacy PDF root topics created before create-pdf-topic! attached a sources row
   have source_id IS NULL, which leaves 'Refetch bibliography' disabled. Create a
   document source mirroring create-pdf-topic! (csl_type 'document', title from the
   topic) and link it. Idempotent: only touches kind='pdf' rows with source_id IS NULL."
  []
  (let [pdfs (jdbc/execute! ds
               ;; Skip orphaned topics (user deleted → user_id SET NULL); a
               ;; sources row requires a non-null user_id and they have no owner.
               ["SELECT id, user_id, title FROM topics
                 WHERE kind = 'pdf' AND source_id IS NULL AND user_id IS NOT NULL"]
               {:builder-fn rs/as-unqualified-maps})]
    (when (seq pdfs)
      (jdbc/with-transaction [tx ds]
        (doseq [{:keys [id user_id title]} pdfs]
          (let [csl {:type "document" :title title
                     :accessed {:date-parts (now-date-parts)}}
                src (jdbc/execute-one! tx
                      (sql/format {:insert-into :sources
                                   :values [{:user_id user_id
                                             :csl_type "document"
                                             :csl (->jsonb csl)
                                             :title title}]
                                   :returning [:id]}))
                sid (:sources/id src)]
            (jdbc/execute! tx
              ["UPDATE topics SET source_id = ? WHERE id = ? AND source_id IS NULL"
               sid id])))
        (tel/log! {:level :info :id ::backfill-pdf-sources :data {:count (count pdfs)}}
          (str "Backfilled " (count pdfs) " PDF source(s)"))))))

;; ---------------------------------------------------------------------------
;; Compound creation helpers
;; ---------------------------------------------------------------------------

(defn- insert-page-stubs!
  "Batch-insert kind='page' topic stubs titled \"Page N\" for page numbers
   `from-n`..`to-n` inclusive, under parent-id. One multi-row INSERT instead
   of one per page. ON CONFLICT (parent_id, page_number) DO NOTHING — a
   re-run, or a page already materialized by OCR, is a no-op.
   tx may be the pool `ds` or an in-flight transaction."
  [tx user-id parent-id from-n to-n]
  (when (<= from-n to-n)
    (jdbc/execute! tx
      (sql/format {:insert-into :topics
                   :values (mapv (fn [n] {:user_id user-id
                                           :parent_id parent-id
                                           :kind "page"
                                           :title (str "Page " n)
                                           :page_number n})
                             (range from-n (inc to-n)))
                   :on-conflict [:parent_id :page_number {:where [:is-not :page_number nil]}]
                   :do-nothing true}))))

(defn create-pdf-topic!
  "Create a PDF root topic with file data, page stubs, and a `sources` row.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Returns the result with :topics/id."
  [user-id filename file-bytes file-size page-count]
  (let [clean-name (input/prettify-title (input/sanitize-title filename))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [topic-title (available-import-title tx user-id clean-name)
            csl {:type "document" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "document"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "pdf"
                                           :title topic-title
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size "application/pdf"])
        (when (pos? page-count)
          (insert-page-stubs! tx user-id topic-id 1 page-count))
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-audio-topic!
  "Create an audio root topic with file data and a `sources` row.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Sets content to \"\" (not NULL) so the editor mounts and can receive a
   transcript instead of showing a permanent loading spinner.
   Returns the result with :topics/id."
  [user-id filename file-bytes file-size mime-type]
  (let [clean-name (input/prettify-title (input/sanitize-title filename))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [topic-title (available-import-title tx user-id clean-name)
            csl {:type "song" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "song"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "audio"
                                           :title topic-title
                                           :content ""
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size mime-type])
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-score-topic!
  "Create a score root topic owning TWO blobs: the sheet-music PDF as
   role='main' (so /api/pdf and the PDF viewer work unchanged) and the
   recording as role='audio'. No page stubs — cards attach to the root.
   Atomically checks quota against the combined byte size.
   Returns the result with :topics/id."
  [user-id title ^bytes pdf-bytes ^bytes audio-bytes audio-mime]
  (let [clean-name (input/prettify-title (input/sanitize-title title))
        pdf-size (alength pdf-bytes)
        audio-size (alength audio-bytes)]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id (+ pdf-size audio-size))
      (let [topic-title (available-import-title tx user-id clean-name)
            csl {:type "song" :title clean-name
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "song"
                                            :csl (->jsonb csl)
                                            :title clean-name}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "score"
                                           :title topic-title
                                           :content ""
                                           :source_id source-id}]
                                 :returning [:id]}))
            topic-id (:topics/id topic)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'main', ?, ?, 'application/pdf')"
           topic-id pdf-bytes pdf-size])
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'audio', ?, ?, ?)"
           topic-id audio-bytes audio-size audio-mime])
        (audit-doc-created! user-id topic-id)
        topic))))

(defn create-live-doc!
  "Create an empty Live Document: a kind='pdf', is_live=true root topic with a
   bibliography `sources` row and NO file blob or page stubs. The backing PDF is
   created lazily on the first appended image batch (see commit-live-append!).
   Returns the topic with :topics/id."
  [user-id]
  (let [title (str "Live Document " (java.time.LocalDate/now))]
    (input/check-length! :title title input/title-max)
    (jdbc/with-transaction [tx ds]
      (let [csl {:type "document" :title title
                 :accessed {:date-parts (now-date-parts)}}
            source (jdbc/execute-one! tx
                     (sql/format {:insert-into :sources
                                  :values [{:user_id user-id
                                            :csl_type "document"
                                            :csl (->jsonb csl)
                                            :title title}]
                                  :returning [:id]}))
            source-id (:sources/id source)
            topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "pdf"
                                           :title title
                                           :is_live true
                                           :source_id source-id}]
                                 :returning [:id]}))]
        (audit-doc-created! user-id (:topics/id topic))
        topic))))

(defn commit-live-append!
  "Atomically persist an appended PDF blob for a Live Document.
   Pre:  topic-id is a kind='pdf', is_live=true topic owned by user-id (the HTTP
         handler enforces ownership); new-bytes is the full rebuilt PDF;
         new-total is its page count; prev-total the count before this batch;
         delta-bytes >= 0 is the blob growth to charge against quota.
   Post: blob replaced, users.usage_bytes bumped by delta-bytes, page stubs
         created for page numbers (prev-total+1 .. new-total). A quota violation
         throws quota/quota-error and aborts the whole tx — nothing persists.
   Returns {:pages-added (- new-total prev-total)}."
  [user-id topic-id ^bytes new-bytes new-size delta-bytes prev-total new-total]
  (jdbc/with-transaction [tx ds]
    (when (pos? delta-bytes)
      (quota/check-and-bump! tx user-id delta-bytes))
    (jdbc/execute! tx
      ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
        VALUES (?, ?, ?, 'application/pdf')
        ON CONFLICT (topic_id, role)
        DO UPDATE SET file_data = excluded.file_data, file_size = excluded.file_size,
                      mime_type = excluded.mime_type"
       topic-id new-bytes new-size])
    (insert-page-stubs! tx user-id topic-id (inc prev-total) new-total)
    {:pages-added (- new-total prev-total)}))

(defn find-web-topic-by-title
  "Find an existing root-level web topic by title (case-insensitive).
   Returns {:topics/id ...} or nil."
  [user-id title]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :title]
                 :from [:topics]
                 :where [:and
                         [:= :user_id user-id]
                         [:= :kind "web"]
                         [:is :parent_id nil]
                         [:= [:lower :title] (str/lower-case title)]]})))

(defn find-web-topic-by-source-id
  "Find an existing root-level web topic for (user-id, source-id).
   Used to dedupe URL imports — a single sources row may already be
   linked from a topic, in which case re-importing should return the
   existing topic instead of creating a duplicate.
   Returns {:topics/id ...} or nil.

   Ordered, because a source can now hold more than one root. The capture
   extension's version setting creates siblings that share a source row
   deliberately, and an unordered `LIMIT 1` over them returns whichever the
   planner reaches first — so the same re-import could resolve to a different
   topic on consecutive calls. Oldest wins."
  [user-id source-id]
  (when (and user-id source-id)
    (jdbc/execute-one! ds
      (sql/format {:select [:id :title]
                   :from [:topics]
                   :where [:and
                           [:= :user_id user-id]
                           [:= :kind "web"]
                           [:is :parent_id nil]
                           [:= :source_id source-id]
                           [:is :staged_delete_id nil]]
                   :order-by [[:id :asc]]
                   :limit 1}))))

(defn find-topic-by-content-digest
  "The user's topic under `source-id` whose stored text matches `content-text`.

   Pre : user-id and source-id non-nil. content-text is the plain text a
         caller is about to store, already derived from sanitized HTML.
   Post: {:topics/id :topics/title} or nil.
   Inv : compares digests, never text, so the query stays index-eligible
         against `idx_topics_source_content_digest`.

   Blank `content-text` matches nothing. A screenshot has none until OCR runs,
   and every screenshot would otherwise collide with every other one — that
   case belongs to `find-pdf-topic-by-source-id`."
  [user-id source-id content-text]
  (when (and user-id source-id (not (str/blank? content-text)))
    (jdbc/execute-one! ds
      ["SELECT id, title FROM topics
        WHERE user_id = ? AND source_id = ? AND parent_id IS NULL
          AND staged_delete_id IS NULL
          AND content_text IS NOT NULL
          AND md5(content_text) = md5(?)
        ORDER BY id ASC
        LIMIT 1"
       user-id source-id content-text])))

(defn find-pdf-topic-by-source-id
  "The user's root PDF topic for `source-id`, oldest first.

   Pre : user-id and source-id non-nil.
   Post: {:topics/id :topics/title} or nil.
   Inv : matches on the source alone, which for a PDF means the URL.

   A screenshot capture stores no text until the user runs OCR, so a content
   digest cannot tell two of them apart. The source row carries the URL, and
   the URL is the whole identity of a screenshot."
  [user-id source-id]
  (when (and user-id source-id)
    (jdbc/execute-one! ds
      (sql/format {:select [:id :title]
                   :from [:topics]
                   :where [:and
                           [:= :user_id user-id]
                           [:= :kind "pdf"]
                           [:is :parent_id nil]
                           [:= :source_id source-id]
                           [:is :staged_delete_id nil]]
                   :order-by [[:id :asc]]
                   :limit 1}))))

(defn ensure-source-for-url!
  "Find or create a sources row for (user-id, url). Returns row map (with id).
   bib = {:source-type 'wikipedia'|'webpage'|nil
          :title nil-or-str
          :issued-date-parts nil-or-[[Y M? D?]]}.
   Used at import time to dedupe by URL."
  [user-id url bib]
  (when (and user-id (not (str/blank? url)))
    (or (find-source-by-url user-id url)
        (let [wikipedia? (= (:source-type bib) "wikipedia")
              title (:title bib)
              issued-dp (:issued-date-parts bib)
              csl (cond-> {:type "webpage"
                           :title title
                           :URL url
                           :accessed {:date-parts (now-date-parts)}}
                    wikipedia? (assoc :container-title "Wikipedia")
                    (seq issued-dp) (assoc :issued {:date-parts (csl/pad-date-parts issued-dp)}))]
          (create-source! {:user-id user-id
                           :csl-type "webpage"
                           :csl csl
                           :url url
                           :title title})))))

(defn create-web-topic!
  "Create a web article topic. Returns topic-id.
   `bib` (optional) = {:url ... :source-type 'wikipedia'|'webpage'|nil}.
   When :url is non-blank, ensures a `sources` row (per-user dedup by URL)
   and links topics.source_id. :source-type 'wikipedia' tags
   csl.container-title='Wikipedia'."
  [user-id title html-content bib]
  (let [clean-title (input/prettify-title (input/sanitize-title title))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (sanitize-utf8 html-content)
        url (some-> bib :url str/trim not-empty)
        source-id (some-> (ensure-source-for-url! user-id url
                            (assoc bib :title clean-title))
                    :sources/id)
        topic-title (available-import-title user-id clean-title)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [(cond-> {:user_id user-id
                                               :kind "web"
                                               :title topic-title
                                               :content sanitized
                                               :content_text (text/strip-html sanitized)}
                                        source-id (assoc :source_id source-id))]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    (:topics/id topic)))

(defn create-markdown-topic!
  "Create a Markdown-imported topic. Returns topic-id."
  [user-id title html-content]
  (let [clean-title (input/prettify-title (input/sanitize-title title))
        _ (input/check-length! :title clean-title input/title-max)
        sanitized (sanitize-utf8 html-content)
        topic-title (available-import-title user-id clean-title)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "markdown"
                                       :title topic-title
                                       :content sanitized
                                       :content_text (text/strip-html sanitized)}]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    (:topics/id topic)))

(defn insert-epub-topic!
  "Create the EPUB topic, its `sources` row and its chapter children, and NOTHING
   about where the bytes live.

   Pre : `tx` is an open transaction. `chapters` is a vec of {:html :title}.
         Quota is the CALLER's to settle — the two callers settle it differently
         and neither may be assumed here.
   Post: {:topic-id N :chapter-ids [...]}, chapter ids in spine order.
   Blame: caller bug to invoke this outside a transaction — a half-built book
         with no blob row is not something this fn can clean up.

   Extracted so `create-epub-topic!` (BYTEA, multipart) and
   `finalize-epub-session!` (large object, chunked) share one definition of what
   an EPUB topic IS. Only the blob row differs between them."
  [tx user-id title parent-id chapters]
  (let [clean-title (input/prettify-title (input/sanitize-title title))
        _ (input/check-length! :title clean-title input/title-max)
        topic-title (if parent-id
                      clean-title
                      (available-import-title tx user-id clean-title))
        csl {:type "book" :title clean-title
             :accessed {:date-parts (now-date-parts)}}
        source (jdbc/execute-one! tx
                 (sql/format {:insert-into :sources
                              :values [{:user_id user-id
                                        :csl_type "book"
                                        :csl (->jsonb csl)
                                        :title clean-title}]
                              :returning [:id]}))
        topic (jdbc/execute-one! tx
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :parent_id parent-id
                                       :kind "epub"
                                       :title topic-title
                                       :source_id (:sources/id source)}]
                             :returning [:id]}))
        topic-id (:topics/id topic)
        ;; Create chapter children — one multi-row INSERT instead of one per
        ;; chapter. Each row already carries a stable, unique-within-this-batch
        ;; ordinal (:page_number = 1-based chapter index); RETURNING it back
        ;; alongside id and re-associating by that value keeps chapter-ids
        ;; correctly ordered without trusting Postgres' VALUES/RETURNING row
        ;; order, which SQL does not guarantee.
        chapter-rows (map-indexed
                       (fn [i ch]
                         (let [sanitized (sanitize-utf8 (:html ch))]
                           {:user_id user-id
                            :parent_id topic-id
                            :kind "basic"
                            :title (input/prettify-title
                                     (or (:title ch) (str "Chapter " (inc i))))
                            :content sanitized
                            :content_text (text/strip-html sanitized)
                            :page_number (inc i)
                            :status "active"
                            :priority priority/priority-default}))
                       chapters)
        chapter-ids (when (seq chapter-rows)
                      (let [returned (jdbc/execute! tx
                                       (sql/format {:insert-into :topics
                                                    :values (vec chapter-rows)
                                                    :returning [:id :page_number]}))
                            id-by-page (into {} (map (juxt :topics/page_number :topics/id)) returned)]
                        (mapv (comp id-by-page :page_number) chapter-rows)))]
    {:topic-id topic-id :chapter-ids (vec (remove nil? chapter-ids))}))

(defn create-epub-topic!
  "Create an EPUB root topic with file, chapter children, and a `sources` row.
   chapters is a vec of {:html :title} maps.
   Atomically checks quota and increments users.usage_bytes by file-size.
   Throws `quota/quota-error` on cap violation — caller's tx aborts.
   Returns {:topic-id N :chapter-ids [...]}

   The multipart path, which stores bytes in `topic_files.file_data` (BYTEA).
   A chunk-uploaded book goes through `finalize-epub-session!` instead and
   stores a large object — see plans/large-epub-import.md §7.4."
  [user-id title file-bytes file-size chapters]
  (let [clean-title (input/prettify-title (input/sanitize-title title))]
    (input/check-length! :title clean-title input/title-max)
    (jdbc/with-transaction [tx ds]
      (quota/check-and-bump! tx user-id file-size)
      (let [{:keys [topic-id] :as r} (insert-epub-topic! tx user-id title nil chapters)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, file_data, file_size, mime_type)
            VALUES (?, ?, ?, ?)"
           topic-id file-bytes file-size "application/epub+zip"])
        (audit-doc-created! user-id topic-id)
        r))))

(defn create-standalone-topic!
  "Create a standalone empty topic. Returns {:topic-id N}."
  [user-id title]
  (let [clean-title (input/prettify-title (input/sanitize-title title))
        _ (input/check-length! :title clean-title input/title-max)
        topic (jdbc/execute-one! ds
                (sql/format {:insert-into :topics
                             :values [{:user_id user-id
                                       :kind "basic"
                                       :title clean-title
                                       :content ""
                                       :content_text ""}]
                             :returning [:id]}))]
    (audit-doc-created! user-id (:topics/id topic))
    {:topic-id (:topics/id topic)}))

;; ---------------------------------------------------------------------------
;; Page-specific operations (kind='page' child topics)
;; ---------------------------------------------------------------------------

(defn create-page-stubs!
  "Batch-insert empty page topics for a parent. ON CONFLICT DO NOTHING."
  [parent-id page-count user-id]
  (when (pos? page-count)
    (insert-page-stubs! ds user-id parent-id 1 page-count)))

(defn save-page-text!
  "Save or update OCR text for a page topic (by parent + page_number).
   Upserts the page topic row."
  [parent-id page-number html]
  (let [plain (text/strip-html html)]
    (jdbc/execute! ds
      ["INSERT INTO topics (parent_id, kind, title, content, content_text, page_number,
                            user_id)
        SELECT ?, 'page', ?, ?, ?, ?,
               (SELECT user_id FROM topics WHERE id = ?)
        WHERE NOT EXISTS (SELECT 1 FROM topics WHERE parent_id = ? AND page_number = ?)
        ON CONFLICT DO NOTHING"
       parent-id (str "Page " page-number) html plain page-number
       parent-id parent-id page-number])
    ;; Update existing
    (jdbc/execute! ds
      ["UPDATE topics SET content = ?, content_text = ?
        WHERE parent_id = ? AND page_number = ?"
       html plain parent-id page-number])))

(defn get-page-text
  "Get the content (OCR text) for a page by parent + page_number."
  [parent-id page-number]
  (jdbc/execute-one! ds
    (sql/format {:select [:content]
                 :from [:topics]
                 :where [:and
                         [:= :parent_id parent-id]
                         [:= :page_number page-number]]})))

(defn list-pages
  "List all page topics for a parent, ordered by page_number."
  [parent-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:topics]
                 :where [:and [:= :parent_id parent-id] [:= :kind "page"]
                         [:is :staged_delete_id nil]]
                 :order-by [[:page_number :asc]]})))

(defn get-page-done-status
  "Check if a page is done. Returns boolean."
  [parent-id page-number]
  (let [result (jdbc/execute-one! ds
                 (sql/format {:select [:status]
                              :from [:topics]
                              :where [:and
                                      [:= :parent_id parent-id]
                                      [:= :page_number page-number]]}))]
    (= "done" (:topics/status result))))

(defn toggle-page-done!
  "Toggle a page topic between 'active' and 'done' status.

   Post: `done_at` moves with `status` in the SAME statement — stamped on the
         way into 'done', NULL on the way out. Two statements could interleave
         with a concurrent read and show a done page with no timestamp, which
         the daily quota would count as covered on some earlier day.
   Post: this is a TOGGLE, so re-marking a page takes two calls — one to clear
         the stamp and one to set a fresh instant. Both land on the same day, so
         the daily quota counts that page once however many times it is toggled."
  [parent-id page-number]
  (jdbc/execute-one! ds
    ["UPDATE topics
        SET status  = CASE WHEN status = 'done' THEN 'active' ELSE 'done' END,
            done_at = CASE WHEN status = 'done' THEN NULL ELSE now() END
      WHERE parent_id = ? AND page_number = ?"
     parent-id page-number]))

(defn set-page-done!
  "Set one owned PDF page to an explicit completion state.

   Pre: `user-id` owns the page and `done?` is boolean.
   Post: returns the persisted boolean, or nil when no owned page matched.
   Inv: an idempotent write preserves `done_at`; only a transition into done
        stamps the completion time and only a transition out clears it."
  [user-id parent-id page-number done?]
  (some-> (jdbc/execute-one! ds
           ["UPDATE topics
               SET status = CASE WHEN ? THEN 'done' ELSE 'active' END,
                   done_at = CASE
                     WHEN ? AND status <> 'done' THEN now()
                     WHEN NOT ? AND status = 'done' THEN NULL
                     ELSE done_at
                   END
             WHERE parent_id = ?
               AND page_number = ?
               AND user_id = ?
               AND kind = 'page'
               AND staged_delete_id IS NULL
             RETURNING status"
            done? done? done? parent-id page-number user-id])
    :topics/status
    (= "done")))

(defn get-context-pages
  "Get content from page topics in a range, for card generation context."
  [parent-id start-page end-page]
  (jdbc/execute! ds
    (sql/format {:select [:id :page_number :content :created_at]
                 :from [:topics]
                 :where [:and
                         [:= :parent_id parent-id]
                         [:= :kind "page"]
                         [:>= :page_number start-page]
                         [:<= :page_number end-page]]
                 :order-by [[:page_number :asc]]})))

(defn- epub-chapter-where
  "The rows that are chapters of EPUB `epub-id`. A chapter is a `basic` child
   whose `page_number` is its 1-based index (`insert-epub-topic!`). A staged
   chapter is not one."
  [epub-id]
  [:and [:= :parent_id epub-id] [:= :kind "basic"]
   [:is-not :page_number nil] [:is :staged_delete_id nil]])

(defn get-epub-chapters
  "The chapters of EPUB `epub-id` numbered `start` to `end`, in order.
   Post: rows {:topics/id :topics/page_number :topics/title :topics/content
         :topics/created_at}."
  [epub-id start end]
  (jdbc/execute! ds
    (sql/format {:select [:id :page_number :title :content :created_at]
                 :from [:topics]
                 :where [:and (epub-chapter-where epub-id)
                         [:>= :page_number start] [:<= :page_number end]]
                 :order-by [[:page_number :asc]]})))

(defn get-epub-reading-chapter
  "The index of the first chapter of EPUB `epub-id` that is not done, or nil
   when every chapter is done or the book has none."
  [epub-id]
  (:min (jdbc/execute-one! ds
          (sql/format {:select [[[:min :page_number] :min]]
                       :from [:topics]
                       :where [:and (epub-chapter-where epub-id)
                               [:or [:is :status nil] [:<> :status "done"]]]})
          {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; Flashcard operations
;; ---------------------------------------------------------------------------

(def ^:private occlusion-mask-count-expr
  "Rects in the whole group — the masks the card tables render as
   \"Occlusion · N masks → M cards\", where M is the group's row count and this
   is N. Group-wide rather than per-ordinal because the tables collapse a group
   to ONE row (freememo.card-rows), so no row stands for a single ordinal any
   more. 0 for non-occlusion rows: og.geometry is NULL there, so the coalesce
   yields an empty array."
  [:raw "jsonb_array_length(coalesce(og.geometry->'rects', '[]'::jsonb))"])

(def ^:private card-group-selects
  "Occlusion/score group columns, projected by every query that returns whole
   flashcard rows: get-flashcards, get-all-flashcards, get-flashcards-for-subtree,
   get-flashcards-by-ids, get-user-flashcards. Defined once so those five cannot
   drift — the mask-count expression made the copy-paste load-bearing.
   Pre:  the query aliases flashcards as f and left-joins occlusion_groups as og
         and score_groups as sg.
   Post: aliases are the unqualified keys normalize-flashcard-row promises; the
         count is an expression column, which pgjdbc leaves unqualified already."
  [[:og.image_media_id :occlusion_image_media_id]
   [:og.mode :occlusion_mode]
   [occlusion-mask-count-expr :occlusion_mask_count]
   [:sg.start_ms :score_start_ms]
   [:sg.end_ms :score_end_ms]])

(defn- normalize-flashcard-row
  "Post-process a flashcard query row so it is plain data (Electric-wire safe):
   parse the io_fields JSONB, and move the occlusion-group and score-group
   join columns to the unqualified keys this ns promises
   (:occlusion_image_media_id :occlusion_mode :score_start_ms :score_end_ms) —
   pgjdbc qualifies plain aliased columns by their base table (see
   get-user-flashcards), so cover both key shapes."
  [row]
  (-> row
    (update :flashcards/io_fields pgobject->clj)
    (update :flashcards/overlapping pgobject->clj)
    (assoc :occlusion_image_media_id (or (:occlusion_image_media_id row)
                                       (:occlusion_groups/occlusion_image_media_id row))
      :occlusion_mode (or (:occlusion_mode row)
                        (:occlusion_groups/occlusion_mode row))
      :score_start_ms (or (:score_start_ms row)
                        (:score_groups/score_start_ms row))
      :score_end_ms (or (:score_end_ms row)
                      (:score_groups/score_end_ms row)))
    (dissoc :occlusion_groups/occlusion_image_media_id :occlusion_groups/occlusion_mode
      :score_groups/score_start_ms :score_groups/score_end_ms)))

;; ── Parameter ceiling for multi-row INSERTs ────────────────────────
;; Every batch insert below renders one `VALUES (…), (…), …` statement, and
;; PostgreSQL's wire protocol caps a prepared statement at 65 535 bound
;; parameters. A collection import reaches five figures of rows in one call, so
;; the batch fns partition rather than trusting the caller to keep lists small —
;; only this layer knows the column count, so only this layer can compute the
;; bound.

(def ^:private max-statement-params
  "PostgreSQL's ceiling on bound parameters in one prepared statement. The
   driver refuses the whole statement past it; it does not truncate."
  65535)

(defn- param-bounded-batches
  "Partition `rows` so each batch renders as one INSERT within
   `max-statement-params`.

   Rows are first normalized to the union of every row's keys, so each batch
   emits the same column list the unpartitioned statement would have. Without
   that step a key present in one batch and absent from another would be an
   explicit NULL in the first and a column DEFAULT in the second.

   Batch size uses the column count as the per-row parameter bound. That
   over-counts — HoneySQL renders nil as inline NULL rather than a parameter —
   which is the safe direction to err.

   Pre : `rows` is a non-empty seq of maps.
   Post: seq of row vectors that concatenate to `rows` in order. A row wider
         than the ceiling still gets its own batch rather than none, so the
         driver reports the real problem instead of this fn hanging on a
         zero-sized partition.
   Invariant: order is preserved — callers align returned ids positionally
         against the rows they passed."
  [rows]
  (let [cols (into [] (distinct) (mapcat keys rows))
        blanks (zipmap cols (repeat nil))]
    (partition-all (max 1 (quot max-statement-params (max 1 (count cols))))
      (mapv #(merge blanks %) rows))))

;; ---------------------------------------------------------------------------
;; local_current_digest — the one staleness predicate
;; ---------------------------------------------------------------------------
;;
;; See the column's comment in `setup-schema-flashcards!`. ONE entry point,
;; `restamp-card-digests!`, called by every writer that can change what the
;; digest covers — the four content writers, the settings fan-out and the push
;; refresh. It re-reads the rows rather than digesting the values it was handed,
;; which is what makes it correct for a PARTIAL update: `update-flashcard!` is
;; given one field at a time by the pull path, and digesting only that field
;; would treat every other column as empty.
;;
;; A content writer that skips it leaves a stale digest, and a stale digest is a
;; card that reads synced when it is not. That is the failure mode this column
;; has, and it is why the helper lives here rather than at the call sites.

(def ^:private digest-columns
  "THE flashcards columns `anki-digest/local-digest` reads.

   One definition, two readers: `restamp-card-digests!` and
   `get-cards-for-reconcile`. A third appears the moment the digest covers a new
   column — `local-digest` treats a missing column as \"\", so a query that omits
   one silently stops detecting changes to it, which is exactly how a settings edit
   went undetected before `anki_header_text` existed."
  [:f.id :f.kind :f.question :f.answer :f.cloze
   :f.io_fields :f.overlapping
   :f.score_direction :f.score_group_id
   :f.anki_header_text :f.anki_bibliography_html])

(defn- parse-digest-jsonb
  "Parse the two JSONB columns of a digest row.
   `anki-digest/local-digest` THROWS on a raw PGobject rather than digesting
   nothing, so every reader of `digest-columns` owes this."
  [row]
  (-> row
    (update :flashcards/io_fields pgobject->clj)
    (update :flashcards/overlapping pgobject->clj)))

(defn restamp-card-digests!
  "Recompute `local_current_digest` for `card-ids` from the rows as they stand.

   pre : `connectable` sees the rows; ownership is the caller's to enforce.
   post: every named card's digest equals `local-digest` of its current row.
         Returns the number of cards restamped.
   inv : writes nothing but that derived column, so a caller that passes an id it
         does not own corrupts nothing — the value written is still that row's
         own correct digest.

   THAT LAXITY DOES NOT TRAVEL. It holds only because the column is a pure
   function of the row, so a wrong id costs a wasted correct write. Any writer of a
   column that carries user content owes a real ownership clause;
   `write-anki-settings-snapshot!` is the one to copy, not this fn.

   One statement per batch, two parameters per card, bounded by
   `max-statement-params`. A per-row UPDATE would be thousands of statements for
   a document-wide header change, which is the main caller.

   Pass the enclosing transaction wherever one exists. On a bare `ds` a crash
   between the content write and this leaves the digest NULL, which the staleness
   predicate reads as unsynced for a NEW card through its `anki_note_id IS NULL`
   clause, but as SYNCED for an already-pushed one — `NULL IS DISTINCT FROM NULL`
   is false. That second case is why the content writers below take a
   transaction."
  [connectable card-ids]
  (let [ids (vec (distinct (remove nil? card-ids)))]
    (if (empty? ids)
      0
      (let [pairs (into []
                    (comp (map parse-digest-jsonb)
                      (map (juxt :flashcards/id digest/local-digest)))
                    (jdbc/execute! connectable
                      (sql/format {:select digest-columns
                                   :from [[:flashcards :f]]
                                   :where [:in :f.id ids]})))]
        (doseq [batch (partition-all (quot max-statement-params 2) pairs)]
          (jdbc/execute-one! connectable
            (into [(str "UPDATE flashcards AS f SET local_current_digest = v.digest "
                     "FROM (VALUES "
                     (str/join "," (repeat (count batch) "(?::int, ?::char(64))"))
                     ") AS v(id, digest) WHERE f.id = v.id")]
              (mapcat identity batch))))
        (count pairs)))))

;; ---------------------------------------------------------------------------
;; anki_header_text / anki_bibliography_html — the settings snapshot
;; ---------------------------------------------------------------------------
;;
;; See those columns' comment in `setup-schema-flashcards!`. Three writers, and
;; the split is about how many cards share one value:
;;
;;   `set-anki-header-snapshot!`        one header  → every card under a root
;;   `set-anki-bibliography-snapshot!`  one citation → every card the source cites
;;   `refresh-anki-settings-snapshot!`  per-card values, used by the push path
;;
;; The first two are the EDIT-time fan-out: a `settings` write and a `sources`
;; write touch no flashcards row, so without them a header or citation change is
;; invisible to every staleness surface. The third is what makes a missed fan-out
;; self-healing rather than permanent.
;;
;; All three go through `write-anki-settings-snapshot!`, which owns the ownership
;; clause, the transaction and the parameter bound. None of them may take a
;; caller-built predicate: a callee that cannot check its own scope is how the push
;; refresh came to write content columns with no ownership clause at all.

(defn topic-ids-for-source
  "Topics whose EFFECTIVE source is `source-id`.

   pre : `source-id` is a sources.id.
   post: every topic that cites it, itself included — and NO topic under a nearer
         source. Returns [] for nil.
   inv : the inverse of `resolve-effective-source-id`. For every returned topic t,
         `(resolve-effective-source-id t)` = `source-id`; for every topic under a
         descendant carrying its own source, it does not.

   The descent stops at `t.source_id IS NULL` failing, and that one predicate IS
   the shadowing rule the forward walk expresses as \"prefer the shallowest\". A
   fan-out that instead took the whole root subtree would write a citation onto
   cards that cite something else.

   Written as the inverse relation rather than as a per-topic loop over
   `resolve-effective-source-id`, which would be one query per topic for a
   document-wide citation edit.

   The 2-arity takes the connectable, so a fan-out can resolve its scope inside the
   transaction that then writes it. Reading through `ds` first would leave a window
   in which a re-parent makes the write apply to a scope that no longer holds."
  ([source-id] (topic-ids-for-source ds source-id))
  ([connectable source-id]
   (if (nil? source-id)
     []
     (mapv :id
       (jdbc/execute! connectable
         ["WITH RECURSIVE sub(id) AS (
             SELECT id FROM topics WHERE source_id = ?
             UNION ALL
             SELECT t.id FROM topics t JOIN sub s ON t.parent_id = s.id
              WHERE t.source_id IS NULL)
           SELECT id FROM sub"
          source-id]
         {:builder-fn rs/as-unqualified-maps})))))

(def ^:private snapshot-id-batch
  "How many card ids one snapshot UPDATE may name.

   `SET anki_header_text = ?, anki_bibliography_html = ? WHERE id IN (…)
    AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)` binds two
   column values plus the ownership id alongside the ids, so the id list gets
   `max-statement-params` minus three.

   NOT `param-bounded-batches`. That fn divides the ceiling by a row's COLUMN
   count, which is the right arithmetic for a multi-row `VALUES` insert and the
   wrong arithmetic here: one column per row would give it 65535 ids in a batch,
   and the three other parameters then put the statement one over the ceiling —
   which the driver refuses whole rather than truncating."
  (- max-statement-params 3))

(defn- owned-topic-ids-subquery
  "HoneySQL subquery for the topic ids `user-id` owns.

   pre : `user-id` is NOT nil — callers guard, because HoneySQL renders
         `[:= :user_id nil]` as `user_id IS NULL` rather than as the never-true
         `user_id = NULL`. A nil would therefore scope a write to every topic whose
         `user_id` is NULL, which child topics legitimately are, and fan one
         account's header across others. Measured on dev data."
  [user-id]
  {:select [:id] :from [:topics] :where [:= :user_id user-id]})

(defn- snapshot-scope-predicate
  "HoneySQL predicate for one snapshot scope, ownership included.

   pre : `user-id` non-nil. `scope` is one of
           [:root root-topic-id]  — every card under a root, how the header is stored
           [:topics topic-ids]    — every card under an already-resolved topic set
           [:cards card-ids]      — the exact cards the push is about to write
         `[:source id]` is resolved to `[:topics …]` by `snapshot-scope-card-ids`
         before it reaches here, because that walk needs a connectable.
   post: a predicate no caller can widen. Ownership is applied HERE, which is the
         whole reason `scope` is a descriptor and not a predicate the caller hands
         in — a callee that cannot check its own precondition is how the push
         refresh came to write content columns with no ownership clause at all.

   `[:topics …]` matches on `COALESCE(topic_id, root_topic_id)`, the same key
   `anki-sync-server/attach-card-bibliography` resolves a card's citation by, so
   the fan-out and the push cannot disagree about which cards a source reaches."
  [user-id scope]
  (let [owned [:in :root_topic_id (owned-topic-ids-subquery user-id)]]
    (case (first scope)
      :root   [:and [:= :root_topic_id (second scope)] owned]
      :topics [:and [:in [:coalesce :topic_id :root_topic_id] (vec (second scope))] owned]
      :cards  [:and [:in :id (vec (second scope))] owned])))

(defn- snapshot-scope-card-ids
  "The card ids one snapshot scope names, read through `tx`.

   pre : `user-id` non-nil; `scope` per `snapshot-scope-predicate`, plus
         `[:source source-id]`.
   post: ids of cards `user-id` owns. `[:source …]` resolves the effective-source
         walk HERE rather than in the caller, so the scope is read inside the same
         transaction that writes it."
  [tx user-id scope]
  (let [scope (if (= :source (first scope))
                [:topics (topic-ids-for-source tx (second scope))]
                scope)]
    (if (and (#{:topics :cards} (first scope)) (empty? (second scope)))
      []
      (mapv :id
        (jdbc/execute! tx
          (sql/format {:select [:id] :from [:flashcards]
                       :where (snapshot-scope-predicate user-id scope)})
          {:builder-fn rs/as-unqualified-maps})))))

(defn- write-anki-settings-snapshot!
  "THE writer of `anki_header_text` and `anki_bibliography_html`. One transaction.

   pre : `user-id` non-nil. `scope` per `snapshot-scope-card-ids`. `columns` is a
         map of column → value naming ONLY the columns to write; an absent column
         keeps its stored value.
   post: every card in scope holds those values and a `local_current_digest`
         computed from the row as written. Returns the number of cards touched.
   inv : the column write and the digest restamp commit together. Writing the value
         and failing to restamp would leave every card in scope reading synced with
         a stale digest, which no later edit could correct — so the two must never
         be separate statements outside a transaction.
   inv : `updated_at` is NOT bumped. It means \"a user edited this card's content\",
         and a document-wide citation change did not.

   Scope resolution happens INSIDE the transaction, so a re-parent concurrent with
   a citation edit cannot make the fan-out write a scope that no longer holds.

   The push path calls this too, through `refresh-anki-settings-snapshot!`. It used
   to run its own unscoped, untransacted loop of one UPDATE per card; `e/Offload`'s
   latest-wins cancellation could then interrupt that loop mid-way and leave part
   of a run's cards written with stale digests. One transaction makes a cancelled
   attempt roll back instead."
  [user-id scope columns]
  (if (or (nil? user-id) (empty? columns))
    0
    (jdbc/with-transaction [tx ds]
      (let [ids (snapshot-scope-card-ids tx user-id scope)]
        (when (seq ids)
          (doseq [batch (partition-all snapshot-id-batch ids)]
            (jdbc/execute-one! tx
              (sql/format {:update :flashcards
                           :set columns
                           :where (snapshot-scope-predicate user-id [:cards batch])})))
          (restamp-card-digests! tx ids))
        (count ids)))))

(defn set-anki-header-snapshot!
  "Record the header every card under `root-topic-id` will be pushed with.

   pre : `header-text` is the EFFECTIVE text — nil when the note will carry no
         header. The caller owns that derivation, because
         `anki-sync-helpers/effective-header-text` owns the condition and this
         namespace must not require that one.
   post: those cards' digests move iff the header actually changed. Returns the
         count. A nil `user-id` or `root-topic-id` is a caller bug and returns 0.

   Scoped by `root_topic_id` because that is how the header is stored: two
   `settings` rows keyed `anki_use_header_<root-id>` / `anki_header_text_<root-id>`,
   with no account-wide fallback."
  [user-id root-topic-id header-text]
  (if (nil? root-topic-id)
    0
    (write-anki-settings-snapshot! user-id [:root root-topic-id]
      {:anki_header_text header-text})))

(defn set-anki-bibliography-snapshot-for-topics!
  "Record the citation every card under `topic-ids` will be pushed with.

   pre : `bib-html` is the rendered citation body, or nil. `topic-ids` is the exact
         set whose cards cite it — `topic-ids-for-source` produces it from a source
         id, and `push-bibliography-to-descendants!` returns it directly.
   post: those cards hold it, and their digests move iff it changed. Returns the
         count."
  [user-id topic-ids bib-html]
  (let [topic-ids (vec (distinct (remove nil? topic-ids)))]
    (if (empty? topic-ids)
      0
      (write-anki-settings-snapshot! user-id [:topics topic-ids]
        {:anki_bibliography_html bib-html}))))

(defn set-anki-bibliography-snapshot!
  "Record the citation every card whose EFFECTIVE source is `source-id` will be
   pushed with — the shape the three `update-source!` call sites need.

   pre : `bib-html` is the rendered citation body, or nil.
   post: exactly the cards `topic-ids-for-source` reaches hold it. Returns the
         count.

   The walk resolves inside the write transaction (`snapshot-scope-card-ids`), so a
   re-parent concurrent with a citation edit cannot make this write a scope that no
   longer holds."
  [user-id source-id bib-html]
  (if (nil? source-id)
    0
    (write-anki-settings-snapshot! user-id [:source source-id]
      {:anki_bibliography_html bib-html})))

(defn refresh-anki-settings-snapshot!
  "Set both snapshot columns for the cards a push is about to write, then restamp.

   pre : `user-id` non-nil. `entries` = [{:card-id n :header-text s-or-nil
         :bibliography-html s-or-nil} …], resolved by the push from `settings` and
         the source walk.
   post: every named card the user owns holds those values and a digest computed
         from the row as written. Returns the number of cards touched. An id the
         user does not own is silently skipped — ownership is enforced by
         `write-anki-settings-snapshot!`, not by trusting the caller.

   This is what makes a missed fan-out self-healing. Push reads the header and the
   citation from their real homes, so a card whose snapshot went stale sends the
   RIGHT bytes; without this refresh the baseline taken afterwards would be
   computed from the stale snapshot and disagree with what Anki now holds, for
   good.

   Entries are grouped by their VALUE PAIR, so a whole-document push — every card
   under one root sharing one header and one citation — costs one UPDATE, not one
   per card. The migration re-pushes an entire collection by design, which is the
   run where that difference is measured in thousands of statements."
  [user-id entries]
  (let [by-values (group-by (juxt :header-text :bibliography-html)
                    (remove (comp nil? :card-id) entries))]
    (reduce-kv
      (fn [n [header bib] group]
        (+ n (write-anki-settings-snapshot! user-id [:cards (mapv :card-id group)]
               {:anki_header_text header :anki_bibliography_html bib})))
      0 by-values)))

(defn backfill-local-card-digests!
  "Fill `local_current_digest` for every flashcard that has none. Returns the
   number filled.

   pre : the column exists (`setup-schema-flashcards!` has run).
   post: every row this pass could digest holds one. Returns the count.
   inv : restartable and idempotent — it selects only NULL rows, so an
         interrupted run resumes where it stopped and a completed run is a no-op
         costing one indexless count.

   Deliberately does NOT touch `local_baseline_digest`. Leaving the baselines
   alone is what makes the first reconcile after this deploy classify every synced
   card as locally moved, so the collection re-pushes once — the alternative was
   to declare current content to be the baseline, which would write off any
   pre-deploy edit that never reached Anki.

   Paginates by id rather than by `LIMIT` alone, so a row whose digest cannot be
   computed cannot stall the loop. Such a row stays NULL, which reads as unsynced
   against a non-NULL baseline and is therefore repaired by its next push — the
   safe direction, and the reason a failed batch is logged and stepped over
   instead of aborting boot."
  []
  (let [batch-size 2000]
    (loop [filled 0 after 0]
      (let [ids (mapv :id
                  (jdbc/execute! ds
                    ["SELECT id FROM flashcards
                      WHERE local_current_digest IS NULL AND id > ?
                      ORDER BY id LIMIT ?"
                     after batch-size]
                    {:builder-fn rs/as-unqualified-maps}))]
        (if (empty? ids)
          filled
          (let [ok (try
                     (restamp-card-digests! ds ids)
                     (catch Exception e
                       (tel/log! {:level :error
                                  :id ::backfill-local-card-digests-batch
                                  :data {:from (first ids) :to (last ids) :count (count ids)
                                         :error (.getMessage e)}}
                         "skipped a digest backfill batch")
                       0))]
            (recur (+ filled ok) (apply max ids))))))))

(defn insert-flashcards!
  "Batch insert flashcards. Rows should include :topic_id and :root_topic_id.
   Uses ON CONFLICT DO NOTHING to prevent duplicates.
   Returns a vector of the inserted flashcards' ids in insertion order; rows
   skipped by ON CONFLICT DO NOTHING return no id, so the result may be shorter
   than `rows`. Returns nil when `rows` is empty.

   The 2-arity takes the connectable. Pass an open transaction when the cards'
   topics were inserted in that same transaction — on `ds` this runs on a
   different connection, cannot see the uncommitted parents, and fails the
   flashcards_topic_id_fkey constraint.

   Large lists are split across statements by `param-bounded-batches`, so the
   insert is atomic only to the extent `connectable` makes it so: a transaction
   covers every batch, `ds` covers one batch each. Every caller past a single
   batch (collection import) already passes a transaction."
  ([rows] (insert-flashcards! ds rows))
  ([connectable rows]
   (when (seq rows)
     (doseq [row rows]
       (input/check-length! :question (:question row) input/card-max)
       (input/check-length! :answer (:answer row) input/card-max)
       (input/check-length! :cloze (:cloze row) input/card-max))
     ;; :overlapping is a plain map at the call site; convert to jsonb here so
     ;; callers never touch PGobject (same ownership as the io_fields path).
     (let [rows (mapv (fn [row]
                        (cond-> row
                          (some? (:overlapping row)) (update :overlapping ->jsonb)))
                  rows)]
       (let [ids (into []
                   (comp (mapcat (fn [batch]
                                   (jdbc/execute! connectable
                                     (sql/format {:insert-into :flashcards
                                                  :values (vec batch)
                                                  :on-conflict []
                                                  :do-nothing true
                                                  :returning [:id]}))))
                     (map :flashcards/id))
                   (param-bounded-batches rows))]
         ;; One extra select+update per 32k cards, not per card — a new card must
         ;; carry a `local_current_digest` or its first edit cannot be detected.
         (restamp-card-digests! connectable ids)
         ids)))))

;; ── Batch inserts for collection import ────────────────────────────
;; An importer owns values the user never typed — titles from a foreign
;; registry, schedules from a foreign algorithm — so these three bypass the
;; single-row creators (create-topic!, create-source!, log-topic-repetition!)
;; and their prettification, defaulting and per-row audit logging. They take
;; an explicit `tx` so a whole collection commits or rolls back as one unit.

(defn reserve-topic-ids!
  "Draw `n` ids from the topics id sequence without inserting anything.

   Exists so an importer can resolve references BETWEEN rows before writing any
   of them. A documentation build's pages link to each other, so rewriting one
   page's links needs the topic ids of pages not yet inserted; the alternative
   is inserting every row and then UPDATEing its content, which doubles the
   write for a 2000-page build.

   Pre : `n` >= 0.
   Post: a vector of `n` ids, in ascending order, none of them in use. Ids are
         consumed whether or not the caller inserts — a failed import burns
         them, which is what a sequence is for and why this needs no cleanup.
   Invariant: reads through `pg_get_serial_sequence`, so the sequence's name is
         not duplicated here. Runs outside any transaction on purpose: a
         sequence does not roll back, so holding one open would buy nothing."
  [n]
  (if (pos? (long n))
    (mapv (comp val first)
      (jdbc/execute! ds
        ["SELECT nextval(pg_get_serial_sequence('topics','id')) AS id
          FROM generate_series(1, ?)" n]))
    []))

(defn insert-topic-rows!
  "Batch-insert fully-formed topic rows; return their ids in insertion order.
   Pre : `tx` is an open transaction. Every row carries :user_id, :kind,
         :title, :status and :priority. A :parent_id, when present, names a
         topic already committed in `tx` — callers insert breadth-first so a
         parent precedes its children. Titles are already within title-max;
         over-length is a caller bug because only the caller can decide
         whether to truncate or reject.
   Post: vector of ids positionally aligned with `rows`; nil when empty.
   Invariant: no ON CONFLICT — topics have no natural key here, so a conflict
         would mean a malformed row rather than a duplicate.
   Invariant: `param-bounded-batches` splits wide lists across statements, all
         inside `tx`, so the whole depth level still commits or rolls back as
         one unit."
  [tx rows]
  (when (seq rows)
    (doseq [row rows]
      (input/check-length! :title (:title row) input/title-max))
    (into []
      (comp (mapcat (fn [batch]
                      (jdbc/execute! tx
                        (sql/format {:insert-into :topics :values (vec batch)
                                     :returning [:id]}))))
        (map :topics/id))
      (param-bounded-batches rows))))

(defn insert-source-rows!
  "Batch-insert sources rows; return their ids in insertion order.
   Pre : `tx` is an open transaction; each row carries :user_id and :csl_type,
         and :csl is a plain Clojure map.
   Post: vector of ids positionally aligned with `rows`; nil when empty.
   `:csl` is converted to jsonb here so callers never touch PGobject — the
   same ownership rule insert-flashcards! applies to :overlapping."
  [tx rows]
  (when (seq rows)
    (let [rows (mapv #(update % :csl ->jsonb) rows)]
      (into []
        (comp (mapcat (fn [batch]
                        (jdbc/execute! tx
                          (sql/format {:insert-into :sources :values (vec batch)
                                       :returning [:id]}))))
          (map :sources/id))
        (param-bounded-batches rows)))))

(defn insert-topic-repetitions!
  "Batch-append rows to topic_repetitions; return the number inserted.
   Pre : `tx` is an open transaction. Each row carries :topic_id, :user_id,
         :event_type and :event_at. The three snapshot columns
         (:status_before, :interval_days_before, :a_factor_before) MAY be nil
         only when :event_type is \"import\" — a foreign collection records
         when a repetition happened but not the state it replaced. The
         database can no longer enforce this, so it is enforced here.
   Post: row count.
   Invariant: violating the nil rule throws rather than writing an
         unattributable gap into a log the history view presents as fact."
  [tx rows]
  (when (seq rows)
    (doseq [{:keys [event_type status_before interval_days_before a_factor_before] :as row} rows]
      (when (and (not= "import" event_type)
              (or (nil? status_before) (nil? interval_days_before) (nil? a_factor_before)))
        (throw (ex-info "Non-import repetition rows must carry their before-snapshot"
                 {:type ::incomplete-repetition-snapshot :event-type event_type
                  :topic-id (:topic_id row)}))))
    (transduce (map (fn [batch]
                      (count (jdbc/execute! tx
                               (sql/format {:insert-into :topic_repetitions
                                            :values (vec batch)
                                            :returning [:id]})))))
      + 0 (param-bounded-batches rows))))

(defn- strip-foreign-jsonb
  "Remove map entries whose value is an unparsed org.postgresql.util.PGobject.
   These are JSONB columns this branch's schema does not model — e.g. a column
   added by another branch sharing the same database — which are not
   Electric-serializable and must never cross the wire to the client. Modeled
   JSONB is parsed by its own query (pgobject->clj), so it is never raw here.
   Pre:  row is a result-set map, or nil.
   Post: the same map without PGobject-valued entries; nil → nil.
   Blame: environment (schema drift) — a client-render row must not carry a
   column this branch cannot serialize."
  [row]
  (when row
    (into {} (remove (fn [[_ v]] (instance? PGobject v))) row)))

(defn get-flashcards
  "Get all flashcards for a specific topic (page or extract).
   Includes page_number from the topic hierarchy (direct or via parent)."
  [topic-id]
  (if topic-id
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:= :f.topic_id topic-id]
                     :order-by [[:f.created_at :desc]]})))
    []))

(defn get-all-flashcards
  "Get all flashcards under a root topic.
   Includes page_number from the topic hierarchy (direct or via parent)."
  [root-topic-id]
  (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
    (jdbc/execute! ds
      (sql/format {:select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                             card-group-selects)
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                               [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                               [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                   :where [:= :f.root_topic_id root-topic-id]
                   :order-by [[:f.created_at :desc]]}))))

(defn get-user-flashcards
  "Get every flashcard owned by user-id, with topic/root context for the
   Library cards view. Ownership is scoped by the ROOT topic's user_id —
   root topics always carry user_id; child rows may not.
   Returns flashcard columns plus :topic_title, :root_title, :page_number
   (direct or via parent topic) and :formatted_date.
   pgjdbc qualifies plain aliased columns by their base table (here both
   titles → :topics/*), unlike expression columns — rename to the
   unqualified keys this fn promises."
  [user-id]
  (mapv (fn [row]
          (-> row
            normalize-flashcard-row
            strip-foreign-jsonb
            (assoc :topic_title (:topics/topic_title row)
              :root_title (:topics/root_title row))
            (dissoc :topics/topic_title :topics/root_title)))
    (jdbc/execute! ds
      (sql/format {:select (into [[:f.*]
                                  [[:coalesce :t.page_number :parent.page_number] :page_number]
                                  [:t.title :topic_title]
                                  [:root.title :root_title]
                                  [[:to_char :f.created_at [:inline "Mon DD"]] :formatted_date]]
                             card-group-selects)
                   :from [[:flashcards :f]]
                   :join [[:topics :t] [:= :f.topic_id :t.id]
                          [:topics :root] [:= :f.root_topic_id :root.id]]
                   :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                               [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                               [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                   :where [:= :root.user_id user-id]
                   :order-by [[:f.created_at :desc]]}))))

(defn get-pushed-card-manifest
  "Card-id/note-id pairs for every pushed card the user owns (anki_note_id NOT
   NULL). Filter-independent — this is the Anki overlay's fetch set, so it must
   not narrow with the /library/cards filters. Empty → []."
  [user-id]
  (mapv (fn [r] {:card-id (:flashcards/id r) :note-id (:flashcards/anki_note_id r)})
    (jdbc/execute! ds
      (sql/format {:select [:f.id :f.anki_note_id]
                   :from [[:flashcards :f]]
                   :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                   :where [:and
                           [:= :root.user_id user-id]
                           [:not= :f.anki_note_id nil]]}))))

(defn get-flashcards-by-ids
  "Flashcards owned by user-id (via root topic) with ids in card-ids.
   Full rows plus :page_number — the inputs for bulk push and bulk pull. Empty
   ids → []. The Anki push resolves its own page locator through
   resolve-pdf-page-numbers, which walks the whole ancestor chain; this
   one-level coalesce has no reader left in that path."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:select (into [[:f.*]
                                    [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]
                            [:topics :root] [:= :f.root_topic_id :root.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:and
                             [:= :root.user_id user-id]
                             [:in :f.id (vec card-ids)]]})))))

(defn- fully-deleted-groups-of
  "Rows of `group-table` that deleting `card-ids` would leave with no member.
   fk-col is the flashcards column referencing that table (unqualified keyword).
   Pre: card-ids non-empty."
  [user-id card-ids group-table fk-col]
  (let [ids (vec card-ids)
        f-fk (keyword "f" (name fk-col))
        f2-fk (keyword "f2" (name fk-col))]
    (jdbc/execute! ds
      (sql/format {:select [:g.*]
                   :from [[group-table :g]]
                   :where [:and
                           ;; at least one member is going away, and it is ours
                           [:exists {:select [1]
                                     :from [[:flashcards :f]]
                                     :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                                     :where [:and
                                             [:= :root.user_id user-id]
                                             [:= f-fk :g.id]
                                             [:in :f.id ids]]}]
                           ;; and none survives
                           [:not [:exists {:select [1]
                                           :from [[:flashcards :f2]]
                                           :where [:and
                                                   [:= f2-fk :g.id]
                                                   [:not-in :f2.id ids]]}]]]}))))

(defn fully-deleted-group-rows
  "Occlusion and score group rows that deleting `card-ids` would destroy — the
   groups every one of whose flashcard rows is in the set.
   Pre:  called BEFORE the delete. remove-occlusion-mask! and
         remove-score-card-cleanup! drop a group row once its last member is
         gone, so afterwards there is nothing left to snapshot.
   Post: {:occlusion-groups [row ...] :score-groups [row ...]}, with qualified
         keys and PGobject geometry exactly as jdbc returns them —
         insert-card-delete-undo! owns the unqualifying and JSONB parsing.
         A group with a surviving member is ABSENT: a partial delete leaves its
         group standing, which remove-occlusion-mask! then reconciles.
   Ownership is scoped through the root topic, matching delete-user-flashcards!,
   so an id the caller does not own contributes nothing."
  [user-id card-ids]
  (if (empty? card-ids)
    {:occlusion-groups [] :score-groups []}
    {:occlusion-groups (fully-deleted-groups-of user-id card-ids :occlusion_groups :occlusion_group_id)
     :score-groups (fully-deleted-groups-of user-id card-ids :score_groups :score_group_id)}))

(defn get-cards-for-reconcile
  "Flashcards `user-id` owns, carrying the columns the reconcile digest reads
   plus both baselines. The 1-arity reads the whole collection (the run's
   manifest); the 2-arity narrows to `card-ids` (re-reading the few a write
   touched), because scanning the collection to digest three rows is the same
   query run once per phase.

   JSONB is parsed here. `anki-digest/local-digest` THROWS on a raw PGobject
   rather than digesting nothing, so a query that skipped this would fail the
   run loudly on the first occlusion or overlapping card.

   The content columns come from `digest-columns`, so a column the digest starts
   covering cannot be forgotten here. That mattered the moment the digest gained
   `anki_header_text` — a hand-listed select would have digested it as \"\" and
   quietly stopped detecting header changes, with nothing failing."
  ([user-id] (get-cards-for-reconcile user-id nil))
  ([user-id card-ids]
   (if (and (some? card-ids) (empty? card-ids))
     []
     (mapv parse-digest-jsonb
       (jdbc/execute! ds
         (sql/format {:select (into digest-columns
                                [:f.anki_note_id :f.anki_baseline_digest
                                 :f.local_baseline_digest
                                 :f.baseline_digest_version])
                      :from [[:flashcards :f]]
                      :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                      :where (cond-> [:and [:= :root.user_id user-id]]
                               (seq card-ids) (conj [:in :f.id (vec card-ids)]))}))))))

(defn set-anki-conflicts!
  "Make `user-id`'s stored conflict set exactly `card-ids`.

   pre : `card-ids` came from one run over the whole collection.
   post: those cards carry `anki_conflict_at`; every other card of this user
         carries NULL. Clearing the rest is the point — a card that stopped
         being a conflict must stop reading as one without a second action."
  [user-id card-ids]
  (jdbc/with-transaction [tx ds]
    (let [owned {:select [:id] :from [:topics] :where [:= :user_id user-id]}]
      (jdbc/execute! tx
        (sql/format {:update :flashcards
                     :set {:anki_conflict_at nil}
                     :where (cond-> [:and
                                     [:in :root_topic_id owned]
                                     [:not= :anki_conflict_at nil]]
                              (seq card-ids) (conj [:not-in :id (vec card-ids)]))}))
      (when (seq card-ids)
        (jdbc/execute! tx
          (sql/format {:update :flashcards
                       :set {:anki_conflict_at [:now]}
                       :where [:and
                               [:in :root_topic_id owned]
                               [:in :id (vec card-ids)]
                               [:= :anki_conflict_at nil]]}))))))

(defn clear-anki-conflicts!
  "Drop the conflict flag from `card-ids` only, leaving every other card's alone.
   Distinct from `set-anki-conflicts!`, which makes the whole set exact: that one
   belongs to a run over the collection, this one to resolving a few by hand."
  [user-id card-ids]
  (when (seq card-ids)
    (jdbc/execute-one! ds
      (sql/format {:update :flashcards
                   :set {:anki_conflict_at nil}
                   :where [:and
                           [:in :root_topic_id
                            {:select [:id] :from [:topics] :where [:= :user_id user-id]}]
                           [:in :id (vec card-ids)]]}))))

(defn list-anki-conflicts
  "Card ids `user-id` currently has flagged as reconcile conflicts, oldest
   first. The Library filter and the run toast read the same set."
  [user-id]
  (mapv :flashcards/id
    (jdbc/execute! ds
      (sql/format {:select [:f.id]
                   :from [[:flashcards :f]]
                   :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                   :where [:and [:= :root.user_id user-id]
                           [:not= :f.anki_conflict_at nil]]
                   :order-by [[:f.anki_conflict_at :asc]]}))))

(defn set-card-baselines!
  "Write both reconcile baselines for `user-id`'s cards, in ONE statement per
   card.

   pre : `entries` = [{:card-id n :anki-digest s :local-digest s :anki-mod n?} …].
   post: both digests move together. Writing them in two statements would let a
         crash between the two leave a half-written baseline, which
         `anki-reconcile/classify` must then read as unknown — see its
         `:needs-baseline` branch.
   Also stamps `anki_synced_at`, which is now a display timestamp only — no
   staleness predicate reads it.

   `local_current_digest` is set to the SAME value as `local_baseline_digest`, in
   the same statement. The caller derived `local-digest` from the row it just
   wrote, so the two already agree; writing both makes the card read synced at
   every surface as an invariant of this fn rather than as a consequence of
   whoever restamped last.

   `baseline_digest_version` is stamped here, which is why it is stamped exactly
   once: this is the only writer of the two baselines, and all four callers —
   `record-anki-writes!`, `apply-pulls!`, `backfill-baselines!` and
   `resolve-conflicts!` — reach the columns through it. A caller that wrote a
   baseline without the version would leave a row that re-baselines on every
   subsequent run, forever."
  [user-id entries]
  (when (seq entries)
    (jdbc/with-transaction [tx ds]
      (doseq [{:keys [card-id anki-digest local-digest anki-mod]} entries]
        (jdbc/execute-one! tx
          (sql/format {:update :flashcards
                       :set (cond-> {:anki_baseline_digest anki-digest
                                     :local_baseline_digest local-digest
                                     :local_current_digest local-digest
                                     :baseline_digest_version digest/digest-version
                                     :anki_synced_at [:now]}
                              anki-mod (assoc :anki_baseline_mod anki-mod))
                       :where [:and
                               [:= :id card-id]
                               [:in :root_topic_id
                                {:select [:id] :from [:topics]
                                 :where [:= :user_id user-id]}]]}))))
    (count entries)))

(defn list-anki-tombstones
  "Every outstanding tombstone for `user-id`, oldest first.
   post: rows with :anki_note_tombstones/anki_note_id and /anki_baseline_digest."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [:*] :from [:anki_note_tombstones]
                 :where [:= :user_id user-id]
                 :order-by [[:deleted_at :asc]]})))

(defn drop-anki-tombstones!
  "Forget tombstones whose work is finished — the note is gone from Anki.
   pre : `note-ids` were either deleted from Anki, or found already absent.
   post: those rows no longer exist for `user-id`. Returns the count removed."
  [user-id note-ids]
  (if (empty? note-ids)
    0
    (or (::jdbc/update-count
         (jdbc/execute-one! ds
           (sql/format {:delete-from :anki_note_tombstones
                        :where [:and [:= :user_id user-id]
                                [:in :anki_note_id (vec note-ids)]]})))
      0)))

(defn delete-user-flashcards!
  "Bulk delete of user-id's flashcards. Ownership enforced via the root
   topic join. Returns the full deleted rows (RETURNING flashcards.*) so
   callers can both read :flashcards/anki_note_id and snapshot the rows for
   undo. Tombstones every synced row in the same transaction."
  [user-id card-ids]
  (if (empty? card-ids)
    []
    (jdbc/with-transaction [tx ds]
      (let [deleted (jdbc/execute! tx
                      (sql/format {:delete-from :flashcards
                                   :using [[:topics :root]]
                                   :where [:and
                                           [:= :flashcards.root_topic_id :root.id]
                                           [:= :root.user_id user-id]
                                           [:in :flashcards.id (vec card-ids)]]
                                   :returning [:flashcards.*]}))]
        (record-anki-tombstones! tx user-id deleted)
        deleted))))

(defn get-flashcards-by-anki-note-ids
  "Flashcards owned by user-id (via root topic) whose anki_note_id is in
   note-ids. Returns id, kind, content fields, and anki_note_id — the diff
   inputs for the Anki overlay. Empty note-ids → []."
  [user-id note-ids]
  (if (empty? note-ids)
    []
    (jdbc/execute! ds
      (sql/format {:select [:f.id :f.kind :f.question :f.answer :f.cloze :f.anki_note_id]
                   :from [[:flashcards :f]]
                   :join [[:topics :root] [:= :f.root_topic_id :root.id]]
                   :where [:and
                           [:= :root.user_id user-id]
                           [:in :f.anki_note_id (vec note-ids)]]}))))

(defn delete-flashcard!
  "Delete flashcard `card-id` iff owned by `user-id` (via its root topic).
   Returns the full deleted row (RETURNING *) so callers can read
   :flashcards/anki_note_id and snapshot it for undo, or nil when the card is
   not the user's (0 rows — the ownership predicate is the security gate).
   Tombstones the row's Anki note in the same transaction."
  [user-id card-id]
  (jdbc/with-transaction [tx ds]
    (let [deleted (jdbc/execute-one! tx
                    ["DELETE FROM flashcards
                      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
                      RETURNING *" card-id user-id])]
      (when deleted (record-anki-tombstones! tx user-id [deleted]))
      deleted)))

(defn update-flashcard!
  "Update content fields of flashcard `card-id` iff owned by `user-id` (via its
   root topic). Sets updated_at for sync tracking. :io_fields (a plain map) is
   converted to JSONB here — callers never touch PGobject. No-op (0 rows) when
   the card is not the user's.

   Transactional so the content write and the `local_current_digest` restamp
   commit together. A crash between them would leave an already-pushed card
   reading synced with changed content — see `restamp-card-digests!`."
  [user-id card-id fields]
  (jdbc/with-transaction [tx ds]
    (let [result (jdbc/execute-one! tx
                   (sql/format {:update :flashcards
                                :set (cond-> (assoc fields :updated_at [:now])
                                       (contains? fields :io_fields) (update :io_fields ->jsonb)
                                       (contains? fields :overlapping) (update :overlapping ->jsonb))
                                :where [:and
                                        [:= :id card-id]
                                        [:in :root_topic_id {:select :id :from :topics
                                                             :where [:= :user_id user-id]}]]}))]
      ;; Only on a row that was actually the user's — a 0-row update must not
      ;; restamp a foreign card, harmless though that write would be.
      (when (pos? (or (:next.jdbc/update-count result) 0))
        (restamp-card-digests! tx [card-id]))
      result)))

;; ---------------------------------------------------------------------------
;; Card edit history — write-behind version log (plans/card-edit-history.md).
;; Every write here logs the rendition it REPLACED. Deliberately NOT folded
;; into update-flashcard!: the Anki pull path shares that fn
;; (anki_sync_server/apply-pull-updates!) and pulls must not version.
;; ---------------------------------------------------------------------------

(def ^:private card-content-columns
  "Flashcard columns that together constitute a card-scoped rendition."
  [:question :answer :cloze :overlapping])

(defn- any-changed?
  "True when any [stored, incoming] pair differs — the single change-detection
   rule for every versioned write path (card fields, occlusion group, score
   group). Stored values may be PGobjects; they are parsed first, so JSONB
   compares by value and key order is not significant. Non-PGobject values pass
   through pgobject->clj untouched, so TEXT and integer columns use the same
   path.

   Pre:  `pairs` is a seq of 2-element [stored incoming] tuples.
   Post: numeric widening (10 vs 10.0) reads as a change. That direction is
         deliberate — a false positive costs one redundant version row, a false
         negative would silently lose history."
  [pairs]
  (boolean (some (fn [[stored incoming]] (not= (pgobject->clj stored) incoming))
             pairs)))

(defn- card-content-changed?
  "any-changed? for a flashcard row: resolves each key of `fields` against
   `old`'s namespace-qualified keys.
   Pre: `fields` keys ⊆ card-content-columns (asserted by the caller)."
  [old fields]
  (any-changed?
    (map (fn [[k v]] [(get old (keyword "flashcards" (name k))) v]) fields)))

(defn- insert-card-version!
  "Insert one card_versions row. `payload` carries the scope keys and the
   non-null rendition columns; JSONB values MUST already be PGobjects (passed
   through verbatim from the row being superseded) or Clojure values wrapped by
   the caller. Returns nil."
  [tx payload]
  (jdbc/execute-one! tx (sql/format {:insert-into :card_versions :values [payload]}))
  nil)

(defn update-flashcard-versioned!
  "Log the superseded rendition, then apply `fields` to flashcard `card-id`.
   The versioning counterpart of update-flashcard!, for user-initiated saves.

   Pre:  `fields` keys ⊆ card-content-columns (ASSERTED — a wider key would be
         compared against a column this fn never selected, so it would read as
         changed on every save and its value would be written unwrapped);
         :overlapping is a plain map.
   Post: when `fields` would change at least one value, exactly one
         card_versions row holds the FULL pre-edit rendition (all four content
         columns, verbatim), and the card holds the new values — atomically.
         An unchanged save writes no version row and still bumps updated_at, so
         updated_at does NOT imply the card has history.
   Inv:  card_versions never holds the current rendition.
   No-op (0 rows, no version) when the card is not `user-id`'s.
   Returns {:updated? bool :versioned? bool}."
  [user-id card-id fields]
  (assert (every? (set card-content-columns) (keys fields))
    (str "update-flashcard-versioned! got non-content keys "
      (vec (remove (set card-content-columns) (keys fields)))))
  (jdbc/with-transaction [tx ds]
    (if-let [old (jdbc/execute-one! tx
                   ["SELECT id, kind, root_topic_id, question, answer, cloze, overlapping
                     FROM flashcards
                     WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
                     FOR UPDATE"
                    card-id user-id])]
      (let [changed? (card-content-changed? old fields)]
        (when changed?
          (insert-card-version! tx
            (into {:root_topic_id (:flashcards/root_topic_id old)
                   :scope_type "card"
                   :scope_id card-id
                   :kind (:flashcards/kind old)}
              (map (fn [k] [k (get old (keyword "flashcards" (name k)))]))
              card-content-columns)))
        (jdbc/execute-one! tx
          (sql/format {:update :flashcards
                       :set (cond-> (assoc fields :updated_at [:now])
                              (contains? fields :overlapping) (update :overlapping ->jsonb))
                       :where [:= :id card-id]}))
        ;; Same transaction as the version row and the content write, so the
        ;; digest can never describe a rendition that was rolled back.
        (restamp-card-digests! tx [card-id])
        {:updated? true :versioned? changed?})
      {:updated? false :versioned? false})))

(def max-versions-read
  "Read cap for one history view. Retention is unbounded by design, so without
   a cap a pathologically-edited card would materialize its entire log server-
   side. Callers MUST surface truncation when a result hits this count — a
   silent cap would read as 'this is the whole history'."
  500)

(defn- versions-for-scopes
  "Rows for the given scope predicates, newest first, capped at
   max-versions-read.
   Pre:  `scopes` is a non-empty seq of HoneySQL predicates over
         (scope_type, scope_id); ownership is ALREADY enforced by the caller —
         this fn does no authorization.
   Post: rows are plain data with UNQUALIFIED keys and JSONB parsed
         (Electric-wire safe — no PGobject, and no OffsetDateTime: the
         timestamp crosses only as the preformatted :superseded_label).
         A result of exactly max-versions-read means older versions exist."
  [scopes]
  (mapv (fn [row]
          (-> (update-keys row (comp keyword name))
            (update :overlapping pgobject->clj)
            (update :io_fields pgobject->clj)
            (update :geometry pgobject->clj)))
    (jdbc/execute! ds
      (sql/format {:select [:id :scope_type :kind
                            [[:to_char :superseded_at "YYYY-MM-DD HH24:MI"] :superseded_label]
                            :question :answer :cloze :overlapping :io_fields :geometry
                            :occlusion_mode :occlusion_image_media_id
                            :score_direction :score_start_ms :score_end_ms]
                   :from [:card_versions]
                   :where (into [:or] scopes)
                   :order-by [[:superseded_at :desc] [:id :desc]]
                   :limit max-versions-read}))))

(defn get-card-versions
  "Superseded renditions visible for flashcard `card-id`, newest first.
   Unions the card's own versions with those of the group it belongs to, so an
   occlusion card shows every group edit — including the geometry that still
   contained a mask this card no longer has.
   Pre:  card owned by `user-id` (enforced; a foreign card yields []).
   Returns a vector of rows, [] when the card has no history."
  [user-id card-id]
  (if-let [card (jdbc/execute-one! ds
                  ["SELECT occlusion_group_id, score_group_id FROM flashcards
                    WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
                   card-id user-id])]
    (versions-for-scopes
      (cond-> [[:and [:= :scope_type "card"] [:= :scope_id card-id]]]
        (:flashcards/occlusion_group_id card)
        (conj [:and [:= :scope_type "occlusion_group"]
               [:= :scope_id (:flashcards/occlusion_group_id card)]])
        (:flashcards/score_group_id card)
        (conj [:and [:= :scope_type "score_group"]
               [:= :scope_id (:flashcards/score_group_id card)]])))
    []))

(defn get-occlusion-group-versions
  "Superseded renditions of occlusion group `group-id`, newest first — the
   group-keyed entry point, for the occlusion editor which knows a group but no
   single card.
   Pre:  group owned by `user-id` — enforced here via the group's cards, which
         carry root_topic_id (occlusion_groups has no user column). A foreign
         group, or one whose last card is gone, yields [].
   Returns a vector of rows, [] when the group has never been edited."
  [user-id group-id]
  (if (jdbc/execute-one! ds
        ["SELECT 1 FROM flashcards
          WHERE occlusion_group_id = ?
            AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)
          LIMIT 1"
         group-id user-id])
    (versions-for-scopes [[:and [:= :scope_type "occlusion_group"]
                           [:= :scope_id group-id]]])
    []))

(defn get-anki-note-ids
  "Get anki_note_ids for a specific topic's flashcards."
  [topic-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE topic_id = ? AND anki_note_id IS NOT NULL"
          topic-id])
    (mapv :flashcards/anki_note_id)))

(defn get-all-anki-note-ids
  "Get all anki_note_ids under a root topic."
  [root-topic-id]
  (->> (jdbc/execute! ds
         ["SELECT anki_note_id FROM flashcards WHERE root_topic_id = ? AND anki_note_id IS NOT NULL"
          root-topic-id])
    (mapv :flashcards/anki_note_id)))

(defn set-anki-note-id
  "Set anki_note_id and anki_synced_at for a `user-id`-owned flashcard."
  [user-id card-id anki-note-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_note_id = ?, anki_synced_at = CURRENT_TIMESTAMP
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     anki-note-id card-id user-id]))

(defn set-anki-note-ids
  "Bulk set anki_note_id + anki_synced_at for `user-id`'s cards.
   Takes [[card-id note-id] ...]."
  [user-id pairs]
  (doseq [[card-id note-id] pairs]
    (set-anki-note-id user-id card-id note-id)))

(defn mark-anki-synced
  "Update anki_synced_at to now for a `user-id`-owned flashcard."
  [user-id card-id]
  (jdbc/execute-one! ds
    ["UPDATE flashcards SET anki_synced_at = CURRENT_TIMESTAMP
      WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     card-id user-id]))

(defn merge-flashcard-io-fields!
  "Shallow-merge a partial map into a card's io_fields (JSONB ||) and mark it
   synced — the Anki-pull write path for occlusion text fields. Unchanged
   keys keep their local values; updated_at is untouched so the row does not
   read as locally modified.

   The digest IS restamped, unlike `updated_at`. A pull that lands Anki's text
   makes the row's content differ from what the last sync agreed on, and the
   baseline stamp that follows is what records the new agreement — leaving the
   current digest stale would make the card read as changed against its own new
   baseline."
  [user-id card-id partial-fields]
  (jdbc/with-transaction [tx ds]
    (let [result (jdbc/execute-one! tx
                   ["UPDATE flashcards
                     SET io_fields = COALESCE(io_fields, '{}'::jsonb) || ?,
                         anki_synced_at = CURRENT_TIMESTAMP
                     WHERE id = ? AND root_topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
                    (->jsonb partial-fields) card-id user-id])]
      (when (pos? (or (:next.jdbc/update-count result) 0))
        (restamp-card-digests! tx [card-id]))
      result)))

;; ---------------------------------------------------------------------------
;; Occlusion groups — geometry lives here, one 'occlusion' flashcard row per
;; MASK GROUP. Ordinal contract: an ordinal identifies a mask group, so several
;; rects MAY share one and sharing it IS the grouping (freememo.occlusion-ordinals
;; owns the algebra). Ordinals are assigned from the group's next_ordinal and
;; never reused — they bind row ↔ Anki note ↔ SVG rect id ↔ media filename.
;; All geometry writes are transactional with their row changes.
;; ---------------------------------------------------------------------------

(defn- parse-occlusion-group [row]
  (some-> row (update :occlusion_groups/geometry pgobject->clj)))

(defn get-occlusion-group
  "Group row with parsed :occlusion_groups/geometry, or nil."
  [group-id]
  (parse-occlusion-group
    (jdbc/execute-one! ds
      ["SELECT * FROM occlusion_groups WHERE id = ?" group-id])))

(defn get-occlusion-groups-by-ids
  "Map of group-id -> group row (geometry parsed). Empty ids → {}."
  [group-ids]
  (if (empty? group-ids)
    {}
    (into {}
      (map (fn [row]
             (let [row (parse-occlusion-group row)]
               [(:occlusion_groups/id row) row])))
      (jdbc/execute! ds
        (sql/format {:select [:*]
                     :from [:occlusion_groups]
                     :where [:in :id (vec group-ids)]})))))

(defn get-occlusion-cards
  "Flashcard rows of a group ordered by mask_ordinal, io_fields parsed."
  [group-id]
  (mapv normalize-flashcard-row
    (jdbc/execute! ds
      (sql/format {:select [:*]
                   :from [:flashcards]
                   :where [:= :occlusion_group_id group-id]
                   :order-by [[:mask_ordinal :asc]]}))))

(defn insert-occlusion-group!
  "Insert a group plus one 'occlusion' flashcard row per MASK GROUP, transactionally.
   attrs = {:topic-id :root-topic-id :image-media-id :mode :geometry :io-fields}
   Pre:  geometry = {:width :height :rects [{:x :y :w :h (:gid)} ...]}; rects
         sharing a :gid are one mask group (no :ordinal exists yet on create).
   Post: ordinals are assigned HERE, from 1, one per mask group — the group row
         stays the single ordinal authority — and next_ordinal points past them;
         stored rects carry :ordinal and no :gid.
   Returns {:group-id id :ids [flashcard-id ...]}, one id per mask group in
   first-appearance order."
  [{:keys [topic-id root-topic-id image-media-id mode geometry io-fields]}]
  (jdbc/with-transaction [tx ds]
    (let [[rects next-ordinal] (ord/assign-ordinals (vec (:rects geometry)) 1)
          group (jdbc/execute-one! tx
                  (sql/format {:insert-into :occlusion_groups
                               :values [{:anki_key (str (java.util.UUID/randomUUID))
                                         :image_media_id image-media-id
                                         :mode mode
                                         :geometry (->jsonb (assoc geometry :rects rects))
                                         :next_ordinal next-ordinal}]
                               :returning [:id]}))
          group-id (:occlusion_groups/id group)
          ids (mapv (fn [ordinal]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id topic-id
                                                :root_topic_id root-topic-id
                                                :kind "occlusion"
                                                :occlusion_group_id group-id
                                                :mask_ordinal ordinal
                                                :io_fields (->jsonb (or io-fields {}))}]
                                      :returning [:id]}))))
                (ord/ordinals-in-order rects))]
      {:group-id group-id :ids ids})))

(defn reconcile-occlusion-group!
  "Apply a group edit in one transaction (full reconcile).
   attrs = {:group-id :mode :geometry :io-fields}
   Pre:  rects sharing an :ordinal are one mask group; rects sharing a :gid are
         one NEW mask group. An :ordinal naming a row this group no longer has
         is tolerated — the modal's snapshot goes stale whenever the card is
         deleted elsewhere while it is open.
   Post: rects with a live :ordinal are kept (position/size updated) and add no
         row; each distinct :gid mints one ordinal from next_ordinal and gets one
         row; rows whose ordinal is absent from the incoming rects are deleted;
         rects naming a dead ordinal are DROPPED, so a concurrent card deletion
         wins over the stale snapshot instead of persisting a rect no card covers.
   io-fields overwrite every surviving row, and every surviving row gets
   updated_at=now — geometry dirtiness is group-scoped because hide-all
   question masks embed the whole rect set.
   Returns {:group-id .. :added-ids [..]
            :removed [{:id .. :anki-note-id ..} ..]}."
  [{:keys [group-id mode geometry io-fields]}]
  (jdbc/with-transaction [tx ds]
    (let [group (jdbc/execute-one! tx
                  ["SELECT * FROM occlusion_groups WHERE id = ? FOR UPDATE" group-id])]
      (when-not group
        (throw (ex-info "Occlusion group not found" {:group-id group-id})))
      (let [next0 (:occlusion_groups/next_ordinal group)
            ;; ORDER BY mask_ordinal: `template` below supplies the io_fields
            ;; that get LOGGED as history and drives the change comparison, and
            ;; an Anki pull can diverge io_fields per row
            ;; (merge-flashcard-io-fields!). Unordered, which row Postgres
            ;; returned first would decide what history records. Lowest ordinal
            ;; matches get-group-for-edit's documented choice.
            existing (jdbc/execute! tx
                       ["SELECT id, mask_ordinal, anki_note_id, anki_baseline_digest,
                                topic_id, root_topic_id, io_fields
                         FROM flashcards WHERE occlusion_group_id = ?
                         ORDER BY mask_ordinal ASC" group-id])
            existing-ordinals (set (map :flashcards/mask_ordinal existing))
            ;; Re-apply deletions that landed since the modal loaded: a rect
            ;; whose ordinal has no row belongs to a card someone deleted.
            {live-rects true stale-rects false}
            (group-by (fn [rect] (or (nil? (:ordinal rect))
                                   (contains? existing-ordinals (:ordinal rect))))
              (:rects geometry))
            [rects next'] (ord/assign-ordinals (vec live-rects) next0)
            kept-ordinals (set (map :ordinal rects))
            added-ordinals (filterv #(>= % next0) (ord/ordinals-in-order rects))
            template (first existing)
            removed (filterv #(not (contains? kept-ordinals (:flashcards/mask_ordinal %)))
                      existing)
            ;; Superseded rendition, captured before any write. The old geometry
            ;; still carries the rects of masks this edit removes, so a removed
            ;; mask stays viewable even though its flashcard row is deleted
            ;; below and card_versions is scoped to the GROUP, not the row.
            geometry' (assoc geometry :rects rects)
            io-fields' (or io-fields {})
            group-changed? (any-changed?
                             [[(:occlusion_groups/geometry group) geometry']
                              [(:occlusion_groups/mode group) mode]
                              [(:flashcards/io_fields template) io-fields']])]
        (when (seq stale-rects)
          (tel/log! {:level :warn :id ::stale-occlusion-rects
                     :data {:group-id group-id
                            :ordinals (vec (distinct (keep :ordinal stale-rects)))}}
            "Dropped occlusion rects whose card was deleted elsewhere"))
        (when (empty? rects)
          (throw (ex-info "Every mask in this edit was deleted elsewhere — reopen the group"
                   {:group-id group-id})))
        ;; History is logged AFTER the guards: an edit that aborts must not
        ;; leave a version row claiming a rendition was superseded.
        (when (and group-changed? template)
          (insert-card-version! tx
            {:root_topic_id (:flashcards/root_topic_id template)
             :scope_type "occlusion_group"
             :scope_id group-id
             :kind "occlusion"
             :geometry (:occlusion_groups/geometry group)
             :occlusion_mode (:occlusion_groups/mode group)
             :occlusion_image_media_id (:occlusion_groups/image_media_id group)
             :io_fields (:flashcards/io_fields template)}))
        (doseq [r removed]
          (jdbc/execute-one! tx ["DELETE FROM flashcards WHERE id = ?" (:flashcards/id r)]))
        ;; The caller hands `:removed`'s note ids to `try-delete-anki-notes!`
        ;; (occlusion_modal.cljc:432), which is fire-and-forget. The owner is not
        ;; an argument here, so it is resolved from the group's root topic —
        ;; inside the same transaction, before anything can move.
        (when-some [owner (:user_id (jdbc/execute-one! tx
                                      ["SELECT user_id FROM topics WHERE id = ?"
                                       (:flashcards/root_topic_id template)]
                                      {:builder-fn rs/as-unqualified-maps}))]
          (record-anki-tombstones! tx owner removed))
        (jdbc/execute! tx
          (sql/format {:update :flashcards
                       :set {:io_fields (->jsonb io-fields')
                             :updated_at [:now]}
                       :where [:= :occlusion_group_id group-id]}))
        (let [added-ids
              (mapv (fn [ordinal]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id (:flashcards/topic_id template)
                                                :root_topic_id (:flashcards/root_topic_id template)
                                                :kind "occlusion"
                                                :occlusion_group_id group-id
                                                :mask_ordinal ordinal
                                                :io_fields (->jsonb io-fields')}]
                                      :returning [:id]}))))
                added-ordinals)]
          (jdbc/execute-one! tx
            (sql/format {:update :occlusion_groups
                         :set {:mode mode
                               :geometry (->jsonb geometry')
                               :next_ordinal next'
                               :updated_at [:now]}
                         :where [:= :id group-id]}))
          {:group-id group-id
           :added-ids added-ids
           :removed (mapv (fn [r] {:id (:flashcards/id r)
                                   :anki-note-id (:flashcards/anki_note_id r)})
                      removed)})))))

(defn remove-occlusion-mask!
  "Cleanup after a single occlusion flashcard row was deleted elsewhere:
   retire its rect (ordinal never reused), dirty the sibling rows so the next
   push regenerates the group's masks, and delete the group once no rows
   remain. Returns {:group-deleted? bool} (nil when the group is gone)."
  [group-id ordinal]
  (jdbc/with-transaction [tx ds]
    (when-let [group (jdbc/execute-one! tx
                       ["SELECT * FROM occlusion_groups WHERE id = ? FOR UPDATE" group-id])]
      (let [geometry (pgobject->clj (:occlusion_groups/geometry group))
            rects' (vec (remove #(= ordinal (:ordinal %)) (:rects geometry)))
            remaining (:n (jdbc/execute-one! tx
                            ["SELECT COUNT(*) AS n FROM flashcards WHERE occlusion_group_id = ?"
                             group-id]))]
        (if (zero? remaining)
          (do (jdbc/execute-one! tx ["DELETE FROM occlusion_groups WHERE id = ?" group-id])
            {:group-deleted? true})
          (do (jdbc/execute-one! tx
                (sql/format {:update :occlusion_groups
                             :set {:geometry (->jsonb (assoc geometry :rects rects'))
                                   :updated_at [:now]}
                             :where [:= :id group-id]}))
            (jdbc/execute! tx
              (sql/format {:update :flashcards
                           :set {:updated_at [:now]}
                           :where [:= :occlusion_group_id group-id]}))
            {:group-deleted? false}))))))

;; ---------------------------------------------------------------------------
;; Score groups — one (audio segment × notation rects) pair on a kind='score'
;; topic, fanning out to one 'score' flashcard row per direction. Clip and
;; per-rect crops are pre-materialized media rows; the group only references
;; them. Unlike occlusion there is no ordinal authority: rects don't bind to
;; individual cards, so an edit replaces geometry wholesale.
;; ---------------------------------------------------------------------------

(defn- parse-score-group [row]
  (some-> row (update :score_groups/geometry pgobject->clj)))

(defn get-score-group
  "Group row with parsed :score_groups/geometry, or nil."
  [group-id]
  (parse-score-group
    (jdbc/execute-one! ds
      ["SELECT * FROM score_groups WHERE id = ?" group-id])))

(defn get-score-groups-by-ids
  "Map of group-id -> group row (geometry parsed). Empty ids → {}."
  [group-ids]
  (if (empty? group-ids)
    {}
    (into {}
      (map (fn [row]
             (let [row (parse-score-group row)]
               [(:score_groups/id row) row])))
      (jdbc/execute! ds
        (sql/format {:select [:*]
                     :from [:score_groups]
                     :where [:in :id (vec group-ids)]})))))

(defn get-score-cards
  "Flashcard rows of a score group, audio-front first."
  [group-id]
  (jdbc/execute! ds
    (sql/format {:select [:*]
                 :from [:flashcards]
                 :where [:= :score_group_id group-id]
                 :order-by [[:score_direction :asc]]})))

(defn insert-score-group!
  "Insert a group plus one 'score' flashcard row per direction, transactionally.
   attrs = {:topic-id :root-topic-id :start-ms :end-ms :clip-media-id
            :geometry :directions [\"audio-front\" ...]}
   Rect ordinals are assigned HERE (1..N across pages in page order) — they
   index the crop stack on the card and name the Anki crop files.
   Returns {:group-id id :ids [flashcard-id ...]}."
  [{:keys [topic-id root-topic-id start-ms end-ms clip-media-id geometry directions]}]
  (jdbc/with-transaction [tx ds]
    (let [pages (vec (:pages geometry))
          pages' (first
                   (reduce (fn [[acc n] page]
                             (let [rects (mapv (fn [rect ordinal] (assoc rect :ordinal ordinal))
                                           (:rects page) (iterate inc n))]
                               [(conj acc (assoc page :rects rects))
                                (+ n (count rects))]))
                     [[] 1] pages))
          group (jdbc/execute-one! tx
                  (sql/format {:insert-into :score_groups
                               :values [{:anki_key (str (java.util.UUID/randomUUID))
                                         :start_ms start-ms
                                         :end_ms end-ms
                                         :clip_media_id clip-media-id
                                         :geometry (->jsonb {:pages pages'})}]
                               :returning [:id]}))
          group-id (:score_groups/id group)
          ids (mapv (fn [direction]
                      (:flashcards/id
                       (jdbc/execute-one! tx
                         (sql/format {:insert-into :flashcards
                                      :values [{:topic_id topic-id
                                                :root_topic_id root-topic-id
                                                :kind "score"
                                                :score_group_id group-id
                                                :score_direction direction}]
                                      :returning [:id]}))))
                directions)]
      {:group-id group-id :ids ids})))

(defn update-score-group!
  "Replace a group's segment, clip, and geometry wholesale (rect ordinals
   re-assigned 1..N — crops are regenerated media, nothing binds to old
   ordinals). Directions are fixed at creation; the edit dirties every card
   row so the next push re-uploads media and re-builds both notes.
   Returns {:group-id id}."
  [{:keys [group-id start-ms end-ms clip-media-id geometry]}]
  (jdbc/with-transaction [tx ds]
    (let [old (jdbc/execute-one! tx
                ["SELECT * FROM score_groups WHERE id = ? FOR UPDATE" group-id])]
      (when-not old
        (throw (ex-info "Score group not found" {:group-id group-id})))
      (let [pages' (first
                     (reduce (fn [[acc n] page]
                               (let [rects (mapv (fn [rect ordinal] (assoc rect :ordinal ordinal))
                                             (:rects page) (iterate inc n))]
                                 [(conj acc (assoc page :rects rects))
                                  (+ n (count rects))]))
                       [[] 1] (vec (:pages geometry))))
            geometry' {:pages pages'}
            ;; Superseded rendition, captured before the update. Direction is
            ;; per-card and fixed at creation, so it is not part of a group
            ;; rendition — the renderer reads it from the surviving card row.
            owner (jdbc/execute-one! tx
                    ["SELECT root_topic_id FROM flashcards WHERE score_group_id = ? LIMIT 1"
                     group-id])
            group-changed? (any-changed?
                             [[(:score_groups/geometry old) geometry']
                              [(:score_groups/start_ms old) start-ms]
                              [(:score_groups/end_ms old) end-ms]
                              [(:score_groups/clip_media_id old) clip-media-id]])]
        (when (and group-changed? owner)
          (insert-card-version! tx
            {:root_topic_id (:flashcards/root_topic_id owner)
             :scope_type "score_group"
             :scope_id group-id
             :kind "score"
             :geometry (:score_groups/geometry old)
             :score_start_ms (:score_groups/start_ms old)
             :score_end_ms (:score_groups/end_ms old)
             :score_clip_media_id (:score_groups/clip_media_id old)}))
        (jdbc/execute-one! tx
          (sql/format {:update :score_groups
                       :set {:start_ms start-ms
                             :end_ms end-ms
                             :clip_media_id clip-media-id
                             :geometry (->jsonb geometry')
                             :updated_at [:now]}
                       :where [:= :id group-id]}))
        (jdbc/execute! tx
          (sql/format {:update :flashcards
                       :set {:updated_at [:now]}
                       :where [:= :score_group_id group-id]}))
        {:group-id group-id}))))

(defn remove-score-card-cleanup!
  "After a single score flashcard row was deleted elsewhere: delete the group
   once no direction rows remain (its media rows stay — sha-deduped, orphan
   tolerated). Returns {:group-deleted? bool}."
  [group-id]
  (jdbc/with-transaction [tx ds]
    (let [remaining (:n (jdbc/execute-one! tx
                          ["SELECT COUNT(*) AS n FROM flashcards WHERE score_group_id = ?"
                           group-id]))]
      (if (zero? remaining)
        (do (jdbc/execute-one! tx ["DELETE FROM score_groups WHERE id = ?" group-id])
          {:group-deleted? true})
        {:group-deleted? false}))))

(defn unsynced-sql
  "THE staleness predicate, as SQL. `prefix` is a table alias with a trailing
   dot (\"f.\"), or \"\" for an unaliased `flashcards`.

   A card is unsynced when its content no longer matches what the last sync agreed
   on, or when it has never been pushed:

     local_current_digest IS DISTINCT FROM local_baseline_digest
       OR anki_note_id IS NULL

   `IS DISTINCT FROM` rather than `<>` because both sides are nullable and
   `NULL <> NULL` is NULL, not true. The `anki_note_id` clause is not redundant:
   for a never-pushed card both digests can be NULL, and `NULL IS DISTINCT FROM
   NULL` is FALSE — so without it a new card would read as synced.

   One definition, four readers: the two counts below, `synced_cards` in the
   document summary refresh, and `card-components/sync-state` (the Clojure
   restatement, which reads the same two columns off the row).

   It replaced `updated_at > anki_synced_at`. That test moved on writes the digest
   normalises away and stayed still on settings changes the digest now covers, so
   it and the reconcile disagreed — and a card caught between them could never be
   cleared, because the count said work existed and the runner said none did.

   Both operators are strict: `IS DISTINCT FROM` and `IS NULL` return true or
   false, never NULL. So `NOT (…)` around this is total, which is what lets the
   document tree count SYNCED cards from the same definition instead of writing a
   second one."
  ([] (unsynced-sql ""))
  ([prefix]
   (str "(" prefix "local_current_digest IS DISTINCT FROM " prefix "local_baseline_digest"
     " OR " prefix "anki_note_id IS NULL)")))

(defn get-unsynced-card-count
  "Count unsynced cards for a specific topic. See `unsynced-sql`."
  [topic-id]
  (let [result (jdbc/execute-one! ds
                 [(str "SELECT COUNT(*) AS cnt FROM flashcards
                        WHERE topic_id = ? AND " (unsynced-sql))
                  topic-id])]
    (or (:cnt result) 0)))

(defn get-unsynced-card-count-for-root
  "Count unsynced cards for an entire root topic tree. See `unsynced-sql`."
  [root-topic-id]
  (let [result (jdbc/execute-one! ds
                 [(str "SELECT COUNT(*) AS cnt FROM flashcards
                        WHERE root_topic_id = ? AND " (unsynced-sql))
                  root-topic-id])]
    (or (:cnt result) 0)))

;; ---------------------------------------------------------------------------
;; Status overview (denormalized per-document stats)
;; ---------------------------------------------------------------------------

(defn get-document-status
  "Get maintained progress stats for all root documents owned by user-id.
   For PDFs: counts kind='page' direct children.
   For non-PDFs: counts root itself + all descendants.
   Returns a vector of maps with :id, :title, :kind, :created_at,
   :total_items, :done_items, :total_cards, :synced_cards, plus
   bibliography fields from sources (:sources/title etc.).

   Preconditions: setup-schema-document-summaries! installed and backfilled the
   read model. Postconditions: one row per visible root, ordered newest first.
   Invariant: database triggers update the summary inside the source write's
   transaction. A missing summary row is an implementation bug."
  [user-id]
  (jdbc/execute! ds
    ["SELECT t.id, t.title, t.kind, t.created_at,
             s.title, s.csl_type, s.container_title,
             summary.total_items AS total_items,
             summary.done_items AS done_items,
             summary.total_cards AS total_cards,
             summary.synced_cards AS synced_cards
      FROM topics t
      LEFT JOIN sources s ON s.id = t.source_id
      JOIN document_summaries summary ON summary.root_topic_id = t.id
      WHERE t.user_id = ? AND t.parent_id IS NULL AND t.staged_delete_id IS NULL
      ORDER BY t.created_at DESC"
     user-id]))

;; ---------------------------------------------------------------------------
;; Scheduling (unified — no topic-type dispatch)
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Repetition history — append-only log of session-driven mutations.
;; Every mutation that writes to topics from a learn session also writes one
;; row capturing the pre-mutation snapshot of the six SR-relevant fields,
;; inside the same transaction.
;; ---------------------------------------------------------------------------

(defn- snapshot-topic-row
  "Read the six SR-relevant fields + user_id from `topics` inside an open tx.
   Returns nil if the topic doesn't exist."
  [tx id]
  (jdbc/execute-one! tx
    ["SELECT user_id, status, priority, interval_days, a_factor,
            next_review_at, last_review_at
       FROM topics WHERE id = ?" id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- insert-repetition!
  "Append one row to topic_repetitions inside an open tx.
   Pre:  `before` is the snapshot returned by `snapshot-topic-row`.
   Post: one row exists with `event_type` and pre-mutation fields equal to `before`."
  [tx topic-id event-type before]
  (jdbc/execute-one! tx
    ["INSERT INTO topic_repetitions
        (topic_id, user_id, event_type,
         status_before, priority_before, interval_days_before, a_factor_before,
         next_review_at_before, last_review_at_before)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
     topic-id (:user_id before) event-type
     (or (:status before) "active")
     (:priority before)
     (double (or (:interval_days before) 1.0))
     (double (or (:a_factor before) 2.0))
     (:next_review_at before)
     (:last_review_at before)]))

;; Forward decl: the scheduling + pin mutations below own-gate via subtree-owner,
;; which is defined further down (near owns-topic?).
(declare subtree-owner)

(def max-topic-interval-days
  "The longest interval, in days, that a topic write may schedule: 100 years,
   the same as Anki's default maximum interval (`fsrs/make-scheduler`).
   Without it, the 27th advance at the default A-Factor of 2.0 computes
   NOW() + 2^27 days and PostgreSQL rejects it with `timestamp out of range`.
   See plans/topic-interval-upper-bound.md."
  36500.0)

(defn advance-topic!
  "Advance a topic's review schedule using A-Factor algorithm.
   Logs 'advance' event with pre-mutation snapshot. Owner-gated by `user-id`
   (the topic's root carries the owner); a non-owner call is a no-op.
   Post: interval_days <= max-topic-interval-days, and next_review_at is
         NOW() plus that same interval."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "advance" before)
      ;; Floor the interval to >= 1 day before multiplying. COALESCE guards NULL
      ;; but not 0/negatives, and 0 is a multiplication fixed point (0*a_factor=0
      ;; → next_review_at = NOW() → topic stays due forever). GREATEST(...,1.0)
      ;; enforces the "interval >= 1 day" invariant so every advance pushes the
      ;; review strictly into the future.
      ;; LEAST(..., cap) is the upper bound. Both expressions read the old row,
      ;; so with the same cap in each, the stored interval and the date agree.
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET interval_days = LEAST(GREATEST(COALESCE(interval_days, 1.0), 1.0) * COALESCE(a_factor, 2.0), ?),
              next_review_at = NOW() + LEAST(GREATEST(COALESCE(interval_days, 1.0), 1.0) * COALESCE(a_factor, 2.0), ?) * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         max-topic-interval-days max-topic-interval-days id])))))

(defn update-topic-priority!
  "Update a topic's priority (0=highest … 100=lowest, three decimal places).
   Owner-gated by `user-id`.
   Pre : `raw-priority` is unvalidated caller input — priority/normalize is
         the write boundary, so nil, NaN and out-of-range all arrive legally.
   Post: topics.priority holds the normalized value.
   Logs 'priority-change' only when the normalized value differs from the
   stored one. BOTH SIDES ARE COERCED TO double before that comparison: the
   column reads back as a Double while Transit delivers a whole number as a
   Long, and (not= 50.0 50) is true — an uncoerced compare would log a
   priority-change event on every save of an unchanged whole number.
   A stored NULL stays distinct from every normalized value, so the first
   write to a NULL row always lands.
   Returns the outcome, because a caller cannot otherwise tell a refused write
   from a completed one: :updated, :unchanged (already at that value), or
   :denied (not the owner, or no such topic). :unchanged and :updated are both
   successes — only :denied means the stored value is not what was asked for."
  [user-id id raw-priority]
  (let [normalized (priority/normalize raw-priority)]
    (jdbc/with-transaction [tx ds]
      (if (not= user-id (subtree-owner tx id))
        :denied
        (if-let [before (snapshot-topic-row tx id)]
          (if (= (some-> (:priority before) double) normalized)
            :unchanged
            (do
              (insert-repetition! tx id "priority-change" before)
              (jdbc/execute-one! tx
                ["UPDATE topics SET priority = ? WHERE id = ?" normalized id])
              :updated))
          :denied)))))

(defn postpone-topic!
  "Postpone a topic by N days without changing interval/a-factor. Owner-gated by
   `user-id`. Logs 'postpone' event with pre-mutation snapshot.
   `days` is typed by the user and the Postpone input's max is advisory, so
   LEAST caps it at max-topic-interval-days before it reaches the timestamp."
  [user-id id days]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "postpone" before)
      (jdbc/execute-one! tx
        ["UPDATE topics
          SET next_review_at = NOW() + LEAST(?, ?) * INTERVAL '1 day',
              last_review_at = NOW()
          WHERE id = ?"
         (double days) max-topic-interval-days id])))))

(defn done-topic!
  "Mark a topic as done. Owner-gated by `user-id`. Logs 'done' event with
   pre-mutation snapshot."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "done" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'done', done_at = now() WHERE id = ?" id])))))

(defn restore-topic!
  "Restore a done topic back to active queue. Owner-gated by `user-id`.
   Logs 'restore' event."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "restore" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET status = 'active', next_review_at = NULL, done_at = NULL
          WHERE id = ?" id])))))

(defn touch-topic!
  "Update last_review_at without advancing the interval (subset-review soft rep).
   Owner-gated by `user-id`. Logs 'touch' event with pre-mutation snapshot."
  [user-id id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx id))
     (when-let [before (snapshot-topic-row tx id)]
      (insert-repetition! tx id "touch" before)
      (jdbc/execute-one! tx
        ["UPDATE topics SET last_review_at = NOW() WHERE id = ?" id])))))

(defn get-topic-history
  "Return repetition history rows for a topic, newest first.
   Timestamps pre-formatted server-side; interval_since_prev_days computed
   via LAG over ascending event_at so each row shows the gap from the prior
   chronological event. Numeric columns are cast to float8 so they cross the
   Transit wire as JS doubles (BigDecimal would arrive as an opaque tagged value)."
  [topic-id]
  (try
    (jdbc/execute! ds
      ["SELECT id,
              event_type,
              TO_CHAR(event_at, 'YYYY-MM-DD HH24:MI:SS')               AS event_at,
              status_before,
              priority_before,
              interval_days_before::float8                             AS interval_days_before,
              a_factor_before::float8                                  AS a_factor_before,
              TO_CHAR(next_review_at_before, 'YYYY-MM-DD HH24:MI:SS')  AS next_review_at_before,
              TO_CHAR(last_review_at_before, 'YYYY-MM-DD HH24:MI:SS')  AS last_review_at_before,
              (EXTRACT(EPOCH FROM
                       (event_at - LAG(event_at)
                                     OVER (ORDER BY event_at ASC, id ASC)))
                / 86400.0)::float8                                     AS interval_since_prev_days
         FROM topic_repetitions
         WHERE topic_id = ?
         ORDER BY event_at DESC, id DESC"
       topic-id]
      {:builder-fn rs/as-unqualified-maps})
    (catch Exception e
      (tel/error! {:id ::get-topic-history :data {:topic-id topic-id}} e)
      [])))

(defn get-topic-next-review
  "Return current next_review_at + status + title for a topic, formatted for the
   HistoryModal's 'Next' header row (SuperMemo parity).
   days_until_next is cast to float8 — see get-topic-history for the rationale."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT title,
            status,
            priority,
            interval_days::float8                            AS interval_days,
            a_factor::float8                                 AS a_factor,
            TO_CHAR(next_review_at, 'YYYY-MM-DD HH24:MI:SS') AS next_review_at,
            CASE WHEN next_review_at IS NULL THEN NULL
                 ELSE (EXTRACT(EPOCH FROM (next_review_at - NOW())) / 86400.0)::float8 END
                                                              AS days_until_next
       FROM topics WHERE id = ?"
     topic-id]
    {:builder-fn rs/as-unqualified-maps}))

;; ---------------------------------------------------------------------------
;; Queue queries (single SELECT, no UNION ALL)
;; ---------------------------------------------------------------------------

;; Kinds that scaffold a document tree rather than holding something a user
;; studies: a PDF page stub, plus the repository, commit and folder nodes a git
;; forge import creates (plans/git-forge-integration.md §4.1).
;;
;; Two readers, and both properties are load-bearing:
;;   - every review-queue query below excludes them, so a folder never enters
;;     the Learn queue and never inflates a queue count;
;;   - reparent-topic! refuses to move one, because their parent_id IS their
;;     identity — a page stub is (parent_id, page_number), and a repo node's
;;     descendants are addressed by repo_path under one commit.
;;
;; Invariant: `structural-kinds-sql` is derived from this set and never written
;; by hand, so a kind added here reaches all nine readers at once.
(def ^:private structural-kinds #{"page" "repo" "commit" "dir"})

(def ^:private structural-kinds-sql
  "`structural-kinds` as a SQL IN list: \"('commit', 'dir', 'page', 'repo')\".
   Sorted, so the rendered SQL is stable across JVM runs."
  (str "(" (str/join ", " (map #(str "'" % "'") (sort structural-kinds))) ")"))

(defn get-learning-queue
  "Due topics for incremental reading. Single query, no UNION.
   Feeds BOTH the Start Learning session and the Learn overview list, so the
   list IS the queue: the top row is the topic the session opens.
   Ordering (SuperMemo outstanding-queue model): date gates membership only;
   priority orders the queue; a per-day hash of the id breaks ties within a
   priority band, so equal-priority topics shuffle daily (stable within a day,
   immune to mid-session :refresh) rather than ordering by due date.
   `sm_rank` sits between the two: rounding a SuperMemo collection's priority
   order into 0-100 collapses ~15 topics per band, and without this term the
   daily hash would discard the order the import went to the trouble of
   preserving. NULLS LAST keeps natively created topics on the hash.
   Selects `t.content_preview`, the STORED generated column holding 400
   characters of stripped text. learn-page/prepare-queue-rows takes 80
   characters from it, so the bound leaves 5x of headroom. It is a real column,
   so pgjdbc QUALIFIES the key (`:topics/content_preview`).
   NEVER take the prefix here with `left(t.content, n)`. A prefix over a TOASTed
   column asks PostgreSQL for a byte slice that can end mid-character, and the
   resulting `invalid byte sequence for encoding \"UTF8\"` aborts the whole
   statement — which this fn then reports as an empty queue. That is the defect
   this column exists to remove (plans/learn-queue-toast-slice-defect.md).
   The content column is deliberately absent: the learn session reads only :id
   and :kind off these rows, and shipping whole document bodies down the queue
   cost one body per review step.
   Joins `sources` to surface bibliography fields (source_url, source_title,
   source_csl_type, source_container); NULL when the topic has no source_id.

   Pre : `user-id` identifies an existing user.
   Post: `{:rows [row …] :error nil}`, or `{:rows [] :error message}` when the
         query fails. NEVER throws — an exception here would cross an `e/server`
         boundary and close the WebSocket. The caller MUST render `:error`
         rather than treat an empty `:rows` as an empty queue: reporting a
         failed query as \"nothing due\" told three users they were caught up
         while 2512 topics waited."
  [user-id]
  (try
    {:rows (jdbc/execute! ds
             [(str "SELECT t.id, t.parent_id, t.kind, t.title, t.priority,
                     t.next_review_at, t.interval_days, t.a_factor,
                     t.status, t.content_preview,
                     s.url, s.title, s.csl_type, s.container_title
              FROM topics t
              LEFT JOIN sources s ON s.id = t.source_id
              WHERE t.user_id = ?
                AND t.kind NOT IN " structural-kinds-sql "
                AND t.staged_delete_id IS NULL
                AND (t.next_review_at::date <= CURRENT_DATE OR t.next_review_at IS NULL)
                AND (t.status = 'active' OR t.status IS NULL)
                AND NOT t.dismissed
              ORDER BY t.priority ASC, t.sm_rank ASC NULLS LAST,
                       md5(t.id::text || CURRENT_DATE::text) ASC, t.id ASC")
              user-id])
     :error nil}
    (catch Exception e
      (tel/error! {:id ::get-learning-queue} e)
      {:rows [] :error (ex-message e)})))

(defn get-learning-queue-count
  "Count of due topics."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 [(str "SELECT COUNT(*) AS total FROM topics
                   WHERE user_id = ?
                     AND kind NOT IN " structural-kinds-sql "
                     AND staged_delete_id IS NULL
                     AND (next_review_at::date <= CURRENT_DATE OR next_review_at IS NULL)
                     AND (status = 'active' OR status IS NULL)
                     AND NOT dismissed")
                  user-id])]
    (or (:total result) 0)))

(defn get-total-topic-count
  "Count ALL topics (due and not due, excluding done and pages)."
  [user-id]
  (let [result (jdbc/execute-one! ds
                 [(str "SELECT COUNT(*) AS total FROM topics
                   WHERE user_id = ?
                     AND kind NOT IN " structural-kinds-sql "
                     AND staged_delete_id IS NULL
                     AND (status = 'active' OR status IS NULL)
                     AND NOT dismissed")
                  user-id])]
    (or (:total result) 0)))

;; ── Getting-started checklist (plans/getting-started-checklist.md §4) ──────

(defn get-getting-started-progress
  "Which getting-started steps `user-id` has done, from one round trip (1.1).

   Pre : `user-id` is a users.id.
   Post: six booleans, each true when existing data proves the step.
         :imported?    a root topic not staged for deletion, any kind or status
         :read?        a Learn Next, Postpone or Done (`topic_repetitions`)
         :extracted?   an `is_extract` topic not staged for deletion
         :carded?      a flashcard whose root topic is the user's, not staged
         :reviewed?    a Quiz review of a flashcard (`kg_reviews.flashcard_id`)
         :anki-pushed? such a flashcard that also has an `anki_note_id`"
  [user-id]
  (let [row (jdbc/execute-one! ds
              ["SELECT
                  EXISTS (SELECT 1 FROM topics
                          WHERE user_id = ? AND parent_id IS NULL
                            AND staged_delete_id IS NULL) AS has_imported,
                  EXISTS (SELECT 1 FROM topic_repetitions
                          WHERE user_id = ?
                            AND event_type IN ('advance', 'postpone', 'done')) AS has_read,
                  EXISTS (SELECT 1 FROM topics
                          WHERE user_id = ? AND is_extract
                            AND staged_delete_id IS NULL) AS has_extracted,
                  EXISTS (SELECT 1 FROM flashcards f
                          JOIN topics r ON r.id = f.root_topic_id
                          WHERE r.user_id = ? AND r.staged_delete_id IS NULL) AS has_cards,
                  EXISTS (SELECT 1 FROM kg_reviews
                          WHERE user_id = ? AND flashcard_id IS NOT NULL) AS has_reviewed,
                  EXISTS (SELECT 1 FROM flashcards f
                          JOIN topics r ON r.id = f.root_topic_id
                          WHERE r.user_id = ? AND r.staged_delete_id IS NULL
                            AND f.anki_note_id IS NOT NULL) AS has_anki_pushed"
               user-id user-id user-id user-id user-id user-id])]
    {:imported? (boolean (:has_imported row))
     :read? (boolean (:has_read row))
     :extracted? (boolean (:has_extracted row))
     :carded? (boolean (:has_cards row))
     :reviewed? (boolean (:has_reviewed row))
     :anki-pushed? (boolean (:has_anki_pushed row))}))

(defn get-getting-started-targets
  "The topics a getting-started \"Show me\" opens, from one round trip (§7).

   Reads `content_preview`, the stored generated column, rather than
   `content_text`: a test of the TOASTed column would detoast every row the
   scan passes.

   Pre : `user-id` is a users.id.
   Post: each key is a topic id or nil. Every topic is the user's and not
         staged for deletion, and the highest id wins.
         :text-topic-id  a topic with text, pages included (1.5)
         :extract-id     an `is_extract` topic with text
         :pdf-id         a `pdf` root
         :carded-root-id the root of the newest flashcard"
  [user-id]
  (let [row (jdbc/execute-one! ds
              ["SELECT
                  (SELECT id FROM topics
                   WHERE user_id = ? AND staged_delete_id IS NULL
                     AND content_preview <> ''
                   ORDER BY id DESC LIMIT 1) AS text_topic_id,
                  (SELECT id FROM topics
                   WHERE user_id = ? AND staged_delete_id IS NULL
                     AND is_extract AND content_preview <> ''
                   ORDER BY id DESC LIMIT 1) AS extract_id,
                  (SELECT id FROM topics
                   WHERE user_id = ? AND staged_delete_id IS NULL
                     AND parent_id IS NULL AND kind = 'pdf'
                   ORDER BY id DESC LIMIT 1) AS pdf_id,
                  (SELECT f.root_topic_id FROM flashcards f
                   JOIN topics r ON r.id = f.root_topic_id
                   WHERE r.user_id = ? AND r.staged_delete_id IS NULL
                   ORDER BY f.id DESC LIMIT 1) AS carded_root_id"
               user-id user-id user-id user-id])]
    {:text-topic-id (:text_topic_id row)
     :extract-id (:extract_id row)
     :pdf-id (:pdf_id row)
     :carded-root-id (:carded_root_id row)}))

(defn get-queue-summary
  "Aggregate queue stats in a single SQL query."
  [user-id]
  (try
    (let [result (jdbc/execute-one! ds
                   [(str "SELECT COUNT(*) AS total,
                           COUNT(*) FILTER (WHERE status = 'done' AND NOT dismissed) AS inactive,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
                                             AND NOT dismissed
                                             AND (next_review_at IS NULL
                                                  OR next_review_at <= CURRENT_TIMESTAMP)) AS due_today,
                           COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL)
                                             AND NOT dismissed
                                             AND (next_review_at IS NULL
                                                  OR next_review_at <= CURRENT_TIMESTAMP + INTERVAL '7 days')) AS due_week
                    FROM topics
                    WHERE user_id = ? AND kind NOT IN " structural-kinds-sql
                      " AND staged_delete_id IS NULL")
                    user-id])]
      {:total (or (:total result) 0)
       :inactive (or (:inactive result) 0)
       :due-today (or (:due_today result) 0)
       :due-week (or (:due_week result) 0)})
    (catch Exception e
      (tel/error! {:id ::get-queue-summary} e)
      {:total 0 :inactive 0 :due-today 0 :due-week 0})))

(defn get-review-calendar
  "Topic counts per calendar day from today (inclusive) through +N days.
   Date-bucketed (not timestamp), so today's already-past items count at day 0
   rather than leaking into the overdue backlog (see get-overdue-count)."
  [user-id days]
  (jdbc/execute! ds
    [(str "SELECT DATE(next_review_at) AS review_date, COUNT(*) AS count
      FROM topics
      WHERE user_id = ?
        AND kind NOT IN " structural-kinds-sql "
        AND staged_delete_id IS NULL
        AND next_review_at::date BETWEEN CURRENT_DATE AND CURRENT_DATE + ?::int
        AND (status = 'active' OR status IS NULL)
        AND NOT dismissed
      GROUP BY DATE(next_review_at)
      ORDER BY review_date")
     user-id days]))

(defn get-overdue-count
  "Backlog: active topics whose review date is before today (overdue).
   Charted as the single backlog bar in the Future Due chart."
  [user-id]
  (let [r (jdbc/execute-one! ds
            [(str "SELECT COUNT(*) AS n
              FROM topics
              WHERE user_id = ?
                AND kind NOT IN " structural-kinds-sql "
                AND staged_delete_id IS NULL
                AND next_review_at::date < CURRENT_DATE
                AND (status = 'active' OR status IS NULL)
                AND NOT dismissed")
             user-id])]
    (or (:n r) 0)))

(defn get-study-calendar
  "Distinct topics advanced per day over the last 14 days (incl. today).
   Keys: :d (YYYY-MM-DD string, server zone), :c (distinct topic count)."
  [user-id]
  (try
    (jdbc/execute! ds
      ["SELECT TO_CHAR(DATE(event_at), 'YYYY-MM-DD') AS d,
               COUNT(DISTINCT topic_id) AS c
        FROM topic_repetitions
        WHERE user_id = ?
          AND event_type = 'advance'
          AND event_at >= CURRENT_DATE - INTERVAL '13 days'
        GROUP BY DATE(event_at)
        ORDER BY d"
       user-id])
    (catch Exception e
      (tel/error! {:id ::get-study-calendar} e)
      [])))

(defn get-study-streak
  "Consecutive calendar days with >=1 advance, Anki-style.
   Anchor = today if studied today, else yesterday; counts backward from anchor.
   A zero-advance today does not break the streak until the day fully passes.

   Pre : topic_repetitions has idx_topic_repetitions_user_event_day.
   Post: returns one non-negative count.
   Invariant: work is proportional to the current streak, not all-time history."
  [user-id]
  (try
    (let [row (jdbc/execute-one! ds
                ["WITH RECURSIVE streak(day, length) AS (
                    SELECT CASE
                             WHEN EXISTS (
                               SELECT 1 FROM topic_repetitions
                               WHERE user_id = ? AND event_type = 'advance'
                                 AND DATE(event_at) = CURRENT_DATE
                             ) THEN CURRENT_DATE
                             WHEN EXISTS (
                               SELECT 1 FROM topic_repetitions
                               WHERE user_id = ? AND event_type = 'advance'
                                 AND DATE(event_at) = CURRENT_DATE - 1
                             ) THEN CURRENT_DATE - 1
                           END,
                           1
                    UNION ALL
                    SELECT day - 1, length + 1
                    FROM streak
                    WHERE day IS NOT NULL
                      AND EXISTS (
                        SELECT 1 FROM topic_repetitions
                        WHERE user_id = ? AND event_type = 'advance'
                          AND DATE(event_at) = streak.day - 1
                      )
                  )
                  SELECT COALESCE(MAX(length) FILTER (WHERE day IS NOT NULL), 0) AS streak
                  FROM streak"
                 user-id user-id user-id])]
      (long (or (:streak row) 0)))
    (catch Exception e
      (tel/error! {:id ::get-study-streak} e)
      0)))

(defn get-review-counts
  "Advance-event counts. :all-time = all advances; :this-week = rolling 7 days."
  [user-id]
  (try
    (let [r (jdbc/execute-one! ds
              ["SELECT COUNT(*) AS all_time,
                       COUNT(*) FILTER (WHERE event_at >= NOW() - INTERVAL '7 days') AS this_week
                FROM topic_repetitions
                WHERE user_id = ? AND event_type = 'advance'" user-id])]
      {:all-time (or (:all_time r) 0)
       :this-week (or (:this_week r) 0)})
    (catch Exception e
      (tel/error! {:id ::get-review-counts} e)
      {:all-time 0 :this-week 0})))

(defn get-status-breakdown
  "Topic counts by status, same scope as get-queue-summary."
  [user-id]
  (try
    (let [r (jdbc/execute-one! ds
              [(str "SELECT COUNT(*) FILTER (WHERE (status = 'active' OR status IS NULL) AND NOT dismissed) AS active,
                       COUNT(*) FILTER (WHERE status = 'done' AND NOT dismissed) AS done,
                       COUNT(*) FILTER (WHERE dismissed) AS dismissed
                FROM topics
                WHERE user_id = ? AND kind NOT IN " structural-kinds-sql
                  " AND staged_delete_id IS NULL")
                user-id])]
      {:active (or (:active r) 0)
       :done (or (:done r) 0)
       :dismissed (or (:dismissed r) 0)})
    (catch Exception e
      (tel/error! {:id ::get-status-breakdown} e)
      {:active 0 :done 0 :dismissed 0})))

(defn get-inactive-topics
  "Topics excluded from the Learning Queue for a user: done OR dismissed.
   (dismissed is an orthogonal flag, so a topic can be both.)"
  [user-id]
  (try
    (jdbc/execute! ds
      [(str "SELECT id, parent_id, kind, title, created_at, status, dismissed
       FROM topics
       WHERE user_id = ?
         AND kind NOT IN " structural-kinds-sql "
         AND staged_delete_id IS NULL
         AND (status = 'done' OR dismissed)
       ORDER BY created_at DESC")
       user-id])
    (catch Exception e
      (tel/error! {:id ::get-inactive-topics} e)
      [])))

;; ---------------------------------------------------------------------------
;; Tree navigation + Knowledge tree
;; ---------------------------------------------------------------------------

(defn get-knowledge-tree
  "Fetch all topics with parent references for building the knowledge tree.
   Includes page topics so parent chain (PDF → page → extract) is intact.
   Joins `sources` to surface bibliography fields (source_title,
   source_csl_type, source_container) for badge + subtitle rendering."
  [user-id]
  (jdbc/execute! ds
    (sql/format {:select [[:t.id :id] [:t.parent_id :parent_id] [:t.title :title]
                          [:t.kind :kind] [:t.status :status] [:t.created_at :created_at]
                          [:t.dismissed :dismissed]
                          [:t.page_number :page_number] [:t.last_review_at :last_review_at]
                          :s.title :s.csl_type :s.container_title
                          [[:coalesce :tf.file_size [:octet_length [:coalesce :t.content ""]]] :file_size]
                          [[:to_char :t.created_at [:inline "Mon DD"]] :formatted_date]]
                 :from [[:topics :t]]
                 ;; Aggregated file sizes — a direct topic_files join would
                 ;; duplicate score topics (two file rows: PDF + audio).
                 :left-join [[topic-file-sizes :tf] [:= :tf.topic_id :t.id]
                             [:sources :s] [:= :t.source_id :s.id]]
                 :where [:and [:= :t.user_id user-id] [:is :t.staged_delete_id nil]]
                 :order-by [[:t.parent_id :asc-nulls-first] [:t.page_number :asc-nulls-first] [:t.created_at :asc]]})))

(defn get-library-tree-items
  "Fetch roots plus the children of expanded Library rows.

   PDF-like roots also fetch page children and their direct extracts, because
   the Library flattens page stubs out of the visible tree.

   Pre : expansion is {:mode :set|:all :ids #{integer ...}}.
   Post: roots are always present. Set mode adds only requested branches.
         All mode returns the complete tree.
   Invariant: :has_children reports undisclosed children without fetching them."
  [user-id {:keys [mode ids]}]
  (let [expanded-ids (int-array (map int ids))]
    (jdbc/execute! ds
      ["SELECT t.id, t.parent_id, t.title, t.kind, t.status, t.created_at,
               t.dismissed, t.page_number, t.last_review_at,
               s.title, s.csl_type, s.container_title,
               TO_CHAR(t.created_at, 'Mon DD') AS formatted_date,
               EXISTS (
                 SELECT 1 FROM topics child
                 WHERE child.parent_id = t.id
                   AND child.staged_delete_id IS NULL
               ) AS has_children
         FROM topics t
         LEFT JOIN sources s ON s.id = t.source_id
         WHERE t.user_id = ?
           AND t.staged_delete_id IS NULL
           AND (
             t.parent_id IS NULL
             OR ? = 'all'
             OR t.parent_id = ANY(?)
             OR EXISTS (
               SELECT 1
               FROM topics page
               WHERE page.id = t.parent_id
                 AND page.kind = 'page'
                 AND page.parent_id = ANY(?)
             )
           )
         ORDER BY t.parent_id ASC NULLS FIRST,
                  t.page_number ASC NULLS FIRST,
                  t.created_at ASC"
       user-id (name mode) expanded-ids expanded-ids])))

(defn get-extract-source-page
  "For an extract whose immediate parent is a PDF page, return
   {:root <pdf-root-id> :page <page-number>}; nil otherwise (e.g. a web/epub
   extract, or a page/root topic). Used by the 'Go to page' toolbar button."
  [extract-id]
  (when extract-id
    (let [ext    (get-topic extract-id)
          parent (when (:topics/parent_id ext) (get-topic (:topics/parent_id ext)))]
      (when (and parent
              (= "page" (:topics/kind parent))
              (:topics/page_number parent))
        (let [root-id (get-root-topic-id extract-id)
              root    (when root-id (get-topic root-id))]
          (when (= "pdf" (:topics/kind root))
            {:root root-id :page (:topics/page_number parent)}))))))

(defn get-subtree
  "Get a topic and all its descendants via recursive CTE.
   Selects from topics directly (not the CTE alias) so JDBC metadata
   reports the correct table name and next.jdbc produces :topics/xxx keys.

   Pre : columns, when given, are `topics` column keywords written in code.
         They reach the SQL text, so they never come from a request.
   Post: the 2-arity selects t.*. The 3-arity selects only `columns`, so a
         caller that needs no content reads no `content` or `content_text`."
  ([user-id root-id]
   (get-subtree user-id root-id nil))
  ([user-id root-id columns]
   (jdbc/execute! ds
     [(str "WITH RECURSIVE subtree(id) AS (
        SELECT id FROM topics WHERE id = ?
        UNION ALL
        SELECT child.id FROM topics child
        JOIN subtree ON child.parent_id = subtree.id
      )
      SELECT " (if columns
                 (str/join ", " (map #(str "t." (name %)) columns))
                 "t.*") " FROM topics t
      JOIN subtree s ON t.id = s.id
      WHERE (t.user_id = ? OR t.user_id IS NULL)
        AND t.staged_delete_id IS NULL
      ORDER BY t.parent_id ASC NULLS FIRST, t.page_number ASC NULLS LAST, t.id ASC")
      root-id user-id])))

;; ---------------------------------------------------------------------------
;; Media registry — per-user blobs (images today)
;; ---------------------------------------------------------------------------

(defn- bytes-sha256
  "Hex-encoded SHA-256 of a byte array."
  [^bytes b]
  (let [md (java.security.MessageDigest/getInstance "SHA-256")
        digest (.digest md b)]
    (apply str (map #(format "%02x" (bit-and % 0xff)) digest))))

(defn find-media-by-sha
  "Look up an existing media row for (user-id, sha256). Returns row or nil."
  [user-id sha256]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :mime_type :byte_size :kind]
                 :from [:media]
                 :where [:and [:= :user_id user-id] [:= :sha256 sha256]]})))

(defn get-media
  "Get a media row by id (includes bytes for serving)."
  [id]
  (jdbc/execute-one! ds
    (sql/format {:select [:id :user_id :kind :bytes :mime_type :byte_size]
                 :from [:media]
                 :where [:= :id id]})))

(defn upsert-media-row!
  "Insert media, or return the existing row's id if the same bytes are already
   stored for this user. Charges quota only on actual insert.

   Reports WHICH of the two happened, because a caller that must undo its own
   writes may only delete rows it created — deleting a reused row would destroy
   content the user already owned. `upsert-media!` is the id-only wrapper for
   the callers that do not compensate.

   `:meter-quota?` defaults to true and MUST stay true for anything accepting
   new bytes from a user. Pass false only when the bytes are already stored
   elsewhere in this user's own rows and are merely being relocated — metering
   those would charge for storage the user already holds, and
   quota/check-and-bump! THROWS on cap violation, which would abort the
   relocation. Sole such caller: normalize-inline-card-images!.

   Pre : `bytes` is non-nil; `user-id` owns the write.
   Post: {:media/id N :media/inserted? bool}. `:media/inserted?` is true only
         when this call created the row, i.e. only then is the row this
         caller's to delete."
  [{:keys [user-id kind ^bytes bytes mime-type source-url meter-quota?]
    :or {meter-quota? true}}]
  (let [sha256 (bytes-sha256 bytes)
        byte-size (alength bytes)]
    (if-let [existing (find-media-by-sha user-id sha256)]
      {:media/id (:media/id existing) :media/inserted? false}
      (jdbc/with-transaction [tx ds]
        (when meter-quota?
          (quota/check-and-bump! tx user-id byte-size))
        {:media/id (-> (jdbc/execute-one! tx
                         (sql/format {:insert-into :media
                                      :values [{:user_id user-id
                                                :kind (or kind "image")
                                                :bytes bytes
                                                :mime_type mime-type
                                                :sha256 sha256
                                                :byte_size byte-size
                                                :source_url source-url}]
                                      :returning [:id]}))
                     :media/id)
         :media/inserted? true}))))

(defn upsert-media!
  "Insert media or return existing id if same bytes already stored for this user.
   See `upsert-media-row!`, which this wraps; use that one when the caller has
   to undo what it stored.
   Post: media id."
  [opts]
  (:media/id (upsert-media-row! opts)))

(defn delete-media-rows!
  "Delete `ids` from `media` for `user-id` and refund their bytes.

   The compensating action for an import that stored media before opening its
   transaction: the bytes are too large to hold a transaction across, so a
   rolled-back import must undo them explicitly or bill the user for content
   nothing references.

   Pre : every id in `ids` was CREATED by the caller — an id that merely
         matched an existing sha256 belongs to content the user already owns,
         and deleting it would destroy it. `upsert-media-row!`'s
         `:media/inserted?` is how a caller knows the difference.
   Post: {:deleted N :refunded B}. `usage_bytes` is down by exactly the summed
         `byte_size` of the rows actually deleted.
   Invariant: every statement is scoped by user_id, so a guessed id cannot
         reach another user's row.
   Invariant: the id list is itself partitioned against
         `max-statement-params` — a failed import can have stored more images
         than one IN list can bind."
  [user-id ids]
  (if (empty? ids)
    {:deleted 0 :refunded 0}
    (jdbc/with-transaction [tx ds]
      (let [deleted (into []
                      (mapcat (fn [id-batch]
                                (jdbc/execute! tx
                                  (sql/format {:delete-from :media
                                               :where [:and [:= :user_id user-id]
                                                       [:in :id (vec id-batch)]]
                                               :returning [:byte_size]}))))
                      (partition-all (dec max-statement-params) (vec ids)))
            bytes (transduce (map :media/byte_size) + 0 deleted)]
        (when (pos? bytes)
          (bump-user-usage! tx user-id (- bytes)))
        {:deleted (count deleted) :refunded bytes}))))

(defn- rewrite-inline-images
  "Replace every data:image URI in `html` with /api/media/<id>, storing each
   payload as media owned by `user-id`. Returns [html' image-count].
   Pre : `html` may be nil or blank.
   Post: html' contains no 'data:image'; identical payloads collapse to one
         media row (upsert-media! dedupes on (user_id, sha256))."
  [user-id html]
  (if (or (str/blank? html) (not (str/includes? html "data:image")))
    [html 0]
    (let [n (volatile! 0)
          ;; The payload class excludes quotes, so a match cannot run past the
          ;; closing attribute delimiter; \s is included because HTML may wrap
          ;; long base64, and getMimeDecoder tolerates the whitespace.
          html' (str/replace html
                  #"data:image/([A-Za-z0-9.+-]+);base64,([A-Za-z0-9+/=\s]+)"
                  (fn [[_ subtype payload]]
                    (let [bytes (.decode (java.util.Base64/getMimeDecoder) ^String payload)
                          media-id (upsert-media! {:user-id user-id :kind "image"
                                                   :bytes bytes
                                                   :mime-type (str "image/" subtype)
                                                   ;; Relocation, not an upload —
                                                   ;; see upsert-media!.
                                                   :meter-quota? false})]
                      (vswap! n inc)
                      (str "/api/media/" media-id))))]
      [html' @n])))

(defn normalize-inline-card-images!
  "One-time: move inline data: URIs out of card fields into media rows.

   A few production cards predate the editor's upload-and-rewrite paths
   (the quill-field paste matcher and editor-image-menu, which both produce
   /api/media/<id>) and carry base64 inline. Left there, every version of such
   a card would duplicate the whole payload — measured at 8.86 MB for three
   cards, 2.6x the entire fleet's text history (plans/card-edit-history.md §3).
   Running at boot guarantees the precondition holds before any save can
   version anything.

   Pre : none — safe on an already-clean database.
   Post: every card this run converted has no 'data:image' left, and each
         payload is a media row referenced as /api/media/<id>. updated_at is
         deliberately NOT touched: this is not a user edit. No card_versions row
         is written — the pre-normalization rendition is deliberately
         unversioned.
   Inv : idempotent — a second run matches converted cards no more.
   Inv : NEVER throws. It runs inside setup-schema, so an escaping exception
         would stop the server from booting; a card that fails to convert is
         logged and skipped, leaving it exactly as it was. Its versions will
         duplicate its payload, which is a storage cost, not a correctness bug.
   Returns {:cards n :images n :failed n}."
  []
  (let [rows (jdbc/execute! ds
               ["SELECT f.id, f.question, f.answer, f.cloze, t.user_id
                 FROM flashcards f JOIN topics t ON t.id = f.root_topic_id
                 WHERE f.question LIKE '%data:image%'
                    OR f.answer   LIKE '%data:image%'
                    OR f.cloze    LIKE '%data:image%'"])
        result (reduce
                 (fn [acc row]
                   ;; Per-card isolation: malformed base64, a missing user, or a
                   ;; DB error must cost one card, not the boot.
                   (try
                     (let [user-id (:topics/user_id row)
                           [q nq] (rewrite-inline-images user-id (:flashcards/question row))
                           [a na] (rewrite-inline-images user-id (:flashcards/answer row))
                           [c nc] (rewrite-inline-images user-id (:flashcards/cloze row))
                           total (+ nq na nc)]
                       (if (zero? total)
                         acc
                         (do
                           ;; Direct UPDATE, deliberately NOT
                           ;; update-flashcard-versioned! — see Post above.
                           (jdbc/execute-one! ds
                             (sql/format {:update :flashcards
                                          :set {:question q :answer a :cloze c}
                                          :where [:= :id (:flashcards/id row)]}))
                           (-> acc (update :cards inc) (update :images + total)))))
                     (catch Exception e
                       (tel/error! {:id ::normalize-inline-card-images
                                    :data {:card-id (:flashcards/id row)}} e)
                       (update acc :failed inc))))
                 {:cards 0 :images 0 :failed 0}
                 rows)]
    (when (or (pos? (:cards result)) (pos? (:failed result)))
      (tel/log! {:level (if (pos? (:failed result)) :warn :info)
                 :id ::normalize-inline-card-images :data result}
        "Normalized inline card images to media references"))
    result))

;; ---------------------------------------------------------------------------
;; Topic pins — per-topic media references with front/back placement (K1 cap = 2)
;; ---------------------------------------------------------------------------

(defn get-pins
  "Return all pins for a topic, ordered by ord ASC."
  [topic-id]
  (jdbc/execute! ds
    (sql/format {:select [:id :topic_id :media_id :placement :ord]
                 :from [:topic_pins]
                 :where [:= :topic_id topic-id]
                 :order-by [[:ord :asc]]})))

(defn set-pin!
  "Insert a new pin for a topic. Enforces K1 cap (≤2 per topic).
   Throws ex-info :pin-cap-exceeded on violation. Returns new row id."
  [{:keys [topic-id media-id placement]}]
  (when-not (#{"front" "back"} placement)
    (throw (ex-info "Invalid placement" {:placement placement})))
  (jdbc/with-transaction [tx ds]
    (let [existing (jdbc/execute! tx
                     (sql/format {:select [:id] :from [:topic_pins]
                                  :where [:= :topic_id topic-id]}))]
      (when (>= (count existing) 2)
        (throw (ex-info "Topic already has 2 pins (max)"
                 {:reason :pin-cap-exceeded :topic-id topic-id})))
      (-> (jdbc/execute-one! tx
            (sql/format {:insert-into :topic_pins
                         :values [{:topic_id topic-id
                                   :media_id media-id
                                   :placement placement
                                   :ord (count existing)}]
                         :returning [:id]}))
        :topic_pins/id))))

(defn remove-pin!
  "Delete pin `pin-id` iff owned by `user-id` (the pin's topic's root carries the
   owner). Returns the full deleted row (RETURNING *) for undo, or nil when not
   owned / absent."
  [user-id pin-id]
  (jdbc/with-transaction [tx ds]
    (when-let [pin (jdbc/execute-one! tx
                     ["SELECT topic_id FROM topic_pins WHERE id = ?" pin-id])]
      (when (= user-id (subtree-owner tx (:topic_pins/topic_id pin)))
        (jdbc/execute-one! tx
          (sql/format {:delete-from :topic_pins
                       :where [:= :id pin-id]
                       :returning [:*]}))))))

(defn update-pin-placement!
  "Update pin `pin-id`'s placement iff owned by `user-id`. Throws on invalid value."
  [user-id pin-id new-placement]
  (when-not (#{"front" "back"} new-placement)
    (throw (ex-info "Invalid placement" {:placement new-placement})))
  (jdbc/with-transaction [tx ds]
    (when-let [pin (jdbc/execute-one! tx
                     ["SELECT topic_id FROM topic_pins WHERE id = ?" pin-id])]
      (when (= user-id (subtree-owner tx (:topic_pins/topic_id pin)))
        (jdbc/execute-one! tx
          (sql/format {:update :topic_pins
                       :set {:placement new-placement}
                       :where [:= :id pin-id]}))))))

(defn copy-pins-to-child!
  "Snapshot parent topic's pins into a newly created child topic.
   Called from extract-create flow (EC-snapshot). No-op if parent has no pins."
  [parent-topic-id child-topic-id]
  (when-let [parent-pins (seq (get-pins parent-topic-id))]
    (jdbc/execute! ds
      (sql/format {:insert-into :topic_pins
                   :values (mapv (fn [row]
                                   {:topic_id child-topic-id
                                    :media_id (:topic_pins/media_id row)
                                    :placement (:topic_pins/placement row)
                                    :ord (:topic_pins/ord row)})
                             parent-pins)}))))

(defn- tree-order-items
  "Sort items in depth-first tree order."
  [items]
  (let [id-set (set (map :topics/id items))
        by-parent (group-by :topics/parent_id items)
        roots (->> items
                (filter #(not (id-set (:topics/parent_id %))))
                (sort-by (juxt :topics/page_number :topics/id)))]
    (loop [result []
           stack (vec (reverse roots))]
      (if (empty? stack)
        result
        (let [node (peek stack)
              children (->> (get by-parent (:topics/id node))
                         (sort-by (juxt :topics/page_number :topics/id))
                         reverse vec)]
          (recur (conj result node)
            (into (pop stack) children)))))))

(def ^:private subset-review-columns
  "The `topics` columns get-subset-review-queue reads: the session's fields,
   the filters' status, kind and last review, and the outstanding test's
   next review. No content, so the subtree read loads no TOAST."
  [:id :parent_id :kind :title :status :page_number :last_review_at :next_review_at])

(defn get-subset-review-queue
  "Get the subset review queue for a topic subtree.
   Filters out done items. Annotates :outstanding?.
   When include-reviewed-today? is false (default), also filters out items reviewed today."
  [user-id root-id & {:keys [include-reviewed-today?] :or {include-reviewed-today? false}}]
  (let [raw (get-subtree user-id root-id subset-review-columns)
        now (java.sql.Timestamp. (System/currentTimeMillis))
        today (java.time.LocalDate/now)]
    (let [active (->> raw
                   (remove #(structural-kinds (:topics/kind %)))
                   (filter #(let [s (:topics/status %)] (or (= s "active") (nil? s)))))
          filtered (if include-reviewed-today?
                     active
                     (remove (fn [item]
                               (when-let [la (:topics/last_review_at item)]
                                 (= today (.. la toInstant (atZone (java.time.ZoneId/systemDefault)) toLocalDate))))
                       active))
          ordered (tree-order-items filtered)]
      ;; Slim to the fields the review session reads. TopicPage fetches
      ;; content by id, so the session never needs it here.
      (mapv (fn [item]
              (let [nra (:topics/next_review_at item)]
                (assoc (select-keys item [:topics/id :topics/parent_id :topics/kind
                                          :topics/title :topics/status :topics/page_number])
                  :outstanding? (or (nil? nra)
                                  (.before nra now)))))
        ordered))))

;; ---------------------------------------------------------------------------
;; Undo log — per-user action history (12h window; ordinary entries ≤100).
;; Forward (log) writes happen at delete time; reverse (restore) replays a
;; snapshot. Restores are idempotent via ON CONFLICT (id) DO NOTHING, so a
;; double-undo cannot resurrect a row twice.
;; ---------------------------------------------------------------------------

(defn- unqualify-row
  "Strip the table namespace from a next.jdbc row's keys (:flashcards/id → :id)
   so a stored snapshot is portable across the wire and back into an INSERT.
   JSONB column values (PGobject) are parsed to plain data — cheshire cannot
   encode PGobject when the snapshot itself is stored as JSONB."
  [row]
  (-> row
    (update-keys (comp keyword name))
    (update-vals pgobject->clj)))

(def ^:private flashcard-ts-cols
  "Timestamp columns that arrive as ISO strings after a JSONB round-trip and
   must be CAST back to timestamp on restore."
  #{:created_at :updated_at :anki_synced_at})

(defn- cast-timestamps
  "Wrap present, non-nil timestamp values as CAST(? AS timestamp) for honeysql."
  [row ts-cols]
  (reduce (fn [m k]
            (if (some? (get m k))
              (update m k (fn [v] [:cast v :timestamp]))
              m))
    row ts-cols))

(defn prune-undo-log!
  "Drop user-id's undo entries older than 12h or beyond the 100 most recent.
   Idempotent; called after every insert to keep the log bounded.
   Excludes staged-delete entries entirely: removing one would SET NULL its
   root marker and resurface data whose dependent rows may already be gone.
   The staged purges delete their roots first, then remove their entries."
  [conn user-id]
  (jdbc/execute! conn
    ["DELETE FROM undo_log
      WHERE user_id = ?
        AND action_type NOT IN ('delete-document', 'delete-curriculum',
                                'delete-assistant-chat')
        AND (occurred_at < now() - interval '12 hours'
             OR id IN (SELECT id FROM undo_log
                       WHERE user_id = ?
                         AND action_type NOT IN ('delete-document', 'delete-curriculum',
                                'delete-assistant-chat')
                       ORDER BY occurred_at DESC
                       OFFSET 100))"
     user-id user-id]))

(defn insert-undo-entry-raw!
  "Insert an undo entry with an already-shaped snapshot value (vector OR map),
   on the given connectable (joins a caller's transaction). Returns the id.
   Does NOT prune — the caller prunes once its mutation is committed."
  [conn user-id action-type entity-type entity-refs snapshot]
  (-> (jdbc/execute-one! conn
        (sql/format {:insert-into :undo_log
                     :values [{:user_id user-id
                               :action_type action-type
                               :entity_type entity-type
                               :entity_refs (->jsonb (vec entity-refs))
                               :snapshot (->jsonb snapshot)}]
                     :returning [:id]}))
    :undo_log/id))

(defn insert-undo-entry!
  "Append an undo entry and prune the user's log. snapshot-rows is a seq of
   maps (next.jdbc rows or plain maps) restored verbatim on undo; their keys
   are unqualified before storage. Returns the new entry id.
   Not transactional with the caller's mutation: a failure here loses
   undoability for that action but never corrupts data."
  [user-id action-type entity-type entity-refs snapshot-rows]
  (let [id (insert-undo-entry-raw! ds user-id action-type entity-type entity-refs
             (mapv unqualify-row snapshot-rows))]
    (prune-undo-log! ds user-id)
    id))

(defn snapshot-cards
  "The card rows inside an undo snapshot, whichever shape it has.
   A card delete writes a MAP (insert-card-delete-undo!) so it can carry the
   group rows the delete destroyed; entries written before groups collapsed
   hold a bare vector of card rows, and those stay readable for the 12h the log
   keeps them. Every reader of a 'flashcard' snapshot goes through here."
  [snapshot]
  (if (map? snapshot) (:cards snapshot) snapshot))

(defn insert-card-delete-undo!
  "Undo entry for a card delete that may have destroyed whole groups.
   Pre:  snapshot = {:cards [row ...] :occlusion-groups [row ...]
         :score-groups [row ...]}, rows exactly as jdbc returned them.
   Post: keys unqualified and JSONB values parsed for every row in every
         vector — the nesting puts them out of reach of insert-undo-entry!'s
         flat mapv, and cheshire cannot encode a PGobject.
   Returns the entry id. Reversed by restore-card-rows!, which the undo
   dispatch selects on the snapshot being a MAP; the flat-vector shape written
   before this existed still restores through restore-flashcards!."
  [user-id action-type card-ids snapshot]
  (let [id (insert-undo-entry-raw! ds user-id action-type "flashcard" card-ids
             (update-vals snapshot #(mapv unqualify-row %)))]
    (prune-undo-log! ds user-id)
    id))

(defn- parse-undo-row
  "Decode a raw undo_log row: keywordized unqualified keys with :entity_refs
   and :snapshot parsed back from JSONB."
  [row]
  (-> row
    (update :entity_refs pgobject->clj)
    (update :snapshot pgobject->clj)))

(defn get-undo-entries
  "Live (not-yet-undone) undo entries for user-id within the 12h window,
   newest first. Ordinary entries are capped by prune-undo-log!, while staged
   deletions stay visible until their own purges run."
  [user-id]
  (mapv parse-undo-row
    (jdbc/execute! ds
      (sql/format {:select [:*]
                   :from [:undo_log]
                   :where [:and
                           [:= :user_id user-id]
                           [:is :undone_at nil]
                           [:> :occurred_at [:raw "now() - interval '12 hours'"]]]
                   :order-by [[:occurred_at :desc]]})
      {:builder-fn rs/as-unqualified-maps})))

(defn get-undo-entry
  "Single undo entry by id, or nil."
  [entry-id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:*] :from [:undo_log] :where [:= :id entry-id]})
            {:builder-fn rs/as-unqualified-maps})
    parse-undo-row))

(defn undo-entry-within-window?
  "True when entry-id remains live in the database's 12-hour Undo window.
   The server, not a stale client, owns the deadline check."
  [entry-id]
  (boolean (:exists
             (jdbc/execute-one! ds
               ["SELECT EXISTS (
                    SELECT 1 FROM undo_log
                     WHERE id = ?
                       AND undone_at IS NULL
                       AND occurred_at > now() - interval '12 hours') AS exists"
                entry-id]
               {:builder-fn rs/as-unqualified-maps}))))

(defn mark-undone!
  "Stamp undone_at on a live entry. Idempotent: a no-op if already undone.
   Returns the jdbc update count (0 when already undone)."
  [entry-id]
  (::jdbc/update-count
   (jdbc/execute-one! ds
     (sql/format {:update :undo_log
                  :set {:undone_at [:now]}
                  :where [:and [:= :id entry-id] [:is :undone_at nil]]}))))

(defn restore-flashcards-tx!
  "Re-insert deleted flashcards on the given connectable, preserving original
   ids. ON CONFLICT DO NOTHING — a card still present is left untouched, and so
   is a card whose question or cloze a live card in its topic already holds.

   The arbiter is deliberately absent. `ON CONFLICT (id)` absorbs only the id,
   so a digest clash with `flashcard-unique-index-ddl` failed the WHOLE restore,
   and undo.clj reports that to nobody. Two shapes reach it: the user re-created
   a deleted card before undoing, or the snapshot predates the duplicate repair
   and holds both copies of a pair.

   Clears each card's Anki association (anki_note_id, anki_synced_at): the delete
   path removed the Anki notes, so a restored card must come back UNSYNCED rather
   than falsely rendering as pushed/synced."
  [conn snapshot-rows]
  (when (seq snapshot-rows)
    (jdbc/execute! conn
      (sql/format {:insert-into :flashcards
                   :values (mapv #(as-> % row
                                    (assoc row :anki_note_id nil :anki_synced_at nil)
                                    (cast-timestamps row flashcard-ts-cols)
                                    ;; JSONB round-trip left a plain map — re-wrap.
                                    (cond-> row
                                      (some? (:io_fields row))
                                      (update :io_fields ->jsonb)))
                                  snapshot-rows)
                   :on-conflict []
                   :do-nothing true}))))

(defn restore-flashcards!
  "Re-insert deleted flashcards from a snapshot (non-transactional)."
  [snapshot-rows]
  (restore-flashcards-tx! ds snapshot-rows))

(def ^:private group-ts-cols
  "Timestamp columns shared by occlusion_groups and score_groups."
  [:created_at :updated_at])

(defn- restore-group-rows-tx!
  "Re-insert deleted occlusion/score group rows on the given connectable,
   preserving original ids. ON CONFLICT (id) DO NOTHING — a group the delete
   never destroyed is left untouched. geometry came back from JSONB as a plain
   map and must be re-wrapped; timestamps came back as ISO strings and must be
   CAST. Restoring the row verbatim brings back anki_key and next_ordinal, so
   regenerated mask filenames stay consistent with surviving card_versions."
  [conn table snapshot-rows]
  (when (seq snapshot-rows)
    (jdbc/execute! conn
      (sql/format {:insert-into table
                   :values (mapv #(-> %
                                    (cast-timestamps group-ts-cols)
                                    (update :geometry ->jsonb))
                               snapshot-rows)
                   :on-conflict [:id]
                   :do-nothing true}))))

(defn restore-card-rows!
  "Reverse a card delete from its map-shaped snapshot.
   Pre:  snapshot = {:cards [...] :occlusion-groups [...] :score-groups [...]},
         as written by insert-card-delete-undo!.
   Post: every row is back under its original id. Group rows go in FIRST —
         flashcards.occlusion_group_id and .score_group_id are foreign keys, so
         a card inserted before its group is rejected. One transaction, so a
         failure restores nothing rather than a group with no cards.
         Cards come back UNSYNCED (restore-flashcards-tx!), so the next push
         re-creates their Anki notes and regenerates the group's mask media."
  [snapshot]
  (jdbc/with-transaction [tx ds]
    (restore-group-rows-tx! tx :occlusion_groups (:occlusion-groups snapshot))
    (restore-group-rows-tx! tx :score_groups (:score-groups snapshot))
    (restore-flashcards-tx! tx (:cards snapshot))))

(defn restore-pin!
  "Re-insert a removed pin from its snapshot, preserving the original id.
   ON CONFLICT (id) DO NOTHING. Bypasses the ≤2 cap (set-pin!): undo restores
   exactly what was removed, so the cap cannot be exceeded."
  [snapshot-row]
  (when snapshot-row
    (jdbc/execute! ds
      (sql/format {:insert-into :topic_pins
                   :values [snapshot-row]
                   :on-conflict [:id]
                   :do-nothing true}))))

;; ---------------------------------------------------------------------------
;; Staged document deletion — soft-hide a topic subtree + its binary, hard-
;; delete its cards (snapshotted), reversible for 12h as one undo_log entry.
;; A background scheduler hard-deletes staged subtrees once the window elapses.
;; ---------------------------------------------------------------------------

(defn- subtree-topic-ids
  "Ids of root-id and all its descendants (via parent_id)."
  [conn root-id]
  (mapv :id
    (jdbc/execute! conn
      ["WITH RECURSIVE subtree(id) AS (
          SELECT id FROM topics WHERE id = ?
          UNION ALL
          SELECT c.id FROM topics c JOIN subtree s ON c.parent_id = s.id)
        SELECT id FROM subtree" root-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn get-subtree-ids
  "Public: vector of topic-id and all its descendant ids. Used by the drag-and-
   drop nesting UI to grey out invalid (cycle-forming) drop targets."
  [topic-id]
  (subtree-topic-ids ds topic-id))

(defn get-flashcards-for-subtree
  "Flashcards for a topic and all its descendants (via parent_id). Same row
   shape as get-all-flashcards (coalesced page_number + card-group-selects) so
   the Anki-sync 'subtree' scope feeds the same downstream as 'self'/'document'.
   The card table reads it for every non-root topic, so a PDF page lists the
   cards of the extracts made from it.
   The subtree is a recursive CTE inside this one statement, never an id list:
   an IN over get-subtree-ids binds one parameter per topic, and a large folder
   topic would cross the 65,535-parameter bound.
   Pre:  topic-id is a topic id, or nil.
   Post: every card whose topic is topic-id or a descendant of it, newest
         first; nil → []."
  [topic-id]
  (if topic-id
    (mapv (comp strip-foreign-jsonb normalize-flashcard-row)
      (jdbc/execute! ds
        (sql/format {:with-recursive [[[:subtree {:columns [:id]}]
                                       {:union-all [{:select [:id] :from [:topics]
                                                     :where [:= :id topic-id]}
                                                    {:select [:c.id] :from [[:topics :c]]
                                                     :join [[:subtree :s] [:= :c.parent_id :s.id]]}]}]]
                     :select (into [[:f.*] [[:coalesce :t.page_number :parent.page_number] :page_number]]
                               card-group-selects)
                     :from [[:flashcards :f]]
                     :join [[:topics :t] [:= :f.topic_id :t.id]]
                     :left-join [[:topics :parent] [:= :t.parent_id :parent.id]
                                 [:occlusion_groups :og] [:= :f.occlusion_group_id :og.id]
                                 [:score_groups :sg] [:= :f.score_group_id :sg.id]]
                     :where [:in :f.topic_id {:select [:id] :from [:subtree]}]
                     :order-by [[:f.created_at :desc]]})))
    []))

(defn topic-scope-context
  "Scope-selector context for a topic: its kind and whether it has any
   non-staged child topic. {:kind <string> :has-children? <bool>}. nil topic-id
   → nil. Drives the Anki-sync scope label (kind) and the 'subtree' leaf gate."
  [topic-id]
  (when topic-id
    (let [row (jdbc/execute-one! ds
                (sql/format {:select [:kind
                                      [[:exists {:select [1]
                                                 :from [[:topics :c]]
                                                 :where [:and [:= :c.parent_id topic-id]
                                                         [:is :c.staged_delete_id nil]]}]
                                       :has_children]]
                             :from [:topics]
                             :where [:= :id topic-id]}))]
      {:kind (:topics/kind row)
       :has-children? (boolean (:has_children row))})))

(defn- subtree-owner
  "user_id of the root document above topic-id (root carries the owner;
   children may have NULL user_id). Returns nil if not found."
  [conn topic-id]
  (:user_id
   (jdbc/execute-one! conn
     ["WITH RECURSIVE anc(id, parent_id, user_id) AS (
         SELECT id, parent_id, user_id FROM topics WHERE id = ?
         UNION ALL
         SELECT t.id, t.parent_id, t.user_id FROM topics t JOIN anc a ON t.id = a.parent_id)
       SELECT user_id FROM anc WHERE parent_id IS NULL" topic-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn owns-topic?
  "True iff `user-id` owns `topic-id` — the root document above it carries the
   owner. Works for root or child topic-ids (walks parent_id to the root).
   Pre: user-id server-authoritative (from the Ring session). Post: boolean.
   The security predicate for topic/group ownership checks outside db.clj."
  [user-id topic-id]
  (boolean (and user-id topic-id (= user-id (subtree-owner ds topic-id)))))

(defn occlusion-group-owner
  "Owner (root user_id) of occlusion `group-id`, resolved via any member
   flashcard's root topic, or nil. occlusion_groups carries no owner column."
  [group-id]
  (:user_id
   (jdbc/execute-one! ds
     ["SELECT root.user_id FROM flashcards f
       JOIN topics root ON f.root_topic_id = root.id
       WHERE f.occlusion_group_id = ? LIMIT 1" group-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn score-group-owner
  "Owner (root user_id) of score `group-id`, resolved via any member flashcard's
   root topic, or nil. score_groups carries no owner column."
  [group-id]
  (:user_id
   (jdbc/execute-one! ds
     ["SELECT root.user_id FROM flashcards f
       JOIN topics root ON f.root_topic_id = root.id
       WHERE f.score_group_id = ? LIMIT 1" group-id]
     {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; Dismiss / Undismiss — remove a topic + its whole subtree from the Learning
;; Queue (SuperMemo Dismiss) while keeping it in the collection. Recursive by
;; design; orthogonal to status (Done survives a dismiss round-trip). Compare
;; done-topic!/restore-topic! (single-topic, status-column). One repetition
;; event is logged on the target only, not per descendant.
;; ---------------------------------------------------------------------------

(defn- set-subtree-dismissed!
  "Flip `dismissed` across topic-id + its whole subtree, logging one repetition
   `event-type` on topic-id. Shared body of dismiss-topic!/undismiss-topic!.
   Pre:  caller owns topic-id. Post: every subtree row has dismissed=`value`;
   exactly one `event-type` row on topic-id. Returns {:count n} | nil (not owned)."
  [user-id topic-id value event-type]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [ids    (subtree-topic-ids tx topic-id)
            before (snapshot-topic-row tx topic-id)]
        (insert-repetition! tx topic-id event-type before)
        (jdbc/execute! tx
          (sql/format {:update :topics
                       :set {:dismissed value}
                       :where [:in :id ids]}))
        {:count (count ids)}))))

(defn dismiss-topic!
  "Dismiss topic-id + its whole subtree (dismissed=true). Reversible via
   undismiss-topic!. Returns {:count n} | nil when not owned by user-id."
  [user-id topic-id]
  (set-subtree-dismissed! user-id topic-id true "dismiss"))

(defn undismiss-topic!
  "Reverse a dismiss: clear dismissed across topic-id + its whole subtree.
   Leaves each row's status untouched (a done child stays done).
   Returns {:count n} | nil when not owned by user-id."
  [user-id topic-id]
  (set-subtree-dismissed! user-id topic-id false "undismiss"))

(defn topic-dismissed?
  "True iff topic-id is currently dismissed. Drives the viewer toolbar's
   Dismiss/Undismiss toggle label. False for a missing topic."
  [topic-id]
  (boolean (:dismissed (jdbc/execute-one! ds
                         ["SELECT dismissed FROM topics WHERE id = ?" topic-id]
                         {:builder-fn rs/as-unqualified-maps}))))

(defn- over-quota?
  "True when the user's usage now exceeds an explicit quota (nil quota ⇒ unlimited)."
  [conn user-id]
  (let [{:keys [usage_bytes quota_bytes]}
        (jdbc/execute-one! conn
          ["SELECT usage_bytes, quota_bytes FROM users WHERE id = ?" user-id]
          {:builder-fn rs/as-unqualified-maps})]
    (boolean (and quota_bytes usage_bytes (> usage_bytes quota_bytes)))))

(defn stage-topic-for-deletion!
  "Hide topic-id + its whole subtree (set staged_delete_id) and hard-delete the
   subtree's flashcards (snapshotted into one 'delete-document' undo entry).
   Frees usage_bytes immediately. Pre: caller passes a topic the user owns.
   Returns {:entry-id :card-count :note-ids :parent-id :freed-bytes}, or nil
   when the topic is not owned by user-id (caller bug / stale client)."
  [user-id topic-id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [ids (subtree-topic-ids tx topic-id)
            parent-id (:parent_id
                       (jdbc/execute-one! tx
                         ["SELECT parent_id FROM topics WHERE id = ?" topic-id]
                         {:builder-fn rs/as-unqualified-maps}))
            deleted (jdbc/execute! tx
                      (sql/format {:delete-from :flashcards
                                   :where [:in :topic_id ids]
                                   :returning [:*]}))
            note-ids (into [] (keep :flashcards/anki_note_id) deleted)
            entry-id (insert-undo-entry-raw! tx user-id "delete-document" "document"
                       [topic-id] {:cards (mapv unqualify-row deleted)})
            freed (subtree-file-bytes tx topic-id)]
        ;; Staging DOES destroy the Anki notes — the caller hands `:note-ids`
        ;; straight to `try-delete-anki-notes!` (knowledge_tree.cljc:447), and
        ;; `restore-flashcards-tx!` brings a card back UNSYNCED because of it.
        ;; So the tombstone belongs here and not at purge time: the note is
        ;; already being destroyed and the client attempt is fire-and-forget.
        ;; An undo does not resurrect the note, so the tombstone stays correct.
        (record-anki-tombstones! tx user-id deleted)
        (jdbc/execute! tx
          (sql/format {:update :topics
                       :set {:staged_delete_id entry-id}
                       :where [:in :id ids]}))
        (when (pos? freed) (bump-user-usage! tx user-id (- freed)))
        (prune-undo-log! tx user-id)
        {:entry-id entry-id :card-count (count deleted)
         :note-ids note-ids :parent-id parent-id :freed-bytes freed}))))

(defn restore-staged-document!
  "Reverse a staged deletion: re-insert the cards, clear staged_delete_id on the
   subtree, re-add the freed usage_bytes. `entry` is a parsed undo_log row
   (:id, :entity_refs [topic-id], :snapshot {:cards [...]}).
   Returns {:over-quota? bool :card-count n}."
  [user-id entry]
  (jdbc/with-transaction [tx ds]
    (let [cards (:cards (:snapshot entry))
          topic-id (first (:entity_refs entry))]
      (restore-flashcards-tx! tx cards)
      (jdbc/execute! tx
        (sql/format {:update :topics
                     :set {:staged_delete_id nil}
                     :where [:= :staged_delete_id (:id entry)]}))
      (let [bytes (subtree-file-bytes tx topic-id)]
        (when (pos? bytes) (bump-user-usage! tx user-id bytes))
        {:over-quota? (over-quota? tx user-id) :card-count (count cards)}))))

;; ---------------------------------------------------------------------------
;; Topic re-parenting — custom nesting via drag-and-drop. A move is a single
;; parent_id update; the whole subtree follows (children reference the node,
;; not its ancestors). Cycle-guarded because the recursive CTEs above have no
;; visited check — a cycle would loop them forever, not just corrupt display.
;; ---------------------------------------------------------------------------

;; `structural-kinds` is defined above, beside the review queues that share it.
;; Its members' parent_id is structural, not user-arranged: a page stub's
;; identity is (parent_id = pdf-root, page_number), and get-page-text keys the
;; editor content off that pair — reparenting one orphans its content. A repo,
;; commit or dir node carries the same property through repo_path. These may
;; never be moved by reparent-topic!.

(defn reparent-topic!
  "Move topic-id under new-parent-id (nil ⇒ promote to root) for user-id.
   Pre: user owns topic-id. Rejected (returns nil) when the move is not owned,
   would create a cycle (new-parent is topic-id or one of its descendants),
   targets another user's topic, new-parent already equals the current parent
   (no-op), or topic-id is a structural kind (page stub). On success writes one
   reversible 'move-topic' undo entry capturing the prior parent and returns
   {:entry-id _ :old-parent-id _ :new-parent-id _}."
  [user-id topic-id new-parent-id]
  (jdbc/with-transaction [tx ds]
    (when (= user-id (subtree-owner tx topic-id))
      (let [{old-parent-id :parent_id kind :kind}
            (jdbc/execute-one! tx
              ["SELECT parent_id, kind FROM topics WHERE id = ?" topic-id]
              {:builder-fn rs/as-unqualified-maps})
            forbidden (set (subtree-topic-ids tx topic-id))]
        (when (and (not (structural-kinds kind))
                (not= old-parent-id new-parent-id)
                (not (contains? forbidden new-parent-id))
                (or (nil? new-parent-id)
                  (= user-id (subtree-owner tx new-parent-id))))
          (let [entry-id (insert-undo-entry-raw! tx user-id "move-topic" "document"
                           [topic-id] {:topic-id topic-id :old-parent-id old-parent-id})]
            (jdbc/execute! tx
              (sql/format {:update :topics
                           :set {:parent_id new-parent-id}
                           :where [:= :id topic-id]}))
            (prune-undo-log! tx user-id)
            {:entry-id entry-id :old-parent-id old-parent-id :new-parent-id new-parent-id}))))))

(defn restore-topic-parent!
  "Undo a move: set topic's parent_id back to the snapshot's :old-parent-id.
   Idempotent — a plain UPDATE, safe to replay. `entry` is a parsed undo_log row."
  [entry]
  (let [{:keys [topic-id old-parent-id]} (:snapshot entry)]
    (when topic-id
      (jdbc/execute! ds
        (sql/format {:update :topics
                     :set {:parent_id old-parent-id}
                     :where [:= :id topic-id]})))))

(defn push-bibliography-to-descendants!
  "Copy topic-id's effective bibliography into every descendant extract as a
   fresh private sources row (overwrite-all). Excludes kind='page' descendants
   and topic-id itself, so each affected extract owns an independent copy.
   Pre:  user-id owns the subtree root above topic-id.
   Post: {:ok true :count n :topic-ids [...] :csl m} — n descendants re-pointed to
         new source copies;
         {:ok false :error :no-source} when topic-id has no effective source;
         {:ok false :error :not-owned} when ownership fails.
   Invariant: one transaction; prior descendant source rows are left in place
   (sources are not garbage-collected here).

   CALLER OBLIGATION: `:topic-ids` and `:csl` are returned so the caller can call
   `set-anki-bibliography-snapshot-for-topics!` with the rendered citation. This
   is overwrite-all, so a descendant that carried a DIFFERENT citation now carries
   this one, and its cards' Anki notes are out of date. The fan-out is not folded
   in for the reason `update-source!` gives — rendering the citation belongs to
   `anki-sync-helpers` — and it runs outside this transaction, because
   `set-anki-bibliography-snapshot-for-topics!` opens its own."
  [user-id topic-id]
  (let [src-id (resolve-effective-source-id topic-id)]
    (if-not src-id
      {:ok false :error :no-source}
      (let [src      (get-source src-id)
            csl      (:sources/csl src)
            csl-type (:sources/csl_type src)
            url      (:sources/url src)
            title    (:sources/title src)]
        (jdbc/with-transaction [tx ds]
          (if-not (= user-id (subtree-owner tx topic-id))
            {:ok false :error :not-owned}
            (let [descendants (jdbc/execute! tx
                                ["WITH RECURSIVE subtree(id, kind) AS (
                                    SELECT id, kind FROM topics WHERE id = ?
                                    UNION ALL
                                    SELECT c.id, c.kind FROM topics c
                                      JOIN subtree s ON c.parent_id = s.id)
                                  SELECT id FROM subtree
                                   WHERE id <> ? AND (kind IS DISTINCT FROM 'page')"
                                 topic-id topic-id]
                                {:builder-fn rs/as-unqualified-maps})]
              (doseq [{:keys [id]} descendants]
                (let [new-src (jdbc/execute-one! tx
                                (sql/format {:insert-into :sources
                                             :values [{:user_id         user-id
                                                       :csl_type        csl-type
                                                       :csl             (->jsonb csl)
                                                       :url             url
                                                       :title           title
                                                       :container_title (when (map? csl)
                                                                          (:container-title csl))}]
                                             :returning [:id]}))]
                  (jdbc/execute! tx
                    ["UPDATE topics SET source_id = ? WHERE id = ?"
                     (:sources/id new-src) id])))
              {:ok true :count (count descendants)
               :topic-ids (mapv :id descendants)
               :csl csl})))))))

(defn purge-staged-documents!
  "Hard-delete topic subtrees whose staging entry has aged past the 12h window,
   then drop those entries. CASCADE clears the binary + remaining rows.
   usage_bytes is untouched (already freed at stage time). Returns topics deleted.

   §4.7 7.1 — this is where video large objects are unlinked, NOT at staging
   time: staging is reversible for 12 h, and `restore-staged-document!` must be
   able to hand back a playable video. The OIDs are read before the DELETE (the
   topic_videos rows cascade away with the topics) and unlinked after it, in the
   same transaction. A purged repository node's git clone is removed once that
   transaction commits (plans/commit-range-walkthrough.md §6.1.5); staging
   keeps it, as it keeps the large objects."
  []
  (let [[n repo-ids]
        (jdbc/with-transaction [tx ds]
    (let [repo-ids (mapv :id (jdbc/execute! tx
                               ["SELECT id FROM topics WHERE kind = 'repo' AND staged_delete_id IN
                                   (SELECT id FROM undo_log
                                     WHERE action_type = 'delete-document'
                                       AND occurred_at < now() - interval '12 hours')"]
                               {:builder-fn rs/as-unqualified-maps}))
          oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id
                     WHERE t.staged_delete_id IN
                       (SELECT id FROM undo_log
                        WHERE action_type = 'delete-document'
                          AND occurred_at < now() - interval '12 hours')
                     UNION ALL
                     SELECT te.lo_oid FROM topic_epubs te
                     JOIN topics t ON te.topic_id = t.id
                     WHERE t.staged_delete_id IN
                       (SELECT id FROM undo_log
                        WHERE action_type = 'delete-document'
                          AND occurred_at < now() - interval '12 hours')"]
                   {:builder-fn rs/as-unqualified-maps}))
          deleted (jdbc/execute! tx
                    ["DELETE FROM topics
                      WHERE staged_delete_id IN
                        (SELECT id FROM undo_log
                         WHERE action_type = 'delete-document'
                           AND occurred_at < now() - interval '12 hours')"])
          n (or (some-> deleted first ::jdbc/update-count) 0)]
      (doseq [oid oids] (lo/unlink! tx oid))
      (jdbc/execute! tx
        ["DELETE FROM undo_log
          WHERE action_type = 'delete-document'
            AND occurred_at < now() - interval '12 hours'"])
      [n repo-ids]))]
    (delete-repo-clones! repo-ids)
    n))

(defn purge-staged-curricula!
  "Hard-delete curricula whose staged Undo entries are older than 12 hours.
   Delete the root before its entry because the entry owns the staged marker."
  []
  (jdbc/with-transaction [tx ds]
    (let [deleted (jdbc/execute! tx
                    ["DELETE FROM curricula
                      WHERE staged_delete_id IN
                        (SELECT id FROM undo_log
                          WHERE action_type = 'delete-curriculum'
                            AND occurred_at < now() - interval '12 hours')"])
          n (or (some-> deleted first ::jdbc/update-count) 0)]
      (jdbc/execute! tx
        ["DELETE FROM undo_log
          WHERE action_type = 'delete-curriculum'
            AND occurred_at < now() - interval '12 hours'"])
      n)))

(defn purge-staged-assistant-chats!
  "Hard-delete chats whose staged undo entries are older than 12 hours, then
   those entries. The chat goes first because the entry owns its marker. The
   cascade removes messages, runs, checkpoints and evidence."
  []
  (jdbc/with-transaction [tx ds]
    (let [deleted (jdbc/execute! tx
                    ["DELETE FROM assistant_chats
                      WHERE staged_delete_id IN
                        (SELECT id FROM undo_log
                          WHERE action_type = 'delete-assistant-chat'
                            AND occurred_at < now() - interval '12 hours')"])
          n (or (some-> deleted first ::jdbc/update-count) 0)]
      (jdbc/execute! tx
        ["DELETE FROM undo_log
          WHERE action_type = 'delete-assistant-chat'
            AND occurred_at < now() - interval '12 hours'"])
      n)))

(defn purge-orphan-video-objects!
  "§4.7 7.4 — unlink every large object this role owns that no live row
   references: neither a `topic_videos` row nor an in-flight
   `upload_sessions` row.

   Catches the leaks no deletion path can: a crashed finalize between
   `lo_create` and the INSERT, a manual SQL delete, a restored-from-backup
   mismatch. Scoped to `lomowner = current_user` so a large object belonging to
   another role in the same database is never touched.

   Pre:  none. Post: returns the number unlinked. Safe to run repeatedly."
  []
  (jdbc/with-transaction [tx ds]
    (let [orphans (mapv :oid
                    (jdbc/execute! tx
                      ["SELECT m.oid FROM pg_largeobject_metadata m
                        WHERE m.lomowner = (SELECT oid FROM pg_roles WHERE rolname = current_user)
                          AND NOT EXISTS (SELECT 1 FROM topic_videos tv WHERE tv.lo_oid = m.oid)
                          AND NOT EXISTS (SELECT 1 FROM topic_epubs te WHERE te.lo_oid = m.oid)
                          AND NOT EXISTS (SELECT 1 FROM upload_sessions s WHERE s.lo_oid = m.oid)"]
                      {:builder-fn rs/as-unqualified-maps}))]
      (doseq [oid orphans] (lo/unlink! tx oid))
      (when (pos? (count orphans))
        (tel/log! {:level :warn :id ::orphan-large-objects :data {:count (count orphans)}}
          "Unlinked orphaned large objects"))
      (count orphans))))

(defn reclaim-unowned-videos!
  "§4.7 7.3 — reclaim videos whose topic has no owner.

   `topics.user_id` is ON DELETE SET NULL, so deleting a user orphans their
   topics instead of cascading them: the topic_videos row survives, the large
   object stays referenced (so 7.4's detector will not see it), and nobody can
   reach or be billed for the bytes. Delete those topics outright, which
   cascades the row, then unlink.

   Prefer `delete-user!` — this is the backstop for a user removed by raw SQL."
  []
  (jdbc/with-transaction [tx ds]
    (let [oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id
                     WHERE t.user_id IS NULL
                     UNION ALL
                     SELECT te.lo_oid FROM topic_epubs te
                     JOIN topics t ON te.topic_id = t.id
                     WHERE t.user_id IS NULL"]
                   {:builder-fn rs/as-unqualified-maps}))]
      (when (seq oids)
        (jdbc/execute! tx
          ["DELETE FROM topics WHERE user_id IS NULL
            AND (id IN (SELECT topic_id FROM topic_videos)
                 OR id IN (SELECT topic_id FROM topic_epubs))"])
        (doseq [oid oids] (lo/unlink! tx oid))
        (tel/log! {:level :warn :id ::unowned-videos :data {:count (count oids)}}
          "Reclaimed videos belonging to deleted users"))
      (count oids))))

(defn delete-user!
  "§4.7 7.3 — remove a user and everything they own. REPL/SQL admin path.

   Deletes the user's topics FIRST so `topic_videos` cascades while the OIDs are
   still reachable, unlinks them, then deletes the user row. Doing it in the
   other order would trip `topics.user_id ON DELETE SET NULL` and strand the
   bytes. Returns {:topics n :objects n}."
  [user-id]
  (jdbc/with-transaction [tx ds]
    (let [oids (mapv :lo_oid
                 (jdbc/execute! tx
                   ["SELECT tv.lo_oid FROM topic_videos tv
                     JOIN topics t ON tv.topic_id = t.id WHERE t.user_id = ?
                     UNION ALL
                     SELECT te.lo_oid FROM topic_epubs te
                     JOIN topics t ON te.topic_id = t.id WHERE t.user_id = ?"
                    user-id user-id]
                   {:builder-fn rs/as-unqualified-maps}))
          deleted (jdbc/execute! tx ["DELETE FROM topics WHERE user_id = ?" user-id])]
      (doseq [oid oids] (lo/unlink! tx oid))
      (jdbc/execute! tx ["DELETE FROM users WHERE id = ?" user-id])
      (log/audit! {:id ::user-deleted :user-id user-id :action :delete
                   :entity :document :entity-id user-id})
      {:topics (or (some-> deleted first ::jdbc/update-count) 0)
       :objects (count oids)})))

(defonce ^:private purge-scheduler (atom nil))

(defonce ^{:doc "Extra jobs the hourly sweep runs after the staged-document purge.
   Registered by freememo.storage-meter and freememo.video-upload at namespace
   load so db.clj keeps no dependency on either. Each entry is [label thunk];
   a throwing thunk is logged and does not stop the others."}
  !sweep-jobs (atom []))

(defn register-sweep-job!
  "Add [label thunk] to the hourly sweep. Idempotent per label — re-registers
   replace, so a namespace reload does not double up the job."
  [label thunk]
  (swap! !sweep-jobs (fn [jobs] (conj (vec (remove #(= label (first %)) jobs)) [label thunk])))
  nil)

(defn- run-sweep!
  "One pass of the hourly sweep: staged purges, then the large-object reclaimers,
   then every registered job. Total — each step's failure is logged and the rest
   still run, because a wedged metering job must not stop deletions."
  []
  (doseq [[label thunk] (into [[::purge-staged-documents purge-staged-documents!]
                               [::purge-staged-curricula purge-staged-curricula!]
                               [::purge-staged-assistant-chats purge-staged-assistant-chats!]
                               [::reclaim-unowned-videos reclaim-unowned-videos!]
                               [::purge-orphan-video-objects purge-orphan-video-objects!]]
                         @!sweep-jobs)]
    (try (thunk)
      (catch Throwable t (tel/error! {:id label} t)))))

(defn start-purge-scheduler!
  "Start the hourly sweep (staged-document purge + large-object reclamation +
   registered jobs). Idempotent — once per process."
  []
  (when-not @purge-scheduler
    (let [exec (java.util.concurrent.Executors/newSingleThreadScheduledExecutor)]
      (.scheduleAtFixedRate exec run-sweep!
        1 60 java.util.concurrent.TimeUnit/MINUTES)
      (reset! purge-scheduler exec)
      (tel/log! :info "Hourly sweep scheduler started"))))

;; ---------------------------------------------------------------------------
;; Incremental Video (see plans/incremental-video.md)
;;
;; Byte ownership, stated once because three tables have to agree:
;;   users.usage_bytes = SUM(topic_files.file_size) + SUM(topic_videos.byte_size)
;; Bytes are RESERVED at upload init (against the declared total, via
;; quota/check-and-bump!) and only released by a deletion path or by an aborted/
;; reaped session. The large object and the reservation are created and undone
;; together — a path that does one without the other leaks.
;; ---------------------------------------------------------------------------

;; Casting note: Postgres has no implicit bigint → oid coercion, and pgjdbc
;; sends a Clojure long as int8. Every OID bind therefore goes through
;; CAST(CAST(? AS bigint) AS oid).
(def ^:private oid-param "CAST(CAST(? AS bigint) AS oid)")

(defn get-topic-video
  "topic_videos row for a topic, or nil. Unqualified keys."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT topic_id, lo_oid, byte_size, mime_type, duration_ms, last_pos_ms
      FROM topic_videos WHERE topic_id = ?" topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-topic-video-for-user
  "topic_videos row joined to its topic and scoped to `user-id`; nil when the
   topic is missing, staged for deletion, or owned by somebody else.
   The single ownership gate for playback and processing."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["SELECT tv.topic_id, tv.lo_oid, tv.byte_size, tv.mime_type,
             tv.duration_ms, tv.last_pos_ms, tv.remux_pending, t.title
      FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
      WHERE tv.topic_id = ? AND t.user_id = ? AND t.staged_delete_id IS NULL"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn replace-video-bytes!
  "§12.4 2.1/2.2 — swap a video's stored bytes for `file`, atomically with its
   size, MIME and duration.

   Rewrites the SAME large object rather than allocating a new OID: every sweep
   job, the playback handler and `usage_bytes` all key off `lo_oid`, and
   `lo/write-file!` truncates before writing, so the object ends exactly
   `file`'s length. One transaction, because a size that disagrees with the
   object is a `Content-Length` that lies.

   Not gated by quota, matching `save-video-audio!`: the delta is a container
   rewrite of bytes the quota already accepted (measured +0.59 % on 192 MB).
   `usage_bytes` still moves, so any overage stays visible and is recovered on
   deletion.

   Pre:  topic-id has a topic_videos row; `file` is readable; the caller owns
         the topic (checked upstream — this function does not re-check).
   Post: {:ok true :size n}; remux_pending is FALSE, so the bytes are now
         immutable and cacheable.
   Invariant: byte_size = the object's real length = the delta applied to
              usage_bytes."
  [user-id topic-id ^java.io.File file mime-type duration-ms]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, byte_size FROM topic_videos
                  WHERE topic_id = ? FOR UPDATE" topic-id]
                {:builder-fn rs/as-unqualified-maps})]
      (if (nil? row)
        {:ok false :error "Video row not found"}
        (let [prior (long (:byte_size row))
              size (lo/write-file! tx (:lo_oid row) file)]
          (jdbc/execute! tx
            ["UPDATE topic_videos
              SET byte_size = ?, mime_type = ?, duration_ms = COALESCE(?, duration_ms),
                  remux_pending = FALSE
              WHERE topic_id = ?"
             size mime-type (when duration-ms (int duration-ms)) topic-id])
          (bump-user-usage! tx user-id (- size prior))
          {:ok true :size size})))))

(defn clear-video-remux-pending!
  "Mark a video's bytes settled — no remux will replace them.

   Called on every terminal outcome of the pipeline's normalization stage,
   including failure: after a remux that MP4 cannot accept, the uploaded bytes
   ARE the final bytes, and leaving the flag set would suppress caching forever
   for a file that never changes again."
  [topic-id]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET remux_pending = FALSE WHERE topic_id = ?" topic-id]))

(defn set-video-duration!
  "Record the ffprobe-measured duration (§4.4 4.3)."
  [topic-id duration-ms]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET duration_ms = ? WHERE topic_id = ?"
     (when duration-ms (int duration-ms)) topic-id]))

(defn save-video-position!
  "Persist the resume position (§4.8 8.7). Ownership-scoped so a forged topic-id
   writes nothing. Clamped at 0; the caller writes on pause/unmount, not per frame."
  [user-id topic-id pos-ms]
  (jdbc/execute! ds
    ["UPDATE topic_videos SET last_pos_ms = GREATEST(0, ?)
      WHERE topic_id = ? AND topic_id IN (SELECT id FROM topics WHERE user_id = ?)"
     (int (max 0 (or pos-ms 0))) topic-id user-id]))

(defn save-video-audio!
  "Store the ffmpeg-extracted MP3 as the video topic's role='audio' blob,
   replacing any previous one, and keep `usage_bytes` in step.

   Deliberately NOT gated by `quota/check-and-bump!`: the extraction is a
   derived artifact of an upload the quota already accepted, and it is ~2 % of
   the video's size. Refusing it here would leave a stored video that can never
   be played back on the waveform or transcribed — a worse state than being a
   few MB over. The counter is still incremented, so the overage is visible and
   is recovered on the next deletion.

   Post: exactly one role='audio' row for the topic; usage_bytes adjusted by the
   signed difference against whatever it replaced."
  [user-id topic-id ^bytes audio-bytes]
  (let [size (alength audio-bytes)]
    (jdbc/with-transaction [tx ds]
      (let [prior (or (:file_size (jdbc/execute-one! tx
                                    ["SELECT file_size FROM topic_files
                                      WHERE topic_id = ? AND role = 'audio'" topic-id]
                                    {:builder-fn rs/as-unqualified-maps}))
                    0)]
        (jdbc/execute! tx
          ["INSERT INTO topic_files (topic_id, role, file_data, file_size, mime_type)
            VALUES (?, 'audio', ?, ?, 'audio/mpeg')
            ON CONFLICT (topic_id, role)
            DO UPDATE SET file_data = excluded.file_data,
                          file_size = excluded.file_size,
                          mime_type = excluded.mime_type"
           topic-id audio-bytes size])
        (bump-user-usage! tx user-id (- size prior))
        {:ok true :size size}))))

;; ── Transcript (§4.4 / §4.8) ───────────────────────────────────────────────

(def ^:private transcript-insert-chunk
  "Rows per INSERT. 6 columns × 5000 rows stays far under Postgres' 65 535
   bind-parameter ceiling."
  5000)

(defn replace-video-transcript!
  "Replace a video's whole transcript with `segments`, each
   {:ord :start-ms :end-ms :text}. One transaction, so a partial transcript is
   never visible. Re-transcribing is therefore idempotent from the reader's side."
  [topic-id segments]
  (jdbc/with-transaction [tx ds]
    (jdbc/execute! tx ["DELETE FROM video_transcripts WHERE topic_id = ?" topic-id])
    (doseq [batch (partition-all transcript-insert-chunk segments)]
      (jdbc/execute! tx
        (sql/format {:insert-into :video_transcripts
                     :values (mapv (fn [s]
                                     {:topic_id topic-id
                                      :ord (int (:ord s))
                                      :start_ms (int (:start-ms s))
                                      :end_ms (int (:end-ms s))
                                      :text (sanitize-utf8 (:text s))})
                               batch)})))
    (count segments)))

(defn get-video-transcript-count
  "Segment count for a video — the virtual-scroll record count."
  [topic-id]
  (or (:c (jdbc/execute-one! ds
            ["SELECT COUNT(*) AS c FROM video_transcripts WHERE topic_id = ?" topic-id]
            {:builder-fn rs/as-unqualified-maps}))
    0))

(defn get-video-transcript
  "Every transcript segment for a video, in `ord` order.

   Read into a SERVER-FORM binding by the transcript pane, never into an e/defn
   return: the caller reads `(e/server (nth segs i nil))` per visible row, so a
   three-hour transcript stays server-side and only the scroll window crosses
   the wire (CLAUDE.md, 'e/defn Returns Materialize at the Call Site')."
  [topic-id]
  (jdbc/execute! ds
    ["SELECT id, ord, start_ms, end_ms, text FROM video_transcripts
      WHERE topic_id = ? ORDER BY ord" topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn copy-video-transcript-to-content!
  "§4.8 8.6 — replace a video topic's `content` with its whole transcript.

   The one bridge between the reference transcript and the editable body. It is
   an explicit user action, not a default, because the design decision was that
   `content` starts empty and the user curates into it: auto-filling would make
   every video's content a wall of unedited speech-to-text that card generation
   then chews through.

   Post: {:ok true :segments n}, or {:ok false :error S} when there is no
   transcript to copy."
  [topic-id]
  (let [segs (get-video-transcript topic-id)]
    (if (empty? segs)
      {:ok false :error "No transcript yet"}
      (let [html (->> segs
                   (map :text)
                   (map str/trim)
                   (remove str/blank?)
                   (map #(str "<p>" (text/escape-html %) "</p>"))
                   (str/join))]
        (update-topic-content! topic-id html)
        {:ok true :segments (count segs)}))))

(defn get-video-transcript-overlapping
  "Segments of `topic-id` overlapping [start-ms, end-ms), in `ord` order.

   Overlap, not containment (§4.9 9.1): a marked range whose edges fall
   mid-segment must include both partial segments, or the extract loses the
   half-sentences at each end. Two segments overlap iff each starts before the
   other ends."
  [topic-id start-ms end-ms]
  (jdbc/execute! ds
    ["SELECT id, ord, start_ms, end_ms, text FROM video_transcripts
      WHERE topic_id = ? AND start_ms < ? AND end_ms > ? ORDER BY ord"
     topic-id (int end-ms) (int start-ms)]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-video-segment
  "The [start_ms, end_ms) range an extract was cut from, or nil for an ordinary
   topic. Provenance only — it never rewrites the extract's content."
  [topic-id]
  (jdbc/execute-one! ds
    ["SELECT start_ms, end_ms FROM video_segments WHERE topic_id = ? ORDER BY id LIMIT 1"
     topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn create-video-extract!
  "§4.9 — create an extract child of a video topic over [start-ms, end-ms).

   The extract is an ORDINARY topic: `content` is the overlapping transcript
   text copied in at creation, editable thereafter, and the `video_segments` row
   records where it came from. Nothing re-derives it, so the extract survives
   the video's bytes being reclaimed.

   Pre:  user-id owns video-topic-id; end-ms > start-ms >= 0.
   Post: {:success true :id N}, or {:success false :error S}.
   Throws: ex-info ::bad-range before touching the DB on a malformed range
           (mirrors score/validate-segment).

   Not atomic across the topic and its range row: `create-topic!` owns its own
   connection (audit trail, title validation), so a failure between the two
   leaves an extract with its text but no recorded range. Tolerable precisely
   because the range is provenance and nothing reads through it — the extract
   is complete and reviewable without it."
  [user-id video-topic-id start-ms end-ms title]
  (when-not (and (int? start-ms) (int? end-ms) (>= start-ms 0) (> end-ms start-ms))
    (throw (ex-info "Malformed video segment"
             {:type ::bad-range :start-ms start-ms :end-ms end-ms})))
  (if-not (get-topic-video-for-user user-id video-topic-id)
    {:success false :error "Video not found"}
    (let [segs (get-video-transcript-overlapping video-topic-id start-ms end-ms)
          content (if (seq segs)
                    (->> segs
                      (map :text)
                      (map str/trim)
                      (remove str/blank?)
                      (map #(str "<p>" (text/escape-html %) "</p>"))
                      (str/join))
                    "")
          inherited (clone-source! user-id (resolve-effective-source-id video-topic-id))
          created (create-topic! {:parent-id video-topic-id
                                  :user-id user-id
                                  :content content
                                  :kind "basic"
                                  :title title
                                  :source-id inherited
                                  :extract? true})
          extract-id (:topics/id created)]
      (jdbc/execute! ds
        ["INSERT INTO video_segments (topic_id, start_ms, end_ms) VALUES (?, ?, ?)"
         extract-id (int start-ms) (int end-ms)])
      {:success true :id extract-id})))

;; ── Chunked upload sessions (§4.3) ─────────────────────────────────────────

(defn init-upload-session!
  "§4.3 3.1/3.1.1/3.1.2 — open an upload session for `flow`.

   One transaction does three things that must not come apart: reserve
   `total-bytes` against the user's quota, create the large object, and record
   the session. A quota rejection therefore leaves no object behind, and a
   successful init has already paid for every byte the client may send.

   Gated by `quota/reserve-bytes!`, NOT `check-and-bump!`: the latter also
   enforces `upload_max_bytes`, which is the single-HTTP-request ceiling
   (100 MB by default) that chunking exists to get past. Applying it here
   rejected every video over 100 MB before a byte was sent. The per-upload
   bound is the caller's: `video-http/max-video-bytes` for video,
   `archive/max-archive-bytes` for an archive.

   `flow` decides what finalize does with the completed object, and nothing
   else. Video keeps it as the stored artifact; an archive extracts it and
   refunds every reserved byte.

   Pre:  total-bytes > 0 and within the caller's per-upload ceiling; user-id
         exists; parent-id nil or a topic they own; flow is \"video\" or
         \"archive\".
   Post: {:ok true :session-id S :lo-oid N}.
   Throws: quota/quota-error :over-quota — the tx aborts, unwinding both the
           reservation and the object."
  [user-id flow filename mime-type total-bytes parent-id]
  (jdbc/with-transaction [tx ds]
    (quota/reserve-bytes! tx user-id total-bytes)
    (let [oid (lo/create! tx)
          session-id (str (random-uuid))]
      (jdbc/execute! tx
        [(str "INSERT INTO upload_sessions
                 (id, user_id, lo_oid, total_bytes, filename, mime_type, parent_id, flow)
               VALUES (?, ?, " oid-param ", ?, ?, ?, ?, ?)")
         session-id user-id oid (long total-bytes) filename mime-type parent-id flow])
      {:ok true :session-id session-id :lo-oid oid})))

(defn get-upload-session
  "Session row scoped to its owner, or nil. Unqualified keys."
  [user-id session-id]
  (jdbc/execute-one! ds
    ["SELECT id, user_id, lo_oid, total_bytes, received_bytes, filename, mime_type,
             parent_id, flow, archive_path
      FROM upload_sessions WHERE id = ? AND user_id = ?" session-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn append-upload-chunk!
  "§4.3 3.1.2/3.1.3 — append one chunk to a session's large object.

   Locks the session row FOR UPDATE, which serializes concurrent chunk POSTs
   for the same session: `lo/append!` writes at the object's current end, so
   two unserialized appenders would interleave bytes.

   Pre:  session exists and belongs to user-id.
   Post: {:ok true :received N :total N} with exactly one chunk in heap, or
         {:ok false :error S :code C} — :code \"overflow\" when the chunk would
         push past the declared total, \"not-found\" when the session is gone.
   Invariant: received_bytes always equals the large object's real size."
  [user-id session-id ^bytes buf n]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes FROM upload_sessions
                  WHERE id = ? AND user_id = ? FOR UPDATE" session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload session not found" :code "not-found"}

        (> (+ (:received_bytes row) (long n)) (:total_bytes row))
        {:ok false
         :error "Chunk exceeds the declared upload size"
         :code "overflow"}

        :else
        (let [new-size (lo/append! tx (:lo_oid row) buf n)]
          (jdbc/execute! tx
            ["UPDATE upload_sessions SET received_bytes = ?, updated_at = now()
              WHERE id = ?" new-size session-id])
          {:ok true :received new-size :total (:total_bytes row)})))))

(defn- delete-staged-path!
  "Best-effort removal of an upload session's `archive_path`.

   The archive flows stage one FILE there. The forge flow stages the extracted
   DIRECTORY, because its picker reads the tree before any row exists. A plain
   `.delete` returns false on a non-empty directory and removes nothing, so both
   owners of the column — release and the reap sweep — go through this.
   Post: nothing remains at `path`, unless the filesystem refused a delete.
   Invariant: never throws, and never follows a symbolic link out of `path`."
  [path]
  (when path
    (try
      (let [root (.toPath (java.io.File. ^String path))]
        (when (java.nio.file.Files/exists root (make-array java.nio.file.LinkOption 0))
          (with-open [paths (java.nio.file.Files/walk root
                              (make-array java.nio.file.FileVisitOption 0))]
            (doseq [^java.nio.file.Path p (reverse (iterator-seq (.iterator paths)))]
              (try (java.nio.file.Files/deleteIfExists p) (catch Exception _ nil))))))
      (catch Exception _ nil))))

(defn release-upload-session!
  "§4.3 3.1.4 — give back everything a session holds: unlink the object, delete
   the materialized archive, refund the reservation, drop the row. Idempotent;
   unknown ids are no-ops.

   Two callers, one operation. A cancelled or failed upload releases because
   the bytes were never wanted. A COMPLETED archive import releases for the
   opposite reason — the archive was transport, the import already stored what
   it needed through `upsert-media!`, and leaving the reservation would bill the
   user for a file that no longer exists. Video's finalize is the only path that
   does NOT release: it transfers the object to `topic_videos` instead, because
   there the bytes ARE the artifact.

   Refunds `total_bytes`, not `received_bytes`, because init reserved the
   declared total.

   Pre:  none — an unknown or already-released id is a no-op.
   Post: nil for an unknown id, else {:ok true :refunded N}. No
         `pg_largeobject` row and no `archive_path` file remain, and
         `usage_bytes` is down by exactly `total_bytes`.
   Invariant: the file delete is best-effort and must not roll the tx back — a
         refund lost to a stuck file handle is worse than a stray temp file,
         which the reap sweep would collect anyway."
  [user-id session-id]
  (jdbc/with-transaction [tx ds]
    (when-let [row (jdbc/execute-one! tx
                     ["SELECT lo_oid, total_bytes, archive_path FROM upload_sessions
                       WHERE id = ? AND user_id = ? FOR UPDATE" session-id user-id]
                     {:builder-fn rs/as-unqualified-maps})]
      (lo/unlink! tx (:lo_oid row))
      (delete-staged-path! (:archive_path row))
      (jdbc/execute! tx ["DELETE FROM upload_sessions WHERE id = ?" session-id])
      (bump-user-usage! tx user-id (- (:total_bytes row)))
      {:ok true :refunded (:total_bytes row)})))

(defn materialize-upload-session!
  "Put a completed session's bytes on local disk and return the File.

   The large object is authoritative; `archive_path` is a cache. A cached path
   that still exists and matches `received_bytes` is returned as is, so the
   common case reads the object once (at finalize) rather than twice. A blue-
   green deploy flip lands the import in a different container, where the path
   does not exist — that re-materializes rather than failing, which is the whole
   reason the object stays authoritative.

   Pre:  session exists, belongs to user-id, and its flow is `expected-flow`.
   Post: {:ok true :file F :filename S} where F holds exactly `received_bytes`
         bytes, or {:ok false :error S} when the session is missing, has the
         wrong flow, or the upload is short of its declared total.
   Invariant: heap high-water is one `largeobj/io-buffer-bytes` buffer — the
         copy streams, so a 7.5 GB archive never exists in heap.
   Invariant: the returned File is owned by the SESSION, not the caller. Only
         `release-upload-session!` and the reap sweep delete it, so a retried
         import reuses it.

   `expected-flow` is a parameter rather than a hardcoded 'archive' because the
   epub flow needs exactly this, down to the `archive_path` cache — the column
   name predates the second caller and renaming it would be churn."
  [user-id session-id expected-flow]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes, filename, flow, archive_path
                  FROM upload_sessions WHERE id = ? AND user_id = ? FOR UPDATE"
                 session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload not found or expired"}

        (not= expected-flow (:flow row))
        {:ok false :error (str "Not a" (when (= "archive" expected-flow) "n") " "
                            expected-flow " upload: " (:flow row))}

        (not= (:received_bytes row) (:total_bytes row))
        {:ok false :error (str "Upload incomplete: " (:received_bytes row)
                            " of " (:total_bytes row) " bytes")}

        :else
        (let [cached (some-> (:archive_path row) (java.io.File.))]
          (if (and cached (.exists cached) (= (.length cached) (:received_bytes row)))
            {:ok true :file cached :filename (:filename row)}
            (let [f (.toFile (java.nio.file.Files/createTempFile
                               "upload-archive" ".bin"
                               (make-array java.nio.file.attribute.FileAttribute 0)))]
              (lo/copy-to-file! tx (:lo_oid row) f)
              (jdbc/execute! tx
                ["UPDATE upload_sessions SET archive_path = ?, updated_at = now()
                  WHERE id = ?" (.getAbsolutePath f) session-id])
              {:ok true :file f :filename (:filename row)})))))))

(defn materialize-upload-archive!
  "`materialize-upload-session!` for the archive flow. The three archive import
   paths call this rather than naming their own flow string."
  [user-id session-id]
  (materialize-upload-session! user-id session-id "archive"))

;; ── Git forge imports (plans/git-forge-integration.md) ─────────────────────

(defn open-forge-stage!
  "Record an extracted forge archive as an upload session (§4.3 3.2).

   The session exists for OWNERSHIP, not transport. `release-upload-session!`
   and the six-hour reap sweep already remove what a session holds, so a picker
   the user abandons leaves nothing on disk (§4.3 3.2.3, 3.2.4). No quota is
   reserved — `total_bytes` is 0 — because an import charges nothing (§4.3 3.7).
   The large object is empty: `lo_oid` is NOT NULL and every cleanup path unlinks
   it.

   Pre : this call takes ownership of `dir`, an extracted commit tree.
         `forge-source` is {:forge :origin-url :repo-url :path :ref :sha}.
   Post: the new session id; its `flow` is 'forge' and `archive_path` is `dir`."
  [user-id filename ^java.io.File dir forge-source]
  (jdbc/with-transaction [tx ds]
    (let [oid (lo/create! tx)
          session-id (str (random-uuid))]
      (jdbc/execute! tx
        [(str "INSERT INTO upload_sessions
                 (id, user_id, lo_oid, total_bytes, received_bytes, filename, mime_type,
                  flow, archive_path, forge_source)
               VALUES (?, ?, " oid-param ", 0, 0, ?, 'application/zip', 'forge', ?, ?)")
         session-id user-id oid filename (.getAbsolutePath dir) (->jsonb forge-source)])
      session-id)))

(defn get-forge-stage
  "Post: {:dir File :forge-source map} for a forge session `user-id` owns whose
   directory still exists, else nil. A directory lost to a deploy flip reads as
   nil, the same as an expired session."
  [user-id session-id]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT archive_path, forge_source FROM upload_sessions
                     WHERE id = ? AND user_id = ? AND flow = 'forge'" session-id user-id]
                   {:builder-fn rs/as-unqualified-maps})]
    (let [dir (some-> (:archive_path row) (java.io.File.))]
      (when (and dir (.isDirectory dir))
        {:dir dir :forge-source (pgobject->clj (:forge_source row))}))))

(defn find-repo-node
  "Post: the id of `user-id`'s live `repo` topic for `repo-url`, or nil."
  [user-id repo-url]
  (:id (jdbc/execute-one! ds
         ["SELECT id FROM topics
           WHERE user_id = ? AND kind = 'repo' AND repo_url = ? AND staged_delete_id IS NULL
           ORDER BY id LIMIT 1" user-id repo-url]
         {:builder-fn rs/as-unqualified-maps})))

(defn find-commit-node
  "Post: the id of the live `commit` child of `repo-id` at `sha`, or nil.
   The 3-arity reads inside a caller's transaction."
  ([repo-id sha] (find-commit-node ds repo-id sha))
  ([connectable repo-id sha]
   (:id (jdbc/execute-one! connectable
          ["SELECT id FROM topics
            WHERE parent_id = ? AND kind = 'commit' AND commit_sha = ? AND staged_delete_id IS NULL
            LIMIT 1" repo-id sha]
          {:builder-fn rs/as-unqualified-maps}))))

(defn count-subtree-topics
  "Post: the topic rows in the subtree rooted at `root-id`, the root included."
  [root-id]
  (:n (jdbc/execute-one! ds
        ["WITH RECURSIVE sub AS (
            SELECT id FROM topics WHERE id = ?
            UNION ALL
            SELECT t.id FROM topics t JOIN sub ON t.parent_id = sub.id)
          SELECT count(*) AS n FROM sub" root-id]
        {:builder-fn rs/as-unqualified-maps})))

(defn- selection->jsonb
  "String keys on purpose: a directory path is not a keyword, and
   `pgobject->clj` keywordizes."
  [{:keys [dirs extensions]}]
  (->jsonb {"dirs" dirs "extensions" (vec (sort extensions))}))

(defn get-repo-import
  "What repository node `repo-id` was imported with.
   Post: {:repo-url :forge :ref :selection} for `user-id`'s live repo node, else
         nil. `:selection` is {:dirs {dir boolean} :extensions #{ext}}."
  [user-id repo-id]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT t.repo_url, rs.forge, rs.ref, rs.selection
                     FROM topics t JOIN repo_selections rs ON rs.topic_id = t.id
                     WHERE t.id = ? AND t.user_id = ? AND t.kind = 'repo'
                       AND t.staged_delete_id IS NULL" repo-id user-id]
                   {:builder-fn rs/as-unqualified-maps})]
    (let [sel (json/parse-string (.getValue ^PGobject (:selection row)))]
      {:repo-url (:repo_url row)
       :forge (:forge row)
       :ref (:ref row)
       :selection {:dirs (into {} (map (fn [[d v]] [d (boolean v)])) (get sel "dirs"))
                   :extensions (set (get sel "extensions"))}})))

(defn get-repo-summary
  "Post: {:repo-url :forge :ref :commits} for `user-id`'s live repo node, else
   nil. `:commits` counts its live commit children."
  [user-id repo-id]
  (jdbc/execute-one! ds
    ["SELECT t.repo_url, rs.forge, rs.ref,
             (SELECT count(*) FROM topics c
              WHERE c.parent_id = t.id AND c.kind = 'commit' AND c.staged_delete_id IS NULL) AS commits
      FROM topics t JOIN repo_selections rs ON rs.topic_id = t.id
      WHERE t.id = ? AND t.user_id = ? AND t.kind = 'repo' AND t.staged_delete_id IS NULL"
     repo-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn insert-repo-selection!
  "Pre : `tx` is open and `repo-id` is a `repo` topic inserted in it.
   Post: the repository's forge, ref and selection are stored."
  [tx repo-id forge ref selection]
  (jdbc/execute-one! tx
    ["INSERT INTO repo_selections (topic_id, forge, ref, selection) VALUES (?, ?, ?, ?)"
     repo-id forge ref (selection->jsonb selection)]))

(defn insert-topic-code-rows!
  "Batch-insert `topic_code` rows.
   Pre : `tx` is open; each row carries :topic_id :source :byte_size :lang, and
         its topic is inserted in `tx`.
   Post: the number of rows written.
   Invariant: routed through `param-bounded-batches` (§4.3 3.5). Bounding the
         BYTES of one statement is the caller's job — 16 000 one-megabyte
         sources are within the parameter ceiling and far past a sane statement."
  [tx rows]
  (if (empty? rows)
    0
    (transduce (map (fn [batch]
                      (::jdbc/update-count
                        (jdbc/execute-one! tx
                          (sql/format {:insert-into :topic_code :values (vec batch)})))))
      + 0 (param-bounded-batches rows))))

(defn get-topic-code
  "Post: {:source :lang :byte_size :repo_path} for `user-id`'s live code topic,
   or nil for an empty leaf, another user's topic, or a staged one."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["SELECT tc.source, tc.lang, tc.byte_size, t.repo_path
      FROM topic_code tc JOIN topics t ON t.id = tc.topic_id
      WHERE tc.topic_id = ? AND t.user_id = ? AND t.staged_delete_id IS NULL"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-referenceable-repo-files
  "Repo files that `user-id` can @-reference from an assistant chat.
   Pre : `needle` is lower case and not blank.
   Post: at most `limit` rows {:id :label}. Each is a live file with stored
         source in the NEWEST live commit of a live repository. `:label` is
         `<repo title>/<repo_path>` and contains `needle`. Newest repository
         first, then shallower paths first, then by path.
   Invariant: one statement. The walk visits only `dir` and `code` nodes under
         those commits, so it never reads an older commit or a learner's
         extract. The label the picker shows is the string the needle matched."
  [user-id needle limit]
  (jdbc/execute! ds
    ["WITH RECURSIVE newest AS (
        SELECT DISTINCT ON (r.id) c.id, r.id AS repo_id,
               coalesce(r.title, 'Untitled') AS repo_title
          FROM topics r JOIN topics c ON c.parent_id = r.id
         WHERE r.user_id = ? AND r.parent_id IS NULL AND r.kind = 'repo'
           AND r.staged_delete_id IS NULL
           AND c.kind = 'commit' AND c.staged_delete_id IS NULL
         ORDER BY r.id, c.id DESC),
      sub AS (
        SELECT id, repo_id, repo_title FROM newest
        UNION ALL
        SELECT t.id, sub.repo_id, sub.repo_title
          FROM topics t JOIN sub ON t.parent_id = sub.id
         WHERE t.kind IN ('dir', 'code') AND t.staged_delete_id IS NULL),
      files AS (
        SELECT t.id, sub.repo_id, t.repo_path, sub.repo_title || '/' || t.repo_path AS label
          FROM sub JOIN topics t ON t.id = sub.id
         WHERE t.kind = 'code' AND t.repo_path IS NOT NULL
           AND EXISTS (SELECT 1 FROM topic_code tc WHERE tc.topic_id = t.id))
      SELECT id, label FROM files
       WHERE strpos(lower(label), ?) > 0
       ORDER BY repo_id DESC,
                length(repo_path) - length(replace(repo_path, '/', '')),
                repo_path
       LIMIT ?"
     user-id needle limit]
    {:builder-fn rs/as-unqualified-maps}))

(defn get-repo-file-origin
  "Where `user-id`'s repo file `topic-id` came from, for building links out of it
   (plans/markdown-rendered-code-pane.md §1.5).
   Post: {:commit-id :commit-sha :repo-url :forge}, or nil when the topic is not
         the user's or has no commit and repository ancestor."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["WITH RECURSIVE up AS (
        SELECT id, parent_id, kind, commit_sha, repo_url
          FROM topics WHERE id = ? AND user_id = ?
        UNION ALL
        SELECT t.id, t.parent_id, t.kind, t.commit_sha, t.repo_url
          FROM topics t JOIN up ON t.id = up.parent_id)
      SELECT c.id AS commit_id, c.commit_sha, r.repo_url, rs.forge
        FROM up c
        JOIN up r ON r.id = c.parent_id AND r.kind = 'repo'
        JOIN repo_selections rs ON rs.topic_id = r.id
       WHERE c.kind = 'commit'"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn repo-topic-ids-by-path
  "The live topics under commit node `commit-id` whose `repo_path` is in `paths`.
   Post: {repo-path topic-id}, holding only the paths that resolve. One
         statement with one array parameter, whatever the size of `paths`."
  [commit-id paths]
  (if (empty? paths)
    {}
    (into {}
      (map (juxt :repo_path :id))
      (jdbc/execute! ds
        ["WITH RECURSIVE sub AS (
            SELECT id FROM topics WHERE id = ?
            UNION ALL
            SELECT t.id FROM topics t JOIN sub ON t.parent_id = sub.id)
          SELECT t.id, t.repo_path
            FROM topics t JOIN sub ON sub.id = t.id
           WHERE t.repo_path = ANY(?) AND t.kind IN ('code', 'dir')
             AND t.staged_delete_id IS NULL"
         commit-id (into-array String (set paths))]
        {:builder-fn rs/as-unqualified-maps}))))

(defn topic-code-exists?
  "Post: true when `topic-id` has stored source, i.e. is not an empty leaf."
  [topic-id]
  (some? (jdbc/execute-one! ds ["SELECT 1 FROM topic_code WHERE topic_id = ?" topic-id])))

(defn finalize-epub-session!
  "Turn a completed epub session into an `epub` topic, chapters and all.

   Transfers ownership of the large object from the session row to a new
   `topic_epubs` row; the session row is deleted, NOT compensated, because the
   bytes it reserved are now the topic's. A short upload refunds the difference —
   the object's real size is what the topic owns.

   Pre:  `chapters` is the parse result for THIS session's bytes, and the caller
         has already read them (through `materialize-upload-session!`). The
         session's received_bytes equals its total_bytes.
   Post: {:ok true :topic-id N}, or {:ok false :error S}.
   Invariant: exactly one row references the OID at every point — the session
         row before this commits, the topic_epubs row after.
   Invariant: quota is NOT bumped here. `init-upload-session!` already reserved
         every byte through `reserve-bytes!`, and bumping again would bill the
         user twice for one book."
  [user-id session-id title chapters]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes, filename, parent_id
                  FROM upload_sessions WHERE id = ? AND user_id = ? AND flow = 'epub'
                  FOR UPDATE"
                 session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload not found or expired"}

        (not= (:received_bytes row) (:total_bytes row))
        {:ok false :error (str "Upload incomplete: " (:received_bytes row)
                            " of " (:total_bytes row) " bytes")}

        :else
        (let [{:keys [topic-id]} (insert-epub-topic! tx user-id
                                   (or title (:filename row))
                                   (:parent_id row) chapters)]
          (jdbc/execute! tx
            [(str "INSERT INTO topic_epubs (topic_id, lo_oid, byte_size)
                   VALUES (?, " oid-param ", ?)")
             topic-id (:lo_oid row) (:received_bytes row)])
          ;; Init reserved total_bytes. Only the shortfall is given back.
          (when (< (:received_bytes row) (:total_bytes row))
            (bump-user-usage! tx user-id (- (:received_bytes row) (:total_bytes row))))
          (jdbc/execute! tx ["DELETE FROM upload_sessions WHERE id = ?" session-id])
          (audit-doc-created! user-id topic-id)
          {:ok true :topic-id topic-id})))))

(defn with-epub-source
  "Call `f` with a File holding the topic's stored EPUB, or return nil when it
   has none.

   The ONLY thing that knows an EPUB's bytes live in two places: `topic_epubs`
   (a large object, chunk-uploaded) or `topic_files.file_data` (BYTEA, multipart
   and every book imported before the large-object path existed). Callers ask for
   a File and get one either way.

   Pre:  `topic-id` names a topic the caller has already authorized.
   Post: `f`'s value, or nil when the topic stores no EPUB. The temp file is
         deleted before returning, on every path including a throw.
   Invariant: heap high-water is one `largeobj/io-buffer-bytes` buffer for the
         large-object path. The BYTEA path is bounded by the column, which is why
         nothing new writes there."
  [topic-id f]
  (let [lo-row (jdbc/execute-one! ds
                 ["SELECT lo_oid FROM topic_epubs WHERE topic_id = ?" topic-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (if lo-row
      (let [tmp (.toFile (java.nio.file.Files/createTempFile
                           "epub-source" ".epub"
                           (make-array java.nio.file.attribute.FileAttribute 0)))]
        (try
          (jdbc/with-transaction [tx ds]
            (lo/copy-to-file! tx (:lo_oid lo-row) tmp))
          (f tmp)
          (finally (.delete tmp))))
      (when-let [^bytes b (some-> (get-topic-file topic-id) :topic_files/file_data)]
        (let [tmp (.toFile (java.nio.file.Files/createTempFile
                             "epub-source" ".epub"
                             (make-array java.nio.file.attribute.FileAttribute 0)))]
          (try
            (with-open [o (java.io.FileOutputStream. tmp)] (.write o b))
            (f tmp)
            (finally (.delete tmp))))))))

(defn finalize-video-upload!
  "§4.3 3.1 — turn a completed session into a `video` topic.

   Transfers ownership of the large object from the session row to a new
   `topic_videos` row; the session row is deleted, NOT compensated, because the
   bytes it reserved are now the topic's. When fewer bytes arrived than were
   declared, the difference is refunded here — the object's real size is what
   the topic owns.

   Pre:  the session's received_bytes equals its total_bytes.
   Post: {:ok true :topic-id N}, or {:ok false :error S} when incomplete.
   Invariant: exactly one row references the OID at every point."
  [user-id session-id]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["SELECT lo_oid, total_bytes, received_bytes, filename, mime_type, parent_id
                  FROM upload_sessions WHERE id = ? AND user_id = ? FOR UPDATE"
                 session-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (cond
        (nil? row)
        {:ok false :error "Upload session not found"}

        (not= (:received_bytes row) (:total_bytes row))
        {:ok false :error (str "Upload incomplete: " (:received_bytes row)
                            " of " (:total_bytes row) " bytes")}

        :else
        (let [clean-name (input/prettify-title (input/sanitize-title (:filename row)))
              _ (input/check-length! :title clean-name input/title-max)
              csl {:type "motion_picture" :title clean-name
                   :accessed {:date-parts (now-date-parts)}}
              source (jdbc/execute-one! tx
                       (sql/format {:insert-into :sources
                                    :values [{:user_id user-id
                                              :csl_type "motion_picture"
                                              :csl (->jsonb csl)
                                              :title clean-name}]
                                    :returning [:id]}))
              topic (jdbc/execute-one! tx
                      (sql/format {:insert-into :topics
                                   :values [{:user_id user-id
                                             :parent_id (:parent_id row)
                                             :kind "video"
                                             :title clean-name
                                             ;; "" not NULL: the editor mounts on
                                             ;; an empty body. Content starts
                                             ;; empty by design — the transcript
                                             ;; is a reference pane the user
                                             ;; curates from, not the content.
                                             :content ""
                                             :source_id (:sources/id source)}]
                                   :returning [:id]}))
              topic-id (:topics/id topic)]
          (jdbc/execute! tx
            [(str "INSERT INTO topic_videos (topic_id, lo_oid, byte_size, mime_type, remux_pending)
                   VALUES (?, " oid-param ", ?, ?, ?)")
             topic-id (:lo_oid row) (:received_bytes row) (:mime_type row)
             ;; §12.4 3.1 — anything that is not already an MP4 is a remux
             ;; candidate, so its bytes are not yet immutable and must not be
             ;; cached. Derived from the declared MIME because finalize has not
             ;; probed the file; the pipeline's probe is authoritative and
             ;; clears this either way.
             (not= "video/mp4" (:mime_type row))])
          (jdbc/execute! tx ["DELETE FROM upload_sessions WHERE id = ?" session-id])
          (audit-doc-created! user-id topic-id)
          {:ok true :topic-id topic-id})))))

(defn list-video-playlists
  "§14.3 3.1 — the user's playlists, newest first, for the import modal's picker.

   Deliberately NOT `get-root-topics`, which would answer the same question: it
   also selects `content` and a summed file size per root, and a title picker
   reads neither. Two columns is the whole contract.

   Staged-deleted playlists are excluded — offering one as an upload target
   would hang new videos under a subtree already on its way out.

   Post: [{:id N :title S}] ordered newest first; [] when the user has none."
  [user-id]
  (mapv (fn [r] {:id (:id r) :title (or (:title r) "Untitled")})
    (jdbc/execute! ds
      ["SELECT id, title FROM topics
        WHERE user_id = ? AND kind = 'video-playlist' AND staged_delete_id IS NULL
        ORDER BY created_at DESC, id DESC" user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn create-video-playlist!
  "§4.10 10.1 — the parent a multi-file upload hangs its videos under.
   An ordinary topic of kind `video-playlist` holding no bytes of its own."
  [user-id title]
  (let [clean-name (input/prettify-title (input/sanitize-title title))]
    (input/check-length! :title clean-name input/title-max)
    (jdbc/with-transaction [tx ds]
      (let [topic (jdbc/execute-one! tx
                    (sql/format {:insert-into :topics
                                 :values [{:user_id user-id
                                           :kind "video-playlist"
                                           :title clean-name
                                           :content ""}]
                                 :returning [:id]}))]
        (audit-doc-created! user-id (:topics/id topic))
        (:topics/id topic)))))

(defn reap-stale-upload-sessions!
  "§4.3 3.3 — abort sessions untouched for longer than `ttl-hours`.

   A browser tab closed mid-upload leaves a large object, a byte reservation
   and — for an archive session that reached finalize — a materialized temp
   file. Nothing else reclaims any of the three, so this runs in the hourly
   sweep.

   Pre:  ttl-hours > 0. Callers pass `storage-meter/upload-session-ttl-hours`,
         which is generous on purpose: `updated_at` advances on every chunk, so
         only a genuinely abandoned upload goes stale.
   Post: returns the number reaped. Each reaped session leaves no
         `pg_largeobject` row, no `archive_path` file, and `usage_bytes`
         restored by exactly `total_bytes`.
   Invariant: the temp file is deleted outside the SQL but inside the tx —
         a failed delete must not roll back the refund, so it is best-effort."
  [ttl-hours]
  (jdbc/with-transaction [tx ds]
    (let [stale (jdbc/execute! tx
                  [(str "SELECT id, user_id, lo_oid, total_bytes, archive_path FROM upload_sessions
                         WHERE updated_at < now() - make_interval(hours => ?) FOR UPDATE")
                   (int ttl-hours)]
                  {:builder-fn rs/as-unqualified-maps})]
      (doseq [{:keys [id user_id lo_oid total_bytes archive_path]} stale]
        (lo/unlink! tx lo_oid)
        (delete-staged-path! archive_path)
        (jdbc/execute! tx ["DELETE FROM upload_sessions WHERE id = ?" id])
        (bump-user-usage! tx user_id (- total_bytes)))
      (when (seq stale)
        (tel/log! {:level :info :id ::reaped-upload-sessions :data {:count (count stale)}}
          "Reaped abandoned upload sessions"))
      (count stale))))

;; ── Storage metering (§4.6) ────────────────────────────────────────────────

(defn get-storage-meter-state
  "Everything one accrual pass needs about a user, read in one row:
   `usage_bytes`, `credit_balance_iqd`, `last_metered_at`,
   `storage_grace_started_at`, and whether they hold any video bytes at all."
  ([user-id] (get-storage-meter-state ds user-id))
  ([connectable user-id]
   (jdbc/execute-one! connectable
     ["SELECT u.usage_bytes, u.credit_balance_iqd, u.last_metered_at,
              u.storage_grace_started_at,
              EXISTS (SELECT 1 FROM topic_videos tv JOIN topics t ON tv.topic_id = t.id
                      WHERE t.user_id = u.id) AS has_video
       FROM users u WHERE u.id = ?" user-id]
     {:builder-fn rs/as-unqualified-maps})))

(defn set-storage-grace!
  "Open or close the storage grace window. `nil` clears it (a top-up); a
   timestamp opens it. Writes only on a real transition, so `storage-grace-days`
   counts from the FIRST time the balance hit zero, not from the latest sweep."
  [user-id ts]
  (if ts
    (jdbc/execute! ds
      ["UPDATE users SET storage_grace_started_at = ?
        WHERE id = ? AND storage_grace_started_at IS NULL" ts user-id])
    (jdbc/execute! ds
      ["UPDATE users SET storage_grace_started_at = NULL
        WHERE id = ? AND storage_grace_started_at IS NOT NULL" user-id])))

(defn storage-grace-started-at
  "When the user's storage grace window opened, or nil when not in grace.
   Read on the playback path (§4.6 6.3)."
  [user-id]
  (:storage_grace_started_at
   (jdbc/execute-one! ds
     ["SELECT storage_grace_started_at FROM users WHERE id = ?" user-id]
     {:builder-fn rs/as-unqualified-maps})))

(def ^:const micro-per-iqd
  "Denominator of the storage-rent carry. Rent accrues in millionths of an IQD
   and is debited in whole IQD once a millionth-sum crosses the unit."
  1000000)

(defn accrue-storage-charge!
  "§4.6 6.1 — accrue one user's storage rent and advance their meter, atomically.

   `micro-fn` receives {:usage-bytes :elapsed-ms} and returns the rent for that
   interval in MICRO-IQD. The carry (`users.storage_debt_micro`) is what makes
   the meter exact at any tick rate: an hourly pass over a small library prices
   well below 1 IQD, so without a carry every pass would round to zero and the
   meter would collect nothing. Whole IQD are debited as the carry crosses each
   unit; the remainder stays.

   Idempotent, which is the property the whole design hangs on: locking the row
   and re-reading `last_metered_at` inside the transaction means a second call
   an instant later sees elapsed ≈ 0, accrues ≈ 0, and debits nothing — and two
   concurrent sweeps cannot bill the same interval twice.

   `last_metered_at` always advances, so an interval is consumed exactly once
   whether or not it produced a whole-IQD charge.

   Pre:  micro-fn is pure and returns a non-negative long.
   Post: {:charged n :balance-after n :elapsed-ms n :carry-micro n}, or nil when
         the user is gone."
  [user-id micro-fn]
  (jdbc/with-transaction [tx ds]
    (when-let [row (jdbc/execute-one! tx
                     ["SELECT usage_bytes, credit_balance_iqd, last_metered_at, storage_debt_micro
                       FROM users WHERE id = ? FOR UPDATE" user-id]
                     {:builder-fn rs/as-unqualified-maps})]
      (let [now (java.time.Instant/now)
            last (some-> ^java.sql.Timestamp (:last_metered_at row) .toInstant)
            elapsed-ms (if last
                         (max 0 (- (.toEpochMilli now) (.toEpochMilli last)))
                         0)
            accrued (long (max 0 (micro-fn {:usage-bytes (or (:usage_bytes row) 0)
                                            :elapsed-ms elapsed-ms})))
            debt (+ (or (:storage_debt_micro row) 0) accrued)
            cost (quot debt micro-per-iqd)
            carry (rem debt micro-per-iqd)
            bal (or (:credit_balance_iqd row) 0)
            new-bal (- bal cost)]
        (jdbc/execute! tx
          ["UPDATE users SET last_metered_at = now(), storage_debt_micro = ? WHERE id = ?"
           carry user-id])
        (when (pos? cost)
          (jdbc/execute! tx
            ["UPDATE users SET credit_balance_iqd = ? WHERE id = ?" new-bal user-id])
          (jdbc/execute! tx
            (sql/format {:insert-into :credit_transactions
                         :values [{:user_id user-id
                                   :kind "debit"
                                   :amount_iqd (- cost)
                                   :balance_after new-bal
                                   :endpoint "storage"
                                   :model nil}]})))
        {:charged cost
         :balance-after (if (pos? cost) new-bal bal)
         :elapsed-ms elapsed-ms
         :carry-micro carry}))))

(defn users-due-for-metering
  "Ids of users whose meter is older than `stale-hours` AND who hold stored
   bytes. §4.6 6.4 / decision T1: a dormant account never triggers lazy accrual,
   so the sweep has to accrue for it or its bytes are stored free forever."
  [stale-hours]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT id FROM users
        WHERE usage_bytes > 0
          AND (last_metered_at IS NULL
               OR last_metered_at < now() - make_interval(hours => ?))"
       (int stale-hours)]
      {:builder-fn rs/as-unqualified-maps})))

(defn users-with-expired-grace
  "Ids of users whose grace window opened more than `grace-days` ago and who
   still hold video bytes — the reclamation worklist (§4.6 6.4)."
  [grace-days]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT DISTINCT u.id FROM users u
        JOIN topics t ON t.user_id = u.id
        JOIN topic_videos tv ON tv.topic_id = t.id
        WHERE u.storage_grace_started_at IS NOT NULL
          AND u.storage_grace_started_at < now() - make_interval(days => ?)"
       (int grace-days)]
      {:builder-fn rs/as-unqualified-maps})))

(defn reclaim-user-videos!
  "§4.6 6.5 / decision H3 — drop a user's video BYTES, keep everything else.

   Unlinks each large object and deletes the `topic_videos` rows, decrementing
   `usage_bytes` by exactly what was freed. The topic row, its transcript, its
   extracts, its cards and its schedule all survive: an extract's text was
   copied in at creation, so it reviews and generates cards unchanged — only
   playback is gone. This is the whole reason derivation is initial-only.

   Returns {:videos n :bytes n}."
  [user-id]
  (jdbc/with-transaction [tx ds]
    (let [rows (jdbc/execute! tx
                 ["SELECT tv.topic_id, tv.lo_oid, tv.byte_size FROM topic_videos tv
                   JOIN topics t ON tv.topic_id = t.id WHERE t.user_id = ? FOR UPDATE"
                  user-id]
                 {:builder-fn rs/as-unqualified-maps})
          freed (reduce + 0 (map :byte_size rows))]
      (doseq [{:keys [topic_id lo_oid]} rows]
        (lo/unlink! tx lo_oid)
        (jdbc/execute! tx ["DELETE FROM topic_videos WHERE topic_id = ?" topic_id]))
      (when (pos? freed) (bump-user-usage! tx user-id (- freed)))
      (when (seq rows)
        (tel/log! {:level :warn :id ::storage-reclaimed
                   :data {:user-id user-id :videos (count rows) :bytes freed}}
          "Reclaimed video bytes after storage grace expired"))
      {:videos (count rows) :bytes freed})))

(defn promote-to-paid-storage-tier!
  "§4.2 2.4 — raise a user's storage cap to the paid tier on their first
   completed credit order.

   `WHERE quota_bytes IS NULL` makes this both idempotent (a second purchase is
   a no-op) and safe against the admin override path — an operator-set
   `quota_bytes` is never overwritten by a purchase.
   Pre: called inside the order-completion transaction. Post: rows updated (0 or 1)."
  [connectable user-id paid-bytes]
  (when paid-bytes
    (jdbc/execute! connectable
      ["UPDATE users SET quota_bytes = ? WHERE id = ? AND quota_bytes IS NULL"
       (long paid-bytes) user-id])))

;; ---------------------------------------------------------------------------
;; Knowledge graph operations (see plans/knowledge-graph-quizzes.md)
;; ---------------------------------------------------------------------------

(defn- kg-aliases->vec
  "Normalize the aliases TEXT[] column (PgArray) to a Clojure vector — rows
   feed pr-str'd LLM prompts and Electric wires, neither of which can take a
   raw java.sql.Array."
  [rows]
  (mapv #(update % :aliases (fn [a] (some-> ^java.sql.Array a .getArray vec))) rows))

;; entity-batch-chunk-size caps rows/labels per statement so a large repo
;; distill's distinct-entity count never approaches Postgres' 65 535
;; bind-parameter limit — mirrors insert-kg-facts!'s chunking, one column
;; short of it since these rows are narrower.
(def ^:private entity-batch-chunk-size 20000)

(defn find-entity-link-candidates-batch
  "Batched find-entity-link-candidates: one query for every label in `labels`
   instead of a round trip per label (a VALUES list cross-joined against the
   user's kg_entities, ranked per label with a window function — same total
   work as the per-label loop, fewer round trips).
   Pre:  labels non-empty; entries need not be distinct.
   Post: {label [candidate...]}, same row shape as the single-label version
   (:id :label :aliases :sim, sim DESC, sim ≥ 0.25, ≤ `limit` per label). A
   label with zero qualifying candidates is absent from the map."
  [user-id labels limit]
  (when (seq labels)
    (let [distinct-labels (distinct labels)
          placeholders (str/join ", " (repeat (count distinct-labels) "(?)"))
          rows (jdbc/execute! ds
                 (vec (concat
                        [(str
                           "WITH query_labels(q_label) AS (VALUES " placeholders "),
                            scored AS (
                              SELECT q.q_label, e.id, e.label, e.aliases,
                                     GREATEST(
                                       similarity(e.label, q.q_label),
                                       COALESCE((SELECT MAX(similarity(a, q.q_label)) FROM unnest(e.aliases) a), 0)
                                     ) AS sim
                              FROM query_labels q
                              CROSS JOIN kg_entities e
                              WHERE e.user_id = ?
                            ),
                            ranked AS (
                              SELECT *, ROW_NUMBER() OVER (PARTITION BY q_label ORDER BY sim DESC) AS rn
                              FROM scored
                              WHERE sim >= 0.25
                            )
                            SELECT q_label, id, label, aliases, sim
                            FROM ranked
                            WHERE rn <= ?
                            ORDER BY q_label, sim DESC")]
                        distinct-labels
                        [user-id limit]))
                 {:builder-fn rs/as-unqualified-maps})]
      (->> rows
        kg-aliases->vec
        (reduce (fn [m {:keys [q_label] :as row}]
                  (update m q_label (fnil conj []) (dissoc row :q_label)))
          {})))))

(defn insert-kg-entities!
  "Batch-insert entities for `labels`. No ON CONFLICT — kg_entities has no
   UNIQUE(user_id,label) constraint (see get-or-create-kg-entities!); a race,
   or a caller that (like kg-extract's LLM-linker) inserts without an
   existence check, can produce a duplicate label — acceptable, matching the
   prior single-row insert-kg-entity!'s semantics.
   `from-diagnosis?` marks rows a question-map diagnosis created, so the
   Knowledge view can tell them from extracted ones (plans/anchored-questions.md
   §6.4.2.2).
   Pre:  labels non-empty; distinct within any one chunk (both callers dedupe
         before calling — a duplicate label in the same chunk would share one
         id in the result, since re-association below is by label).
   Post: vector of ids, positionally matching `labels`. RETURNING also pulls
   back `label` and re-associates each id to its input label by that value —
   NOT by trusting Postgres' VALUES/RETURNING row order, which SQL does not
   guarantee (reliable in practice for a single INSERT, but not a contract)."
  ([user-id labels] (insert-kg-entities! user-id labels {}))
  ([user-id labels {:keys [from-diagnosis?]}]
   (when (seq labels)
     (into []
       (mapcat (fn [chunk]
                 (let [returned (jdbc/execute! ds
                                  (sql/format {:insert-into :kg_entities
                                               :values (mapv #(hash-map :user_id user-id
                                                                :label (sanitize-utf8 %)
                                                                :from_diagnosis (boolean from-diagnosis?))
                                                         chunk)
                                               :returning [:id :label]})
                                  {:builder-fn rs/as-unqualified-maps})
                       id-by-label (into {} (map (juxt :label :id)) returned)]
                   (mapv (comp id-by-label sanitize-utf8) chunk))))
       (partition-all entity-batch-chunk-size labels)))))

(defn kg-entity-ids-by-labels
  "`user-id`'s entities whose label matches one of `labels`, ignoring case.
   Post: {lower-label id}, the oldest entity of a label when several match."
  [user-id labels]
  (if (empty? labels)
    {}
    (reduce (fn [m {:keys [k id]}] (if (contains? m k) m (assoc m k id)))
      {}
      (jdbc/execute! ds
        ["SELECT lower(label) AS k, id FROM kg_entities
           WHERE user_id = ? AND lower(label) = ANY(?) ORDER BY id"
         user-id (into-array String (map #(str/lower-case (str %)) labels))]
        {:builder-fn rs/as-unqualified-maps}))))

(defn get-or-create-kg-entities!
  "Batched exact get-or-create of entities by (user-id, label) — the
   DETERMINISTIC linker for code facts, whose labels are canonical
   fully-qualified names, so the prose path's fuzzy trigram/LLM merge
   (find-entity-link-candidates-batch) is neither needed nor wanted.
   One SELECT ... WHERE label IN (...) plus one multi-row INSERT for the
   labels not found, instead of a round trip per label. Same
   select-then-insert semantics as the old single-label get-or-create-kg-entity!
   (no UNIQUE(user_id,label) — a race could still insert a duplicate;
   acceptable, single-user ingest is low-concurrency).
   Pre:  labels non-empty; entries need not be distinct.
   Post: {label id} covering every distinct label in `labels`."
  [user-id labels]
  (when (seq labels)
    (let [clean (into {} (map (juxt identity sanitize-utf8)) (distinct labels))
          clean-vals (distinct (vals clean))
          existing (into []
                     (mapcat (fn [chunk]
                               (jdbc/execute! ds
                                 (sql/format {:select [:label :id]
                                              :from [:kg_entities]
                                              :where [:and [:= :user_id user-id] [:in :label chunk]]
                                              :order-by [:id]})
                                 {:builder-fn rs/as-unqualified-maps})))
                     (partition-all entity-batch-chunk-size clean-vals))
          found (reduce (fn [m {:keys [label id]}] (if (contains? m label) m (assoc m label id)))
                  {} existing)
          missing (vec (remove found clean-vals))
          id-by-clean (merge found (zipmap missing (insert-kg-entities! user-id missing)))]
      (into {} (map (fn [[raw c]] [raw (id-by-clean c)])) clean))))

(defn get-or-create-kg-predicate!
  "Upsert a predicate by (user-id, slug); new rows land status='approved' —
   the graph curates by exception (edit/reject in the facts browser), not by
   pre-approval (decision revised 2026-07-04, see plan doc).
   The no-op DO UPDATE makes RETURNING yield the id on conflict too.
   Pre:  slug matches the table's CHECK (callers slugify first — violation
         throws, blame: caller).
   Post: {:id int :status str} of the existing-or-new row."
  [user-id slug label]
  (jdbc/execute-one! ds
    ["INSERT INTO kg_predicates (user_id, slug, label, status) VALUES (?, ?, ?, 'approved')
      ON CONFLICT (user_id, slug) DO UPDATE SET slug = EXCLUDED.slug
      RETURNING id, status"
     user-id slug (sanitize-utf8 label)]
    {:builder-fn rs/as-unqualified-maps}))

(defn insert-kg-facts!
  "Batch insert facts. Rows: {:user_id :subject_entity_id :predicate_id
   :object_entity_id|:object_literal :graph_topic_id :page_number
   :source_model :status}. ON CONFLICT DO NOTHING — the partial unique
   indexes drop same-document s/p/o duplicates (including rows already
   reviewed: a re-distill never resurrects a rejected fact).
   Chunked to stay under Postgres' 65 535 bind-parameter cap per statement:
   one multi-row INSERT emits cols×rows params, and a repo distill can produce
   tens of thousands of facts (the prose path never approached the cap). Chunks
   are independent statements, not one transaction — acceptable because ON
   CONFLICT DO NOTHING + a re-distill make the insert idempotent, so a partial
   run self-heals on retry.
   Post: vector of inserted ids (shorter than rows when duplicates skipped)."
  [rows]
  (when (seq rows)
    (let [rows (map #(update % :object_literal sanitize-utf8) rows)
          cols (count (keys (first rows)))
          ;; 60 000 leaves margin below the 65 535 driver cap.
          per-chunk (max 1 (quot 60000 (max 1 cols)))]
      (into []
        (mapcat (fn [chunk]
                  (mapv :id
                    (jdbc/execute! ds
                      (sql/format {:insert-into :kg_facts
                                   :values chunk
                                   :on-conflict []
                                   :do-nothing true
                                   :returning [:id]})
                      {:builder-fn rs/as-unqualified-maps}))))
        (partition-all per-chunk rows)))))

;; ---------------------------------------------------------------------------
;; Per-page extraction log — the facts-extraction page skip.
;; See plans/kg-page-skip-extraction.md.
;; ---------------------------------------------------------------------------

(defn- extractable-page-text
  "The condition that makes a page worth extracting from, as SQL for table `alias`.

   ONE definition, because three readers apply it and a drift between any two is
   a page silently extracted twice or never: `document-page-extraction-state`
   selects on it, `extractable-pages-cte` counts on it, and
   `root-topic-extraction-state` applies it to the fallback root. It takes an alias
   rather than hardcoding one so each caller supplies its own.

   `content_text` rather than `content`: it is the stripped text extraction
   actually sends, every content writer maintains it, and `backfill-content-text!`
   runs unconditionally at schema setup (db.clj:785), so it is non-null wherever
   `content` is."
  [alias]
  (str alias ".content_text IS NOT NULL AND btrim(" alias ".content_text) <> ''"))

(defn- truncated-error
  "An error message clipped to fit `kg_page_extractions.last_error` without
   dominating the row. Post: nil, or a string of at most 500 characters."
  [message]
  (when-let [s (some-> message sanitize-utf8)]
    (if (> (count s) 500) (subs s 0 500) s)))

(defn document-page-extraction-state
  "Every extractable page of `root-topic-id`, joined to its extraction record.

   ONE query, not one per page. The digest itself is NOT computed here —
   `freememo.kg-page-digest` owns it, and computing it in SQL as well would put
   the definition in two languages where a drift is undetectable. The caller
   digests `content_text` and compares.

   THE COST that choice carries: every page's full text crosses into heap, INCLUDING
   pages that will be skipped. A 500-page book re-distilled with nothing changed
   makes zero LLM calls and still reads the whole book, because a page cannot be
   known skippable without its digest and the digest needs the text. Bounded by one
   document, and the alternative is a second hashing algorithm in SQL that could
   drift from the first undetectably. If this ever needs fixing, fix it by making
   SQL the ONLY home of the digest, never by adding a second one.

   Pre : `root-topic-id` is a root document topic owned by the caller's user.
   Post: rows of {:page_topic_id :page_number :content_text :extraction_id
         :content_digest :digest_version :fact_count :attempt_count :has_staged},
         page order. The last six are nil/false for a page never attempted.
   Inv : a row appears here iff `extractable-page-text` admits it, so this cannot
         disagree with `kg-extraction-progress` about what a document holds."
  [root-topic-id]
  (jdbc/execute! ds
    [(str "SELECT t.id AS page_topic_id, t.page_number, t.content_text,
                  e.id AS extraction_id, e.content_digest, e.digest_version,
                  e.fact_count, e.attempt_count,
                  COALESCE(EXISTS (SELECT 1 FROM kg_staged_triples s
                                    WHERE s.page_extraction_id = e.id), false)
                    AS has_staged
             FROM topics t
             LEFT JOIN kg_page_extractions e ON e.page_topic_id = t.id
            WHERE t.parent_id = ? AND t.kind = 'page'
              AND " (extractable-page-text "t")
       " ORDER BY t.page_number ASC")
     root-topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn root-topic-extraction-state
  "The same row shape as `document-page-extraction-state`, for a document with no
   page children (wiki, web, markdown imports).

   Those documents extract from the root topic's own content, so the extraction
   row's `page_topic_id` IS the root id. That is what keeps this table free of a
   nullable page number and every reader free of an `IS NULL` branch.

   Pre : `root-topic-id` is a root document topic.
   Post: one row, or none when the root holds no extractable text."
  [root-topic-id]
  (jdbc/execute-one! ds
    [(str "SELECT t.id AS page_topic_id, NULL::int AS page_number, t.content_text,
                  e.id AS extraction_id, e.content_digest, e.digest_version,
                  e.fact_count, e.attempt_count,
                  COALESCE(EXISTS (SELECT 1 FROM kg_staged_triples s
                                    WHERE s.page_extraction_id = e.id), false)
                    AS has_staged
             FROM topics t
             LEFT JOIN kg_page_extractions e ON e.page_topic_id = t.id
            WHERE t.id = ? AND " (extractable-page-text "t"))
     root-topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- upsert-page-extraction!
  "Write one page's extraction record and drop whatever it staged before.

   Every caller reaches this through `record-page-extraction!` or
   `record-page-extraction-failure!`, which is where the two contracts differ.
   Both share the stale-staged delete, and that delete is not optional: a page
   selected for re-extraction may still carry staged triples from an OLDER digest,
   and leaving them would make the new digest plus those rows read as satisfied.

   `attempt_count` counts CONSECUTIVE FAILURES on the page's current text, not
   writes. The two callers therefore need different behaviour from this column and
   `attempts` says which:

     :reset     a success. The count goes to 0, because a page that produced
                output owes nothing to its earlier failures. Without this a page
                that succeeded and then failed ONCE sat at the cap immediately,
                since the success itself had left the count at 1.
     :increment a failure. One more attempt when the digest and version are
                unchanged, or 1 when they are not — which is how editing a page
                clears a cap reached against its old text.

   The rule lives in SQL rather than in the callers so it has one home, and the
   two `attempts` fragments are literal code chosen by `case`, never caller data.

   Pre : `tx` is a connectable inside a transaction the caller owns.
         `attempts` is :reset or :increment.
   Post: exactly one row for `page-topic-id`, its id returned, and no staged row
         referencing it."
  [tx {:keys [user-id graph-topic-id page-topic-id content-digest digest-version
              fact-count last-error prompt-version extract-model-id attempts]}]
  (let [insert-attempts (case attempts :reset "0" :increment "1")
        update-attempts (case attempts
                          :reset "0"
                          :increment
                          "CASE WHEN kg_page_extractions.content_digest = EXCLUDED.content_digest
                                 AND kg_page_extractions.digest_version = EXCLUDED.digest_version
                                THEN kg_page_extractions.attempt_count + 1
                                ELSE 1 END")
        row (jdbc/execute-one! tx
              [(str "INSERT INTO kg_page_extractions
                       (user_id, graph_topic_id, page_topic_id, content_digest,
                        digest_version, fact_count, attempt_count, last_error,
                        prompt_version, extract_model_id)
                     VALUES (?, ?, ?, ?, ?, ?, " insert-attempts ", ?, ?, ?)
                     ON CONFLICT (page_topic_id) DO UPDATE SET
                       graph_topic_id = EXCLUDED.graph_topic_id,
                       content_digest = EXCLUDED.content_digest,
                       digest_version = EXCLUDED.digest_version,
                       fact_count = EXCLUDED.fact_count,
                       attempt_count = " update-attempts ",
                       last_error = EXCLUDED.last_error,
                       prompt_version = EXCLUDED.prompt_version,
                       extract_model_id = EXCLUDED.extract_model_id,
                       extracted_at = now()
                     RETURNING id")
               user-id graph-topic-id page-topic-id content-digest digest-version
               fact-count last-error prompt-version extract-model-id]
              {:builder-fn rs/as-unqualified-maps})]
    (jdbc/execute! tx
      ["DELETE FROM kg_staged_triples WHERE page_extraction_id = ?" (:id row)])
    (:id row)))

(defn record-page-extraction!
  "Record a page's successful extraction and stage its triples, atomically.

   `fact_count` is set to 0 for an EMPTY triple list and left NULL otherwise. That
   0 is the barren-page marker: nothing was staged, so linking will never visit
   the page, and without the 0 the selector would read it as unfinished and pay to
   extract it again on every run. A page with triples gets its count from
   `finalize-linked-pages!` once linking has run.

   `attempt_count` is RESET here, because a page that produced output owes nothing
   to its earlier failures. See `upsert-page-extraction!` for what happens without
   that reset.

   Pre : `triples` are shape-valid per `kg-extract/valid-triple?` — exactly one of
         :o / :lit non-blank. The XOR CHECK on `kg_staged_triples` enforces it, so
         a violation surfaces here as a constraint error rather than as a bad fact
         later.
   Post: one extraction row for the page, `(count triples)` staged rows, no staged
         row from any earlier attempt, and all of it committed together — so a
         process death after this call cannot lose the page's paid LLM work."
  [{:keys [user-id graph-topic-id page-topic-id content-digest digest-version
           prompt-version extract-model-id triples]}]
  (jdbc/with-transaction [tx ds]
    (let [extraction-id (upsert-page-extraction! tx
                          {:user-id user-id :graph-topic-id graph-topic-id
                           :page-topic-id page-topic-id
                           :content-digest content-digest
                           :digest-version digest-version
                           :fact-count (when (empty? triples) 0)
                           :last-error nil
                           :attempts :reset
                           :prompt-version prompt-version
                           :extract-model-id extract-model-id})]
      (when (seq triples)
        (jdbc/execute! tx
          (sql/format {:insert-into :kg_staged_triples
                       :values (mapv (fn [{:keys [s p o lit]}]
                                       {:user_id user-id
                                        :page_extraction_id extraction-id
                                        :subject_label (sanitize-utf8 s)
                                        :predicate_label (sanitize-utf8 p)
                                        :object_label (some-> o sanitize-utf8)
                                        :object_literal (some-> lit sanitize-utf8)})
                                 triples)})))
      extraction-id)))

(defn record-page-extraction-failure!
  "Record that a page's extraction failed, and count the attempt.

   The row stores the digest it failed ON, which is what scopes `attempt_count` to
   the text: editing the page changes the digest and `upsert-page-extraction!`
   resets the count to 1, so a cap reached against old text never blocks new text.

   `fact_count` stays NULL and nothing is staged, so the selector reads this row as
   \"retry, until the cap\" rather than as work already done.

   Pre : `error-message` is a string or nil.
   Post: the page's `attempt_count` is one higher when the digest is unchanged, or
         1 when it is not; no staged row references the page."
  [{:keys [user-id graph-topic-id page-topic-id content-digest digest-version
           prompt-version extract-model-id error-message]}]
  (jdbc/with-transaction [tx ds]
    (upsert-page-extraction! tx
      {:user-id user-id :graph-topic-id graph-topic-id
       :page-topic-id page-topic-id
       :content-digest content-digest
       :digest-version digest-version
       :fact-count nil
       :last-error (truncated-error error-message)
       :attempts :increment
       :prompt-version prompt-version
       :extract-model-id extract-model-id})))

(defn staged-triples-for-document
  "Every unlinked triple of a document, oldest page first.

   This is what makes linking one call per INVOCATION rather than one per page: it
   returns the triples this invocation just staged AND everything an earlier
   cancelled or timed-out invocation left behind, so a resumed run links the whole
   accumulated set at once.

   Pre : `root-topic-id` is owned by `user-id`.
   Post: rows of {:id :page_extraction_id :page_number :subject_label
         :predicate_label :object_label :object_literal}. `page_number` is nil for
         a document extracted from its root topic."
  [user-id root-topic-id]
  (jdbc/execute! ds
    ["SELECT s.id, s.page_extraction_id, pt.page_number,
             s.subject_label, s.predicate_label, s.object_label, s.object_literal
        FROM kg_staged_triples s
        JOIN kg_page_extractions e ON e.id = s.page_extraction_id
        JOIN topics pt ON pt.id = e.page_topic_id
       WHERE e.user_id = ? AND e.graph_topic_id = ?
       ORDER BY pt.page_number ASC NULLS FIRST, s.id ASC"
     user-id root-topic-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn finalize-linked-pages!
  "Mark pages linked and discard the triples that fed the link, atomically.

   `fact_count` is set from the staged-row count, which OVERCOUNTS rows the
   flashcard-style unique indexes on `kg_facts` dropped. That is deliberate: the
   column's job is to be non-NULL, and no surface displays it — the Knowledge views
   read fact counts from `kg_facts`. See the table comment in setup-schema-kg!.

   Pre : the caller has already inserted the facts these triples produced. Calling
         this first would mark pages linked whose facts do not exist, which no
         later run would repair, because the selector would skip them.
   Post: every named page has a non-NULL `fact_count` and no staged rows. Both
         happen in one transaction, so a crash between them is not a state the
         selector can observe. Returns how many pages were marked."
  [user-id extraction-ids]
  (if (empty? extraction-ids)
    0
    (let [ids (int-array extraction-ids)]
      (jdbc/with-transaction [tx ds]
        (let [marked (:next.jdbc/update-count
                      (jdbc/execute-one! tx
                        ["UPDATE kg_page_extractions e
                             SET fact_count = c.n
                            FROM (SELECT page_extraction_id, COUNT(*) AS n
                                    FROM kg_staged_triples
                                   WHERE page_extraction_id = ANY(?)
                                   GROUP BY page_extraction_id) c
                           WHERE e.id = c.page_extraction_id AND e.user_id = ?"
                         ids user-id]))]
          (jdbc/execute! tx
            ["DELETE FROM kg_staged_triples s
               USING kg_page_extractions e
               WHERE s.page_extraction_id = e.id
                 AND s.page_extraction_id = ANY(?) AND e.user_id = ?"
             ids user-id])
          marked)))))

(defn clear-document-extraction-records!
  "Forget every extraction record of a document, so the next run re-extracts it.

   This is the ONLY path by which a prompt or model improvement reaches pages
   already extracted, because the skip gates on content alone. It is destructive of
   nothing a user made: facts, edits and rejections live in `kg_facts` and are
   untouched, and `insert-kg-facts!`'s unique indexes stop the re-run resurrecting
   a rejected fact.

   Pre : `root-topic-id` is owned by `user-id`.
   Post: no `kg_page_extractions` row for the document, and by cascade no staged
         triple. Returns the number of pages forgotten."
  [user-id root-topic-id]
  (:next.jdbc/update-count
   (jdbc/execute-one! ds
     ["DELETE FROM kg_page_extractions WHERE user_id = ? AND graph_topic_id = ?"
      user-id root-topic-id])))

(def ^:private extractable-pages-cte
  "The user's extractable pages, one row per page, as (page_topic_id, graph_topic_id).

   Both branches drive from the user's ROOT topics, which is what scopes the walk
   to one account. An earlier revision counted pages in a subquery with no
   `user_id` predicate at all: the plan was a seq scan aggregating every page topic
   of every account, with a `btrim` detoast per row, on a path that runs on every
   Knowledge render and every :kg-mutations bump. One account's growth slowed every
   other account's Knowledge tab.

   The second branch is the root fallback `kg-extract/document-pages` applies —
   a document with no extractable page children extracts from its own content, and
   its extraction row keys on the root id. `NOT EXISTS` makes the two branches
   disjoint, so `UNION ALL` needs no dedup.

   Two `?`, both the user id."
  (str "SELECT t.id AS page_topic_id, r.id AS graph_topic_id
          FROM topics r
          JOIN topics t ON t.parent_id = r.id
         WHERE r.user_id = ? AND r.parent_id IS NULL
           AND t.kind = 'page' AND " (extractable-page-text "t")
    " UNION ALL
        SELECT r.id AS page_topic_id, r.id AS graph_topic_id
          FROM topics r
         WHERE r.user_id = ? AND r.parent_id IS NULL
           AND " (extractable-page-text "r")
    "     AND NOT EXISTS (SELECT 1 FROM topics t
                           WHERE t.parent_id = r.id AND t.kind = 'page'
                             AND " (extractable-page-text "t") ")"))

(defn kg-extraction-progress
  "Per-document extraction progress: pages extracted, given up on, and held.

   ONE query, because the three figures have to agree. `total` and `done` were two
   independent queries — one counting currently-extractable pages, the other
   counting extraction ROWS — and nothing joined them, so clearing a page's content
   dropped it from `total` while its row stayed in `done` and the column rendered
   `30/29`. Counting both over the same LEFT JOIN makes `done <= total` structural
   rather than something a caller has to clamp.

   `done` counts pages the selector would skip as accounted for — linked, or barren
   (`fact_count` 0). `at-cap` counts pages it would skip for the OTHER reason,
   having failed `attempt-cap` times on text unchanged since. The two are disjoint
   by construction, since `fact_count` is non-NULL for exactly the first set.

   A page recorded but neither done nor at cap is mid-flight or retryable, and is
   deliberately in neither figure — calling it done would be the lie this table
   exists to prevent. So `done + at-cap <= attempted <= total`, and each shortfall
   is work outstanding.

   `attempted` exists because `total` alone cannot answer \"has anything ever run
   here?\". Every document with extractable text gets a row from this query, so
   presence in the result means \"has pages\", NOT \"has been distilled\". Callers
   that need the second question — the spend confirm, and whether there is anything
   to forget — MUST read `attempted`; reading presence reported every document as
   already distilled and put the confirm in front of every FIRST distill.

   Pre : `attempt-cap` is the cap the extraction run enforces. A different value
         here makes the surface disagree with the run about which pages are dead.
   Inv : admission is `extractable-page-text`, the same definition
         `document-page-extraction-state` selects on, so this cannot disagree with
         the selector about which pages a document holds.
   Post: {root-topic-id {:done n :at-cap n :attempted n :total n}}, omitting
         documents with no extractable text at all."
  [user-id attempt-cap]
  (into {}
    (map (juxt :graph_topic_id
           #(hash-map :done (:done %) :at-cap (:at_cap %)
              :attempted (:attempted %) :total (:total %))))
    (jdbc/execute! ds
      ;; `e.user_id = ?` belongs in the JOIN condition, not in a WHERE. In a WHERE it
       ;; would discard the null-extended rows and silently turn this into an inner
       ;; join, so every never-attempted page would vanish from `total`. In the ON it
       ;; bounds the hash build to this account — without it the join reads every
       ;; account's extraction rows, which is the same leak the CTE comment describes
       ;; one table down. The result was already correct either way, because
       ;; page_topic_id is unique and the pages are this user's; only the cost leaked.
      [(str "WITH pages AS (" extractable-pages-cte ")
             SELECT p.graph_topic_id,
                    COUNT(*) AS total,
                    COUNT(e.id) AS attempted,
                    COUNT(*) FILTER (WHERE e.fact_count IS NOT NULL) AS done,
                    COUNT(*) FILTER (WHERE e.fact_count IS NULL
                                       AND e.attempt_count >= ?) AS at_cap
               FROM pages p
               LEFT JOIN kg_page_extractions e
                 ON e.page_topic_id = p.page_topic_id AND e.user_id = ?
              GROUP BY p.graph_topic_id")
       user-id user-id attempt-cap user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn get-kg-facts
  "Facts for a root document, display-joined (subject/object/predicate labels).
   status nil = all; non-blank `query` filters case-insensitively across
   subject/predicate/object labels and the literal. Ordered by page, then id."
  [user-id graph-topic-id status query]
  (let [q (when-not (or (nil? query) (str/blank? query))
            (str "%" (str/trim query) "%"))]
    (jdbc/execute! ds
      (sql/format {:select [[:f.id :id] [:f.status :status]
                            [:f.page_number :page_number]
                            [:f.object_literal :object_literal]
                            [:f.subject_entity_id :subject_entity_id]
                            [:f.object_entity_id :object_entity_id]
                            [:f.predicate_id :predicate_id]
                            [:s.label :subject_label]
                            [:o.label :object_label]
                            [:p.label :predicate_label] [:p.slug :predicate_slug]]
                   :from [[:kg_facts :f]]
                   :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                          [:kg_predicates :p] [:= :p.id :f.predicate_id]]
                   :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
                   :where [:and [:= :f.user_id user-id]
                           [:= :f.graph_topic_id graph-topic-id]
                           (when status [:= :f.status status])
                           (when q
                             [:or [:ilike :s.label q] [:ilike :p.label q]
                              [:ilike :o.label q] [:ilike :f.object_literal q]])]
                   :order-by [[:f.page_number :asc] [:f.id :asc]]})
      {:builder-fn rs/as-unqualified-maps})))

(defn kg-fact-ids-for-doc
  "Just the ids of a document's facts in `status` — the target set for \"select
   every fact in this document\", which by design ignores the view's search filter.
   Ids only: the display-joined rows get-kg-facts returns are far more than a
   selection needs, and the caller already holds rows for what it can see.
   `status` MUST match the status the view is currently listing: a select-all that
   returned approved ids while the view showed duplicates would delete rows the
   user never saw.
   Pre:  status is one of the kg_facts_status_check values.
   Post: vector of ids, page order then id, matching get-kg-facts."
  [user-id graph-topic-id status]
  (mapv :id
    (jdbc/execute! ds
      ["SELECT id FROM kg_facts
        WHERE user_id = ? AND graph_topic_id = ? AND status = ?
        ORDER BY page_number NULLS LAST, id"
       user-id graph-topic-id status]
      {:builder-fn rs/as-unqualified-maps})))

(defn kg-documents-with-facts
  "Root topic ids holding at least one approved fact — the scope of the
   all-documents duplicate sweep. Not get-root-topics: that returns whole rows
   including :content, and a document with no facts has nothing to sweep.
   Post: vector of graph_topic_ids, ascending."
  [user-id]
  (mapv :graph_topic_id
    (jdbc/execute! ds
      ["SELECT DISTINCT graph_topic_id FROM kg_facts
        WHERE user_id = ? AND status = 'approved' ORDER BY 1"
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(def sweep-statuses
  "The `kg_facts.status` values the shipped duplicate sweep compares.

   One definition, because kg-oversized-page-keys and
   kg-duplicate-candidate-pairs MUST read the same population: the first reports
   which pages the second refused to expand, and a page counted against one
   status set and expanded against another makes that report wrong rather than
   merely incomplete.

   A measurement that rebuilds the labelled benchmark passes
   #{\"approved\" \"duplicate\"} instead, because the sweep's own marks removed
   its true positives from the approved set (plans/jev-dedup-benchmark-and-gate-waste.md
   §1.1). Nothing in src/ passes anything but this."
  #{"approved"})

(defn kg-oversized-page-keys
  "Page keys in this document holding more than `max-facts` facts in `statuses` —
   the keys kg-duplicate-candidate-pairs refuses to expand. Reported so a bounded
   sweep can say what it skipped instead of reading as complete coverage.
   Pre:  `statuses` is a non-empty collection of kg_facts.status values.
   Post: vector of page keys (-1 for the page_number IS NULL group)."
  [user-id graph-topic-id max-facts statuses]
  (let [statuses (vec statuses)
        sph (str/join ", " (repeat (count statuses) "?"))]
    (mapv :pk
      (jdbc/execute! ds
        (into [(str "SELECT coalesce(page_number, -1) AS pk FROM kg_facts
                     WHERE user_id = ? AND graph_topic_id = ? AND status IN (" sph ")
                     GROUP BY 1 HAVING count(*) > ?")
               user-id graph-topic-id]
          (conj statuses max-facts))
        {:builder-fn rs/as-unqualified-maps}))))

(defn kg-duplicate-candidate-pairs
  "Every pair of facts in `statuses` sharing a page within one document, with the
   trigram scores freememo.kg-dedup needs. The SQL half of the hybrid detector.

   `statuses` is `sweep-statuses` at every call site in src/. It is a parameter
   because the sweep's own marks move a fact out of 'approved', so the labelled
   benchmark cannot be rebuilt from the approved set alone — and a copy of this
   CTE in a test would drift from the page-keying rule below, which 43 of the 49
   labelled duplicates depend on.

   Returns ALL pairs on a page and applies NO similarity threshold. That is
   deliberate: h3 is a three-branch disjunction and its containment branch fires
   where trigram is low (`minecraft | uses | leb128` vs `minecraft |
   uses-leb128-for | measuring lengths` scores trigram 0.34, containment 1.00), so
   a similarity-gated shortlist here would silently delete that branch's recall.
   Only the trigram scores come from SQL, because pg_trgm's `similarity` is what
   the thresholds were fitted against and this project carries no trigram
   implementation in Clojure to reproduce it with.

   Pages are keyed `coalesce(page_number, -1)`: SQL equality is never true for
   NULL, and wiki/web/markdown imports carry NULL on every fact (they have no page
   children). On production those documents hold 43 of 49 labelled duplicates, so
   a plain `a.page_number = b.page_number` would miss 88% of them.

   `new-ids` nil compares every pair — the user-invoked sweep. A non-empty
   collection restricts to (pre-existing × new), which is what the automatic
   post-distill sweep wants: same-run pair behaviour is unmeasured, and this is
   the comparison the 234 labelled pairs actually cover. An empty (non-nil)
   collection is a no-op.

   Invariant: `dup_id` is always the LATER-inserted fact of the pair, because
   kg_facts.id is SERIAL and the join is `a.id < b.id`. Callers mark `dup_id` and
   keep `keep_id`, so the earlier provenance and any questions already grounded on
   it survive.
   Pre:  graph-topic-id owned by user-id; max-facts-per-page > 0; `statuses` is a
         non-empty collection of kg_facts.status values, and the SAME collection
         kg-oversized-page-keys was called with — the two read one population and
         the second reports what the first refused to expand.
   Post: rows of {:keep_id :dup_id :page_key :flat_sim :obj_sim :subj_eq :obj_eq
         :keep_flat :dup_flat}, excluding page keys over max-facts-per-page."
  [user-id graph-topic-id new-ids max-facts-per-page statuses]
  (if (and (some? new-ids) (empty? new-ids))
    []
    (let [restrict (when (seq new-ids)
                     (let [ph (str/join ", " (repeat (count new-ids) "?"))]
                       (str " AND b.id IN (" ph ") AND a.id NOT IN (" ph ")")))
          ids (vec new-ids)
          statuses (vec statuses)
          sph (str/join ", " (repeat (count statuses) "?"))]
      (jdbc/execute! ds
        (into [(str
                 "WITH base AS (
                    SELECT f.id, coalesce(f.page_number, -1) AS pk,
                           lower(trim(s.label)) AS subj,
                           lower(trim(coalesce(o.label, f.object_literal))) AS obj,
                           lower(s.label) || ' ' || replace(lower(p.slug), '-', ' ')
                             || ' ' || lower(coalesce(o.label, f.object_literal)) AS flat
                    FROM kg_facts f
                    JOIN kg_entities s ON s.id = f.subject_entity_id
                    JOIN kg_predicates p ON p.id = f.predicate_id
                    LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                    WHERE f.user_id = ? AND f.graph_topic_id = ? AND f.status IN (" sph ")
                  ),
                  sized AS (SELECT *, count(*) OVER (PARTITION BY pk) AS pk_n FROM base),
                  ok AS (SELECT * FROM sized WHERE pk_n <= ?)
                  SELECT a.id AS keep_id, b.id AS dup_id, a.pk AS page_key,
                         similarity(a.flat, b.flat) AS flat_sim,
                         similarity(a.obj,  b.obj)  AS obj_sim,
                         (a.subj = b.subj) AS subj_eq,
                         (a.obj  = b.obj)  AS obj_eq,
                         a.flat AS keep_flat, b.flat AS dup_flat
                  FROM ok a JOIN ok b ON a.pk = b.pk AND a.id < b.id"
                 restrict)
               user-id graph-topic-id]
          (concat statuses [max-facts-per-page] ids ids))
        {:builder-fn rs/as-unqualified-maps}))))

(defn get-kg-facts-context
  "Approved facts for assistant grounding — same display-join as get-kg-facts,
   with an optional inclusive page range and an optional row limit so a large
   document or code repo cannot return unbounded rows.
   Pre: graph-topic-id owned by user-id. Post: rows shaped like get-kg-facts,
   all status='approved'; page_number ∈ [start-page,end-page] when both given
   (NULL-page rows are excluded by the range); ≤ limit rows when limit given."
  [user-id graph-topic-id {:keys [start-page end-page limit]}]
  (jdbc/execute! ds
    (sql/format
      (cond-> {:select [[:f.id :id] [:f.status :status]
                        [:f.page_number :page_number]
                        [:f.object_literal :object_literal]
                        [:f.subject_entity_id :subject_entity_id]
                        [:f.object_entity_id :object_entity_id]
                        [:f.predicate_id :predicate_id]
                        [:s.label :subject_label]
                        [:o.label :object_label]
                        [:p.label :predicate_label] [:p.slug :predicate_slug]]
               :from [[:kg_facts :f]]
               :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                      [:kg_predicates :p] [:= :p.id :f.predicate_id]]
               :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
               :where [:and [:= :f.user_id user-id]
                       [:= :f.graph_topic_id graph-topic-id]
                       [:= :f.status "approved"]
                       (when (and start-page end-page)
                         [:between :f.page_number start-page end-page])]
               :order-by [[:f.page_number :asc] [:f.id :asc]]}
        limit (assoc :limit limit)))
      {:builder-fn rs/as-unqualified-maps}))

(defn get-fact-mastery
  "Per-fact FSRS mastery signal, aggregated over the approved kg_questions linked
   to each fact via kg_question_facts. Shared by the assistant (weak-fact steering)
   and the facts→cards bridge (weak-first ordering).
   Pre:  fact-ids is a seq of kg_facts ids owned by user-id.
   Post: {fact-id {:tested? bool :due? bool :lapses int}} for every input id with
         ≥1 linked approved question. Ids with no linked approved question are
         OMITTED — the caller treats an absent id as untested (never quizzed).
         :tested? = some linked question reviewed ≥once; :due? = some introduced
         linked question is due now; :lapses = max lapses across linked questions."
  [user-id fact-ids]
  (if (empty? fact-ids)
    {}
    (into {}
      (map (juxt :fact_id
             (fn [r] {:tested? (boolean (:tested r))
                      :due? (boolean (:due r))
                      :lapses (or (:lapses r) 0)})))
      (jdbc/execute! ds
        (sql/format
          {:select [[:qf.fact_id :fact_id]
                    [[:raw "BOOL_OR(q.fsrs_reps > 0)"] :tested]
                    [[:raw "BOOL_OR(q.fsrs_due IS NOT NULL AND q.fsrs_due <= now())"] :due]
                    [[:raw "COALESCE(MAX(q.fsrs_lapses), 0)"] :lapses]]
           :from [[:kg_question_facts :qf]]
           :join [[:kg_questions :q] [:= :q.id :qf.question_id]]
           :where [:and [:= :q.user_id user-id]
                   [:= :q.status "approved"]
                   [:in :qf.fact_id (vec fact-ids)]]
           :group-by [:qf.fact_id]})
        {:builder-fn rs/as-unqualified-maps}))))

(defn kg-fact-stats
  "Per-document fact counts by status — feeds the Knowledge page badges.
   Post: {graph-topic-id {status-string n}}."
  [user-id]
  (reduce (fn [acc {:keys [graph_topic_id status n]}]
            (assoc-in acc [graph_topic_id status] n))
    {}
    (jdbc/execute! ds
      ["SELECT graph_topic_id, status, COUNT(*) AS n FROM kg_facts
        WHERE user_id = ? GROUP BY graph_topic_id, status"
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn list-kg-entities
  "Entities with their APPROVED fact usage count; trigram-filtered when query is
   non-blank, else busiest first. Optional `source-id` (a graph_topic_id) keeps
   only entities that appear in an approved fact of that document. aliases
   normalized PgArray → vector so rows are Electric-wire safe.

   Both fact reads filter status='approved', matching every other fact read in the
   app. They did not until 'duplicate' existed, and the omission was invisible
   while rejects were rare: an entity whose facts were all withdrawn still counted
   them, so the Entities view reported usage for a concept no longer in the graph.

   An entity a question-map diagnosis created is left out until a fact uses it:
   it names a concept a reader asked about, not one extraction found
   (plans/anchored-questions.md §6.4.2.3). Once extraction links a fact to it,
   it is an ordinary entity and is listed."
  ([user-id query limit] (list-kg-entities user-id query limit nil))
  ([user-id query limit source-id]
   (let [blank? (or (nil? query) (str/blank? query))
         ;; Optional source filter: entity participates in a fact of the document.
         ;; No FK on kg_entities — reachable via kg_facts subject/object columns.
         src-sql (if source-id
                   " AND EXISTS (SELECT 1 FROM kg_facts fs
                       WHERE (fs.subject_entity_id = e.id OR fs.object_entity_id = e.id)
                         AND fs.graph_topic_id = ? AND fs.status = 'approved')"
                   "")
         src-params (if source-id [source-id] [])
         base "SELECT e.id, e.label, e.aliases,
                 (SELECT COUNT(*) FROM kg_facts f
                  WHERE (f.subject_entity_id = e.id OR f.object_entity_id = e.id)
                    AND f.status = 'approved') AS fact_count
               FROM kg_entities e WHERE e.user_id = ?
                AND (NOT e.from_diagnosis OR EXISTS (
                      SELECT 1 FROM kg_facts fd
                       WHERE (fd.subject_entity_id = e.id OR fd.object_entity_id = e.id)
                         AND fd.status = 'approved'))"
         rows (if blank?
                (jdbc/execute! ds
                  (into [(str base src-sql
                           " ORDER BY fact_count DESC, e.label LIMIT ?")
                         user-id]
                    (concat src-params [limit]))
                  {:builder-fn rs/as-unqualified-maps})
                (jdbc/execute! ds
                  (into [(str base src-sql
                           " AND GREATEST(
                               similarity(e.label, ?),
                               COALESCE((SELECT MAX(similarity(a, ?)) FROM unnest(e.aliases) a), 0)
                             ) >= 0.2
                             ORDER BY GREATEST(
                               similarity(e.label, ?),
                               COALESCE((SELECT MAX(similarity(a, ?)) FROM unnest(e.aliases) a), 0)
                             ) DESC LIMIT ?")
                         user-id]
                    (concat src-params [query query query query limit]))
                  {:builder-fn rs/as-unqualified-maps}))]
     (kg-aliases->vec rows))))

(defn set-kg-fact-status!
  "Fact status transition. Approving also approves the fact's predicate;
   REJECTING retires every live question that covers the fact (the question's
   ground truth is gone — done-condition: reject never orphans a drawable
   question). Ownership enforced in SQL.
   Post: true iff the fact row changed."
  [user-id fact-id status]
  (jdbc/with-transaction [tx ds]
    (let [row (jdbc/execute-one! tx
                ["UPDATE kg_facts SET status = ?, reviewed_at = now()
                  WHERE id = ? AND user_id = ? RETURNING predicate_id"
                 status fact-id user-id]
                {:builder-fn rs/as-unqualified-maps})]
      (when (and row (= "approved" status))
        (jdbc/execute! tx
          ["UPDATE kg_predicates SET status = 'approved', reviewed_at = now()
            WHERE id = ? AND status = 'proposed'" (:predicate_id row)]))
      (when (and row (= "rejected" status))
        (jdbc/execute! tx
          ["UPDATE kg_questions SET status = 'retired'
            WHERE status = 'approved' AND id IN
              (SELECT question_id FROM kg_question_facts WHERE fact_id = ?)"
           fact-id]))
      (some? row))))

(defn reject-kg-question!
  "Reject a question and log an undo entry (so the toast can offer Undo).
   Returns the undo entry id, or nil when the question was missing or already
   rejected. The undo insert follows the mutation non-transactionally, matching
   insert-undo-entry!'s contract."
  [user-id question-id]
  (let [prev (jdbc/with-transaction [tx ds]
               (let [row (jdbc/execute-one! tx
                           ["SELECT status FROM kg_questions WHERE id = ? AND user_id = ?"
                            question-id user-id]
                           {:builder-fn rs/as-unqualified-maps})]
                 (when (and row (not= "rejected" (:status row)))
                   (jdbc/execute! tx
                     ["UPDATE kg_questions SET status = 'rejected' WHERE id = ? AND user_id = ?"
                      question-id user-id])
                   (:status row))))]
    (when prev
      (insert-undo-entry! user-id "reject-question" "question"
        [question-id]
        [{:question-id question-id :prev-status prev}]))))

(defn restore-rejected-question!
  "Undo a reject-question: restore the question's pre-reject status."
  [snapshot]
  (let [{:keys [question-id prev-status]} (first snapshot)]
    (jdbc/execute! ds
      ["UPDATE kg_questions SET status = ? WHERE id = ?" prev-status question-id])))

(defn reject-kg-fact!
  "Reject a fact and, like set-kg-fact-status!'s reject path, retire every live
   question that covers it — but snapshot the EXACT question ids retired so undo
   un-retires precisely those (never one already retired before this reject,
   never missing one). Returns the undo entry id, or nil when the fact was
   missing or already rejected."
  [user-id fact-id]
  (let [snap (jdbc/with-transaction [tx ds]
               (let [row (jdbc/execute-one! tx
                           ["SELECT status FROM kg_facts WHERE id = ? AND user_id = ?"
                            fact-id user-id]
                           {:builder-fn rs/as-unqualified-maps})]
                 (when (and row (not= "rejected" (:status row)))
                   ;; Capture the approved covering questions BEFORE retiring — these,
                   ;; and only these, are the ones this reject transitions to retired.
                   (let [qids (mapv :id
                                (jdbc/execute! tx
                                  ["SELECT id FROM kg_questions
                                    WHERE status = 'approved' AND id IN
                                      (SELECT question_id FROM kg_question_facts WHERE fact_id = ?)"
                                   fact-id]
                                  {:builder-fn rs/as-unqualified-maps}))]
                     (jdbc/execute! tx
                       ["UPDATE kg_facts SET status = 'rejected', reviewed_at = now()
                         WHERE id = ? AND user_id = ?" fact-id user-id])
                     (when (seq qids)
                       (jdbc/execute! tx
                         (sql/format {:update :kg_questions :set {:status "retired"}
                                      :where [:in :id qids]})))
                     {:fact-id fact-id :prev-status (:status row)
                      :retired-question-ids (vec qids)}))))]
    (when snap
      (insert-undo-entry! user-id "reject-fact" "fact" [fact-id] [snap]))))

(defn- withdraw-kg-facts!
  "Move many facts OUT of the approved set as ONE operation: one transaction, one
   undo entry. `target-status` is \"rejected\" (a human's judgement) or
   \"duplicate\" (freememo.kg-dedup's sweep); `action-type` names the undo entry.

   Not a loop over reject-kg-fact!. That would write one undo entry, one toast and
   one invalidation bump per fact — and the undo log is hard-capped at 100 rows per
   user, so a large selection would evict the user's unrelated undo history and
   truncate its own reversal.

   Per-fact precision is preserved, which is the whole difficulty: each fact keeps
   its own prev-status, and the retired question ids are recorded PER FACT rather
   than unioned, so undo un-retires exactly the questions this operation retired and
   never one that was already retired beforehand.

   Both statuses retire covering questions, and that is load-bearing: every fact
   read outside this namespace filters status='approved' (7 Ontop mapping queries,
   get-doc-facts*, get-kg-facts-context, kg-fact-ids-for-doc, cards-from-facts), so
   a withdrawn fact leaves any question grounded solely on it grading against
   nothing. Invariant: no approved question is left covering zero approved facts.
   Pre:  fact-ids from the user's selection or a sweep; ownership re-checked in SQL.
         target-status is a kg_facts_status_check value other than 'approved'.
   Post: {:withdrawn n :undo-id id-or-nil} — n counts only facts that were owned and
         not already in target-status; :undo-id is nil iff n = 0. Foreign ids and
         ids already in target-status are skipped silently, exactly as the
         single-fact path skips them."
  [user-id fact-ids target-status action-type]
  (if (empty? fact-ids)
    {:withdrawn 0 :undo-id nil}
    (let [{:keys [snaps undo-id]}
          (jdbc/with-transaction [tx ds]
            (let [rows (jdbc/execute! tx
                         (sql/format {:select [:id :status] :from :kg_facts
                                      :where [:and [:= :user_id user-id]
                                              [:in :id (vec fact-ids)]
                                              [:not= :status target-status]]
                                      :for :update})
                         {:builder-fn rs/as-unqualified-maps})
                  ids (mapv :id rows)]
              (if (empty? ids)
                {:snaps [] :undo-id nil}
                ;; Capture each fact's approved covering questions BEFORE retiring
                ;; anything: after the UPDATE below there is no way to tell which
                ;; questions this operation retired from ones already retired.
                (let [links (jdbc/execute! tx
                              (sql/format {:select [:qf.fact_id :q.id]
                                           :from [[:kg_question_facts :qf]]
                                           :join [[:kg_questions :q] [:= :q.id :qf.question_id]]
                                           :where [:and [:in :qf.fact_id ids]
                                                   [:= :q.status "approved"]]})
                              {:builder-fn rs/as-unqualified-maps})
                      by-fact (reduce (fn [m {:keys [fact_id id]}]
                                        (update m fact_id (fnil conj []) id))
                                {} links)
                      snaps (mapv (fn [{:keys [id status]}]
                                    {:fact-id id :prev-status status
                                     :retired-question-ids (vec (get by-fact id []))})
                              rows)
                      qids (into [] (distinct) (map :id links))]
                  (jdbc/execute! tx
                    (sql/format {:update :kg_facts
                                 :set {:status target-status :reviewed_at [:now]}
                                 :where [:and [:= :user_id user-id] [:in :id ids]]}))
                  (when (seq qids)
                    (jdbc/execute! tx
                      (sql/format {:update :kg_questions :set {:status "retired"}
                                   :where [:in :id qids]})))
                  {:snaps snaps
                   :undo-id (insert-undo-entry-raw! tx user-id action-type "fact"
                              ids snaps)}))))]
      ;; Pruning is deliberately outside the transaction — insert-undo-entry-raw!
      ;; does not prune, and the caller owns it once the mutation has committed.
      (when undo-id (prune-undo-log! ds user-id))
      {:withdrawn (count snaps) :undo-id undo-id})))

(defn reject-kg-facts!
  "Reject a selection of facts — a human's judgement that they are wrong.
   Post: {:rejected n :undo-id id-or-nil}; see withdraw-kg-facts! for the rest."
  [user-id fact-ids]
  (let [{:keys [withdrawn undo-id]}
        (withdraw-kg-facts! user-id fact-ids "rejected" "bulk-reject-facts")]
    {:rejected withdrawn :undo-id undo-id}))

(defn mark-kg-facts-duplicate!
  "Mark a sweep's findings as near-duplicates of a fact already in the document.
   Distinct from reject: the detector runs at 0.90 measured precision, so roughly
   one in ten marks is wrong and the user needs to tell machine marks from their
   own deletions to find and reverse them.
   Post: {:marked n :undo-id id-or-nil}; see withdraw-kg-facts! for the rest."
  [user-id fact-ids]
  (let [{:keys [withdrawn undo-id]}
        (withdraw-kg-facts! user-id fact-ids "duplicate" "mark-duplicate-facts")]
    {:marked withdrawn :undo-id undo-id}))

(defn unmark-kg-fact-duplicate!
  "Return one duplicate-marked fact to approved — the exit the Facts view's
   duplicate filter offers on every marked row.

   Writes 'approved' literally rather than replaying an undo snapshot: undo
   snapshots are pruned at 100 rows per user, so a mark old enough to have fallen
   out of the log would otherwise be unreversible. The sweep only ever withdraws
   approved facts (freememo.kg-dedup/candidate-pairs filters status='approved'),
   so 'approved' IS the prior status.

   Questions this mark retired are NOT un-retired — the undo entry owns that, and
   replaying it here would un-retire questions a later reject also retired.
   Pre:  fact-id owned by user-id; ownership enforced in SQL.
   Post: true iff a duplicate-marked row changed."
  [user-id fact-id]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_facts SET status = 'approved', reviewed_at = now()
              WHERE id = ? AND user_id = ? AND status = 'duplicate'"
             fact-id user-id]))))

(defn restore-withdrawn-facts!
  "Undo a bulk-reject-facts or a mark-duplicate-facts: restore every fact's own
   prior status and set exactly the questions that operation retired back to
   approved. Named for withdraw-kg-facts!, not for 'rejected' — it reverses both
   statuses that function writes.
   Pre:  snapshot is the vector of {:fact-id :prev-status :retired-question-ids}
         written by withdraw-kg-facts!.
   Post: each fact carries its own prior status again — not a single shared one."
  [snapshot]
  (jdbc/with-transaction [tx ds]
    (doseq [{:keys [fact-id prev-status]} snapshot]
      (jdbc/execute! tx
        ["UPDATE kg_facts SET status = ? WHERE id = ?" prev-status fact-id]))
    (let [qids (into [] (distinct) (mapcat :retired-question-ids snapshot))]
      (when (seq qids)
        (jdbc/execute! tx
          (sql/format {:update :kg_questions :set {:status "approved"}
                       :where [:in :id qids]}))))))

(defn restore-rejected-fact!
  "Undo a reject-fact: restore the fact's pre-reject status and set exactly the
   questions this reject retired back to approved."
  [snapshot]
  (let [{:keys [fact-id prev-status retired-question-ids]} (first snapshot)]
    (jdbc/with-transaction [tx ds]
      (jdbc/execute! tx
        ["UPDATE kg_facts SET status = ? WHERE id = ?" prev-status fact-id])
      (when (seq retired-question-ids)
        (jdbc/execute! tx
          (sql/format {:update :kg_questions :set {:status "approved"}
                       :where [:in :id retired-question-ids]}))))))

(defn update-kg-fact-literal!
  "Edit-then-approve for literal-object facts (entity edits go through
   entity rename/merge instead). Ownership enforced in SQL."
  [user-id fact-id literal]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_facts SET object_literal = ?
              WHERE id = ? AND user_id = ? AND object_literal IS NOT NULL"
             (sanitize-utf8 literal) fact-id user-id]))))

(defn rename-kg-entity!
  "Rename an entity; the old label joins aliases (deduped)."
  [user-id entity-id new-label]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_entities
              SET aliases = (SELECT ARRAY(SELECT DISTINCT x FROM unnest(aliases || label) x
                                          WHERE x <> ?)),
                  label = ?
              WHERE id = ? AND user_id = ?"
             (sanitize-utf8 new-label) (sanitize-utf8 new-label) entity-id user-id]))))

(defn merge-kg-entities!
  "Merge `absorbed-id` into `target-id`: facts repointed, labels/aliases
   unioned, absorbed row deleted. Facts whose repoint would collide with the
   per-document s/p/o unique indexes are deleted (they are duplicates by
   definition). All-or-nothing transaction.
   Pre:  both entities belong to user-id and differ (violation = caller bug).
   Post: no kg_facts row references absorbed-id; absorbed row gone."
  [user-id target-id absorbed-id]
  (jdbc/with-transaction [tx ds]
    (let [owned (jdbc/execute! tx
                  ["SELECT id FROM kg_entities WHERE user_id = ? AND id IN (?, ?)"
                   user-id target-id absorbed-id])]
      (when (or (= target-id absorbed-id) (not= 2 (count owned)))
        (throw (ex-info "merge-kg-entities!: entities must be two distinct rows owned by user"
                 {:user-id user-id :target target-id :absorbed absorbed-id})))
      ;; Duplicates-after-repoint die first, then the survivors repoint.
      (jdbc/execute! tx
        ["DELETE FROM kg_facts f WHERE f.subject_entity_id = ?
          AND EXISTS (SELECT 1 FROM kg_facts g
                      WHERE g.subject_entity_id = ? AND g.predicate_id = f.predicate_id
                        AND g.graph_topic_id = f.graph_topic_id
                        AND (g.object_entity_id = f.object_entity_id
                             OR (g.object_literal IS NOT NULL AND g.object_literal = f.object_literal)))"
         absorbed-id target-id])
      (jdbc/execute! tx
        ["DELETE FROM kg_facts f WHERE f.object_entity_id = ?
          AND EXISTS (SELECT 1 FROM kg_facts g
                      WHERE g.object_entity_id = ? AND g.predicate_id = f.predicate_id
                        AND g.graph_topic_id = f.graph_topic_id
                        AND g.subject_entity_id = f.subject_entity_id)"
         absorbed-id target-id])
      ;; Facts joining the pair in either direction (or absorbed to itself)
      ;; would become self-loops after repointing; drop them.
      (jdbc/execute! tx
        ["DELETE FROM kg_facts
          WHERE (subject_entity_id = ? AND object_entity_id IN (?, ?))
             OR (subject_entity_id = ? AND object_entity_id = ?)"
         absorbed-id target-id absorbed-id target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_facts SET subject_entity_id = ? WHERE subject_entity_id = ?"
         target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_facts SET object_entity_id = ? WHERE object_entity_id = ?"
         target-id absorbed-id])
      (jdbc/execute! tx
        ["UPDATE kg_entities t
          SET aliases = (SELECT ARRAY(SELECT DISTINCT x
                                      FROM unnest(t.aliases || a.aliases || a.label) x
                                      WHERE x <> t.label))
          FROM kg_entities a
          WHERE t.id = ? AND a.id = ?" target-id absorbed-id])
      (jdbc/execute! tx ["DELETE FROM kg_entities WHERE id = ?" absorbed-id])
      true)))

(defn get-kg-approved-predicates
  "The user's approved vocabulary, for the extraction prompt."
  [user-id]
  (jdbc/execute! ds
    ["SELECT slug, label FROM kg_predicates WHERE user_id = ? AND status = 'approved'
      ORDER BY label" user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn relink-kg-fact-object!
  "Point `fact-id`'s object at the entity named `label` — matched
   case-insensitively among the user's entities, created if absent. Fact-local:
   other facts referencing the old entity are untouched (rename-everywhere is
   rename-kg-entity!'s job). The old entity may become unreferenced; it stays
   visible in the entities browser for cleanup.
   Pre:  the fact is entity-valued and owned by user-id.
   Post: :done — object repointed; :duplicate — the target s/p/o already
         exists in this document (fact left unchanged); nil — fact not found /
         not entity-valued."
  [user-id fact-id label]
  (let [label (sanitize-utf8 (str/trim (str label)))]
    (when-not (str/blank? label)
      (jdbc/with-transaction [tx ds]
        (let [entity-id (or (:id (jdbc/execute-one! tx
                                   ["SELECT id FROM kg_entities
                                     WHERE user_id = ? AND lower(label) = lower(?) LIMIT 1"
                                    user-id label]
                                   {:builder-fn rs/as-unqualified-maps}))
                            (:id (jdbc/execute-one! tx
                                   ["INSERT INTO kg_entities (user_id, label) VALUES (?, ?) RETURNING id"
                                    user-id label]
                                   {:builder-fn rs/as-unqualified-maps})))]
          (try
            (when (pos? (::jdbc/update-count
                          (jdbc/execute-one! tx
                            ["UPDATE kg_facts SET object_entity_id = ?
                              WHERE id = ? AND user_id = ? AND object_entity_id IS NOT NULL
                                AND subject_entity_id <> ?"
                             entity-id fact-id user-id entity-id])))
              :done)
            (catch org.postgresql.util.PSQLException e
              (if (= "23505" (.getSQLState e)) ; unique_violation on the s/p/o index
                :duplicate
                (throw e)))))))))

;; ── Question bank ───────────────────────────────────────────────────────────

(defn create-kg-question!
  "Insert a question and its fact links in one transaction.
   Pre:  fact-ids non-empty, all owned by user-id (generation queries only the
         user's facts — violation = caller bug).
   Post: the new question id."
  [user-id kind question answer fact-ids source-model]
  (jdbc/with-transaction [tx ds]
    (let [qid (:id (jdbc/execute-one! tx
                     ["INSERT INTO kg_questions (user_id, kind, question, reference_answer, source_model)
                       VALUES (?, ?, ?, ?, ?) RETURNING id"
                      user-id kind (sanitize-utf8 question) (sanitize-utf8 answer) source-model]
                     {:builder-fn rs/as-unqualified-maps}))]
      (jdbc/execute! tx
        (sql/format {:insert-into :kg_question_facts
                     :values (mapv (fn [fid] {:question_id qid :fact_id fid}) fact-ids)}))
      qid)))

(defn get-kg-questions
  "The user's questions with covered-fact counts; non-blank `query` filters
   question + answer text case-insensitively. status nil = all live views
   should pass \"approved\". Optional opts {:source-id gid :entity-id eid}:
   source-id keeps questions with ANY linked fact in that document; entity-id
   keeps questions with a linked fact touching that entity (subject or object).
   Neither is a stored column on kg_questions — both route via kg_question_facts
   → kg_facts. Newest first.

   The bank is the unflag/unsuspend surface, so a suspended question MUST stay
   visible here — this is the one question query that does not apply
   drawable-question-where. Opts {:flagged? true} / {:suspended? true} narrow to
   the corresponding worklist; false and nil both mean \"don't filter\"."
  ([user-id status query] (get-kg-questions user-id status query nil))
  ([user-id status query {:keys [source-id entity-id flagged? suspended?]}]
   (let [q (when-not (or (nil? query) (str/blank? query))
             (str "%" (str/trim query) "%"))]
     (jdbc/execute! ds
       (sql/format {:select [[:q.id :id] [:q.kind :kind] [:q.question :question]
                             [:q.reference_answer :reference_answer]
                             [:q.status :status]
                             [:q.flagged :flagged] [:q.suspended :suspended]
                             [[:raw "(SELECT COUNT(*) FROM kg_question_facts qf WHERE qf.question_id = q.id)"]
                              :fact_count]]
                    :from [[:kg_questions :q]]
                    :where [:and [:= :q.user_id user-id]
                            (when status [:= :q.status status])
                            (when flagged? [:= :q.flagged true])
                            (when suspended? [:= :q.suspended true])
                            (when q [:or [:ilike :q.question q]
                                     [:ilike :q.reference_answer q]])
                            (when source-id
                              [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                        :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                        :where [:and [:= :qf.question_id :q.id]
                                                [:= :f.graph_topic_id source-id]]}])
                            (when entity-id
                              [:exists {:select [1] :from [[:kg_question_facts :qf2]]
                                        :join [[:kg_facts :f2] [:= :f2.id :qf2.fact_id]]
                                        :where [:and [:= :qf2.question_id :q.id]
                                                [:or [:= :f2.subject_entity_id entity-id]
                                                     [:= :f2.object_entity_id entity-id]]]}])]
                    :order-by [[:q.id :desc]]})
       {:builder-fn rs/as-unqualified-maps}))))

(defn fact-clusters-without-question
  "The atomic generator's work list, clustered by ambiguity.

   A cluster is every approved fact of the document sharing one
   (subject_entity_id, predicate_id) — the direction in which a question has more
   than one true answer: one subject, one predicate, N objects, so \"what does S
   P?\" is answerable N ways and is unlearnable as a single item. A cluster of size
   1 is unambiguous and yields one direct question.

   :members carries EVERY approved sibling, covered or not, because the generator
   must know the ambiguity is over N to avoid naming one member in another's
   question. :targets are the members with no live atomic question yet — the only
   ones a question is written for. A cluster whose targets are all covered is
   omitted entirely.

   Post: [{:subject_label :predicate_label
           :members [{:id :object_label :object_literal :object_entity_id
                      :page_number}]
           :targets #{fact-id}}], :targets non-empty, ordered by the cluster's
         lowest fact id so two runs over unchanged data agree."
  [user-id graph-topic-id]
  (let [rows (jdbc/execute! ds
               ["SELECT f.id, f.subject_entity_id, f.predicate_id, f.object_entity_id,
                        f.object_literal, f.page_number,
                        s.label AS subject_label, p.label AS predicate_label,
                        o.label AS object_label,
                        EXISTS (SELECT 1 FROM kg_question_facts qf
                                JOIN kg_questions kq ON kq.id = qf.question_id
                                WHERE qf.fact_id = f.id AND kq.kind = 'atomic'
                                  AND kq.status = 'approved') AS covered
                 FROM kg_facts f
                 JOIN kg_entities s ON s.id = f.subject_entity_id
                 JOIN kg_predicates p ON p.id = f.predicate_id
                 LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                 WHERE f.user_id = ? AND f.graph_topic_id = ? AND f.status = 'approved'
                 ORDER BY f.id"
                user-id graph-topic-id]
               {:builder-fn rs/as-unqualified-maps})]
    (->> (group-by (juxt :subject_entity_id :predicate_id) rows)
      (keep (fn [[_key members]]
              (let [targets (into #{} (comp (remove :covered) (map :id)) members)]
                (when (seq targets)
                  {:subject_label (:subject_label (first members))
                   :predicate_label (:predicate_label (first members))
                   :members (mapv #(select-keys % [:id :object_label :object_literal
                                                   :object_entity_id :page_number])
                              members)
                   :targets targets}))))
      (sort-by #(reduce min (map :id (:members %))))
      vec)))

(defn kg-entity-discriminators
  "Approved facts touching each of `entity-ids`, as compact prompt rows keyed by
   entity id — the material that lets a question ask for one member of an ambiguous
   cluster by a property it alone has, instead of asking for \"one of\" the group.

   One query for the whole batch: called once per generation batch, never per
   object. An entity is absent from the result when it has no approved facts at all
   — the caller reads that as \"undiscriminable\" and omits the member.

   Post: {entity-id [{:fact_id :s :p :o}]}, ≤ `per-entity` rows each, lowest fact
         id first."
  [user-id entity-ids per-entity]
  (if (empty? entity-ids)
    {}
    (let [ids (vec (distinct entity-ids))
          wanted (set ids)
          rows (jdbc/execute! ds
                 (sql/format
                   {:select [[:f.id :fact_id] :f.subject_entity_id :f.object_entity_id
                             [:s.label :s] [:p.label :p]
                             [[:coalesce :o.label :f.object_literal] :o]]
                    :from [[:kg_facts :f]]
                    :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                           [:kg_predicates :p] [:= :p.id :f.predicate_id]]
                    :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
                    :where [:and [:= :f.user_id user-id] [:= :f.status "approved"]
                            [:or [:in :f.subject_entity_id ids]
                             [:in :f.object_entity_id ids]]]
                    :order-by [[:f.id :asc]]})
                 {:builder-fn rs/as-unqualified-maps})
          row->prompt #(select-keys % [:fact_id :s :p :o])]
      ;; A fact can touch two wanted entities; it discriminates both.
      (reduce (fn [acc row]
                (reduce (fn [a eid]
                          (if (and (wanted eid) (< (count (get a eid)) per-entity))
                            (update a eid (fnil conj []) (row->prompt row))
                            a))
                  acc
                  [(:subject_entity_id row) (:object_entity_id row)]))
        {} rows))))

(defn entity-fact-neighborhood
  "Approved facts touching an entity (as subject or object), labeled with
   provenance and entity ids — input for synthesis generation AND the concept
   popover (whose rows link onward through the ids).
   Post: rows {:id :subject_label :predicate_label :object_label
   :object_literal :page_number :doc_title :graph_topic_id :subject_entity_id
   :object_entity_id}."
  [user-id entity-id]
  (jdbc/execute! ds
    ["SELECT f.id, s.label AS subject_label, p.label AS predicate_label,
             o.label AS object_label, f.object_literal, f.page_number,
             t.title AS doc_title, f.graph_topic_id,
             f.subject_entity_id, f.object_entity_id
      FROM kg_facts f
      JOIN kg_entities s ON s.id = f.subject_entity_id
      JOIN kg_predicates p ON p.id = f.predicate_id
      JOIN topics t ON t.id = f.graph_topic_id
      LEFT JOIN kg_entities o ON o.id = f.object_entity_id
      WHERE f.user_id = ? AND f.status = 'approved'
        AND (f.subject_entity_id = ? OR f.object_entity_id = ?)
      ORDER BY f.id"
     user-id entity-id entity-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-entity-card
  "Entity + its fact neighborhood — the concept popover's payload.
   Post: {:id :label :aliases [str] :facts [...]} (wire-safe) or nil."
  [user-id entity-id]
  (when-let [e (jdbc/execute-one! ds
                 ["SELECT id, label, aliases FROM kg_entities
                   WHERE id = ? AND user_id = ?" entity-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (assoc (first (kg-aliases->vec [e]))
      :facts (entity-fact-neighborhood user-id entity-id))))

(defn kg-graph-elements
  "The edges-only concept graph for the Graph tab. Node = an entity that
   participates in ≥1 approved entity→entity fact; edge = such a fact.
   Literal-object facts are excluded (no second node) — they reach the UI only
   through kg-entity-card's side panel. Degree is derived from the edge list by
   the caller (freememo.kg-graph), not per-node subqueries.
   Post: {:nodes [{:id :label}] :edges [{:s :t :p :topic}] :predicates
   [{:id :label}] :docs [{:id :title}]} — all wire-safe scalars."
  [user-id]
  (let [edges (jdbc/execute! ds
                ["SELECT subject_entity_id AS s, object_entity_id AS t,
                         predicate_id AS p, graph_topic_id AS topic
                  FROM kg_facts
                  WHERE user_id = ? AND status = 'approved'
                    AND object_entity_id IS NOT NULL"
                 user-id]
                {:builder-fn rs/as-unqualified-maps})
        nodes (jdbc/execute! ds
                ["SELECT id, label FROM kg_entities
                  WHERE user_id = ? AND id IN (
                    SELECT subject_entity_id FROM kg_facts
                      WHERE user_id = ? AND status = 'approved' AND object_entity_id IS NOT NULL
                    UNION
                    SELECT object_entity_id FROM kg_facts
                      WHERE user_id = ? AND status = 'approved' AND object_entity_id IS NOT NULL)"
                 user-id user-id user-id]
                {:builder-fn rs/as-unqualified-maps})
        predicates (jdbc/execute! ds
                     ["SELECT DISTINCT p.id, p.label FROM kg_predicates p
                       JOIN kg_facts f ON f.predicate_id = p.id
                       WHERE f.user_id = ? AND f.status = 'approved'
                         AND f.object_entity_id IS NOT NULL"
                      user-id]
                     {:builder-fn rs/as-unqualified-maps})
        docs (jdbc/execute! ds
               ["SELECT DISTINCT t.id, t.title FROM topics t
                 JOIN kg_facts f ON f.graph_topic_id = t.id
                 WHERE f.user_id = ? AND f.status = 'approved'
                   AND f.object_entity_id IS NOT NULL"
                user-id]
               {:builder-fn rs/as-unqualified-maps})]
    {:nodes nodes :edges edges :predicates predicates :docs docs}))

(defn read-graph-layout
  "Cached render payload for a Graph-tab scope, or nil on cold cache.
   Post: {:version <long> :payload {...}} | nil."
  [user-id scope]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT version, payload FROM kg_graph_layout
                     WHERE user_id = ? AND scope = ?" user-id scope]
                   {:builder-fn rs/as-unqualified-maps})]
    {:version (:version row) :payload (pgobject->clj (:payload row))}))

(defn write-graph-layout!
  "Upsert the positioned render payload for (user, scope) at version.
   Post: exactly one row for (user, scope); its version/payload are the args."
  [user-id scope version payload]
  (jdbc/execute! ds
    ["INSERT INTO kg_graph_layout (user_id, scope, version, payload, computed_at)
      VALUES (?, ?, ?, ?, now())
      ON CONFLICT (user_id, scope)
      DO UPDATE SET version = EXCLUDED.version, payload = EXCLUDED.payload,
                    computed_at = now()"
     user-id scope version (->jsonb payload)]))

(defn set-kg-question-status!
  "reject (curation) or retire. Ownership enforced in SQL.
   Post: true iff the row changed."
  [user-id question-id status]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions SET status = ? WHERE id = ? AND user_id = ?"
             status question-id user-id]))))

(defn set-kg-question-flags!
  "Set a question's curation flags; nil field = keep current. Ownership enforced in
   SQL. Writes NO FSRS column by design (Anki parity): a suspension neither freezes
   nor shifts the schedule, so a question unsuspended after its due date returns as
   overdue rather than resurfacing on a fabricated timeline.
   Post: true iff the row exists and is owned; the question's fsrs_* columns are
         bit-identical across any suspend/unsuspend round-trip."
  [user-id question-id {:keys [flagged suspended]}]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions
              SET flagged = COALESCE(?, flagged),
                  suspended = COALESCE(?, suspended)
              WHERE id = ? AND user_id = ?"
             flagged suspended question-id user-id]))))

(defn update-kg-question!
  "Edit question text and/or reference answer (nil field = keep current)."
  [user-id question-id question answer]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_questions
              SET question = COALESCE(?, question),
                  reference_answer = COALESCE(?, reference_answer)
              WHERE id = ? AND user_id = ?"
             (some-> question sanitize-utf8) (some-> answer sanitize-utf8)
             question-id user-id]))))

;; ── Quiz/exam sessions ──────────────────────────────────────────────────────

(defn- sql-int-array->vec [a]
  (some->> ^java.sql.Array a .getArray (mapv long)))

;; Write-side counterparts to sql-int-array->vec. A bare ARRAY[] has no inferable
;; element type in Postgres, so the EMPTY case must carry an explicit cast. The
;; failure mode is nasty enough to be worth centralising: it is a runtime SQL error
;; on the empty path only, so a writer that is only ever tested with a non-empty
;; array looks correct and then throws in production the first time it is handed
;; nothing — inside an Electric server thunk that kills the session.

(defn- int-array-value
  "HoneySQL value for an INTEGER[] column.
   Post: an expression valid for both empty and non-empty `ids`."
  [ids]
  (if (seq ids) [:array (vec ids)] [:raw "'{}'::integer[]"]))

(defn- text-array-value
  "HoneySQL value for a TEXT[] column; elements are UTF-8 sanitised.
   Post: an expression valid for both empty and non-empty `strs`."
  [strs]
  (if (seq strs) [:array (mapv sanitize-utf8 strs)] [:raw "'{}'::text[]"]))

(defn- drawable-question-where
  "SQL predicate for \"this question may be drawn into a quiz or review\": approved
   curation status AND not suspended. The single definition shared by every draw
   site (kg-questions-per-doc, draw-kg-questions, draw-fsrs-due-queue) so the scope
   picker's counts can never drift from what a draw actually returns, and a future
   withholding rule is added in one place.
   Pre:  `alias` is the kg_questions alias in the enclosing query (no user input).
   Post: a SQL fragment safe to AND into a WHERE clause."
  [alias]
  (str alias ".status = 'approved' AND NOT " alias ".suspended"))

(defn kg-questions-per-doc
  "Drawable-question count per document, for the quiz scope picker. Counts exactly
   what a draw over that document would be allowed to return, suspensions included.
   Post: {graph-topic-id n} (a question spanning docs counts in each)."
  [user-id]
  (into {}
    (map (juxt :graph_topic_id :n))
    (jdbc/execute! ds
      [(str "SELECT f.graph_topic_id, COUNT(DISTINCT q.id) AS n
             FROM kg_questions q
             JOIN kg_question_facts qf ON qf.question_id = q.id
             JOIN kg_facts f ON f.id = qf.fact_id
             WHERE q.user_id = ? AND " (drawable-question-where "q")
        " GROUP BY f.graph_topic_id")
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn draw-kg-questions
  "Random draw of up to n drawable questions ALL of whose facts lie inside the
   scope documents (a question must be answerable from the chosen material).
   Post: vector of question ids, ≤ n, random order; no suspended question."
  [user-id scope-topic-ids n]
  (mapv :id
    (jdbc/execute! ds
      (sql/format {:select [:q.id]
                   :from [[:kg_questions :q]]
                   :where [:and [:= :q.user_id user-id]
                           [:raw (drawable-question-where "q")]
                           [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                     :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                     :where [:and [:= :qf.question_id :q.id]
                                             [:in :f.graph_topic_id scope-topic-ids]]}]
                           [:not [:exists {:select [1] :from [[:kg_question_facts :qf]]
                                           :join [[:kg_facts :f] [:= :f.id :qf.fact_id]]
                                           :where [:and [:= :qf.question_id :q.id]
                                                   [:not-in :f.graph_topic_id scope-topic-ids]]}]]]
                   :order-by [[[:raw "random()"]]]
                   :limit n})
      {:builder-fn rs/as-unqualified-maps})))

(defn create-kg-session!
  "Freeze a draw into a session row. Pre: question-ids non-empty (callers
   refuse an empty draw — violation = caller bug). Post: session id."
  [user-id kind scope-topic-ids question-ids time-limit-seconds]
  (:id (jdbc/execute-one! ds
         (sql/format {:insert-into :kg_sessions
                      :values [{:user_id user-id :kind kind
                                :scope_topic_ids [:array (vec scope-topic-ids)]
                                :question_ids [:array (vec question-ids)]
                                :time_limit_seconds time-limit-seconds}]
                      :returning [:id]})
         {:builder-fn rs/as-unqualified-maps})))

(defn get-active-kg-session
  "Latest unfinished session of `kind`, wire-lean, or nil.
   Post: {:id :kind :question-ids [long] :answered n :time-limit-seconds
          :elapsed-seconds n} — :answered counts SAVED answers (graded or
   not), so an exam resumes at the first unanswered question even when
   grading hasn't run. Timestamps stay server-side; the client gets derived
   numbers only."
  [user-id kind]
  (when-let [row (jdbc/execute-one! ds
                   ["SELECT id, kind, question_ids, time_limit_seconds,
                       EXTRACT(EPOCH FROM (now() - started_at))::bigint AS elapsed_seconds,
                       (SELECT COUNT(*) FROM kg_answers a
                        WHERE a.session_id = s.id) AS answered
                     FROM kg_sessions s
                     WHERE user_id = ? AND kind = ? AND finished_at IS NULL
                     ORDER BY id DESC LIMIT 1"
                    user-id kind]
                   {:builder-fn rs/as-unqualified-maps})]
    {:id (:id row)
     :kind (:kind row)
     :question-ids (sql-int-array->vec (:question_ids row))
     :answered (long (:answered row))
     :time-limit-seconds (:time_limit_seconds row)
     :elapsed-seconds (long (:elapsed_seconds row))}))

(defn get-kg-question-for-session
  "Everything one quiz turn needs: the question, its facts with document +
   page provenance, and the highlight lexicon (labels + aliases of every
   entity in the linked facts). Wire-lean. nil when not found/not owned."
  [user-id question-id]
  (when-let [q (jdbc/execute-one! ds
                 ["SELECT id, kind, question, reference_answer, flagged, suspended
                   FROM kg_questions
                   WHERE id = ? AND user_id = ?" question-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (let [facts (jdbc/execute! ds
                  ["SELECT f.id, s.label AS subject_label, p.label AS predicate_label,
                       o.label AS object_label, f.object_literal, f.page_number,
                       t.title AS doc_title, f.graph_topic_id,
                       f.subject_entity_id, f.object_entity_id
                    FROM kg_question_facts qf
                    JOIN kg_facts f ON f.id = qf.fact_id
                    JOIN kg_entities s ON s.id = f.subject_entity_id
                    JOIN kg_predicates p ON p.id = f.predicate_id
                    JOIN topics t ON t.id = f.graph_topic_id
                    LEFT JOIN kg_entities o ON o.id = f.object_entity_id
                    WHERE qf.question_id = ?" question-id]
                  {:builder-fn rs/as-unqualified-maps})
          entity-ids (into #{} (comp (mapcat (juxt :subject_entity_id :object_entity_id))
                                 (remove nil?))
                       facts)
          entities (when (seq entity-ids)
                     (kg-aliases->vec
                       (jdbc/execute! ds
                         (sql/format {:select [:id :label :aliases]
                                      :from [:kg_entities]
                                      :where [:in :id (vec entity-ids)]})
                         {:builder-fn rs/as-unqualified-maps})))
          keywords (into []
                     (comp (mapcat (fn [{:keys [label aliases]}] (cons label aliases)))
                       (remove str/blank?)
                       (distinct))
                     entities)]
      {:id (:id q) :kind (:kind q)
       :question (:question q) :reference-answer (:reference_answer q)
       :flagged (boolean (:flagged q)) ; the in-quiz Flag toggle renders from this
       :suspended (boolean (:suspended q))
       :facts facts ; entity ids stay on rows — feedback links through them
       :entities (or entities [])
       :keywords keywords})))

(defn kg-facts-labeled
  "The given facts with labels and provenance — same row shape as
   entity-fact-neighborhood, so FactLine renders them unchanged. Used by the review
   detail to show missed facts. Ownership enforced in SQL.
   Post: labeled rows for the owned subset of `fact-ids`, lowest id first (a
         cascade-deleted fact simply drops out)."
  [user-id fact-ids]
  (if (empty? fact-ids)
    []
    (jdbc/execute! ds
      (sql/format
        {:select [[:f.id :id] [:s.label :subject_label] [:p.label :predicate_label]
                  [:o.label :object_label] [:f.object_literal :object_literal]
                  [:f.page_number :page_number] [:t.title :doc_title]
                  [:f.graph_topic_id :graph_topic_id]
                  [:f.subject_entity_id :subject_entity_id]
                  [:f.object_entity_id :object_entity_id]]
         :from [[:kg_facts :f]]
         :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                [:kg_predicates :p] [:= :p.id :f.predicate_id]
                [:topics :t] [:= :t.id :f.graph_topic_id]]
         :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
         :where [:and [:= :f.user_id user-id] [:in :f.id (vec fact-ids)]]
         :order-by [[:f.id :asc]]})
      {:builder-fn rs/as-unqualified-maps})))

(defn record-kg-answer!
  "Persist the user's answer text before grading (an LLM failure must not
   lose what they typed). Idempotent per (session, question).

   question_text is snapshotted here rather than passed in by each of the three
   call sites, and is deliberately NOT touched by the upsert: it records the
   wording as FIRST asked, so re-answering within a session cannot rewrite the
   record the history list renders.
   Post: answer row id; question_text set on insert and unchanged thereafter."
  [user-id session-id question-id position user-answer]
  (:id (jdbc/execute-one! ds
         ["INSERT INTO kg_answers (session_id, question_id, position, user_answer, question_text)
           SELECT s.id, ?, ?, ?, (SELECT q.question FROM kg_questions q WHERE q.id = ?)
           FROM kg_sessions s
           WHERE s.id = ? AND s.user_id = ?
           ON CONFLICT (session_id, question_id)
             DO UPDATE SET user_answer = EXCLUDED.user_answer
           RETURNING id"
          question-id position (sanitize-utf8 user-answer) question-id session-id user-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn grade-kg-answer!
  "Write the grading verdict. Only freememo.kg-grade calls this — it owns the
   ⊆-validation of missed-fact-ids and matched-keywords."
  [answer-id verdict explanation missed-fact-ids matched-keywords]
  (jdbc/execute-one! ds
    (sql/format {:update :kg_answers
                 :set {:verdict verdict
                       :explanation (sanitize-utf8 explanation)
                       :missed_fact_ids (int-array-value missed-fact-ids)
                       :matched_keywords (text-array-value matched-keywords)
                       :graded_at [:now]}
                 :where [:= :id answer-id]}))
  answer-id)

(defn finish-kg-session!
  "Close a session (idempotent). Post: true iff this call closed it."
  [user-id session-id]
  (pos? (::jdbc/update-count
          (jdbc/execute-one! ds
            ["UPDATE kg_sessions SET finished_at = now()
              WHERE id = ? AND user_id = ? AND finished_at IS NULL"
             session-id user-id]))))

(defn kg-session-verdict-counts
  "Graded-verdict tally for a session's summary. Post: {verdict-string n}."
  [user-id session-id]
  (into {}
    (map (juxt :verdict :n))
    (jdbc/execute! ds
      ["SELECT a.verdict, COUNT(*) AS n FROM kg_answers a
        JOIN kg_sessions s ON s.id = a.session_id
        WHERE s.id = ? AND s.user_id = ? AND a.verdict IS NOT NULL
        GROUP BY a.verdict"
       session-id user-id]
      {:builder-fn rs/as-unqualified-maps})))

;; ---------------------------------------------------------------------------
;; FSRS review scheduling (freememo.fsrs). The Review quiz is the sole writer;
;; custom quiz/exam never touch these. "Today" is server-local (CURRENT_DATE),
;; matching every other due-query in this file.
;; ---------------------------------------------------------------------------

(defn fsrs-daily-counts
  "Today's FSRS tallies for cap enforcement, from the append-only log.
   Post: {:new-today n :reviews-today n} — new = first-ever reviews
   (reps_before = 0); reviews = reviews of cards already in Review state."
  [user-id]
  (let [row (jdbc/execute-one! ds
              ["SELECT COUNT(*) FILTER (WHERE reps_before = 0)  AS new_today,
                       COUNT(*) FILTER (WHERE state_before = 2) AS reviews_today
                FROM kg_reviews
                WHERE user_id = ? AND reviewed_at::date = CURRENT_DATE"
               user-id]
              {:builder-fn rs/as-unqualified-maps})]
    {:new-today (long (:new_today row)) :reviews-today (long (:reviews_today row))}))

;; --- Card quiz items -------------------------------------------------------
;; A quiz item is [:question id] or [:card flashcard-id ord]. The draw, the client
;; queue, apply-fsrs-review! and the review log all speak this one shape
;; (plans/cards-in-quiz-queue.md §1.5).

(defn- card-quiz-where
  "SQL predicate for \"this flashcard yields quiz items\". Occlusion, score and
   overlapping cards carry no typed answer, so the quiz never asks them.
   Pre:  `alias` is the flashcards alias in the enclosing query (no user input).
   Post: a SQL fragment safe to AND into a WHERE clause."
  [alias]
  (str alias ".kind IN ('basic','cloze')"))

(defn- live-ord?
  "Whether `ord` still exists in this card's text — the guard that keeps a stale
   schedule row (its deletion edited away) out of a draw. A basic card has exactly
   ord 0; a cloze card's ords come from freememo.cloze, the single scan.
   Pre:  `kind` is 'basic' or 'cloze' (card-quiz-where guarantees it)."
  [kind cloze-text ord]
  (if (= "basic" kind)
    (zero? ord)
    (boolean (some #{ord} (cloze/ords cloze-text)))))

(defn- card-refs
  "Rows of {:flashcard_id :ord :kind :cloze} → [:card id ord] refs, dropping any
   whose ord no longer exists in the card text.
   Post: order preserved; every ref's ord is live."
  [rows]
  (into []
    (comp (filter #(live-ord? (:kind %) (:cloze %) (:ord %)))
      (map (fn [r] [:card (:flashcard_id r) (long (:ord r))])))
    rows))

(defn- due-card-items
  "Card items whose schedule is due, for one tier. Not SQL-limited: the ord-liveness
   filter runs in Clojure, so a LIMIT here could silently under-fill. The caller's
   cap bounds the result instead.
   Post: refs ordered by due ascending."
  [user-id where]
  (card-refs
    (jdbc/execute! ds
      [(str "SELECT cs.flashcard_id, cs.ord, f.kind, f.cloze
             FROM card_schedules cs
             JOIN flashcards f ON f.id = cs.flashcard_id
             JOIN topics root ON root.id = f.root_topic_id
             WHERE root.user_id = ? AND " (card-quiz-where "f")
        " AND " where
        " ORDER BY cs.due ASC")
       user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn- fresh-card-items
  "Card items never yet reviewed, up to `room`.

   Two passes, cheap first. An untouched card (no schedule row at all) yields every
   ord it has, so a SQL LIMIT over untouched cards is safe and usually fills the
   room on its own. Only when it does not do we scan partially-reviewed cloze cards,
   whose fresh ords need the text parsed.
   Post: ≤ room refs; none has a card_schedules row."
  [user-id room]
  (let [untouched
        (into []
          (mapcat (fn [{:keys [id kind cloze]}]
                    (if (= "basic" kind)
                      [[:card id 0]]
                      (map (fn [n] [:card id n]) (cloze/ords cloze)))))
          (jdbc/execute! ds
            [(str "SELECT f.id, f.kind, f.cloze FROM flashcards f
                   JOIN topics root ON root.id = f.root_topic_id
                   WHERE root.user_id = ? AND " (card-quiz-where "f")
              " AND NOT EXISTS (SELECT 1 FROM card_schedules cs
                                WHERE cs.flashcard_id = f.id)
                ORDER BY f.id LIMIT ?")
             user-id room]
            {:builder-fn rs/as-unqualified-maps}))]
    (if (>= (count untouched) room)
      (into [] (take room) untouched)
      (let [partial-refs
            (into []
              (mapcat (fn [{:keys [id cloze ords]}]
                        (let [seen (set (sql-int-array->vec ords))]
                          (into [] (comp (remove seen) (map (fn [n] [:card id n])))
                            (cloze/ords cloze)))))
              (jdbc/execute! ds
                ["SELECT f.id, f.cloze, array_agg(cs.ord) AS ords
                  FROM flashcards f
                  JOIN topics root ON root.id = f.root_topic_id
                  JOIN card_schedules cs ON cs.flashcard_id = f.id
                  WHERE root.user_id = ? AND f.kind = 'cloze'
                  GROUP BY f.id, f.cloze
                  ORDER BY f.id"
                 user-id]
                {:builder-fn rs/as-unqualified-maps}))]
        (into [] (take room) (concat untouched partial-refs))))))

(defn- alternate
  "Lazily take from `a` and `b` in turn; when one runs out the other supplies the
   rest. Post: a seq of (count a) + (count b) items, each input's order preserved."
  [a b]
  (lazy-seq
    (cond
      (empty? a) b
      (empty? b) a
      :else (cons (first a) (cons (first b) (alternate (rest a) (rest b)))))))

(defn draw-review-queue
  "Ordered quiz items for a Review sitting, mixing questions and card items.

   Three tiers in order: learning/relearning due now (uncapped), then Review items
   due today (capped at review-limit − reviews so far), then never-introduced items
   (capped at new-limit − new so far). Inside each capped tier, questions and cards
   alternate, so a card bank several times the size of the question bank cannot
   crowd questions out (plans/cards-in-quiz-queue.md D11). One budget spans both
   types — fsrs_daily_counts reads the single kg_reviews log.

   The client holds this as a live queue and re-enqueues items still due today.
   Post: vector of item refs, order learning → review → new; no suspended question
         and no stale cloze ord in any tier."
  [user-id new-limit review-limit]
  (let [{:keys [new-today reviews-today]} (fsrs-daily-counts user-id)
        review-room (max 0 (- review-limit reviews-today))
        new-room    (max 0 (- new-limit new-today))
        q (fn [where order limit]
            (mapv (fn [r] [:question (:id r)])
              (jdbc/execute! ds
                (into [(str "SELECT q.id FROM kg_questions q
                             WHERE q.user_id = ? AND " (drawable-question-where "q")
                            " AND " where
                            " ORDER BY " order (when limit " LIMIT ?"))]
                  (cond-> [user-id] limit (conj limit)))
                {:builder-fn rs/as-unqualified-maps})))
        ;; Uncapped, so alternating cannot starve either type — it only mixes them.
        learning (alternate
                   (q "q.fsrs_state IN (1,3) AND q.fsrs_due IS NOT NULL AND q.fsrs_due <= now()"
                     "q.fsrs_due ASC" nil)
                   (due-card-items user-id
                     "cs.state IN (1,3) AND cs.due IS NOT NULL AND cs.due <= now()"))
        reviews  (if (pos? review-room)
                   (take review-room
                     (alternate
                       (q "q.fsrs_state = 2 AND q.fsrs_due IS NOT NULL AND q.fsrs_due::date <= CURRENT_DATE"
                         "q.fsrs_due ASC" review-room)
                       (due-card-items user-id
                         "cs.state = 2 AND cs.due IS NOT NULL AND cs.due::date <= CURRENT_DATE")))
                   [])
        news     (if (pos? new-room)
                   (take new-room
                     (alternate (q "q.fsrs_due IS NULL" "q.id ASC" new-room)
                       (fresh-card-items user-id new-room)))
                   [])]
    (vec (concat learning reviews news))))

(defn- load-question-memory!
  "The question's FSRS state, row-locked. nil when it is not this user's.
   Post: {:state :step :stability :difficulty :reps :lapses :gap_secs} or nil."
  [tx user-id question-id]
  (jdbc/execute-one! tx
    ["SELECT fsrs_state AS state, fsrs_step AS step, fsrs_stability AS stability,
             fsrs_difficulty AS difficulty, fsrs_reps AS reps, fsrs_lapses AS lapses,
             EXTRACT(EPOCH FROM (now() - fsrs_last_review)) AS gap_secs
      FROM kg_questions
      WHERE id = ? AND user_id = ? FOR UPDATE"
     question-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn- load-card-memory!
  "The card item's FSRS state, row-locked, creating the row on first review.

   A missing row means never introduced, so it is inserted at fsrs/new-card state
   before the advance — this is where §2.3's lazy materialization happens. Ownership
   comes from flashcards.root_topic_id → topics; nil when the card is not this
   user's, in which case nothing is written.
   Post: the same shape load-question-memory! returns, or nil."
  [tx user-id card-id ord]
  (when (= user-id (:user_id (jdbc/execute-one! tx
                               ["SELECT root.user_id FROM flashcards f
                                 JOIN topics root ON root.id = f.root_topic_id
                                 WHERE f.id = ?" card-id]
                               {:builder-fn rs/as-unqualified-maps})))
    (jdbc/execute-one! tx
      ["INSERT INTO card_schedules (flashcard_id, ord, state, step)
        VALUES (?, ?, ?, ?)
        ON CONFLICT (flashcard_id, ord) DO NOTHING"
       card-id ord (:state fsrs/new-card) (:step fsrs/new-card)])
    (jdbc/execute-one! tx
      ["SELECT state, step, stability, difficulty, reps, lapses,
               EXTRACT(EPOCH FROM (now() - last_review)) AS gap_secs
        FROM card_schedules
        WHERE flashcard_id = ? AND ord = ? FOR UPDATE"
       card-id ord]
      {:builder-fn rs/as-unqualified-maps})))

(def duration-cap-ms
  "Upper bound on one turn's recorded duration, in milliseconds.

   A learner who walks away mid-turn would otherwise record hours and poison every
   average that reads the column. 120 s rather than Anki's 60 s because both graded
   arms here take a typed free-text answer, which routinely exceeds a minute."
  120000)

(defn- clamp-duration-ms
  "Bound one client-supplied turn duration.

   The value crosses from an untrusted peer, so this is the single place it is
   bounded — every kg_reviews write goes through apply-fsrs-review!, so a caller
   cannot skip it.
   Post: nil (not recorded) for a nil or non-numeric input; otherwise an int in
         [0, duration-cap-ms]. Never throws."
  [v]
  (when (number? v)
    (-> (long v) (max 0) (min duration-cap-ms))))

(defn apply-fsrs-review!
  "Advance one quiz item's FSRS schedule by `rating` and append a kg_reviews row,
   in one transaction. Postgres now() is the transaction clock, so the elapsed gap
   and the new due date are computed against a single consistent instant.

   `item-ref` is [:question id] or [:card flashcard-id ord]. Both types advance by
   the same freememo.fsrs step and land in the same log; only the state table and
   the wording snapshot differ.

   The Review flow writes no kg_answers row, so this row IS its answer history:
   `record` carries {:verdict :explanation :user-answer :missed-fact-ids
   :grade-source :duration-ms} and the item's current wording is snapshotted by
   subselect. On the self-graded arm there is no verdict, explanation or answer —
   :grade-source 'self' is what tells that row apart from one written before
   grade_source existed. :duration-ms is untrusted and passes through
   clamp-duration-ms; absent or unusable leaves the column NULL.
   Pre:  the item belongs to user-id (else returns nil, no write); rating ∈ 1..4.
   Post: the item's FSRS state advanced; exactly one kg_reviews row inserted;
         returns {:state :reps :lapses :scheduled-days :due-today?}."
  [user-id item-ref rating record scheduler enable-fuzzing]
  (let [[item-kind a b] item-ref
        card-id (when (= :card item-kind) a)
        ord (when (= :card item-kind) b)
        question-id (when (= :question item-kind) a)]
    (jdbc/with-transaction [tx ds]
      (when-let [row (if card-id
                       (load-card-memory! tx user-id card-id ord)
                       (load-question-memory! tx user-id question-id))]
        (let [state-before (long (:state row))
              reps-before  (long (:reps row))
              gap          (:gap_secs row)
              days-since   (when gap (long (Math/floor (/ (double gap) 86400.0))))
              elapsed      (max 0 (or days-since 0))
              card {:state state-before
                    :step (some-> (:step row) long)
                    :stability (some-> (:stability row) double)
                    :difficulty (some-> (:difficulty row) double)}
              res (fsrs/review-card scheduler card rating elapsed days-since)
              state-after (:state res)
              base-secs (:interval-seconds res)
              secs (if (and enable-fuzzing (= 2 state-after) (>= base-secs 86400))
                     (* 86400 (fsrs/fuzz-interval-days scheduler (quot base-secs 86400) (rand)))
                     base-secs)
              scheduled-days (quot secs 86400)
              lapse? (and (= 2 state-before) (= 1 rating))
              upd (if card-id
                    (jdbc/execute-one! tx
                      ["UPDATE card_schedules
                        SET state = ?, step = ?, stability = ?, difficulty = ?,
                            reps = reps + 1, lapses = lapses + ?, last_review = now(),
                            due = now() + make_interval(secs => ?)
                        WHERE flashcard_id = ? AND ord = ?
                        RETURNING (due::date <= CURRENT_DATE) AS due_today"
                       state-after (:step res) (:stability res) (:difficulty res)
                       (if lapse? 1 0) (double secs) card-id ord]
                      {:builder-fn rs/as-unqualified-maps})
                    (jdbc/execute-one! tx
                      ["UPDATE kg_questions
                        SET fsrs_state = ?, fsrs_step = ?, fsrs_stability = ?,
                            fsrs_difficulty = ?, fsrs_reps = fsrs_reps + 1,
                            fsrs_lapses = fsrs_lapses + ?, fsrs_last_review = now(),
                            fsrs_due = now() + make_interval(secs => ?)
                        WHERE id = ? AND user_id = ?
                        RETURNING (fsrs_due::date <= CURRENT_DATE) AS due_today"
                       state-after (:step res) (:stability res) (:difficulty res)
                       (if lapse? 1 0) (double secs) question-id user-id]
                      {:builder-fn rs/as-unqualified-maps}))]
          (jdbc/execute-one! tx
            (sql/format
              {:insert-into :kg_reviews
               :values [{:user_id user-id :rating rating
                         :question_id question-id
                         :flashcard_id card-id :ord ord
                         :grade_source (or (:grade-source record) "ai")
                         :verdict (:verdict record)
                         :state_before state-before :state_after state-after
                         :reps_before reps-before
                         :stability_after (:stability res) :difficulty_after (:difficulty res)
                         :elapsed_days elapsed :scheduled_days scheduled-days
                         :user_answer (some-> (:user-answer record) sanitize-utf8)
                         :explanation (some-> (:explanation record) sanitize-utf8)
                         :duration_ms (clamp-duration-ms (:duration-ms record))
                         :missed_fact_ids (int-array-value (:missed-fact-ids record))
                         ;; The wording as asked. A cloze card stores its RAW text —
                         ;; with `ord` alongside, history can re-derive the exact
                         ;; prompt, and a later edit cannot rewrite the record.
                         :question_text (if card-id
                                          {:select [[[:coalesce :question :cloze]]]
                                           :from [:flashcards] :where [:= :id card-id]}
                                          {:select [:question] :from [:kg_questions]
                                           :where [:= :id question-id]})
                         :reviewed_at [:now]}]}))
          {:state state-after
           :reps (inc reps-before)
           :lapses (+ (long (:lapses row)) (if lapse? 1 0))
           :scheduled-days scheduled-days
           :due-today? (boolean (:due_today upd))})))))

(defn fsrs-review-history-daily
  "Per-day Review tallies over the last `days` days (server-local).

   One query feeds the whole dashboard footer: the heatmap grid, the daily average,
   the days-learned share, and today's line — today is simply the row whose :day is
   CURRENT_DATE. Days with no reviews are ABSENT rather than zero-filled; the caller
   builds the grid and reads a missing day as 0.

   :timed is how many of the day's reviews carry a duration, so a caller can state
   its own denominator: duration_ms is NULL for every review taken before the column
   existed, and NULL is not zero.
   Post: [{:day date :reviews n :good n :again n :new n :timed n :duration_ms n|nil}]
         newest first."
  [user-id days]
  (jdbc/execute! ds
    ["SELECT reviewed_at::date AS day,
             COUNT(*)                                AS reviews,
             COUNT(*) FILTER (WHERE rating = 3)      AS good,
             COUNT(*) FILTER (WHERE rating = 1)      AS again,
             COUNT(*) FILTER (WHERE reps_before = 0) AS new,
             COUNT(duration_ms)                      AS timed,
             SUM(duration_ms)                        AS duration_ms
      FROM kg_reviews
      WHERE user_id = ? AND reviewed_at >= CURRENT_DATE - make_interval(days => ?::int)
      GROUP BY day ORDER BY day DESC"
     user-id days]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-review-streaks
  "Consecutive-day review streaks over ALL history, Anki-style.

   All-time on purpose, while the heatmap beside it is bounded to a rolling window:
   a 400-day streak reported as 365 because the grid stops there would be visibly
   wrong to the one person who has it.

   Deliberately NOT get-study-streak, which answers the same question over
   topic_repetitions — incremental-reading advances, a different activity that
   disagrees with this one on most days. Only the anchor rule is shared: today
   counts if reviewed, else yesterday, so a day not yet over does not break a
   streak.
   Post: EITHER {:current n :longest n} — both 0 when nothing was ever reviewed —
         OR {:error msg} with NEITHER count present.

         The failure branch carries no numbers on purpose. Returning zeros would
         report a 400-day streak as 0 and be indistinguishable from a true zero,
         which is the defect get-learning-queue's empty-collection catch produced
         (CLAUDE.md). A caller MUST test :error before reading a count — a
         (:current s 0) default would put the lie straight back."
  [user-id]
  (try
    (let [dayset (into #{}
                   (map #(java.time.LocalDate/parse (str (:d %))))
                   (jdbc/execute! ds
                     ["SELECT DISTINCT reviewed_at::date AS d
                       FROM kg_reviews WHERE user_id = ?" user-id]
                     {:builder-fn rs/as-unqualified-maps}))
          today (java.time.LocalDate/now)
          anchor (cond (contains? dayset today) today
                       (contains? dayset (.minusDays today 1)) (.minusDays today 1)
                       :else nil)
          run-back (fn [from] (loop [d from n 0]
                                (if (contains? dayset d)
                                  (recur (.minusDays ^java.time.LocalDate d 1) (inc n))
                                  n)))]
      {:current (if anchor (run-back anchor) 0)
       ;; Walk back from each run's END, never its start. run-back counts backwards,
       ;; so a day whose PREDECESSOR is absent returns 1 however long the run ahead
       ;; of it is — that selector capped every streak at 1. A day whose SUCCESSOR
       ;; is absent ends exactly one run, so each run is still measured once and the
       ;; whole sweep stays linear.
       :longest (transduce
                  (comp (remove #(contains? dayset (.plusDays ^java.time.LocalDate % 1)))
                    (map run-back))
                  max 0 dayset)})
    (catch Exception e
      (tel/error! {:id ::kg-review-streaks} e)
      {:error "Streaks are unavailable right now."})))

(defn review-due-count
  "How many items the next Review sitting would draw — the dashboard's due tile.
   Runs the same draw under the same caps, so the tile can never disagree with the
   queue Start opens. Counts questions and card items together.
   Post: a non-negative count."
  [user-id new-limit review-limit]
  (count (draw-review-queue user-id new-limit review-limit)))

(defn get-card-item
  "One card quiz item, as the review turn needs it.

   `prompt` is what the learner sees: a basic card's question, or the cloze text with
   this ord hidden and every other deletion revealed. `answer` is what it hides. Both
   are stored HTML — the caller renders them through freememo.math.
   Pre:  ord is live for this card (the draw guarantees it).
   Post: {:card-id :ord :kind :prompt :answer} or nil when not found, not owned, or
         the ord no longer exists in the card text."
  [user-id card-id ord]
  (when-let [c (jdbc/execute-one! ds
                 ["SELECT f.id, f.kind, f.question, f.answer, f.cloze
                   FROM flashcards f
                   JOIN topics root ON root.id = f.root_topic_id
                   WHERE f.id = ? AND root.user_id = ?"
                  card-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (when (live-ord? (:kind c) (:cloze c) ord)
      {:card-id (:id c) :ord ord :kind (:kind c)
       :prompt (if (= "basic" (:kind c))
                 (:question c)
                 (cloze/mask-ord (:cloze c) ord))
       :answer (if (= "basic" (:kind c))
                 (:answer c)
                 (cloze/answer-for-ord (:cloze c) ord))})))

(defn kg-question-bank-counts
  "Bank-wide tallies for the quiz dashboard tiles. Counts live (approved) questions
   only — a rejected or retired question is not part of the bank.
   Post: {:live n :flagged n :suspended n}; :flagged and :suspended overlap, since
         the two flags are independent."
  [user-id]
  (let [r (jdbc/execute-one! ds
            ["SELECT COUNT(*) AS live,
                     COUNT(*) FILTER (WHERE flagged)   AS flagged,
                     COUNT(*) FILTER (WHERE suspended) AS suspended
              FROM kg_questions
              WHERE user_id = ? AND status = 'approved'"
             user-id]
            {:builder-fn rs/as-unqualified-maps})]
    {:live (long (:live r)) :flagged (long (:flagged r))
     :suspended (long (:suspended r))}))

(defn- review-row-prompt
  "The wording to show for one kg_reviews row.

   A question row's snapshot is already display text. A CARD row's snapshot is the
   card's raw text, so a cloze snapshot still carries its {{cN::…}} markers and must
   be masked back to the prompt that ord was asked as. ord 0 means a basic card —
   cloze numbering is a gap-free 1..max, so 0 can only be the basic ord.
   Post: a string, or nil when the row predates the snapshot column."
  [{:keys [flashcard_id ord question_text]}]
  (cond
    (nil? question_text) nil
    (nil? flashcard_id) question_text
    (zero? (long ord)) question_text
    :else (cloze/mask-ord question_text (long ord))))

(defn fsrs-review-log
  "One row per graded review, newest first — the Reviews tab's list. Covers both
   item types: a card row carries :flashcard_id + :ord instead of :question_id.

   :prompt is the wording as asked; the LIST renders that snapshot so a later edit
   cannot rewrite the record, while the detail reads the live item by id. A nil
   :prompt marks a row written before the snapshot column existed — \"not
   recorded\", which the UI must not present as \"unchanged\".

   `day` (ISO date string) scopes the log to one calendar day — the review-day
   detail. One fn rather than two: the row shape and the review-row-prompt mapping
   are identical, and a parallel query would be a duplicate free to drift.
   `limit` still applies, but a day cannot plausibly exceed it.
   Post: ≤ `limit` rows {:id :question_id :flashcard_id :ord :prompt :question_text
         :verdict :rating :grade_source :duration_ms :reviewed_at}, newest first;
         scoped to `day` when given."
  ([user-id limit] (fsrs-review-log user-id limit nil))
  ([user-id limit day]
   (into []
     (map (fn [r] (assoc r :prompt (review-row-prompt r))))
     (jdbc/execute! ds
       (into [(str "SELECT r.id, r.question_id, r.flashcard_id, r.ord, r.question_text,
                           r.verdict, r.rating, r.grade_source, r.duration_ms,
                           to_char(r.reviewed_at, 'YYYY-MM-DD HH24:MI') AS reviewed_at
                    FROM kg_reviews r
                    WHERE r.user_id = ?"
               ;; Half-open range, NEVER reviewed_at::date = ?::date. The cast form
               ;; is not sargable, so idx_kg_reviews_user_time can serve only the
               ;; user_id half and the LIMIT cannot stop at the day boundary —
               ;; opening a two-year-old day walks every newer row of that user's
               ;; history. reviewed_at is TIMESTAMPTZ and the bounds are dates, so
               ;; both sides resolve through the same session TimeZone the cast used
               ;; and the two forms select exactly the same rows.
               (when day " AND r.reviewed_at >= ?::date AND r.reviewed_at < ?::date + 1")
               " ORDER BY r.reviewed_at DESC, r.id DESC
                 LIMIT ?")]
         ;; Parameter order follows the ? order above: the day appears TWICE, once
         ;; per bound, and sits between the two constants rather than at the end.
         (if day [user-id day day limit] [user-id limit]))
       {:builder-fn rs/as-unqualified-maps}))))

(defn kg-review-detail
  "One review as the answer view needs it: what was answered, how it was graded, the
   wording as asked, and the item's wording NOW (by id, so an edit is visible here
   even though the list keeps the snapshot).

   The joins are LEFT joins because exactly one of them matches: a question row has
   no flashcard and a card row has no question. A card row's live wording comes from
   get-card-item, so an edited deletion shows here the same way an edited question
   does; a card whose ord was edited away has no live wording at all.
   Post: {:review {…} :missed-facts [labeled rows]} or nil when not owned/found;
         :question_text may be nil (pre-column row); :live_question is nil only for a
         card item whose ord is gone."
  [user-id review-id]
  (when-let [r (jdbc/execute-one! ds
                 ["SELECT r.id, r.question_id, r.flashcard_id, r.ord, r.verdict,
                          r.rating, r.grade_source, r.user_answer,
                          r.explanation, r.question_text, r.missed_fact_ids,
                          to_char(r.reviewed_at, 'YYYY-MM-DD HH24:MI') AS reviewed_at,
                          q.question AS live_question, q.reference_answer,
                          q.flagged, q.suspended
                   FROM kg_reviews r
                   LEFT JOIN kg_questions q ON q.id = r.question_id
                   WHERE r.id = ? AND r.user_id = ?"
                  review-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (let [missed (or (sql-int-array->vec (:missed_fact_ids r)) [])
          card (when-let [cid (:flashcard_id r)]
                 (get-card-item user-id cid (long (:ord r))))]
      {:review (assoc r :missed_fact_ids missed
                 :prompt (review-row-prompt r)
                 :live_question (or (:live_question r) (:prompt card))
                 :reference_answer (or (:reference_answer r) (:answer card))
                 :flagged (boolean (:flagged r))
                 :suspended (boolean (:suspended r)))
       :missed-facts (kg-facts-labeled user-id missed)})))

(defn fsrs-question-states
  "Per-question FSRS snapshot for the history browser — reviewed questions,
   most-lapsed first. Post: ≤ `limit` rows with schedule state + due date."
  [user-id limit]
  (jdbc/execute! ds
    ["SELECT id, question, fsrs_state, fsrs_reps, fsrs_lapses,
             fsrs_stability, fsrs_due
      FROM kg_questions
      WHERE user_id = ? AND status = 'approved' AND fsrs_reps > 0
      ORDER BY fsrs_lapses DESC, fsrs_reps DESC
      LIMIT ?"
     user-id limit]
    {:builder-fn rs/as-unqualified-maps}))

;; ── Per-item lifetime stats ────────────────────────────────────────────────
;; One quiz item's whole scheduling record: what it is, where it came from, its
;; current memory state, and every review of it. The two arms return the SAME map,
;; because a question and a card deletion differ in how they are asked, not in how
;; they are scheduled — freememo.quiz-item-stats renders one panel for both.
;;
;; Every instant is returned TWICE: a to_char label for display and an epoch for
;; arithmetic. The forgetting curve needs real deltas and freememo.fsrs is .cljc, so
;; the curve is computed on the client from the epochs rather than as a point series
;; built here and shipped.

(def ^:private item-review-sql
  ;; `id` is here to be a stable diff key: at_epoch is truncated to the second, and
  ;; three learning-step reviews of one item inside one second is ordinary.
  "SELECT id,
          to_char(reviewed_at, 'YYYY-MM-DD HH24:MI') AS reviewed_at,
          EXTRACT(EPOCH FROM reviewed_at)::bigint     AS at_epoch,
          rating, state_before, elapsed_days, scheduled_days,
          stability_after, difficulty_after, duration_ms, grade_source
   FROM kg_reviews
   WHERE user_id = ? AND ")

(defn- item-stats-shape
  "Assemble the arms' common return from a schedule row and its reviews.
   Pre:  `sched` carries :state :stability :difficulty :reps :lapses :due
         :due_epoch :last_review :days_since, all nilable — an item never reviewed
         has no schedule row at all and passes nil.
   Post: the map quiz-item-stats documents."
  [base sched reviews]
  (assoc base
    :schedule sched
    :reviews (vec reviews)))

(defn- question-item-stats [user-id question-id]
  (when-let [q (jdbc/execute-one! ds
                 ["SELECT q.id, q.kind, q.question,
                          to_char(q.created_at, 'YYYY-MM-DD') AS added,
                          q.fsrs_state AS state, q.fsrs_stability AS stability,
                          q.fsrs_difficulty AS difficulty, q.fsrs_reps AS reps,
                          q.fsrs_lapses AS lapses,
                          to_char(q.fsrs_due, 'YYYY-MM-DD') AS due,
                          EXTRACT(EPOCH FROM q.fsrs_due)::bigint AS due_epoch,
                          -- ::float8, not the bare division: EXTRACT yields
                          -- numeric, which reaches the client as a BigDecimal that
                          -- fsrs/retrievability cannot multiply.
                          (EXTRACT(EPOCH FROM (now() - q.fsrs_last_review))/86400.0)::float8
                            AS days_since
                   FROM kg_questions q
                   WHERE q.id = ? AND q.user_id = ?"
                  question-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (item-stats-shape
      {:item-kind :question
       :item-label (str "Question " (:id q))
       :prompt (:question q)
       :card-kind (:kind q)
       :added (:added q)
       ;; A question's document is where its facts were distilled from, so it can
       ;; legitimately be several — the inverse walk of the question generator.
       :sources (mapv :title
                  (jdbc/execute! ds
                    ["SELECT DISTINCT t.title
                      FROM kg_question_facts qf
                      JOIN kg_facts f ON f.id = qf.fact_id
                      JOIN topics t ON t.id = f.graph_topic_id
                      WHERE qf.question_id = ?
                      ORDER BY t.title" question-id]
                    {:builder-fn rs/as-unqualified-maps}))}
      (select-keys q [:state :stability :difficulty :reps :lapses :due :due_epoch
                      :days_since])
      (jdbc/execute! ds
        [(str item-review-sql "question_id = ? ORDER BY reviewed_at ASC")
         user-id question-id]
        {:builder-fn rs/as-unqualified-maps}))))

(defn- card-item-stats [user-id card-id ord]
  (when-let [c (jdbc/execute-one! ds
                 ["SELECT f.id, f.kind,
                          to_char(f.created_at, 'YYYY-MM-DD') AS added,
                          root.title AS doc_title
                   FROM flashcards f
                   JOIN topics root ON root.id = f.root_topic_id
                   WHERE f.id = ? AND root.user_id = ?"
                  card-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    (item-stats-shape
      {:item-kind :card
       :item-label (str "Card " card-id (when (pos? (long ord)) (str " · c" ord)))
       ;; nil when the deletion has been edited away since it was last reviewed —
       ;; the history stays, the prompt is gone.
       :prompt (:prompt (get-card-item user-id card-id ord))
       :card-kind (:kind c)
       :added (:added c)
       :sources [(:doc_title c)]}
      ;; ABSENT means never introduced — card_schedules rows are created lazily on
      ;; first review (load-card-memory!), so nil here is normal, not an error.
      (jdbc/execute-one! ds
        ["SELECT state, stability, difficulty, reps, lapses,
                 to_char(due, 'YYYY-MM-DD') AS due,
                 EXTRACT(EPOCH FROM due)::bigint AS due_epoch,
                 -- ::float8 — see question-item-stats.
                 (EXTRACT(EPOCH FROM (now() - last_review))/86400.0)::float8 AS days_since
          FROM card_schedules WHERE flashcard_id = ? AND ord = ?"
         card-id ord]
        {:builder-fn rs/as-unqualified-maps})
      (jdbc/execute! ds
        [(str item-review-sql "flashcard_id = ? AND ord = ? ORDER BY reviewed_at ASC")
         user-id card-id ord]
        {:builder-fn rs/as-unqualified-maps}))))

(defn card-quiz-ords
  "The live quiz-item ords of one card, in ascending order.

   A card row in the Library is ONE flashcard but can be several quiz items, and
   only this layer knows which: the kind restriction and the live-ord rule both
   live here. A caller therefore never parses cloze text to decide whether to offer
   a picker — it asks.
   Post: [] when the card is not this user's, is not basic/cloze (occlusion, score
         and overlapping yield no quiz items — card-quiz-where), or has no live
         deletion left; [0] for a basic card; the live cloze ords otherwise."
  [user-id card-id]
  (if-let [c (jdbc/execute-one! ds
               [(str "SELECT f.kind, f.cloze FROM flashcards f
                      JOIN topics root ON root.id = f.root_topic_id
                      WHERE f.id = ? AND root.user_id = ? AND " (card-quiz-where "f"))
                card-id user-id]
               {:builder-fn rs/as-unqualified-maps})]
    (if (= "basic" (:kind c)) [0] (vec (sort (cloze/ords (:cloze c)))))
    []))

(defn quiz-item-stats
  "One quiz item's lifetime record — the stats panel's whole payload.

   `item-ref` is [:question id] or [:card flashcard-id ord], the same shape the
   review queue and the review log speak. Ownership is enforced in SQL on both arms
   (kg_questions.user_id; flashcards → topics.root.user_id), so an id that is not
   this user's returns nil and reads nothing further.

   Pre:  a card ref names a basic or cloze card — those are the only kinds that
         yield quiz items (card-quiz-where), so the caller must not offer this for
         an occlusion, score or overlapping card.
   Post: nil when not found or not owned, else
         {:item-kind :item-label :prompt :card-kind :added :sources [str]
          :schedule {…}|nil :reviews [{…}]}.
         :schedule nil = never introduced. :reviews is OLDEST FIRST, because the
         forgetting curve reads it as a time series; the panel's table reverses it."
  [user-id item-ref]
  (let [[item-kind a b] item-ref]
    (case item-kind
      :question (question-item-stats user-id a)
      :card (card-item-stats user-id a (long b))
      nil)))

(defn kg-fact-alternates
  "Approved facts that share a predicate+object or subject+predicate with any
   of `fact-ids` (excluding them) — the grader's 'also true' context, so an
   answer confirmed by a sibling fact is never marked wrong.
   Post: ≤ 12 labeled rows, same shape as entity-fact-neighborhood."
  [user-id fact-ids]
  (when (seq fact-ids)
    (jdbc/execute! ds
      (sql/format
        {:select [[:f.id :id] [:s.label :subject_label] [:p.label :predicate_label]
                  [:o.label :object_label] [:f.object_literal :object_literal]]
         :from [[:kg_facts :f]]
         :join [[:kg_entities :s] [:= :s.id :f.subject_entity_id]
                [:kg_predicates :p] [:= :p.id :f.predicate_id]]
         :left-join [[:kg_entities :o] [:= :o.id :f.object_entity_id]]
         :where [:and [:= :f.user_id user-id] [:= :f.status "approved"]
                 [:not-in :f.id (vec fact-ids)]
                 [:exists {:select [1] :from [[:kg_facts :lf]]
                           :where [:and [:in :lf.id (vec fact-ids)]
                                   [:= :lf.predicate_id :f.predicate_id]
                                   [:or
                                    [:and [:!= :lf.object_entity_id nil]
                                     [:= :lf.object_entity_id :f.object_entity_id]]
                                    [:= :lf.subject_entity_id :f.subject_entity_id]]]}]]
         :limit 12})
      {:builder-fn rs/as-unqualified-maps})))

;; ── Exam grading + session history ─────────────────────────────────────────

(defn kg-ungraded-answers
  "Saved-but-ungraded answers of a session, in sitting order — the exam
   grader's work list. Ownership enforced via the session join."
  [user-id session-id]
  (jdbc/execute! ds
    ["SELECT a.question_id, a.position, a.user_answer
      FROM kg_answers a JOIN kg_sessions s ON s.id = a.session_id
      WHERE s.id = ? AND s.user_id = ? AND a.verdict IS NULL
      ORDER BY a.position"
     session-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(def quiz-history-limit
  "Rows one history payload carries. The list is windowed, so this bounds the wire
   payload and nothing else — deliberately far above any plausible history, unlike
   the 100-sitting cap it replaces, which a daily user hit inside four months."
  500)

(defn list-quiz-history
  "The Quiz history list: finished quiz/exam sittings AND Review days, interleaved.

   A Review sitting is persisted nowhere — the FSRS flow writes no kg_sessions row
   — so its unit here is the CALENDAR DAY of kg_reviews. That is a real boundary
   rather than a guessed one, and it is how Anki reports too. A day sorts by its
   LAST review, so a day and a sitting on the same date interleave by clock rather
   than the day always leading.

   The two shapes share a row and each keeps its own column names, NULL where the
   column does not apply — `:kind` is what a reader dispatches on. The score itself
   is NOT computed here: quiz-dashboard/session-score and /retention own it, and
   duplicating either in SQL would give the same number two definitions.
   Post: ≤ quiz-history-limit rows, newest first,
         {:kind \"quiz\"|\"exam\"|\"review\" :key str :at_label str
          :total :correct :partial :incorrect   — sittings only, else NULL
          :reviewed :again}                     — review days only, else NULL.
         :key is the session id as a string, or the ISO date."
  [user-id]
  (jdbc/execute! ds
    ["WITH sittings AS (
        SELECT s.kind, s.id::text AS key, s.started_at AS at,
               to_char(s.started_at, 'YYYY-MM-DD HH24:MI') AS at_label,
               cardinality(s.question_ids) AS total,
               COUNT(a.verdict) FILTER (WHERE a.verdict = 'correct')   AS correct,
               COUNT(a.verdict) FILTER (WHERE a.verdict = 'partial')   AS partial,
               COUNT(a.verdict) FILTER (WHERE a.verdict = 'incorrect') AS incorrect
        FROM kg_sessions s
        LEFT JOIN kg_answers a ON a.session_id = s.id
        WHERE s.user_id = ? AND s.finished_at IS NOT NULL
        GROUP BY s.id
      ), days AS (
        SELECT MAX(r.reviewed_at) AS at,
               to_char(r.reviewed_at::date, 'YYYY-MM-DD') AS key,
               COUNT(*)                             AS reviewed,
               COUNT(*) FILTER (WHERE r.rating = 1) AS again
        FROM kg_reviews r
        WHERE r.user_id = ?
        GROUP BY r.reviewed_at::date
      )
      SELECT kind, key, at, at_label, total, correct, partial, incorrect,
             NULL::bigint AS reviewed, NULL::bigint AS again
      FROM sittings
      UNION ALL
      SELECT 'review', key, at, key, NULL::int, NULL::bigint, NULL::bigint,
             NULL::bigint, reviewed, again
      FROM days
      ORDER BY at DESC
      LIMIT ?"
     user-id user-id quiz-history-limit]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-review-day-summary
  "One Review day's totals — the header above that day's review list.

   :duration_ms sums only the rows that carry one, and :timed says how many did, so
   the caller can state its own denominator. NULL in that column means NOT RECORDED
   (every row predating it, and any turn whose measurement failed), which is not
   zero and must not be averaged as zero.
   Pre:  `day` is an ISO date string.
   Post: {:reviewed n :new n :again n :hard n :good n :easy n :timed n
          :duration_ms n|nil} — :reviewed 0 for a day with no reviews."
  [user-id day]
  (jdbc/execute-one! ds
    ;; Half-open range, for the reason fsrs-review-log states: reviewed_at::date =
    ;; ?::date would aggregate one day by scanning the user's entire history.
    ["SELECT COUNT(*)                                  AS reviewed,
             COUNT(*) FILTER (WHERE reps_before = 0)   AS new,
             COUNT(*) FILTER (WHERE rating = 1)        AS again,
             COUNT(*) FILTER (WHERE rating = 2)        AS hard,
             COUNT(*) FILTER (WHERE rating = 3)        AS good,
             COUNT(*) FILTER (WHERE rating = 4)        AS easy,
             COUNT(duration_ms)                        AS timed,
             SUM(duration_ms)                          AS duration_ms
      FROM kg_reviews
      WHERE user_id = ?
        AND reviewed_at >= ?::date AND reviewed_at < ?::date + 1"
     user-id day day]
    {:builder-fn rs/as-unqualified-maps}))

(defn kg-session-detail
  "One finished (or being-reviewed) session with its per-question record.

   Both wordings are returned: :question_text is the snapshot as asked (nil on rows
   written before that column existed) and :question is the question NOW. The list
   renders the snapshot, the detail renders live, and their difference is what raises
   the edited-since marker.
   Post: {:session {:id :kind :total :started} :answers [{:position :question
   :question_text :reference_answer :user_answer :verdict :explanation}]} or nil
   when not owned/found. Verdict nil = saved but ungraded (grading failure or skip);
   a question skipped via Suspend & Skip has no row at all, so it counts toward
   :total and shows as unanswered."
  [user-id session-id]
  (when-let [s (jdbc/execute-one! ds
                 ["SELECT id, kind, cardinality(question_ids) AS total,
                     to_char(started_at, 'YYYY-MM-DD HH24:MI') AS started
                   FROM kg_sessions WHERE id = ? AND user_id = ?"
                  session-id user-id]
                 {:builder-fn rs/as-unqualified-maps})]
    {:session s
     :answers (jdbc/execute! ds
                ["SELECT a.position, a.user_answer, a.verdict, a.explanation,
                     a.question_text, a.question_id,
                     q.question, q.reference_answer
                  FROM kg_answers a JOIN kg_questions q ON q.id = a.question_id
                  WHERE a.session_id = ? ORDER BY a.position"
                 session-id]
                {:builder-fn rs/as-unqualified-maps})}))

;; ── NeoReader highlights ───────────────────────────────────────────

(defn replace-neoreader-highlights!
  "Record one export's highlights for one document, replacing the previous set.

   Pre : `user-id` owns `root-topic-id`. `rows` is a seq of maps carrying
         :chapter_topic_id :chapter_label :highlighted_at :quote :note :ordinal
         :outcome. A caller MUST NOT pass :quote_hash — the column is generated.
   Post: the table holds exactly these highlights for `root-topic-id`, and
         nothing for any other document changes.
   Post: a highlight already recorded keeps its row id. A re-sync of an unchanged
         export therefore changes no row identity, and a highlight deleted on the
         device removes exactly one row.
   Inv : one transaction. A failure between the upsert and the delete would leave
         rows the export no longer holds, which the next sync would report as
         highlights the reader does not have.
   Inv : rows sharing an identity collapse to the last one. Postgres refuses to
         let one statement update the same row twice, so a duplicate pair in the
         export would fail the whole insert — the dedupe belongs here, next to the
         constraint that needs it."
  [user-id root-topic-id rows]
  (let [stamp (java.sql.Timestamp. (System/currentTimeMillis))
        deduped (->> rows
                  (reduce (fn [m r] (assoc m [(:highlighted_at r) (:quote r)] r)) {})
                  vals
                  (mapv #(assoc % :user_id user-id
                            :root_topic_id root-topic-id
                            :synced_at stamp)))]
    (jdbc/with-transaction [tx ds]
      (doseq [batch (param-bounded-batches deduped)]
        (jdbc/execute! tx
          (sql/format {:insert-into :neoreader_highlights
                       :values (vec batch)
                       :on-conflict [:root_topic_id :highlighted_at :quote_hash]
                       :do-update-set [:chapter_topic_id :chapter_label :note
                                       :ordinal :outcome :synced_at]})))
      ;; Whatever this sync did not touch is a highlight the device no longer
      ;; has. The stamp is the marker: it is one value for the whole run.
      (jdbc/execute! tx
        (sql/format {:delete-from :neoreader_highlights
                     :where [:and [:= :root_topic_id root-topic-id]
                             [:<> :synced_at stamp]]})))))

;; ── Study Scheduler (plans/study-scheduler-spec.md §3) ─────────────
;;
;; Two concerns, kept apart. `study_schedules` is the user's intent and is
;; durable. `anki_card_states` is a cache of FSRS state replayed out of Anki
;; and is derivable at any time (§3.5). NOTHING here writes `card_schedules`
;; or `topics.next_review_at` — §1.4 forbids this feature from touching the
;; Learn queue, and that is the boundary those two columns mark.

(defn- schedule-row->map
  "One `study_schedules` row as plain data. `excluded_pages` arrives as a raw
   java.sql.Array, which no caller past this namespace should have to know."
  [r]
  (-> r
    (update :study_schedules/excluded_pages #(vec (or (sql-int-array->vec %) [])))
    (assoc :study_schedules/target_date (some-> (:study_schedules/target_date r) str))))

(defn- schedules-where
  "Schedules the user owns, filtered by `archived-clause`, soonest target first.

   Shared by the two public readers so their only difference is the one
   predicate that names them. Both restrict to documents that still exist and
   are not staged for deletion — a schedule over a deleted document has nothing
   to forecast either way."
  [user-id archived-clause]
  (mapv schedule-row->map
    (jdbc/execute! ds
      (sql/format {:select [:s/* [:t/title :doc_title] [:t/kind :doc_kind]]
                   :from [[:study_schedules :s]]
                   :join [[:topics :t] [:= :s/root_topic_id :t/id]]
                   :where [:and [:= :s/user_id user-id]
                           archived-clause
                           [:is :t/staged_delete_id nil]]
                   :order-by [[:s/target_date :asc] [:s/id :asc]]}))))

(defn list-study-schedules
  "Every ACTIVE schedule the user owns, soonest target first.

   Post: archived rows are absent. §1.3 sums the daily load across exactly this
         set, so a retired schedule must not add load the user will not do."
  [user-id]
  (schedules-where user-id [:is :s/archived_at nil]))

(defn list-archived-study-schedules
  "Every ARCHIVED schedule the user owns.

   The counterpart `archive-study-schedule!` always implied and nothing
   provided. Without it `archived_at` was a one-way door: the row survived, the
   daily load stopped counting it, and no query anywhere could read it back —
   so 'archive' meant 'delete, but leave the row behind'."
  [user-id]
  (schedules-where user-id [:not= :s/archived_at nil]))

(defn active-study-schedules-for-root
  "EVERY active schedule over `root-topic-id`, soonest target first.

   Pre : ownership is checked HERE, in the same clause `schedules-where`
         applies to every other read of this table.
   Post: a vector, empty when none covers the root. Nothing in the schema
         forbids two schedules over one document — a book split into a finished
         first half and a live second half is the ordinary case — so a reader
         that took `first` of this list picked by target date, which is not the
         question a reading view asks. `schedule-forecast/pick-for-page` ranks
         them by the page in view; this query does not choose.
   Post: the order is `schedules-where`'s (target_date, id), so a caller that
         does want the most urgent one still reads it off the front and agrees
         with the row the Schedule tab lists first."
  [user-id root-topic-id]
  (schedules-where user-id [:and [:is :s/archived_at nil]
                            [:= :s/root_topic_id root-topic-id]]))

(defn get-study-schedule
  "One schedule, or nil when it is absent, another user's, or over a document
   staged for deletion.

   Pre : ownership is checked HERE, not by the caller. Every read of a schedule
         crosses a reactive boundary where the id came from a URL.
   Post: `:topics/doc_title` is the document's title. next.jdbc qualifies an
         aliased column by the table it was selected FROM, not by the alias, so
         a read of a bare `:doc_title` returns nil — which renders as
         'Untitled' with nothing in any log. `schedule/display-rows` states the
         same trap for the same reason.
   Post: nil for a staged document — the SAME condition `schedules-where`
         applies. The two readers must agree: both lists hide such a schedule,
         and a detail pane that still rendered one offered its document link a
         topic the user has already deleted. `stage-topic-for-deletion!` sets
         `staged_delete_id` across the whole subtree, root included, so the
         root's own column is the test.
   Post: the caller reads `:not-found` from that nil (`schedule/forecast`),
         which is the branch the detail pane already renders."
  [user-id schedule-id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:s/* [:t/title :doc_title]]
                         :from [[:study_schedules :s]]
                         :join [[:topics :t] [:= :s/root_topic_id :t/id]]
                         :where [:and [:= :s/id schedule-id]
                                 [:= :s/user_id user-id]
                                 [:is :t/staged_delete_id nil]]}))
    schedule-row->map))

(defn create-study-schedule!
  "Insert a schedule and return its row.

   Pre : `user-id` owns `root-topic-id`. `target-date` is an ISO date string.
         `theta` is nil (meaning Anki's own `desiredRetention`, θ1) or a
         probability strictly inside (0,1) — the CHECK enforces the range and a
         violation is the caller's bug.
   Post: exactly one new active schedule. `start_known` freezes the known-page
         count at creation, because §6.2's straight line runs from where the
         user WAS, not from zero.
   Post: `covered_baseline_at` is this statement's `now()`, which is also
         `created_at`'s default — the two are the same transaction timestamp.
         A new schedule therefore has ONE origin, and only a row that predates
         the column gets the migration's later one. Leaving it null here would
         hand a fresh schedule to the boot backfill, which would re-baseline it
         at the next restart."
  [user-id {:keys [root-topic-id target-date first-page last-page daily-capacity
                   theta excluded-pages start-known]}]
  (some-> (jdbc/execute-one! ds
            (sql/format {:insert-into :study_schedules
                         :values [{:user_id user-id
                                   :root_topic_id root-topic-id
                                   :target_date [:cast target-date :date]
                                   :first_page first-page
                                   :last_page last-page
                                   :daily_capacity (or daily-capacity 60)
                                   :theta theta
                                   :excluded_pages (int-array-value excluded-pages)
                                   :start_known (or start-known 0)
                                   :covered_baseline_at [:now]}]
                         :returning [:*]})
            {:return-keys true})
    schedule-row->map))

(defn update-study-schedule!
  "Apply the levers of §6.6 to one schedule the user owns.

   Pre : `fields` holds only lever columns — target_date (I1), daily_capacity
         (I2), theta (I4), excluded_pages (I5) — plus the page range. Any other
         key is ignored rather than written, so a caller cannot reach
         `user_id`, `root_topic_id` or `start_known` through this path.
   Post: the updated row, or nil when the id is not the user's."
  [user-id schedule-id fields]
  (let [allowed (select-keys fields [:target_date :first_page :last_page
                                     :daily_capacity :theta :excluded_pages])
        set-map (cond-> allowed
                  (contains? allowed :target_date)
                  (update :target_date #(when % [:cast % :date]))
                  (contains? allowed :excluded_pages)
                  (update :excluded_pages int-array-value))]
    (when (seq set-map)
      (some-> (jdbc/execute-one! ds
                (sql/format {:update :study_schedules
                             :set set-map
                             :where [:and [:= :id schedule-id] [:= :user_id user-id]]
                             :returning [:*]})
                {:return-keys true})
        schedule-row->map))))

(defn set-schedule-baselines!
  "Freeze a schedule's baseline page counts (§6.2).

   Both baselines in ONE write, because both are measured from the same
   `forecast` call and a schedule with one of them set is a schedule whose
   pace figure is wrong. Two setters would make that partial state reachable.

   Separate from `update-study-schedule!` on purpose: that fn is the LEVER
   path and must not reach these columns, because moving a baseline would let
   a lever rewrite history and make the pace figure meaningless. This is
   called once, right after creation, by the only caller that knows the
   measurement.

   `covered_baseline_at` is NOT written here. `create-study-schedule!` stamps
   it in the INSERT so it agrees with `created_at`, and a later write would
   move the origin away from the moment the counts describe.

   Pre : `start-known` and `start-covered` come from the same `current-progress`
         map. Mixing two measurements is what makes a brand-new schedule open
         several pages behind.
   Post: rows changed, 0 when the id is not the user's."
  [user-id schedule-id start-known start-covered]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      (sql/format {:update :study_schedules
                   :set {:start_known start-known
                         :start_covered start-covered}
                   :where [:and [:= :id schedule-id] [:= :user_id user-id]]}))
    0))

(defn restore-study-schedule!
  "Return an archived schedule to the active set. Returns rows changed.

   The inverse of `archive-study-schedule!`, and the reason that fn archives
   rather than deletes. `start_known` is untouched, so §6.2's pace line still
   runs from where the user was when they made the plan — a restore resumes a
   schedule, it does not start a new one."
  [user-id schedule-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      (sql/format {:update :study_schedules
                   :set {:archived_at nil}
                   :where [:and [:= :id schedule-id] [:= :user_id user-id]
                           [:not= :archived_at nil]]}))
    0))

(defn archive-study-schedule!
  "Retire a schedule without deleting it. Returns the number of rows changed.
   Archiving rather than deleting: §1.3's combined daily load must stop
   counting it, and the user may still want to see that the schedule existed."
  [user-id schedule-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      (sql/format {:update :study_schedules
                   :set {:archived_at [:raw "now()"]}
                   :where [:and [:= :id schedule-id] [:= :user_id user-id]
                           [:is :archived_at nil]]}))
    0))

;; --- The page and card set a schedule forecasts ----------------------------

(def ^:private schedulable-kinds
  "Root topic kinds a schedule can be built over.

   `pdf` alone. The whole feature counts PAGES — the range, the progress
   denominator, the daily plan — and a page is a `kind = 'page'` child topic.
   Measured against a real collection: PDFs carry every page row in the
   database, and `epub`, `web`, `markdown`, `basic`, `audio`, `supermemo` and
   `video-playlist` roots carry none. Offering them would list a thousand
   documents that can never produce a plan.

   A kind that grows page children later belongs here, and nowhere else.

   `poem` is not one (poem-reader-spec row 2.4.2). A poem is a single topic and
   carries no `page` children, so a schedule over it would have a denominator of
   zero. Spec row 6.7 makes it ONE Learn-queue item instead."
  #{"pdf"})

(defn list-schedulable-documents
  "Root documents a schedule can be built over, newest first.

   Pre : `user-id` is a user id.
   Post: {:id :title :kind :pages} per document, `:pages` > 0 always. A
         document with no pages is absent rather than listed with a zero — it
         cannot produce a range, so offering it is offering a dead end.
   Post: one query, not a page count per document. The N+1 shape it replaces
         issued one `list-pages` per root, which on a library of a thousand
         markdown notes was a thousand queries to build a list of eight."
  [user-id]
  (mapv (fn [r] {:id (:topics/id r)
                 :title (or (:topics/title r) "Untitled")
                 :kind (:topics/kind r)
                 :pages (:pages r)})
    (jdbc/execute! ds
      (sql/format {:select [:t/id :t/title :t/kind [[:count :p/id] :pages]]
                   :from [[:topics :t]]
                   :join [[:topics :p] [:and [:= :p/parent_id :t/id]
                                        [:= :p/kind "page"]
                                        [:is :p/staged_delete_id nil]]]
                   :where [:and [:= :t/user_id user-id]
                           [:is :t/parent_id nil]
                           [:is :t/staged_delete_id nil]
                           [:in :t/kind (vec schedulable-kinds)]]
                   :group-by [:t/id :t/title :t/kind]
                   :order-by [[:t/created_at :desc]]}))))

(defn get-schedule-pages
  "The pages of one document inside a page range, in page order.

   Pre : `user-id` owns `root-topic-id`. `first-page` and `last-page` are page
         NUMBERS, either may be nil for an open end.
   Post: {:page-id :page-number :done? :done-at} per page. Every page of the
         range is present, including pages with no cards and pages never opened
         — §5.5 requires an untouched book to read 0 of N rather than 0 of 0,
         and that only holds if the denominator comes from here.
   Post: `:done-at` is the raw instant, NOT a same-day flag. Deciding which day
         it falls on needs the day boundary, and `freememo.schedule` owns that
         — one definition, already used by `start-of-today-ms` and `days-until`.
         Comparing here would put a second one in SQL."
  [user-id root-topic-id first-page last-page]
  (mapv (fn [r] {:page-id (:topics/id r)
                 :page-number (:topics/page_number r)
                 :done? (= "done" (:topics/status r))
                 :done-at (:topics/done_at r)})
    (jdbc/execute! ds
      (sql/format {:select [:t/id :t/page_number :t/status :t/done_at]
                   :from [[:topics :t]]
                   :where (cond-> [:and
                                   [:= :t/parent_id root-topic-id]
                                   [:= :t/kind "page"]
                                   [:= :t/user_id user-id]
                                   [:is :t/staged_delete_id nil]]
                            first-page (conj [:>= :t/page_number first-page])
                            last-page (conj [:<= :t/page_number last-page]))
                   :order-by [[:t/page_number :asc]]}))))

(defn get-schedule-cards
  "Every pushed flashcard of one document's pages, with the page it belongs to.

   Pre : `user-id` owns `root-topic-id`.
   Post: {:flashcard-id :page-id :anki-note-id} per card, for cards with an
         `anki_note_id` only. A card never pushed to Anki has no revlog and no
         memory state, so it cannot contribute to a page's mean R (§5.1) — it
         is absent here rather than present with a zero that would drag the
         mean down.
   Post: cards are scoped through their PAGE's parent, not through
         `root_topic_id`. The two agree for a PDF, and scoping through the page
         is what keeps an extract's cards attached to the page they came from."
  [user-id root-topic-id]
  (mapv (fn [r] {:flashcard-id (:flashcards/id r)
                 :page-id (:flashcards/topic_id r)
                 :anki-note-id (:flashcards/anki_note_id r)})
    (jdbc/execute! ds
      (sql/format {:select [:f/id :f/topic_id :f/anki_note_id]
                   :from [[:flashcards :f]]
                   :join [[:topics :p] [:= :f/topic_id :p/id]]
                   :where [:and
                           [:= :p/parent_id root-topic-id]
                           [:= :p/user_id user-id]
                           [:is :p/staged_delete_id nil]
                           [:not= :f/anki_note_id nil]]
                   :order-by [[:p/page_number :asc] [:f/id :asc]]}))))

(defn get-schedule-range-anki-notes
  "Return pushed note IDs and unpushed-card count for one owned active schedule.

   Pre: `schedule-id` is supplied by a schedule-detail route.
   Post: only cards attached to PDF pages inside the schedule's inclusive range
         are considered. Repeated local cards for one note produce one note ID."
  [user-id schedule-id]
  (let [rows (jdbc/execute! ds
               ["SELECT f.anki_note_id
                   FROM study_schedules s
                   JOIN topics p ON p.parent_id = s.root_topic_id
                   JOIN flashcards f ON f.topic_id = p.id
                  WHERE s.id = ?
                    AND s.user_id = ?
                    AND s.archived_at IS NULL
                    AND p.user_id = ?
                    AND p.kind = 'page'
                    AND p.staged_delete_id IS NULL
                    AND (s.first_page IS NULL OR p.page_number >= s.first_page)
                    AND (s.last_page IS NULL OR p.page_number <= s.last_page)
                  ORDER BY p.page_number ASC, f.id ASC"
                schedule-id user-id user-id])]
    {:note-ids (->> rows (keep :flashcards/anki_note_id) distinct vec)
     :unpushed-card-count (count (remove :flashcards/anki_note_id rows))}))

;; --- The derived-state cache (§3.1 – §3.3) ---------------------------------

(defn get-anki-card-states
  "Cached FSRS state for a user's Anki cards, keyed by Anki card id.

   Pre : `note-ids` is a seq of Anki note ids, possibly empty.
   Post: {} for an empty `note-ids` — never a query with an empty IN list.
   Post: rows whose `config_fingerprint` differs from `fingerprint` are ABSENT.
         §3.3 requires a config change to discard cached state and replay in
         full, and filtering here is what makes the caller replay it: a row it
         cannot see is a card it must refetch."
  [user-id note-ids fingerprint]
  (if (empty? note-ids)
    {}
    (into {}
      (map (fn [r] [(:anki_card_states/anki_card_id r) r]))
      (jdbc/execute! ds
        (sql/format {:select [:*] :from [:anki_card_states]
                     :where [:and [:= :user_id user-id]
                             [:in :anki_note_id (vec note-ids)]
                             [:= :config_fingerprint fingerprint]]})))))

(defn upsert-anki-card-states!
  "Write derived FSRS state for a batch of Anki cards.

   Pre : each row carries :anki_card_id and :config_fingerprint. `user_id` is
         set here, never trusted from the row.
   Post: one row per (user, Anki card id). A re-refresh overwrites in place, so
         the table's size is bounded by the user's pushed card count and never
         grows with refresh count.
   Inv : `param-bounded-batches` splits the list, so a 5 000-card document
         inserts without a 65 535-parameter error (§3.2). The write is atomic
         only to the extent one transaction covers it, which is why this opens
         one."
  [user-id rows]
  (when (seq rows)
    (let [stamped (mapv #(assoc % :user_id user-id
                           :updated_at [:raw "now()"]) rows)]
      (jdbc/with-transaction [tx ds]
        (doseq [batch (param-bounded-batches stamped)]
          (jdbc/execute! tx
            (sql/format {:insert-into :anki_card_states
                         :values (vec batch)
                         :on-conflict [:user_id :anki_card_id]
                         :do-update-set [:anki_note_id :flashcard_id :deck :state :step
                                         :stability :difficulty :interval_days
                                         :last_review_at :due_at :reps
                                         :config_fingerprint :high_water_revlog_id
                                         :updated_at]}))))
      (count stamped))))

;; `anki-state-high-water` lived here. It fed the deck-scoped `cardReviews`
;; incremental pull, which AnkiConnect scopes by a card's CURRENT deck and
;; which therefore returned nothing for cards gathered into a filtered deck.
;; The revlog is read whole and by card id now, so no mark selects a pull.
;; `anki_card_states.high_water_revlog_id` is still WRITTEN, as the per-card
;; anchor `card-rows` derives from the replay, and is read by nobody.

(defn upsert-anki-deck-config!
  "Cache one deck's Anki config. Returns the row count written.

   Pre : `config` is the parsed `getDeckConfig` result and `fingerprint` is
         `schedule-fsrs/config-fingerprint` over it. The two travel together —
         a config stored under a fingerprint that does not describe it would
         make every cached state read as current while the weights disagree."
  [user-id deck config fingerprint]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      (sql/format {:insert-into :anki_deck_configs
                   :values [{:user_id user-id
                             :deck deck
                             :config (->jsonb config)
                             :fingerprint fingerprint
                             :fetched_at [:raw "now()"]}]
                   :on-conflict [:user_id :deck]
                   :do-update-set [:config :fingerprint :fetched_at]}))
    0))

(defn get-anki-deck-config
  "The user's most recently fetched deck config, or nil.

   One row is what the reader wants: FreeMemo pushes to one deck, and the
   scheduler picks that deck as the mode over its own cards' decks. Ordering by
   `fetched_at` rather than taking a named deck keeps the read correct for a
   user who moved their cards between decks."
  [user-id]
  (some-> (jdbc/execute-one! ds
            (sql/format {:select [:*] :from [:anki_deck_configs]
                         :where [:= :user_id user-id]
                         :order-by [[:fetched_at :desc]]
                         :limit 1}))
    (update :anki_deck_configs/config pgobject->clj)))

(defn anki-state-cached-at
  "When this user's cached state was last written, or nil.
   §8.2 renders it beside the staleness marker when Anki is unreachable — a
   stale forecast with no timestamp is a forecast the user cannot judge."
  [user-id]
  (:cached_at
    (jdbc/execute-one! ds
      (sql/format {:select [[[:max :updated_at] :cached_at]]
                   :from [:anki_card_states]
                   :where [:= :user_id user-id]}))))

;; ---------------------------------------------------------------------------
;; Question maps — plans/anchored-questions.md §6
;;
;; Every write here carries its ownership as a clause of the statement: a map
;; row holds `user_id`, and pins and roots are reached only through a map the
;; caller owns. A forged id therefore writes nothing.
;; ---------------------------------------------------------------------------

(defn question-doc-key
  "The document key of `user-id`'s repository file `topic-id`.
   Post: {:repo_topic_id :repo_path} when the topic is the user's, has a
         non-blank `repo_path`, and its root is a `repo` node; else nil."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["WITH RECURSIVE up AS (
        SELECT id, parent_id, kind, repo_path, id AS file_id
          FROM topics WHERE id = ? AND user_id = ? AND kind = 'code'
                        AND repo_path IS NOT NULL AND repo_path <> ''
        UNION ALL
        SELECT t.id, t.parent_id, t.kind, up.repo_path, up.file_id
          FROM topics t JOIN up ON t.id = up.parent_id)
      SELECT id AS repo_topic_id, repo_path FROM up
       WHERE parent_id IS NULL AND kind = 'repo'"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn question-doc-ref
  "The document ref of `user-id`'s repository file `topic-id`
   (plans/commit-range-walkthrough.md §6.3.2): its repository node, its path,
   and the sha of the commit node above it.
   Post: {:repo_topic_id :repo_path :commit_sha}, or nil as `question-doc-key`."
  [user-id topic-id]
  (jdbc/execute-one! ds
    ["WITH RECURSIVE up AS (
        SELECT id, parent_id, kind, repo_path, commit_sha, NULL::text AS sha
          FROM topics WHERE id = ? AND user_id = ? AND kind = 'code'
                        AND repo_path IS NOT NULL AND repo_path <> ''
        UNION ALL
        SELECT t.id, t.parent_id, t.kind, up.repo_path, t.commit_sha,
               coalesce(up.sha, CASE WHEN t.kind = 'commit' THEN t.commit_sha END)
          FROM topics t JOIN up ON t.id = up.parent_id)
      SELECT id AS repo_topic_id, repo_path, sha AS commit_sha FROM up
       WHERE parent_id IS NULL AND kind = 'repo'"
     topic-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn find-file-topic-at
  "The live file topic of `user-id`'s repository node `repo-id` at `path` under
   the commit node of `sha`, or nil when that commit or file was not imported."
  [user-id repo-id path sha]
  (:id (jdbc/execute-one! ds
         ["WITH RECURSIVE down AS (
             SELECT c.id FROM topics c
              WHERE c.parent_id = ? AND c.kind = 'commit' AND c.commit_sha = ?
                AND c.user_id = ? AND c.staged_delete_id IS NULL
             UNION ALL
             SELECT t.id FROM topics t JOIN down ON t.parent_id = down.id
              WHERE t.kind IN ('dir', 'code'))
           SELECT t.id FROM down JOIN topics t ON t.id = down.id
            WHERE t.kind = 'code' AND t.repo_path = ? LIMIT 1"
          repo-id sha user-id path]
         {:builder-fn rs/as-unqualified-maps})))

(defn owned-repo-node?
  "Post: true when `repo-id` is a live `repo` node `user-id` owns."
  [user-id repo-id]
  (some? (jdbc/execute-one! ds
           ["SELECT 1 FROM topics WHERE id = ? AND user_id = ? AND kind = 'repo'
               AND staged_delete_id IS NULL" repo-id user-id])))

(defn question-map-id
  "The id of the question map for a document key, or nil when none exists."
  [user-id repo-topic-id repo-path]
  (:id (jdbc/execute-one! ds
         ["SELECT id FROM question_maps
            WHERE user_id = ? AND repo_topic_id = ? AND repo_path = ?"
          user-id repo-topic-id repo-path]
         {:builder-fn rs/as-unqualified-maps})))

(defn ensure-question-map!
  "The id of the question map for a document key, created when absent.
   Pre:  `repo-topic-id` is a `repo` node `user-id` owns, else nil.
   Post: one row per document key, whatever the concurrency."
  [user-id repo-topic-id repo-path]
  (jdbc/execute-one! ds
    ["INSERT INTO question_maps (user_id, repo_topic_id, repo_path)
      SELECT ?, t.id, ? FROM topics t
       WHERE t.id = ? AND t.user_id = ? AND t.kind = 'repo'
      ON CONFLICT (repo_topic_id, repo_path) DO NOTHING"
     user-id repo-path repo-topic-id user-id])
  (question-map-id user-id repo-topic-id repo-path))

(defn create-question-pin!
  "Create one open pin in map `map-id` and give it the map's next label.
   Pre:  `quote` is non-blank. `snapshot-topic-id` is the file topic it was
         made on, or nil for a file at a commit that is not a topic; a topic
         the user does not own is stored as NULL. `snapshot-sha` is the
         commit it was made at (plans/commit-range-walkthrough.md §6.3.4).
   Post: the new row as an unqualified map, or nil when the map is not the
         user's. The label is taken and the counter moved in one transaction,
         so two concurrent creates never share a label, and a deleted pin's
         label is never given out again (§6.2.3)."
  [user-id map-id {:keys [snapshot-topic-id snapshot-sha quote heading-anchor offset-hint
                          question]}]
  (jdbc/with-transaction [tx ds]
    (when-let [label (:label (jdbc/execute-one! tx
                               ["UPDATE question_maps SET next_label = next_label + 1
                                  WHERE id = ? AND user_id = ?
                                 RETURNING next_label - 1 AS label"
                                map-id user-id]
                               {:builder-fn rs/as-unqualified-maps}))]
      (jdbc/execute-one! tx
        ["INSERT INTO question_pins
            (map_id, label, snapshot_topic_id, snapshot_sha, quote, heading_anchor,
             offset_hint, question)
          VALUES (?, ?, (SELECT id FROM topics WHERE id = ? AND user_id = ?), ?, ?, ?, ?, ?)
          RETURNING id, label, quote, heading_anchor, offset_hint, question, status, root_id"
         map-id label snapshot-topic-id user-id snapshot-sha (sanitize-utf8 quote)
         heading-anchor offset-hint (sanitize-utf8 (or question ""))]
        {:builder-fn rs/as-unqualified-maps}))))

(def ^:private owned-pin-clause
  "map_id IN (SELECT id FROM question_maps WHERE user_id = ?)")

(defn update-question-pin-question!
  "Replace the question text of `user-id`'s pin. Post: the update count."
  [user-id pin-id question]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      [(str "UPDATE question_pins SET question = ?, updated_at = now()
              WHERE id = ? AND " owned-pin-clause)
       (sanitize-utf8 (or question "")) pin-id user-id])))

(defn set-question-pin-status!
  "Set `user-id`'s pin to `status`, `open` or `closed`. Post: the update count."
  [user-id pin-id status]
  {:pre [(#{"open" "closed"} status)]}
  (::jdbc/update-count
    (jdbc/execute-one! ds
      [(str "UPDATE question_pins SET status = ?, updated_at = now()
              WHERE id = ? AND " owned-pin-clause)
       status pin-id user-id])))

(defn delete-question-pin!
  "Delete `user-id`'s pin. Its label is not given out again. Post: the count."
  [user-id pin-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      [(str "DELETE FROM question_pins WHERE id = ? AND " owned-pin-clause)
       pin-id user-id])))

(defn question-pins
  "Every pin of `user-id`'s map `map-id`, in label order."
  [user-id map-id]
  (jdbc/execute! ds
    [(str "SELECT id, label, quote, heading_anchor, offset_hint, question, status, root_id
             FROM question_pins WHERE map_id = ? AND " owned-pin-clause
          " ORDER BY label")
     map-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn question-roots
  "Every root of `user-id`'s map `map-id`, oldest first.
   Post: each row carries `:known_sources`, the sources of the known marks on
         its entity (`self`, `tutor`), empty when it has none or no entity, and
         `:citations` as Clojure data."
  [user-id map-id]
  (mapv #(-> %
           (update :citations pgobject->clj)
           (update :known_sources (fn [a] (if a (vec (.getArray ^java.sql.Array a)) []))))
    (jdbc/execute! ds
      [(str "SELECT r.id, r.label, r.entity_id, r.in_document, r.citations, r.status,
                    (SELECT array_agg(k.source ORDER BY k.source) FROM known_marks k
                      WHERE k.entity_id = r.entity_id) AS known_sources
               FROM question_roots r
              WHERE r.map_id = ? AND r." owned-pin-clause
            " ORDER BY r.id")
       map-id user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn- set-next-root!
  "Inside `tx`: make root `root-id` of map `map-id` the one `next` root.
   The old `next` root becomes `open` first, because the partial unique index
   allows one `next` per map at every statement boundary (§6.3.8)."
  [tx map-id root-id]
  (jdbc/execute! tx ["UPDATE question_roots SET status = 'open', updated_at = now()
                       WHERE map_id = ? AND status = 'next' AND id <> ?"
                     map-id root-id])
  (jdbc/execute! tx ["UPDATE question_roots SET status = 'next', updated_at = now()
                       WHERE map_id = ? AND id = ?"
                     map-id root-id]))

(defn apply-question-map-plan!
  "Write one validated reply plan into `user-id`'s map `map-id`, atomically.

   `plan` is `question-map-block/plan`'s output with entity ids added:
     :new-roots   [{:label :in-document? :citations [..] :entity-id}]
     :relinks     {root-label entity-id}, for roots whose entity was deleted
     :pin-roots   {pin-label root-label}
     :learned     #{root-label}
     :next        root-label or nil
     :as-of-sha   the commit of the document the reply was about, or nil

   Pre:  every label the plan names is a root after :new-roots is applied —
         the planner guarantees it.
   Post: nil when the map is not the user's. Otherwise, in one transaction:
         new roots exist; each named pin links its root; each learned root is
         `learned`, its open pins are `closed`, and its entity has a `tutor`
         known mark (§6.4.5); the `next` root, when named and not learned, is
         the map's one `next` root. A `tutor` mark records the map's
         repository node and `as-of-sha` as its version (§6.8.2 of
         plans/commit-range-walkthrough.md). Returns {:closed-pins n}."
  [user-id map-id {:keys [new-roots relinks pin-roots learned next as-of-sha]}]
  (jdbc/with-transaction [tx ds]
    (when (jdbc/execute-one! tx ["SELECT 1 FROM question_maps WHERE id = ? AND user_id = ?"
                                 map-id user-id])
      (doseq [{:keys [label in-document? citations entity-id]} new-roots]
        (jdbc/execute! tx
          ["INSERT INTO question_roots (map_id, label, entity_id, in_document, citations)
            VALUES (?, ?, (SELECT id FROM kg_entities WHERE id = ? AND user_id = ?), ?, ?)
            ON CONFLICT (map_id, label) DO NOTHING"
           map-id (sanitize-utf8 label) entity-id user-id (boolean in-document?)
           (->jsonb (vec citations))]))
      (doseq [[label entity-id] relinks]
        (jdbc/execute! tx
          ["UPDATE question_roots
               SET entity_id = (SELECT id FROM kg_entities WHERE id = ? AND user_id = ?),
                   updated_at = now()
             WHERE map_id = ? AND label = ? AND entity_id IS NULL"
           entity-id user-id map-id label]))
      (let [root-id (into {} (map (juxt :label :id))
                      (jdbc/execute! tx ["SELECT id, label FROM question_roots WHERE map_id = ?"
                                         map-id]
                        {:builder-fn rs/as-unqualified-maps}))]
        (doseq [[pin-label root-label] pin-roots
                :let [rid (root-id root-label)]
                :when rid]
          (jdbc/execute! tx ["UPDATE question_pins SET root_id = ?, updated_at = now()
                               WHERE map_id = ? AND label = ?"
                             rid map-id pin-label]))
        (let [learned-ids (vec (keep root-id learned))
              closed (if (seq learned-ids)
                       (let [ids (into-array Integer (map int learned-ids))]
                         (jdbc/execute! tx ["UPDATE question_roots SET status = 'learned', updated_at = now()
                                              WHERE map_id = ? AND id = ANY(?)"
                                            map-id ids])
                         (jdbc/execute! tx ["INSERT INTO known_marks
                                               (entity_id, source, as_of_repo_topic_id, as_of_sha)
                                             SELECT r.entity_id, 'tutor', m.repo_topic_id, ?
                                               FROM question_roots r
                                               JOIN question_maps m ON m.id = r.map_id
                                              WHERE r.map_id = ? AND r.id = ANY(?)
                                                AND r.entity_id IS NOT NULL
                                             ON CONFLICT (entity_id, source) DO UPDATE
                                               SET as_of_repo_topic_id = excluded.as_of_repo_topic_id,
                                                   as_of_sha = coalesce(excluded.as_of_sha,
                                                                        known_marks.as_of_sha)"
                                            as-of-sha map-id ids])
                         (::jdbc/update-count
                           (jdbc/execute-one! tx ["UPDATE question_pins SET status = 'closed', updated_at = now()
                                                    WHERE map_id = ? AND status = 'open' AND root_id = ANY(?)"
                                                  map-id ids])))
                       0)]
          (when-let [rid (and next (not (contains? (set learned) next)) (root-id next))]
            (when (jdbc/execute-one! tx ["SELECT 1 FROM question_roots
                                           WHERE id = ? AND status <> 'learned'" rid])
              (set-next-root! tx map-id rid)))
          {:closed-pins closed})))))

(defn set-question-root-next!
  "\"Teach me\": make `user-id`'s root `root-id` its map's one `next` root,
   whatever its status was (§6.3.7). Post: true when the root is the user's."
  [user-id root-id]
  (jdbc/with-transaction [tx ds]
    (when-let [map-id (:map_id (jdbc/execute-one! tx
                                 [(str "SELECT map_id FROM question_roots
                                         WHERE id = ? AND " owned-pin-clause)
                                  root-id user-id]
                                 {:builder-fn rs/as-unqualified-maps}))]
      (set-next-root! tx map-id root-id)
      true)))

(defn ensure-question-root-entity!
  "The entity id of `user-id`'s root `root-id`, creating one from the root's
   label when the root has none — its entity was deleted (§6.4.2.4), or it was
   never linked. A created row is marked `from_diagnosis`.
   Post: the entity id, or nil when the root is not the user's."
  [user-id root-id]
  (when-let [root (jdbc/execute-one! ds
                    [(str "SELECT id, label, entity_id FROM question_roots
                            WHERE id = ? AND " owned-pin-clause)
                     root-id user-id]
                    {:builder-fn rs/as-unqualified-maps})]
    (or (:entity_id root)
      (let [entity-id (first (insert-kg-entities! user-id [(:label root)]
                               {:from-diagnosis? true}))]
        (jdbc/execute! ds ["UPDATE question_roots SET entity_id = ?, updated_at = now()
                             WHERE id = ? AND entity_id IS NULL"
                           entity-id root-id])
        (:entity_id (jdbc/execute-one! ds ["SELECT entity_id FROM question_roots WHERE id = ?"
                                           root-id]
                      {:builder-fn rs/as-unqualified-maps}))))))

(defn set-known-mark!
  "Write a known mark with `source` on `user-id`'s entity. Idempotent.
   `as-of` is {:repo-id :sha}, the commit the reader learned the idea at, or
   nil when it is not about a repository (K2, plans/commit-range-walkthrough.md
   §6.8). A repeated mark records the newer version: the reader knows the idea
   as it stands there.
   Post: true when the entity is the user's. A repository node the user does
         not own is stored as no version."
  ([user-id entity-id source] (set-known-mark! user-id entity-id source nil))
  ([user-id entity-id source {:keys [repo-id sha]}]
   {:pre [(#{"self" "tutor"} source)]}
   (some? (jdbc/execute-one! ds
            ["INSERT INTO known_marks (entity_id, source, as_of_repo_topic_id, as_of_sha)
              SELECT e.id, ?, r.id, CASE WHEN r.id IS NULL THEN NULL ELSE ? END
                FROM kg_entities e
                LEFT JOIN topics r ON r.id = ? AND r.user_id = e.user_id AND r.kind = 'repo'
               WHERE e.id = ? AND e.user_id = ?
              ON CONFLICT (entity_id, source) DO UPDATE
                SET as_of_repo_topic_id = coalesce(excluded.as_of_repo_topic_id,
                                                   known_marks.as_of_repo_topic_id),
                    as_of_sha = CASE WHEN excluded.as_of_repo_topic_id IS NULL
                                     THEN known_marks.as_of_sha ELSE excluded.as_of_sha END
              RETURNING entity_id"
             source sha repo-id entity-id user-id]))))

(defn delete-known-marks!
  "Remove every known mark on `user-id`'s entity, of both sources (§6.4.4).
   Post: the number of marks removed."
  [user-id entity-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      ["DELETE FROM known_marks
         WHERE entity_id IN (SELECT id FROM kg_entities WHERE id = ? AND user_id = ?)"
       entity-id user-id])))


(defn known-marks-for-entities
  "The known marks on `user-id`'s entities `entity-ids`.
   Post: [{:entity_id :label :source :as_of_repo_topic_id :as_of_sha}]."
  [user-id entity-ids]
  (if (empty? entity-ids)
    []
    (jdbc/execute! ds
      ["SELECT k.entity_id, e.label, k.source, k.as_of_repo_topic_id, k.as_of_sha
          FROM known_marks k JOIN kg_entities e ON e.id = k.entity_id
         WHERE e.user_id = ? AND k.entity_id = ANY(?)
         ORDER BY e.label, k.source"
       user-id (into-array Integer (map int entity-ids))]
      {:builder-fn rs/as-unqualified-maps})))

(defn known-marks-on-repo
  "Every known mark of `user-id` whose version is a commit of repository node
   `repo-id` (K2, plans/commit-range-walkthrough.md §6.8).
   Post: [{:entity_id :label :source :as_of_sha}], by label."
  [user-id repo-id]
  (jdbc/execute! ds
    ["SELECT k.entity_id, e.label, k.source, k.as_of_sha
        FROM known_marks k JOIN kg_entities e ON e.id = k.entity_id
       WHERE e.user_id = ? AND k.as_of_repo_topic_id = ?
       ORDER BY e.label, k.source"
     user-id repo-id]
    {:builder-fn rs/as-unqualified-maps}))

;; ---------------------------------------------------------------------------
;; Commit-range walkthroughs — plans/commit-range-walkthrough.md §6.4–§6.6
;;
;; Ownership is a clause of every statement: a walkthrough row holds
;; `user_id`, and concepts and hunks are reached only through one the caller
;; owns. A forged id therefore reads and writes nothing.
;; ---------------------------------------------------------------------------

(def ^:private owned-walkthrough-clause
  "walkthrough_id IN (SELECT id FROM walkthroughs WHERE user_id = ?)")

(defn- walkthrough-row [row]
  (some-> row (update :commits pgobject->clj)))

(defn create-walkthrough!
  "Start a walkthrough of `user-id`'s repository node `repo-id` from `sha-a`
   to `sha-b`, in status `preparing`.
   Pre : `commits` is the range, oldest first.
   Post: the new id, or nil when the repository node is not the user's."
  [user-id repo-id sha-a sha-b commits]
  (:id (jdbc/execute-one! ds
         ["INSERT INTO walkthroughs (user_id, repo_topic_id, sha_a, sha_b, commits)
           SELECT ?, t.id, ?, ?, ? FROM topics t
            WHERE t.id = ? AND t.user_id = ? AND t.kind = 'repo' AND t.staged_delete_id IS NULL
           RETURNING id"
          user-id sha-a sha-b (->jsonb (vec commits)) repo-id user-id]
         {:builder-fn rs/as-unqualified-maps})))

(defn get-walkthrough
  "Post: `user-id`'s walkthrough `walkthrough-id` with `:commits` as data, or nil."
  [user-id walkthrough-id]
  (walkthrough-row
    (jdbc/execute-one! ds
      ["SELECT w.*, r.title AS repo_title FROM walkthroughs w
          JOIN topics r ON r.id = w.repo_topic_id
         WHERE w.id = ? AND w.user_id = ?"
       walkthrough-id user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn list-walkthroughs
  "`user-id`'s walkthroughs of repository node `repo-id`, newest first.
   Post: [{:id :sha_a :sha_b :status :step :concepts :created_at}], without
         the range."
  [user-id repo-id]
  (jdbc/execute! ds
    ["SELECT w.id, w.sha_a, w.sha_b, w.status, w.error, w.step, w.created_at,
             (SELECT count(*) FROM walkthrough_concepts c WHERE c.walkthrough_id = w.id) AS concepts,
             jsonb_array_length(w.commits) AS commit_count
        FROM walkthroughs w
       WHERE w.user_id = ? AND w.repo_topic_id = ?
       ORDER BY w.id DESC"
     user-id repo-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn set-walkthrough-failed!
  "Mark `user-id`'s walkthrough failed with `error`. Post: the update count."
  [user-id walkthrough-id error]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      ["UPDATE walkthroughs SET status = 'failed', error = ?, updated_at = now()
         WHERE id = ? AND user_id = ?"
       (sanitize-utf8 (str error)) walkthrough-id user-id])))

(defn save-walkthrough-grouping!
  "Store the grouping of `user-id`'s walkthrough and make it `ready`, in one
   transaction (§6.5).

   Pre : `concepts` is [{:label :summary :commits [sha] :noise?}] in step
         order, labels distinct. `hunks` is [{:commit-sha :old-path :new-path
         :header :index :added :removed :concept label}], and every
         `:concept` names one of `concepts`.
   Post: true when the walkthrough is the user's and still `preparing`; the
         concepts hold positions 1..n and every hunk one row. False and
         nothing written otherwise: a cancelled preparation has been marked
         `failed`, and its grouping must not land (§6.5.5)."
  [user-id walkthrough-id concepts hunks]
  (jdbc/with-transaction [tx ds]
    (if-not (jdbc/execute-one! tx ["SELECT 1 FROM walkthroughs
                                     WHERE id = ? AND user_id = ? AND status = 'preparing'
                                     FOR UPDATE"
                                    walkthrough-id user-id])
      false
      (let [ids (into {}
                  (map-indexed
                    (fn [i {:keys [label summary commits noise?]}]
                      [label (:id (jdbc/execute-one! tx
                                    ["INSERT INTO walkthrough_concepts
                                        (walkthrough_id, position, label, summary, commits, noise)
                                      VALUES (?, ?, ?, ?, ?, ?) RETURNING id"
                                     walkthrough-id (inc i) (sanitize-utf8 label)
                                     (sanitize-utf8 (or summary "")) (->jsonb (vec commits))
                                     (boolean noise?)]
                                    {:builder-fn rs/as-unqualified-maps}))]))
                  concepts)]
        (doseq [batch (param-bounded-batches
                        (mapv (fn [{:keys [commit-sha old-path new-path header index added removed concept]}]
                                {:walkthrough_id walkthrough-id
                                 :concept_id (or (ids concept)
                                               (throw (ex-info "A hunk names no concept"
                                                        {:concept concept})))
                                 :commit_sha commit-sha :old_path old-path :new_path new-path
                                 :header (sanitize-utf8 (or header "")) :hunk_index index
                                 :added (or added 0) :removed (or removed 0)})
                          hunks))]
          (jdbc/execute! tx (sql/format {:insert-into :walkthrough_hunks :values (vec batch)})))
        (jdbc/execute! tx ["UPDATE walkthroughs SET status = 'ready', error = NULL, step = 0,
                                   updated_at = now()
                             WHERE id = ?" walkthrough-id])
        true))))

(defn walkthrough-concepts
  "The concepts of `user-id`'s walkthrough, in step order.
   Post: [{:id :position :label :summary :commits :noise :understood_at
           :hunk_count}]."
  [user-id walkthrough-id]
  (mapv #(update % :commits pgobject->clj)
    (jdbc/execute! ds
      [(str "SELECT c.id, c.position, c.label, c.summary, c.commits, c.noise, c.understood_at,
                    (SELECT count(*) FROM walkthrough_hunks h WHERE h.concept_id = c.id) AS hunk_count
               FROM walkthrough_concepts c
              WHERE c.walkthrough_id = ? AND c." owned-walkthrough-clause
            " ORDER BY c.position")
       walkthrough-id user-id]
      {:builder-fn rs/as-unqualified-maps})))

(defn walkthrough-hunks
  "Every hunk row of `user-id`'s walkthrough, in commit and file order.
   Post: [{:id :concept_id :commit_sha :old_path :new_path :header
           :hunk_index :added :removed}]."
  [user-id walkthrough-id]
  (jdbc/execute! ds
    [(str "SELECT h.id, h.concept_id, h.commit_sha, h.old_path, h.new_path, h.header,
                  h.hunk_index, h.added, h.removed
             FROM walkthrough_hunks h
            WHERE h.walkthrough_id = ? AND h." owned-walkthrough-clause
          " ORDER BY h.id")
     walkthrough-id user-id]
    {:builder-fn rs/as-unqualified-maps}))

(defn set-walkthrough-step!
  "Move `user-id`'s walkthrough to `step`, clamped to 0..its concept count.
   Post: the step stored, or nil when the walkthrough is not the user's."
  [user-id walkthrough-id step]
  (:step (jdbc/execute-one! ds
           ["UPDATE walkthroughs w
               SET step = greatest(0, least(?, (SELECT count(*) FROM walkthrough_concepts c
                                                 WHERE c.walkthrough_id = w.id))),
                   updated_at = now()
             WHERE w.id = ? AND w.user_id = ?
             RETURNING step"
            step walkthrough-id user-id]
           {:builder-fn rs/as-unqualified-maps})))

(defn mark-concept-understood!
  "\"I understand\" on the concept at `user-id`'s walkthrough's current step
   (§6.6.5): the concept records when, and the walkthrough moves to the next
   step. On the opener (step 0) only the step moves.
   Post: the new step, or nil when the walkthrough is not the user's."
  [user-id walkthrough-id]
  (jdbc/with-transaction [tx ds]
    (when-let [{:keys [step]} (jdbc/execute-one! tx
                                ["SELECT step FROM walkthroughs WHERE id = ? AND user_id = ?
                                  FOR UPDATE" walkthrough-id user-id]
                                {:builder-fn rs/as-unqualified-maps})]
      (jdbc/execute! tx ["UPDATE walkthrough_concepts SET understood_at = coalesce(understood_at, now())
                           WHERE walkthrough_id = ? AND position = ?"
                         walkthrough-id step])
      (:step (jdbc/execute-one! tx
               ["UPDATE walkthroughs w
                   SET step = least(? + 1, (SELECT count(*) FROM walkthrough_concepts c
                                             WHERE c.walkthrough_id = w.id)),
                       updated_at = now()
                 WHERE w.id = ? RETURNING step"
                step walkthrough-id]
               {:builder-fn rs/as-unqualified-maps})))))

(defn move-walkthrough-hunk!
  "Move `user-id`'s hunk `hunk-id` to concept `concept-id` of the same
   walkthrough (§6.5.7.1). Post: the update count, 0 when either is not the
   user's or they belong to different walkthroughs."
  [user-id hunk-id concept-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      [(str "UPDATE walkthrough_hunks h SET concept_id = c.id
               FROM walkthrough_concepts c
              WHERE h.id = ? AND c.id = ? AND c.walkthrough_id = h.walkthrough_id
                AND h." owned-walkthrough-clause)
       hunk-id concept-id user-id])))

(defn rename-walkthrough-concept!
  "Rename `user-id`'s concept (§6.5.7.2). Post: the update count; 0 when the
   label is taken in its walkthrough or the concept is not the user's."
  [user-id concept-id label]
  (::jdbc/update-count
    (jdbc/execute-one! ds
      [(str "UPDATE walkthrough_concepts c SET label = ?
              WHERE c.id = ? AND c." owned-walkthrough-clause
            " AND NOT EXISTS (SELECT 1 FROM walkthrough_concepts o
                               WHERE o.walkthrough_id = c.walkthrough_id
                                 AND o.label = ? AND o.id <> c.id)")
       (sanitize-utf8 label) concept-id user-id (sanitize-utf8 label)])))

(defn merge-walkthrough-concepts!
  "Merge `user-id`'s concept `from-id` into `into-id` of the same walkthrough
   (§6.5.7.3): its hunks move, it is deleted, and the positions after it close
   up. The walkthrough's step stays on the concept it showed.
   Post: true when both are the user's and in one walkthrough."
  [user-id from-id into-id]
  (jdbc/with-transaction [tx ds]
    (let [rows (jdbc/execute! tx
                 [(str "SELECT id, walkthrough_id, position FROM walkthrough_concepts
                         WHERE id = ANY(?) AND " owned-walkthrough-clause " FOR UPDATE")
                  (into-array Integer [(int from-id) (int into-id)]) user-id]
                 {:builder-fn rs/as-unqualified-maps})
          by-id (into {} (map (juxt :id identity)) rows)
          from (by-id from-id)
          into* (by-id into-id)]
      (when (and from into* (not= from-id into-id)
              (= (:walkthrough_id from) (:walkthrough_id into*)))
        (let [wid (:walkthrough_id from)
              pos (:position from)]
          (jdbc/execute! tx ["UPDATE walkthrough_hunks SET concept_id = ? WHERE concept_id = ?"
                             into-id from-id])
          (jdbc/execute! tx ["UPDATE walkthrough_concepts c
                                 SET commits = (SELECT coalesce(jsonb_agg(DISTINCT v), '[]')
                                                  FROM (SELECT jsonb_array_elements(c.commits) v
                                                        UNION SELECT jsonb_array_elements(f.commits)
                                                          FROM walkthrough_concepts f WHERE f.id = ?) x)
                               WHERE c.id = ?" from-id into-id])
          (jdbc/execute! tx ["DELETE FROM walkthrough_concepts WHERE id = ?" from-id])
          (jdbc/execute! tx ["UPDATE walkthrough_concepts SET position = position - 1
                               WHERE walkthrough_id = ? AND position > ?" wid pos])
          (jdbc/execute! tx ["UPDATE walkthroughs SET step = CASE WHEN step > ? THEN step - 1
                                                                   ELSE step END,
                                                      updated_at = now()
                               WHERE id = ?" pos wid])
          true)))))

(defn delete-walkthrough!
  "Delete `user-id`'s walkthrough. Post: the update count."
  [user-id walkthrough-id]
  (::jdbc/update-count
    (jdbc/execute-one! ds ["DELETE FROM walkthroughs WHERE id = ? AND user_id = ?"
                           walkthrough-id user-id])))
