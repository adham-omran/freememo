(ns freememo.neoreader-kinds
  "Which document kinds a NeoReader sync accepts, as shared vocabulary.

   `.cljc` with NO requires and NO reader conditional, because both peers need
   this and for different reasons: the server gates two HTTP routes on it
   (`api/syncable-topic-for`), and the client gates a Library row action on it
   (`library-row-actions/RowActionsMenu`).

   Putting it in `neoreader-sync` would not work. That is a `.clj`, so the var
   would not exist on the CLJS peer — and a CLJ-only var read from client scope
   compiles with no warning, emits nothing, and makes the two peers disagree on
   the frame's shape. The mismatch kills the WebSocket at mount with code 1011
   rather than raising a missing-var error. `transcribe_language.cljc` records
   that exact crash on the Settings page.

   The vocabulary lives in one place so the client's button and the server's
   route cannot disagree. A set literal at each site was the alternative, and a
   button that appears over a route that 404s is the failure it invites.")

(def syncable-kinds
  "Document kinds a NeoReader export can sync into.

   The rule, not the list, is what to reason from: a kind qualifies when Boox
   exports ONE file per document AND the document's text lives on CHILD topics.
   `document-sections` walks those children, so a kind holding its text on the
   topic itself has nothing to walk.

   That excludes `web`, `wikipedia`, `markdown` and `supermemo`, which are
   single topics, and `video`, `audio` and `score`, which are not books. A
   single-topic sync mode is a different feature."
  #{"epub" "pdf"})

(defn syncable-kind?
  "Pre : `kind` is a `topics.kind` string, or nil.
   Post: true iff a NeoReader sync accepts that kind."
  [kind]
  (contains? syncable-kinds kind))

(def section-noun
  "What one of a document's children is called, per kind, SINGULAR.

   An EPUB's are chapters and a PDF's are pages. Reported text says which, so a
   reader is told about the thing they navigate rather than about `topics` rows.
   A caller pluralizes — the shape varies by sentence."
  {"epub" "chapter"
   "pdf" "page"})
