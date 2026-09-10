(ns freememo.code-languages
  "The code-block languages this app offers — the single vocabulary.

   NO REQUIRES, deliberately, and that rule is load-bearing. Three namespaces
   read this list and they sit on opposite sides of two boundaries:

     1. `freememo.quill-field`'s `:syntax` module. Quill renders one
        `select.ql-ui` per code block from it, and `freememo.code-lang-picker`
        mirrors that select's options into its own popup — so the per-block
        picker shows this list without naming it.
     2. `freememo.apply-code-language`'s modal, which needs the list with no
        editor mounted and no code block on screen.
     3. `freememo.code-language-html`, a plain-Clojure Jsoup namespace with no
        database dependency, which rejects a language that is not here before
        writing it into stored HTML.

   Reader 1 is an Electric `.cljc`; reader 3 must NOT load Electric. Holding the
   list in either one would drag that peer's whole require graph into the other.
   Same shape and same reason as `freememo.landing-copy`.

   NO reader conditional either. Reader 2 names `code-languages` from client
   scope, and a `#?(:clj …)`-only def emits nothing on the CLJS peer — the two
   peers then disagree on frame shape and `runtime3/socket-transfer` closes the
   WebSocket with code 1011 at mount, rather than raising a missing-var error.
   `transcribe_language.cljc` records the Settings-page crash that taught this.")

(def code-languages
  "Picker order. `:key` is the highlight.js language id AND the value Quill
   writes into `data-language`; `:label` is display text only.

   \"plain\" means no highlighting. It stays in the list because clearing every
   block's language is a legitimate thing to ask for."
  [{:key "plain" :label "Plain"}
   {:key "bash" :label "Bash"}
   {:key "c" :label "C"}
   {:key "cpp" :label "C++"}
   {:key "cs" :label "C#"}
   {:key "clojure" :label "Clojure"}
   {:key "css" :label "CSS"}
   {:key "diff" :label "Diff"}
   {:key "xml" :label "HTML/XML"}
   {:key "java" :label "Java"}
   {:key "javascript" :label "JavaScript"}
   {:key "markdown" :label "Markdown"}
   {:key "php" :label "PHP"}
   {:key "python" :label "Python"}
   {:key "ruby" :label "Ruby"}
   {:key "rust" :label "Rust"}
   {:key "sql" :label "SQL"}
   {:key "zig" :label "Zig"}])

(def code-language-keys
  "`code-languages` as a set of `:key` strings.

   The vector answers \"what should the picker show\". This answers \"may this
   string be written into `data-language`\" — the question every writer asks and
   the vector cannot answer without a linear scan."
  (into #{} (map :key) code-languages))
