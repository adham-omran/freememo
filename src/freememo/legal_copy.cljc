(ns freememo.legal-copy
  "Every word of the public legal documents, as data, on both peers.

   Two renderers read this namespace. `freememo.legal-page` renders it through
   Electric for a visitor, and `freememo.crawler-page` renders it to static HTML
   for a crawler. One home per sentence is the point, for the reason
   `freememo.landing-copy` states: a crawler that reads different words than a
   visitor sees is what search engines call cloaking.

   ## This namespace MUST stay free of requires

   `freememo.crawler-page` is plain Clojure on the server boot path, and
   `freememo.legal-page` pulls in Electric. A require here would drag Electric's
   compiler into the server's boot, which `dev/load-app-namespaces!` exists to
   keep off the main thread (see CLAUDE.md, dev JVM stack). Pure data needs
   nothing, so this costs nothing.

   ## `documents` owns the path set

   Three dispatch sites ask this namespace which paths are legal documents:
   `freememo.shell/crawler-response`, `freememo.main/Main`, and
   `freememo.crawler-page/legal-document`. The keys of `documents` are that set.
   A path list repeated at those three sites would drift, and the failure would
   be a page that renders for a visitor and 404s for a crawler.

   ## Two sentences here are claims about the world, not about the code

   `:controller` names the operator, and the transfer paragraph names Standard
   Contractual Clauses as the safeguard. Neither is derivable from the source,
   and no test can check either. Both are marked at their definition below.

   ## The drift this design accepts

   The data and recipient sections describe what `api.clj`, `openrouter.clj` and
   `wayl.clj` do, but they live here. A recipient added in code drifts from this
   text with no failing test. `legal-copy-keys-are-read-by-both-renderers` guards
   renderer parity, not code-to-copy truth. See
   plans/legal-pages-and-ads-tag-removal.md item 1.5.

   ## Body part shapes

   A section body is a vector of parts. Each part is one of:

     {:p \"text\"}                         :text — a paragraph
     {:p [{:t \"…\"} {:t \"…\" :href \"…\"}]}  :run  — a paragraph that carries a link
     {:ul [\"…\" \"…\"]}                     :list — a bullet list

   `part-kind` is the one classifier, and both renderers dispatch on it. Neither
   may re-derive a shape from the part's keys: a renderer that decides for itself
   that a `:p` is a string drops every paragraph carrying a link, and nothing in
   the page says so. `unrenderable-part-marker` is what each renderer emits when
   `part-kind` returns nil, so a shape neither of them knows is visible on the
   page instead of missing from it.")

(def part-kinds
  "Every body-part shape a renderer must implement.
   `legal-renderers-implement-every-part-kind` runs each renderer over one part
   of each kind, so a kind added here fails that test until both handle it."
  #{:text :run :list})

(defn part-kind
  "Body part → its kind, or nil when it matches no shape in `part-kinds`.
   Pre: `part` is a map from a section body.
   Post: a member of `part-kinds`, or nil. A nil is a copy bug, not a renderer
   bug — the renderers show `unrenderable-part-marker` rather than dropping it."
  [part]
  (let [p (:p part)]
    (cond
      (string? p) :text
      (vector? p) :run
      (vector? (:ul part)) :list)))

(def unrenderable-part-marker
  "What a renderer emits for a part `part-kind` does not classify.

   Visible text, not an exception. A legal document exists to be read, so a
   dropped disclosure is the worst outcome and a crashed page is the second
   worst — an Electric exception closes the WebSocket and blanks everything.
   `no-document-holds-an-unrenderable-part` fails on this string, so it never
   reaches a deploy."
  "[unrenderable part]")

(def contact-email
  "The one address every legal document routes a request to.
   `freememo.landing-page` and `freememo.crawler-page` carry the same literal in
   their contact lines."
  "contact@adham-omran.com")

(def controller
  "The operator named as data controller.

   NOT derivable from source. This is a claim about who runs the deployment, and
   no test can check it. A deployment that is run by somebody else MUST edit
   this line before it serves the document."
  "Adham Omran")

(def privacy-updated
  "The date the privacy text last changed. Edit it whenever `privacy-sections`
   changes, so a reader can tell a revision from a redeploy."
  "16 August 2026")

(def privacy-sections
  "The privacy policy, in the order a reader meets it.

   The order lives in both renderers as well, so a section added here is
   invisible until each renderer reads it too. That is the same cost
   `freememo.landing-copy` accepts, and `legal-copy-keys-are-read-by-both-renderers`
   is the guard against it."
  [{:heading "Who runs FreeMemo"
    :body [{:p (str "FreeMemo is operated by " controller
                 ". The operator is the data controller for the personal data described below.")}
           {:p [{:t "Write to "}
                {:t contact-email :href (str "mailto:" contact-email)}
                {:t " about anything on this page. That address reaches the operator directly."}]}]}

   {:heading "What FreeMemo collects"
    :body [{:p "FreeMemo collects four kinds of data about you."}
           {:ul ["Account data. If you sign in with Google, FreeMemo stores your Google account identifier and the email address on that account. If you register with the form, FreeMemo stores your username and a hash of your password. FreeMemo never stores a password itself."
                 "The material you import. This is every file and every page you upload or fetch: PDFs, EPUBs, HTML, Markdown, images, audio, video, and code archives."
                 "The work you do on that material. This is your extracts, your flashcards, your quiz answers, your messages to the study assistant, your bibliography entries, your settings, and your review history."
                 "Payment records. FreeMemo stores which credit packs you bought and what each one cost. FreeMemo never sees your card details."]}
           {:p "FreeMemo reads your IP address to decide which currency to show you at checkout. It does not store your IP address."}]}

   {:heading "Why FreeMemo processes it, and on what basis"
    :body [{:p "FreeMemo processes all of the above to give you the service you asked for. Under the GDPR the basis is Article 6(1)(b), performance of a contract with you."}
           {:p "FreeMemo runs no advertising and no analytics. It builds no profile of you, and it makes no automated decision about you."}
           {:p "FreeMemo never sells your data."}]}

   {:heading "Who else receives it"
    :body [{:p "Three companies receive some of your data, and each receives only what its job needs."}
           {:ul ["OpenRouter. FreeMemo sends page text, card text and your quiz answers to OpenRouter, which passes them to the AI model that reads a page, writes a card, transcribes audio, or grades an answer. This happens only when you start one of those steps."
                 "Google. If you sign in with Google, Google handles that sign-in. If you connect Google Drive, Google serves the files you pick."
                 "Wayl. Wayl takes your payment. You enter your card details on Wayl's own page, and they never reach FreeMemo."]}
           {:p "No other company receives your data."}]}

   {:heading "Where your data goes"
    :body [{:p "FreeMemo runs its servers outside the European Economic Area, and the three companies above are outside it as well."}
           {:p "FreeMemo relies on the European Commission's Standard Contractual Clauses for those transfers."}]}

   {:heading "How long FreeMemo keeps it"
    :body [{:p "FreeMemo keeps your material for as long as your account exists. It has no automatic expiry, because a reading queue that deleted itself would be useless."}
           {:p "When you delete a document, FreeMemo holds it for 12 hours so that you can undo the deletion, then removes it for good."}
           {:p "When you ask to close your account, FreeMemo deletes everything you own. Nothing survives that step."}]}

   {:heading "Your rights"
    :body [{:p "You can ask FreeMemo to do any of the following."}
           {:ul ["Show you the data it holds about you."
                 "Correct data that is wrong."
                 "Delete your account and everything in it."
                 "Give you your data in a portable file."
                 "Stop a particular kind of processing, or object to it."]}
           {:p "You can export your own data at any time from the Settings page. You do not need to ask for that one."}
           {:p [{:t "For the rest, write to "}
                {:t contact-email :href (str "mailto:" contact-email)}
                {:t ". FreeMemo answers within one month, as Article 12(3) requires."}]}]}

   {:heading "Complaints"
    :body [{:p "If you think FreeMemo has handled your data badly, tell the operator first. That is usually the fastest route to a fix."}
           {:p "You also have the right to complain to your national data protection authority. You do not have to contact FreeMemo before you do that."}]}

   {:heading "What FreeMemo never sees"
    :body [{:p "Zotero imports go from your Zotero library straight to your browser. Those files never reach a FreeMemo server."}
           {:p "Anki sync writes to the Anki running on your own machine. Your Anki collection never reaches a FreeMemo server."}]}

   {:heading "Cookies"
    :body [{:p "FreeMemo sets one cookie. It holds your sign-in session and nothing else."}
           {:p "FreeMemo sets no advertising cookie and no analytics cookie. There is nothing here to consent to, which is why you see no cookie banner."}]}])

(def privacy-path
  "Where the privacy policy is served. Every footer link reads this rather than
   a literal, so a path change cannot leave a dead link behind."
  "/privacy")

(def documents
  "Path → document. The keys are the whole legal path set.

   Every value carries `:title`, `:updated` and `:sections`. `:title` reaches a
   `<title>` element and an `<h1>`, so it MUST read as a name rather than a
   sentence."
  {privacy-path {:title "Privacy Policy"
                 :updated privacy-updated
                 :sections privacy-sections}})

(defn document-at
  "Path → the document served there, or nil.
   Pre: `path` is a request path or nil. Post: nil for every path that is not a
   legal document, which is what both dispatch sites branch on."
  [path]
  (get documents path))
