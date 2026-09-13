(ns freememo.poem-panel
  "الشرح — the poem word panel, and the selection state the reader and the panel
   share.

   The two live in different subtrees. `freememo.poem-pane` renders inside
   ContentColumn and `PoemPanel` renders inside RightSidePanel, so a tapped word
   cannot travel down a prop from one to the other. `!selection` is the seam: a
   plain client atom, written by the reader's delegated click handler and
   watched here.

   It lives in THIS namespace rather than in the reader for the require
   direction. `right-side-panel` must reach `PoemPanel`, and the reader must
   reach the writer; putting the atom here means poem-pane requires poem-panel
   and nothing requires poem-pane but document-view. The reverse placement makes
   a cycle.

   No reader conditional on the atoms, matching `freememo.client-state`: the
   vars must resolve on BOTH peers for Electric's dual compile, and every call
   site is client-sited, so the server peer's copy is inert.

   Everything the panel shows is quoted from a book. Nothing here writes a word
   of its own — an absent note renders row 4.3's empty state, never a guess."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.math :as math]
   [freememo.poem-cards :as pc]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.content-toolbar-helpers :as helpers]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; ---------------------------------------------------------------------------
;; Shared selection state
;; ---------------------------------------------------------------------------

(defonce ^{:doc "The tapped word: {:topic-id n :verse n :word i}, or nil.
                 :topic-id is carried so a selection made on one poem cannot
                 paint into another — the panel compares it against its own
                 topic rather than trusting that a navigation cleared this."}
  !selection
  (atom nil))

(defonce ^{:doc "Which face of the panel is showing: :word or :verse. Row 5.3
                 requires that back from the verse view returns to the SAME
                 word, so this is separate from !selection and a view switch
                 never touches it."}
  !view
  (atom :word))

(def panel-tab
  "The right panel's tab id for this panel. `settings/right-panel-tabs` bounds
   the persisted value to this and \"assistant\"."
  "poem")

(defn select-word!
  "Open the panel on `word-i` of verse `verse-n`. Called from the reader's
   delegated click handler, which is a plain CLJS callback.

   Also reveals the right panel on the الشرح tab, through the context fn
   RightSidePanel publishes. A tap that only wrote the atom would change
   nothing visible while the panel was collapsed, which is its state on a first
   visit."
  [topic-id verse-n word-i]
  (reset! !view :word)
  (reset! !selection {:topic-id topic-id :verse verse-n :word word-i})
  (when-let [show! (:show-right-pane! @bus/!ctx)]
    (show! panel-tab))
  nil)

(defn clear-selection!
  "Drop the selection. The reader calls this when it mounts for a topic, so a
   word tapped in the previous poem does not survive the navigation."
  []
  (reset! !view :word)
  (reset! !selection nil)
  nil)

;; ---------------------------------------------------------------------------
;; Package reads
;; ---------------------------------------------------------------------------

(defn get-poem-package*
  "The whole stored package for `topic-id`, or nil. Body-level reader
   conditional because the bare symbol is named inside `(e/server (e/Offload …))`
   in a reactive body, which CLJS compiles too (CLAUDE.md, server-only fns)."
  [_refresh topic-id]
  #?(:clj (when topic-id (db/get-poem-package topic-id))
     :cljs nil))

(def commentary-source-order
  "The sources a verse view quotes, in the order it quotes them: الدرة,
   التبريزي, الأنباري, الزوزني, الشيباني (spec rows 5.1 and 5.2). A source with
   no section for the verse is left out, never shown empty."
  ["151017" "37488" "124335" "11253" "21574"])

(defn- sources-by-id [pkg]
  (into {} (map (juxt :id identity)) (:sources pkg)))

(defn- note-view
  "One quoted block: its package HTML and its source chip. `sources` is the
   by-id map."
  [sources note]
  {:id (:id note)
   :html (:html note)
   :label (pc/source-label (:short (get sources (:source_id note)))
            (:printed_page note))})

(defn- verse-by-n [pkg n]
  (first (filter #(= n (:n %)) (:verses pkg))))

#?(:clj
   (defn- saved-field-set
     "Normalized question and cloze text of every card already on `topic-id`.

      Read through `poem-cards/normalize-for-match` on both sides, because
      `:add-card` sanitizes a field before the insert and Jsoup re-serializes
      attributes and whitespace — the stored bytes are not the built bytes.

      One query per word tap. A poem is one topic (S2), so this is every card
      the poem holds; at three cards per word that is a few hundred rows, and
      only the derived boolean crosses the wire."
     [topic-id]
     (into #{}
       (comp (mapcat (juxt :flashcards/question :flashcards/cloze))
         (filter some?)
         (map pc/normalize-for-match))
       (db/get-flashcards topic-id))))

#?(:clj
   (defn- card-entry
     "One card button's whole state: what to send, and whether the topic already
      holds it (row 6.9). `match-field` is the field the uniqueness index keys
      on — :c for a cloze card, :q for a basic one."
     [saved kind card-data match-field]
     {:kind kind
      :card-data [card-data]
      :saved? (contains? saved (pc/normalize-for-match (get card-data match-field)))}))

(defn word-panel-view*
  "Everything the word panel renders for one tapped word, derived server-side so
   the package never crosses the wire.

   Returns nil when the topic has no package or the verse is absent; the caller
   treats nil as still-loading, which is also what `e/Offload` returns while its
   thunk runs."
  [_refresh topic-id verse-n word-i]
  #?(:clj
     (when-let [pkg (and topic-id verse-n (get-poem-package* nil topic-id))]
       (when-let [verse (verse-by-n pkg verse-n)]
         (let [sources (sources-by-id pkg)
               for-word (filter #(and (= verse-n (:verse %)) (= word-i (:word %)))
                          (:notes pkg))
               gloss (mapv #(note-view sources %) (filter #(= "gloss" (:type %)) for-word))
               iraab (mapv #(note-view sources %) (filter #(= "iraab" (:type %)) for-word))
               saved (saved-field-set topic-id)]
           {:verse-n verse-n
            :word-i word-i
            :word-text (pc/word-text (:words verse) word-i)
            ;; The verse with the tapped word emphasised (row 4.2). The same
            ;; markup the two word cards carry as their first block, so what the
            ;; panel shows and what a card asks cannot drift.
            :verse-html (pc/verse-question-html verse word-i)
            :gloss gloss
            :iraab iraab
            :verse-card (card-entry saved "cloze" (pc/verse-card verse) :c)
            :meaning-card (card-entry saved "basic" (pc/meaning-card verse word-i gloss) :q)
            :iraab-card (card-entry saved "basic" (pc/iraab-card verse word-i iraab) :q)
            :extract-html (pc/verse-extract-html verse)
            :extract-title (pc/verse-extract-title verse)})))
     :cljs nil))

(defn verse-panel-view*
  "The verse view (rows 5.1 to 5.3): المعنى from الدرة first, then each
   commentary in `commentary-source-order`, each quoted with its chip."
  [_refresh topic-id verse-n]
  #?(:clj
     (when-let [pkg (and topic-id verse-n (get-poem-package* nil topic-id))]
       (when-let [verse (verse-by-n pkg verse-n)]
         (let [sources (sources-by-id pkg)
               of-verse (filter #(= verse-n (:verse %)) (:notes pkg))
               rank (into {} (map-indexed (fn [i s] [s i]) commentary-source-order))
               ;; A note whose source is not in the order list still shows, at
               ;; the end. Leaving it out would silently drop a book the package
               ;; carried, which the reader has no way to notice.
               by-rank (fn [n] [(get rank (:source_id n) (count commentary-source-order))
                                (str (:id n))])
               meanings (sort-by by-rank (filter #(= "meaning" (:type %)) of-verse))
               commentaries (sort-by by-rank (filter #(= "commentary" (:type %)) of-verse))]
           {:verse-n verse-n
            :verse-html (pc/verse-question-html verse nil)
            :sections (mapv #(note-view sources %) (concat meanings commentaries))})))
     :cljs nil))

(defn create-verse-extract!*
  "حفظ البيت (row 6.6): an extract child topic holding the verse.

   Goes through `content-toolbar-helpers/create-extract-topic-safe!`, the same
   entry the Extract toolbar button uses, so the new topic inherits the poem's
   source, copies its pins, and is sanitized the one way extracts are."
  [user-id topic-id html title]
  #?(:clj (helpers/create-extract-topic-safe! topic-id user-id html title)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(e/defn QuotedNote
  "One quoted block and its source chip. The HTML comes from the package, which
   the importer validated (format §4: no img, no /api/media), so it is written
   through `math/set-html!` like every other trusted display site rather than
   being re-sanitized per render."
  [note]
  (e/client
    (dom/div
      (dom/props {:class "poem-note"})
      (dom/div
        (dom/props {:class "poem-note__body" :dir "rtl" :lang "ar"})
        (math/set-html! dom/node (:html note)))
      (dom/span
        (dom/props {:class "poem-chip poem-chip--source"})
        (dom/text (:label note))))))

(e/defn NoteSection [title notes]
  (e/client
    (dom/section
      (dom/props {:class "poem-panel__section"})
      (dom/h4 (dom/props {:class "poem-panel__section-title"}) (dom/text title))
      (e/for [note (e/diff-by :id notes)]
        (QuotedNote note)))))

(e/defn AddCardButton
  "One card button. A click dispatches `:add-card`, which is a :queue command —
   the invocation lands in this tab's client queue and the pump runs the save,
   bumps :card-mutations and toasts. That is a local effect, so it needs no
   token here (CLAUDE.md, \"Selection that invokes a side effect\").

   `entry` is a `card-entry` map. The button is never rendered from a pending
   read: the caller gates the whole body on a non-nil view, so this props map
   can never go absent and null its own :class and :disabled."
  [label entry topic-id root-topic-id]
  (e/client
    (let [saved? (boolean (:saved? entry))]
      (dom/button
        (dom/props {:class (str "btn btn-sm " (if saved? "btn-secondary" "btn-primary"))
                    :type "button"
                    :disabled saved?})
        (dom/text (if saved? "محفوظ" label))
        (dom/On "click"
          (fn [_]
            (when-not saved?
              (bus/dispatch! :add-card
                {:topic-id topic-id
                 :root-topic-id root-topic-id
                 :kind (:kind entry)
                 :card-data (:card-data entry)})))
          nil)))))

(e/defn SaveVerseButton
  "حفظ البيت — an extract child topic holding the verse (row 6.6).

   A real server mutation, so it is token-sequenced: the `case` resolves the
   server call before `(t)` spends the token, and the :extract views bump only
   on success."
  [view user-id topic-id]
  (e/client
    (let [!error (atom nil)
          error (e/watch !error)]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
        (dom/text "حفظ البيت")
        ;; identity, not a mapper that returns the verse number: dom/On holds
        ;; the latest value and e/Token fires on a CHANGE, so two clicks on one
        ;; verse would emit the same number and the second would do nothing.
        ;; The event object is a fresh reference every time.
        (let [[t _] (e/Token (dom/On "click" identity nil))]
          (when t
            (let [r (e/server
                      (e/Offload #(create-verse-extract!* user-id topic-id
                                    (:extract-html view) (:extract-title view))))]
              (case r
                (if (:success r)
                  (do (reset! !error nil)
                      (case (e/server (commands/bump! user-id :extract)) (t)))
                  (do (reset! !error (or (:error r) "تعذر حفظ البيت"))
                      (t (or (:error r) "تعذر حفظ البيت")))))))))
      (when error
        (dom/span (dom/props {:class "poem-panel__error"}) (dom/text error))))))

(e/defn WordPanelBody [view user-id topic-id root-topic-id]
  (e/client
    (dom/div
      (dom/props {:class "poem-panel__body"})
      (dom/div
        (dom/props {:class "poem-panel__verse" :dir "rtl" :lang "ar"})
        (math/set-html! dom/node (:verse-html view)))
      (dom/div
        (dom/props {:class "poem-panel__word" :dir "rtl" :lang "ar"})
        (dom/text (str (:word-text view))))

      ;; One container, always exactly one sibling. The e/amb inside it is 0, 1
      ;; or 2 elements wide, and a sibling whose SIZE changes shifts every
      ;; sibling after it — which here would be the action row, rebuilt on every
      ;; word whose note coverage differs from the last one's.
      (dom/div
        (dom/props {:class "poem-panel__notes"})
        (if (and (empty? (:gloss view)) (empty? (:iraab view)))
          (dom/p
            (dom/props {:class "poem-panel__empty" :dir "rtl" :lang "ar"})
            (dom/text "لا يوجد شرح لهذه الكلمة في المصادر"))
          (e/amb
            (when (seq (:gloss view)) (NoteSection "الشرح" (:gloss view)))
            (when (seq (:iraab view)) (NoteSection "الإعراب" (:iraab view))))))

      (dom/div
        (dom/props {:class "poem-panel__actions"})
        (AddCardButton "بطاقة البيت" (:verse-card view) topic-id root-topic-id)
        (AddCardButton "بطاقة المعنى" (:meaning-card view) topic-id root-topic-id)
        (AddCardButton "بطاقة الإعراب" (:iraab-card view) topic-id root-topic-id)
        (SaveVerseButton view user-id topic-id)
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
          (dom/text "اشرح هذا البيت")
          (dom/On "click" (fn [_] (reset! !view :verse)) nil))))))

(e/defn VersePanelBody [view]
  (e/client
    (dom/div
      (dom/props {:class "poem-panel__body"})
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary poem-panel__back" :type "button"})
        (dom/text "‹ رجوع إلى الكلمة")
        (dom/On "click" (fn [_] (reset! !view :word)) nil))
      (dom/div
        (dom/props {:class "poem-panel__verse" :dir "rtl" :lang "ar"})
        (math/set-html! dom/node (:verse-html view)))
      (if (empty? (:sections view))
        (dom/p
          (dom/props {:class "poem-panel__empty" :dir "rtl" :lang "ar"})
          (dom/text "لا يوجد شرح لهذا البيت في المصادر"))
        (e/for [note (e/diff-by :id (:sections view))]
          (QuotedNote note))))))

(e/defn PoemPanelHint [message]
  (e/client
    (dom/p
      (dom/props {:class "poem-panel__empty" :dir "rtl" :lang "ar"})
      (dom/text message))))

(e/defn PoemWordPanel [user-id topic-id root-topic-id verse-n word-i]
  (e/client
    ;; The saved-state clock. Inline rather than wrapped in a helper fn:
    ;; `e/watch` is Electric, and Electric inside a plain `defn` is a compile
    ;; error ("Electric code inside a Clojure function"). Same shape as
    ;; topic-state's `scanning-pages` binding.
    ;;
    ;; :card-mutations is what `:add-card` declares in its :views, so a card
    ;; this panel just created flips its own button to محفوظ with no reload.
    (let [refresh (e/server (e/watch (us/get-atom user-id :card-mutations)))
          view (e/server (e/Offload #(word-panel-view* refresh topic-id verse-n word-i)))]
      (if (nil? view)
        (PoemPanelHint "…")
        (WordPanelBody view user-id topic-id root-topic-id)))))

(e/defn PoemVersePanel [user-id topic-id verse-n]
  (e/client
    (let [view (e/server (e/Offload #(verse-panel-view* nil topic-id verse-n)))]
      (if (nil? view)
        (PoemPanelHint "…")
        (VersePanelBody view)))))

(e/defn PoemPanel
  "The الشرح tab's body. Reads the shared selection; a selection belonging to
   another topic is ignored rather than rendered, so a stale atom cannot paint
   one poem's word over another's."
  [page-topic-id root-topic-id user-id]
  (e/client
    (let [selection (e/watch !selection)
          view-mode (e/watch !view)
          mine? (and (some? selection) (= page-topic-id (:topic-id selection)))]
      (dom/div
        (dom/props {:class "poem-panel"})
        (if-not mine?
          (PoemPanelHint "اضغط على كلمة في القصيدة لعرض شرحها")
          (if (= :verse view-mode)
            (PoemVersePanel user-id page-topic-id (:verse selection))
            (PoemWordPanel user-id page-topic-id root-topic-id
              (:verse selection) (:word selection))))))))
