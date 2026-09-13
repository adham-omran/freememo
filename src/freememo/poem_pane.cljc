(ns freememo.poem-pane
  "The read-only poem reader (spec rows 3.3 to 3.5).

   Mounts in ContentColumn in place of EditorPane, which is what makes a poem
   uneditable on the client: EditorPane owns the Quill instance and the auto-save
   that writes `topics.content` on every text-change, so a poem page has no
   `.ql-editor` at all (row 3.2.1). The server-side half of that guard lives in
   `update-topic-content!` and is not this namespace's.

   Two things about the rendering are contractual rather than stylistic.

   WORDS ARE RENDERED FROM `words`, NEVER BY RE-SPLITTING `sadr` OR `ajuz`.
   A note's `word` index addresses the package's own tokenization
   (`plans/poem-package-format.md` §3), so a second tokenization here would put
   a note on the wrong word — silently, and differently per verse.

   ONE DELEGATED CLICK HANDLER, not one per word. A معلقة is ~900 words, and a
   listener per span is 900 reactive nodes to mount and tear down. The handler
   sits on the container and resolves the target with `.closest`, the shape
   `assistant_evidence.cljc` uses for citation clicks."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.command-bus :as bus]
   [freememo.poem-panel :as panel]))

;; ---------------------------------------------------------------------------
;; Click target resolution
;; ---------------------------------------------------------------------------
;;
;; Reader conditionals at the TOP LEVEL, never inside the e/defn body: Electric
;; compiles a different signal count for CLJ and CLJS when a #? sits in a
;; reactive body, which raises ArrayIndexOutOfBoundsException at mount. The CLJ
;; arm exists only so the symbol resolves on that peer (CLAUDE.md), and
;; `assistant_evidence/clicked-citation` has the same pair for the same reason.

#?(:cljs
   (defn clicked-word
     "The {:verse n :word i} a click landed on, or nil. Reads the two data
      attributes the word span carries; a click on the gap between words, on a
      verse number, or on the header resolves to nil and does nothing."
     [event]
     (when-let [el (.closest (.-target event) "[data-word]")]
       (let [v (js/parseInt (.getAttribute el "data-verse") 10)
             w (js/parseInt (.getAttribute el "data-word") 10)]
         (when-not (or (js/isNaN v) (js/isNaN w))
           {:verse v :word w})))))

#?(:clj (defn clicked-word [_event] nil))

;; ---------------------------------------------------------------------------
;; Server-side views
;; ---------------------------------------------------------------------------

(defn poem-header
  "Header scalars for `pkg`: title, poet, بحر, verse count and the short name of
   the book the verse list came from. Derived from a package value already on
   the server, so five small strings cross the wire instead of the package."
  [pkg]
  #?(:clj
     (when pkg
       (let [m (:manifest pkg)
             src (first (filter #(= (:verse_source_id m) (:id %)) (:sources pkg)))]
         {:title (:title m)
          :poet (:poet m)
          :meter (:meter m)
          :verse-count (count (:verses pkg))
          :source-short (:short src)}))
     :cljs nil))

(defn verse-rows
  "The per-verse render input: the verse number and its word vector, nothing
   else. `sadr` and `ajuz` stay on the server — the reader renders words, and
   the cards that need the raw halves are built server-side in poem-panel."
  [pkg]
  #?(:clj (mapv (fn [v] {:n (:n v) :words (vec (:words v))}) (:verses pkg))
     :cljs []))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(e/defn PoemChip [label]
  (e/client
    (dom/span (dom/props {:class "poem-chip"}) (dom/text label))))

(e/defn PoemHeader [header]
  (e/client
    (dom/header
      (dom/props {:class "poem-reader__header"})
      (dom/h1 (dom/props {:class "poem-reader__title"}) (dom/text (str (:title header))))
      (dom/p (dom/props {:class "poem-reader__poet"}) (dom/text (str (:poet header))))
      (dom/div
        (dom/props {:class "poem-reader__chips"})
        (e/amb
          (when (:meter header) (PoemChip (str "البحر: " (:meter header))))
          (when (:verse-count header) (PoemChip (str (:verse-count header) " بيتًا")))
          (when (:source-short header) (PoemChip (str "المتن: " (:source-short header)))))))))

(e/defn PoemHalf
  "One hemistich. `words` is already filtered to this half, in package order.

   `active-i` is the tapped word's index when the SELECTION IS IN THIS VERSE and
   nil otherwise. Passing nil for every other verse is what keeps a tap from
   re-rendering all ~900 word spans: only the verse losing the selection and the
   verse gaining it see their argument change."
  [verse-n words active-i]
  (e/client
    (e/for [w (e/diff-by :i words)]
      (e/amb
        (dom/span
          (dom/props {:class (str "poem-word" (when (= (:i w) active-i) " poem-word--active"))
                      :data-verse (str verse-n)
                      :data-word (str (:i w))})
          (dom/text (:text w)))
        ;; A separate text node, because adjacent inline elements render with no
        ;; space between them and the space must not be part of the word's own
        ;; hit area or highlight.
        (dom/text " ")))))

(e/defn PoemVerseRow [row active-i]
  (e/client
    (let [words (:words row)]
      (dom/div
        (dom/props {:class "poem-verse"})
        (dom/div
          (dom/props {:class "poem-verse__half poem-verse__half--sadr"})
          (PoemHalf (:n row) (filterv #(= "sadr" (:half %)) words) active-i))
        (dom/div
          (dom/props {:class "poem-verse__half poem-verse__half--ajuz"})
          (PoemHalf (:n row) (filterv #(= "ajuz" (:half %)) words) active-i))
        (dom/div
          (dom/props {:class "poem-verse__num"})
          (dom/text (str (:n row))))))))

(e/defn PoemBody
  "The verse list. `rows` is a SERVER-sited differential collection, diffed on
   the server so one row crosses per verse and a re-read moves only the rows
   that changed — a nested (e/server …) inside the e/for would be a second
   transfer, a frame behind the count (CLAUDE.md)."
  [rows selection topic-id]
  (e/client
    (let [sel-verse (when (= topic-id (:topic-id selection)) (:verse selection))
          sel-word (when (= topic-id (:topic-id selection)) (:word selection))]
      (e/for [row rows]
        (PoemVerseRow row (when (= (:n row) sel-verse) sel-word))))))

(e/defn PoemPane
  "The whole reader. Reads ambient doc-context, as every other pane in
   ContentColumn does."
  []
  (e/client
    (let [topic-id dctx/page-topic-id
          selection (e/watch panel/!selection)
          ;; A server-form binding, NOT an e/defn call: the package would
          ;; otherwise materialize whole at the call boundary. Bound once and
          ;; read twice, so the reader costs one query.
          ;;
          ;; The refresh argument is a constant. A poem is read-only (row 3.2)
          ;; and its package row is written once, by the import transaction — so
          ;; there is no channel whose bump would mean new verses, and watching
          ;; :refresh would re-query the whole package on every unrelated
          ;; content mutation in the account.
          pkg (e/server (e/Offload #(panel/get-poem-package* 0 topic-id)))
          header (e/server (poem-header pkg))
          rows (e/server (e/diff-by :n (verse-rows pkg)))]

      ;; Leaving the reader drops the selection, so returning to a DIFFERENT
      ;; poem cannot open its panel on the previous poem's word. The panel also
      ;; checks the selection's :topic-id, so this is the cheap half of a guard
      ;; that does not depend on it.
      (e/on-unmount panel/clear-selection!)

      ;; The mounted reader IS the answer to "is this topic a poem" for
      ;; :show-poem-panel. The command's handler is a plain client fn with no
      ;; access to doc-context, and this pane mounts for exactly the topics that
      ;; have a package — so an absent key is the refusal, and a publisher that
      ;; forgot to retract would make the command lie (command-bus's context
      ;; contract).
      (bus/publish-ctx! {:poem-topic-id topic-id})
      (e/on-unmount #(bus/retract-ctx! [:poem-topic-id]))

      (dom/div
        (dom/props {:class "poem-reader" :dir "rtl" :lang "ar"})

        ;; One handler for every word in the poem.
        (dom/On "click"
          (fn [ev]
            (when-let [{:keys [verse word]} (clicked-word ev)]
              (panel/select-word! topic-id verse word)))
          nil)

        (if (nil? header)
          (dom/p
            (dom/props {:class "poem-reader__empty"})
            (dom/text "…"))
          (e/amb
            (PoemHeader header)
            (dom/div
              (dom/props {:class "poem-reader__verses"})
              (PoemBody rows selection topic-id))))))))
