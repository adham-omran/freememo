(ns freememo.card-modals
  "Extracted modal components for ocr-page — kept separate to stay below JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.modal-shell :as modal]
   [clojure.string :as str]
   [freememo.typeahead :refer [Suggest]]
   [freememo.segmented :as seg]
   [freememo.pre-prompt-mode :as ppm]
   [freememo.quill-field :refer [QuillField flush-syntax-tokens!]]
   [freememo.anki-note-actions :refer [AnkiNoteActions]]
   [freememo.card-history-modal :refer [CardHistoryModal]]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.cloze :as cloze]
   [freememo.icons :as icons]
   #?(:clj [freememo.toasts :as toasts])
   [freememo.optimistic :as opt]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.settings :as settings])))

(defn sanitize-card-field
  "Sanitize a card field's HTML server-side. Uses clean-html-fragment: a card is
   a piece cut out of a document, so it keeps no heading `id` — the anchors that
   pointed at those headings stayed in the parent. Unlike clean-html-llm this is
   the user-trust variant, so user-pasted <img> tags survive (pin-aware
   authoring may use them)."
  [html]
  #?(:clj (when html (cleaner/clean-html-fragment html))
     :cljs html))

(defn quill-html->items
  "Parse a QuillField's HTML into an ordered vector of list items — the inner
   HTML of each block line (<p> or <li>), trimmed, dropping blank lines (an
   empty Quill line is <p><br></p>). Inline formatting and <img> within a line
   are preserved. CLJS only; CLJ returns []."
  [html]
  #?(:cljs
     (let [tmp (.createElement js/document "div")]
       (set! (.-innerHTML tmp) (or html ""))
       (->> (array-seq (.querySelectorAll tmp "p, li"))
         (keep (fn [el]
                 (let [inner   (str/trim (.-innerHTML el))
                       text    (str/trim (.-textContent el))
                       has-img (pos? (.-length (.querySelectorAll el "img")))]
                   (when (and (not (str/blank? inner))
                           (or (not (str/blank? text)) has-img))
                     inner))))
         vec))
     :clj []))

(defn blank-html?
  "True when QuillField HTML has no visible content — no text and no <img>.
   An empty Quill editor renders as <p><br></p>, so a bare str/blank? misses it
   (same content test quill-html->items uses per line)."
  [html]
  #?(:cljs
     (let [tmp (.createElement js/document "div")]
       (set! (.-innerHTML tmp) (or html ""))
       (and (str/blank? (.-textContent tmp))
            (zero? (.-length (.querySelectorAll tmp "img")))))
     :clj (str/blank? html)))

(defn ol-items->html
  "Build QuillField mount HTML from a stored items vector — one <p> block per
   item so Quill renders each item on its own line."
  [items]
  (apply str (map #(str "<p>" % "</p>") items)))

(defn ol-before-int
  "Parse the 'context lines before' input to a non-negative int (default 1)."
  [s]
  #?(:cljs (let [n (js/parseInt (str s) 10)] (if (js/isNaN n) 1 (max 0 n)))
     :clj (max 0 (or (parse-long (str s)) 1))))

(defn composed-card
  "The authored card as plain comparable data — the identity Create copy… tests
   against its source, and the same value the dirty test reads.

   Pre : `kind` is \"basic\", \"cloze\" or \"overlapping\". `fields` is a
         card-fields map (see below). BOTH sides of a comparison MUST be built
         here, or they are not comparable.
   Post: a map holding no atoms and no editor handles, so `=` decides it. Only
         the keys the kind actually uses appear, so a stale value in an unused
         field cannot read as a difference.
   Invariant: no HTML normalization. A difference Quill introduced when it
         mounted the source counts as an edit and unblocks Save, which is the
         safe direction — the duplicate then reaches the flashcard uniqueness
         indexes, and run-command! :add-card reports the drop rather than
         claiming a card was added. The guard and that report are two layers
         over one hazard, and neither is load-bearing alone.

   A card-fields map is {:question :answer :cloze :ol-question :ol-items-html
   :ol-before :ol-reveal}. It names BOTH text fields rather than one 'primary',
   because the two modals disagree about which is which: the edit modal keeps
   question and cloze in separate atoms, while the add modal shares one across
   the Basic/Cloze switch and writes it to both keys. Reading the field the
   kind selects here is what lets one shape serve both."
  [kind {:keys [question answer cloze ol-question ol-items-html ol-before ol-reveal]}]
  (case kind
    "overlapping" {:kind kind :question ol-question
                   :items (quill-html->items ol-items-html)
                   :before (ol-before-int ol-before)
                   :reveal? (boolean ol-reveal)}
    "basic" {:kind kind :primary question :answer answer}
    {:kind kind :primary cloze :answer answer}))

(defn copy-prefill
  "What AddCardModal must be seeded with to open a copy of an authored card.
   Pre : `fields` is a card-fields map (see composed-card).
   Post: the :prefill shape AddCardModal reads — :primary/:answer for the two
         text kinds, :ol for overlapping."
  [kind fields]
  (case kind
    "overlapping" {:ol {:question (:ol-question fields)
                        :items (quill-html->items (:ol-items-html fields))
                        :before (ol-before-int (:ol-before fields))
                        :reveal? (boolean (:ol-reveal fields))}}
    "basic" {:primary (:question fields) :answer (:answer fields)}
    {:primary (:cloze fields) :answer (:answer fields)}))

(defn flush-editors!
  "Force-flush each [editor-atom target-atom] pair's Quill syntax-highlight
   spans into the DOM and reset! the resulting HTML into target-atom.
   Shared by EditCardModal's and AddCardModal's submission handlers — see
   flush-syntax-tokens! (freememo.quill-field) for why this MUST run at
   click time rather than relying on QuillField's on-change."
  [pairs]
  (doseq [[!editor !target] pairs]
    (when-let [html (flush-syntax-tokens! @!editor)]
      (reset! !target html))))

(defn add-card-command
  "Build one validated Add-card command from the current add-modal fields.
   Pre: field values have been flushed from their Quill editors. Post: a command
   vector, or an ExceptionInfo that Forms5 can present without an enqueue."
  [kind topic-id root-topic-id fields]
  (case kind
    "overlapping"
    (let [items (quill-html->items (:ol-items-html fields))]
      (if (empty? items)
        (ex-info "Add at least one list item" {})
        [`Add-card {:kind "overlapping" :topic-id topic-id :root-topic-id root-topic-id
                    :ol {:question (:ol-question fields) :items items
                         :settings {:before (ol-before-int (:ol-before fields))
                                    :after 0
                                    :reveal-all? (:ol-reveal fields)}}}]))
    "basic"
    (if (blank-html? (:question fields))
      (ex-info "Question can't be empty" {})
      [`Add-card {:kind "basic" :topic-id topic-id :root-topic-id root-topic-id
                  :primary (:question fields) :answer (:answer fields)}])
    (if-let [error (cloze/validate (:cloze fields))]
      (ex-info error {})
      [`Add-card {:kind "cloze" :topic-id topic-id :root-topic-id root-topic-id
                  :primary (:cloze fields) :answer (:answer fields)}])))

(defn switch-add-card-kind!
  "Move one add-modal draft between card-kind layouts.
   Pre: every argument atom belongs to the same modal. Post: Basic and
   Overlapping exchange their raw front/back fields before the target mounts."
  [!kind !primary !answer !ol-question !ol-items-html next-kind]
  (let [current-kind @!kind]
    (when (not= current-kind next-kind)
      (when (or (= current-kind "overlapping") (= next-kind "overlapping"))
        (if (= next-kind "overlapping")
          (do
            (reset! !ol-question @!primary)
            (reset! !ol-items-html @!answer))
          (do
            (reset! !primary @!ol-question)
            (reset! !answer @!ol-items-html))))
      (reset! !kind next-kind))))

(defn flip-basic-card-fields!
  "Swap the Basic card's front and back, then remount its editors."
  [!primary !answer !draft-revision]
  (let [question @!primary]
    (reset! !primary @!answer)
    (reset! !answer question)
    (swap! !draft-revision inc)))

(defn clear-add-card-draft!
  "Clear a successful Add & Keep draft according to its frozen side.
   Post: :front retains the first editable field; :back retains the second;
   :both retains both fields and settings. Other modes reset Overlapping settings."
  [freeze-side kind !primary !answer !ol-question !ol-items-html !ol-before !ol-reveal]
  (if (= kind "overlapping")
    (case freeze-side
      :both nil
      :front (reset! !ol-items-html "")
      :back (reset! !ol-question "")
      (do (reset! !ol-question "") (reset! !ol-items-html "")))
    (case freeze-side
      :both nil
      :front (reset! !answer "")
      :back (reset! !primary "")
      (do (reset! !primary "") (reset! !answer ""))))
  (when (not= :both freeze-side)
    (reset! !ol-before "1")
    (reset! !ol-reveal true)))

(defn add-command-card-data
  "Translate one parsed Add-card command into the optimistic card shape."
  [[_ {:keys [kind primary answer ol]}]]
  (case kind
    "basic" [{:q primary :a answer}]
    "overlapping" [ol]
    [(cond-> {:c primary}
       (and answer (not (str/blank? answer)))
       (assoc :a answer))]))

;; ---------------------------------------------------------------------------
;; Modal Tab navigation — Tab moves focus editor→editor→Add→Cancel instead of
;; letting Quill insert a tab character. Toolbar buttons are excluded (they
;; carry ql-* classes, not .btn); the Basic/Cloze radios are not tab-stops.
;; ---------------------------------------------------------------------------

#?(:cljs
   (defn modal-tab-stops
     "Ordered focus targets for `container` (the .card-modal-inner div): the
      rendered Quill editors in DOM order, then the Add then Cancel button."
     [container]
     (let [editors (array-seq (.querySelectorAll container ".ql-editor"))
           save    (.querySelector container "button.btn-primary")
           cancel  (.querySelector container "button.btn-secondary")]
       (vec (concat editors (remove nil? [save cancel]))))))

#?(:cljs
   (defn handle-modal-tab!
     "Advance focus one tab-stop. Shift+Tab reverses; both ends wrap. No-op when
      there are no stops, and when focus is inside an editor — Tab there
      indents the line (`a11y/install-tab-indent!`), and Escape is the way out.
      Pre : `e` is a keydown event on `container`.
      Post: on Tab outside an editor, document.activeElement is the next/prev
            stop and the event is prevented and stopped. The stop that holds
            focus is the one equal to, containing, or contained in the active
            element — so after Escape focuses an editor's Quill container,
            Tab moves to the stop after that editor."
     [container e]
     (when (and (= (.-key e) "Tab")
                (not (some-> (.-activeElement js/document) (.closest ".ql-editor"))))
       (let [stops (modal-tab-stops container)
             n     (count stops)]
         (when (pos? n)
           (.preventDefault e)
           (.stopPropagation e)
           (let [active (.-activeElement js/document)
                 idx    (reduce (fn [_ i]
                                  (let [s (nth stops i)]
                                    (when (or (= s active)
                                              (.contains s active)
                                              (.contains active s))
                                      (reduced i))))
                          nil (range n))
                 delta  (if (.-shiftKey e) -1 1)
                 nxt    (if idx
                          (mod (+ idx delta) n)
                          (if (.-shiftKey e) (dec n) 0))]
             (.focus (nth stops nxt))))))))

#?(:cljs
   (defn attach-modal-tab-nav!
     "Install the capture-phase Tab navigator on `container`, returning a 0-arg
      cleanup that removes the listener. The navigator yields while focus is in
      an editor, so Quill's own keydown handler receives Tab there."
     [container]
     (let [handler (fn [e] (handle-modal-tab! container e))]
       (.addEventListener container "keydown" handler true)
       (fn [] (.removeEventListener container "keydown" handler true)))))
#?(:clj (defn attach-modal-tab-nav! [_container] nil))

#?(:cljs
   (defn attach-submit-flush!
     "Install a capture-phase `submit` listener on `container` that runs `flush!`
      (Quill syntax-token flush → field atoms) BEFORE any bubble/target-phase
      submit handler reads the form. Returns a 0-arg cleanup.

      Capture phase on an ANCESTOR of the <form> is required: the Forms5 submit
      handler (Form!*) and the form itself are the event target, so a listener
      on `container` (a strict ancestor) is the only placement that reliably
      precedes them — sibling registration order across concurrent Electric
      mounts is not guaranteed. Running flush here makes the flushed HTML live in
      the field atoms before Form!*'s :Parse stamps the commit, so the committed
      command carries the syntax-tokenized content."
     [container flush!]
     (let [handler (fn [_] (flush!))]
       (.addEventListener container "submit" handler true)
       (fn [] (.removeEventListener container "submit" handler true)))))
#?(:clj (defn attach-submit-flush! [_container _flush!] nil))


;; Pre-prompt dialog
;; state map keys: :!show :!prompt-submit :!pre-prompt :!prompt-history
;;                 :!history-save-trigger :captured-selection :prompt-dialog-kind
;;                 :kind-label :initial-mode
;;
;; The strictness segments write a LOCAL atom and never touch the server: the
;; dialog's choice governs this one generation, and the persisted default lives
;; in Settings (PrePromptModeField). `initial-mode` seeds it and `kind-label`
;; arrives pre-formatted because content-toolbar-helpers owns card-type
;; capitalization — the label is not re-derived here.
(e/defn PromptDialog [state]
  (e/client
    (let [{:keys [!show !prompt-submit !pre-prompt !prompt-history
                  !history-save-trigger captured-selection prompt-dialog-kind
                  kind-label initial-mode]} state
          pre-prompt-value (e/watch !pre-prompt)
          !local-prompt (atom pre-prompt-value)
          local-prompt (e/watch !local-prompt)
          !mode (atom initial-mode)
          mode (e/watch !mode)
          SelectMode (e/fn [t value]
                       (e/client (reset! !mode value) (t)))
          one-card? (ppm/single-card? mode prompt-dialog-kind local-prompt)
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape (fn [] (reset! !show false))
          (str "Generate " kind-label " Cards"))
        (dom/On "keydown"
          (fn [e] (modal/mod-enter-submit! e !primary-btn))
          nil)
        (dom/div
          (dom/props {:style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :width "500px" :max-width "90%" :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e] (modal/drag-modal-by-title! e))
            nil)
          (dom/h3
            (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                :border-bottom "1px solid var(--color-border)" :font-size "18px"}})
            (dom/text "Generate " kind-label " Cards"))
          (dom/div
            (dom/props {:style {:margin-bottom "20px"}})
            (dom/label
              (dom/props {:style {:display "block" :margin-bottom "var(--sp-2)" :font-size "14px"}})
              (dom/text "Pre-prompt (will be added to the system prompt):"))
            ;; Suggest, not Typeahead: a novel pre-prompt is the common case, so
            ;; the typed text must be the value. History is a shortcut, not the
            ;; set of legal inputs.
            (let [prompt-history (e/watch !prompt-history)]
              (Suggest !local-prompt (vec (take 20 prompt-history))
                "e.g., Focus on accounting terminology..." true)))
          (dom/div
            (dom/props {:style {:margin-bottom "20px"}})
            (dom/label
              (dom/props {:style {:display "block" :margin-bottom "var(--sp-2)" :font-size "14px"}})
              (dom/text "Strictness:"))
            (dom/div
              (dom/props {:class "segmented" :role "radiogroup" :aria-label "Pre-prompt strictness"})
              (seg/Segment mode ppm/guide (ppm/label ppm/guide) SelectMode)
              (seg/Segment mode ppm/clarify (ppm/label ppm/clarify) SelectMode)
              (seg/Segment mode ppm/verbatim (ppm/label ppm/verbatim) SelectMode))
            (dom/div
              (dom/props {:class "segmented-description" :style {:margin-top "var(--sp-2)"}})
              (dom/text (ppm/description mode)
                (if one-card? " Generates 1 card." ""))))
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-3)"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:order "1"}})
              (reset! !primary-btn dom/node)
              (dom/text "Generate")
              (dom/On "click"
                (fn [_]
                  (reset! !pre-prompt local-prompt)
                  (when (seq local-prompt)
                    (let [new-history (->> (cons local-prompt @!prompt-history)
                                        (distinct)
                                        (take 50)
                                        (vec))]
                      (reset! !prompt-history new-history)
                      (reset! !history-save-trigger new-history)))
                  (reset! !prompt-submit {:selection captured-selection
                                          :pre-prompt local-prompt
                                          :kind prompt-dialog-kind
                                          :pre-prompt-mode @!mode})
                  (reset! !show false))
                nil))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil))))))))

;; Overlapping-cloze authoring pane — shared by Add + Edit
(e/defn OverlappingPane
  "Authoring inputs for an overlapping-cloze card, shared by Add + Edit. The
   question and the list are QuillField editors (rich text + toolbar + images)
   like Basic/Cloze; the list's HTML is parsed into items on save via
   quill-html->items. question-key/items-key MUST each be unique per instance
   (QuillField frame isolation)."
  [!question !question-editor question-html question-key
   !items-html !items-editor items-html items-key !before !reveal modal-font]
  (e/client
    (let [before (e/watch !before)
          reveal (e/watch !reveal)]
      (dom/div
        (dom/label (dom/text "Question:"))
        (dom/div (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
          (QuillField question-html
            (fn [html] (reset! !question html))
            "e.g. What are the steps of X?" question-key !question-editor nil true))
        (dom/label (dom/text "List items (one per line):"))
        (dom/div (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
          (QuillField items-html
            (fn [html] (reset! !items-html html))
            "One item per line…" items-key !items-editor nil nil))
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center"
                              :gap "var(--sp-4)" :flex-wrap "wrap" :font-size "13px"}})
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)"}})
            (dom/text "Context lines before:")
            (dom/input
              (dom/props {:type "number" :min "0" :max "5" :value (str before)
                          :style {:width "3.5em"}})
              (dom/On "input" (fn [e] (reset! !before (-> e .-target .-value))) nil)))
          (dom/label
            (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)"}})
            (dom/input
              (dom/props {:type "checkbox" :checked reveal})
              (dom/On "change" (fn [e] (reset! !reveal (-> e .-target .-checked))) nil))
            (dom/text "Reveal-all card")))))))

(defn edit-validation-error
  "The message the edit modal must show instead of saving, or nil to proceed.
   Pre : `fields` is a card-fields map (see composed-card). `items` is already
         parsed, because that parse is CLJS DOM work the caller does once and
         hands to both this and edit-fields.
   Post: nil, or a string a user can act on."
  [kind fields items]
  (cond
    (= kind "overlapping") (when (empty? items) "Add at least one list item")
    (= kind "cloze")       (cloze/validate (:cloze fields))
    (= kind "basic")       (when (blank-html? (:question fields)) "Question can't be empty")))

(defn edit-fields
  "The edit modal's live values as the plain map save-card-edit!* takes.
   Pre : `fields` is a card-fields map, `items` its parsed list.
   Post: for overlapping, an :ol map carrying the expanded settings; otherwise
         the three raw HTML fields, unsanitized — sanitation belongs at the
         call that persists, so the text crosses the wire once."
  [kind fields items]
  (if (= kind "overlapping")
    {:ol {:question (:ol-question fields) :items items
          :settings {:before (ol-before-int (:ol-before fields))
                     :after 0 :reveal-all? (:ol-reveal fields)}}}
    {:question (:question fields) :answer (:answer fields) :cloze (:cloze fields)}))

(defn save-card-edit!*
  "Persist the edit modal's current fields to card `card-id`.

   Pre : `kind` is \"basic\", \"cloze\" or \"overlapping\", `fields` came from
         edit-fields, and edit-validation-error already returned nil.
   Post: {:success true} or {:success false :error msg}. Both update paths log
         the superseded rendition, so a card_versions row is written.
   Invariant: Save and Create copy… MUST persist through here. Create copy…
         commits the card first, and a second copy of this sequence could
         sanitize or shape a field differently from Save without any test
         noticing — the two write the same row.

   Body-level reader conditional, not whole-form: the bare symbol is named
   inside (e/server (e/Offload #(…))) in a reactive body, which CLJS compiles,
   so the var must exist on both peers."
  [user-id card-id kind fields]
  #?(:clj
     (if (= kind "overlapping")
       (cards/update-overlapping-card user-id card-id (:ol fields))
       (let [clean-q (sanitize-card-field (:question fields))
             clean-a (sanitize-card-field (:answer fields))
             clean-c (sanitize-card-field (:cloze fields))
             updated (if (= kind "basic")
                       {:question clean-q :answer clean-a}
                       (cond-> {:cloze clean-c}
                         (and clean-a (not (str/blank? clean-a)))
                         (assoc :answer clean-a)))]
         (cards/update-card user-id card-id updated)))
     :cljs nil))

(e/defn CopyCardButton
  "Create copy… — commit the card being edited, then hand its content to a
   fresh Copy Card modal.

   Pre : `live-fields` is a card-fields map of the modal's CURRENT values.
         `flush!` writes the Quill syntax-tokenized HTML into the atoms behind
         them, and MUST run in the click handler, before anything reads them.
         `dirty?` says whether those values differ from what the editors
         mounted with.
   Post: !editing-card holds a {:mode :copy} request. That single reset! is
         both halves of the transition — the edit modal unmounts and the copy
         modal mounts — so no frame has both open, and none has neither.
   Invariant: the original is committed FIRST, and only when dirty. A clean
         form skips the write because cards/update-card versions every call,
         and a no-op version entry per copy would bury the card's real history.
   Blame: a caller that passes stale live-fields (no flush!) copies whatever
         the atoms held before the click, which for a code block is the
         untokenized HTML.

   Its own e/defn rather than three more buttons inline: card_modals.cljc is
   the file the 'Method code too large!' guidance was written about, and each
   e/defn compiles to its own JVM method."
  [!editing-card user-id live-fields dirty? flush! !save-anki-remarks]
  (e/client
    (let [editing-card (e/watch !editing-card)
          card-id (:id editing-card)
          kind (:kind editing-card)
          ;; Derived from live-fields, so it tracks the flush. An e/on-unmount
          ;; closure reads the LATEST value of a signal at fire time, which is
          ;; what makes reading it from the callback below correct.
          copy-request {:mode :copy
                        :kind kind
                        :topic-id (:topic-id editing-card)
                        :root-topic-id (:root-topic-id editing-card)
                        :prefill (copy-prefill kind live-fields)
                        :source (composed-card kind live-fields)}
          [click-event btn-node]
          (dom/button
            (dom/props {:class "btn btn-secondary" :style {:order "0"}
                        :data-tooltip "Save this card, then start a new one from its content"})
            (dom/text "Create copy…")
            ;; The palette entry is tab-scoped and cannot say "a card editor is
            ;; open", so this registration IS the modal-scope guard. Retracting
            ;; on unmount is required: a stale invoker makes :copy-card look
            ;; available with nothing to click.
            ;;
            ;; Keyed by node, because :copy-card has three publishers and two of
            ;; them CAN be mounted together — a score topic holding a basic card
            ;; shows the score toolbar's Duplicate while this modal is open.
            ;; Unkeyed, whichever closed first would retract the command out from
            ;; under the other.
            (let [node dom/node]
              (bus/publish-invoker! :copy-card node (fn [] (.click node)))
              (e/on-unmount (fn [] (bus/retract-invoker! :copy-card node))))
            [(dom/On "click" (fn [e] (flush!) e) nil)
             dom/node])
          [t ?error] (e/Token click-event)]
      (dom/props btn-node {:disabled (some? t) :aria-busy (some? t)})
      (when ?error
        (dom/div (dom/props {:style {:order "-1" :margin-right "auto"
                                     :color "var(--color-danger-text)" :font-size "12px"}})
          (dom/text "Error: " ?error)))
      (when t
        (if dirty?
          (let [items (quill-html->items (:ol-items-html live-fields))
                validation-error (edit-validation-error kind live-fields items)]
            (if validation-error
              ;; The copy is refused along with the save. Opening a copy of a
              ;; card whose own edit does not validate would strand the invalid
              ;; edit with nowhere to go.
              (t validation-error)
              (let [result (e/server
                             (e/Offload
                               #(save-card-edit!* user-id card-id kind
                                  (edit-fields kind live-fields items))))]
                (if (:success result)
                  (do (e/on-unmount (fn [] (reset! !editing-card copy-request)))
                      (case (e/client ((deref !save-anki-remarks)))
                        (case (e/server (commands/bump! user-id :edit-card))
                          (t))))
                  (t (:error result))))))
          (do (e/on-unmount (fn [] (reset! !editing-card copy-request)))
              (case (e/client ((deref !save-anki-remarks)))
                (t))))))))

(e/defn EditCardModal [!editing-card user-id]
  (e/client
    (let [editing-card (e/watch !editing-card)
          card-id (:id editing-card)
          kind (:kind editing-card)
          init-q (or (:question editing-card) "")
          init-a (or (:answer editing-card) "")
          init-c (or (:cloze editing-card) "")
          init-ol (:overlapping editing-card)
          anki-note-id (:anki-note-id editing-card)
          ;; Bound once and used three ways: to seed the atoms, to mount the
          ;; QuillFields, and to build init-shape below. All three MUST read
          ;; the same values, or the dirty test compares the editors against
          ;; something they were never given.
          init-ol-question (or (:question init-ol) "")
          init-ol-items-html (ol-items->html (:items init-ol))
          init-ol-before (str (get-in init-ol [:settings :before] 1))
          init-ol-reveal (get-in init-ol [:settings :reveal-all?] true)
          !question (atom init-q)
          !answer (atom init-a)
          !cloze (atom init-c)
          !ol-question (atom init-ol-question)
          !ol-question-editor (atom nil)
          !ol-items-html (atom init-ol-items-html)
          !ol-items-editor (atom nil)
          !ol-before (atom init-ol-before)
          !ol-reveal (atom init-ol-reveal)
          !q-editor (atom nil)
          !a-editor (atom nil)
          !c-editor (atom nil)
          !save-anki-remarks (atom (fn [] nil))
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)
          ol-question (e/watch !ol-question)
          ol-items-html (e/watch !ol-items-html)
          ol-before (e/watch !ol-before)
          ol-reveal (e/watch !ol-reveal)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")
          ;; One derivation, two jobs. init-shape is what the editors were
          ;; mounted with and live-shape is what they hold now, so their
          ;; inequality IS the dirty test — and live-shape is also the identity
          ;; Create copy… hands the copy modal to guard against. Deriving both
          ;; through composed-card is what makes them comparable at all.
          init-fields {:question init-q :answer init-a :cloze init-c
                       :ol-question init-ol-question :ol-items-html init-ol-items-html
                       :ol-before init-ol-before :ol-reveal init-ol-reveal}
          live-fields {:question question :answer answer :cloze cloze
                       :ol-question ol-question :ol-items-html ol-items-html
                       :ol-before ol-before :ol-reveal ol-reveal}
          dirty? (not= (composed-card kind init-fields)
                   (composed-card kind live-fields))
          ;; Flushing is the Quill syntax module's tokenized HTML landing in the
          ;; field atoms, and BOTH buttons that read those atoms must do it —
          ;; see flush-syntax-tokens!. Bound once so the two cannot list
          ;; different editors.
          flush! (fn []
                   (flush-editors! [[!q-editor !question] [!a-editor !answer] [!c-editor !cloze]
                                    [!ol-items-editor !ol-items-html] [!ol-question-editor !ol-question]]))
          !primary-btn (atom nil)
          !history-open? (atom false)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape (fn [] (reset! !editing-card nil))
          (str "Edit " (cond (= kind "overlapping") "Overlapping" (= kind "basic") "Basic" :else "Cloze") " Card"))
        (dom/On "keydown"
          (fn [e] (modal/mod-enter-submit! e !primary-btn))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e] (modal/drag-modal-by-title! e))
            nil)
          (let [cleanup (attach-modal-tab-nav! dom/node)]
            (e/on-unmount (fn [] (when cleanup (cleanup)))))
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text "Edit " (cond (= kind "overlapping") "Overlapping"
                                (= kind "basic") "Basic" :else "Cloze") " Card"))
          (cond
            (= kind "overlapping")
            (OverlappingPane !ol-question !ol-question-editor ol-question [:edit-ol-q card-id]
              !ol-items-html !ol-items-editor ol-items-html [:edit-ol card-id]
              !ol-before !ol-reveal modal-font)
            (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField init-q
                  (fn [html] (reset! !question html))
                  "Question..." [:edit-q card-id] !q-editor nil true))
              (dom/label (dom/text "Answer:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField init-a
                  (fn [html] (reset! !answer html))
                  "Answer..." [:edit-a card-id] !a-editor nil nil)))
            :else
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField init-c
                  (fn [html] (reset! !cloze html))
                  "Cloze text with {{c1::deletion}}..." [:edit-c card-id] !c-editor true true))
              (dom/label (dom/text "Back Extra:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField init-a
                  (fn [html] (reset! !answer html))
                  "Optional back extra..." [:edit-be card-id] !a-editor nil nil))))
          (dom/div
            (dom/props {:style {:margin-top "var(--sp-3)"}})
            (AnkiNoteActions anki-note-id !save-anki-remarks))
          (dom/div
            (dom/props {:style {:display "flex" :flex-wrap "wrap" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
            (let [[click-event btn-node]
                  (dom/button
                    (dom/props {:class "btn btn-primary" :style {:order "1"}})
                    (reset! !primary-btn dom/node)
                    (dom/text "Save")
                    ;; Force the syntax module to flush its hljs-* spans into the
                    ;; DOM, then read the post-flush innerHTML and reset! the
                    ;; matching atom directly (flush-editors!). Quill 2.0.3's
                    ;; syntax module does NOT fire any observable text-change for
                    ;; its formatAt pass, so on-change cannot deliver the
                    ;; tokenized HTML; we MUST capture it via the flush return
                    ;; value here. The vector is the button's last form, so the
                    ;; button returns [click-event node] for the e/Token below —
                    ;; letting the error render beside the buttons (not inside
                    ;; this blue one) and letting the button gate its own
                    ;; disabled/aria-busy state on the resulting token.
                    [(dom/On "click" (fn [e] (flush!) e) nil)
                     dom/node])
                  [t ?error] (e/Token click-event)]
              ;; Disable + aria-busy while a Save is in flight — closes the
              ;; double-submit hole (a second click while the first commit is
              ;; still pending would otherwise fire a second concurrent e/server
              ;; round-trip; e/Token alone does not prevent that).
              (dom/props btn-node {:disabled (some? t) :aria-busy (some? t)})
              (when ?error
                (dom/div (dom/props {:style {:order "-1" :margin-right "auto" :color "var(--color-danger-text)" :font-size "12px"}})
                  (dom/text "Error: " ?error)))
              (when t
                ;; Sanitation stays inside save-card-edit!*, at the call that
                ;; persists. Three separate (e/server (sanitize-card-field …))
                ;; bindings sent each field to the server, returned the cleaned
                ;; HTML to the client, and then sent the assembled map back —
                ;; four round trips carrying the same text.
                (let [items (quill-html->items ol-items-html)
                      validation-error (edit-validation-error kind live-fields items)]
                  (if validation-error
                    (t validation-error)
                    (let [result (e/server
                                   (e/Offload
                                     #(save-card-edit!* user-id card-id kind
                                        (edit-fields kind live-fields items))))]
                      (if (:success result)
                        (do (e/on-unmount #(reset! !editing-card nil))
                            (case (e/client ((deref !save-anki-remarks)))
                              (case (e/server (commands/bump! user-id :edit-card))
                                (t))))
                        (t (:error result))))))))
            (dom/button
              (dom/props {:class "btn btn-secondary" :style {:order "0"}
                          :data-tooltip "View previous versions of this card"})
              (dom/text "History")
              (dom/On "click" (fn [_] (reset! !history-open? true)) nil))
            ;; Between History and Cancel. Save carries :order "1" and so stays
            ;; rightmost whatever the DOM order of the :order "0" group is.
            (CopyCardButton !editing-card user-id live-fields dirty? flush! !save-anki-remarks)
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !editing-card nil)) nil)))))
      ;; Sibling of the overlay div above, NOT a descendant of it — the overlay
      ;; is pointer-events:none (a transparent, non-blocking layer), and that
      ;; property inherits: nested, the history modal's own backdrop and Close
      ;; button became un-hit-testable (elementFromPoint at Close returned the
      ;; page BEHIND the overlay) and its Escape bubbled into the overlay's
      ;; ModalEscape, discarding the card edit. CardHistoryModal also sets
      ;; pointer-events:auto on its own backdrop; both are load-bearing — this
      ;; placement fixes the Escape bubbling, that prop makes the modal
      ;; hit-testable in any mount context. Both overlays are z-index 1000, so
      ;; this call must stay LAST in the body: later DOM order paints on top,
      ;; and a non-final body form's value is unused.
      (CardHistoryModal [:card card-id] user-id !history-open?))))


#?(:clj
   (defn insert-flashcards-safe! [rows]
     (try
       (db/insert-flashcards! rows)
       {:success true}
       (catch Exception e
         {:success false :error (.getMessage e)}))))

;; Optimistic add-card effect (freememo.optimistic). Pre: payload has
;; :topic-id :root-topic-id :kind :card-data. Post: cards saved + success toast,
;; or error toast. Effect + toast only, returning the outcome map — the client
;; dispatcher flips the overlay entry (:confirmed with real ids, or :error) and
;; run-command-effect! bumps the registry :views.
;;
;; An all-duplicate save is reported as a FAILURE here, though save-cards calls
;; it a success. The two are not in conflict: save-cards serves batch generate,
;; where re-proposing a card the topic already holds is normal and inserting
;; nothing is not an error. This path submits exactly one card at a user's
;; request, so "nothing was inserted" is the whole outcome and must reach them.
;; Without this branch the modal toasts "Card added", closes, and leaves no
;; card — the shape Create copy… would hit on every unedited copy, because the
;; flashcard uniqueness indexes key on (topic_id, kind, md5(question|cloze)).
#?(:clj
   (defmethod opt/run-command! :add-card [user-id {:keys [payload]}]
     (let [{:keys [topic-id root-topic-id kind card-data]} payload
           ;; Sanitize here, at the call that persists. The modal enqueues the
           ;; typed HTML so the optimistic row paints with no round trip.
           ;; Overlapping cards stay verbatim, matching prior behavior.
           card-data (if (= kind "overlapping")
                       card-data
                       (mapv (fn [m]
                               (reduce (fn [acc k]
                                         (if (contains? acc k)
                                           (update acc k sanitize-card-field)
                                           acc))
                                 m [:q :a :c]))
                         card-data))
           ;; bake? false — the Add-Card modal already inlined pinned images
           ;; into card-data via pins-prefill-html; baking would duplicate them.
           result (cards/save-cards user-id topic-id root-topic-id kind card-data false)
           dropped-every-row? (and (:success result)
                                (empty? (:ids result))
                                (pos? (or (:skipped result) 0)))
           duplicate-msg "That card already exists in this topic"]
       (cond
         dropped-every-row?
         (do (toasts/push! user-id {:level :error :message duplicate-msg})
             {:ok? false :error duplicate-msg})

         (:success result)
         (do (toasts/push! user-id {:level :success :message "Card added"})
             {:ok? true :real-ids (:ids result)})

         :else
         (do (toasts/push! user-id {:level :error
                                    :message (or (:error result) "Failed to add card")})
             {:ok? false :error (:error result)})))))

;; Cancel affordance for the Add-Card Form!. MUST be :type "button" — a bare
;; <button> inside a <form> defaults to type=submit and would submit on click.
;; Returns empty e/amb so it contributes no edit to the form. Mirrors
;; ExportCancelButton.
(e/defn AddCancelButton [close!]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (close!)) nil))
  (e/amb))

(e/defn AddKeepButton
  "Write one flushed add-modal snapshot without closing the modal.
   The caller owns validation and enqueue because it owns the optimistic card
   lifecycle. Post: each button event writes one fresh request id."
  [snapshot! !request]
  (e/amb
    (e/for [[token _] (forms/Button! [::add-keep]
                         :label "Add & Keep" :class "btn btn-secondary"
                         :style {:border-radius "var(--radius-sm)"})]
      (case (reset! !request (assoc (snapshot!) :id (str (random-uuid))))
        (token)))))

(e/defn FreezeCardField [!freeze-side]
  (dom/label
    (dom/props {:style {:display "flex" :align-items "center"
                        :gap "var(--sp-1)" :font-size "12px"}})
    (dom/text "Freeze")
    (dom/select
      (dom/props {:class "input" :value (name (e/watch !freeze-side))
                  :aria-label "Freeze card field"})
      (dom/option (dom/props {:value "none"}) (dom/text "None"))
      (dom/option (dom/props {:value "front"}) (dom/text "Freeze Front"))
      (dom/option (dom/props {:value "back"}) (dom/text "Freeze Back"))
      (dom/option (dom/props {:value "both"}) (dom/text "Freeze Both"))
      (dom/On "change"
        (fn [event]
          (reset! !freeze-side (keyword (.. event -target -value))))
        nil)))
  ;; This preference is not a Forms5 edit.
  (e/amb))

;; Add card modal — uses topic-id and root-topic-id instead of doc-id + page-number
;;
;; Serves two acts, told apart by `copy-source` alone. nil = compose a card from
;; nothing (the Add-new button, optionally seeded with the editor selection).
;; Non-nil = compose a card from an existing one (Create copy…), and it holds
;; that card's `composed-card` shape.
;;
;; ONE parameter carries the whole difference on purpose. A copy differs in four
;; ways — its title, its source identity, its suppressed topic pins, and the Save
;; guard — and separate flags for each could be set inconsistently (pins on with
;; a source set would silently add images the source never had). Derived from one
;; value, the four cannot disagree.
;;
;; `close!` is a 0-arg thunk rather than a show-atom, because the two callers end
;; the modal differently: AddCardButton flips its own boolean, while the copy
;; host clears !editing-card. Neither should be a concern of this component.
(e/defn AddCardModal [close! !card-kind !prefill topic-id root-topic-id user-id copy-source]
  (e/client
    (let [prefill (or @!prefill {})
          copy? (some? copy-source)
          initial-text (or (:primary prefill) "")
          prefill? (not (str/blank? initial-text))
          ;; Pinned images inline into the editors (model B): front pin → primary
          ;; (Question/Cloze), back pin → answer (Answer/Back-Extra). The Save
          ;; below passes bake? false so these are not appended a second time.
          ;;
          ;; A copy takes NO pins. Its content already holds whatever the source
          ;; baked in at ITS creation, so appending the topic's current pins
          ;; would give the copy images the original never had — and the guard
          ;; below would then read that as an edit and let an unwanted duplicate
          ;; through.
          pins-prefill (e/server (when-not copy? (cards/pins-prefill-html topic-id)))
          front-pin-html (:front pins-prefill)
          back-pin-html (:back pins-prefill)
          ;; Front (basic Question) and Cloze Text share !primary so typed
          ;; content carries across a Basic↔Cloze switch; Back/Extra already
          ;; share !answer. A captured selection prefills whichever renders;
          ;; pin HTML is appended after it to match bake ordering.
          init-primary (str (if prefill? initial-text "") front-pin-html)
          init-answer (str (or (:answer prefill) "") back-pin-html)
          ol-prefill (:ol prefill)
          ;; Bound once and used twice — as the atom's seed AND as the
          ;; QuillField's mount HTML. The two MUST agree, or the pane paints
          ;; content the atom does not hold and the first save loses it.
          init-ol-question (or (:question ol-prefill) "")
          init-ol-items-html (ol-items->html (:items ol-prefill))
          kind (e/watch !card-kind)
          !primary (atom init-primary)
          !answer (atom init-answer)
          !primary-editor (atom nil)
          !a-editor (atom nil)
          !ol-question (atom init-ol-question)
          !ol-question-editor (atom nil)
          !ol-items-html (atom init-ol-items-html)
          !ol-items-editor (atom nil)
          !ol-before (atom (str (or (:before ol-prefill) 1)))
          !ol-reveal (atom (if (some? ol-prefill) (boolean (:reveal? ol-prefill)) true))
          !freeze-side (atom :none)
          !draft-revision (atom 0)
          !keep-request (atom nil)
          primary (e/watch !primary)
          answer (e/watch !answer)
          ol-question (e/watch !ol-question)
          ol-items-html (e/watch !ol-items-html)
          ol-before (e/watch !ol-before)
          ol-reveal (e/watch !ol-reveal)
          draft-revision (e/watch !draft-revision)
          keep-request (e/watch !keep-request)
          [keep-token keep-error] (e/Token keep-request)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")
          modal-title (if copy? "Copy Card" "Add Card")
          ;; :question and :cloze both read `primary` because this modal shares
          ;; one atom across the Basic↔Cloze switch — composed-card picks the
          ;; one the kind uses, so the same map serves either.
          live-fields {:question primary :cloze primary :answer answer
                       :ol-question ol-question :ol-items-html ol-items-html
                       :ol-before ol-before :ol-reveal ol-reveal}
          ;; Each Add and Add & Keep request gets its own idempotency key. A
          ;; repeated event reuses its request key; a later card gets a new one.
          flush! (fn []
                   (flush-editors! [[!primary-editor !primary] [!a-editor !answer]
                                    [!ol-items-editor !ol-items-html]
                                    [!ol-question-editor !ol-question]]))
          keep-snapshot! (fn []
                           (flush!)
                           {:kind @!card-kind
                            :topic-id topic-id
                            :root-topic-id root-topic-id
                            :freeze-side @!freeze-side
                            :fields {:question @!primary :cloze @!primary :answer @!answer
                                     :ol-question @!ol-question
                                     :ol-items-html @!ol-items-html
                                     :ol-before @!ol-before
                                     :ol-reveal @!ol-reveal}})]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape (fn [] (close!)) modal-title)
        (dom/On "keydown"
          (fn [e] (modal/mod-enter-request-submit! e))
          nil)
        (dom/div
          (dom/props {:class "card-modal-inner"
                      :style {:background "var(--color-bg-card)" :border-radius "var(--radius-lg)" :padding "var(--sp-6)"
                              :box-shadow "0 4px 20px rgba(0,0,0,0.25)"
                              :pointer-events "auto"}})
          (dom/On "pointerdown"
            (fn [e] (modal/drag-modal-by-title! e))
            nil)
          (let [inner dom/node
                tab-cleanup (attach-modal-tab-nav! inner)
                flush-cleanup (attach-submit-flush! inner flush!)]
            (e/on-unmount (fn [] (when tab-cleanup (tab-cleanup)) (when flush-cleanup (flush-cleanup)))))
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text modal-title))
          ;; Copies can use any supported card type.
          (do
            (dom/div
              (dom/props {:style {:display "flex" :align-items "center"
                                  :gap "var(--sp-2)" :margin-bottom "var(--sp-4)"}})
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center"
                                    :gap "var(--sp-1)" :font-size "14px"}})
                (dom/text "Type")
                (dom/select
                  (dom/props {:class "input" :value kind :aria-label "Card type"})
                  (e/for [option (e/diff-by :value
                                  [{:value "basic" :label "Basic"}
                                   {:value "cloze" :label "Cloze"}
                                   {:value "overlapping" :label "Overlapping"}])]
                    (dom/option
                      (dom/props {:value (:value option)
                                  :selected (= kind (:value option))})
                      (dom/text (:label option))))
                  (dom/On "change"
                    (fn [event]
                      ;; Flushing editors can restore the previous DOM selection.
                      (let [next-kind (.. event -target -value)]
                        (flush!)
                        (switch-add-card-kind! !card-kind !primary !answer
                          !ol-question !ol-items-html next-kind)))
                    nil)))
              (when (= kind "basic")
                (dom/button
                  (dom/props {:class "btn btn-secondary" :type "button"
                              :title "Flip question and answer"
                              :aria-label "Flip question and answer"
                              :style {:padding "4px" :border-radius "999px"}})
                  (icons/Icon :refresh-cw :size 15)
                  (dom/On "click"
                    (fn [_]
                      (flush!)
                      (flip-basic-card-fields! !primary !answer !draft-revision))
                    nil)))))
          (cond
            (= kind "overlapping")
            (OverlappingPane !ol-question !ol-question-editor ol-question [:add-ol-q draft-revision]
              !ol-items-html !ol-items-editor ol-items-html [:add-ol draft-revision]
              !ol-before !ol-reveal modal-font)
            (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField primary
                  (fn [html] (reset! !primary html))
                  "Question..." [:add-q draft-revision] !primary-editor nil true))
              (dom/label (dom/text "Answer:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField answer
                  (fn [html] (reset! !answer html))
                  "Answer..." [:add-a draft-revision] !a-editor nil nil)))
            :else
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField primary
                  (fn [html] (reset! !primary html))
                  "Cloze text with {{c1::deletion}}..." [:add-c draft-revision] !primary-editor true true))
              (dom/label (dom/text "Back Extra:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField answer
                  (fn [html] (reset! !answer html))
                  "Optional back extra..." [:add-be draft-revision] !a-editor nil nil))))
          (when keep-error
            (dom/div
              (dom/props {:class "form-error" :role "alert"})
              (dom/text (ex-message keep-error))))
          ;; Forms5 Add. The command becomes one identity-stable element in the
          ;; commits channel (keyed by its token), so the enqueue runs once per
          ;; submit — replacing the hand-rolled e/Token whose e/server enqueue
          ;; re-fired whenever the reactively-derived card-data changed, adding
          ;; the card 2–3 times. Validation lives in :Parse (ex-info ⇒ Forms5
          ;; disables commit + renders the message). Content capture: the
          ;; capture-phase submit flush (attach-submit-flush!, on the modal-inner
          ;; ancestor) writes the Quill syntax-tokenized HTML into the field atoms
          ;; before :Parse stamps the commit.
          (let [commits (forms/Form! {}
                          (e/fn Fields [_]
                            (dom/div
                              (dom/props {:style {:display "flex" :flex-wrap "wrap" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
                              (e/amb
                                (forms/SubmitButton! :label (if copy? "Save" "Add") :class "btn btn-primary"
                                  :style {:border-radius "var(--radius-sm)" :order "1"})
                                (when-not copy?
                                  (FreezeCardField !freeze-side))
                                (when-not copy?
                                  (AddKeepButton keep-snapshot! !keep-request))
                                (AddCancelButton close!))))
                          :Parse (e/fn [_ _]
                                   (if (and copy? (= copy-source (composed-card kind live-fields)))
                                     ;; Saving here would insert a row the
                                     ;; flashcard uniqueness indexes drop, and
                                     ;; the user would be told a card was added
                                     ;; that does not exist. Refusing with a
                                     ;; reason is the whole point of the copy
                                     ;; opening pre-filled: it is a starting
                                     ;; point, not a finished card.
                                     (ex-info "Change something first — this is still identical to the card it was copied from" {})
                                     (add-card-command kind topic-id root-topic-id live-fields)))
                          :type :command
                          :show-buttons false)]
            ;; Service: one branch per committed submit. e/snapshot freezes the
            ;; parsed command at commit time — Forms5 sequential mode tracks
            ;; parsed-form-v LIVE, so without the snapshot a late editor mutation
            ;; during the enqueue round-trip would re-fire the enqueue. Sanitize
            ;; the frozen HTML server-side (basic/cloze only; overlapping is
            ;; stored verbatim, matching prior behavior), enqueue optimistically,
            ;; close on ack.
            (e/amb
              (e/for [[t cmd] (e/diff-by first (e/as-vec commits))]
                (let [frozen-command (e/snapshot cmd)
                      {:keys [kind topic-id root-topic-id]} (nth frozen-command 1)
                      add-id (e/snapshot (str (random-uuid)))
                      ;; Typed text only. Sanitation happens inside the :add-card
                      ;; effect, which is where the save runs — a per-field
                      ;; (e/server (sanitize-card-field …)) here sent each field to
                      ;; the server and back before the payload made the same trip.
                      ;; The overlay row therefore shows what the user typed, and
                      ;; the sanitized form arrives with the server row.
                      card-data (add-command-card-data frozen-command)]
                  (case (opt/enqueue-add-card! add-id
                          {:topic-id topic-id :root-topic-id root-topic-id
                           :kind kind :card-data card-data})
                    (do (e/on-unmount (fn [] (close!))) (t)))))
              (when keep-token
                (let [{:keys [id kind topic-id root-topic-id freeze-side fields]}
                      keep-request
                      command (add-card-command kind topic-id root-topic-id fields)]
                  (if (vector? command)
                    (let [card-data (add-command-card-data command)]
                      (case (opt/enqueue-add-card! id
                              {:topic-id topic-id :root-topic-id root-topic-id
                               :kind kind :card-data card-data})
                        (case (clear-add-card-draft! freeze-side kind
                                !primary !answer !ol-question !ol-items-html
                                !ol-before !ol-reveal)
                          (case (swap! !draft-revision inc)
                            (keep-token)))))
                    (keep-token command)))))))))))

(e/defn CopyCardModal
  "Mounts AddCardModal in copy mode from a {:mode :copy} !editing-card request.

   Pre : @!editing-card matches the copy shape written by CopyCardButton —
         {:mode :copy :kind :topic-id :root-topic-id :prefill :source}. The
         host mounts this only while that holds.
   Post: closing clears !editing-card, so the card table returns to no modal.
   Invariant: :topic-id is the SOURCE CARD's topic, never the ambient one. On a
         root topic the card table lists every descendant's cards, so a copy
         sited from the ambient topic would land on the root instead of the
         page the card lives on.

   A wrapper rather than a call from the two hosts, so the kind and prefill
   atoms have one stable frame to live in and neither host has to build them."
  [!editing-card user-id]
  (e/client
    (let [request (e/watch !editing-card)
          !card-kind (atom (:kind request))
          !prefill (atom (:prefill request))]
      (AddCardModal (fn [] (reset! !editing-card nil))
        !card-kind !prefill
        (:topic-id request) (:root-topic-id request)
        user-id (:source request)))))
