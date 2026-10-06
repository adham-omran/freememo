(ns freememo.apply-code-language
  "The `:apply-code-language` command — set one syntax language on every code
   block in the open topic.

   Mounted once by `freememo.document-body`, where `dctx/user-id` and
   `dctx/page-topic-id` are bound. It renders nothing until the palette
   dispatches, so the cost of mounting it is a subscription and no DOM.

   THE ONLY :exec :electric OWNER. `bus/Requests` yields the pending
   invocations, and `bus/consume!` MUST run exactly once per invocation or the
   modal reappears on the next remount. `:ui-button` was the alternative and
   would have needed an invisible publisher — a component that draws no control
   and exists only to make the command look available.

   TWO WRITE PATHS, AND THAT IS NOT AN OVERSIGHT. Prose is rewritten in the
   LIVE Quill instance; cards are rewritten from stored HTML on the server. The
   editor can hold content the database has not seen, so a server-side rewrite
   of `topics.content` would be overwritten by the next autosave. Pushing the
   rewritten body back with `set-content!` is the other route and returns the
   reader to scroll position 0 — see `!content-generation` in
   `freememo.rich-text-editor`.

   THE OPERATION IS NOT REVERSIBLE, on purpose. The prose write uses source
   \"api\" so Quill's history does not hold it, which is what stops one command
   from having two undo behaviours: Cmd+Z could never have reached the cards.
   Source \"api\" also means `text-change` never fires — its listener guards on
   `source == \"user\"` — so this namespace sets `!dirty-html` by hand or the
   change never reaches the database at all. `rich-text-editor/highlight-range!`
   is the same shape for the same reason.

   Each rewritten card DOES keep its previous rendition, because the write goes
   through `db/update-flashcard-versioned!` and lands in the card history modal.
   That is a byproduct of using the writer built for user-initiated saves, not
   an undo for this command.

   A PICK IS A LONG OPERATION AND THE PICKER SAYS SO. The prose half cannot be
   moved off the main thread — Quill is a DOM library — so the picker shows
   which phase is running and stops answering Escape until it is done. See
   `set-editor-code-language!` for the arithmetic that made the difference
   between a second and a hung tab."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.code-languages :as langs]
   [freememo.command-bus :as bus]
   [freememo.modal-shell :as modal-shell]
   [freememo.quill-field :as quill-field]
   [freememo.rich-text-editor :as editor]
   #?(:clj [freememo.code-language-html :as clh])
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.toasts :as toasts])))

;; ── Prose: the live editor ─────────────────────────────────────────────────

(defn editor-code-block-count
  "How many code blocks the mounted document editor shows.

   Pre : none. Post: 0 when no editor is mounted."
  []
  #?(:clj 0
     :cljs
     ;; `^js` keeps `.querySelectorAll` off Closure's rename list under
     ;; :advanced. `current-editor` returns an untyped value, so nothing else
     ;; here can infer it — see the same hint in `code_lang_picker.cljs`,
     ;; where its absence renamed `.domNode` and threw on every selection.
     (if-let [^js ed (editor/current-editor)]
       (.-length (.querySelectorAll (.-root ed) ".ql-code-block-container"))
       0)))

(defn set-editor-code-language!
  "Set every code line in the mounted editor to `language`, and make the change
   persist without entering Quill's undo history.

   Pre : `language` ∈ `langs/code-language-keys` (caller checks — the server
         asserts it again before anything is stored). The caller has already
         painted a busy state and yielded a frame, because this BLOCKS the main
         thread for its whole run.
   Post: every `div.ql-code-block` in the editor carries `language`; no other
         line is touched; `!dirty-html` holds the new stored-form body tagged
         with the editor's own topic id, so the auto-save writes it. Returns the
         number of code BLOCKS changed, which is what a reader counts. 0 when no
         editor is mounted, and then `!dirty-html` is left alone.
   Inv : Quill's document LENGTH does not change, which is why the newline
         indices are collected before the write and stay valid through it.

   ONE `updateContents`, NEVER ONE `formatLine` PER LINE. `formatLine` ends in
   `Editor.update`, which rebuilds the WHOLE document's Delta and composes it
   against the previous one — O(document) per call, with no batching to share.
   A production document with 707 code blocks made that O(707 × document) and
   hung the tab for more than five minutes, until Chrome offered to kill the
   page. The same edit as one delta of retains pays that cost once, inside
   Quill's own `batchStart`/`batchEnd`.

   A code line's `code-block` attribute lives on its trailing NEWLINE, so each
   op retains up to that newline and then retains the newline itself with the
   attribute. `Block.formatAt` applies a block format only when the range ends
   exactly at the block's end, which is what makes the one-character retain the
   right target and a whole-line retain the wrong one."
  [language]
  #?(:clj 0
     :cljs
     (let [^js ed (editor/current-editor)
           ^js Quill (.-Quill js/window)]
       (if (or (nil? ed) (nil? Quill))
         0
         (let [^js root (.-root ed)
               blocks (.-length (.querySelectorAll root ".ql-code-block-container"))
               Delta (.import Quill "delta")
               newlines (->> (js/Array.from (.querySelectorAll root "div.ql-code-block"))
                          (keep (fn [node]
                                  (when-some [^js blot (.find Quill node)]
                                    ;; `length` counts the newline the block
                                    ;; format hangs on, so the last index of the
                                    ;; line IS that newline.
                                    (+ (.getIndex ed blot) (.length blot) -1))))
                          sort)
               ^js delta (loop [^js d (new Delta) pos 0 xs (seq newlines)]
                           (if-some [nl (first xs)]
                             (recur (-> d
                                      (.retain (- nl pos))
                                      (.retain 1 #js {"code-block" language}))
                               (inc nl)
                               (next xs))
                             d))]
           (when (seq newlines)
             ;; "api" keeps this out of Quill's history AND out of the
             ;; text-change listener, hence the explicit !dirty-html below.
             (.updateContents ed delta "api")
             ;; Stored form via editor-html, never .root.innerHTML — a raw read
             ;; would persist Quill's rendered KaTeX subtree instead of the TeX.
             (reset! editor/!dirty-html
               {:html (quill-field/editor-html ed)
                :topic-id (:topic-id @editor/!editor-state)}))
           blocks)))))

(defn after-paint!
  "Run `f` on a later task, once the browser has painted what the caller just
   wrote to the DOM.

   Pre : `f` is a plain 0-arg CLJS function. Post: `f` runs exactly once, off
         the current task; nothing runs on CLJ.
   Why : a `requestAnimationFrame` callback runs BEFORE that frame is painted,
         so scheduling blocking work there hides the busy state it was supposed
         to reveal. The timeout scheduled from inside the frame lands after the
         frame is on screen."
  [f]
  #?(:clj nil
     :cljs (js/requestAnimationFrame (fn [_] (js/setTimeout f 0)))))

;; ── Cards: stored HTML on the server ───────────────────────────────────────

#?(:clj
   (defn- relanguage-card!
     "Rewrite one card's three HTML fields. Returns true iff it was written.

      Pre : `card` is a `db/get-flashcards` row, so its content keys are
            namespaced (`:flashcards/question`).
      Post: a card whose fields all come back nil from `relanguage-field` is not
            written at all, so it produces no `card_versions` row and its sync
            digest does not move."
     [user-id language card]
     (let [fields (into {}
                    (keep (fn [k]
                            (when-let [html (clh/relanguage-field
                                              (get card (keyword "flashcards" (name k)))
                                              language)]
                              [k html])))
                    [:question :answer :cloze])]
       (when (seq fields)
         (db/update-flashcard-versioned! user-id (:flashcards/id card) fields)
         true))))

#?(:clj
   (defn- report!
     "Toast the whole run — prose and cards in one line, because the user asked
      for one operation."
     [user-id label prose-blocks cards]
     (toasts/push! user-id
       {:level :info
        :message (cond
                   (and (zero? prose-blocks) (zero? cards))
                   (str "No code blocks to set to " label)
                   (zero? cards)
                   (str "Set " prose-blocks " code block" (when (not= 1 prose-blocks) "s")
                     " to " label)
                   :else
                   (str "Set " prose-blocks " code block" (when (not= 1 prose-blocks) "s")
                     " to " label ", " cards " card" (when (not= 1 cards) "s") " updated"))})))

(defn apply-code-language!*
  "Rewrite the topic's cards, then report the run.

   `prose-blocks` is DISPLAY ONLY — the client has already applied the prose
   half against its live editor, and passes the count so one call produces one
   toast instead of two round trips for a message.

   Pre : `language` ∈ `langs/code-language-keys`. A caller that fails this gets
         `{:ok false}` rather than an assertion, because the value reaches here
         from a rendered button and a mismatch means the vocabulary drifted.
   Post: every card of `topic-id` whose HTML holds a code block now names
         `language`; unchanged cards are not written. `:card-mutations` is
         bumped iff at least one card changed. Returns
         `{:ok true :cards n}` or `{:ok false :error msg}`."
  [user-id topic-id language label prose-blocks]
  #?(:clj
     (if-not (contains? langs/code-language-keys language)
       {:ok false :error (str "Unknown language: " language)}
       (try
         (let [cards (->> (db/get-flashcards topic-id)
                       (filter #(relanguage-card! user-id language %))
                       count)]
           (when (pos? cards)
             (commands/bump! user-id :apply-code-language))
           (report! user-id label prose-blocks cards)
           {:ok true :cards cards})
         (catch Exception e
           {:ok false :error (or (.getMessage e) "Could not apply the language")})))
     :cljs nil))

(defn card-code-block-count*
  "How many of `topic-id`'s cards hold at least one code block.

   Pre : none. Post: 0 on any read failure — this figure is disclosure in the
         picker, and a modal that refuses to open over it would be worse than a
         modal that opens saying 0."
  [topic-id]
  #?(:clj
     (try
       (->> (db/get-flashcards topic-id)
         (filter (fn [card]
                   (some #(pos? (clh/code-block-count
                                  (get card (keyword "flashcards" (name %)))))
                     [:question :answer :cloze])))
         count)
       (catch Exception _ 0))
     :cljs 0))

;; ── The picker ─────────────────────────────────────────────────────────────

(e/defn LanguageButton
  "One language in the picker.

   `pick!` is LOWERCASE on purpose. It is a plain 2-arg CLJS function, and it is
   called from inside a `dom/On` callback — plain CLJS, not reactive position.
   Capitalizing it would make Electric compile `(pick! …)` as an Electric call
   (`lang3/electric-sym?` reads the first character), and an `e/fn` cannot be
   invoked from a DOM callback at all."
  [{:keys [key label]} pick!]
  (e/client
    (dom/button
      (dom/props {:class "input"
                  :style {:cursor "pointer" :text-align "left" :padding "8px 10px"}})
      (dom/text label)
      (dom/On "click" (fn [_] (pick! key label)) nil))))

(defn- busy-message
  "The line the picker shows while a pick is running.

   Pre : `stage` is nil or `{:phase :prose|:cards :label s :blocks n}`.
   Post: nil when nothing is running, so one `some?` decides every busy prop."
  [stage]
  (when-some [{:keys [phase label blocks]} stage]
    (cond
      (not= phase :prose) "Updating cards…"
      ;; A document with no prose code block still has cards to rewrite, and
      ;; "Setting 0 code blocks" is not a thing to tell anyone.
      (zero? blocks) (str "Applying " label "…")
      :else (str "Setting " blocks " code block" (when (not= 1 blocks) "s")
              " to " label "…"))))

(e/defn ApplyCodeLanguageModal
  "The picker for one pending invocation. Unmounts when the invocation is
   consumed, which is the only way it closes.

   A PICK IS A LONG OPERATION AND THE PICKER SAYS SO. The prose half blocks the
   main thread and the card half is a server round trip, so `!stage` carries
   which of the two is running: the language grid hides, a spinner and a line
   naming the phase take its place, and Escape and the backdrop stop closing —
   `consume!` would unmount the frame holding the card write and cancel it
   half-done."
  [invocation-id user-id topic-id]
  (e/client
    (let [close! (fn [] (bus/consume! invocation-id))
          !stage (atom nil)
          stage (e/watch !stage)
          busy (busy-message stage)
          busy? (some? busy)
          ;; The success path closes through `close!` while a stage is still
          ;; set, so the guard cannot live in `close!` itself.
          dismiss! (fn [] (when (nil? @!stage) (close!)))
          !pick (atom nil)
          pick (e/watch !pick)
          [t ?error] (e/Token pick)
          prose-blocks (e/snapshot (editor-code-block-count))
          card-count (e/server (e/Offload #(card-code-block-count* topic-id)))]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (modal-shell/ModalEscape dismiss! "Apply code language")
        (dom/On "click" (fn [_] (dismiss!)) nil)
        (dom/div
          (dom/props {:class "modal-content"
                      :aria-busy (if busy? "true" "false")
                      :style {:width "min(420px, 92vw)" :display "flex"
                              :flex-direction "column" :gap "12px"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)

          (dom/h3 (dom/props {:style {:margin "0" :font-size "15px"}})
            (dom/text "Apply language to all code"))

          ;; Disclosure, not a confirmation step: the numbers are here so the
          ;; card half is not invisible, and the pick applies immediately.
          (dom/p
            (dom/props {:style {:margin "0" :font-size "13px"
                                :color "var(--color-text-hint)"}})
            ;; `card-count` is nil while the Offload is in flight, and
            ;; `(str nil)` would render " card s". Read it through `or`.
            (let [cards (or card-count 0)]
              (dom/text (str prose-blocks " code block"
                          (when (not= 1 prose-blocks) "s") " in this document, "
                          cards " card" (when (not= 1 cards) "s")
                          " with code"))))

          ;; ALWAYS mounted, hidden by style rather than by `when`. A
          ;; `(when-some [err ?error] …)` here would be a 0-to-1 sibling, and an
          ;; incseq diff falls back on POSITION — so an error appearing would
          ;; shift the language grid and the token block below it, unmounting
          ;; and rebuilding both. `document_body.cljc` records that failure.
          (dom/p
            (dom/props {:style {:margin "0" :font-size "13px"
                                :display (if (some? ?error) "block" "none")
                                :color "var(--color-danger, #c00)"}})
            (dom/text (if (some? ?error) (str ?error) "")))

          ;; The busy row and the grid swap by `display`, for the reason the
          ;; error paragraph above states: a `when` here would make them
          ;; position-shifting siblings and rebuild the whole grid on every
          ;; phase change.
          (dom/div
            (dom/props {:role "status"
                        :style {:display (if busy? "flex" "none")
                                :align-items "center" :gap "8px"
                                :padding "18px 2px" :font-size "13px"
                                :color "var(--color-text-secondary)"}})
            (dom/span (dom/props {:class "spinner"}))
            (dom/span (dom/text (or busy ""))))

          (dom/div
            (dom/props {:style {:display (if busy? "none" "grid")
                                :gap "6px" :max-height "50vh"
                                :overflow "auto"
                                :grid-template-columns "repeat(2, minmax(0, 1fr))"}})
            (e/for [lang (e/diff-by :key langs/code-languages)]
              (LanguageButton lang
                (fn [key label]
                  ;; One pick per modal. The grid is hidden while a stage is
                  ;; set, but a second click can already be in the queue behind
                  ;; the first, and the prose write is not idempotent about its
                  ;; cost.
                  (when (nil? @!stage)
                    (reset! !stage {:phase :prose :label label
                                    :blocks prose-blocks})
                    ;; The prose write runs in a plain callback, so it happens
                    ;; once per click rather than on every reactive
                    ;; re-evaluation of this body — and it runs a frame LATER,
                    ;; so the busy row above is on screen before the main
                    ;; thread stops answering. The token below carries only the
                    ;; server half.
                    (after-paint!
                      (fn []
                        (let [n (set-editor-code-language! key)]
                          (swap! !stage assoc :phase :cards)
                          (reset! !pick {:language key :label label
                                         :prose n})))))))))

          (when t
            (let [r (e/server (e/Offload #(apply-code-language!*
                                            user-id topic-id
                                            (:language pick) (:label pick)
                                            (:prose pick))))]
              (when (some? r)
                (if (:ok r)
                  ;; Registered INSIDE the success branch, not beside the token.
                  ;; `(t err)` unmounts `when t` as surely as `(t)` does, so a
                  ;; close! registered one level up would tear the modal down
                  ;; over the very error the branch below is showing. The
                  ;; success branch only mounts on success.
                  ;;
                  ;; It must also be an unmount callback rather than a sibling
                  ;; of the server call: consume! destroys this frame, and a
                  ;; sibling would race the effect it is meant to follow.
                  (do (e/on-unmount close!)
                      (case r (t)))
                  ;; The failure branch clears the stage the same way, so the
                  ;; grid comes back under the error and Escape closes again.
                  ;; The prose half has already been written; only the cards
                  ;; failed, and the toast is what says so.
                  (do (e/on-unmount #(reset! !stage nil))
                      (t (:error r))))))))))))

(e/defn ApplyCodeLanguage
  "Mount point. Renders the picker while an invocation is pending."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/page-topic-id]
      (e/for [{:keys [invocation-id]} (bus/Requests :apply-code-language)]
        (ApplyCodeLanguageModal invocation-id user-id topic-id)))))
