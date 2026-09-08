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
   an undo for this command."
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
         asserts it again before anything is stored).
   Post: every `div.ql-code-block` in the editor carries `language`; no other
         line is touched; `!dirty-html` holds the new stored-form body tagged
         with the editor's own topic id, so the auto-save writes it. Returns the
         number of code BLOCKS changed, which is what a reader counts. 0 when no
         editor is mounted, and then `!dirty-html` is left alone.
   Inv : Quill's document LENGTH does not change, which is why the line indices
         are collected before the first `formatLine` and stay valid through the
         loop."
  [language]
  #?(:clj 0
     :cljs
     (let [^js ed (editor/current-editor)
           Quill (.-Quill js/window)]
       (if (or (nil? ed) (nil? Quill))
         0
         (let [root (.-root ed)
               blocks (.-length (.querySelectorAll root ".ql-code-block-container"))
               indices (into []
                         (keep (fn [node]
                                 (when-let [blot (.find Quill node)]
                                   (.getIndex ed blot))))
                         (js/Array.from (.querySelectorAll root "div.ql-code-block")))]
           (doseq [i indices]
             ;; "api" keeps this out of Quill's history AND out of the
             ;; text-change listener, hence the explicit !dirty-html below.
             (.formatLine ed i 1 "code-block" language "api"))
           (when (seq indices)
             ;; Stored form via editor-html, never .root.innerHTML — a raw read
             ;; would persist Quill's rendered KaTeX subtree instead of the TeX.
             (reset! editor/!dirty-html
               {:html (quill-field/editor-html ed)
                :topic-id (:topic-id @editor/!editor-state)}))
           blocks)))))

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
                     " to " label " · " cards " card" (when (not= 1 cards) "s") " updated"))})))

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

(e/defn ApplyCodeLanguageModal
  "The picker for one pending invocation. Unmounts when the invocation is
   consumed, which is the only way it closes."
  [invocation-id user-id topic-id]
  (e/client
    (let [close! (fn [] (bus/consume! invocation-id))
          !pick (atom nil)
          pick (e/watch !pick)
          [t ?error] (e/Token pick)
          prose-blocks (e/snapshot (editor-code-block-count))
          card-count (e/server (e/Offload #(card-code-block-count* topic-id)))]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (modal-shell/ModalEscape close! "Apply code language")
        (dom/On "click" (fn [_] (close!)) nil)
        (dom/div
          (dom/props {:class "modal-content"
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
                          (when (not= 1 prose-blocks) "s") " in this document · "
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

          (dom/div
            (dom/props {:style {:display "grid" :gap "6px" :max-height "50vh"
                                :overflow "auto"
                                :grid-template-columns "repeat(2, minmax(0, 1fr))"}})
            (e/for [lang (e/diff-by :key langs/code-languages)]
              (LanguageButton lang
                (fn [key label]
                  ;; The prose write runs HERE, in a plain callback, so it
                  ;; happens once per click rather than on every reactive
                  ;; re-evaluation of this body. The token below carries only
                  ;; the server half.
                  (let [n (set-editor-code-language! key)]
                    (reset! !pick {:language key :label label :prose n}))))))

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
                  (t (:error r)))))))))))

(e/defn ApplyCodeLanguage
  "Mount point. Renders the picker while an invocation is pending."
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/page-topic-id]
      (e/for [{:keys [invocation-id]} (bus/Requests :apply-code-language)]
        (ApplyCodeLanguageModal invocation-id user-id topic-id)))))
