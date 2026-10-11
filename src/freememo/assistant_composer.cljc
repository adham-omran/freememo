(ns freememo.assistant-composer
  "Message editor and controls for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.assistant-annotations :as annotations]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-context-usage :as context-usage]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   [freememo.assistant-slash-menu :as slash-menu]
   #?(:clj [freememo.assistant :as assistant])))

(defn active-pending-fork
  "The pending fork when it was made in chat `active`, else nil. A fork made
   in another chat waits, unseen, until the learner opens that chat again."
  [pending-fork active]
  (when (and pending-fork (= active (:chat-id pending-fork)))
    pending-fork))

(defn- submit-edit
  "The submit payload, or nil when there is nothing to send. Annotations make a
   send with a blank draft valid: their message is the turn. A pending fork of
   the open chat rides along as :fork, so the send creates that fork
   (plans/assistant-chat-forks.md §4.3.3)."
  [draft-atom active-atom refs-atom annotation-items mode-id web-search? pending-fork-atom]
  (let [text (str/trim @draft-atom)
        fork (active-pending-fork @pending-fork-atom @active-atom)]
    (when (or (not (str/blank? text)) (seq annotation-items))
      {:id (str (random-uuid)) :text text :chat @active-atom
       :refs (mapv :id @refs-atom) :mode mode-id :web-search? web-search?
       :annotations (annotations/wire-annotations annotation-items)
       :fork (when fork {:message-id (:message-id fork)})})))

(defn- annotation-preview
  "One line of an annotation for the composer's list."
  [{quoted :quote note :note}]
  (let [clip (fn [s n] (if (> (count s) n) (str (subs s 0 n) "…") s))]
    (str "“" (clip quoted 80) "”"
      (when-not (str/blank? note) (str " — " (clip note 80))))))

(defn sync-composer-draft-value!
  "Set the visible composer value after a programmatic draft edit.

   Forms5 owns the textarea's value, so a `reset!` on the draft atom alone does
   not move what the learner sees. Picking a reference, removing one, and
   sending all rewrite the draft from outside the input, and all go through here."
  [value]
  #?(:cljs
     (when-let [input (.querySelector js/document ".assistant-panel__input")]
       (set! (.-value input) value))
     :clj nil))

(defn replace-composer-draft!
  "Replace the draft from outside the textarea, in the atom and on screen.

   Enter leaves focus in the textarea, and Forms5 writes no value into a focused
   field. So a `reset!` alone left the sent text on screen with an empty draft.

   Post: `@!draft` and the visible textarea value both equal `value`."
  [!draft value]
  (sync-composer-draft-value! value)
  (reset! !draft value))

(defn close-reference-menu-after-input!
  "Close the menu after Forms5 finishes the current input transaction."
  [value !at-open?]
  #?(:cljs
     (js/setTimeout
       #(when-not (panel-data/active-document-mention? value)
          (reset! !at-open? false))
       0)
     :clj nil))

(e/defn ReferencePickEffect
  "Add the picked document as a reference chip and complete its `@` mention.
   The server list stays server-sited: `reference-documents` arrives as a
   positional argument, which does not transfer it, and the indexed lookup
   transfers only the selected document.
   Pre:  `picked` is an index into the mention menu, or nil."
  [picked reference-documents reference-document-count draft
   !refs !draft !mention-index !at-open? !picked]
  (e/client
    (let [[pt _] (e/Token picked)]
      (when pt
        (let [document (e/server
                         (when (pos? reference-document-count)
                           (nth reference-documents
                             (mod picked reference-document-count) nil)))]
          (case document
            (if document
              (let [next-draft (panel-data/draft-with-reference
                                 draft (:title document))]
                (case (swap! !refs
                        (fn [references]
                          (if (some #(= (:id document) (:id %)) references)
                            references
                            (conj references {:id (:id document)
                                              :title (:title document)}))))
                  (case (e/client (sync-composer-draft-value! next-draft))
                    (case (reset! !draft next-draft)
                      (case (reset! !mention-index 0)
                        (case (reset! !at-open? false)
                          (case (reset! !picked nil)
                            (pt))))))))
              (case (reset! !picked nil)
                (pt)))))))))

(e/defn AssistantComposer
  [{:keys [at-open? mention-query active-reference-index reference-document-count refs
           sending? mode-entry web-search? web-search-supported? draft user-id active root-topic-id
           annotations context-usage]}
   reference-documents !picked !refs !mention-index !at-open? !draft !active !submit !pending-mode
   !pending-web-search? !error !annotations !pending-fork]
  (e/client
    (let [;; The slash menu's selection counter, and the draft Escape closed
          ;; it on. Browser state only, so no token.
          !slash-index (atom 0)
          !slash-dismissed (atom nil)
          slash-state (slash-menu/menu-state draft (e/watch !slash-dismissed)
                        (e/watch !slash-index) at-open?)
          pick-slash! (fn [command]
                        (reset! !slash-index 0)
                        (replace-composer-draft! !draft (:completion command)))]
      (dom/div
        (dom/props {:class "assistant-panel__composer-shell"})
        ;; Outside the Input! `e/amb` below, for the prop-loss reason recorded
        ;; there. Cancelling is browser state only, so it needs no token.
        (when-let [fork (active-pending-fork (e/watch !pending-fork) active)]
          (dom/div
            (dom/props {:class "assistant-panel__fork-banner" :role "status"})
            (dom/span
              (dom/props {:class "assistant-panel__fork-text" :dir "auto"})
              (dom/text (if (:excerpt fork)
                          (str "Forking at “" (:excerpt fork) "”")
                          "Forking at the latest message")))
            (dom/button
              (dom/props {:class "assistant-panel__ref-remove" :type "button"
                          :title "Cancel the fork" :aria-label "Cancel the fork"})
              (dom/text "×")
              (dom/On "click" (fn [_] (reset! !pending-fork nil)) nil))))
        ;; Keep the virtual-scroll frame stable while the form processes an input
        ;; event. Conditional mounting in the same frame strips Forms5 input props.
        (rows/ReferenceMenu at-open? reference-documents reference-document-count
          mention-query active-reference-index !picked)
        (slash-menu/SlashMenu slash-state pick-slash!)
        (when (seq refs)
          (dom/div
            (dom/props {:class "assistant-panel__refs"})
            (e/for-by :id [reference refs]
              (dom/span
                (dom/props {:class "assistant-panel__ref-chip"})
                (dom/text (str "@" (:title reference)))
                (dom/button
                  (dom/props {:class "assistant-panel__ref-remove" :type "button"
                              :title "Remove reference"
                              :aria-label "Remove reference"})
                  (dom/text "×")
                  (let [event (dom/On "click" identity nil)
                        [remove-token _] (e/Token event)]
                    (when remove-token
                      (let [next-draft (panel-data/draft-without-reference
                                         draft (:title reference))]
                        (case (swap! !refs
                                (fn [current]
                                  (into []
                                    (remove #(= (:id reference) (:id %)))
                                    current)))
                          (case (sync-composer-draft-value! next-draft)
                            (case (reset! !draft next-draft)
                              (remove-token))))))))))))
        ;; Annotations waiting for the next send (plans/assistant-answer-
        ;; selection-actions.md §5.4.4). Outside the Input! `e/amb` below, for
        ;; the prop-loss reason recorded there. Removing one is browser state
        ;; only, so it needs no token.
        (when (seq annotations)
          (dom/element "details"
            (dom/props {:class "assistant-panel__annotations"})
            (dom/element "summary"
              (dom/text (str (count annotations) " annotation"
                          (when (not= 1 (count annotations)) "s"))))
            (dom/ul
              (dom/props {:class "assistant-panel__annotation-list"})
              (e/for-by :id [item annotations]
                (dom/li
                  (dom/props {:class "assistant-panel__annotation" :dir "auto"})
                  (dom/span (dom/text (annotation-preview item)))
                  (dom/button
                    (dom/props {:class "assistant-panel__ref-remove" :type "button"
                                :title "Remove annotation"
                                :aria-label "Remove annotation"})
                    (dom/text "×")
                    (dom/On "click"
                      (fn [_] (swap! !annotations answer-selection/remove-annotation
                                (:id item)))
                      nil)))))))
        (dom/div
          (dom/props {:class "assistant-panel__composer"})
          (dom/On "input"
            (fn [event]
              ;; Escape closes the slash menu until the draft changes.
              (reset! !slash-dismissed nil)
              (close-reference-menu-after-input!
                (.. event -target -value) !at-open?))
            nil)
          (dom/On "keydown"
            (fn [event]
              ;; The slash menu reads the textarea's value, not `draft`, which
              ;; lags it by a commit. Only the textarea's value can start with `/`.
              (let [value (.. event -target -value)
                    slash (slash-menu/menu-state value @!slash-dismissed
                            @!slash-index at-open?)
                    command (when (:open? slash)
                              (nth (:matches slash) (:selected slash)))]
                (cond
                  (and command (= (.-key event) "ArrowDown"))
                  (do (.preventDefault event) (swap! !slash-index inc))

                  (and command (= (.-key event) "ArrowUp"))
                  (do (.preventDefault event) (swap! !slash-index dec))

                  (and command
                    (or (= (.-key event) "Tab")
                      (and (= (.-key event) "Enter")
                        (not (.-shiftKey event))
                        (slash-menu/enter-picks? value command))))
                  (do (.preventDefault event) (pick-slash! command))

                  (and command (= (.-key event) "Escape"))
                  (do (.preventDefault event) (reset! !slash-dismissed value))

                  (and at-open? (= (.-key event) "ArrowDown")
                    (pos? reference-document-count))
                  (do
                    (.preventDefault event)
                    (swap! !mention-index #(mod (inc %) reference-document-count)))

                  (and at-open? (= (.-key event) "ArrowUp")
                    (pos? reference-document-count))
                  (do
                    (.preventDefault event)
                    (swap! !mention-index #(mod (dec %) reference-document-count)))

                  (and at-open? (= (.-key event) "Enter")
                    (pos? reference-document-count))
                  (do
                    (.preventDefault event)
                    (reset! !picked active-reference-index))

                  (and at-open? (= (.-key event) "Escape"))
                  (do (.preventDefault event) (reset! !at-open? false))

                  (and (= (.-key event) "Enter")
                    (not (.-shiftKey event))
                    (not sending?))
                  (do
                    (.preventDefault event)
                    (when-let [edit
                               (submit-edit !draft !active !refs annotations
                                 (:id mode-entry) web-search? !pending-fork)]
                      (reset! !submit edit)))

                  (= (.-key event) "@")
                  (do (reset! !mention-index 0) (reset! !at-open? true))

                  (and at-open? (= (.-key event) " "))
                  (reset! !at-open? false))))
            nil)
          ;; The DOM-bearing `e/amb` is BOUND, and the loop iterates a re-diffed
          ;; vector of it. Iterating the amb directly puts the widgets' own DOM
          ;; inside the collection `e/for` mounts frames over, and Input! returns
          ;; `(e/When waiting? …)` — cardinality 1 while an edit is pending, 0
          ;; once the token is spent. Committing a send therefore reshaped the
          ;; iterated collection and tore down the textarea's `dom/props` frame:
          ;; measured at the send frame, class, id, name, type, rows, dir,
          ;; placeholder and aria-label were all removed. The element itself is
          ;; created in an outer frame and survives, so nothing puts the props
          ;; back — the composer stayed unstyled for the rest of the session.
          ;;
          ;; `e/as-vec` + `e/diff-by first` is the shape every other Forms5 site
          ;; here uses (bibliography_form, anki_sync_form): the amb is consumed
          ;; once as a value, and the loop iterates a separate collection.
          (let [commands
                (e/amb
                  (dom/div
                    (dom/props {:class "assistant-panel__input-row"})
                    ;; NO :disabled here, and that is not an oversight. Every
                    ;; entry of a props map is one element of an `e/for` carrying
                    ;; `(e/on-unmount #(set-property! node attr nil))`
                    ;; (electric_dom3_props/Properties), so a props map that goes
                    ;; ABSENT nulls every prop on the node at once. `sending?` is
                    ;; derived from a server read and is absent for the round trip
                    ;; a send takes, so `:disabled sending?` tore down the whole
                    ;; map: measured at the send frame, class, id, name, type,
                    ;; rows, dir, placeholder and aria-label all went to null. The
                    ;; textarea's element is created in an outer frame, so nothing
                    ;; put them back and the composer stayed unstyled.
                    ;;
                    ;; The prop was self-defeating anyway: setting it is what
                    ;; destroyed it, and the same trace shows `disabled=false`
                    ;; while the button read `Stop`. It never disabled anything.
                    ;; Typing during a run is still harmless — the keydown `cond`
                    ;; below only submits `(not sending?)`, and the button offers
                    ;; Stop rather than Send.
                    (forms/Input! :assistant/draft draft :as :textarea
                      :class "assistant-panel__input" :dir "auto" :rows 2
                      :placeholder
                      "Ask about this page… (@ for another document)"
                      :aria-label "Assistant message"))
                  (dom/div
                    (dom/props {:class "assistant-panel__composer-footer"})
                    (dom/div
                      (dom/props {:class "assistant-panel__composer-controls"})
                      (rows/ModeRow user-id active mode-entry !pending-mode)
                      (rows/WebSearchRow user-id active web-search?
                        web-search-supported? !pending-web-search?))
                    ;; ONE always-mounted button whose command and look depend on
                    ;; `sending?`, never two behind an `if`. Mounting one branch
                    ;; and unmounting the other re-evaluates this `e/amb`, and
                    ;; that tears down the sibling Input!'s prop frame: measured
                    ;; at the send frame, the textarea lost class, placeholder,
                    ;; rows, dir, name and aria-label, and — because its element
                    ;; is NOT recreated, only its props torn down — never got
                    ;; them back. The button recovered only because the swap
                    ;; built it a new node. It looked self-healing until the
                    ;; panel stopped remounting on every run transition.
                    ;;
                    ;; Same rule as the ReferenceMenu above, which stays mounted
                    ;; for exactly this reason.
                    (forms/Button! (if sending? [::stop] [::submit])
                      :class (str "btn assistant-panel__send "
                               (if sending? "btn-secondary" "btn-primary"))
                      :label (if sending? "Stop" "↑")
                      :aria-label (if sending? "Stop response" "Send")
                      :disabled (and (not sending?) (str/blank? draft)
                                  (empty? annotations)))))]
            (e/for [[command-token command] (e/diff-by first (e/as-vec commands))]
              (cond
                (map? command)
                (case (reset! !draft (:assistant/draft command))
                  (command-token))

                (= [::submit] command)
                (case
                  (when-let [edit
                             (submit-edit !draft !active !refs annotations
                               (:id mode-entry) web-search? !pending-fork)]
                    (reset! !submit edit))
                  (command-token))

                (= [::stop] command)
                (let [result
                      (e/server
                        (e/Offload #(assistant/stop-active-run! user-id active)))]
                  (case result
                    (case
                      (reset! !error
                        (when-not (:stopped result)
                          "No active response to stop."))
                      (command-token))))))))
        ;; The context meter (plans/assistant-context-meter-and-slash-menu.md §1).
        ;; Outside the Input! `e/amb` above, for the prop-loss reason recorded
        ;; there. It shows nothing until the chat has an answer with a count.
        (when context-usage
          (dom/div
            (dom/props {:class "assistant-panel__context-usage tooltip-right"
                        :data-tooltip context-usage/usage-tooltip})
            (dom/text (context-usage/usage-text context-usage))))))))
