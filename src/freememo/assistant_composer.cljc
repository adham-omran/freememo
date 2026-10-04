(ns freememo.assistant-composer
  "Message editor and controls for the assistant panel."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   [freememo.assistant-annotations :as annotations]
   [freememo.assistant-answer-selection :as answer-selection]
   [freememo.assistant-panel-data :as panel-data]
   [freememo.assistant-panel-rows :as rows]
   #?(:clj [freememo.assistant :as assistant])))

(defn- submit-edit
  "The submit payload, or nil when there is nothing to send. Annotations make a
   send with a blank draft valid: their message is the turn."
  [draft-atom active-atom refs-atom annotation-items mode-id web-search?]
  (let [text (str/trim @draft-atom)]
    (when (or (not (str/blank? text)) (seq annotation-items))
      {:id (str (random-uuid)) :text text :chat @active-atom
       :refs (mapv :id @refs-atom) :mode mode-id :web-search? web-search?
       :annotations (annotations/wire-annotations annotation-items)})))

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
           annotations]}
   reference-documents !picked !refs !mention-index !at-open? !draft !active !submit !pending-mode
   !pending-web-search? !error !annotations]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__composer-shell"})
      ;; Keep the virtual-scroll frame stable while the form processes an input
      ;; event. Conditional mounting in the same frame strips Forms5 input props.
      (rows/ReferenceMenu at-open? reference-documents reference-document-count
        mention-query active-reference-index !picked)
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
            (close-reference-menu-after-input!
              (.. event -target -value) !at-open?))
          nil)
        (dom/On "keydown"
          (fn [event]
            (cond
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
                             (:id mode-entry) web-search?)]
                  (reset! !submit edit)))

              (= (.-key event) "@")
              (do (reset! !mention-index 0) (reset! !at-open? true))

              (and at-open? (= (.-key event) " "))
              (reset! !at-open? false)))
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
                             (:id mode-entry) web-search?)]
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
                    (command-token)))))))))))
