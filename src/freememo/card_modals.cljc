(ns freememo.card-modals
  "Extracted modal components for ocr-page — kept separate to stay below JVM 64KB method limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.modal-shell :as modal]
   [clojure.string :as str]
   [freememo.typeahead :refer [Typeahead]]
   [freememo.quill-field :refer [QuillField flush-syntax-tokens!]]
   [freememo.commands :as commands]
   [freememo.cloze :as cloze]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.optimistic :as opt])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.settings :as settings])))

(defn sanitize-card-field
  "Sanitize a card field's HTML server-side. Uses clean-html (the user-trust
   variant) — user-pasted <img> tags survive (pin-aware authoring may use them)."
  [html]
  #?(:clj (when html (cleaner/clean-html html))
     :cljs html))

;; Browser download helper
(defn trigger-download! [filename content]
  #?(:cljs
     (let [blob (js/Blob. #js [content] #js {:type "text/csv"})
           url (.createObjectURL js/URL blob)
           a (.createElement js/document "a")]
       (set! (.-href a) url)
       (set! (.-download a) filename)
       (.click a)
       (.revokeObjectURL js/URL url))))

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

(defn flush-editors!
  "Force-flush each [editor-atom target-atom] pair's Quill syntax-highlight
   spans into the DOM and reset! the resulting HTML into target-atom.
   Shared by EditCardModal's and AddCardModal's Save click handlers — see
   flush-syntax-tokens! (freememo.quill-field) for why this MUST run at
   click time rather than relying on QuillField's on-change."
  [pairs]
  (doseq [[!editor !target] pairs]
    (when-let [html (flush-syntax-tokens! @!editor)]
      (reset! !target html))))

;; ---------------------------------------------------------------------------
;; Modal Tab navigation — Tab moves focus editor→editor→Save→Cancel instead of
;; letting Quill insert a tab character. Toolbar buttons are excluded (they
;; carry ql-* classes, not .btn); the Basic/Cloze radios are not tab-stops.
;; ---------------------------------------------------------------------------

#?(:cljs
   (defn modal-tab-stops
     "Ordered focus targets for `container` (the .card-modal-inner div): the
      rendered Quill editors in DOM order, then the Save then Cancel button."
     [container]
     (let [editors (array-seq (.querySelectorAll container ".ql-editor"))
           save    (.querySelector container "button.btn-primary")
           cancel  (.querySelector container "button.btn-secondary")]
       (vec (concat editors (remove nil? [save cancel]))))))

#?(:cljs
   (defn handle-modal-tab!
     "Advance focus one tab-stop and consume the event so Quill cannot insert a
      tab. Shift+Tab reverses; both ends wrap. No-op when there are no stops.
      Pre : `e` is a keydown event on `container`.
      Post: on Tab, document.activeElement is the next/prev stop; the event is
            prevented and stopped (Quill's own Tab handler never fires)."
     [container e]
     (when (= (.-key e) "Tab")
       (let [stops (modal-tab-stops container)
             n     (count stops)]
         (when (pos? n)
           (.preventDefault e)
           (.stopPropagation e)
           (let [active (.-activeElement js/document)
                 idx    (reduce (fn [_ i]
                                  (let [s (nth stops i)]
                                    (when (or (= s active) (.contains s active))
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
      cleanup that removes the listener. Capture phase is required: Quill's
      keydown handler lives on the .ql-editor and would insert a tab before a
      bubble-phase listener could act."
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

;; Export modal — Forms5 (Form! + Input!/Checkbox!).
;; Scope and Card Type render via A1-fallback selects (atom-backed); their
;; values are merged into the parsed command at commit time. Pattern mirrors
;; bibliography_form.cljc CslTypeSelect.
(def ^:private export-scope-options
  [["Current Page" "Current Page"]
   ["Entire Doc" "Entire Document"]])

(def ^:private export-kind-options
  [["Both" "Both (Basic + Cloze)"]
   ["Basic" "Basic Only"]
   ["Cloze" "Cloze Only"]])

(e/defn ExportScopeSelect [!scope]
  (let [current (e/watch !scope)]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-4)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
        (dom/text "Scope:"))
      (dom/select
        (dom/props {:class "select" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !scope (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first export-scope-options)]
          (dom/option
            (dom/props {:value v :selected (= v current)})
            (dom/text label))))))
  (e/amb))

(e/defn ExportKindSelect [!kind]
  (let [current (e/watch !kind)]
    (dom/div
      (dom/props {:style {:margin-bottom "var(--sp-6)"}})
      (dom/label (dom/props {:style {:display "block" :margin-bottom "4px" :font-weight "500"}})
        (dom/text "Card Type:"))
      (dom/select
        (dom/props {:class "select" :style {:width "100%"}})
        (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil)
        (e/for [[v label] (e/diff-by first export-kind-options)]
          (dom/option
            (dom/props {:value v :selected (= v current)})
            (dom/text label))))))
  (e/amb))

(e/defn ExportCancelButton [!show-export]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (reset! !show-export false)) nil))
  (e/amb))

;; !show-export: atom bool  topic-id: current topic  root-topic-id: root topic  user-id: int
(e/defn ExportModal [!show-export topic-id root-topic-id user-id]
  (e/client
    ;; e/snapshot seeds A1 atoms once at mount — mirrors bibliography_form.cljc:296-297.
    (let [!scope (atom (e/snapshot "Current Page"))
          !kind (atom (e/snapshot "Both"))]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [_] (reset! !show-export false)) nil)
        (modal/ModalEscape (fn [] (reset! !show-export false)) "Export cards")
        (dom/div
          (dom/props {:class "modal-content modal-sm"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
            (dom/text "Export Cards"))
          (let [commits (forms/Form! {:use-header? false :header-text ""}
                          (e/fn Fields [{:keys [use-header? header-text]}]
                            (e/amb
                              (dom/div
                                (dom/props {:style {:margin-bottom "var(--sp-4)"}})
                                (dom/label
                                  (dom/props {:style {:display "flex" :align-items "center"
                                                      :gap "var(--sp-2)" :margin-bottom "var(--sp-2)"}})
                                  (forms/Checkbox! :use-header? (boolean use-header?))
                                  (dom/text "Add custom header to each card"))
                                (e/When use-header?
                                  (forms/Input! :header-text (or header-text "")
                                    :placeholder "e.g., Chapter 5: Accounting"
                                    :class "input input-full"
                                    :style {:padding "var(--sp-2)"})))

                              (ExportScopeSelect !scope)
                              (ExportKindSelect !kind)

                              (dom/div
                                (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "var(--sp-3)"}})
                                (e/amb
                                  (forms/SubmitButton! :label "Export"
                                    :class "btn btn-primary"
                                    :style {:font-weight "600" :order "1"})
                                  (ExportCancelButton !show-export)))))
                          :Parse (e/fn [fields _tempid]
                                   [`Export-cards
                                    {:topic-id topic-id
                                     :root-topic-id root-topic-id
                                     :user-id user-id
                                     :scope (e/watch !scope)
                                     :kind (e/watch !kind)
                                     :header-text (when (:use-header? fields)
                                                    (:header-text fields))}])
                          :type :command
                          :show-buttons false)]
            (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
              (let [opts (nth cmd 1)
                    kind (:kind opts)
                    results (if (= "Both" kind)
                              (e/server
                                (e/Offload
                                  #(mapv (fn [k] (cards/export-cards-csv (assoc opts :kind k)))
                                     ["basic" "cloze"])))
                              (e/server
                                (e/Offload
                                  #(vector (cards/export-cards-csv
                                             (assoc opts :kind (str/lower-case kind)))))))]
                (when (some? results)
                  (let [successes (filter :success results)]
                    (if (seq successes)
                      (do (doseq [r successes]
                            (trigger-download! (:filename r) (:csv r)))
                        (reset! !show-export false)
                        (token))
                      (token (or (some :error results) "Export failed")))))))))))))

;; Pre-prompt dialog
;; state map keys: :!show :!prompt-submit :!pre-prompt :!prompt-history
;;                 :!history-save-trigger :captured-selection :prompt-dialog-kind
(e/defn PromptDialog [state]
  (e/client
    (let [{:keys [!show !prompt-submit !pre-prompt !prompt-history
                  !history-save-trigger captured-selection prompt-dialog-kind]} state
          pre-prompt-value (e/watch !pre-prompt)
          !local-prompt (atom pre-prompt-value)
          local-prompt (e/watch !local-prompt)
          !primary-btn (atom nil)]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape (fn [] (reset! !show false))
          (str "Generate " (if (= prompt-dialog-kind "basic") "Basic" "Cloze") " Cards"))
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
            (dom/text "Generate " (if (= prompt-dialog-kind "basic") "Basic" "Cloze") " Cards"))
          (dom/div
            (dom/props {:style {:margin-bottom "20px"}})
            (dom/label
              (dom/props {:style {:display "block" :margin-bottom "var(--sp-2)" :font-size "14px"}})
              (dom/text "Pre-prompt (will be added to the system prompt):"))
            (let [prompt-history (e/watch !prompt-history)]
              (Typeahead !local-prompt (take 20 prompt-history) "e.g., Focus on accounting terminology..." nil true)))
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
                                          :kind prompt-dialog-kind})
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
  [!question !question-editor init-question-html question-key
   !items-html !items-editor init-items-html items-key !before !reveal modal-font]
  (e/client
    (let [before (e/watch !before)
          reveal (e/watch !reveal)]
      (dom/div
        (dom/label (dom/text "Question:"))
        (dom/div (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
          (QuillField init-question-html
            (fn [html] (reset! !question html))
            "e.g. What are the steps of X?" question-key !question-editor nil true))
        (dom/label (dom/text "List items (one per line):"))
        (dom/div (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
          (QuillField init-items-html
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

(e/defn EditCardModal [!editing-card user-id]
  (e/client
    (let [editing-card (e/watch !editing-card)
          card-id (:id editing-card)
          kind (:kind editing-card)
          init-q (or (:question editing-card) "")
          init-a (or (:answer editing-card) "")
          init-c (or (:cloze editing-card) "")
          init-ol (:overlapping editing-card)
          !question (atom init-q)
          !answer (atom init-a)
          !cloze (atom init-c)
          !ol-question (atom (or (:question init-ol) ""))
          !ol-question-editor (atom nil)
          !ol-items-html (atom (ol-items->html (:items init-ol)))
          !ol-items-editor (atom nil)
          !ol-before (atom (str (get-in init-ol [:settings :before] 1)))
          !ol-reveal (atom (get-in init-ol [:settings :reveal-all?] true))
          !q-editor (atom nil)
          !a-editor (atom nil)
          !c-editor (atom nil)
          question (e/watch !question)
          answer (e/watch !answer)
          cloze (e/watch !cloze)
          ol-question (e/watch !ol-question)
          ol-items-html (e/watch !ol-items-html)
          ol-before (e/watch !ol-before)
          ol-reveal (e/watch !ol-reveal)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")
          !primary-btn (atom nil)]
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
            (OverlappingPane !ol-question !ol-question-editor (or (:question init-ol) "") [:edit-ol-q card-id]
              !ol-items-html !ol-items-editor (ol-items->html (:items init-ol)) [:edit-ol card-id]
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
            (dom/props {:style {:display "flex" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
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
                    [(dom/On "click"
                       (fn [e]
                         (flush-editors! [[!q-editor !question] [!a-editor !answer] [!c-editor !cloze]
                                           [!ol-items-editor !ol-items-html] [!ol-question-editor !ol-question]])
                         e)
                       nil)
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
                (if (= kind "overlapping")
                  (let [items (quill-html->items ol-items-html)]
                    (if (empty? items)
                      (t "Add at least one list item")
                      (let [result (e/server (e/Offload #(cards/update-overlapping-card user-id card-id
                                                            {:question ol-question :items items
                                                             :settings {:before (ol-before-int ol-before)
                                                                        :after 0 :reveal-all? ol-reveal}})))]
                        (if (:success result)
                          (do (e/on-unmount #(reset! !editing-card nil))
                              (case (e/server (commands/bump! user-id :edit-card))
                                (t)))
                          (t (:error result))))))
                  (let [validation-error (cond
                                           (= kind "cloze") (cloze/validate cloze)
                                           (= kind "basic") (when (blank-html? question)
                                                              "Question can't be empty"))]
                    (if validation-error
                      (t validation-error)
                      (let [clean-q (e/server (sanitize-card-field question))
                            clean-a (e/server (sanitize-card-field answer))
                            clean-c (e/server (sanitize-card-field cloze))
                            fields (if (= kind "basic")
                                     {:question clean-q :answer clean-a}
                                     (cond-> {:cloze clean-c}
                                       (and clean-a (not (str/blank? clean-a)))
                                       (assoc :answer clean-a)))
                            result (e/server (e/Offload #(cards/update-card user-id card-id fields)))]
                        (if (:success result)
                          (do (e/on-unmount #(reset! !editing-card nil))
                              (case (e/server (commands/bump! user-id :edit-card))
                                (t)))
                          (t (:error result)))))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !editing-card nil)) nil))))))))


#?(:clj
   (defn insert-flashcards-safe! [rows]
     (try
       (db/insert-flashcards! rows)
       {:success true}
       (catch Exception e
         {:success false :error (.getMessage e)}))))

;; Optimistic add-card dispatch (freememo.optimistic). Pre: payload has
;; :topic-id :root-topic-id :kind :card-data. Post: cards saved (success toast,
;; overlay entry flipped :confirmed with real ids) or overlay entry flipped
;; :error + error toast. Effect + toast only — optimistic/execute! bumps the
;; registry :views and removes the command. Returns :done.
#?(:clj
   (defmethod opt/run-command! :add-card [user-id {:keys [id payload]}]
     (let [{:keys [topic-id root-topic-id kind card-data]} payload
           ;; bake? false — the Add-Card modal already inlined pinned images
           ;; into card-data via pins-prefill-html; baking would duplicate them.
           result (cards/save-cards user-id topic-id root-topic-id kind card-data false)]
       (if (:success result)
         (do (swap! (us/get-atom user-id :pending-cards) update id merge
               {:status :confirmed :real-ids (:ids result)})
             (toasts/push! user-id {:level :success :message "Card added"}))
         (do (swap! (us/get-atom user-id :pending-cards) update id merge
               {:status :error :error (:error result)})
             (toasts/push! user-id {:level :error
                                    :message (or (:error result) "Failed to add card")})))
       :done)))

;; Cancel affordance for the Add-Card Form!. MUST be :type "button" — a bare
;; <button> inside a <form> defaults to type=submit and would submit on click.
;; Returns empty e/amb so it contributes no edit to the form. Mirrors
;; ExportCancelButton.
(e/defn AddCancelButton [!show-add]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (reset! !show-add false)) nil))
  (e/amb))

;; Add card modal — uses topic-id and root-topic-id instead of doc-id + page-number
(e/defn AddCardModal [!show-add !card-kind !captured-selection topic-id root-topic-id user-id]
  (e/client
    (let [initial-text (or @!captured-selection "")
          prefill? (not (str/blank? initial-text))
          ;; Pinned images inline into the editors (model B): front pin → primary
          ;; (Question/Cloze), back pin → answer (Answer/Back-Extra). The Save
          ;; below passes bake? false so these are not appended a second time.
          pins-prefill (e/server (cards/pins-prefill-html topic-id))
          front-pin-html (:front pins-prefill)
          back-pin-html (:back pins-prefill)
          ;; Front (basic Question) and Cloze Text share !primary so typed
          ;; content carries across a Basic↔Cloze switch; Back/Extra already
          ;; share !answer. A captured selection prefills whichever renders;
          ;; pin HTML is appended after it to match bake ordering.
          init-primary (str (if prefill? initial-text "") front-pin-html)
          kind (e/watch !card-kind)
          !primary (atom init-primary)
          !answer (atom back-pin-html)
          !primary-editor (atom nil)
          !a-editor (atom nil)
          !ol-question (atom "")
          !ol-question-editor (atom nil)
          !ol-items-html (atom "")
          !ol-items-editor (atom nil)
          !ol-before (atom "1")
          !ol-reveal (atom true)
          primary (e/watch !primary)
          answer (e/watch !answer)
          ol-question (e/watch !ol-question)
          ol-items-html (e/watch !ol-items-html)
          ol-before (e/watch !ol-before)
          ol-reveal (e/watch !ol-reveal)
          card-font-sz (e/server (settings/get-card-font-size user-id))
          modal-font (str (or card-font-sz 14) "px")]
      (dom/div
        (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                            :background "transparent" :display "flex" :align-items "center"
                            :justify-content "center" :z-index "1000"
                            :pointer-events "none"}
                    :tabindex "-1"})
        (modal/ModalEscape (fn [] (reset! !show-add false)) "Add Card")
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
                flush-cleanup (attach-submit-flush! inner
                                (fn [] (flush-editors! [[!primary-editor !primary] [!a-editor !answer]
                                                        [!ol-items-editor !ol-items-html] [!ol-question-editor !ol-question]])))]
            (e/on-unmount (fn [] (when tab-cleanup (tab-cleanup)) (when flush-cleanup (flush-cleanup)))))
          (dom/h3 (dom/props {:style {:margin-top "0" :cursor "move" :user-select "none"
                                      :padding-bottom "var(--sp-2)" :margin-bottom "var(--sp-3)"
                                      :border-bottom "1px solid var(--color-border)"}})
            (dom/text "Add Card"))
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-3)" :margin-bottom "var(--sp-4)"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "basic"
                            :checked (= kind "basic")})
                (dom/On "change" (fn [_] (reset! !card-kind "basic")) nil))
              (dom/text "Basic"))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "cloze"
                            :checked (= kind "cloze")})
                (dom/On "change" (fn [_] (reset! !card-kind "cloze")) nil))
              (dom/text "Cloze"))
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "var(--sp-1)" :font-size "14px" :cursor "pointer"}})
              (dom/input
                (dom/props {:type "radio" :name "add-card-kind" :value "overlapping"
                            :checked (= kind "overlapping")})
                (dom/On "change" (fn [_] (reset! !card-kind "overlapping")) nil))
              (dom/text "Overlapping")))
          (cond
            (= kind "overlapping")
            (OverlappingPane !ol-question !ol-question-editor "" [:add-ol-q]
              !ol-items-html !ol-items-editor "" [:add-ol]
              !ol-before !ol-reveal modal-font)
            (= kind "basic")
            (dom/div
              (dom/label (dom/text "Question:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField primary
                  (fn [html] (reset! !primary html))
                  "Question..." [:add-q] !primary-editor nil true))
              (dom/label (dom/text "Answer:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField answer
                  (fn [html] (reset! !answer html))
                  "Answer..." [:add-a] !a-editor nil nil)))
            :else
            (dom/div
              (dom/label (dom/text "Cloze:"))
              (dom/div
                (dom/props {:style {:margin-bottom "var(--sp-3)" :font-size modal-font}})
                (QuillField primary
                  (fn [html] (reset! !primary html))
                  "Cloze text with {{c1::deletion}}..." [:add-c] !primary-editor true true))
              (dom/label (dom/text "Back Extra:"))
              (dom/div
                (dom/props {:style {:font-size modal-font}})
                (QuillField answer
                  (fn [html] (reset! !answer html))
                  "Optional back extra..." [:add-be] !a-editor nil nil))))
          ;; Forms5 Save. The command becomes one identity-stable element in the
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
                              (dom/props {:style {:display "flex" :justify-content "flex-end" :align-items "center" :gap "var(--sp-2)" :margin-top "var(--sp-4)"}})
                              (e/amb
                                (forms/SubmitButton! :label "Save" :class "btn btn-primary"
                                  :style {:border-radius "var(--radius-sm)" :order "1"})
                                (AddCancelButton !show-add))))
                          :Parse (e/fn [_ _]
                                   (case kind
                                     "overlapping"
                                     (let [items (quill-html->items ol-items-html)]
                                       (if (empty? items)
                                         (ex-info "Add at least one list item" {})
                                         [`Add-card {:kind "overlapping" :topic-id topic-id :root-topic-id root-topic-id
                                                     :ol {:question ol-question :items items
                                                          :settings {:before (ol-before-int ol-before)
                                                                     :after 0 :reveal-all? ol-reveal}}}]))
                                     "basic"
                                     (if (blank-html? primary)
                                       (ex-info "Question can't be empty" {})
                                       [`Add-card {:kind "basic" :topic-id topic-id :root-topic-id root-topic-id
                                                   :primary primary :answer answer}])
                                     (if-let [err (cloze/validate primary)]
                                       (ex-info err {})
                                       [`Add-card {:kind "cloze" :topic-id topic-id :root-topic-id root-topic-id
                                                   :primary primary :answer answer}])))
                          :type :command
                          :show-buttons false)]
            ;; Service: one branch per committed submit. e/snapshot freezes the
            ;; parsed command at commit time — Forms5 sequential mode tracks
            ;; parsed-form-v LIVE, so without the snapshot a late editor mutation
            ;; during the enqueue round-trip would re-fire the enqueue. Sanitize
            ;; the frozen HTML server-side (basic/cloze only; overlapping is
            ;; stored verbatim, matching prior behavior), enqueue optimistically,
            ;; close on ack.
            (e/for [[t cmd] (e/diff-by first (e/as-vec commits))]
              (let [{:keys [kind topic-id root-topic-id primary answer ol]} (nth (e/snapshot cmd) 1)
                    card-data (case kind
                                "basic" [{:q (e/server (sanitize-card-field primary))
                                          :a (e/server (sanitize-card-field answer))}]
                                "overlapping" [ol]
                                [(let [clean-a (e/server (sanitize-card-field answer))]
                                   (cond-> {:c (e/server (sanitize-card-field primary))}
                                     (and clean-a (not (str/blank? clean-a)))
                                     (assoc :a clean-a)))])]
                (case (e/server (opt/enqueue-add-card! user-id
                                  {:topic-id topic-id :root-topic-id root-topic-id
                                   :kind kind :card-data card-data}))
                  (do (e/on-unmount #(reset! !show-add false)) (t)))))))))))

