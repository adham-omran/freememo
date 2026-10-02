(ns freememo.document-options
  "\"Document options\" — a sheet of per-document and per-item settings that
   save as they change (plans/document-options-autosave-sheet.md).

   Four groups: Review (priority, learning queue), Card generation (approved
   facts, learning goal, custom prompt), Text extraction (PDF only) and
   Bibliography. Every field is a Forms5 field, or forms-ext/Select!, whose
   edit reaches ONE service in DocumentOptionsBody. The service persists the
   edit through apply-edit!* and spends its token.

   Closing is a request. Done, Escape and a backdrop click set
   !close-requested, and the modal closes once no edit is in flight, so the
   frame that holds a save is never torn down under it. A failed save drops
   the request and keeps the modal open on the error.

   The viewer mounts the modal in DocumentToolbars, outside the content
   toolbar, whose collapse tiers hide every .icon-label inside it. The library
   row mounts its own copy in <body>."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.forms-ext :as forms-ext]
   [freememo.icons :as icons]
   [freememo.copy-text :as copy]
   [freememo.modal-shell :as modal]
   [freememo.priority-control :refer [PriorityControl get-topic-priority*]]
   [freememo.bibliography-button :as bib-btn]
   [freememo.commands :as commands]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.anki-sync-helpers :as anki-helpers])
   #?(:clj [freememo.bibliography-form :as bibform])
   #?(:clj [freememo.settings :as settings])))

(def ^:private style-options [["ask"    "Ask every time (compare A vs B)"]
                              ["client" "Client (PDF.js, in-browser)"]
                              ["remote" "Remote (PDFBox, server)"]])

(def ^:private use-facts-options [[""      "Default"]
                                  ["true"  "On"]
                                  ["false" "Off"]])

;; ---------------------------------------------------------------------------
;; Server
;; ---------------------------------------------------------------------------

(defn field-values*
  "Server: the saved value of each autosaving field, as the string its control
   shows, plus the account default that the facts hint names.
   Pre:  targets is a document-options-targets/targets map.
   Post: {:goal :prompt :use-facts :style :default-facts?}. :style is nil for a
         non-PDF topic."
  [user-id {:keys [root-id item-id]} pdf?]
  #?(:clj {:goal (or (settings/get-learning-goal user-id root-id) "")
           :prompt (or (settings/get-custom-prompt user-id item-id) "")
           :use-facts (case (settings/get-use-facts-for-document user-id root-id)
                        true "true"
                        false "false"
                        "")
           :style (when pdf? (or (copy/get-extract-style* nil user-id root-id) "ask"))
           :default-facts? (boolean (settings/get-use-facts-for-card-generation user-id))}
     :cljs nil))

#?(:clj
   (defn- set-learning-queue!
     "Dismiss or undismiss the document and its subtree, then bump the
      command's channels. One plain fn, so the write precedes the bump."
     [user-id root-id action]
     (if (some? (if (= action :dismiss)
                  (db/dismiss-topic! user-id root-id)
                  (db/undismiss-topic! user-id root-id)))
       (do (commands/bump! user-id action)
           {:success true})
       {:success false :error "Could not change the learning queue."})))

(defn queue-dismissed?*
  "Server: true when the document root is dismissed from the learning queue.
   `_refresh` is a reactive dep, so the row re-reads after a dismiss bumps it."
  [_refresh root-id]
  #?(:clj (db/topic-dismissed? root-id)
     :cljs false))

#?(:clj (defonce ^:private edit-seq (java.util.concurrent.atomic.AtomicLong.)))

(defn next-edit-seq!*
  "Server: a number larger than every one returned before. The service takes
   one each time an edit's value changes, in the order the values arrive, so
   a later value always holds the larger number. `_v` is the reactive dep."
  [_v]
  #?(:clj (.incrementAndGet ^java.util.concurrent.atomic.AtomicLong edit-seq)
     :cljs nil))

;; One lock per field, and the number of the newest write that field took.
;; Cancelling an e/Offload cannot stop a JDBC write that has started, so the
;; save of older text can still be running when the save of newer text starts.
#?(:clj (defonce ^:private edit-locks (java.util.concurrent.ConcurrentHashMap.)))
#?(:clj (defonce ^:private !newest-written (atom {})))

#?(:clj
   (defn- write-if-newest!
     "Run write! unless a newer edit of the same field has already written.
      Pre:  edit-seq comes from next-edit-seq!*.
      Post: the field's stored value is the one from the largest edit-seq that
            wrote successfully, whatever order the threads reach the lock in.
            A skipped write returns {:success true}: a newer value is stored."
     [field-key edit-seq write!]
     (locking (.computeIfAbsent ^java.util.concurrent.ConcurrentHashMap edit-locks field-key
                (reify java.util.function.Function (apply [_ _] (Object.))))
       (if (< edit-seq (get @!newest-written field-key 0))
         {:success true}
         (let [result (write!)]
           (when (:success result)
             (swap! !newest-written assoc field-key edit-seq))
           result)))))

(defn apply-edit!*
  "Server: persist one edit from the modal.
   Pre:  k is a field key the modal emits. v is the value its control holds:
         a string for the text fields and selects, :dismiss or :undismiss for
         :queue. edit-seq is next-edit-seq!* for this value.
   Post: {:success true} | {:success false :error msg}. Of two edits of one
         field, the one with the larger edit-seq is the one stored."
  [user-id {:keys [root-id item-id]} k v edit-seq]
  #?(:clj (write-if-newest! [user-id k (if (= k :prompt) item-id root-id)] edit-seq
            (fn []
              (case k
                :goal      (settings/save-learning-goal user-id root-id v)
                :prompt    (settings/save-custom-prompt user-id item-id v)
                :use-facts (settings/save-use-facts-for-document user-id root-id
                             (case v "true" true "false" false nil))
                :style     (copy/save-style!* user-id root-id v)
                :queue     (set-learning-queue! user-id root-id v)
                {:success false :error (str "Unknown field " k)})))
     :cljs nil))

(defn citation-line*
  "Server: the Bibliography row's citation, or nil when the topic resolves to
   no source. `_refresh` re-reads it after a bibliography edit.
   Post: {:text citation :own? true when the topic owns the source} | nil."
  [_refresh user-id bib-id]
  #?(:clj (when-let [text (bibform/get-topic-citation* nil user-id bib-id)]
            {:text text :own? (bib-btn/has-source?* nil user-id bib-id)})
     :cljs nil))

(defn copy-biblio-to-extracts!*
  "Server: copy the item's effective bibliography into every descendant extract
   (overwrite-all), then bump :push-biblio.
   Pre:  user-id, topic-id non-nil.
   Post: {:ok true :count n} | {:ok false :error kw}.

   The snapshot fan-out is this fn's obligation, stated at
   `db/push-bibliography-to-descendants!`. Overwrite-all means a descendant that
   carried a different citation now carries this one, so its cards' Anki notes are
   out of date — and a `sources` write touches no flashcards row, so nothing else
   would say so."
  [user-id topic-id]
  #?(:clj (let [r (db/push-bibliography-to-descendants! user-id topic-id)]
            (when (:ok r)
              (db/set-anki-bibliography-snapshot-for-topics! user-id (:topic-ids r)
                (anki-helpers/format-bibliography-html (:csl r)))
              (commands/bump! user-id :push-biblio))
            r)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Client
;; ---------------------------------------------------------------------------

(defn settle-edit!
  "Spend an edit's token on the server's result. On success the saved value
   becomes the field's value, so the field does not revert on blur. On failure
   the error shows under the field and the close request is dropped.
   One plain fn, so the atoms change before the token spends."
  [t k v result !values !errors !saved !close-requested]
  (if (:success result)
    (do (swap! !values assoc k v)
        (swap! !errors dissoc k)
        (swap! !saved conj k)
        (t))
    (let [msg (or (:error result) "Could not save.")]
      (swap! !errors assoc k msg)
      (swap! !saved disj k)
      (reset! !close-requested false)
      (t msg))))

(defn- field-id [k] (str "doc-options-" (name k)))

(e/defn GroupHeading [title]
  (dom/div (dom/props {:class "settings-group__heading"})
    (dom/h4 (dom/props {:class "settings-group__title"}) (dom/text title))))

(e/defn RowText
  "The left side of a row: label, hint, and the field's error when it has one."
  [label hint for-id error]
  (dom/div (dom/props {:class "setting-row__text"})
    (if for-id
      (dom/label (dom/props {:class "label" :for for-id}) (dom/text label))
      (dom/div (dom/props {:class "label"}) (dom/text label)))
    (dom/div (dom/props {:class "hint" :id (when for-id (str for-id "-hint"))})
      (dom/text hint))
    (when error
      (dom/div (dom/props {:class "hint doc-options__error" :role "alert"})
        (dom/text error)))))

(e/defn QueueRow
  "Learning queue membership of the whole document. Returns the Dismiss or
   Undismiss button's edit, {:queue :dismiss|:undismiss}."
  [user-id root-id error]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          dismissed? (e/server (e/Offload #(queue-dismissed?* refresh root-id)))]
      (dom/div (dom/props {:class "setting-row"})
        (RowText "Learning queue"
          (if dismissed?
            "Dismissed. Its extracts are out of the queue too."
            "In the learning queue.")
          nil error)
        (dom/div (dom/props {:class "setting-row__control"})
          (forms/Button! {:queue (if dismissed? :undismiss :dismiss)}
            :label (if dismissed? "Undismiss" "Dismiss")
            :class "btn btn-sm btn-secondary"))))))

(e/defn ReviewGroup [user-id root-id review-id viewer? errors]
  (e/client
    (dom/section (dom/props {:class "settings-group"})
      (GroupHeading "Review")
      ;; The stepper saves each step itself (NumberStepper's Save); its writes
      ;; do not pass through the service.
      (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))]
        (dom/div (dom/props {:class "setting-row"})
          (RowText "Priority" "0 comes first, 100 last." nil nil)
          (dom/div (dom/props {:class "setting-row__control"})
            (PriorityControl user-id review-id
              (e/server (e/Offload #(get-topic-priority* refresh review-id)))))))
      ;; The library row menu has its own Dismiss item.
      (if viewer? (QueueRow user-id root-id (:queue errors)) (e/amb)))))

(e/defn FieldStatus [saving? saved? error]
  (dom/div (dom/props {:class (if error
                                "doc-options__status doc-options__error"
                                "doc-options__status")
                       :aria-live "polite"})
    (dom/text (cond error error
                    saving? "Saving…"
                    saved? "Saved"
                    :else ""))))

(e/defn TextRow
  "A stacked textarea row. Returns the Input!'s edit."
  [k label hint maxlength v saved? error]
  (let [id (field-id k)]
    (dom/div (dom/props {:class "setting-row setting-row--stacked"})
      (dom/label (dom/props {:class "label" :for id}) (dom/text label))
      (dom/div (dom/props {:class "hint" :id (str id "-hint")}) (dom/text hint))
      (let [edit (forms/Input! k v :as :textarea
                   :id id :class "input" :rows 3 :maxlength maxlength
                   :aria-describedby (str id "-hint"))]
        (FieldStatus (pos? (e/Count edit)) saved? error)
        edit))))

(e/defn SelectRow
  "A label-left, select-right row. Returns the Select!'s edit."
  [k label hint options v error]
  (let [id (field-id k)]
    (dom/div (dom/props {:class "setting-row"})
      (RowText label hint id error)
      (dom/div (dom/props {:class "setting-row__control"})
        (forms-ext/Select! k v options
          :id id :class "input" :aria-describedby (str id "-hint"))))))

(e/defn CardGenerationGroup [values saved errors]
  (dom/section (dom/props {:class "settings-group"})
    (GroupHeading "Card generation")
    (e/amb
      (SelectRow :use-facts "Use approved facts"
        (str "Default follows your account setting, which is "
          (if (:default-facts? values) "On" "Off") ".")
        use-facts-options (:use-facts values) (:use-facts errors))
      (TextRow :goal "Learning goal"
        "Why you study this. The assistant and card generation use it. Applies to the whole document."
        2000 (:goal values) (contains? saved :goal) (:goal errors))
      (TextRow :prompt "Custom prompt"
        "Extra card-generation instructions. Applies to this item and everything under it."
        5000 (:prompt values) (contains? saved :prompt) (:prompt errors)))))

(e/defn TextExtractionGroup [values errors]
  (dom/section (dom/props {:class "settings-group"})
    (GroupHeading "Text extraction")
    (SelectRow :style "Quick text extraction"
      "How Copy text reads this PDF. Applies to the whole document."
      style-options (:style values) (:style errors))))

(e/defn CopyToExtractsButton
  "Copy this item's effective bibliography into every descendant extract
   (overwrite-all), after a confirm. Disabled when the item has no effective
   source or while a roundtrip is in flight; flashes green on success, shows
   the error text on failure.

   Its own token path, not the modal's service: forms/Button! creates the token
   on the click itself, so it cannot ask the confirm question first
   (plans/document-options-autosave-sheet.md 3.7.2)."
  [user-id bib-id enabled?]
  (e/client
    (dom/button
      (let [!flash-success (atom false)
            flash-success? (e/watch !flash-success)
            click-event (dom/On "click"
                          (fn [e]
                            (when (js/confirm "Overwrite the bibliography of all descendant extracts with this item's bibliography? This cannot be undone.")
                              e))
                          nil)
            [t ?error] (e/Token click-event)]
        (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                    :disabled (or (not enabled?) (some? t))
                    :aria-busy (some? t)
                    :style (when flash-success?
                             {:background "var(--color-success-light)"
                              :border-color "var(--color-success)"
                              :color "var(--color-success-dark)"
                              :transition "background-color 0.2s ease, border-color 0.2s ease, color 0.2s ease"})})
        (tooltip/Tooltip! "Copy this bibliography to all descendant extracts")
        (icons/Icon :download :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Copy to extracts"))
        (when ?error
          (dom/div
            (dom/props {:class "doc-options__error" :style {:font-size "11px" :margin-top "4px"}})
            (dom/text ?error)))
        (when t
          (let [result (e/server (e/Offload #(copy-biblio-to-extracts!* user-id bib-id)))]
            (when (some? result)
              (if (:ok result)
                (do (e/on-unmount
                      (fn []
                        (reset! !flash-success true)
                        (js/setTimeout #(reset! !flash-success false) 1200)))
                    (t))
                (t (str "Failed: " (name (or (:error result) :error))))))))))))

(e/defn BibliographyGroup
  "Citation line, then Edit (viewer only), Refetch and Copy to extracts. Edit
   closes this modal and opens the bibliography modal mounted beside it.
   Pre: bib-id is the item's bibliography owner — the root, for a PDF page."
  [user-id bib-id !open !show-bib viewer?]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          citation (e/server (e/Offload #(citation-line* refresh user-id bib-id)))]
      (dom/section (dom/props {:class "settings-group"})
        (GroupHeading "Bibliography")
        (dom/p (dom/props {:class "doc-options__citation"})
          (dom/text (cond (nil? citation) "No bibliography"
                          (:own? citation) (:text citation)
                          :else (str (:text citation) " (inherited)"))))
        (dom/div (dom/props {:class "doc-options__actions"})
          (when viewer?
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
              (tooltip/Tooltip! "Edit bibliography")
              (icons/Icon :book-open :size 16)
              (dom/span (dom/props {:class "icon-label"}) (dom/text "Edit"))
              (dom/On "click" (fn [_] (reset! !open false) (reset! !show-bib true)) nil)))
          (bib-btn/BibliographyButton user-id bib-id (boolean (:own? citation)) nil)
          (CopyToExtractsButton user-id bib-id (some? citation)))
        (e/amb)))))

(e/defn DocumentOptionsBody
  "The groups, the footer and the edit service, for one set of targets.
   Pre:  targets is a document-options-targets/targets map. `values` is
         field-values* for those targets, read once at mount.
   Post: every field's edit reaches the e/for at the end, which persists it
         through apply-edit!* and spends its token through settle-edit!.
   Invariant: the modal closes only while no edit is held (busy? false), so a
         save in flight is never torn down."
  [user-id targets pdf? values !open !show-bib viewer? !close-requested]
  (e/client
    (let [!values (atom values)
          values (e/watch !values)
          !errors (atom {})
          errors (e/watch !errors)
          !saved (atom #{})
          saved (e/watch !saved)
          close-requested? (e/watch !close-requested)
          edits (e/amb
                  (ReviewGroup user-id (:root-id targets) (:review-id targets) viewer? errors)
                  (CardGenerationGroup values saved errors)
                  (if pdf? (TextExtractionGroup values errors) (e/amb))
                  (BibliographyGroup user-id (:bib-id targets) !open !show-bib viewer?))
          busy? (pos? (e/Count edits))]
      (dom/div (dom/props {:class "doc-options__footer"})
        (dom/button
          (dom/props {:class "btn btn-primary" :type "button"
                      :aria-busy (and close-requested? busy?)})
          (dom/text "Done")
          (dom/On "click" (fn [_] (reset! !close-requested true)) nil)))
      (when (and close-requested? (not busy?))
        (reset! !open false))
      ;; The service. A button's command is snapshotted at the click: the bump
      ;; that follows a dismiss flips the button's command before the token
      ;; spends. A text field's value is not, because typing during a save
      ;; must reach the save that follows.
      ;;
      ;; Input! keeps one token while the user types, so `v` can change while
      ;; a save is in flight, and the result of the older save can arrive after
      ;; it. That result must not spend the token: the spend would tear down
      ;; the save of the newer value, and the database would keep the older
      ;; one. Each result names the value it saved, and only the result for
      ;; the current `v` settles the edit. The save of the older value can
      ;; still COMMIT after the newer one, since a cancel cannot stop a
      ;; started JDBC write; the edit-seq lets apply-edit!* drop that write.
      (e/for [[t edit] (e/diff-by first (e/as-vec edits))]
        (let [[k v] (first edit)
              v (if (= k :queue) (e/snapshot v) v)
              result (e/server
                       (let [edit-seq (next-edit-seq!* v)]
                         (e/Offload #(assoc (apply-edit!* user-id targets k v edit-seq)
                                       :saved-value v))))]
          (when (and (some? result) (= v (:saved-value result)))
            (settle-edit! t k v result !values !errors !saved !close-requested)))))))

(e/defn DocumentOptionsModal
  "The Document options dialog.
   Pre:  targets is a document-options-targets/targets map. pdf? adds the Text
         extraction group. viewer? adds Dismiss and Edit, which need the
         viewer's bibliography modal (!show-bib) — the library row passes false
         and nil.
   Post: Done, Escape and a backdrop click request the close; !open goes false
         once no save is in flight."
  [user-id targets pdf? !open !show-bib viewer?]
  (e/client
    (let [!close-requested (atom false)
          request-close! (fn [] (reset! !close-requested true))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal/ModalEscape request-close! "Document options")
        (dom/On "click" (fn [e] (when (= (.-target e) (.-currentTarget e)) (request-close!))) nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg doc-options"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:class "doc-options__title"}) (dom/text "Document options"))
          ;; Keyed on the targets: a navigation while the modal is open
          ;; remounts the body, so no field shows or saves another topic's value.
          (e/for-by identity [_k [targets]]
            (let [values (e/server (e/Offload #(field-values* user-id targets pdf?)))]
              (DocumentOptionsBody user-id targets pdf? (e/snapshot values)
                !open !show-bib viewer? !close-requested))))))))

(e/defn DocumentOptionsTrigger
  "The button that opens Document options by setting `!open`.
   class-name restyles it (the library row menu passes a menu-item class);
   nil gives the toolbar's secondary button. on-open! (nil-able) runs first,
   for a caller that closes its own menu."
  [!open class-name on-open!]
  (e/client
    (dom/button
      (dom/props {:class (or class-name "btn btn-sm btn-secondary") :type "button"
                  :aria-label "Document options"})
      (tooltip/Tooltip! "Per-document options")
      (icons/Icon :settings :size 16)
      (dom/span (dom/props {:class "icon-label"}) (dom/text "Document options"))
      ;; stop propagation so a click doesn't reach an ancestor row handler
      ;; (the library tree row navigates on click); inert in the toolbar.
      (dom/On "click" (fn [e]
                        (.stopPropagation e)
                        (when on-open! (on-open!))
                        (reset! !open true))
        nil))))
