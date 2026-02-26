(ns electric-starter-app.anki-sync
  "Full-stack Anki sync component — client-side AnkiConnect calls + modal UI."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [electric-starter-app.anki-sync-server :as sync])))

;; ---------------------------------------------------------------------------
;; Client-side AnkiConnect wrapper
;; ---------------------------------------------------------------------------

#?(:cljs
   (defn anki-call!
     "Call AnkiConnect API. Returns a Promise resolving to the result value."
     [action params]
     (-> (js/fetch "http://127.0.0.1:8765"
           (clj->js {:method "POST"
                     :headers {"Content-Type" "application/json"}
                     :body (js/JSON.stringify
                             (clj->js (cond-> {:action action :version 6}
                                        params (assoc :params params))))}))
         (.then (fn [resp]
                  (when-not (.-ok resp)
                    (throw (js/Error. (str "HTTP " (.-status resp)))))
                  (.json resp)))
         (.then (fn [json]
                  (let [result (js->clj json :keywordize-keys true)]
                    (when (:error result)
                      (throw (js/Error. (str (:error result)))))
                    (:result result)))))))

#?(:cljs
   (defn fetch-anki-config!
     "Fetch decks + models from Anki. Returns Promise of {:decks [...] :models [...]}."
     []
     (-> (js/Promise.all
           #js [(anki-call! "deckNames" nil)
                (anki-call! "modelNames" nil)])
         (.then (fn [results]
                  {:decks (vec (js->clj (aget results 0)))
                   :models (vec (js->clj (aget results 1)))})))))

#?(:cljs
   (defn build-note
     "Build an AnkiConnect note map for addNote."
     [card deck-name basic-model cloze-model basic-fields cloze-fields doc-id allow-dupes]
     (let [kind (:flashcards/kind card)
           basic? (= kind "basic")
           model (if basic? basic-model cloze-model)
           fields (if basic? basic-fields cloze-fields)
           field-map (if basic?
                       {(first fields) (or (:flashcards/question card) "")
                        (second fields) (or (:flashcards/answer card) "")}
                       {(first fields) (or (:flashcards/cloze card) "")})]
       (cond-> {:deckName deck-name
                :modelName model
                :fields field-map
                :tags ["card-maker" (str "doc-" doc-id)]}
         (not allow-dupes) (assoc :options {:allowDuplicate false
                                            :duplicateScope "deck"})
         allow-dupes       (assoc :options {:allowDuplicate true})))))

#?(:cljs
   (defn do-anki-push!
     "Push cards to Anki. Returns Promise resolving to result map."
     [cards deck-name basic-model cloze-model basic-fields cloze-fields doc-id allow-dupes]
     (let [new-cards (filter #(nil? (:flashcards/anki_note_id %)) cards)
           existing-cards (filter #(some? (:flashcards/anki_note_id %)) cards)]
       (->
         (.resolve js/Promise {:pairs [] :updated 0 :skipped [] :errors []})
         (.then
           (fn [acc]
             (reduce
               (fn [promise-chain card]
                 (.then promise-chain
                   (fn [result]
                     (let [note (build-note card deck-name basic-model cloze-model
                                  basic-fields cloze-fields doc-id allow-dupes)]
                       (-> (anki-call! "addNote" {:note note})
                           (.then (fn [note-id]
                                    (if note-id
                                      (update result :pairs conj
                                        {:card-id (:flashcards/id card)
                                         :anki-note-id note-id})
                                      (update result :skipped conj
                                        {:card-id (:flashcards/id card)
                                         :reason "No note ID returned"}))))
                           (.catch (fn [err]
                                     (update result :errors conj
                                       {:card-id (:flashcards/id card)
                                        :error (.-message err)}))))))))
               (.resolve js/Promise acc)
               new-cards)))
         (.then
           (fn [acc]
             (reduce
               (fn [promise-chain card]
                 (.then promise-chain
                   (fn [result]
                     (let [kind (:flashcards/kind card)
                           basic? (= kind "basic")
                           fields (if basic? basic-fields cloze-fields)
                           field-map (if basic?
                                       {(first fields) (or (:flashcards/question card) "")
                                        (second fields) (or (:flashcards/answer card) "")}
                                       {(first fields) (or (:flashcards/cloze card) "")})]
                       (-> (anki-call! "updateNoteFields"
                             {:note {:id (:flashcards/anki_note_id card)
                                     :fields field-map}})
                           (.then (fn [_]
                                    (-> result
                                        (update :updated inc)
                                        (update :pairs conj
                                          {:card-id (:flashcards/id card)
                                           :anki-note-id (:flashcards/anki_note_id card)}))))
                           (.catch (fn [err]
                                     (update result :errors conj
                                       {:card-id (:flashcards/id card)
                                        :error (.-message err)}))))))))
               (.resolve js/Promise acc)
               existing-cards)))))))

#?(:cljs
   (defn do-anki-pull!
     "Pull edits from Anki for previously-synced cards. Returns Promise of updates."
     [cards basic-fields cloze-fields]
     (let [synced (filter #(some? (:flashcards/anki_note_id %)) cards)
           note-ids (mapv :flashcards/anki_note_id synced)
           id->card (into {} (map (fn [c] [(:flashcards/anki_note_id c) c]) synced))]
       (if (empty? note-ids)
         (.resolve js/Promise {:updates []})
         (-> (anki-call! "notesInfo" {:notes note-ids})
             (.then
               (fn [notes]
                 (let [updates
                       (reduce
                         (fn [acc anki-note]
                           (let [anki-note (js->clj anki-note :keywordize-keys true)
                                 note-id (:noteId anki-note)
                                 card (get id->card note-id)]
                             (if-not card
                               acc
                               (let [kind (:flashcards/kind card)
                                     basic? (= kind "basic")
                                     fields-map (:fields anki-note)
                                     get-field (fn [fname]
                                                 (:value (get fields-map (keyword fname))))
                                     update-map
                                     (if basic?
                                       (let [q (get-field (first basic-fields))
                                             a (get-field (second basic-fields))
                                             local-q (or (:flashcards/question card) "")
                                             local-a (or (:flashcards/answer card) "")]
                                         (when (or (not= q local-q) (not= a local-a))
                                           {:card-id (:flashcards/id card)
                                            :question q :answer a}))
                                       (let [c (get-field (first cloze-fields))
                                             local-c (or (:flashcards/cloze card) "")]
                                         (when (not= c local-c)
                                           {:card-id (:flashcards/id card)
                                            :cloze c})))]
                                 (if update-map (conj acc update-map) acc)))))
                         []
                         (js->clj notes))]
                   {:updates updates}))))))))

;; ---------------------------------------------------------------------------
;; Cross-platform wrappers — avoid #?(:cljs) inside e/defn bodies.
;; Top-level defn with #?(:cljs/:clj) body is safe (no frame slot impact).
;; ---------------------------------------------------------------------------

(defn run-fetch-config!
  "Fetch Anki config (decks + models) and populate atoms."
  [!decks !models !selected-deck !basic-model !cloze-model !conn-status !conn-error]
  #?(:cljs
     (-> (fetch-anki-config!)
         (.then (fn [{:keys [decks models]}]
                  (reset! !decks decks)
                  (reset! !models models)
                  (when (seq decks) (reset! !selected-deck (first decks)))
                  (when (seq models)
                    (reset! !basic-model (first models))
                    (reset! !cloze-model (first models)))
                  (reset! !conn-status :connected)))
         (.catch (fn [err]
                   (reset! !conn-error (str "Cannot connect to Anki: " (.-message err)))
                   (reset! !conn-status :error))))
     :clj nil))

(defn run-fetch-fields!
  "Fetch model field names from Anki and store in atom."
  [model-name !fields-atom]
  #?(:cljs
     (when model-name
       (-> (anki-call! "modelFieldNames" {:modelName model-name})
           (.then (fn [fields] (reset! !fields-atom (vec (js->clj fields)))))
           (.catch (fn [_] (reset! !fields-atom [])))))
     :clj nil))

(defn run-push!
  "Execute push to Anki and update state atoms with results."
  [cards deck basic-model cloze-model basic-fields cloze-fields
   doc-id allow-dupes !sync-result !push-pairs !sync-error !sync-phase]
  #?(:cljs
     (-> (do-anki-push! cards deck basic-model cloze-model
           basic-fields cloze-fields doc-id allow-dupes)
         (.then (fn [result]
                  (reset! !sync-result result)
                  (if (seq (:pairs result))
                    (do (reset! !push-pairs (:pairs result))
                        (reset! !sync-phase :recording))
                    (reset! !sync-phase :done))))
         (.catch (fn [err]
                   (reset! !sync-error (.-message err))
                   (reset! !sync-phase :error))))
     :clj nil))

(defn run-pull!
  "Execute pull from Anki and update state atoms with results."
  [cards basic-fields cloze-fields
   !sync-result !pull-updates !sync-error !sync-phase]
  #?(:cljs
     (-> (do-anki-pull! cards basic-fields cloze-fields)
         (.then (fn [result]
                  (if (empty? (:updates result))
                    (do (reset! !sync-result result)
                        (reset! !sync-phase :done))
                    (do (reset! !sync-result result)
                        (reset! !pull-updates (:updates result))
                        (reset! !sync-phase :recording)))))
         (.catch (fn [err]
                   (reset! !sync-error (.-message err))
                   (reset! !sync-phase :error))))
     :clj nil))

(defn escape-key?
  "Cross-platform Escape key check for DOM key events."
  [e]
  #?(:cljs (= (.-key e) "Escape")
     :clj false))

;; ---------------------------------------------------------------------------
;; Sub-components (split to keep frame slot count small)
;; ---------------------------------------------------------------------------

(e/defn AnkiSyncForm
  "The connected-state form: scope, deck, model selection, field mapping."
  [!scope decks !selected-deck models
   !basic-model basic-fields !cloze-model cloze-fields !allow-dupes]
  (e/client
    ;; Scope
    (dom/div
      (dom/props {:style {:margin-bottom "12px"}})
      (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
        (dom/text "Scope"))
      (dom/select
        (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px" :font-size "14px"}})
        (dom/option (dom/props {:value "Current Page"}) (dom/text "Current Page"))
        (dom/option (dom/props {:value "Entire Doc"}) (dom/text "Entire Document"))
        (reset! !scope (dom/On "change" (fn [e] (-> e .-target .-value)) "Current Page"))))

    ;; Deck
    (dom/div
      (dom/props {:style {:margin-bottom "12px"}})
      (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
        (dom/text "Deck"))
      (dom/select
        (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                            :font-size "14px" :width "100%"}})
        (e/for [d (e/diff-by {} decks)]
          (dom/option (dom/props {:value d}) (dom/text d)))
        (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
          (when (some? v) (reset! !selected-deck v)))))

    ;; Basic Note Type
    (dom/div
      (dom/props {:style {:margin-bottom "12px"}})
      (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
        (dom/text "Note Type (Basic)"))
      (dom/select
        (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                            :font-size "14px" :width "100%"}})
        (e/for [m (e/diff-by {} models)]
          (dom/option (dom/props {:value m}) (dom/text m)))
        (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
          (when (some? v) (reset! !basic-model v))))
      (when (seq basic-fields)
        (dom/div
          (dom/props {:style {:font-size "12px" :color "#666" :margin-top "4px"}})
          (dom/text (str "question \u2192 " (first basic-fields)
                         ", answer \u2192 " (second basic-fields))))))

    ;; Cloze Note Type
    (dom/div
      (dom/props {:style {:margin-bottom "12px"}})
      (dom/label (dom/props {:style {:font-weight "600" :font-size "13px" :display "block" :margin-bottom "4px"}})
        (dom/text "Note Type (Cloze)"))
      (dom/select
        (dom/props {:style {:padding "4px 8px" :border "1px solid #ccc" :border-radius "4px"
                            :font-size "14px" :width "100%"}})
        (e/for [m (e/diff-by {} models)]
          (dom/option (dom/props {:value m}) (dom/text m)))
        (let [v (dom/On "change" (fn [e] (-> e .-target .-value)) nil)]
          (when (some? v) (reset! !cloze-model v))))
      (when (seq cloze-fields)
        (dom/div
          (dom/props {:style {:font-size "12px" :color "#666" :margin-top "4px"}})
          (dom/text (str "cloze \u2192 " (first cloze-fields))))))

    ;; Allow duplicates
    (dom/div
      (dom/props {:style {:margin-bottom "16px"}})
      (dom/label
        (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
        (dom/input
          (dom/props {:type "checkbox" :checked (e/watch !allow-dupes)})
          (let [v (dom/On "change" (fn [e] (-> e .-target .-checked)) nil)]
            (when (some? v) (reset! !allow-dupes v))))
        (dom/text "Allow duplicates")))))

(e/defn AnkiSyncStatus
  "Sync status display and action buttons."
  [sync-phase sync-result sync-error !show-modal !sync-phase !sync-result !sync-error !push-pairs !pull-updates]
  (e/client
    ;; Status display
    (when sync-phase
      (dom/div
        (dom/props {:style {:margin-bottom "16px" :padding "12px" :background "#f8f9fa"
                            :border-radius "4px" :font-size "13px"}})
        (cond
          (= sync-phase :pushing)   (dom/text "Pushing cards to Anki...")
          (= sync-phase :pulling)   (dom/text "Pulling edits from Anki...")
          (= sync-phase :recording) (dom/text "Saving to database...")
          (= sync-phase :error)
          (dom/div
            (dom/props {:style {:color "#dc3545"}})
            (dom/text (str "Error: " (or sync-error "Unknown error"))))
          (= sync-phase :done)
          (let [r sync-result
                pairs (or (:pairs r) [])
                updated-count (or (:updated r) 0)
                added-count (max 0 (- (count pairs) updated-count))
                skipped (or (:skipped r) [])
                pull-upds (or (:updates r) [])]
            (dom/div
              (dom/props {:style {:color "#28a745"}})
              (dom/text
                (str "Done! "
                  (cond
                    (seq pull-upds) (str (count pull-upds) " cards updated from Anki")
                    (or (pos? added-count) (pos? updated-count))
                    (str (when (pos? added-count) (str added-count " added"))
                         (when (and (pos? added-count) (pos? updated-count)) ", ")
                         (when (pos? updated-count) (str updated-count " updated"))
                         (when (seq skipped) (str ", " (count skipped) " skipped")))
                    :else "No changes"))))))))

    ;; Action buttons
    (dom/div
      (dom/props {:style {:display "flex" :justify-content "flex-end" :gap "8px" :margin-top "8px"}})
      (dom/button
        (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                            :border "1px solid #ccc" :border-radius "4px" :cursor "pointer" :font-size "14px"}})
        (dom/text (if (= sync-phase :done) "Close" "Cancel"))
        (dom/On "click" (fn [_]
                          (reset! !show-modal false)
                          (reset! !sync-phase nil)
                          (reset! !sync-result nil)
                          (reset! !sync-error nil)
                          (reset! !push-pairs nil)
                          (reset! !pull-updates nil))
          nil))
      (when-not (#{:pushing :pulling :recording} sync-phase)
        (dom/button
          (dom/props {:style {:padding "8px 16px" :background "#17a2b8" :color "white" :border "none"
                              :border-radius "4px" :cursor "pointer" :font-size "14px"}})
          (dom/text "Pull from Anki")
          (dom/On "click"
            (fn [_]
              (reset! !sync-phase :pulling)
              (reset! !sync-result nil)
              (reset! !sync-error nil)
              (reset! !pull-updates nil)
              (reset! !push-pairs nil))
            nil)))
      (when-not (#{:pushing :pulling :recording} sync-phase)
        (dom/button
          (dom/props {:style {:padding "8px 16px" :background "#28a745" :color "white" :border "none"
                              :border-radius "4px" :cursor "pointer" :font-size "14px" :font-weight "500"}})
          (dom/text "Push to Anki")
          (dom/On "click"
            (fn [_]
              (reset! !sync-phase :pushing)
              (reset! !sync-result nil)
              (reset! !sync-error nil)
              (reset! !push-pairs nil)
              (reset! !pull-updates nil))
            nil))))))

(e/defn AnkiSyncExecutor
  "Handles push/pull execution and server recording."
  [sync-phase scope selected-doc current-pdf-page selected-deck
   basic-model cloze-model basic-fields cloze-fields allow-dupes
   !sync-phase !sync-result !sync-error !push-pairs !pull-updates !refresh]
  ;; Record push pairs on server
  (when (and (= sync-phase :recording) (some? (e/watch !push-pairs)))
    (let [pairs (e/watch !push-pairs)
          [?token _] (e/Token :record-push)]
      (when-some [token ?token]
        (let [result (e/server (sync/record-pushed-notes pairs))]
          (if (:success result)
            (do (e/server (swap! !refresh inc))
                (reset! !sync-phase :done)
                (token))
            (do (reset! !sync-error (:error result))
                (reset! !sync-phase :error)
                (token)))))))

  ;; Record pull updates on server
  (when (and (= sync-phase :recording) (some? (e/watch !pull-updates)))
    (let [updates (e/watch !pull-updates)
          [?token _] (e/Token :record-pull)]
      (when-some [token ?token]
        (let [result (e/server (sync/apply-pull-updates updates))]
          (if (:success result)
            (do (e/server (swap! !refresh inc))
                (reset! !sync-phase :done)
                (token))
            (do (reset! !sync-error (:error result))
                (reset! !sync-phase :error)
                (token)))))))

  ;; Pull execution
  (e/client
    (when (= sync-phase :pulling)
      (let [page-num (when (= scope "Current Page") current-pdf-page)
            cards-result (e/server (sync/get-cards-for-sync
                                     {:document-id selected-doc
                                      :page-number page-num}))]
        (if-not (:success cards-result)
          (do (reset! !sync-error (:error cards-result))
              (reset! !sync-phase :error))
          (let [cards (:cards cards-result)]
            (run-pull! cards basic-fields cloze-fields
              !sync-result !pull-updates !sync-error !sync-phase))))))

  ;; Push execution
  (e/client
    (when (= sync-phase :pushing)
      (let [page-num (when (= scope "Current Page") current-pdf-page)
            cards-result (e/server (sync/get-cards-for-sync
                                     {:document-id selected-doc
                                      :page-number page-num}))]
        (if-not (:success cards-result)
          (do (reset! !sync-error (:error cards-result))
              (reset! !sync-phase :error))
          (let [cards (:cards cards-result)]
            (run-push! cards selected-deck basic-model cloze-model
              basic-fields cloze-fields selected-doc allow-dupes
              !sync-result !push-pairs !sync-error !sync-phase))))))
  )

;; ---------------------------------------------------------------------------
;; Top-level button + modal
;; ---------------------------------------------------------------------------

(e/defn AnkiSyncButton [user-id selected-doc current-pdf-page card-type !refresh]
  (e/client
    (let [!show-modal (atom false)
          show-modal (e/watch !show-modal)]

      ;; Toolbar button
      (dom/button
        (dom/props {:style {:padding "4px 12px" :background "#7952b3" :color "white" :border "none"
                            :border-radius "4px" :cursor "pointer" :font-size "13px" :font-weight "500"}})
        (dom/text "Anki Sync...")
        (dom/On "click" (fn [_] (reset! !show-modal true)) nil))

      ;; Modal
      (when show-modal
        (let [!conn-status (atom :connecting)
              conn-status (e/watch !conn-status)
              !conn-error (atom nil)
              conn-error (e/watch !conn-error)
              !decks (atom [])
              decks (e/watch !decks)
              !models (atom [])
              models (e/watch !models)
              !scope (atom "Current Page")
              scope (e/watch !scope)
              !selected-deck (atom nil)
              selected-deck (e/watch !selected-deck)
              !basic-model (atom nil)
              basic-model (e/watch !basic-model)
              !cloze-model (atom nil)
              cloze-model (e/watch !cloze-model)
              !basic-fields (atom [])
              basic-fields (e/watch !basic-fields)
              !cloze-fields (atom [])
              cloze-fields (e/watch !cloze-fields)
              !allow-dupes (atom false)
              allow-dupes (e/watch !allow-dupes)
              !sync-phase (atom nil)
              sync-phase (e/watch !sync-phase)
              !sync-result (atom nil)
              sync-result (e/watch !sync-result)
              !sync-error (atom nil)
              sync-error (e/watch !sync-error)
              !push-pairs (atom nil)
              !pull-updates (atom nil)]

          ;; On mount: fetch decks and models
          (let [[?token _] (e/Token :anki-sync-fetch-config)]
            (when-some [token ?token]
              (run-fetch-config! !decks !models !selected-deck !basic-model !cloze-model
                !conn-status !conn-error)
              (token)))

          ;; Auto-fetch fields when model changes
          (let [[?token _] (e/Token [:anki-sync-basic-fields conn-status basic-model])]
            (when (and basic-model (= conn-status :connected))
              (when-some [token ?token]
                (run-fetch-fields! basic-model !basic-fields)
                (token))))
          (let [[?token _] (e/Token [:anki-sync-cloze-fields conn-status cloze-model])]
            (when (and cloze-model (= conn-status :connected))
              (when-some [token ?token]
                (run-fetch-fields! cloze-model !cloze-fields)
                (token))))

          ;; Executor (server recording + push/pull logic)
          (AnkiSyncExecutor sync-phase scope selected-doc current-pdf-page selected-deck
            basic-model cloze-model basic-fields cloze-fields allow-dupes
            !sync-phase !sync-result !sync-error !push-pairs !pull-updates !refresh)

          ;; Modal overlay
          (dom/div
            (dom/props {:style {:position "fixed" :top "0" :left "0" :width "100%" :height "100%"
                                :background "rgba(0,0,0,0.5)" :display "flex" :align-items "center"
                                :justify-content "center" :z-index "1000"}
                        :tabindex "-1"})
            (dom/On "click" (fn [_] (when-not sync-phase (reset! !show-modal false))) nil)
            (dom/On "keydown"
              (fn [e]
                (when (and (escape-key? e) (not sync-phase))
                  (reset! !show-modal false)))
              nil)

            (dom/div
              (dom/props {:style {:background "white" :border-radius "8px" :padding "24px"
                                  :width "520px" :max-width "90%" :box-shadow "0 4px 6px rgba(0,0,0,0.1)"
                                  :max-height "80vh" :overflow-y "auto"}})
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)

              (dom/h3 (dom/props {:style {:margin-top "0" :margin-bottom "20px"}})
                (dom/text "Anki Sync"))

              (cond
                (= conn-status :connecting)
                (dom/div
                  (dom/props {:style {:text-align "center" :padding "20px" :color "#666"}})
                  (dom/text "Connecting to Anki..."))

                (= conn-status :error)
                (dom/div
                  (dom/props {:style {:text-align "center" :padding "20px"}})
                  (dom/div
                    (dom/props {:style {:color "#dc3545" :margin-bottom "12px"}})
                    (dom/text (or conn-error "Connection failed")))
                  (dom/div
                    (dom/props {:style {:font-size "13px" :color "#666" :margin-bottom "16px"}})
                    (dom/text "Make sure Anki is running with the AnkiConnect plugin installed."))
                  (dom/button
                    (dom/props {:style {:padding "8px 16px" :background "#007bff" :color "white" :border "none"
                                        :border-radius "4px" :cursor "pointer"}})
                    (dom/text "Retry")
                    (dom/On "click"
                      (fn [_]
                        (reset! !conn-status :connecting)
                        (reset! !conn-error nil)
                        (run-fetch-config! !decks !models !selected-deck !basic-model !cloze-model
                          !conn-status !conn-error))
                      nil))
                  (dom/button
                    (dom/props {:style {:padding "8px 16px" :background "#f8f9fa" :color "#333"
                                        :border "1px solid #ccc" :border-radius "4px" :cursor "pointer"
                                        :margin-left "8px"}})
                    (dom/text "Cancel")
                    (dom/On "click" (fn [_] (reset! !show-modal false)) nil)))

                (= conn-status :connected)
                (dom/div
                  (AnkiSyncForm !scope decks !selected-deck models
                    !basic-model basic-fields !cloze-model cloze-fields !allow-dupes)
                  (AnkiSyncStatus sync-phase sync-result sync-error
                    !show-modal !sync-phase !sync-result !sync-error !push-pairs !pull-updates))))))))))
