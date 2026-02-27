(ns electric-starter-app.anki-sync-helpers
  "Plain helper functions for Anki sync — no e/defn, no reactive frame slots."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Client-side AnkiConnect wrapper
;; ---------------------------------------------------------------------------

(defn prepend-header
  "Prepend optional HTML header to note front text."
  [front use-header header-text]
  (let [base (or front "")]
    (if (and use-header (not (str/blank? header-text)))
      (str "<p>" header-text "</p>" base)
      base)))

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
     "Fetch decks + models + tags from Anki. Returns Promise of {:decks [...] :models [...] :tags [...]}."
     []
     (-> (js/Promise.all
           #js [(anki-call! "deckNames" nil)
                (anki-call! "modelNames" nil)
                (anki-call! "getTags" nil)])
         (.then (fn [results]
                  {:decks  (vec (js->clj (aget results 0)))
                   :models (vec (js->clj (aget results 1)))
                   :tags   (vec (js->clj (aget results 2)))})))))

#?(:cljs
   (defn build-note
     "Build an AnkiConnect note map for addNote."
     [card deck-name basic-model cloze-model basic-fields cloze-fields allow-dupes
      use-header header-text tags]
     (let [kind (:flashcards/kind card)
           basic? (= kind "basic")
           model (if basic? basic-model cloze-model)
           fields (if basic? basic-fields cloze-fields)
           field-map (if basic?
                       {(first fields) (prepend-header (or (:flashcards/question card) "") use-header header-text)
                        (second fields) (or (:flashcards/answer card) "")}
                       {(first fields) (prepend-header (or (:flashcards/cloze card) "") use-header header-text)})]
       (cond-> {:deckName deck-name
                :modelName model
                :fields field-map
                :tags tags}
         (not allow-dupes) (assoc :options {:allowDuplicate false
                                            :duplicateScope "deck"})
         allow-dupes       (assoc :options {:allowDuplicate true})))))

#?(:cljs
   (defn do-anki-push!
     "Push cards to Anki. Returns Promise resolving to result map."
     [cards deck-name basic-model cloze-model basic-fields cloze-fields allow-dupes
      use-header header-text tags]
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
                                  basic-fields cloze-fields allow-dupes
                                  use-header header-text tags)]
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
                                       {(first fields) (prepend-header (or (:flashcards/question card) "") use-header header-text)
                                        (second fields) (or (:flashcards/answer card) "")}
                                       {(first fields) (prepend-header (or (:flashcards/cloze card) "") use-header header-text)})]
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
  "Fetch Anki config (decks + models + tags) and populate atoms."
  [!decks !models !selected-deck !basic-model !cloze-model !all-tags !conn-status !conn-error]
  #?(:cljs
     (-> (fetch-anki-config!)
         (.then (fn [{:keys [decks models tags]}]
                  (reset! !decks decks)
                  (reset! !models models)
                  (reset! !all-tags tags)
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
   allow-dupes use-header header-text tags !sync-result !push-pairs !sync-error !sync-phase]
  #?(:cljs
     (-> (do-anki-push! cards deck basic-model cloze-model
           basic-fields cloze-fields allow-dupes use-header header-text tags)
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
