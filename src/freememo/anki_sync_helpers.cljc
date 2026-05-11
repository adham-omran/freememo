(ns freememo.anki-sync-helpers
  "Plain helper functions for Anki sync — no e/defn, no reactive frame slots."
  (:require
   [missionary.core :as m]
   [clojure.string :as str]
   [freememo.logging :as log]))

;; ---------------------------------------------------------------------------
;; Client-side AnkiConnect wrapper
;; ---------------------------------------------------------------------------

(defn prepend-header
  "Prepend optional HTML header to note front text.
   Tagged with fm-header class so pull can strip it without contaminating the question."
  [front use-header header-text]
  (let [base (or front "")]
    (if (and use-header (not (str/blank? header-text)))
      (str "<p class=\"fm-header\">" header-text "</p>" base)
      base)))

(defn wrap-p
  "Wrap text in <p> tags if not already wrapped in HTML block tags."
  [text]
  (if (or (str/blank? text)
        (str/starts-with? (str/trim text) "<"))
    text
    (str "<p>" text "</p>")))

(defn strip-html
  "Remove all HTML tags and push-time decorations from Anki field text.
   Strips: fm-header tagged headers, source suffix (<hr>...), then all remaining HTML tags."
  [text]
  (if (str/blank? text)
    text
    (-> text
      (str/replace #"<p[^>]*class=\"fm-header\"[^>]*>.*?</p>" "") ;; strip tagged header
      (str/replace #"\n?<hr[^>]*class=\"fm-source\"[^>]*>[\s\S]*$" "") ;; strip tagged source
      (str/replace #"<br\s*/?>" "\n") ;; preserve line breaks
      (str/replace #"<[^>]*>" "") ;; strip all remaining tags
      str/trim)))

(defn append-source
  "Append source reference HTML to card content when source-display-mode is 'append'."
  [content source-ref]
  (if (and (not (str/blank? source-ref)))
    (str content "\n<hr class=\"fm-source\"><small style='color:#999'>Source: " source-ref "</small>")
    content))

#?(:cljs
   (defn to-future! [token task]
     (js/Promise.
       (fn [resolve reject]
         (.finally token (task resolve reject))))))

#?(:cljs
   (defn from-future
     ([f]
      (fn [success failure]
        (let [box (volatile! nil)
              token (js/Promise.
                      (fn [resolve _]
                        (vreset! box resolve)))
              resolve @box]
          (.then (f token)
            (fn [x]
              (resolve nil)
              (success x))
            (fn [e]
              (resolve nil)
              (failure e)))
          #(resolve nil))))
     ([f & args]
      (from-future #(apply f % args)))))

#?(:cljs
   (defn anki-call!
     "Call AnkiConnect API. Returns a Promise resolving to the result value."
     [action params]
     (from-future
       (fn [token]
         (let [ctrl (js/AbortController.)]
           (.finally token #(.abort ctrl))
           (-> (js/fetch "http://127.0.0.1:8765"
                 (clj->js {:method "POST"
                           :headers {"Content-Type" "application/json"}
                           :signal (.-signal ctrl)
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
                        (:result result))))))))))

#?(:cljs
   (defn fetch-anki-config! []
     (m/join (fn [decks models tags]
               {:decks (vec (js->clj decks))
                :models (vec (js->clj models))
                :tags (vec (js->clj tags))})
       (anki-call! "deckNames" nil)
       (anki-call! "modelNames" nil)
       (anki-call! "getTags" nil))))

(def ^:private freememo-base-url "https://freememo.net")

(defn html-escape
  "Escape HTML-special characters so titles/URLs can be embedded safely."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")
    (str/replace "'" "&#39;")))

(defn build-source-anchor
  "Build an HTML anchor linking back to the source item in FreeMemo.
   Links unconditionally to /viewer/topic/<topic-id>.
   Anchor text is '<title> - <page>' when the card has a :page_number, else '<title>'.
   Returns nil when title is blank."
  [card settings]
  (let [{:keys [topic-kind root-topic-id topic-title]} settings
        title topic-title
        page (:page_number card)
        url (str freememo-base-url "/viewer/topic/" (:topic_id card))
        anchor-text (if page
                      (str title " - " page)
                      title)]
    (when-not (str/blank? title)
      (str "<a href=\"" (html-escape url) "\">" (html-escape anchor-text) "</a>"))))

#?(:cljs
   (defn build-note
     "Build an AnkiConnect note map for addNote.
      settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
                  :allow-dupes :use-header :header-text :tags :source-display-mode}"
     [card settings]
     (let [{:keys [deck basic-model cloze-model basic-fields cloze-fields
                   allow-dupes use-header header-text tags source-display-mode]} settings
           kind (:flashcards/kind card)
           basic? (= kind "basic")
           model (if basic? basic-model cloze-model)
           fields (if basic? basic-fields cloze-fields)
           source-ref (build-source-anchor card settings)
           append-source? (and (= source-display-mode "append") (not (str/blank? source-ref)))
           field-source? (and (= source-display-mode "field") (not (str/blank? source-ref)))
           field-map (if basic?
                       (cond-> {(first fields) (prepend-header (wrap-p (or (:flashcards/question card) "")) use-header header-text)
                                (second fields) (if append-source?
                                                  (append-source (wrap-p (or (:flashcards/answer card) "")) source-ref)
                                                  (wrap-p (or (:flashcards/answer card) "")))}
                         field-source? (assoc (or (:source-field settings) "Source") (wrap-p source-ref)))
                       (cond-> {(first fields) (if append-source?
                                                 (append-source (prepend-header (or (:flashcards/cloze card) "") use-header header-text) source-ref)
                                                 (prepend-header (or (:flashcards/cloze card) "") use-header header-text))}
                         field-source? (assoc (or (:source-field settings) "Source") source-ref)))]
       (cond-> {:deckName deck
                :modelName model
                :fields field-map
                :tags tags}
         (not allow-dupes) (assoc :options {:allowDuplicate false
                                            :duplicateScope "deck"})
         allow-dupes (assoc :options {:allowDuplicate true})))))

#?(:cljs
   (defn build-update-fields
     "Build Anki field map for updating an existing note."
     [card settings]
     (let [{:keys [basic-fields cloze-fields use-header header-text source-display-mode]} settings
           kind (:flashcards/kind card)
           basic? (= kind "basic")
           fields (if basic? basic-fields cloze-fields)
           source-ref (build-source-anchor card settings)
           append-src? (and (= source-display-mode "append") (not (str/blank? source-ref)))
           field-src? (and (= source-display-mode "field") (not (str/blank? source-ref)))]
       (if basic?
         (cond-> {(first fields) (prepend-header (wrap-p (or (:flashcards/question card) "")) use-header header-text)
                  (second fields) (if append-src?
                                    (append-source (wrap-p (or (:flashcards/answer card) "")) source-ref)
                                    (wrap-p (or (:flashcards/answer card) "")))}
           field-src? (assoc (or (:source-field settings) "Source") (wrap-p source-ref)))
         (cond-> {(first fields) (if append-src?
                                   (append-source (prepend-header (or (:flashcards/cloze card) "") use-header header-text) source-ref)
                                   (prepend-header (or (:flashcards/cloze card) "") use-header header-text))}
           field-src? (assoc (or (:source-field settings) "Source") source-ref))))))

#?(:cljs
   (defn do-anki-push!
     "Push cards to Anki. Returns Missionary task resolving to result map.
      settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
                  :allow-dupes :use-header :header-text :tags}"
     [cards settings]
     (let [new-cards (vec (filter #(nil? (:flashcards/anki_note_id %)) cards))
           ;; Re-push every previously-synced card on every push so changes to
           ;; header text / use-header / source mode / tags propagate even when
           ;; card body content was not edited. Anki-side timestamp checks
           ;; can't see these settings, so optimizing here would silently drop
           ;; legitimate updates.
           changed-cards (vec (filter #(some? (:flashcards/anki_note_id %)) cards))]
       (log/log-debug (str "[anki-push] do-anki-push! starting"
                        " new=" (count new-cards)
                        " update=" (count changed-cards)
                        " mode=" (:source-display-mode settings)
                        " topic-title=" (pr-str (:topic-title settings))
                        " topic-kind=" (pr-str (:topic-kind settings))))
       (m/sp
         (let [;; Phase 1: add new cards (sequential, concurrency 1)
               add-result
               (m/? (m/reduce
                      (fn [acc item] (merge-with into acc item))
                      {:pairs [] :skipped [] :errors []}
                      (m/ap
                        (let [card (m/?> 1 (m/seed new-cards))]
                          (try
                            (let [note (build-note card settings)
                                  note-id (m/? (anki-call! "addNote" {:note note}))]
                              (if note-id
                                {:pairs [{:card-id (:flashcards/id card) :anki-note-id note-id}]}
                                {:skipped [{:card-id (:flashcards/id card) :reason "No note ID returned"}]}))
                            (catch :default err
                              {:errors [{:card-id (:flashcards/id card) :error (.-message err)}]}))))))
               ;; Phase 2: update changed cards (concurrency 10)
               update-result
               (m/? (m/reduce
                      (fn [acc item] (merge-with into acc item))
                      {:pairs [] :updated [] :errors []}
                      (m/ap
                        (let [card (m/?> 10 (m/seed changed-cards))]
                          (try
                            (let [field-map (build-update-fields card settings)]
                              (log/log-debug (str "[anki-push] updateNote card-id=" (:flashcards/id card)
                                               " note-id=" (:flashcards/anki_note_id card)
                                               " field-keys=" (vec (keys field-map))
                                               " field-map=" (pr-str field-map)))
                              (m/? (anki-call! "updateNote"
                                     {:note {:id (:flashcards/anki_note_id card)
                                             :fields field-map
                                             :tags (:tags settings)}}))
                              {:pairs [{:card-id (:flashcards/id card)
                                        :anki-note-id (:flashcards/anki_note_id card)}]
                               :updated [1]})
                            (catch :default err
                              (log/log-error (str "[anki-push] updateNote FAILED card-id=" (:flashcards/id card)
                                               " error=" (.-message err)))
                              {:errors [{:card-id (:flashcards/id card) :error (.-message err)}]}))))))]
           ;; Merge both phases
           (-> (merge-with into add-result update-result)
             (assoc :updated (count (:updated update-result)))))))))

#?(:cljs
   (defn do-anki-pull!
     "Pull edits from Anki for previously-synced cards. Returns Missionary task of updates + deleted IDs.

      Field selection is derived from Anki's notesInfo :order — basic cards read
      the first two fields by order (question, answer); cloze cards read the
      first. This matches Anki's own field ordering and removes any modal-side
      configuration dependency."
     [cards]
     (let [synced (filter #(some? (:flashcards/anki_note_id %)) cards)
           note-ids (mapv :flashcards/anki_note_id synced)]
       (if (empty? note-ids)
         (m/sp {:updates [] :deleted []})
         (m/sp
           (let [notes (m/? (anki-call! "notesInfo" {:notes note-ids}))
                 notes-js (js->clj notes)
                 pairs (map vector synced notes-js)
                 deleted (reduce
                           (fn [acc [card anki-note]]
                             (let [anki-note (if (map? anki-note) anki-note
                                               (js->clj anki-note :keywordize-keys true))]
                               (if (or (empty? anki-note) (nil? (:noteId anki-note)))
                                 (conj acc (:flashcards/id card))
                                 acc)))
                           []
                           pairs)
                 id->card (into {} (keep (fn [[card anki-note]]
                                           (let [anki-note (if (map? anki-note) anki-note
                                                             (js->clj anki-note :keywordize-keys true))]
                                             (when-let [nid (:noteId anki-note)]
                                               [nid card])))
                                     pairs))
                 updates (reduce
                           (fn [acc anki-note]
                             (let [anki-note (if (map? anki-note) anki-note
                                               (js->clj anki-note :keywordize-keys true))
                                   note-id (:noteId anki-note)
                                   card (get id->card note-id)]
                               (if-not card
                                 acc
                                 (let [kind (:flashcards/kind card)
                                       basic? (= kind "basic")
                                       fields-map (:fields anki-note)
                                       ordered-vals (->> fields-map
                                                      (sort-by (fn [[_ v]] (or (:order v) 0)))
                                                      (mapv (fn [[_ v]] (:value v))))
                                       update-map
                                       (if basic?
                                         (let [q (strip-html (first ordered-vals))
                                               a (strip-html (second ordered-vals))
                                               local-q (or (:flashcards/question card) "")
                                               local-a (or (:flashcards/answer card) "")]
                                           (when (or (not= q local-q) (not= a local-a))
                                             {:card-id (:flashcards/id card)
                                              :question q :answer a}))
                                         (let [c (strip-html (first ordered-vals))
                                               local-c (or (:flashcards/cloze card) "")]
                                           (when (not= c local-c)
                                             {:card-id (:flashcards/id card)
                                              :cloze c})))]
                                   (if update-map (conj acc update-map) acc)))))
                           []
                           notes-js)]
             {:updates updates :deleted deleted}))))))

;; ---------------------------------------------------------------------------
;; Cross-platform wrappers — avoid #?(:cljs) inside e/defn bodies.
;; Top-level defn with #?(:cljs/:clj) body is safe (no frame slot impact).
;; ---------------------------------------------------------------------------


(defn run-fetch-config! [conn]
  (let [{:keys [!status !error !decks !models !selected-deck !basic-model !cloze-model !all-tags]} conn]
    #?(:cljs
       (do ((fetch-anki-config!)
            (fn [{:keys [decks models tags]}]
              (reset! !decks decks)
              (reset! !models models)
              (reset! !all-tags tags)
              (when (seq decks) (reset! !selected-deck (first decks)))
              (when (seq models)
                (reset! !basic-model (first models))
                (reset! !cloze-model (first models)))
              (reset! !status :connected))
            (fn [err]
              (reset! !error (str "Cannot connect to Anki: " (.-message err)))
              (reset! !status :error)))
         nil)
       :clj nil)))


(defn run-fetch-fields!
  "Fetch model field names from Anki and store in atom.
   Atom values: :loading while in flight, vector of field names on success, [] on error."
  [model-name !fields-atom]
  #?(:cljs
     (when model-name
       (reset! !fields-atom :loading)
       (do ((anki-call! "modelFieldNames" {:modelName model-name})
            (fn [fields] (reset! !fields-atom (vec (js->clj fields))))
            (fn [_] (reset! !fields-atom [])))
         nil))
     :clj nil))

(defn run-push!
  "Execute push to Anki and update state atoms with results.
   Always transitions to :recording — finalize-push! must run even when
   there are no new pairs, so settings/per-item preset still persist.
   settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
               :allow-dupes :use-header :header-text :tags}
   sync     = {:!phase :!result :!error :!push-pairs}"
  [cards settings sync]
  (let [{:keys [!phase !result !error !push-pairs]} sync]
    #?(:cljs
       (do ((do-anki-push! cards settings)
            (fn [result]
              (let [pairs (or (:pairs result) [])
                    updated (or (:updated result) 0)
                    added (max 0 (- (count pairs) updated))
                    skipped (count (or (:skipped result) []))
                    errors (count (or (:errors result) []))]
                (log/log-debug (str "[anki-push] complete added=" added
                                 " updated=" updated
                                 " skipped=" skipped
                                 " errors=" errors)))
              (reset! !result result)
              (reset! !push-pairs (vec (:pairs result)))
              (reset! !phase :recording))
            (fn [err]
              (reset! !error (.-message err))
              (reset! !phase :error)))
         nil)
       :clj nil)))

(defn run-pull!
  "Execute pull from Anki and update state atoms with results.
   sync = {:!phase :!result :!error :!pull-updates}"
  [cards sync]
  (let [{:keys [!phase !result !error !pull-updates]} sync]
    #?(:cljs
       (do ((do-anki-pull! cards)
            (fn [result]
              (if (and (empty? (:updates result)) (empty? (:deleted result)))
                (do (reset! !result result)
                  (reset! !phase :done))
                (do (reset! !result result)
                  (reset! !pull-updates {:updates (:updates result)
                                         :deleted (:deleted result)})
                  (reset! !phase :recording))))
            (fn [err]
              (reset! !error (.-message err))
              (reset! !phase :error)))
         nil)
       :clj nil)))

(defn escape-key?
  "Cross-platform Escape key check for DOM key events."
  [e]
  #?(:cljs (= (.-key e) "Escape")
     :clj false))
