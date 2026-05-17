(ns freememo.bibliography-form
  "Editable bibliography form, backed by the `sources` table (CSL-JSON shape).
   Opens as a modal scoped to a topic-id; on save, creates or updates the
   sources row and links it via topics.source_id."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [clojure.string :as str]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.biblio-import :as biblio-import])))

(def csl-type-options
  [["webpage" "Webpage"]
   ["book" "Book"]
   ["article-journal" "Journal article"]
   ["document" "Document / report"]
   ["chapter" "Book chapter"]
   ["manuscript" "Manuscript"]])

;; ---------------------------------------------------------------------------
;; CSL <-> form translation
;; ---------------------------------------------------------------------------

(defn- format-date-parts
  "Render a CSL date-parts vector as YYYY-MM-DD / YYYY-MM / YYYY."
  [dp]
  #?(:clj
     (when (and (sequential? dp) (sequential? (first dp)))
       (let [[y m d] (first dp)]
         (cond
           (and y m d) (format "%04d-%02d-%02d" (int y) (int m) (int d))
           (and y m)   (format "%04d-%02d" (int y) (int m))
           y           (str y))))
     :cljs nil))

(defn- parse-date-parts
  "Parse YYYY-MM-DD / YYYY-MM / YYYY into [[Y M D]] CSL date-parts; nil if blank."
  [s]
  #?(:clj
     (when (and s (seq (str/trim s)))
       (let [parts (str/split (str/trim s) #"-")
             ints  (vec (keep #(try (Integer/parseInt %) (catch Exception _ nil)) parts))]
         (when (seq ints) [ints])))
     :cljs nil))

(defn- literal->family-given
  "Split a single literal name string into {:family :given}. Last whitespace-
   separated token → family; everything before → given. Works for Western
   name order; users edit afterwards for outliers (e.g. 'van der Linden')."
  [literal]
  (let [parts (when literal (str/split (str/trim literal) #"\s+"))]
    (cond
      (nil? parts)         {:family "" :given ""}
      (= 1 (count parts))  {:family (first parts) :given ""}
      :else                {:family (last parts)
                            :given  (str/join " " (butlast parts))})))

(defn- csl-author->form
  "Map one CSL author object to a form row {:family :given}. Handles both
   structured ({:family :given}) and literal ({:literal}) forms."
  [a]
  (cond
    (or (seq (:family a)) (seq (:given a)))
    {:family (or (:family a) "") :given (or (:given a) "")}

    (:literal a)
    (literal->family-given (:literal a))

    :else
    {:family "" :given ""}))

(defn- csl->form
  "CSL map → form-shaped map of strings + authors vector."
  [csl]
  {:csl-type        (or (:type csl) "webpage")
   :title           (or (:title csl) "")
   :url             (or (:URL csl) "")
   :container-title (or (:container-title csl) "")
   :page            (or (:page csl) "")
   :accessed        (or (format-date-parts (get-in csl [:accessed :date-parts])) "")
   :issued          (or (format-date-parts (get-in csl [:issued :date-parts])) "")
   :authors         (mapv csl-author->form (or (:author csl) []))})

(defn- form->csl
  "Form map → CSL-JSON map, dropping blank fields."
  [{:keys [csl-type title url container-title page accessed issued authors]}]
  (let [named-authors (filterv (fn [a] (or (seq (:family a)) (seq (:given a))))
                        (or authors []))]
    (cond-> {:type csl-type :title title}
      (seq url)             (assoc :URL url)
      (seq container-title) (assoc :container-title container-title)
      (seq page)            (assoc :page page)
      (parse-date-parts accessed) (assoc :accessed {:date-parts (parse-date-parts accessed)})
      (parse-date-parts issued)   (assoc :issued   {:date-parts (parse-date-parts issued)})
      (seq named-authors)         (assoc :author named-authors))))

;; ---------------------------------------------------------------------------
;; Topic badge — driven by bibliography (container) + topic.kind
;; ---------------------------------------------------------------------------

(defn topic-badge
  "Pick a [label color-var] pair for a topic's badge.
   Container-title='Wikipedia' wins over any kind hint, so Wikipedia sources
   render distinctly even when imported with kind='web'. Falls back to
   topics.kind for structural kinds (pdf/epub/markdown)."
  [kind source-container]
  (cond
    (= source-container "Wikipedia") ["Wikipedia" "var(--color-badge-web)"]
    (= kind "pdf")                   ["PDF"       "var(--color-badge-pdf)"]
    (= kind "epub")                  ["EPUB"      "var(--color-badge-epub)"]
    (#{"web" "wikipedia"} kind)      ["Web"       "var(--color-badge-web)"]
    (= kind "markdown")              ["MD"        "var(--color-badge-web)"]
    :else                            ["Topic"     "var(--color-badge-epub)"]))

;; ---------------------------------------------------------------------------
;; Citation rendering (cljc — server formats, client displays)
;; ---------------------------------------------------------------------------

(defn format-citation
  "Assemble a single-line citation from a CSL map. nil/blank → nil.
   Shape: 'Title — Author (Year)' with parts dropped when absent."
  [csl]
  #?(:clj
     (when (map? csl)
       (let [title       (some-> (:title csl) str/trim not-empty)
             first-author (first (:author csl))
             author-name (or (some-> (:family first-author) str/trim not-empty)
                           (some-> (:given first-author) str/trim not-empty))
             year        (some-> csl :issued :date-parts first first)
             dash        (when (and title (or author-name year)) " — ")
             author-part (or author-name "")
             year-part   (when year (str (when author-name " ") "(" year ")"))
             out (str (or title "") dash author-part year-part)]
         (when (seq (str/trim out)) out)))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Server fns
;; ---------------------------------------------------------------------------

(defn get-topic-citation*
  "Server: returns the citation string for a topic, or nil. `_refresh` is a
   reactive dep so the UI updates after a save."
  [_refresh user-id topic-id]
  #?(:clj
     (when (and user-id topic-id)
       (when-let [topic (db/get-topic-for-user user-id topic-id)]
         (when-let [sid (:topics/source_id topic)]
           (when-let [src (db/get-source sid)]
             (format-citation (:sources/csl src))))))
     :cljs nil))

(defn load-source-for-topic*
  "Returns {:source-id N-or-nil :form {...form-values...}} for the topic's
   current bibliography. `_refresh` provides a reactive dependency."
  [_refresh user-id topic-id]
  #?(:clj
     (when (and user-id topic-id)
       (let [topic     (db/get-topic-for-user user-id topic-id)
             source-id (:topics/source_id topic)
             source    (when source-id (db/get-source source-id))
             csl       (or (:sources/csl source) {})]
         {:source-id source-id
          :form      (csl->form csl)}))
     :cljs nil))

(defn claim-pending-biblio-show?*
  "Server: returns true exactly once per (user-id, topic-id) after
   biblio-import marked the topic as just-imported. Use to auto-open the
   bibliography modal on the topic's first mount post-import."
  [user-id topic-id]
  #?(:clj (when (and user-id topic-id)
            (biblio-import/claim-show? user-id topic-id))
     :cljs false))

(defn save-source-for-topic!*
  "Persist form values to the topic's source row; link via topics.source_id.
   Returns {:ok true :source-id N} or {:ok false :error msg}."
  [user-id topic-id form-values]
  #?(:clj
     (try
       (let [csl       (form->csl form-values)
             title     (:title form-values)
             url       (when (seq (:url form-values)) (:url form-values))
             csl-type  (:csl-type form-values)
             topic     (db/get-topic-for-user user-id topic-id)]
         (when-not topic
           (throw (ex-info "Topic not found" {:type ::topic-missing :id topic-id})))
         (let [current-sid (:topics/source_id topic)]
           (if current-sid
             (do (db/update-source! {:id current-sid :csl-type csl-type
                                     :csl csl :url url :title title})
                 {:ok true :source-id current-sid})
             (let [new-src (db/create-source! {:user-id user-id :csl-type csl-type
                                               :csl csl :url url :title title})
                   sid     (:sources/id new-src)]
               (db/attach-source-to-topic! user-id topic-id sid)
               {:ok true :source-id sid}))))
       (catch Exception e
         {:ok false :error (.getMessage e)}))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(e/defn FieldLabel [text]
  (dom/label
    (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
    (dom/text text)))

(e/defn CslTypeSelect
  "A1-fallback select for CSL type: stores selection in !csl-type atom.
   Each <option> declares its own :selected based on the atom value. Setting
   dom/props :value on <select> races with option mounting — sets .value property,
   which silently no-ops when no matching option exists yet. :selected on each
   option is reconciled at the option level, so the correct one is always chosen
   once its DOM node mounts."
  [!csl-type]
  (let [csl-type (e/watch !csl-type)
        current  (or csl-type "webpage")]
    (dom/select
      (dom/props {:class "input input-full"
                  :style {:margin-bottom "var(--sp-3)"}})
      (dom/On "change" (fn [e] (reset! !csl-type (-> e .-target .-value))) nil)
      (e/for [[v label] (e/diff-by first csl-type-options)]
        (dom/option
          (dom/props {:value v :selected (= v current)})
          (dom/text label)))))
  (e/amb))

(e/defn AuthorRows
  "Renders editable Family/Given rows for the authors vector and mutates !authors.
   Returns the empty e/amb — A1 fallback per spec 1.2.4: collection editor state
   lives in the atom, Form! :Parse merges via @!authors at commit time."
  [!authors]
  (let [authors (e/watch !authors)]
    (e/for [[i author] (e/diff-by first (map-indexed vector authors))]
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px" :margin-bottom "6px"}})
        (dom/input
          (dom/props {:type "text" :placeholder "Family" :class "input"
                      :style {:flex "1"} :value (:family author)})
          (dom/On "input"
            (fn [e] (swap! !authors assoc-in [i :family] (-> e .-target .-value)))
            nil))
        (dom/input
          (dom/props {:type "text" :placeholder "Given" :class "input"
                      :style {:flex "1"} :value (:given author)})
          (dom/On "input"
            (fn [e] (swap! !authors assoc-in [i :given] (-> e .-target .-value)))
            nil))
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                      :data-tooltip "Remove author"})
          (dom/text "×")
          (dom/On "click"
            (fn [_]
              (swap! !authors
                (fn [as] (vec (concat (subvec as 0 i) (subvec as (inc i)))))))
            nil))))
    (e/amb)))

(e/defn AddAuthorButton [!authors]
  (dom/button
    (dom/props {:class "btn btn-sm btn-secondary" :type "button"
                :style {:margin-bottom "var(--sp-3)"}})
    (dom/text "+ Add author")
    (dom/On "click"
      (fn [_] (swap! !authors (fnil conj []) {:family "" :given ""}))
      nil))
  (e/amb))

(e/defn CancelButton
  "Plain Cancel that closes the modal. Spec drift from X1 DiscardButton!: Forms5's
   DiscardButton! resets fields but doesn't expose a signal we can hook to close
   the modal from outside, so we use a plain button instead."
  [!show]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (reset! !show false)) nil))
  (e/amb))

(e/defn BibliographyDialog [!show user-id topic-id initial]
  ;; :type :command (commit always enabled) — :type :entity would lock submit when
  ;; only authors or csl-type changed, because their A1-fallback edits don't
  ;; produce Input! tokens.
  ;;
  ;; e/snapshot seeds the atoms ONCE at first mount. Without it, Electric
  ;; re-evaluates (atom ...) on subsequent reactive cycles when `initial` flickers
  ;; through reactive transitions, recreating the atoms and silently throwing away
  ;; user edits (observed: csl-change reset! to "article-journal" was reverted to
  ;; "book" on the next Parse cycle).
  (let [!authors  (atom (e/snapshot (or (:authors initial) [])))
        !csl-type (atom (e/snapshot (or (:csl-type initial) "webpage")))
        commits (forms/Form! initial
                  (e/fn Fields [{:keys [title url container-title
                                        page accessed issued]}]
                    (e/amb
                      (FieldLabel "Type")
                      (CslTypeSelect !csl-type)

                      (FieldLabel "Title")
                      (forms/Input! :title (or title "")
                        :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (FieldLabel "Authors")
                      (AuthorRows !authors)
                      (AddAuthorButton !authors)

                      (FieldLabel "URL")
                      (forms/Input! :url (or url "")
                        :placeholder "https://..." :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (FieldLabel "Container (journal / website name)")
                      (forms/Input! :container-title (or container-title "")
                        :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (FieldLabel "Publication date")
                      (forms/Input! :issued (or issued "") :type "date"
                        :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (FieldLabel "Page / page range")
                      (forms/Input! :page (or page "")
                        :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (FieldLabel "Accessed")
                      (forms/Input! :accessed (or accessed "") :type "date"
                        :class "input input-full"
                        :style {:margin-bottom "var(--sp-3)"})

                      (dom/div
                        (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                            :justify-content "flex-end"}})
                        (e/amb
                          (forms/SubmitButton! :label "Save"
                            :class "btn btn-primary"
                            :style {:font-weight "600" :order "1"})
                          (CancelButton !show)))))
                  :Parse (e/fn [merged-fields _tempid]
                           ;; e/watch (not @deref) — @!csl-type is non-reactive, so
                           ;; Forms5 wouldn't re-run :Parse on atom mutations and
                           ;; parsed-form-v would stay stale at submit time.
                           [`SaveBibliography topic-id
                            (-> merged-fields
                                (assoc :csl-type (e/watch !csl-type))
                                (assoc :authors  (e/watch !authors)))])
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [data (nth cmd 2)
            r (e/server (e/Offload #(save-source-for-topic!* user-id topic-id data)))]
        (when (some? r)
          (if (:ok r)
            (do (reset! !show false) (token))
            (token (:error r))))))))

(e/defn BibliographyForm [!show user-id topic-id]
  (e/client
    (e/for-by identity [_k [topic-id]]
      (let [server-data (e/server (e/Offload #(load-source-for-topic* 0 user-id topic-id)))]
        (dom/div
          (dom/props {:class "modal-backdrop"})
          (dom/On "click"
            (fn [e]
              (when (= (.-target e) (.-currentTarget e))
                (reset! !show false)))
            nil)
          (dom/div
            (dom/props {:class "modal-content modal-lg"
                        :style {:max-height "85vh" :overflow-y "auto"
                                :display "flex" :flex-direction "column"}})
            (dom/h3
              (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
              (dom/text "Bibliography"))
            (if (nil? server-data)
              (dom/div
                (dom/props {:style {:padding "16px" :color "var(--color-text-secondary)"}})
                (dom/text "Loading…"))
              (BibliographyDialog !show user-id topic-id (:form server-data)))))))))
