(ns freememo.bibliography-form
  "Editable bibliography form, backed by the `sources` table (CSL-JSON shape).
   Opens as a modal scoped to a topic-id; on save, creates or updates the
   sources row and links it via topics.source_id."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.modal-shell :as modal]
   [freememo.loading :as loading]
   [clojure.string :as str]
   [freememo.icons :as icons]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [taoensso.telemere :as tel])
   #?(:clj [freememo.optimistic :as opt])
   #?(:clj [freememo.toasts :as toasts])
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

#?(:clj
   (defn- format-date-parts
     "Render a CSL date-parts vector as YYYY-MM-DD / YYYY-MM / YYYY."
     [dp]
     (when (and (sequential? dp) (sequential? (first dp)))
       (let [[y m d] (first dp)]
         (cond
           (and y m d) (format "%04d-%02d-%02d" (int y) (int m) (int d))
           (and y m)   (format "%04d-%02d" (int y) (int m))
           y           (str y))))))

#?(:clj
   (defn- parse-date-parts
     "Parse YYYY-MM-DD / YYYY-MM / YYYY into [[Y M D]] CSL date-parts; nil if blank."
     [s]
     (when (and s (seq (str/trim s)))
       (let [parts (str/split (str/trim s) #"-")
             ints  (vec (keep #(try (Integer/parseInt %) (catch Exception _ nil)) parts))]
         (when (seq ints) [ints])))))

(defn- pad-date-string
  "Pad partial date strings to YYYY-MM-DD for the native HTML date picker.
   Used as Forms5 :Unparse so legacy year-only / year-month CSL renders in
   the picker. Input not matching a partial-or-full date shape is returned
   stringified-unchanged; Input!'s :Unparse only affects initial display, so
   untouched fields preserve their original CSL precision on save.
   Coerces non-string input (nil, etc.) via str — Input! can pass non-strings
   during reactive transitions."
  [s]
  (let [s (str s)]
    (cond
      (str/blank? s) s
      (re-matches #"\d{4}" s) (str s "-01-01")
      (re-matches #"\d{4}-\d{2}" s) (str s "-01")
      :else s)))


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

#?(:clj
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
      :authors         (mapv csl-author->form (or (:author csl) []))}))

#?(:clj
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
         (seq named-authors)         (assoc :author named-authors)))))

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
    (= kind "audio")                 ["Audio"     "var(--color-badge-web)"]
    (= kind "score")                 ["Score"     "var(--color-badge-pdf)"]
    (= kind "code")                  ["Code"      "var(--color-badge-web)"]
    :else                            ["Topic"     "var(--color-badge-epub)"]))

;; ---------------------------------------------------------------------------
;; Citation rendering (cljc — server formats, client displays)
;; ---------------------------------------------------------------------------

#?(:clj
   (defn format-citation
     "Assemble a single-line citation from a CSL map. nil/blank → nil.
      Shape: 'Title — Author (Year)' with parts dropped when absent."
     [csl]
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
         (when (seq (str/trim out)) out)))))

;; ---------------------------------------------------------------------------
;; Server fns
;; ---------------------------------------------------------------------------

(defn get-topic-citation*
  "Server: returns the citation string for a topic, or nil. Resolves the
   topic's effective source (own source_id, else nearest ancestor), so an
   edited extract shows its own citation while legacy source-less extracts
   still show the root's. `_refresh` is a reactive dep so the UI updates
   after a save."
  [_refresh user-id topic-id]
  #?(:clj
     (when (and user-id topic-id)
       (when (db/get-topic-for-user user-id topic-id)
         (when-let [sid (db/resolve-effective-source-id topic-id)]
           (when-let [src (db/get-source sid)]
             (format-citation (:sources/csl src))))))
     :cljs nil))

(defn load-source-for-topic*
  "Returns the form state for a topic's bibliography. `_refresh` provides a
   reactive dependency.
   Post: {:source-id own-source_id-or-nil
          :effective-source-id resolved-source-id-or-nil
          :inherited? true when the form is pre-filled from an ancestor
          :form {...form-values...}}
   The form pre-fills from the EFFECTIVE source (own, else nearest ancestor),
   so editing a legacy source-less extract starts from its root's fields; the
   save path forks a private row because :source-id (own) is still nil."
  [_refresh user-id topic-id]
  #?(:clj
     (when (and user-id topic-id)
       (when-let [topic (db/get-topic-for-user user-id topic-id)]
         (let [own-sid (:topics/source_id topic)
               eff-sid (db/resolve-effective-source-id topic-id)
               source  (when eff-sid (db/get-source eff-sid))
               csl     (or (:sources/csl source) {})]
           {:source-id           own-sid
            :effective-source-id eff-sid
            :inherited?          (boolean (and eff-sid (not= eff-sid own-sid)))
            :form                (csl->form csl)})))
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
         (let [current-sid (:topics/source_id topic)
               result (if current-sid
                        (do (db/update-source! {:id current-sid :csl-type csl-type
                                                :csl csl :url url :title title})
                            {:ok true :source-id current-sid})
                        (let [new-src (db/create-source! {:user-id user-id :csl-type csl-type
                                                          :csl csl :url url :title title})
                              sid     (:sources/id new-src)]
                          (db/attach-source-to-topic! user-id topic-id sid)
                          {:ok true :source-id sid}))]
           ;; No bump here: optimistic/execute! bumps :save-biblio's :views.
           result))
       (catch Exception e
         (tel/error! {:id ::save-biblio :data {:user-id user-id :topic-id topic-id}} e)
         {:ok false :error (.getMessage e)}))
     :cljs nil))

;; Optimistic-update dispatch (freememo.optimistic). Pre: payload has :topic-id
;; and :data. Post: source persisted (success toast) or error toast. Effect +
;; toast only — optimistic/execute! bumps the registry :views and removes the
;; command. Returns :done.
#?(:clj
   (defmethod opt/run-command! :save-biblio [user-id {:keys [payload]}]
     (let [{:keys [topic-id data]} payload
           r (save-source-for-topic!* user-id topic-id data)]
       (if (:ok r)
         (toasts/push! user-id {:level :success :message "Bibliography saved"})
         (toasts/push! user-id {:level :error
                                :message (or (:error r) "Failed to save bibliography")}))
       :done)))

;; ---------------------------------------------------------------------------
;; UI
;; ---------------------------------------------------------------------------

(e/defn FieldLabel [text]
  (dom/label
    (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
    (dom/text text)))

;; CSL type — A1-fallback select. Forms5 has no tracked-dropdown primitive
;; (RadioPicker! renders radios with raw values shown as <dt> labels, which is
;; unsuitable for a 6-option list). The atom-backed select preserves the
;; dropdown UI; the parent Form! reads (e/watch !csl-type) from :Parse to
;; merge the value into the saved CSL.
;;
;; Pre:  !csl-type is an atom holding the current CSL type string.
;; Post: on change, atom updated and select reflects authoritative value.
;;       Returns (e/amb) — no edit contribution to Forms5's e/amb.
(e/defn CslTypeSelect [!csl-type]
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

(defn- author-family-key [i] (keyword (str "author-" i "-family")))
(defn- author-given-key  [i] (keyword (str "author-" i "-given")))

;; AuthorRows — collection-editing case that doesn't map cleanly to Forms5.
;; Atom !authors holds the vector structure (count, add, remove). Per-cell
;; values are tracked via forms/Input! using positional field keys
;; (:author-0-family etc.). :Parse reads dirty values from merged-fields and
;; falls back to atom values for untouched cells.
;;
;; Pre:  !authors is an atom holding a vector of {:family :given} maps.
;; Post: Each row's Family/Given Input! contributes an edit to the surrounding
;;       Form!'s e/amb. Remove/add buttons mutate the atom directly.
;; Invariant: position-based keys shift on add/remove. Mid-edit removal of
;;            row k discards row k's dirty state (Input! unmounts before
;;            commit). Tracked per CLAUDE.md "Forms5 patterns".
(e/defn AuthorRows [!authors]
  (let [authors (e/watch !authors)]
    (e/for [[i author] (e/diff-by first (map-indexed vector authors))]
      (dom/div
        (dom/props {:style {:display "flex" :gap "8px" :margin-bottom "6px"}})
        (e/amb
          (forms/Input! (author-family-key i) (or (:family author) "")
            :type "text" :placeholder "Family" :class "input" :style {:flex "1"})
          (forms/Input! (author-given-key i) (or (:given author) "")
            :type "text" :placeholder "Given" :class "input" :style {:flex "1"})
          (do
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
              (tooltip/Tooltip! "Remove author" :aria? true)
              (icons/Icon :x :size 12 :title "Remove author")
              (dom/On "click"
                (fn [_]
                  (swap! !authors
                    (fn [as] (vec (concat (subvec as 0 i) (subvec as (inc i)))))))
                nil))
            (e/amb)))))))

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
  ;; !authors holds vector structure (count + values for add/remove). Per-cell
  ;; values are also tracked through Forms5 via :author-i-family/given Input!s;
  ;; on commit, :Parse reads dirty values from merged-fields and falls back
  ;; to atom values for cells the user didn't touch.
  ;;
  ;; :type :command — :entity would require dirty fields to enable Submit, but
  ;; "add author" doesn't fire an Input! token (the new row's Input!s are
  ;; pristine), so a brand-new-author commit wouldn't be reachable under :entity.
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
                        :Unparse (e/fn [s] (pad-date-string s))
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
                           ;; csl-type: read reactively via e/watch — atom is
                           ;; the truth (CslTypeSelect's A1 fallback).
                           ;; Authors: reconstruct from per-cell Input! dirty values,
                           ;; falling back to atom snapshot for untouched cells. Atom
                           ;; is the authority for row count (add/remove mutate it).
                           (let [authors-snapshot (e/watch !authors)
                                 reconstructed-authors
                                 (vec
                                   (map-indexed
                                     (fn [i a]
                                       {:family (or (get merged-fields (author-family-key i))
                                                    (:family a) "")
                                        :given  (or (get merged-fields (author-given-key i))
                                                    (:given a) "")})
                                     authors-snapshot))]
                             [`SaveBibliography topic-id
                              (-> merged-fields
                                  (assoc :csl-type (e/watch !csl-type))
                                  (assoc :authors  reconstructed-authors))]))
                  :type :command
                  :show-buttons false)]
    (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
      (let [data (nth cmd 2)]
        ;; Optimistic: enqueue the save and close immediately; the
        ;; CommandDispatcher persists it server-side and toasts the outcome.
        (case (e/server (opt/enqueue-command! user-id
                          {:type :save-biblio
                           :payload {:topic-id topic-id :data data}}))
          (do (reset! !show false) (token)))))))

(e/defn BibliographyForm [!show user-id topic-id]
  (e/client
    (e/for-by identity [_k [topic-id]]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
        (modal/ModalEscape (fn [] (reset! !show false)) "Bibliography")
        (dom/On "click"
          (fn [e]
            (when (= (.-target e) (.-currentTarget e))
              (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg"
                      :style {:max-height "85vh" :overflow-y "auto"
                              :display "flex" :flex-direction "column"}})
          ;; Header (D-b): title only. Refetch moved to Document Options →
          ;; Bibliography, which is item-scoped like this form.
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Bibliography"))
          (loading/WithLoading
            (e/fn [] (e/server (e/Offload #(load-source-for-topic* 0 user-id topic-id))))
            (e/fn [data] (BibliographyDialog !show user-id topic-id (:form data)))))))))
