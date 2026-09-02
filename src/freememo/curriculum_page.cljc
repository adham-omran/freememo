(ns freememo.curriculum-page
  "Curriculum authoring, study, and long-form exam UI."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.router5 :as r]
   [freememo.icons :as icons]
   [freememo.typeahead :as typeahead]
   ;; One way only. The exam page never requires this namespace back, because
   ;; the chip below and that page share `GenerateExamButton`.
   [freememo.curriculum-exam-page :as exam-page]
   #?(:clj [freememo.curriculum :as curriculum])
   #?(:clj [freememo.user-state :as us])))

(def weekdays
  [{:value 1 :key :mon :label "Mon"}
   {:value 2 :key :tue :label "Tue"}
   {:value 3 :key :wed :label "Wed"}
   {:value 4 :key :thu :label "Thu"}
   {:value 5 :key :fri :label "Fri"}
   {:value 6 :key :sat :label "Sat"}
   {:value 7 :key :sun :label "Sun"}])

(defn parse-positive-int [value]
  (let [value (str/trim (str value))]
    (when-not (str/blank? value)
      (let [parsed #?(:clj (parse-long value)
                      :cljs (js/parseInt value 10))]
        (when (and parsed (pos? parsed)) parsed)))))

(defn result [f]
  #?(:clj (try {:ok true :value (f)}
            (catch Exception error {:ok false :error (ex-message error)}))
     :cljs nil))

(defn list-curricula* [refresh user-id]
  #?(:clj (curriculum/list-curricula refresh user-id) :cljs nil))

(defn curriculum-detail* [refresh user-id curriculum-id]
  #?(:clj (curriculum/curriculum-detail refresh user-id curriculum-id) :cljs nil))

(defn curriculum-schedule* [refresh user-id curriculum-id]
  #?(:clj (curriculum/build-schedule refresh user-id curriculum-id) :cljs nil))

(defn documents-for-picker* [refresh user-id]
  #?(:clj (curriculum/documents-for-picker refresh user-id) :cljs nil))

(defn create-curriculum* [user-id attrs]
  #?(:clj (result #(curriculum/create-curriculum! user-id attrs)) :cljs nil))

(defn update-curriculum* [user-id curriculum-id attrs]
  #?(:clj (result #(curriculum/update-curriculum! user-id curriculum-id attrs)) :cljs nil))

(defn add-block* [user-id curriculum-id title]
  #?(:clj (result #(curriculum/add-block! user-id curriculum-id title)) :cljs nil))

(defn reorder-blocks* [user-id curriculum-id ids]
  #?(:clj (result #(curriculum/reorder-blocks! user-id curriculum-id ids)) :cljs nil))

(defn add-material* [user-id block-id topic-id minutes]
  #?(:clj (result #(curriculum/add-material! user-id block-id topic-id minutes)) :cljs nil))

(defn reorder-materials* [user-id block-id ids]
  #?(:clj (result #(curriculum/reorder-materials! user-id block-id ids)) :cljs nil))

(defn mark-studied* [user-id material-id]
  #?(:clj (result #(curriculum/mark-material-studied! user-id material-id)) :cljs nil))

(defn update-material-estimate* [user-id material-id minutes]
  #?(:clj (result #(curriculum/update-material-estimate! user-id material-id minutes))
     :cljs nil))

(defn focus-block* [user-id block-id]
  #?(:clj (result #(curriculum/focus-block! user-id block-id)) :cljs nil))

(defn unlock-next* [user-id block-id]
  #?(:clj (result #(curriculum/unlock-next-block! user-id block-id)) :cljs nil))













(e/defn AsyncButton
  "A button that runs one server operation and shows that it is running.

   Pre: operation returns {:ok true} or {:ok false :error message}.
   Post: the button is disabled and busy until the server answers."
  [label class-name operation]
  (e/client
    (dom/button
      (let [[token error] (e/Token (dom/On "click" identity nil))]
        (dom/props {:class class-name :type "button"
                    :disabled (some? token)
                    :aria-busy (some? token)
                    :aria-invalid (some? error)})
        (dom/text (if token (str label "…") label))
        (when token
          (let [response (operation)]
            (if (e/server (:ok response))
              (token)
              (token (e/server (:error response))))))
        (when error
          (dom/span (dom/props {:class "curriculum-inline-error"})
            (dom/text error)))))))

(e/defn WeekdayFields [fields]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-weekdays"})
      (e/for [weekday (e/diff-by :key weekdays)]
        (dom/label
          (dom/props {:class "curriculum-weekday"})
          (e/amb
            (forms/Input! (:key weekday) (boolean (get fields (:key weekday)))
              :type "checkbox")
            (dom/span (dom/text (:label weekday)))))))))

(e/defn NewCurriculumForm [user-id navigate! !open?]
  (e/client
    (let [commits
          (dom/div
            (dom/props {:class "curriculum-form-card"})
            (dom/h2 (dom/text "New curriculum"))
            (forms/Form!
              {:title "" :target-date "" :daily-minutes ""
               :mon false :tue false :wed false :thu false
               :fri false :sat false :sun false}
              (e/fn Fields [fields]
                (e/amb
                (dom/label (dom/props {:class "curriculum-field-label"})
                  (dom/text "Curriculum name"))
                (forms/Input! :title (:title fields) :class "input input-full"
                  :placeholder "Runtime systems curriculum")
                (dom/div (dom/props {:class "curriculum-form-grid"})
                  (dom/div
                    (dom/label (dom/props {:class "curriculum-field-label"})
                      (dom/text "Target date"))
                    (forms/Input! :target-date (:target-date fields)
                      :type "date" :class "input input-full"))
                  (dom/div
                    (dom/label (dom/props {:class "curriculum-field-label"})
                      (dom/text "Daily study minutes"))
                    (forms/Input! :daily-minutes (:daily-minutes fields)
                      :type "number" :class "input input-full" :placeholder "60")))
                (dom/label (dom/props {:class "curriculum-field-label"})
                  (dom/text "Study days"))
                (WeekdayFields fields)
                (dom/div (dom/props {:class "curriculum-form-actions"})
                  (e/amb
                    (dom/button
                      (dom/props {:class "btn" :type "button"})
                      (dom/text "Cancel")
                    (dom/On "click" (fn [_] (reset! !open? false)) nil))
                    (forms/SubmitButton! :label "Create curriculum"
                      :class "btn btn-primary")))))
              :Parse
              (e/fn [fields _]
                (let [title (str/trim (:title fields))
                      minutes (parse-positive-int (:daily-minutes fields))
                      selected-days (mapv :value
                                      (filter #(true? (get fields (:key %))) weekdays))
                      target (not-empty (:target-date fields))]
                  (cond
                    (str/blank? title) (ex-info "Enter a curriculum name." {})
                    (not (or target minutes (seq selected-days)))
                    (ex-info "Set a target date, daily study time, or study days." {})
                    :else [`CreateCurriculum
                           {:title title :target-date target
                            :daily-minutes minutes :study-weekdays selected-days}])))
              :type :command
              :show-buttons false))]
      (e/for [[token command] commits]
        (let [response (e/server (e/Offload
                                  #(create-curriculum* user-id (second command))))]
          (if (e/server (:ok response))
            (do
              (e/on-unmount #(reset! !open? false))
              (case (navigate! :curriculum
                      {:type :curriculum-detail :id (e/server (:value response))})
                (token)))
            (token (e/server (:error response)))))))))

(e/defn CurriculumDashboard [user-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :curriculum-mutations)))
          curricula (e/server (list-curricula* refresh user-id))]
      (if (empty? curricula)
        (dom/div (dom/props {:class "curriculum-empty"})
          (icons/Icon :route :size 32)
          (dom/h2 (dom/text "Build a path through your material"))
          (dom/p (dom/text "Order your sources into blocks, schedule the work, and pass a long-form exam to progress.")))
        (dom/div
          (dom/props {:class "curriculum-grid"})
          (e/for [curriculum (e/diff-by :id curricula)]
            (dom/button
              (dom/props {:class "curriculum-card"})
              (dom/On "click" (fn [_]
                                (navigate! :curriculum
                                  {:type :curriculum-detail :id (:id curriculum)})) nil)
              (dom/div (dom/props {:class "curriculum-card-kicker"})
                (dom/text "CURRICULUM"))
              (dom/h2 (dom/text (:title curriculum)))
              (dom/div (dom/props {:class "curriculum-card-progress"})
                (dom/span
                  (dom/text (str (:completed_block_count curriculum) " / "
                              (:block_count curriculum) " blocks complete")))
                (when-let [date (:target_date curriculum)]
                  (dom/span (dom/text (str "Target " date))))))))))))

(e/defn AddBlockForm [user-id curriculum-id !open?]
  (e/client
    (let [commits
          (dom/div
            (dom/props {:class "curriculum-inline-form"})
            (forms/Form! {:title ""}
              (e/fn Fields [{:keys [title]}]
                (e/amb
                  (forms/Input! :title title :class "input" :placeholder "Block name")
                  (forms/SubmitButton! :label "Add block" :class "btn btn-primary")))
              :Parse (e/fn [fields _]
                       (if (str/blank? (:title fields))
                         (ex-info "Enter a block name." {})
                         [`AddBlock {:title (str/trim (:title fields))}]))
              :type :command :show-buttons false))]
      (e/for [[token command] commits]
        (let [response (e/server (e/Offload
                                  #(add-block* user-id curriculum-id
                                     (:title (second command)))))]
          (if (e/server (:ok response))
            (do (e/on-unmount #(reset! !open? false)) (token))
            (token (e/server (:error response)))))))))

(e/defn MaterialPicker [!topic-id docs]
  (e/client
    (typeahead/Typeahead !topic-id
      (mapv (fn [doc] {:id (:id doc) :label (:title doc)}) docs)
      "Search imported material…" nil false)
    (e/amb)))

(e/defn AddMaterialWizard [user-id block-id navigate! !open?]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          docs (e/server (documents-for-picker* refresh user-id))
          !kind (atom nil)
          kind (e/watch !kind)
          !topic-id (atom nil)
          filtered (if kind (filterv #(= kind (:kind %)) docs) docs)
          commits
          (dom/div
            (dom/props {:class "curriculum-form-card curriculum-material-wizard"})
            (dom/h3 (dom/text "Add material"))
            (forms/Form! {:minutes ""}
              (e/fn Fields [{:keys [minutes]}]
                (e/amb
                (dom/div (dom/props {:class "curriculum-kind-row"})
                  (e/for [[value label]
                          (e/diff-by first
                            [["pdf" "PDF"] ["markdown" "Markdown"]
                             ["web" "Web article"] ["wikipedia" "Wikipedia"]])]
                    (dom/button
                      (dom/props {:type "button"
                                  :class (str "curriculum-kind-button"
                                           (when (= kind value) " is-selected"))})
                      (dom/text label)
                      (dom/On "click" (fn [_] (reset! !kind value)
                                        (reset! !topic-id nil)) nil))))
                (when kind
                  (dom/div
                    (dom/props {:class "curriculum-picker-step"})
                    (MaterialPicker !topic-id filtered)
                    (dom/p (dom/props {:class "curriculum-help"})
                      (dom/text "The material is missing? ")
                      (dom/button
                        (dom/props {:type "button" :class "link-button"})
                        (dom/text "Import it first")
                        (dom/On "click" (fn [_] (navigate! :import)) nil)))))
                (dom/label (dom/props {:class "curriculum-field-label"})
                  (dom/text "Estimated minutes (blank = FreeMemo estimate)"))
                (forms/Input! :minutes minutes :type "number" :class "input")
                (dom/div (dom/props {:class "curriculum-form-actions"})
                  (e/amb
                    (dom/button
                      (dom/props {:type "button" :class "btn"})
                      (dom/text "Cancel")
                      (dom/On "click" (fn [_] (reset! !open? false)) nil))
                    (forms/SubmitButton! :label "Add material" :class "btn btn-primary")))))
              :Parse (e/fn [fields _]
                       (let [topic-id (e/watch !topic-id)]
                         (if-not topic-id
                           (ex-info "Choose imported material." {})
                           [`AddMaterial {:topic-id topic-id
                                          :minutes (parse-positive-int (:minutes fields))}])))
              :type :command :show-buttons false))]
      (e/for [[token command] commits]
        (let [{:keys [topic-id minutes]} (second command)
              response (e/server (e/Offload
                                  #(add-material* user-id block-id topic-id minutes)))]
          (if (e/server (:ok response))
            (do (e/on-unmount #(reset! !open? false)) (token))
            (token (e/server (:error response)))))))))

(e/defn MoveButton [label disabled? operation]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "curriculum-icon-button"
                  :disabled disabled? :aria-label label :title label})
      (icons/Icon (if (= label "Move up") :chevron-up :chevron-down) :size 14)
      (let [[token error] (e/Token (dom/On "click" identity nil))]
        (when token
          (let [response (operation)]
            (if (e/server (:ok response)) (token)
              (token (e/server (:error response))))))
        (when error (dom/span (dom/props {:class "sr-only"}) (dom/text error)))))))

(defn moved-ids [items index direction]
  (let [target (+ index direction)
        ids (mapv :id items)]
    (if (or (neg? target) (>= target (count ids))) ids
      (assoc ids index (nth ids target) target (nth ids index)))))












(e/defn ExamChip
  "One exam per subject, as a chip.

   Pre: summary is the `curriculum/form-summary` map, or nil when the subject
   has no form.
   Post: a nil summary offers generation. A summary opens the exam route on
   the CURRENT version. No question text and no textarea render here."
  [user-id navigate! curriculum-id subject summary passed? score]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-exam-chip"})
      (if-not summary
        (exam-page/GenerateExamButton user-id subject "standard" nil
          "Generate exam" "btn btn-sm")
        (dom/button
          (dom/props {:type "button"
                      :class (str "btn btn-sm curriculum-exam-chip-open"
                               (when passed? " is-passed"))})
          (dom/text (str "Exam v" (:version summary) " · "
                      (cond
                        (= "draft" (:status summary)) "Draft"
                        (:stale? summary) "Outdated"
                        :else "Approved")
                      (when score (str " · " score "%"))
                      (when passed? " · passed")))
          (dom/On "click"
            (fn [_] (navigate! :curriculum
                      {:type :curriculum-exam :curriculum-id curriculum-id
                       :form-id (:form-id summary)}))
            nil))))))

(e/defn MaterialEstimate [user-id material]
  (e/client
    (let [!editing? (atom false)
          editing? (e/watch !editing?)]
      (if-not editing?
        (dom/button
          (dom/props {:type "button" :class "link-button curriculum-estimate-edit"})
          (dom/text (str (:estimated_minutes material) " min estimate · edit"))
          (dom/On "click" (fn [_] (reset! !editing? true)) nil))
        (let [commits
              (dom/div
                (dom/props {:class "curriculum-estimate-form"})
                (forms/Form! {:minutes (str (:estimated_minutes material))}
                  (e/fn Fields [{:keys [minutes]}]
                    (e/amb
                      (forms/Input! :minutes minutes :type "number"
                        :class "input curriculum-threshold")
                      (forms/SubmitButton! :label "Save" :class "btn btn-sm")))
                  :Parse (e/fn [fields _]
                           (if-let [minutes (parse-positive-int (:minutes fields))]
                             [`Estimate {:minutes minutes}]
                             (ex-info "Enter positive minutes." {})))
                  :type :command :show-buttons false))]
          (e/for [[token command] commits]
            (let [response (e/server (e/Offload
                                      #(update-material-estimate* user-id (:id material)
                                         (:minutes (second command)))))]
              (if (e/server (:ok response))
                (do (e/on-unmount #(reset! !editing? false)) (token))
                (token (e/server (:error response)))))))))))

(e/defn MaterialRow [user-id block materials index material navigate!]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-material-row"})
      (dom/div (dom/props {:class "curriculum-order-controls"})
        (MoveButton "Move up" (zero? index)
          (e/fn [] (e/server (e/Offload
                              #(reorder-materials* user-id (:block_id material)
                                 (moved-ids materials index -1))))))
        (MoveButton "Move down" (= index (dec (count materials)))
          (e/fn [] (e/server (e/Offload
                              #(reorder-materials* user-id (:block_id material)
                                 (moved-ids materials index 1)))))))
      (dom/div (dom/props {:class "curriculum-material-main"})
        (dom/div (dom/props {:class "curriculum-material-title"})
          (dom/span (dom/props {:class (str "type-badge type-badge-" (:kind material))})
            (dom/text (:kind material)))
          (dom/button
            (dom/props {:type "button" :class "link-button curriculum-material-link"})
            (dom/text (:title material))
            (dom/On "click"
              (fn [_] (navigate! :viewer {:type :topic
                                          :topic-id (:topic_id material)}))
              nil)))
        (dom/div (dom/props {:class "curriculum-material-meta"})
          (MaterialEstimate user-id material)
          (dom/span (dom/text (str (:anki_cards material) " Anki cards")))
          (when (pos? (:anki_cards material))
            (dom/span (dom/text (str "readiness "
                                  (.toFixed #?(:cljs (:anki_stability material) :clj 0) 1)))))))
      (dom/div
        (dom/props {:class "curriculum-material-actions"})
        (if (:studied_at material)
          (dom/span (dom/props {:class "curriculum-done-badge"}) (dom/text "Studied"))
          (AsyncButton "Mark studied" "btn btn-sm"
            (e/fn [] (e/server (e/Offload #(mark-studied* user-id (:id material)))))))
        (ExamChip user-id navigate! (:curriculum_id block) {:material-id (:id material)}
          (:exam material) (:exam-passed? material) (:latest-exam-score material))))))

(e/defn BlockCard [user-id curriculum-id blocks index block navigate!]
  (e/client
    (let [materials (:materials block)
          !add-material? (atom false)
          add-material? (e/watch !add-material?)]
      (dom/section
        (dom/props {:class (str "curriculum-block"
                            (when (:focused_at block) " is-focus")
                            (when-not (:unlocked_at block) " is-locked")
                            (when (:completed_at block) " is-complete"))})
        (dom/div (dom/props {:class "curriculum-block-rail"})
          (dom/span (dom/props {:class "curriculum-block-number"})
            (dom/text (inc index)))
          (dom/div (dom/props {:class "curriculum-rail-line"})))
        (dom/div (dom/props {:class "curriculum-block-body"})
          (dom/header (dom/props {:class "curriculum-block-header"})
            (dom/div (dom/props {:class "curriculum-order-controls"})
              (MoveButton "Move up" (zero? index)
                (e/fn [] (e/server (e/Offload
                                    #(reorder-blocks* user-id curriculum-id
                                       (moved-ids blocks index -1))))))
              (MoveButton "Move down" (= index (dec (count blocks)))
                (e/fn [] (e/server (e/Offload
                                    #(reorder-blocks* user-id curriculum-id
                                       (moved-ids blocks index 1)))))))
            (dom/div (dom/props {:class "curriculum-block-title"})
              (dom/h2 (dom/text (:title block)))
              (cond
                (:completed_at block) (dom/span (dom/props {:class "curriculum-done-badge"})
                                        (dom/text "Complete"))
                (:focused_at block) (dom/span (dom/props {:class "curriculum-focus-badge"})
                                      (dom/text "Focus block"))
                (not (:unlocked_at block)) (dom/span (dom/props {:class "curriculum-lock-badge"})
                                             (dom/text "Locked"))))
            (when (and (:unlocked_at block) (not (:focused_at block))
                    (not (:completed_at block)))
              (AsyncButton "Make focus" "btn"
                (e/fn [] (e/server (e/Offload #(focus-block* user-id (:id block))))))))
          (if-not (:unlocked_at block)
            (dom/p (dom/props {:class "curriculum-help"})
              (dom/text "Pass the previous block exam or use its unlock override."))
            (dom/div
              (if (empty? materials)
                (dom/p (dom/props {:class "curriculum-help"})
                  (dom/text "Add imported material to this block."))
                (e/for [[material-index material]
                        (e/diff-by first (map-indexed vector materials))]
                  (MaterialRow user-id block materials material-index material navigate!)))
              (if add-material?
                (AddMaterialWizard user-id (:id block) navigate! !add-material?)
                (dom/button
                  (dom/props {:type "button" :class "btn curriculum-add-material"})
                  (icons/Icon :plus :size 14)
                  (dom/text "Add material")
                  (dom/On "click" (fn [_] (reset! !add-material? true)) nil)))
              (when (seq materials)
                (ExamChip user-id navigate! curriculum-id {:block-id (:id block)}
                  (:exam block) (some? (:completed_at block)) nil))
              (when-not (:completed_at block)
                (AsyncButton "Unlock next block" "btn btn-subtle"
                  (e/fn [] (e/server (e/Offload #(unlock-next* user-id (:id block))))))))))))))

(e/defn ScheduleSummary [user-id curriculum-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :curriculum-mutations)))
          plan (e/server (curriculum-schedule* refresh user-id curriculum-id))]
      (dom/aside
        (dom/props {:class "curriculum-schedule-summary"})
        (dom/div (dom/props {:class "curriculum-schedule-title"})
          (icons/Icon :clock :size 16)
          (dom/strong (dom/text "Curriculum schedule")))
        (dom/p
          (dom/text (str (:total-minutes plan) " minutes remaining · "
                      (:daily-minutes plan) " minutes per study day")))
        (if (:feasible? plan)
          (dom/p (dom/props {:class "curriculum-feasible"})
            (dom/text (str "Feasible. Estimated finish: "
                        (or (:earliest-date plan) "complete"))))
          (dom/p (dom/props {:class "curriculum-warning"})
            (dom/text (str "The current controls do not reach " (:target-date plan)
                        ". Use " (get-in plan [:adjustment :required-daily-minutes])
                        " minutes per study day or move the target to "
                        (get-in plan [:adjustment :earliest-date]) "."))))
        (dom/div (dom/props {:class "curriculum-next-tasks"})
          (e/for [[_ task]
                  (e/diff-by first
                    (map-indexed vector (take 3 (:tasks plan))))]
            (dom/div
              (dom/time (dom/text (:date task)))
              (dom/span (dom/text (:title task)))
              (dom/em (dom/text (str (:minutes task) " min"))))))))))

(e/defn EditCurriculumForm [user-id detail !open?]
  (e/client
    (let [selected (set (:study_weekdays detail))
          seed (merge {:title (:title detail)
                       :target-date (str (or (:target_date detail) ""))
                       :daily-minutes (str (or (:daily_minutes detail) ""))}
                 (into {} (map (fn [{:keys [value key]}]
                                 [key (contains? selected value)]) weekdays)))
          commits
          (dom/div
            (dom/props {:class "curriculum-form-card"})
            (forms/Form! seed
              (e/fn Fields [fields]
                (e/amb
                (dom/label (dom/props {:class "curriculum-field-label"})
                  (dom/text "Curriculum name"))
                (forms/Input! :title (:title fields) :class "input input-full")
                (dom/div (dom/props {:class "curriculum-form-grid"})
                  (dom/div
                    (dom/label (dom/props {:class "curriculum-field-label"})
                      (dom/text "Target date"))
                    (forms/Input! :target-date (:target-date fields)
                      :type "date" :class "input input-full"))
                  (dom/div
                    (dom/label (dom/props {:class "curriculum-field-label"})
                      (dom/text "Daily study minutes"))
                    (forms/Input! :daily-minutes (:daily-minutes fields)
                      :type "number" :class "input input-full")))
                (WeekdayFields fields)
                (dom/div (dom/props {:class "curriculum-form-actions"})
                  (e/amb
                    (dom/button
                      (dom/props {:type "button" :class "btn"})
                      (dom/text "Cancel")
                      (dom/On "click" (fn [_] (reset! !open? false)) nil))
                    (forms/SubmitButton! :label "Save schedule controls"
                      :class "btn btn-primary")))))
              :Parse (e/fn [fields _]
                       (let [title (str/trim (:title fields))
                             target (not-empty (:target-date fields))
                             minutes (parse-positive-int (:daily-minutes fields))
                             days (mapv :value
                                    (filter #(true? (get fields (:key %))) weekdays))]
                         (cond
                           (str/blank? title) (ex-info "Enter a curriculum name." {})
                           (not (or target minutes (seq days)))
                           (ex-info "Set a target date, daily study time, or study days." {})
                           :else [`EditCurriculum
                                  {:title title :target-date target
                                   :daily-minutes minutes :study-weekdays days}])))
              :type :command :show-buttons false))]
      (e/for [[token command] commits]
        (let [response (e/server (e/Offload
                                  #(update-curriculum* user-id (:id detail)
                                     (second command))))]
          (if (e/server (:ok response))
            (do (e/on-unmount #(reset! !open? false)) (token))
            (token (e/server (:error response)))))))))

(e/defn CurriculumDetail [user-id curriculum-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :curriculum-mutations)))
          detail (e/server (e/Offload #(curriculum-detail* refresh user-id curriculum-id)))
          blocks (:blocks detail)
          !add-block? (atom false)
          add-block? (e/watch !add-block?)
          !edit? (atom false)
          edit? (e/watch !edit?)]
      (if-not detail
        (dom/div (dom/props {:class "curriculum-empty"}) (dom/text "Curriculum not found."))
        (dom/div
          (dom/props {:class "curriculum-detail"})
          (dom/header (dom/props {:class "curriculum-detail-header"})
            (dom/button
              (dom/props {:type "button" :class "curriculum-back"})
              (icons/Icon :arrow-left :size 16)
              (dom/text "Curricula")
              (dom/On "click" (fn [_] (navigate! :curriculum)) nil))
            (dom/div
              (dom/div (dom/props {:class "curriculum-card-kicker"}) (dom/text "PERSONAL CURRICULUM"))
              (dom/h1 (dom/text (:title detail)))
              (when (:complete? detail)
                (dom/span (dom/props {:class "curriculum-done-badge"})
                  (dom/text "Curriculum complete")))
              (dom/button
                (dom/props {:type "button" :class "link-button"})
                (dom/text "Edit schedule controls")
                (dom/On "click" (fn [_] (reset! !edit? true)) nil)))
            (ScheduleSummary user-id curriculum-id))
          (when edit? (EditCurriculumForm user-id detail !edit?))
          (dom/main (dom/props {:class "curriculum-study-rail"})
            (e/for [[index block]
                    (e/diff-by first (map-indexed vector blocks))]
              (BlockCard user-id curriculum-id blocks index block navigate!))
            (if add-block?
              (AddBlockForm user-id curriculum-id !add-block?)
              (dom/button
                (dom/props {:type "button" :class "curriculum-add-block"})
                (icons/Icon :plus :size 16)
                (dom/text "Add block")
                (dom/On "click" (fn [_] (reset! !add-block? true)) nil)))))))))

(e/defn CurriculumPage
  "The curriculum tab. Three screens live behind one active-tab branch.

   `/curriculum` lists curricula. `/curriculum/<id>` is one curriculum.
   `/curriculum/<id>/exam/<form-id>[/sitting/<sitting-id>]` is the exam page,
   which renders in place of the curriculum tree. Dispatching here rather than
   in `Main` is what holds the Routes coverage count at 14."
  [user-id navigate!]
  (e/client
    (let [[sub segment form-id _sitting-segment sitting-id] r/route
          selected-id (when (int? sub) sub)
          exam-form-id (when (and (= (quote exam) segment) (int? form-id)) form-id)
          exam-sitting-id (when (int? sitting-id) sitting-id)
          !new-open? (atom false)
          new-open? (e/watch !new-open?)]
      (if exam-form-id
        (exam-page/ExamRoute user-id navigate! exam-form-id exam-sitting-id)
        (dom/div
        (dom/props {:class "page-container curriculum-page"})
        (if selected-id
          (CurriculumDetail user-id selected-id navigate!)
          (dom/div
            (dom/header (dom/props {:class "curriculum-page-header"})
              (dom/div
                (dom/h1 (dom/text "Curriculum"))
                (dom/p (dom/text "Turn imported material into an ordered, scheduled course of study.")))
              (when-not new-open?
                (dom/button
                  (dom/props {:type "button" :class "btn btn-primary"})
                  (icons/Icon :plus :size 16)
                  (dom/text "New curriculum")
                  (dom/On "click" (fn [_] (reset! !new-open? true)) nil))))
            (when new-open? (NewCurriculumForm user-id navigate! !new-open?))
            (CurriculumDashboard user-id navigate!))))))))
