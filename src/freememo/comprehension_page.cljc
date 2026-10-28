(ns freememo.comprehension-page
  "A diagnostic cycle from frozen material through answers, gaps, and reassessment."
  (:require [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-forms5 :as forms]
            [hyperfiddle.router5 :as r]
            [freememo.navigation :as nav]
            [freememo.typeahead :as typeahead]
            #?(:cljs [freememo.comprehension-upload :as upload])
            #?(:clj [freememo.comprehension :as cycle])
            #?(:clj [freememo.comprehension-ocr :as scan])))

(defn read-page* [user-id cycle-id _revision]
  #?(:clj {:materials (cycle/list-materials user-id)
           :cycles (cycle/list-cycles user-id)
           :detail (when cycle-id (cycle/cycle-detail user-id cycle-id))}
     :cljs nil))

(defn run-command* [user-id command]
  #?(:clj (let [[action id argument extra] command]
            (case action
              :start (cycle/start-cycle! user-id id argument)
              :extract (cycle/extract-facts! user-id id)
              :submit (cycle/submit-answers! user-id id argument)
              :draft (cycle/save-draft-answers! user-id id argument)
              :transcribe (scan/transcribe-scan! user-id id argument)
              :remove-scan (scan/delete-scan! user-id id argument)
              :accept-scan (scan/accept-scan! user-id id argument extra)
              :study (cycle/save-followup! user-id id "source-review")
              :practice (cycle/save-followup! user-id id "targeted-questions")
              :finish-study (cycle/finish-followup! user-id id)
              :reassess (cycle/reassess! user-id id 15)
              {:ok false :error "Unknown cycle action."}))
     :cljs nil))

(defn selected-scan [input-id selection]
  #?(:cljs (when (seq selection)
             (some-> (.getElementById js/document input-id) .-files (aget 0)))
     :clj nil))

(defn upload-task [cycle-id file]
  #?(:cljs (upload/upload! cycle-id file)
     :clj nil))

(e/defn CommandResult [user-id commits !revision !error navigate!]
  (e/client
    (e/for [[token command] (e/diff-by first (e/as-vec commits))]
      (let [captured (e/snapshot command)
            result (e/server (e/Offload #(run-command* user-id captured)))]
        (if (:ok result)
          (do
            (e/on-unmount
              #(do (reset! !error nil)
                   (swap! !revision inc)
                   (when-let [id (:cycle-id result)]
                     (navigate! :comprehension (nav/nav-comprehension id)))))
            (token))
          (case (reset! !error (:error result))
            (token (:error result))))))))

(e/defn Action [user-id command label !revision !error navigate!]
  (e/client
    (CommandResult user-id
      (forms/Button! command :label label :class "btn btn-secondary")
      !revision !error navigate!)))

(e/defn StartCycle [user-id materials !revision !error navigate!]
  (e/client
    (let [!topic (atom (some-> materials first :id))
          initial {:minutes "30"}
          commits
          (forms/Form! initial
            (e/fn Fields [{:keys [minutes]}]
              (e/amb
                (dom/label
                  (dom/text "Learning material")
                  (typeahead/Typeahead !topic
                    (mapv (fn [{:keys [id title]}] {:id id :label title}) materials)
                    "Search learning material…" nil false)
                  (e/amb))
                (forms/Input! :minutes minutes :type "number" :min 5 :max 180
                  :label "Target duration in minutes" :class "input" :required true)
                (forms/SubmitButton! :label "Start cycle" :class "btn btn-primary"
                  :disabled (nil? (e/watch !topic)))))
            :Parse (e/fn [fields _]
                     (let [topic-id (e/watch !topic)
                           minutes (some-> (:minutes fields) str parse-long)]
                       (if (and topic-id minutes (<= 5 minutes 180))
                         [:start topic-id minutes]
                         (ex-info "Choose material and a duration from 5 to 180 minutes." {}))))
            :show-buttons false)]
      (dom/p (dom/props {:class "comprehension-muted"})
        (dom/text "The duration is an estimate. You can take longer without a penalty."))
      (when-let [topic-id (e/watch !topic)]
        (Action user-id [:extract topic-id] "Extract facts" !revision !error navigate!))
      (CommandResult user-id commits !revision !error navigate!))))

(e/defn Question [index question]
  (e/client
    (dom/div
      (dom/props {:class "comprehension-question"})
      (dom/h3 (dom/text (str "Question " (inc index))))
      (dom/p (dom/props {:class "comprehension-muted"})
        (dom/text (str (str/replace (:format question) "-" " ") ", " (:points question) " points")))
      (dom/p (dom/text (:prompt question)))
      (when (seq (:options question))
        (dom/ol
          (dom/props {:class "comprehension-options"})
          (e/for [option (e/diff-by identity (:options question))]
            (dom/li (dom/text (if (map? option) (str (:id option) ". " (:text option)) option)))))))))

(e/defn AnswerForm [user-id detail !revision !error navigate!]
  (e/client
    (let [questions (get-in detail [:form :questions])
          !intent (atom :submit)
          initial (into {} (map #(vector (:id %) (get (:answers detail) (:id %) ""))) questions)
          commits
          (forms/Form! initial
            (e/fn Fields [fields]
              (e/amb
                (e/for [[index question] (e/diff-by (comp :id second) (map-indexed vector questions))]
                  (dom/section
                    (e/amb
                      (Question index question)
                      (forms/Input! (:id question) (get fields (:id question))
                        :as :textarea :rows (if (= "long-essay" (:format question)) 10 4)
                        :label (str "Answer " (inc index)) :class "input"
                        :maxlength 50000 :dir "auto"))))
                (dom/span
                  (dom/On "click" (fn [_] (reset! !intent :draft)) nil)
                  (forms/SubmitButton! :label "Save draft" :class "btn btn-secondary"))
                (dom/span
                  (dom/On "click" (fn [_] (reset! !intent :submit)) nil)
                  (forms/SubmitButton! :label "Submit answers" :class "btn btn-primary"))))
            :Parse (e/fn [fields _] [(e/watch !intent) (:id detail) fields])
            :show-buttons false)]
      (dom/p (dom/props {:class "comprehension-muted"})
        (dom/text "For multiple choice, enter the option ID or its full text. Leave an answer blank if you do not know."))
      (CommandResult user-id commits !revision !error navigate!))))

(defn scan-assignments [fields questions]
  (->> questions
    (map (fn [question]
          {:question_id (:id question)
           :text (get fields (:id question) "")
           :uncertain (boolean (get fields (str "uncertain-" (:id question))))
           :page_numbers (->> (str/split (get fields (str "pages-" (:id question)) "") #"[,\s]+")
                          (keep parse-long) vec)}))
    (filter #(or (not (str/blank? (:text %))) (seq (:page_numbers %))))
    vec))

(e/defn ScanReview [user-id detail scan !revision !error navigate!]
  (e/client
    (dom/section
      (dom/props {:class "comprehension-panel"})
      (dom/h3 (dom/text (:filename scan)))
      (dom/a (dom/props {:href (str "/api/comprehension/original/" (:id scan)) :target "_blank" :rel "noopener"})
        (dom/text "View original scan"))
      (when (contains? #{"uploaded" "failed" "review"} (:status scan))
        (Action user-id [:remove-scan (:id detail) (:id scan)] "Remove scan" !revision !error navigate!))
      (when (= "transcribing" (:status scan))
        (dom/p (dom/props {:class "comprehension-muted"})
          (dom/text "If transcription does not finish, retry 20 minutes after it started. An active transcription cannot start twice.")))
      (if (contains? #{"uploaded" "failed" "transcribing"} (:status scan))
        (Action user-id [:transcribe (:id detail) (:id scan)]
          (if (= "transcribing" (:status scan)) "Retry transcription" "Review transcript")
          !revision !error navigate!)
        (when (contains? #{"review" "accepted"} (:status scan))
          (let [questions (get-in detail [:form :questions])
                suggestions (group-by :question_id (or (:accepted_assignments scan) (:suggestions scan)))
                initial (reduce (fn [fields question]
                                  (let [suggestion (get suggestions (:id question))]
                                    (assoc fields (:id question) (str/join "\n" (map :text suggestion))
                                      (str "pages-" (:id question))
                                      (str/join "," (distinct (mapcat :page_numbers suggestion)))
                                      (str "uncertain-" (:id question)) (boolean (some :uncertain suggestion)))))
                          {} questions)
                commits
                (forms/Form! initial
                  (e/fn Fields [fields]
                    (e/amb
                      (e/for [[index question] (e/diff-by (comp :id second) (map-indexed vector questions))]
                        (dom/div
                          (e/amb
                            (forms/Input! (:id question) (get fields (:id question))
                              :as :textarea :rows 4 :class "input" :label (str "Transcript for answer " (inc index)))
                            (forms/Input! (str "pages-" (:id question)) (get fields (str "pages-" (:id question)))
                              :class "input" :label (str "Scan pages for answer " (inc index)))
                            (forms/Checkbox! (str "uncertain-" (:id question))
                              (get fields (str "uncertain-" (:id question)))
                              :label "The transcript still contains uncertain text"))))
                      (forms/SubmitButton! :label "Accept corrected transcript" :class "btn btn-primary")))
                  :Parse (e/fn [fields _]
                           [:accept-scan (:id detail) (:id scan) (scan-assignments fields questions)])
                  :show-buttons false)]
            (dom/p (dom/text "Compare each transcript with the original. Correct recognition errors and page assignments before accepting."))
            (e/for [uncertainty (e/diff-by identity (:uncertainties scan))]
              (dom/p (dom/props {:class "comprehension-warning"}) (dom/text (str uncertainty))))
            (CommandResult user-id commits !revision !error navigate!))))
      (when (= "accepted" (:status scan))
        (dom/p (dom/text "Transcript accepted. Review the answer fields before submitting."))))))

(e/defn UploadAnswers [cycle-id !revision !error]
  (e/client
    (let [input-id (str "answer-scan-" cycle-id)
          commits
          (forms/Form! {:file ""}
            (e/fn Fields [{:keys [file]}]
              (e/amb
                (forms/Input! :file (or file "") :id input-id :type "file" :accept ".pdf,.png,.jpg,.jpeg"
                  :class "input" :label "Answer scan" :required true)
                (forms/SubmitButton! :label "Upload handwritten answers" :class "btn btn-secondary")))
            :Parse (e/fn [fields _]
                     (if-let [file (selected-scan input-id (:file fields))]
                       [:upload file]
                       (ex-info "Choose a PDF, PNG, or JPEG scan." {})))
            :show-buttons false)]
      (e/for [[token [_ file]] (e/diff-by first (e/as-vec commits))]
        (let [result (e/Task (upload-task cycle-id (e/snapshot file)))]
          (if (:ok result)
            (do (e/on-unmount #(swap! !revision inc)) (token))
            (case (reset! !error (:error result)) (token (:error result)))))))))

(e/defn Gap [user-id gap !revision !error navigate!]
  (e/client
    (dom/section
      (dom/props {:class "comprehension-panel"})
      (dom/h3 (dom/text (str (str/capitalize (:category gap)) ", " (:criterion gap))))
      (dom/p (dom/text (:reason gap)))
      (dom/p (dom/props {:class "comprehension-muted"})
        (dom/text (str "Question " (:question_id gap) ", " (:status gap))))
      (e/for [evidence (e/diff-by identity (:evidence gap))]
        (dom/blockquote
          (dom/p (dom/text (:quote evidence)))
          (dom/a (dom/props {:href (str "/viewer/topic/" (:topic_id evidence))}) (dom/text "Open source"))))
      (Action user-id [:study (:id gap)] "Study source" !revision !error navigate!)
      (Action user-id [:practice (:id gap)] "Practice questions" !revision !error navigate!)
      (e/for [followup (e/diff-by :id (:followups gap))]
        (dom/div
          (dom/p (dom/text (str/replace (:kind followup) "-" " ")))
          (when-let [session-id (:session_id followup)]
            (dom/a (dom/props {:href (str "/quiz/session/" session-id)}) (dom/text "Open practice quiz")))
          (e/for [question (e/diff-by identity (:questions followup))]
            (dom/p (dom/text (or (:prompt question) (:question question) (str question)))))
          (when-not (:completed_at followup)
            (Action user-id [:finish-study (:id followup)] "Mark study complete" !revision !error navigate!))))
      (Action user-id [:reassess (:id gap)] "Reassess" !revision !error navigate!))))

(e/defn CycleDetail [user-id detail !revision !error navigate!]
  (e/client
    (dom/section
      (dom/h2 (dom/text (:title detail)))
      (dom/nav
        (dom/props {:class "comprehension-stages" :aria-label "Study cycle stages"})
        (e/for [stage (e/diff-by identity ["Material" "Exam" "Answers" "Gaps" "Study" "Reassess"])]
          (dom/span (dom/text stage))))
      (when (:parent-gap detail)
        (dom/aside
          (dom/props {:class "comprehension-panel"})
          (dom/h3 (dom/text "Reassessment target"))
          (dom/p (dom/text (get-in detail [:parent-gap :reason])))
          (dom/h4 (dom/text "Earlier answer"))
          (dom/p (dom/text (get-in detail [:parent-gap :original_answer])))
          (dom/a (dom/props {:href (str "/comprehension/" (get-in detail [:parent-gap :cycle_id]))})
            (dom/text "Open original attempt"))))
      (dom/section
        (dom/props {:class "comprehension-panel"})
        (dom/h3 (dom/text "Coverage"))
        (e/for [outcome (e/diff-by identity (get-in detail [:form :blueprint :assessed_outcomes]))]
          (dom/p (dom/text (if (map? outcome) (:statement outcome) (str outcome)))))
        (when (seq (get-in detail [:form :blueprint :unassessed_outcomes]))
          (dom/h4 (dom/text "Not assessed"))
          (e/for [outcome (e/diff-by identity (get-in detail [:form :blueprint :unassessed_outcomes]))]
            (dom/p (dom/text (if (map? outcome)
                              (str (:statement outcome) ": " (:reason outcome))
                              (str outcome))))))
        (e/for [omission (e/diff-by identity (get-in detail [:form :blueprint :omitted_formats]))]
          (dom/p (dom/text (str (:format omission) ": " (:reason omission)))))
        (dom/p (dom/text (str (count (get-in detail [:form :questions])) " questions, " (:target_minutes detail) " estimated minutes")))
        (dom/a (dom/props {:href (str "/api/comprehension/print/" (:id detail)) :target "_blank" :rel "noopener" :class "btn btn-secondary"})
          (dom/text "Print answer sheet")))
      (dom/h3 (dom/text "Handwritten answers"))
      (dom/p (dom/text "Use ordinary paper. Number your answers, then upload a PDF or a photo. Maximum: 20 MB and 20 pages."))
      (UploadAnswers (:id detail) !revision !error)
      (e/for [scan (e/diff-by :id (:scans detail))]
        (ScanReview user-id detail scan !revision !error navigate!))
      (dom/h3 (dom/text "Your answers"))
      (AnswerForm user-id detail !revision !error navigate!)
      (dom/h3 (dom/text "Attempt history"))
      (e/for [submission (e/diff-by :id (:submissions detail))]
        (dom/details
          (dom/summary (dom/text (str "Attempt " (:id submission) ", " (:revision_kind submission) ", " (:status submission)
                                  (when-let [score (get-in submission [:grade :raw-score])] (str ", " score "/100")))))
          (dom/p (dom/text (get-in submission [:grade :explanation])))
          (when (seq (get-in submission [:grade :uncertain_questions]))
            (dom/p (dom/props {:class "comprehension-warning"})
              (dom/text "Some answers contain uncertain transcription. Check the original scan before interpreting the score.")))
          (e/for [criterion (e/diff-by :question_id (get-in submission [:grade :criteria]))]
            (dom/p (dom/text (str (:question_id criterion) ": " (:points criterion) "/" (:max_points criterion)
                                 ", " (:explanation criterion)))))
          (e/for [[question-id answer] (e/diff-by first (:answers submission))]
            (dom/p (dom/text (str question-id ": " answer))))))
      (dom/h3 (dom/text "Gaps and further study"))
      (dom/p (dom/text "A gap describes evidence from this answer. Completing study does not prove the gap has closed."))
      (e/for [gap (e/diff-by :id (:gaps detail))]
        (Gap user-id gap !revision !error navigate!)))))

(e/defn ComprehensionPage [user-id navigate!]
  (e/client
    (let [[route-id] r/route
          cycle-id (some-> route-id str parse-long)
          !revision (atom 0)
          !error (atom nil)
          revision (e/watch !revision)
          page (e/server (e/Offload #(read-page* user-id cycle-id revision)))]
      (dom/link (dom/props {:rel "stylesheet" :href "/freememo/comprehension.css"}))
      (dom/div
        (dom/props {:class "comprehension-page"})
        (dom/header
          (dom/h1 (dom/text "Comprehension"))
          (dom/p (dom/text "Find a gap. Study its source. Check your understanding again."))
          (dom/a (dom/props {:href "/docs/comprehension-loop" :target "_blank" :rel "noopener"})
            (dom/text "How this study cycle works")))
        (when-let [error (e/watch !error)]
          (dom/p (dom/props {:role "alert" :class "comprehension-warning"}) (dom/text error)))
        (if cycle-id
          (if-let [detail (:detail page)]
            (CycleDetail user-id detail !revision !error navigate!)
            (dom/p (dom/text "Cycle not found.")))
          (dom/div
            (dom/h2 (dom/text "Start a diagnostic cycle"))
            (when (empty? (:materials page))
              (dom/p (dom/text "Import learning material and extract its text first.")))
            (StartCycle user-id (:materials page) !revision !error navigate!)
            (dom/h2 (dom/text "Your cycles"))
            (e/for [cycle (e/diff-by :id (:cycles page))]
              (dom/a
                (dom/props {:href (str "/comprehension/" (:id cycle)) :class "comprehension-cycle-link"})
                (dom/text (:title cycle))))))))))
