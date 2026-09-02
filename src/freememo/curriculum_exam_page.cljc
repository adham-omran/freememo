(ns freememo.curriculum-exam-page
  "One exam form as its own route: review it, edit it, sit it, read the grade.

   The curriculum page keeps one chip per subject and mounts nothing of this
   namespace but `GenerateExamButton`. The require runs one way, from
   `freememo.curriculum-page` to here, because the chip and this page share
   that button.

   Two rules shape every component below.

   A textarea means \"your text goes here\". A question prompt occupies a
   textarea only inside the edit gate, so review renders prompts as headings
   and a sitting renders an empty answer box under each heading.

   The client holds the URL, the `!editing?` atom, and the transient state a
   widget owns while the reader works in it. Everything else is a server read
   through `curriculum/exam-page-model`."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.icons :as icons]
   #?(:clj [freememo.curriculum :as curriculum])
   #?(:clj [freememo.curriculum-exam :as curriculum-exam])
   #?(:clj [freememo.user-state :as us])))

;; ── Server calls ───────────────────────────────────────────────────────────
;; Every wrapper keeps its var on both peers, because each name appears inside
;; an (e/server …) form that CLJS compiles too.

(defn result [f]
  #?(:clj (try {:ok true :value (f)}
            (catch Exception error {:ok false :error (ex-message error)}))
     :cljs nil))

(defn exam-page-model* [refresh user-id form-id sitting-id]
  #?(:clj (curriculum/exam-page-model refresh user-id form-id sitting-id) :cljs nil))

(defn evidence-packets* [refresh user-id form-id]
  #?(:clj (curriculum/form-evidence-packets refresh user-id form-id) :cljs nil))

(defn save-form-edits* [user-id form-id attrs]
  #?(:clj (result #(curriculum/save-form-edits! user-id form-id attrs)) :cljs nil))

(defn approve-form* [user-id form-id edits]
  #?(:clj (result #(curriculum/approve-form! user-id form-id edits)) :cljs nil))

(defn reorder-question* [user-id form-id index direction prompts]
  #?(:clj (result #(curriculum/reorder-draft-question! user-id form-id index
                     direction prompts))
     :cljs nil))

(defn generate-form* [user-id subject form-kind threshold]
  #?(:clj (curriculum-exam/start-form-generation! user-id subject form-kind
            (or threshold curriculum/default-threshold))
     :cljs nil))

(defn form-generation-work-key* [subject]
  #?(:clj (curriculum-exam/form-generation-work-key subject) :cljs nil))

(defn start-sitting* [user-id form-id]
  #?(:clj (result #(curriculum/start-sitting! user-id form-id)) :cljs nil))

(defn save-answer-draft* [user-id sitting-id slots]
  #?(:clj (result #(curriculum/save-answer-draft! user-id sitting-id slots))
     :cljs nil))

(defn finalize-sitting* [user-id sitting-id]
  #?(:clj (result #(curriculum/finalize-sitting! user-id sitting-id)) :cljs nil))

(defn cancel-sitting* [user-id sitting-id]
  #?(:clj (result #(curriculum/cancel-sitting! user-id sitting-id)) :cljs nil))

(defn archive-sitting* [user-id sitting-id]
  #?(:clj (result #(curriculum/archive-sitting! user-id sitting-id)) :cljs nil))

(defn grade-sitting* [user-id sitting-id]
  #?(:clj (curriculum-exam/grade-sitting! user-id sitting-id) :cljs nil))

(defn record-self-correction* [user-id sitting-id score reason]
  #?(:clj (result #(curriculum/record-self-correction! user-id sitting-id score reason))
     :cljs nil))

;; ── Plain helpers ──────────────────────────────────────────────────────────

(def question-field-keys [:question-1 :question-2 :question-3 :question-4 :question-5])

(defn parse-percent
  "Return an integer from 1 to 100, or nil."
  [value]
  (let [text (str/trim (str value))]
    (when-not (str/blank? text)
      (let [parsed #?(:clj (parse-long text) :cljs (js/parseInt text 10))]
        (when (and parsed (<= 1 parsed 100)) parsed)))))

(defn format-elapsed [seconds]
  (let [seconds (or seconds 0)
        remaining (mod seconds 60)]
    (str (quot seconds 60) ":" (when (< remaining 10) "0") remaining)))

(defn criterion-label
  "Render a rubric criterion for a reader, not for a database client."
  [criterion]
  (-> (name criterion) (str/replace "_" " ") str/capitalize))

(defn prompts-of
  "Return the five prompts a Forms5 field map holds, in order."
  [fields]
  (mapv #(str/trim (str (get fields %))) question-field-keys))

;; The DOM reads below are CLJS work, but every var must exist on BOTH peers:
;; each name appears at a call site Electric compiles twice, and a reader
;; conditional in an `e/defn` body makes the two peers disagree on frame shape.
;; A body-level conditional inside a plain `defn` keeps the call sites
;; identical. See CLAUDE.md, "Server-Only Functions in .cljc Files".

(defn page-root
  "Return the exam page container, so a DOM read is scoped to this page."
  [node]
  #?(:cljs (or (some-> node (.closest ".curriculum-exam-page")) js/document)
     :clj nil))

(defn prompts-in
  "Return the five prompt textareas' current text, or nil when absent.

   A prompt read from the DOM is current, where a read from the Forms5 field
   map is only as current as the last frame. Approve and reorder both need the
   reader's live text."
  [node]
  #?(:cljs (let [root (page-root node)
                 found (mapv (fn [index]
                               (some-> (.querySelector root
                                         (str "[name=\"question-" index "\"]"))
                                 .-value))
                         (range 5))]
             (when (every? some? found) (mapv str/trim found)))
     :clj nil))

(defn threshold-in
  "Return the pass-threshold field's current value, or nil when absent."
  [node]
  #?(:cljs (some-> (.querySelector (page-root node) "[name=\"threshold\"]") .-value)
     :clj nil))

(defn answer-slot-at
  "Return the draft slot the focus-out target belongs to, or nil.

   Pre: container holds inputs named `answer-<i>` and `unknown-<i>`.
   Post: the slot carries BOTH the text and the flag, because one write
   replaces a whole slot."
  [container target]
  #?(:cljs (when-let [field-name (some-> target .-name)]
             (when-let [[_ digits] (re-matches #"(?:answer|unknown)-(\d)" field-name)]
               (let [index (js/parseInt digits 10)
                     answer (.querySelector container
                              (str "[name=\"answer-" index "\"]"))
                     unknown (.querySelector container
                               (str "[name=\"unknown-" index "\"]"))]
                 {:index index
                  :answer (if answer (.-value answer) "")
                  :do-not-know (boolean (some-> unknown .-checked))})))
     :clj nil))

(defn swap-prompt-fields!
  "Swap two prompt textareas and tell Forms5 about it.

   Pre: a and b are 0-based prompt indexes on this page.
   Post: the DOM and the Forms5 field map both carry the swapped order, which
   is the order the server is about to store.
   Why: Forms5 gives user input precedence over a changed seed. Without this,
   a field the reader edited keeps its old text while its neighbour takes the
   moved value, and the reader sees the same prompt twice."
  [node a b]
  #?(:cljs (let [root (page-root node)
                 ea (.querySelector root (str "[name=\"question-" a "\"]"))
                 eb (.querySelector root (str "[name=\"question-" b "\"]"))]
             (when (and ea eb)
               (let [va (.-value ea) vb (.-value eb)]
                 (set! (.-value ea) vb)
                 (set! (.-value eb) va)
                 (.dispatchEvent ea (js/Event. "input" #js {:bubbles true}))
                 (.dispatchEvent eb (js/Event. "input" #js {:bubbles true})))))
     :clj nil))

(defn start-second-tick!
  "Start a one-second repeating callback. Returns a handle for `stop-tick!`."
  [callback]
  #?(:cljs (js/setInterval callback 1000) :clj nil))

(defn stop-tick! [handle]
  #?(:cljs (js/clearInterval handle) :clj nil))

;; ── Generation, shared with the curriculum chip ────────────────────────────

(def credit-notice
  "The notice every paid action owes before the reader spends anything.

   One string, because five controls across two pages cost credits and a second
   wording would make one of them look cheaper than it is."
  "Uses credits. FreeMemo records the charge after the model answers.")

(e/defn CreditNote
  "The notice as visible text, under the control it belongs to.

   The CALLER mounts it, because where the notice belongs is the page's
   judgement. On the exam page it sits under each paid button. On a curriculum
   row it would repeat once per material, so the chip carries the same words as
   a tooltip instead."
  []
  (e/client
    (dom/p (dom/props {:class "curriculum-credit-note"})
      (dom/text credit-notice))
    (e/amb)))

(e/defn RecordedCharge
  "What FreeMemo actually charged for a completed paid action.

   Pre: charge is a whole number of credits, or nil when nothing was recorded.
   Post: nil renders nothing, because an absent charge is not a free action —
   it is one whose charge never came back."
  [label charge]
  (e/client
    (when (some? charge)
      (dom/span (dom/props {:class "curriculum-recorded-charge"})
        (dom/text (str label " " charge " credits"))))
    (e/amb)))

(e/defn GenerateExamTrigger [label class-name generation-active?]
  (e/client
    (dom/button
      (let [[token error] (e/Token (dom/On "click" identity nil))]
        (dom/props {:type "button"
                    :class class-name
                    :data-tooltip credit-notice
                    :disabled (or generation-active? (some? token))
                    :aria-busy (or generation-active? (some? token))
                    :aria-invalid (some? error)})
        (dom/text (if (or generation-active? token) "Generating exam…" label))
        [token error]))))

(e/defn GenerateExamButton
  "Start one exam form generation. threshold nil takes the server default."
  [user-id subject form-kind threshold label class-name]
  (e/client
    (let [generations (e/server
                        (e/watch (us/get-atom user-id :exam-form-generations)))
          work-key (e/server (form-generation-work-key* subject))
          generation-active? (e/server (contains? generations work-key))]
      (dom/div
        (dom/props {:class "curriculum-generate-control"})
        (let [[token error]
              (GenerateExamTrigger label class-name generation-active?)]
          (when token
            (let [response (e/server (e/Offload
                                      #(generate-form* user-id subject form-kind
                                         threshold)))]
              (if (e/server (:success response)) (token)
                (token (e/server (:error response))))))
          (when error
            (dom/span (dom/props {:class "curriculum-inline-error" :role "alert"})
              (dom/text error)))))
      (e/amb))))

;; ── Blueprint ──────────────────────────────────────────────────────────────

(defn readable
  "The first non-blank string among `ks` in `m`.

   Pre: `m` is a map. `ks` are keys whose values a reader can read.
   Post: nil when every candidate is blank."
  [m ks]
  (some #(let [text (str/trim (str (get m %)))] (when-not (str/blank? text) text)) ks))

(defn outcome-text
  "One blueprint outcome as a sentence.

   Pre: `outcome` is a stored outcome — the map a generated form holds, or a
   plain string an older form holds.
   Post: never EDN punctuation, unless the map carries no readable field at
   all. Showing the raw map then is worse than a blank line, so it stays as the
   last resort rather than the first thing the reader sees."
  [outcome]
  (if (map? outcome)
    (or (readable outcome [:statement :text :outcome]) (pr-str outcome))
    (str outcome)))

(defn packet-text
  "One evidence packet as the passage it froze.

   Pre: `packet` is a stored packet — {:digest … :sections [{:text …}]}, or an
   older shape carrying the text at the top level.
   Post: the section texts in order, one blank line apart."
  [packet]
  (if (map? packet)
    (or (not-empty (str/join "\n\n" (keep #(readable % [:text :quote]) (:sections packet))))
      (readable packet [:quote :text])
      (pr-str packet))
    (str packet)))

(e/defn OutcomeList [heading outcomes]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-outcome-group"})
      (dom/h4 (dom/text heading))
      (if (empty? outcomes)
        (dom/p (dom/props {:class "curriculum-help"}) (dom/text "None."))
        (dom/ul
          (e/for [[_ outcome] (e/diff-by first (map-indexed vector outcomes))]
            (dom/li (dom/text (outcome-text outcome)))))))))

(e/defn BlueprintSection
  "Assessed and unassessed coverage, with the source passages behind a
   disclosure. The packets hold whole passages, so the page model omits them
   and this fetches them only when a reader opens them."
  [user-id form]
  (e/client
    (let [!open? (atom false)
          open? (e/watch !open?)]
      (dom/section
        (dom/props {:class "curriculum-exam-section"})
        (dom/h2 (dom/text "Blueprint"))
        (dom/div (dom/props {:class "curriculum-outcome-grid"})
          (OutcomeList "Assessed outcomes" (:assessed-outcomes form))
          (OutcomeList "Unassessed outcomes" (:unassessed-outcomes form)))
        (when (pos? (:evidence-packet-count form))
          (dom/div
            (dom/button
              (dom/props {:type "button" :class "link-button"})
              (dom/text (str (if open? "Hide" "Show") " "
                          (:evidence-packet-count form) " evidence packet"
                          (when (not= 1 (:evidence-packet-count form)) "s")))
              (dom/On "click" (fn [_] (swap! !open? not)) nil))
            (when open?
              (let [packets (e/server
                              (e/Offload #(evidence-packets* 0 user-id (:id form))))]
                (e/for [[_ packet] (e/diff-by first
                                     (map-indexed vector (e/server packets)))]
                  (dom/blockquote
                    (dom/props {:class "curriculum-evidence-packet"})
                    (dom/text (packet-text packet)))))))))
      (e/amb))))

;; ── Questions, read-only ───────────────────────────────────────────────────

(e/defn QuestionReview [index question]
  (e/client
    (dom/section
      (dom/props {:class "curriculum-exam-question"})
      (dom/div (dom/props {:class "curriculum-question-meta"})
        (dom/span (dom/text (str "Question " index)))
        (dom/span (dom/text (str/capitalize (str (:role question))))))
      (dom/h3 (dom/text (:prompt question)))
      (dom/details
        (dom/summary (dom/text "Rubric and evidence"))
        (dom/ul
          (e/for [criterion (e/diff-by identity
                              [:factual_accuracy :reasoning :source_evidence
                               :integration :completeness])]
            (dom/li
              (dom/strong (dom/text (str (criterion-label criterion) ": ")))
              (dom/span (dom/text (get-in question [:rubric criterion]))))))
        (e/for [[_ evidence] (e/diff-by first
                               (map-indexed vector (:evidence question)))]
          (dom/blockquote (dom/text (:quote evidence))))))))

(e/defn QuestionsReview [questions]
  (e/client
    (e/for [[index question] (e/diff-by first (map vector (range 1 6) questions))]
      (QuestionReview index question))
    (e/amb)))

;; ── Questions, inside the edit gate ────────────────────────────────────────
;;
;; The move buttons sit inside the Forms5 `Fields` body, so they MUST return an
;; empty e/amb — anything else a Fields body returns is read as a tracked FIELD
;; edit. They record the request in an atom that `QuestionsEditor` owns,
;; watches, and turns into a command, which is the rule for any value that
;; feeds an e/Token.
;;
;; A move reads the prompts out of the DOM, so an unsaved edit travels with its
;; own question across the swap. The field keys are positional, and the server
;; is what permutes.

(e/defn MoveQuestionButton [label disabled? index direction !move-request]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "curriculum-icon-button"
                  :disabled disabled? :aria-label label :title label})
      (icons/Icon (if (neg? direction) :chevron-up :chevron-down) :size 14)
      (let [node dom/node]
        (dom/On "click"
          (fn [_]
            ;; Read the prompts in their CURRENT order, which is the order the
            ;; server permutes from, and only then swap them on screen.
            (reset! !move-request
              {:index index :direction direction
               :prompts (prompts-in node)})
            (swap-prompt-fields! node index (+ index direction)))
          nil)))
    (e/amb)))

(e/defn QuestionEditRow [index key question fields !move-request]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-draft-question"})
      (e/amb
        (dom/div (dom/props {:class "curriculum-question-meta"})
          (e/amb
            (dom/span (dom/text (str index ". " (str/capitalize (str (:role question))))))
            (dom/span (dom/props {:class "curriculum-draft-order"})
              (e/amb
                (MoveQuestionButton "Move question up" (= 1 index)
                  (dec index) -1 !move-request)
                (MoveQuestionButton "Move question down" (= 5 index)
                  (dec index) 1 !move-request)))))
        (forms/Input! key (get fields key) :as :textarea
          :name (str "question-" (dec index))
          :class "input curriculum-prompt-input" :rows 3)))))

(e/defn QuestionsEditor
  "The edit gate. Returns commits and calls no server itself."
  [form]
  (e/client
    (let [questions (:questions form)
          !move-request (atom nil)
          move-request (e/watch !move-request)
          seed (merge {:threshold (str (:threshold form))}
                 (into {} (map (fn [[key question]] [key (:prompt question)])
                            (map vector question-field-keys questions))))
          form-commits
          (forms/Form! seed
            (e/fn Fields [fields]
              (e/amb
                (dom/label (dom/props {:class "curriculum-field-label"})
                  (dom/text "Pass threshold"))
                (forms/Input! :threshold (:threshold fields) :name "threshold"
                  :type "number" :class "input curriculum-threshold")
                (e/for [[index key question]
                        (e/diff-by first
                          (map vector (range 1 6) question-field-keys questions))]
                  (QuestionEditRow index key question fields !move-request))
                (dom/div (dom/props {:class "curriculum-form-actions"})
                  (forms/SubmitButton!
                    :label (if (= "draft" (:status form))
                             "Save draft"
                             "Save as a new draft version")
                    :class "btn btn-primary"))))
            :Parse (e/fn [fields _]
                     (let [threshold (parse-percent (:threshold fields))
                           prompts (prompts-of fields)]
                       (cond
                         (nil? threshold) (ex-info "Threshold must be from 1 to 100." {})
                         (some str/blank? prompts)
                         (ex-info "Every question needs text." {})
                         :else [:save-edits {:threshold threshold :prompts prompts}])))
            :type :command :show-buttons false)
          move-commits
          (let [[token _error] (e/Token move-request)]
            (e/When (some? token)
              (e/on-unmount #(reset! !move-request nil))
              [token [:reorder move-request]]))]
      (e/amb form-commits move-commits))))

;; ── Form-level actions ─────────────────────────────────────────────────────

(e/defn EditGateButton [editing? !editing?]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn"})
      (dom/text (if editing? "Done editing" "Edit questions"))
      (dom/On "click" (fn [_] (swap! !editing? not)) nil))
    (e/amb)))

(e/defn CommandButton
  "A button that emits one command. It holds a token and no server call."
  [label class-name command]
  (e/client
    (dom/button
      (dom/props {:type "button" :class class-name})
      (dom/text label)
      (let [[token error] (e/Token (dom/On "click" identity nil))]
        (when error
          (dom/span (dom/props {:class "curriculum-inline-error" :role "alert"})
            (dom/text error)))
        (e/When (some? token) [token command])))))

(e/defn ApproveButton
  "Approve carries the reader's live prompts when the edit gate is open.

   Pre: nothing. A closed gate yields nil prompts.
   Post: a draft freezes with those edits in one transaction. An approved,
   outdated form re-accepts current evidence and writes no prompts."
  [form]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "btn btn-primary"})
      (dom/text (if (:stale? form) "Reapprove frozen form" "Approve form"))
      (let [node dom/node
            event (dom/On "click"
                    (fn [_] {:prompts (prompts-in node)
                             :threshold (parse-percent (threshold-in node))})
                    nil)
            [token error] (e/Token event)]
        (when error
          (dom/span (dom/props {:class "curriculum-inline-error" :role "alert"})
            (dom/text error)))
        (e/When (some? token) [token [:approve event]])))))

;; ── Sittings ───────────────────────────────────────────────────────────────

(e/defn ArchiveSittingButton
  "Take one cancelled sitting off the list.

   Pre: the sitting is cancelled.
   Post: returns the archive commit pair.
   Invariant: the click must not reach the row. A sitting row navigates on
   click, so a control inside it owes its own `stopPropagation`."
  [sitting-id]
  (e/client
    (dom/button
      (dom/props {:type "button" :class "link-button curriculum-archive-sitting"})
      (dom/text "Archive")
      (let [click (dom/On "click" (fn [event] (.stopPropagation event) event) nil)
            [token error] (e/Token click)]
        (e/When (some? error)
          (dom/span (dom/props {:class "curriculum-inline-error" :role "alert"})
            (dom/text error)))
        (e/When (some? token) [token [:archive-sitting {:sitting-id sitting-id}]])))))

(e/defn SittingRow
  "One sitting in the list.

   Pre: `sitting` carries :submitted_at and :cancelled_at, both nilable.
   Post: returns the archive commit for a cancelled row, an empty `e/amb` for
   every other row. The last form of each nested body is its value, so the
   commits reach the caller through the Outcome cell."
  [navigate! breadcrumb form-id sitting]
  (e/client
    (let [cancelled? (some? (:cancelled_at sitting))
          submitted? (some? (:submitted_at sitting))]
      (dom/tr
        (dom/props {:class (str "curriculum-sitting-row"
                             (when cancelled? " is-cancelled"))})
        (dom/On "click"
          (fn [_] (navigate! :curriculum
                    {:type :curriculum-sitting
                     :curriculum-id (:curriculum_id breadcrumb)
                     :form-id form-id :sitting-id (:id sitting)}))
          nil)
        (dom/td (dom/text (str (:started_at sitting))))
        (dom/td (dom/text (format-elapsed (:elapsed_seconds sitting))))
        (dom/td (dom/text (cond
                            cancelled? "—"
                            (not submitted?) "In progress"
                            (nil? (:raw_score sitting)) "Not graded"
                            :else (str (:raw_score sitting) "%"))))
        (dom/td
          (dom/text (cond
                      cancelled? "Cancelled"
                      (nil? (:passed sitting)) "—"
                      (:passed sitting) "Passed"
                      :else "Not passed"))
          (e/When cancelled? (ArchiveSittingButton (:id sitting))))))))

(e/defn SittingsSection [navigate! model]
  (e/client
    (let [sittings (:sittings model)
          form-id (get-in model [:form :id])]
      (dom/section
        (dom/props {:class "curriculum-exam-section"})
        (dom/h2 (dom/text "Sittings"))
        ;; Two siblings, not an `if`. The section's value is its LAST form, so
        ;; the table's archive commits reach the service loop while the empty
        ;; note contributes nothing.
        (e/When (empty? sittings)
          (dom/p (dom/props {:class "curriculum-help"})
            (dom/text "No sitting yet. Approve the form, then take the exam.")))
        (e/When (seq sittings)
          (dom/table
            (dom/props {:class "curriculum-sitting-table"})
            (dom/thead
              (dom/tr
                (dom/th (dom/text "Started"))
                (dom/th (dom/text "Elapsed"))
                (dom/th (dom/text "Score"))
                (dom/th (dom/text "Outcome"))))
            (dom/tbody
              (e/for [sitting (e/diff-by :id sittings)]
                (SittingRow navigate! (:breadcrumb model) form-id sitting)))))))))

;; ── Taking the exam ────────────────────────────────────────────────────────

(defn answer-key [index] (keyword (str "answer-" index)))
(defn unknown-key [index] (keyword (str "unknown-" index)))

(e/defn ElapsedClock
  "Server seconds plus a local tick.

   The base crosses the wire once, already counted from `started_at`, so a
   reload 20 minutes in reads 20 minutes. No timestamp reaches the client and
   no clock is compared."
  [base-seconds]
  (e/client
    (let [!tick (atom 0)
          tick (e/watch !tick)
          timer (start-second-tick! #(swap! !tick inc))]
      (e/on-unmount #(stop-tick! timer))
      (dom/time
        (dom/text (str "Elapsed " (format-elapsed (+ (or base-seconds 0) tick))
                    " · suggested 45 min · no enforced limit"))))
    (e/amb)))

(e/defn AnswerField [index question fields]
  (e/client
    (dom/section
      (dom/props {:class "curriculum-exam-question"})
      (e/amb
        (dom/div (dom/props {:class "curriculum-question-meta"})
          (e/amb
            (dom/span (dom/text (str "Question " (inc index))))
            (dom/span (dom/text (str/capitalize (str (:role question)))))))
        (dom/h3 (dom/text (:prompt question)))
        (forms/Input! (answer-key index) (get fields (answer-key index))
          :as :textarea :name (str "answer-" index)
          :class "input curriculum-answer" :rows 8
          :placeholder "Write your answer")
        (dom/label (dom/props {:class "curriculum-unknown-answer"})
          (e/amb
            (forms/Checkbox! (unknown-key index)
              (boolean (get fields (unknown-key index)))
              :name (str "unknown-" index))
            (dom/span (dom/text "I do not know"))))))))

(e/defn SittingForm
  "The answer sheet. Returns commits and calls no server itself.

   A focus change autosaves that one slot, so a reload resumes the sitting.
   Submit carries all five slots as well, because the two writes are
   concurrent and only the submit payload can order them."
  [questions sitting]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-sitting"})
      (let [container dom/node
            drafts (:draft-answers sitting)
            seed (into {} (mapcat (fn [index]
                                    (let [slot (nth drafts index nil)]
                                      [[(answer-key index) (or (:answer slot) "")]
                                       [(unknown-key index)
                                        (boolean (:do-not-know slot))]]))
                            (range 5)))
            draft-commits
            (e/for [[token slot]
                    (dom/On-all "focusout"
                      (fn [event] (answer-slot-at container (.-target event))))]
              [token [:save-answers {:sitting-id (:id sitting)
                                     :slots (if slot [slot] [])}]])
            form-commits
            (forms/Form! seed
              (e/fn Fields [fields]
                (e/amb
                  (dom/div (dom/props {:class "curriculum-honor-note"})
                    (icons/Icon :book-open :size 16)
                    (dom/span
                      (dom/text "Closed book is the intention. FreeMemo does not enforce it."))
                    (ElapsedClock (:elapsed-seconds sitting)))
                  (e/for [[index question]
                          (e/diff-by first (map-indexed vector questions))]
                    (AnswerField index question fields))
                  (forms/SubmitButton! :label "Submit all five answers"
                    :class "btn btn-primary")))
              :Parse (e/fn [fields _]
                       (let [slots (mapv (fn [index]
                                           {:index index
                                            :answer (str/trim (str (get fields (answer-key index))))
                                            :do-not-know (boolean (get fields (unknown-key index)))})
                                     (range 5))]
                         (if (every? #(or (seq (:answer %)) (:do-not-know %)) slots)
                           [:finalize {:sitting-id (:id sitting) :slots slots}]
                           (ex-info "Answer every question or write “I do not know.”" {}))))
              :type :command :show-buttons false)]
        (e/amb draft-commits form-commits)))))

;; ── Grade ──────────────────────────────────────────────────────────────────

(e/defn SelfCorrectionForm [sitting-id]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-self-correction"})
      (forms/Form! {:score "" :reason ""}
        (e/fn Fields [{:keys [score reason]}]
          (e/amb
            (dom/label (dom/props {:class "curriculum-field-label"})
              (dom/text "Self-correction score"))
            (forms/Input! :score score :type "number" :class "input curriculum-threshold")
            (dom/label (dom/props {:class "curriculum-field-label"})
              (dom/text "What changed in your assessment?"))
            (forms/Input! :reason reason :as :textarea :class "input input-full" :rows 3)
            (forms/SubmitButton! :label "Save reflection" :class "btn")))
        :Parse (e/fn [fields _]
                 (let [score (parse-percent (:score fields))]
                   (if (and score (not (str/blank? (:reason fields))))
                     [:self-correct {:sitting-id sitting-id :score score
                                     :reason (:reason fields)}]
                     (ex-info "Enter a score from 1 to 100 and a reason." {}))))
        :type :command :show-buttons false))))

(e/defn QuestionGrade [index question-grade]
  (e/client
    (dom/section
      (dom/props {:class "curriculum-grade-question"})
      (dom/strong (dom/text (str "Question " (inc index))))
      (dom/div (dom/props {:class "curriculum-criterion-scores"})
        (e/for [[criterion score] (e/diff-by first (:criteria question-grade))]
          (dom/span (dom/text (str (criterion-label criterion) " " score "/4")))))
      (e/for [[_ deduction] (e/diff-by first
                              (map-indexed vector (:deductions question-grade)))]
        (dom/blockquote
          (dom/text (str (criterion-label (:criterion deduction)) ": "
                      (:reason deduction) " — “" (:citation deduction) "”")))))))

(e/defn GradeResult
  "Render one grade and its revisions. Returns commits, calls no server."
  [user-id subject sitting]
  (e/client
    (let [grade (:grade sitting)
          history (:history sitting)]
      (dom/div
        (dom/props {:class (str "curriculum-grade "
                            (cond (nil? grade) "is-ungraded"
                                  (:passed grade) "is-pass"
                                  :else "is-fail"))})
        (dom/div (dom/props {:class "curriculum-grade-score"})
          (dom/strong (dom/text (if grade (str (:raw_score grade) "%") "—")))
          (dom/span (dom/text (cond (nil? grade) "Not graded yet"
                                    (:passed grade) "Passed"
                                    :else "Not passed"))))
        (dom/p (dom/text (or (:explanation grade)
                           "Grade this sitting to see the evidence-bound result.")))
        (dom/details
          (dom/summary (dom/text "Question scores and evidence deductions"))
          (e/for [[index question-grade]
                  (e/diff-by first (map-indexed vector (:criteria grade)))]
            (QuestionGrade index question-grade)))
        (dom/p (dom/props {:class "curriculum-help"})
          (dom/text "The latest successful AI revision is authoritative. Self-correction is for reflection only."))
        (dom/details
          (dom/summary (dom/text (str (count history) " AI grade revision"
                                   (when (not= 1 (count history)) "s"))))
          (e/for [revision (e/diff-by :revision history)]
            (dom/p
              (dom/text (str "Revision " (:revision revision) " · "
                          (:raw_score revision) "% · "
                          (if (:passed revision) "passed" "not passed"))))))
        (RecordedCharge "Graded for" (:grading_cost_credits grade))
        (e/amb
          (dom/div (dom/props {:class "curriculum-form-actions"})
            (e/amb
              ;; The label names what the click does, and an ungraded sitting
              ;; is the primary thing to do here. Submit already asks for a
              ;; grade, but that request runs in the frame the submit itself
              ;; unmounts, so a rejected model answer leaves this screen
              ;; ungraded with nothing said. This button is the recovery, and
              ;; `CommandButton` renders its own error — so a second failure
              ;; reaches the reader instead of the server log.
              (dom/div (dom/props {:class "curriculum-paid-action"})
                (e/amb
                  (CommandButton (if grade "Regrade with AI" "Grade with AI")
                    (if grade "btn" "btn btn-primary")
                    [:regrade {:sitting-id (:id sitting)}])
                  (CreditNote)))
              (e/When (and (some? grade) (not (:passed grade)))
                (dom/div (dom/props {:class "curriculum-paid-action"})
                  (e/amb
                    (GenerateExamButton user-id subject "targeted-retry" nil
                      "Generate targeted retry" "btn btn-primary")
                    (CreditNote))))))
          (SelfCorrectionForm (:id sitting)))))))

(e/defn CancelSittingButton
  "Abandon an open sitting.

   Pre: the sitting is neither submitted nor cancelled.
   Post: returns the cancel commit pair. The help text is a sibling, so the
   button's commits are this component's value."
  [sitting-id]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-cancel-sitting"})
      (dom/p (dom/props {:class "curriculum-help"})
        (dom/text (str "Cancelling closes this sitting without grading it. "
                    "Nothing is charged, and your answers stay on the sitting.")))
      (CommandButton "Cancel this sitting" "btn btn-subtle"
        [:cancel-sitting {:sitting-id sitting-id}]))))

(e/defn CancelledSitting
  "What a cancelled sitting shows instead of an answer sheet.

   Post: returns an empty `e/amb`. Archiving lives in the sittings list, where
   the reader can see the row it removes."
  []
  (e/client
    (dom/div
      (dom/props {:class "curriculum-grade is-ungraded"})
      (dom/div (dom/props {:class "curriculum-grade-score"})
        (dom/strong (dom/text "—"))
        (dom/span (dom/text "Cancelled")))
      (dom/p (dom/text (str "You cancelled this sitting, so it was never graded. "
                         "Take the exam again when you are ready, or archive "
                         "this row from the Sittings list."))))
    (e/amb)))

;; ── The one service loop ───────────────────────────────────────────────────
;;
;; Every page-local command lands here, so no component above holds both a
;; token and a server call. `GenerateExamButton` is the stated exception: it is
;; a self-contained control the curriculum chip mounts too, and that page has
;; no service loop of its own.

(def form-command-heads #{:save-edits :approve :reorder :start})

(e/defn FormCommand [user-id navigate! model token command]
  (e/client
    (let [head (first command)
          arg (second command)
          form-id (e/server (get-in model [:form :id]))
          curriculum-id (e/server (get-in model [:breadcrumb :curriculum_id]))]
      (cond
        (= :save-edits head)
        (let [response (e/server (e/Offload #(save-form-edits* user-id form-id arg)))
              saved-id (e/server (:value response))]
          (if (e/server (:ok response))
            (if (= saved-id form-id)
              (token)
              ;; An approved form yields a NEW draft version, so the reader must
              ;; land on the version that now holds the edits. Navigate on
              ;; UNMOUNT, after the token is spent: navigating first tears this
              ;; frame down with the commit unacked, and Forms5 then re-emits it
              ;; against the old form id, minting a second version.
              ;; `case` sequences the registration before the spend. A plain
              ;; `do` is CONCURRENT in Electric, so `(token)` can unmount this
              ;; frame before `e/on-unmount` has registered anything.
              (case (e/on-unmount
                      #(navigate! :curriculum
                         {:type :curriculum-exam :curriculum-id curriculum-id
                          :form-id saved-id}))
                (token)))
            (token (e/server (:error response)))))

        (= :approve head)
        (let [response (e/server (e/Offload #(approve-form* user-id form-id arg)))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))

        (= :reorder head)
        (let [response (e/server (e/Offload
                                  #(reorder-question* user-id form-id (:index arg)
                                     (:direction arg) (:prompts arg))))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))

        :else
        (let [response (e/server (e/Offload #(start-sitting* user-id form-id)))
              started-id (e/server (:value response))]
          (if (e/server (:ok response))
            ;; Same ordering rule as :save-edits above. A start that navigates
            ;; before spending its token opens a second sitting.
            (case (e/on-unmount
                    #(navigate! :curriculum
                       {:type :curriculum-sitting :curriculum-id curriculum-id
                        :form-id form-id :sitting-id started-id}))
              (token))
            (token (e/server (:error response)))))))))

(e/defn SittingCommand [user-id token command]
  (e/client
    (let [head (first command)
          arg (second command)
          sitting-id (:sitting-id arg)]
      (cond
        (= :cancel-sitting head)
        (let [response (e/server (e/Offload #(cancel-sitting* user-id sitting-id)))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))

        (= :archive-sitting head)
        (let [response (e/server (e/Offload #(archive-sitting* user-id sitting-id)))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))

        ;; :already-closed comes back as {:ok true}, so a focus change fired by
        ;; the Submit click spends its token in silence.
        (= :save-answers head)
        (let [response (e/server (e/Offload
                                  #(save-answer-draft* user-id sitting-id (:slots arg))))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))

        (= :finalize head)
        ;; Write the answers, THEN freeze, then grade. The focus-out write and
        ;; this one are concurrent, so the submit payload is what orders them.
        (case (e/server (e/Offload #(save-answer-draft* user-id sitting-id (:slots arg))))
          (let [response (e/server (e/Offload #(finalize-sitting* user-id sitting-id)))]
            (if (e/server (:ok response))
              (let [graded (e/server (e/Offload #(grade-sitting* user-id sitting-id)))]
                (if (e/server (:success graded)) (token)
                  (token (e/server (:error graded)))))
              (token (e/server (:error response))))))

        (= :regrade head)
        (let [graded (e/server (e/Offload #(grade-sitting* user-id sitting-id)))]
          (if (e/server (:success graded)) (token)
            (token (e/server (:error graded)))))

        :else
        (let [response (e/server (e/Offload
                                  #(record-self-correction* user-id sitting-id
                                     (:score arg) (:reason arg))))]
          (if (e/server (:ok response)) (token)
            (token (e/server (:error response)))))))))

(e/defn ExamService [user-id navigate! model commits]
  (e/client
    (e/for [[token command] commits]
      (if (form-command-heads (first command))
        (FormCommand user-id navigate! model token command)
        (SittingCommand user-id token command)))
    (e/amb)))

;; ── Page shells ────────────────────────────────────────────────────────────

(e/defn ExamBreadcrumb [navigate! model]
  (e/client
    (let [breadcrumb (e/server (:breadcrumb model))
          curriculum-id (e/server (:curriculum_id breadcrumb))
          curriculum-title (e/server (:curriculum_title breadcrumb))
          material? (e/server (some? (:material_id breadcrumb)))
          title (e/server (or (:material_title breadcrumb) (:block_title breadcrumb)))]
      (dom/header
        (dom/props {:class "curriculum-detail-header curriculum-exam-header"})
        (dom/button
          (dom/props {:type "button" :class "curriculum-back"})
          (icons/Icon :arrow-left :size 16)
          (dom/text curriculum-title)
          (dom/On "click"
            (fn [_] (navigate! :curriculum
                      {:type :curriculum-detail :id curriculum-id}))
            nil))
        (dom/div
          (dom/div (dom/props {:class "curriculum-card-kicker"})
            (dom/text (if material? "MATERIAL EXAM" "BLOCK EXAM")))
          (dom/h1 (dom/text title)))))
    (e/amb)))

(e/defn FormStatusLine [form versions]
  (e/client
    (dom/div
      (dom/props {:class "curriculum-form-status"})
      (dom/span (dom/text (str "Form v" (e/server (:version form)) " · "
                            (str/capitalize (str (e/server (:status form)))) " · "
                            (e/server (:threshold form)) "% pass mark")))
      (when (e/server (:stale? form))
        (dom/span (dom/props {:class "curriculum-stale-badge"})
          (dom/text "Outdated exam form")))
      (RecordedCharge "Generated for" (e/server (:generation-cost form)))
      (when (> (count versions) 1)
        (dom/details
          (dom/summary (dom/text (str (count versions) " form versions")))
          (e/for [version (e/diff-by :id versions)]
            (dom/p
              (dom/text (str "Version " (:version version) " · "
                          (str/capitalize (str (:status version))) " · "
                          (:threshold version) "% pass mark")))))))
    (e/amb)))

(e/defn ExamActions [user-id model form]
  (e/client
    (let [status (e/server (:status form))
          stale? (e/server (:stale? form))
          sittable? (e/server (get-in model [:summary :sittable?]))
          subject (e/server (:subject model))
          form-kind (e/server (:form-kind form))
          threshold (e/server (:threshold form))]
      (dom/div
        (dom/props {:class "curriculum-form-actions"})
        (e/amb
          (e/When (or (= "draft" status) stale?) (ApproveButton form))
          (e/When (and (= "approved" status) sittable?)
            (CommandButton "Take exam" "btn btn-primary" [:start {}]))
          (dom/div (dom/props {:class "curriculum-paid-action"})
            (e/amb
              (GenerateExamButton user-id subject form-kind threshold
                "Regenerate" "btn")
              (CreditNote))))))))

(e/defn QuestionsSection [user-id model form]
  (e/client
    (let [!editing? (atom false)
          editing? (e/watch !editing?)]
      (dom/section
        (dom/props {:class "curriculum-exam-section"})
        (dom/div (dom/props {:class "curriculum-section-head"})
          (dom/h2 (dom/text "Questions"))
          (EditGateButton editing? !editing?))
        (e/amb
          (if editing?
            (QuestionsEditor (e/server form))
            (QuestionsReview (e/server (:questions form))))
          (ExamActions user-id model form))))))

(e/defn ExamPageBody [user-id navigate! model]
  (e/client
    (let [form (e/server (:form model))
          commits
          (dom/div
            (dom/props {:class "page-container curriculum-exam-page"})
            (e/amb
              (ExamBreadcrumb navigate! model)
              (FormStatusLine form (e/server (:versions model)))
              (BlueprintSection user-id (e/server form))
              (QuestionsSection user-id model form)
              (SittingsSection navigate! model)))]
      (ExamService user-id navigate! model commits))))

(e/defn SittingBody [user-id navigate! model]
  (e/client
    (let [sitting (e/server (:sitting model))
          sitting-id (e/server (:id sitting))
          submitted? (e/server (:submitted? sitting))
          cancelled? (e/server (:cancelled? sitting))
          commits
          (dom/div
            (dom/props {:class "page-container curriculum-exam-page"})
            (e/amb
              (ExamBreadcrumb navigate! model)
              ;; A cancelled sitting takes no answers, so it must not render an
              ;; answer sheet. The server refuses the write either way, and a
              ;; textarea that silently discards what a reader types is worse
              ;; than no textarea.
              (cond
                cancelled? (CancelledSitting)
                submitted? (GradeResult user-id (e/server (:subject model))
                             (e/server sitting))
                :else (e/amb
                        (SittingForm (e/server (get-in model [:form :questions]))
                          (e/server sitting))
                        (CancelSittingButton sitting-id)))))]
      (ExamService user-id navigate! model commits))))

(e/defn ExamRoute
  "Render one exam form, or one sitting of it.

   Pre: form-id names a form the user owns. sitting-id is nil, or a sitting of
   that form.
   Post: an unknown or unowned id renders Not found.
   Invariant: every piece of exam state is a server read, so a hard reload
   restores the same screen."
  [user-id navigate! form-id sitting-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :curriculum-mutations)))
          model (e/server (e/Offload
                           #(exam-page-model* refresh user-id form-id sitting-id)))
          found? (e/server (some? model))
          sitting-found? (e/server (some? (:sitting model)))]
      (cond
        (not found?)
        (dom/div (dom/props {:class "page-container curriculum-exam-page"})
          (dom/p (dom/props {:class "curriculum-empty"}) (dom/text "Not found.")))

        (and (some? sitting-id) (not sitting-found?))
        (dom/div (dom/props {:class "page-container curriculum-exam-page"})
          (dom/p (dom/props {:class "curriculum-empty"}) (dom/text "Not found.")))

        (some? sitting-id) (SittingBody user-id navigate! model)
        :else (ExamPageBody user-id navigate! model)))))
