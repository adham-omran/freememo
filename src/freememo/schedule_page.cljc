(ns freememo.schedule-page
  "Schedule tab — pick a document and a target date, get a daily plan
   (plans/study-scheduler-spec.md §1, §7).

   The tab is a PLAN, not a queue. It writes nothing to `card_schedules` and
   nothing to `topics.next_review_at` (§1.4): the user follows the numbers by
   hand, the way they would a training programme. Anki holds the review
   telemetry and this tab only reads it.

   Refresh is C1, on opening the tab. The browser acquires from AnkiConnect —
   only it can reach localhost:8765 — and the server replays what it sends
   (§9.4)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [hyperfiddle.router5 :as r]
   [freememo.schedule :as sched]
   [freememo.schedule-anki :as sa]
   [freememo.schedule-forecast :as fc]
   [freememo.schedule-detail :refer [ScheduleForecast]]
   [freememo.icons :as icons]
   [freememo.tooltip :as tooltip]
   [freememo.typeahead :as typeahead]
   [clojure.string :as str]
   #?(:clj [freememo.user-state :as us])))

(defn parse-int-or-nil
  "A form field's text as an integer, or nil.

   Blank means absent, not zero: a blank last page is 'to the end of the
   document' (§5.7), and 0 would be a range that holds nothing."
  [s]
  (let [t (some-> s str str/trim)]
    (when (and t (seq t))
      (try
        #?(:clj (Long/parseLong t)
           :cljs (let [n (js/parseInt t 10)] (when-not (js/isNaN n) n)))
        (catch #?(:clj Exception :cljs :default) _ nil)))))

;; ---------------------------------------------------------------------------
;; Acquisition (§2, §9.4.1)
;; ---------------------------------------------------------------------------

(e/defn AnkiSync [user-id]
  (e/client
    ;; The whole acquire-then-ingest cycle. Mounted once per tab visit.
    ;;
    ;; RETURNS its status rather than writing one into an atom the caller
    ;; watches. The atom was carrying a derived value between siblings, which
    ;; the skill's 'Atoms for DOM Ordering' only justifies when the dependency
    ;; direction fights DOM order. It does not here: this component renders
    ;; nothing, so the caller can bind it before anything that reads it.
    (let [refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          sync-refresh (e/server (e/watch (us/get-atom user-id :sync-mutations)))
          inputs (e/server (sched/refresh-inputs (+ refresh sync-refresh) user-id))
          note-ids (e/server (:note-ids inputs))
          push-deck (e/server (:push-deck inputs))
          !acq (atom nil)
          acq (e/watch !acq)]

      ;; §8.4 — the fetch fires ONCE per mount, inside a keyed frame. A bare
      ;; call in this body would restart on every re-evaluation (CLAUDE.md,
      ;; 'JS Library Init Side Effects in e/defn'); the key makes a change of
      ;; user or of card set an honest unmount and mount instead.
      (e/for-by identity [_k [user-id (count note-ids)]]
        (when (pos? (count note-ids))
          (e/snapshot (sa/start-acquire! note-ids push-deck !acq))))

      ;; §7.3 — the ingest is sequenced, never a `(token)` sibling of the
      ;; server call. `do` is concurrent in Electric, so a sibling token would
      ;; spend before the replay finished and tear it down.
      ;;
      ;; BOTH outcomes spend the token. Spending only on success left a throwing
      ;; replay with the branch mounted forever: no retry, no message, and the
      ;; error slot empty. `?error` survives a failed spend, which is what makes
      ;; it readable below.
      (let [[t ?error] (e/Token acq)]
        (when t
          (let [res (e/server (e/Offload #(sched/ingest-acquisition! user-id acq)))]
            (if (e/server (:ok res))
              (t)
              (t (e/server (:error res))))))

        ;; The status, as the value of this component.
        ;;
        ;; An EMPTY note set is not "in flight" — nothing was started, so
        ;; nothing will ever resolve. Left as nil it rendered the in-flight
        ;; label forever on any account whose documents carry no pushed cards,
        ;; which is every account before its first Anki push (§8.3).
        (cond
          (some? ?error) :ingest-failed
          (zero? (count note-ids)) :no-cards
          :else (:status acq))))))

;; The header chip that used to live here is gone. It rendered on all three
;; sub-views, so `/schedule` and the new-schedule form both showed
;; "FSRS unavailable" beside no forecast at all. The acquisition status now
;; renders only where a forecast depends on it: `StaleBanner` in the detail
;; pane, and a per-schedule status line on each dashboard card.
;;
;; `schedule-anki/acquisition-states` therefore has ONE render site left. The
;; test over that map asserts its SHAPE, not its render sites, so nothing there
;; guards the remaining one.

;; ---------------------------------------------------------------------------
;; New-schedule form (§7.4)
;; ---------------------------------------------------------------------------

(e/defn DocumentSelect [!doc-id docs]
  (e/client
    ;; `Typeahead`, not a `<select>`. This is the widget's canonical case
    ;; (CLAUDE.md, 'Typeahead Is Id-Keyed; Suggest Is Free Text'): the option
    ;; list IS the domain of legal values — a schedule over a document that
    ;; does not exist is meaningless — and the domain is genuinely id-keyed,
    ;; because a topic id is distinct from its title and two documents may
    ;; share one. It is a better fit than the Anki deck field, which has to
    ;; pass the label as the id because Anki exposes no deck id.
    ;;
    ;; A plain select also stops scaling: a library is unbounded, and picking
    ;; one book out of hundreds by scrolling is the problem typeahead exists
    ;; to remove.
    ;;
    ;; Options MUST be {:id :label}. A seq of strings fails SILENTLY here —
    ;; `(:label "text")` is nil, so rows paint blank and selecting one writes
    ;; nil, CLEARING the field instead of setting it.
    (typeahead/Typeahead !doc-id
      (mapv (fn [d] {:id (:id d)
                     :label (str (:title d) " — " (:pages d) " pages")})
        docs)
      "Search your documents…" nil false)
    (e/amb)))

(e/defn FieldLabel [text]
  (e/client
    (dom/label
      (dom/props {:style {:display "block" :font-size "12px" :font-weight "600"
                          :margin-bottom "4px" :color "var(--color-text-secondary)"}})
      (dom/text text))
    (e/amb)))

(e/defn NewScheduleForm [user-id navigate! !open?]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :tree-mutations)))
          docs (e/server (sched/documents-for-picker refresh user-id))
          !doc-id (atom nil)
          commits (forms/Form!
                    {:target-date "" :first-page "1" :last-page ""
                     :capacity (str sched/default-daily-capacity)}
                    (e/fn Fields [{:keys [target-date first-page last-page capacity]}]
                      (e/amb
                        (FieldLabel "Document")
                        (DocumentSelect !doc-id docs)

                        (FieldLabel "Target date")
                        (forms/Input! :target-date (or target-date "") :type "date"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        ;; §5.7 — the range defaults to a page-1 prefix, because
                        ;; 11 of 11 in-progress documents in production start there.
                        (FieldLabel "First page")
                        (forms/Input! :first-page (or first-page "1") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (FieldLabel "Last page (blank = to the end)")
                        (forms/Input! :last-page (or last-page "") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (FieldLabel "Daily review capacity")
                        (forms/Input! :capacity (or capacity "60") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                              :justify-content "flex-end"}})
                          (e/amb
                            (forms/SubmitButton! :label "Create schedule"
                              :class "btn btn-primary" :style {:order "1"})
                            (dom/button
                              (dom/props {:class "btn" :type "button"})
                              (dom/text "Cancel")
                              (dom/On "click" (fn [_] (reset! !open? false)) nil)
                              (e/amb))))))
                    ;; Validation belongs HERE, not in the commit handler.
                    ;; Forms5 reads `(ex-message parsed-form-v)` and, for
                    ;; `:type :command`, `can-submit?` is `(or (not
                    ;; validation-message) tracked-cmd)` — so an `ex-info`
                    ;; DISABLES the submit button and renders its message into
                    ;; the form's own `data-role="errormessage"` element.
                    ;; Checking after submit instead let the user commit an
                    ;; invalid form, spend a token, and be told no afterwards.
                    :Parse (e/fn [fields _tempid]
                             (let [doc-id (e/watch !doc-id)
                                   target (str (:target-date fields))]
                               (cond
                                 (nil? doc-id) (ex-info "Pick a document." {})
                                 (str/blank? target) (ex-info "Pick a target date." {})
                                 :else
                                 [`CreateSchedule
                                  {:root-topic-id doc-id
                                   :target-date target
                                   :first-page (parse-int-or-nil (:first-page fields))
                                   :last-page (parse-int-or-nil (:last-page fields))
                                   :daily-capacity (or (parse-int-or-nil (:capacity fields))
                                                     sched/default-daily-capacity)
                                   :excluded-pages []}])))
                    :type :command
                    :show-buttons false)]
      ;; `e/for` straight over the commits amb — the skill's service shape.
      ;; `(e/diff-by first (e/as-vec commits))` said the same thing through
      ;; `e/as-vec`, which is a debugging tool: it materializes the very
      ;; differential collection the amb exists to avoid.
      (e/for [[token cmd] commits]
        (let [attrs (nth cmd 1)
              res (e/server (e/Offload #(sched/create-schedule! user-id attrs)))]
          (when (some? res)
            (if (e/server (:ok res))
              ;; Create AND open. Landing back on an empty list makes the user
              ;; find the thing they just made; the forecast is what they
              ;; filled the form in for.
              ;;
              ;; Close only on success, and only from inside the branch that
              ;; mounts on success — so a failure keeps the form open with its
              ;; message (CLAUDE.md, Token Lifecycle). The `case` sequences the
              ;; navigation call ahead of the spend; `do` is concurrent, so a
              ;; sibling `(token)` could unmount this frame before `navigate!`
              ;; ran at all. It gates on the CALL, not on the route having
              ;; changed — `navigate!` only resets an atom that `Main`'s own
              ;; token handler drains a beat later.
              (do (e/on-unmount #(reset! !open? false))
                (case (navigate! :schedule {:type :schedule-detail
                                            :id (e/server (:id res))})
                  (token)))
              (token (e/server (:error res))))))))))

;; ---------------------------------------------------------------------------
;; Edit form — the levers of §6.6 that have a control
;; ---------------------------------------------------------------------------

;; Pre:  `user-id` owns `schedule-id`; the four seed values come from the
;;       schedule's own row.
;; Post: a successful save writes through `sched/update-schedule!` and closes
;;       the form, STAYING on the schedule. Create navigates because the user
;;       has nowhere to be yet; an edit is already where it belongs.
;; Inv:  a SIBLING of NewScheduleForm, not that component in an edit mode. The
;;       two differ in four places — the document is fixed here because
;;       `db/update-study-schedule!` deliberately cannot reach `root_topic_id`,
;;       the fields are seeded rather than blank, the parsed command carries no
;;       root id, and the success path does not navigate. A mode flag would
;;       hide four differences behind one boolean.
;; Inv:  θ and excluded pages are NOT here. `FeasibilityPanel` marks those two
;;       levers "(not editable yet)" so the advice stays honest about it.
(e/defn EditScheduleForm [user-id schedule-id seed !open?]
  (e/client
    (let [commits (forms/Form!
                    seed
                    (e/fn Fields [{:keys [target-date first-page last-page capacity]}]
                      (e/amb
                        (FieldLabel "Target date")
                        (forms/Input! :target-date (or target-date "") :type "date"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (FieldLabel "First page")
                        (forms/Input! :first-page (or first-page "1") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (FieldLabel "Last page (blank = to the end)")
                        (forms/Input! :last-page (or last-page "") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (FieldLabel "Daily review capacity")
                        (forms/Input! :capacity (or capacity "60") :type "number"
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})

                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                              :justify-content "flex-end"}})
                          (e/amb
                            (forms/SubmitButton! :label "Save changes"
                              :class "btn btn-primary" :style {:order "1"})
                            (dom/button
                              (dom/props {:class "btn" :type "button"})
                              (dom/text "Cancel")
                              (dom/On "click" (fn [_] (reset! !open? false)) nil)
                              (e/amb))))))
                    ;; Validation HERE, not in the commit handler — Forms5 reads
                    ;; `ex-message` and, for `:type :command`, an `ex-info`
                    ;; disables the submit button instead of letting the user
                    ;; spend a token and be told no afterwards.
                    :Parse (e/fn [fields _tempid]
                             (let [target (str (:target-date fields))]
                               (if (str/blank? target)
                                 (ex-info "Pick a target date." {})
                                 [`UpdateSchedule
                                  {:target_date target
                                   :first_page (parse-int-or-nil (:first-page fields))
                                   :last_page (parse-int-or-nil (:last-page fields))
                                   :daily_capacity (or (parse-int-or-nil (:capacity fields))
                                                     sched/default-daily-capacity)}])))
                    :type :command
                    :show-buttons false)]
      (e/for [[token cmd] commits]
        (let [fields (nth cmd 1)
              res (e/server (e/Offload #(sched/update-schedule! user-id schedule-id fields)))]
          (when (some? res)
            (if (e/server (:ok res))
              ;; Close from inside the SUCCESS branch, so a failure keeps the
              ;; form open with its message (CLAUDE.md, Token Lifecycle).
              (do (e/on-unmount #(reset! !open? false))
                (token))
              (token (e/server (:error res))))))))))

;; ---------------------------------------------------------------------------
;; Schedule list
;; ---------------------------------------------------------------------------

(e/defn ScheduleCard [s navigate! selected?]
  (e/client
    (dom/div
      (dom/props {:class (str "dash-card sched-card" (when selected? " is-selected"))
                  :style {:cursor "pointer" :padding "10px 12px" :margin-bottom "8px"}})
      (dom/On "click" (fn [_] (navigate! :schedule {:type :schedule-detail :id (:id s)})) nil)
      (dom/div
        (dom/props {:style {:font-weight "600" :font-size "13px"
                            :overflow "hidden" :text-overflow "ellipsis"
                            :white-space "nowrap"}})
        (tooltip/Tooltip! (:title s))
        (dom/text (:title s)))
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :margin-top "2px"}})
        (dom/text (str (:target-date s) " · "
                    (if (pos? (:days-left s)) (str (:days-left s) " days left") "today")
                    (when (:first-page s)
                      (str " · pages " (:first-page s) "–"
                        (or (:last-page s) "end")))))))))

(e/defn ArchivedSchedules [user-id navigate! selected-id]
  (e/client
    ;; Archiving is reversible, so the archive has to be reachable. Without
    ;; this section `archived_at` was a one-way door: the row survived, the
    ;; daily load stopped counting it, and nothing could read it back — which
    ;; is deletion wearing another word's name.
    ;;
    ;; A disclosure rather than a second list: an archive is for the rare visit,
    ;; and giving it the same weight as the active set would bury the schedules
    ;; the user is actually working.
    (let [refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          archived (e/server (sched/list-archived-schedules refresh user-id))
          !open? (atom false)
          open? (e/watch !open?)]
      (when (seq archived)
        (dom/div
          (dom/props {:style {:margin-top "14px" :border-top "1px solid var(--color-border)"
                              :padding-top "10px"}})
          (dom/button
            (dom/props {:class "btn" :style {:width "100%" :font-size "12px"
                                             :justify-content "flex-start"}})
            (icons/Icon (if open? :chevron-down :chevron-down) :size 14)
            (dom/text (str "Archived (" (count archived) ")"))
            (dom/On "click" (fn [_] (swap! !open? not)) nil))
          (when open?
            (dom/div
              (dom/props {:style {:margin-top "8px" :opacity "0.75"}})
              (e/for [s (e/diff-by :id archived)]
                (ScheduleCard s navigate! (= (:id s) selected-id))))))))))

(e/defn ScheduleSidebar [user-id navigate! selected-id !new-open?]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          schedules (e/server (sched/list-schedules refresh user-id))]
      (dom/div
        (dom/props {:class "sched-sidebar"})
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:width "100%" :margin-bottom "10px"}})
          (icons/Icon :plus :size 15)
          (dom/text "New schedule")
          (dom/On "click" (fn [_] (reset! !new-open? true)) nil))
        (e/for [s (e/diff-by :id schedules)]
          (ScheduleCard s navigate! (= (:id s) selected-id)))
        (when (empty? schedules)
          (dom/div
            (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"
                                :padding "12px 0"}})
            (dom/text "No schedules yet. Pick a document and a date.")))
        (ArchivedSchedules user-id navigate! selected-id)))))

;; ---------------------------------------------------------------------------
;; Detail pane
;; ---------------------------------------------------------------------------

(e/defn ScheduleHeader [user-id schedule-id navigate! archived? !edit-open?]
  (e/client
    (dom/div
      (dom/props {:style {:display "flex" :align-items "center" :gap "10px"
                          :margin-bottom "12px"}})
      ;; The phone layout replaces the list with this pane, so it needs a way
      ;; back. CSS hides it above 600px, where the list is still on screen and
      ;; a back button would point at something already visible.
      (dom/button
        (dom/props {:class "btn sched-back" :aria-label "Back to schedules"})
        (icons/Icon :chevron-down :size 16)
        (dom/text "Schedules")
        (dom/On "click" (fn [_] (navigate! :schedule)) nil))
      (dom/h2
        (dom/props {:style {:margin "0" :font-size "16px" :flex "1"}})
        (dom/text (if archived? "Forecast · archived" "Forecast")))
      ;; Edit sits beside Archive because both act on the schedule as a whole.
      ;; Hidden while archived: a retired schedule has no plan to revise, and
      ;; Restore is the one action that makes sense there.
      (when-not archived?
        (dom/button
          (dom/props {:class "btn" :style {:font-size "12px"}})
          (dom/text "Edit")
          (dom/On "click" (fn [_] (swap! !edit-open? not)) nil)))
      ;; One button, two directions. Archiving is reversible, so the pane that
      ;; archives must be the pane that restores — an archive reachable only
      ;; through a list you cannot act on is a museum.
      (dom/button
        (dom/props {:class (if archived? "btn btn-primary" "btn")
                    :style {:font-size "12px"}})
        (dom/text (if archived? "Restore" "Archive"))
        (let [[t ?error] (e/Token (dom/On "click" identity nil))]
          ;; BOTH outcomes spend the token. Spending on success alone left a
          ;; failure with the button disabled forever and nothing saying why.
          ;;
          ;; Only ARCHIVE navigates away: the schedule leaves the list the user
          ;; is looking at, so staying would leave them on a pane the sidebar no
          ;; longer offers. A restore puts it back in that list, so the pane
          ;; they are reading is still the right place to be.
          (when t
            (let [res (e/server (e/Offload #(if archived?
                                              (sched/restore-schedule! user-id schedule-id)
                                              (sched/archive-schedule! user-id schedule-id))))]
              (if (e/server (:ok res))
                (if archived?
                  (t)
                  (case (navigate! :schedule) (t)))
                (t (e/server (:error res))))))
          (when ?error
            (dom/span
              (dom/props {:style {:color "var(--color-danger, #b91c1c)" :font-size "12px"}})
              (dom/text (str " " ?error)))))))))

(e/defn ScheduleDetailPane [user-id schedule-id navigate! anki-status]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          ;; A server-FORM binding, not an e/defn call. The forecast carries a
          ;; verdict per page and a load per day; materializing it at a call
          ;; boundary would ship the whole thing to the client, where only
          ;; scalars and the scrolled window are read (CLAUDE.md, 'e/defn
          ;; Returns Materialize at the Call Site').
          forecast (e/server (e/Offload #(sched/forecast refresh user-id schedule-id)))
          archived? (e/server (some? (get-in forecast [:schedule :study_schedules/archived_at])))
          ;; Local to this pane, and keyed on the schedule id below, so opening
          ;; a different schedule never inherits the previous one's open form.
          !edit-open? (atom false)
          edit-open? (e/watch !edit-open?)]
      (ScheduleHeader user-id schedule-id navigate! archived? !edit-open?)
      (when edit-open?
        ;; The seed is read field by field rather than shipped as one row: the
        ;; row carries an array column and a page-range that mean nothing here.
        (e/for-by identity [_k [schedule-id]]
          (EditScheduleForm user-id schedule-id
            {:target-date (e/server (str (get-in forecast [:schedule :study_schedules/target_date])))
             :first-page (e/server (str (or (get-in forecast [:schedule :study_schedules/first_page]) 1)))
             :last-page (e/server (str (or (get-in forecast [:schedule :study_schedules/last_page]) "")))
             :capacity (e/server (str (get-in forecast [:schedule :study_schedules/daily_capacity])))}
            !edit-open?)))
      ;; `:not-found` is its own branch, never a loading message. Electric holds
      ;; this frame pending until the Offload resolves, so a resolved value is
      ;; never "still working" — rendering the absent case as a spinner left a
      ;; stale bookmark waiting forever on a schedule that no longer exists.
      (if (e/server (= :not-found (:status forecast)))
        (dom/div
          (dom/props {:style {:padding "24px" :color "var(--color-text-secondary)"
                              :font-size "13px"}})
          (dom/text "This schedule no longer exists.")
          (dom/button
            (dom/props {:class "btn" :style {:margin-left "10px" :font-size "12px"}})
            (dom/text "Back to schedules")
            (dom/On "click" (fn [_] (navigate! :schedule)) nil)))
        (ScheduleForecast user-id schedule-id forecast anki-status)))))

;; ---------------------------------------------------------------------------
;; Home dashboard
;; ---------------------------------------------------------------------------

;; Pre:  `s` is one row of `sched/schedule-summaries`.
;; Post: a card showing what the user does about this schedule TODAY, plus the
;;       schedule's own status. The status is per-SCHEDULE, which is why the
;;       tab header carries no Anki chip: with FSRS off nothing ingests, so
;;       every row reports "Anki not read yet" on its own rather than one
;;       global banner speaking for figures it cannot vouch for.
(e/defn DashboardCard [s navigate!]
  (e/client
    (dom/div
      (dom/props {:class "dash-card" :style {:cursor "pointer" :padding "12px 14px"
                                             :margin-bottom "10px"}})
      (dom/On "click" (fn [_] (navigate! :schedule {:type :schedule-detail :id (:id s)})) nil)
      (dom/div
        (dom/props {:style {:display "flex" :align-items "baseline" :gap "8px"}})
        (dom/div
          (dom/props {:style {:font-weight "600" :font-size "14px" :flex "1"
                              :overflow "hidden" :text-overflow "ellipsis"
                              :white-space "nowrap"}})
          (tooltip/Tooltip! (:title s))
          (dom/text (:title s)))
        (dom/span
          (dom/props {:class "toolbar-badge"})
          (dom/text (fc/deadline-token (:days-to-target s)))))
      (dom/div
        (dom/props {:style {:margin-top "6px" :font-size "12px"
                            :color "var(--color-text-secondary)"}})
        (dom/text (str (:pace-phrase s)
                    " · " (:covered s) " / " (:total s) " covered"
                    " · " (fc/pages-label (:pages-today s)) " today")))
      ;; :ok needs no line — a working forecast is the ordinary case, and
      ;; labelling it would make the exceptions harder to spot.
      (when (not= :ok (:status s))
        (dom/div
          (dom/props {:style {:margin-top "4px" :font-size "12px"
                              :color "var(--color-text-hint)"}})
          (dom/text (:status-label s)))))))

(e/defn ScheduleDashboard [user-id navigate!]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :schedule-mutations)))
          ;; ONE reader for every row. Mapping a per-schedule reader over the
          ;; list would put a round trip inside the loop; this is a single
          ;; query plus one h=0 projection per schedule, about 5 ms each.
          summaries (e/server (e/Offload #(sched/schedule-summaries refresh user-id)))
          n (e/server (count summaries))]
      (dom/div
        (dom/props {:style {:padding "4px"}})
        (if (zero? n)
          (dom/div
            (dom/props {:style {:padding "32px" :text-align "center"
                                :color "var(--color-text-secondary)"
                                :font-size "14px"}})
            (dom/text "No schedules yet. Pick a document and a date."))
          (e/for [i (e/diff-by identity (range n))]
            (DashboardCard (e/server (nth summaries i)) navigate!)))))))

;; ---------------------------------------------------------------------------
;; Tab entry point (§7.1)
;; ---------------------------------------------------------------------------

(e/defn SchedulePage [user-id navigate!]
  (e/client
    (let [[sub] r/route
          selected-id (when (int? sub) sub)
          !new-open? (atom false)
          new-open? (e/watch !new-open?)
          ;; Headless: acquires from Anki and ingests, rendering nothing. Bound
          ;; rather than mounted as a statement, so its status flows to both
          ;; readers as a value — and so the call cannot be read as an unused
          ;; expression. Mounted at the tab, not at the detail pane, so one
          ;; acquisition serves every schedule (§1.3).
          anki-status (AnkiSync user-id)]
      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                              :margin-bottom "12px"}})
          ;; No Anki chip here. It rendered on all three sub-views, so the home
          ;; view and the new-schedule form both showed "FSRS unavailable" with
          ;; no forecast to qualify it. The acquisition status belongs where a
          ;; forecast depends on it: `StaleBanner` in the detail pane, and a
          ;; per-schedule status line on each dashboard card. `AnkiSync` above
          ;; stays mounted at the tab — one acquisition serves every schedule.
          (dom/h1 (dom/props {:style {:margin "0" :font-size "18px" :flex "1"}})
            (dom/text "Schedule")))

        ;; Layout lives in CSS, not in an inline style map, because it has to
        ;; change with the viewport. `is-detail` tells the stylesheet that a
        ;; pane is open: on a phone the two columns become ONE, showing the
        ;; list or the pane but never both — a 260px sidebar beside a 130px
        ;; pane is two unusable columns instead of one usable one.
        (dom/div
          (dom/props {:class (cond-> "sched-layout"
                               (or selected-id new-open?) (str " is-detail"))})
          (ScheduleSidebar user-id navigate! selected-id !new-open?)
          (dom/div
            (dom/props {:class "sched-pane"})
            (cond
              new-open? (NewScheduleForm user-id navigate! !new-open?)
              selected-id (ScheduleDetailPane user-id selected-id navigate! anki-status)
              :else (ScheduleDashboard user-id navigate!))))))))
