(ns freememo.history-modal
  "Per-topic repetition history modal. Item-level scheduling state
   (priority, interval, A-factor) renders as metric cards above the table;
   the upcoming review is an info-tinted callout; the table itself shows
   past events only as Time / Action / Δ. Triggered from
   ContentToolbar/ExtractActions. Past rows come from db/get-topic-history;
   current scheduling state comes from db/get-topic-next-review."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])))

;; Server bridge — same-namespace wrapper keeps `e/server` codepaths short
;; and shields the modal from cross-ns atom-resolution issues.
(defn get-history* [_refresh topic-id]
  #?(:clj (vec (db/get-topic-history topic-id))
     :cljs []))

(defn get-next* [_refresh topic-id]
  #?(:clj (db/get-topic-next-review topic-id)
     :cljs nil))

;; -- Formatters --------------------------------------------------------------
;; Cross-platform (CLJ + CLJS) so Electric's dual compiler can resolve every
;; call regardless of which side ultimately renders.
;;
;; `coerce-double` defends against non-Double numerics crossing the Transit
;; wire (e.g. PostgreSQL NUMERIC returns as a tagged value in CLJS rather
;; than a JS number) — without it, `.toFixed` fails with "n.toFixed is not
;; a function" on rows from server builds where the SQL cast to float8 has
;; not been hot-reloaded.

(defn- coerce-double [n]
  (cond
    (nil? n) nil
    (number? n) #?(:cljs n :clj (double n))
    :else #?(:cljs (let [p (js/parseFloat (str n))]
                     (when-not (js/isNaN p) p))
             :clj  (try (Double/parseDouble (str n))
                        (catch Exception _ nil)))))

(defn fmt-num
  "Format a number with `decimals` digits after the point; nil/unparseable → \"—\"."
  [n decimals]
  (let [d (coerce-double n)]
    (cond
      (nil? d) "—"
      (zero? decimals) (str #?(:cljs (Math/round d)
                               :clj  (long (Math/round d))))
      :else #?(:cljs (.toFixed d decimals)
               :clj  (format (str "%." decimals "f") d)))))

(defn fmt-days
  "Render an interval in days. nil/unparseable → blank; negative renders with
   leading minus."
  [n]
  (let [d (coerce-double n)]
    (if (nil? d)
      ""
      (str #?(:cljs (.toFixed d 2)
              :clj  (format "%.2f" d))
           "d"))))

(defn fmt-priority [p]
  (if (nil? p) "—" (str p)))

(defn- fmt-interval-days
  "Interval rendered with explicit unit suffix for the metric card."
  [n]
  (let [d (coerce-double n)]
    (if (nil? d) "—" (str (fmt-num d 2) " d"))))

;; event_type → [bg-var fg-var past-tense-label icon-or-nil].
;; All six event types log past actions; the pill encodes them uniformly so
;; the reduced 3-col table loses no information.
(def ^:private event-pill
  {"done"            ["--color-success-light" "--color-success-dark"   "Done"             :check]
   "restore"         ["--color-info-bg-light" "--color-primary"        "Restored"         :refresh-cw]
   "advance"         ["--color-info-bg-light" "--color-primary"        "Advanced"         nil]
   "touch"           ["--color-bg-subtle"     "--color-text-secondary" "Touched"          nil]
   "postpone"        ["--color-warning-light" "--color-warning-dark"   "Postponed"        nil]
   "priority-change" ["--color-bg-subtle"     "--color-text-secondary" "Priority changed" nil]})

(def ^:private default-pill
  ["--color-bg-subtle" "--color-text-secondary" nil nil])

(e/defn MetricCard [label value]
  (e/client
    (dom/div
      (dom/props {:style {:background "var(--color-bg-subtle)"
                          :border-radius "var(--radius-lg)"
                          :padding "10px 12px"
                          :flex "1"
                          :min-width "0"}})
      (dom/div
        (dom/props {:style {:font-size "12px"
                            :font-weight "400"
                            :color "var(--color-text-secondary)"
                            :text-transform "uppercase"
                            :letter-spacing "0.04em"
                            :margin-bottom "4px"}})
        (dom/text label))
      (dom/div
        (dom/props {:style {:font-size "18px"
                            :font-weight "500"
                            :color "var(--color-text-primary)"}})
        (dom/text value)))))

(e/defn UpNextCallout [next-info]
  (e/client
    (let [next-at (:next_review_at next-info)
          status  (:status next-info)]
      ;; Done topics keep their last next_review_at in the row (only restore-topic!
      ;; nulls it; see db.clj/done-topic!). Filter by status, not by date alone.
      (when (and next-at (not= status "done"))
        (dom/div
          (dom/props {:style {:display "flex"
                              :align-items "center"
                              :gap "10px"
                              :background "var(--color-info-bg-light)"
                              :color "var(--color-primary-text)"
                              :border-radius "var(--radius-lg)"
                              :padding "10px 12px"
                              :margin-top "var(--sp-3)"}})
          (icons/Icon :clock :size 18)
          (dom/div
            (dom/props {:style {:display "flex" :flex-direction "column" :gap "2px" :min-width "0"}})
            (dom/div
              (dom/props {:style {:font-size "12px" :font-weight "500"}})
              (dom/text "Up next"))
            (dom/div
              (dom/props {:style {:font-size "13px" :font-weight "400"}})
              (dom/span
                (dom/props {:style {:font-family "ui-monospace, monospace"}})
                (dom/text next-at))
              (dom/span (dom/text " · "))
              (dom/span (dom/text (or status "—"))))))))))

(e/defn ActionPill [event-type]
  (e/client
    (let [[bg fg label icon] (get event-pill event-type default-pill)
          display-label (or label event-type)]
      (dom/span
        (dom/props {:style {:display "inline-flex"
                            :align-items "center"
                            :gap "4px"
                            :padding "2px 10px"
                            :border-radius "var(--radius-pill)"
                            :font-size "12px"
                            :font-weight "500"
                            :background (str "var(" bg ")")
                            :color (str "var(" fg ")")}})
        (when icon (icons/Icon icon :size 12))
        (dom/text display-label)))))

(e/defn HistoryTableHeader []
  (e/client
    (let [th {:padding "8px 8px"
              :border-bottom "0.5px solid var(--color-border-light)"
              :font-weight "500"
              :color "var(--color-text-secondary)"
              :text-align "left"
              :font-size "12px"
              :text-transform "uppercase"
              :letter-spacing "0.04em"}]
      (dom/thead
        (dom/props {:style {:display "contents"}})
        (dom/tr
          (dom/props {:style {:display "contents"}})
          (dom/th (dom/props {:style th}) (dom/text "Time"))
          (dom/th (dom/props {:style th}) (dom/text "Action"))
          (dom/th (dom/props {:style (assoc th :text-align "right")}) (dom/text "Δ")))))))

(e/defn HistoryRow [row]
  (e/client
    (let [td {:padding "6px 8px"
              :border-bottom "0.5px solid var(--color-border-light)"
              :font-size "13px"
              :white-space "nowrap"
              :overflow "hidden"
              :text-overflow "ellipsis"
              :display "flex"
              :align-items "center"
              :height "36px"}
          mono (assoc td :font-family "ui-monospace, monospace")]
      (dom/tr
        (dom/props {:style {:display "contents"}})
        (dom/td (dom/props {:style mono})
          (tooltip/Tooltip! (str (:event_at row)))
          (dom/text (or (:event_at row) "—")))
        (dom/td (dom/props {:style td})
          (ActionPill (:event_type row)))
        (dom/td (dom/props {:style (assoc mono :justify-content "flex-end")})
          (dom/text (fmt-days (:interval_since_prev_days row))))))))

;; Pre:  `topic-id` identifies an existing topic owned by the current user.
;;       `!open?` is the modal's open/closed atom — caller flips true to
;;       open; modal flips false on dismiss (X / Escape / backdrop click).
;;       `refresh` is the already-watched value of the user's :refresh
;;       channel. Bumped by session mutations so the table re-queries
;;       reactively.
;; Post: While `@!open?` is true, a fixed backdrop + modal renders. Metric
;;       cards reflect the topic's CURRENT scheduling parameters. The
;;       Up-next callout shows the scheduled next review (omitted when
;;       next_review_at is nil). The table lists past events newest-first.
(e/defn HistoryModal [topic-id !open? refresh]
  (e/client
    (let [open? (e/watch !open?)]
      (when open?
        (let [next-info (e/server (get-next* refresh topic-id))
              topic-title (:title next-info)
              rows (e/server (get-history* refresh topic-id))
              row-count (e/server (count rows))]
          (dom/div
            (dom/props {:class "modal-backdrop" :tabindex "-1"})
            (modal/ModalEscape (fn [] (reset! !open? false)) "History")
            (dom/On "click" (fn [_] (reset! !open? false)) nil)
            (dom/div
              (dom/props {:class "modal-content"
                          :style {:width "min(680px, 95vw)"
                                  :max-height "85vh"
                                  :overflow "hidden"
                                  :display "flex"
                                  :flex-direction "column"
                                  :padding "0"}})
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)
              ;; Header
              (dom/div
                (dom/props {:style {:padding "16px 20px"
                                    :border-bottom "0.5px solid var(--color-border-light)"
                                    :display "flex"
                                    :align-items "center"
                                    :justify-content "space-between"
                                    :gap "12px"}})
                (dom/div
                  (dom/props {:style {:min-width "0"}})
                  (dom/h3
                    (dom/props {:style {:margin "0" :font-size "16px" :font-weight "500"}})
                    (dom/text "Repetition history"))
                  (dom/div
                    (dom/props {:style {:font-size "13px"
                                        :font-weight "400"
                                        :color "var(--color-text-secondary)"
                                        :margin-top "2px"
                                        :overflow "hidden"
                                        :text-overflow "ellipsis"
                                        :white-space "nowrap"}})
                    (dom/text (str (or topic-title "Topic")
                                " · " row-count " event" (when (not= row-count 1) "s")))))
                (dom/button
                  (dom/props {:aria-label "Close"
                              :style {:width "28px"
                                      :height "28px"
                                      :display "inline-flex"
                                      :align-items "center"
                                      :justify-content "center"
                                      :flex "0 0 28px"
                                      :background "transparent"
                                      :border "0.5px solid var(--color-border)"
                                      :border-radius "var(--radius-md)"
                                      :color "var(--color-text-secondary)"
                                      :cursor "pointer"
                                      :padding "0"}})
                  (icons/Icon :x :size 14)
                  (dom/On "click" (fn [_] (reset! !open? false)) nil)))
              ;; Body
              (dom/div
                (dom/props {:style {:padding "16px 20px"
                                    :overflow "auto"
                                    :flex "1 1 auto"}})
                ;; Metric cards
                (when next-info
                  (dom/div
                    (dom/props {:style {:display "flex"
                                        :gap "var(--sp-3)"}})
                    (MetricCard "Priority" (fmt-priority (:priority next-info)))
                    (MetricCard "Interval" (fmt-interval-days (:interval_days next-info)))
                    (MetricCard "A-Factor" (fmt-num (:a_factor next-info) 2))))
                ;; Up-next callout
                (UpNextCallout next-info)
                ;; History section label
                (dom/div
                  (dom/props {:style {:font-size "12px"
                                      :font-weight "400"
                                      :color "var(--color-text-secondary)"
                                      :text-transform "uppercase"
                                      :letter-spacing "0.04em"
                                      :margin "16px 0 8px"}})
                  (dom/text "History"))
                ;; Table
                (dom/table
                  (dom/props {:style {:width "100%"
                                      :display "grid"
                                      :grid-template-columns "180px 1fr 80px"
                                      :font-size "13px"}})
                  (HistoryTableHeader)
                  (if (pos? row-count)
                    (e/for [row (e/server (e/diff-by :id rows))]
                      (HistoryRow row))
                    (dom/tr
                      (dom/props {:style {:display "contents"}})
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1"
                                            :text-align "center"
                                            :padding "24px 12px"
                                            :color "var(--color-text-secondary)"
                                            :font-size "13px"
                                            :font-weight "400"}})
                        (dom/text "No repetitions yet. Mark this topic Next/Postpone/Done in a learn session to log events.")))))))))))))
