(ns freememo.history-modal
  "Per-topic repetition history modal. Mirrors SuperMemo's Repetition History
   window: chronological table of session-driven events with pre-mutation
   snapshot of SR-relevant fields. Triggered from ContentToolbar/ExtractActions.
   Rows come from db/get-topic-history (server-formatted timestamps + LAG-derived
   interval gap). A leading 'Next' row shows the topic's currently-scheduled
   next review, mirroring SuperMemo's top-of-history Next entry."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   #?(:clj [freememo.db :as db])))

;; Server bridge — same-namespace wrapper keeps `e/server` codepaths short
;; and shields the modal from cross-ns atom-resolution issues.
(defn get-history* [_refresh topic-id]
  #?(:clj (vec (db/get-topic-history topic-id))
     :cljs []))

(defn get-next* [_refresh topic-id]
  #?(:clj (db/get-topic-next-review topic-id)
     :cljs nil))

;; Document-level Escape listener. Kept as a plain (defn) so
;; addEventListener fires exactly once per modal open/close cycle —
;; Electric re-evaluates dom-body let-bindings unpredictably, so an
;; (.addEventListener ...) inside the reactive graph would attach
;; duplicates on every tick. Returns a 0-arg cleanup fn to pass to
;; e/on-unmount.
(defn install-escape-listener! [!open?]
  #?(:cljs
     (let [on-key (fn [e]
                    (when (= (.-key e) "Escape")
                      (reset! !open? false)))]
       (.addEventListener js/document "keydown" on-key)
       (fn [] (.removeEventListener js/document "keydown" on-key)))
     :clj (fn [] nil)))

;; -- Formatters --------------------------------------------------------------
;; Cross-platform (CLJ + CLJS) so Electric's dual compiler can resolve every
;; call regardless of which side ultimately renders. Inputs are scalars from
;; db queries (strings + numbers); date formatting itself happens server-side
;; via TO_CHAR.
;;
;; `coerce-double` defends against non-Double numerics crossing the Transit
;; wire (e.g. PostgreSQL NUMERIC returns as a tagged value in CLJS rather
;; than a JS number). Without this coercion, `.toFixed` fails with
;; "n.toFixed is not a function" on rows from server builds where the SQL
;; cast to float8 has not been hot-reloaded.

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
   leading minus (a 'next' row may be overdue)."
  [n]
  (let [d (coerce-double n)]
    (if (nil? d)
      ""
      (str #?(:cljs (.toFixed d 2)
              :clj  (format "%.2f" d))
           "d"))))

(defn fmt-priority [p]
  (if (nil? p) "—" (str p)))

(defn event-label
  "User-facing label for an event_type."
  [t]
  (case t
    "advance"         "Advance"
    "touch"           "Touch"
    "postpone"        "Postpone"
    "done"            "Done"
    "restore"         "Restore"
    "priority-change" "Priority"
    t))

(defn event-color
  "Color hint for the event-type badge."
  [t]
  (case t
    "advance"         "var(--color-primary)"
    "touch"           "var(--color-text-secondary)"
    "postpone"        "var(--color-warning, #b8860b)"
    "done"            "var(--color-success-dark)"
    "restore"         "var(--color-primary)"
    "priority-change" "var(--color-text-secondary)"
    "var(--color-text-secondary)"))

;; Grid: time | Δ since prev | event | status | priority | interval | a-factor | next | last
;; Timestamp columns (Time/Next/Last) sized to fit "YYYY-MM-DD HH:MM:SS" in a
;; 13px monospace font without truncation. Total = 970px; modal width sized
;; to accommodate.
(def ^:private grid-cols "180px 60px 90px 70px 60px 80px 70px 180px 180px")

(e/defn HistoryHeader []
  (e/client
    (let [th-base {:padding "8px 8px"
                   :border-bottom "2px solid var(--color-border)"
                   :font-weight "600"
                   :color "var(--color-text-primary)"
                   :text-align "left"
                   :font-size "12px"
                   :text-transform "uppercase"
                   :letter-spacing "0.04em"}]
      (dom/thead
        (dom/props {:style {:display "contents"}})
        (dom/tr
          (dom/props {:style {:display "contents"}})
          (dom/th (dom/props {:style th-base}) (dom/text "Time"))
          (dom/th (dom/props {:style (assoc th-base :text-align "right")}) (dom/text "Δ"))
          (dom/th (dom/props {:style th-base}) (dom/text "Event"))
          (dom/th (dom/props {:style th-base}) (dom/text "Status"))
          (dom/th (dom/props {:style (assoc th-base :text-align "right")}) (dom/text "Priority"))
          (dom/th (dom/props {:style (assoc th-base :text-align "right")}) (dom/text "Interval"))
          (dom/th (dom/props {:style (assoc th-base :text-align "right")}) (dom/text "A-Factor"))
          (dom/th (dom/props {:style th-base}) (dom/text "Next"))
          (dom/th (dom/props {:style th-base}) (dom/text "Last")))))))

(e/defn NextRow [next-info]
  (e/client
    (let [td {:padding "6px 8px"
              :border-bottom "1px dashed var(--color-border)"
              :background "var(--color-bg-subtle, #f5f7fa)"
              :font-size "13px"
              :white-space "nowrap"
              :overflow "hidden"
              :text-overflow "ellipsis"}
          status (:status next-info)
          next-at (:next_review_at next-info)
          days (:days_until_next next-info)]
      (dom/tr
        (dom/props {:style {:display "contents"}})
        (dom/td (dom/props {:style (assoc td :font-family "ui-monospace, monospace")})
          (dom/text (or next-at "—")))
        (dom/td (dom/props {:style (assoc td :text-align "right")})
          (dom/text (if (nil? days) "" (fmt-days days))))
        (dom/td (dom/props {:style (assoc td :font-weight "600" :color "var(--color-primary)")})
          (dom/text "Next"))
        (dom/td (dom/props {:style td})
          (dom/text (or status "—")))
        (dom/td (dom/props {:style (assoc td :text-align "right")}) (dom/text ""))
        (dom/td (dom/props {:style (assoc td :text-align "right")}) (dom/text ""))
        (dom/td (dom/props {:style (assoc td :text-align "right")}) (dom/text ""))
        (dom/td (dom/props {:style td}) (dom/text ""))
        (dom/td (dom/props {:style td}) (dom/text ""))))))

(e/defn HistoryRow [row]
  (e/client
    (let [td-base {:padding "6px 8px"
                   :border-bottom "1px solid var(--color-bg-subtle)"
                   :font-size "13px"
                   :height "36px"
                   :line-height "24px"
                   :white-space "nowrap"
                   :overflow "hidden"
                   :text-overflow "ellipsis"}
          mono (assoc td-base :font-family "ui-monospace, monospace")
          right (assoc td-base :text-align "right")
          event-type (:event_type row)]
      (dom/tr
        (dom/props {:style {:display "contents"}})
        ;; Time
        (dom/td (dom/props {:style mono :data-tooltip (str (:event_at row))})
          (dom/text (or (:event_at row) "—")))
        ;; Δ since prev
        (dom/td (dom/props {:style right})
          (dom/text (fmt-days (:interval_since_prev_days row))))
        ;; Event-type badge
        (dom/td (dom/props {:style td-base})
          (dom/span
            (dom/props {:style {:display "inline-block"
                                :padding "2px 8px"
                                :border-radius "10px"
                                :font-size "11px"
                                :font-weight "600"
                                :color "white"
                                :background (event-color event-type)}})
            (dom/text (event-label event-type))))
        ;; Status before
        (dom/td (dom/props {:style td-base})
          (dom/text (or (:status_before row) "—")))
        ;; Priority before
        (dom/td (dom/props {:style right})
          (dom/text (fmt-priority (:priority_before row))))
        ;; Interval-days before
        (dom/td (dom/props {:style right})
          (dom/text (fmt-num (:interval_days_before row) 2)))
        ;; A-factor before
        (dom/td (dom/props {:style right})
          (dom/text (fmt-num (:a_factor_before row) 2)))
        ;; Next-review before
        (dom/td (dom/props {:style (assoc mono :font-size "12px")
                            :data-tooltip (str (:next_review_at_before row))})
          (dom/text (or (:next_review_at_before row) "—")))
        ;; Last-review before
        (dom/td (dom/props {:style (assoc mono :font-size "12px")
                            :data-tooltip (str (:last_review_at_before row))})
          (dom/text (or (:last_review_at_before row) "—")))))))

;; Pre:  `topic-id` identifies an existing topic owned by the current user.
;;       `!open?` is the modal's open/closed atom — the caller flips it true
;;       to open and the modal flips it false when the user dismisses.
;;       `refresh` is the already-watched value of the user's :refresh channel
;;       (caller does `(e/server (e/watch (us/get-atom user-id :refresh)))`).
;;       Bumped by session mutations so the table re-queries reactively.
;; Post: While `@!open?` is true, a fixed backdrop + modal renders. Rows are
;;       re-queried whenever `refresh` changes so a fresh rep advances the
;;       table immediately after the user clicks Next/Done/Postpone.
(e/defn HistoryModal [topic-id !open? refresh]
  (e/client
    (let [open? (e/watch !open?)]
      (when open?
        (let [next-info (e/server (get-next* refresh topic-id))
              topic-title (:title next-info)
              rows (e/server (get-history* refresh topic-id))
              row-count (count rows)]
          (dom/div
            (dom/props {:class "modal-backdrop"})
            (dom/On "click" (fn [_] (reset! !open? false)) nil)
            ;; Escape-to-close. Document-level listener (vs. dom/On "keydown"
            ;; on this div, which would require focus on the backdrop to fire).
            ;; let-bind the cleanup so install runs exactly once at mount —
            ;; mirrors toolbar_generate_dropdown.cljc:install-dropdown-listeners!.
            (let [cleanup (install-escape-listener! !open?)]
              (e/on-unmount cleanup))
            (dom/div
              (dom/props {:class "modal-content modal-lg"
                          :style {:width "min(1020px, 95vw)"
                                  :max-height "85vh"
                                  :overflow "hidden"
                                  :display "flex"
                                  :flex-direction "column"}})
              (dom/On "click" (fn [e] (.stopPropagation e)) nil)
              ;; Header
              (dom/div
                (dom/props {:style {:padding "16px 20px"
                                    :border-bottom "1px solid var(--color-border)"
                                    :display "flex"
                                    :align-items "center"
                                    :justify-content "space-between"}})
                (dom/div
                  (dom/h3
                    (dom/props {:style {:margin "0" :font-size "16px" :font-weight "600"}})
                    (dom/text "Repetition history"))
                  (dom/div
                    (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)" :margin-top "2px"}})
                    (dom/text (str (or topic-title "Topic")
                                " · " row-count " event" (when (not= row-count 1) "s")))))
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary" :aria-label "Close"})
                  (icons/Icon :x :size 16)
                  (dom/On "click" (fn [_] (reset! !open? false)) nil)))
              ;; Table
              (dom/div
                (dom/props {:class "tape-scroll"
                            :style {:flex "1 1 auto"
                                    :overflow "auto"
                                    :padding "0 4px 8px"
                                    :--row-height "36px"}})
                (dom/table
                  (dom/props {:style {:width "100%"
                                      :display "grid"
                                      :grid-template-columns grid-cols
                                      :font-size "13px"}})
                  (HistoryHeader)
                  (when next-info (NextRow next-info))
                  (if (pos? row-count)
                    (e/for [row (e/diff-by :id rows)]
                      (HistoryRow row))
                    (dom/tr
                      (dom/td
                        (dom/props {:style {:grid-column "1 / -1"
                                            :text-align "center"
                                            :padding "24px 12px"
                                            :color "var(--color-text-secondary)"
                                            :font-size "13px"}})
                        (dom/text "No repetitions yet. Mark this topic Next/Postpone/Done in a learn session to log events.")))))))))))))
