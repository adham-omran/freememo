(ns freememo.ai-tier-views
  "The level views of the AI Features card: the view picker, Simple (one level
   for every task) and By task (one level per task). Advanced is the field list
   in freememo.ai-features-section. See plans/ai-settings-simple-advanced.md.

   The levels and their words are freememo.ai-tiers literals, so the client
   renders them with no server read. Only the {task level} map crosses the wire.

   A level choice is a role=radio button that holds more than a text label, so it
   is not forms/Button! (whose Button* renders `(dom/text label)` only). It
   repeats freememo.segmented/Segment's event handling instead: one Save per
   click, Enter or Space, and aria-busy while that Save runs."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]
   [freememo.ai-tiers :as ai-tiers]
   [freememo.commands :as commands]
   [freememo.segmented :as seg]
   #?(:clj [freememo.settings :as settings])))

;; ── The view choice ──────────────────────────────────────────────────────────
;;
;; Per viewer, in localStorage: which view a person reads the card in is a
;; convenience, not account state, and it must not follow them to another device
;; where they read it differently.

(def views
  [["simple" "Simple"] ["by-task" "By task"] ["advanced" "Advanced"]])

(def ^:private view-storage-key "freememo.ai-settings-view")

(defn load-view
  "The stored view, or \"simple\". Never throws: storage can be blocked."
  []
  #?(:cljs (let [v (try (.getItem js/localStorage view-storage-key)
                        (catch :default _ nil))]
             (if (some #{v} (map first views)) v "simple"))
     :clj "simple"))

(defn choose-view!
  "Show view `v` and remember it for this browser. Post: nil."
  [!view v]
  (reset! !view v)
  #?(:cljs (try (.setItem js/localStorage view-storage-key v)
                (catch :default _ nil))
     :clj nil)
  nil)

(e/defn ViewPicker
  "Simple / By task / Advanced. The choice is local, so Save spends its token as
   soon as the view has changed."
  [!view view]
  (e/client
    (let [SaveView (e/fn [t v] (e/client (case (choose-view! !view v) (t))))]
      (dom/div
        (dom/props {:class "segmented ai-view-picker" :role "radiogroup"
                    :aria-label "Settings view"})
        (e/for [[v label] (e/diff-by first views)]
          (seg/Segment view v label SaveView))))))

;; ── Levels ───────────────────────────────────────────────────────────────────

(defn get-ai-tiers*
  "{task level} for the Settings card. `_refresh` is :settings-refresh, so a
   level write re-reads it."
  [_refresh user-id]
  #?(:clj (settings/get-ai-tiers user-id)
     :cljs nil))

;; Enter/Space on the focused choice. Same transform as seg/Segment's: the event
;; enters the fork, and preventDefault stops Space from scrolling the page and
;; the browser from firing a second, synthetic click.
(defn- activation-key [e]
  #?(:cljs (when (a11y/focused-enter-or-space? e)
             (.preventDefault e)
             e)
     :clj nil))

(e/defn LevelChoice
  "One level, as a role=radio button whose content is `Body`.
   Pre:  `Save` is an (e/fn [t level]) that spends the token.
   Post: `class-name` plus `<class-name>--selected` when `selected` = `level`."
  [selected level class-name aria-label Body Save]
  (e/client
    (let [selected? (= selected level)]
      (dom/button
        (dom/props {:class (str class-name (when selected? (str " " class-name "--selected")))
                    :type "button" :role "radio"
                    :aria-checked (str selected?)
                    :aria-label aria-label})
        (e/for [[t e] (dom/On-all "click")]
          (when e
            (dom/props {:aria-busy "true"})
            (Save t level)))
        (e/for [[t e] (dom/On-all "keydown" activation-key)]
          (when e
            (dom/props {:aria-busy "true"})
            (Save t level)))
        (Body)))))

(e/defn SimpleView
  "One level for every task."
  [user-id tiers !view]
  (e/client
    (let [selected (ai-tiers/uniform-level tiers)
          SaveAll (e/fn [t level]
                    (e/client
                      (let [r (e/server (e/Offload #(settings/save-ai-tier-all user-id level)))]
                        (when (some? r)
                          (if (:success r)
                            (case (e/server (commands/bump! user-id :set-setting))
                              (t))
                            (t (:error r)))))))]
      (dom/div
        (dom/props {:class "ai-levels"})
        (dom/p (dom/props {:class "ai-levels__lead"})
          (dom/text "Choose one level for every AI task."))
        (dom/div
          (dom/props {:class "ai-level-cards" :role "radiogroup"
                      :aria-label "Level for every AI task"})
          (e/for [level (e/diff-by identity ai-tiers/levels)]
            (LevelChoice selected level "ai-level-card" nil
              (e/fn []
                (dom/span (dom/props {:class "ai-level-card__head"})
                  (dom/span (dom/props {:class "ai-level-card__name"})
                    (dom/text (ai-tiers/level-labels level)))
                  (dom/span (dom/props {:class "ai-radio" :aria-hidden "true"})))
                (dom/span (dom/props {:class "ai-price"})
                  (dom/text (ai-tiers/level-prices level)))
                (dom/span (dom/props {:class "ai-level-card__desc"})
                  (dom/text (ai-tiers/level-descriptions level))))
              SaveAll)))
        (when (nil? selected)
          (dom/p
            (dom/props {:class "ai-levels__mixed"})
            (dom/text "Your tasks use different levels. Choose a level to use it for every task, or see each task in ")
            (dom/button
              (dom/props {:type "button" :class "link-button"})
              (dom/On "click" (fn [_] (choose-view! !view "by-task")) nil)
              (dom/text "By task"))
            (dom/text ".")))))))

(e/defn TaskLevelRow
  "One task: its name, what it does, and a radio per level."
  [user-id task level]
  (e/client
    (let [custom? (= level ai-tiers/custom)
          task-label (ai-tiers/task-labels task)
          SaveOne (e/fn [t l]
                    (e/client
                      (let [r (e/server (e/Offload #(settings/save-ai-tier user-id task l)))]
                        (when (some? r)
                          (if (:success r)
                            (case (e/server (commands/bump! user-id :set-setting))
                              (t))
                            (t (:error r)))))))]
      (dom/div
        (dom/props {:class "ai-task-row" :role "radiogroup" :aria-label task-label})
        (dom/div
          (dom/props {:class "ai-task-row__label"})
          (dom/div
            (dom/props {:class "ai-task-row__name"})
            (dom/text task-label)
            (when custom?
              (dom/span (dom/props {:class "ai-level-chip ai-level-chip--custom"})
                (dom/text "Custom"))))
          (dom/div
            (dom/props {:class "hint"})
            (dom/text (if custom?
                        "Custom settings. Choose a level to replace them."
                        (ai-tiers/task-descriptions task)))))
        (e/for [l (e/diff-by identity ai-tiers/levels)]
          (dom/div
            (dom/props {:class "ai-task-row__cell"})
            (LevelChoice level l "ai-radio-button"
              (str task-label ": " (ai-tiers/level-labels l))
              (e/fn [] (dom/span (dom/props {:class "ai-radio" :aria-hidden "true"})))
              SaveOne)))))))

(e/defn ByTaskView
  "One level per task."
  [user-id tiers]
  (e/client
    (dom/div
      (dom/props {:class "ai-task-grid"})
      (dom/div
        (dom/props {:class "ai-task-row ai-task-row--head" :aria-hidden "true"})
        (dom/div (dom/props {:class "ai-levels__lead"})
          (dom/text "Choose a level for each task."))
        (e/for [l (e/diff-by identity ai-tiers/levels)]
          (dom/div
            (dom/props {:class "ai-task-row__cell"})
            (dom/span (dom/props {:class "ai-level-head"})
              (dom/text (ai-tiers/level-labels l)))
            (dom/span (dom/props {:class "ai-price"})
              (dom/text (ai-tiers/level-prices l))))))
      (e/for [task (e/diff-by identity ai-tiers/tasks)]
        (TaskLevelRow user-id task (get tiers task))))))

(e/defn GroupHeading
  "An Advanced group's title and its task's level."
  [title level]
  (e/client
    (dom/div
      (dom/props {:class "settings-group__heading"})
      (dom/h4 (dom/props {:class "settings-group__title"}) (dom/text title))
      (when (some? level)
        (dom/span
          (dom/props {:class (str "ai-level-chip"
                               (when (= level ai-tiers/custom) " ai-level-chip--custom"))})
          (dom/text (if (= level ai-tiers/custom)
                      "Custom"
                      (str "Level: " (ai-tiers/level-labels level)))))))))
