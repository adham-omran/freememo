(ns freememo.knowledge-known
  "The Known ideas sub-view of Knowledge and one idea's page
   (plans/known-ideas.md §3, §5): what the reader knows, how they know it,
   the cards made from an idea, and a check of them in the Quiz.

   Its own namespace for the 64KB method cap (§6.5). `KnowledgePage` mounts
   both views and stays the one owner of the tab title."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.forms-ext :as forms-ext]
   [freememo.navigation :as nav]
   [freememo.question-pin-state :as pin-state]
   [freememo.quiz-page :as quiz-page]
   [freememo.tape-table :refer [TapeTable]]
   [freememo.util :as util]
   #?(:clj [freememo.known-ideas :as ki])
   #?(:clj [freememo.logging :as log])
   #?(:clj [freememo.user-state :as us])))

;; ── Server reads and writes ────────────────────────────────────────
;; Body-level reader conditionals: each name appears inside an (e/server …)
;; form, which CLJS compiles too.

(defn list-rows* [_rev user-id]
  #?(:clj (ki/list-rows user-id) :cljs nil))

(defn idea-page* [_rev user-id entity-id]
  #?(:clj (when (int? entity-id) (ki/idea-page user-id entity-id)) :cljs nil))

(defn run-idea-command!*
  "Run one idea command. `kind` is :check, :make, :undo, :unlearn or
   :open-chat, and `arg` is an entity id, or a vector of them for :check.
   Post: {:ok true …} or {:error message}."
  [user-id kind arg]
  #?(:clj (case kind
            :check {:ok true :card-ids (ki/check-card-ids user-id arg)}
            :make (ki/make-cards! user-id arg)
            :undo (ki/undo-self-mark! user-id arg)
            :unlearn (ki/not-learned! user-id arg)
            :open-chat (if-let [topic-id (ki/open-proof-chat! user-id arg)]
                         {:ok true :topic-id topic-id}
                         {:error "That chat is no longer there."})
            ;; An unknown kind is a caller bug. It must not fail the session.
            (do (log/log-warn (str "Unknown idea command " (pr-str kind) " " (pr-str arg)))
                {:error "That action is not available."}))
     :cljs nil))

;; ── Client helpers ─────────────────────────────────────────────────

(def source-options
  [["" "All sources"] ["tutor" "Tutor confirmed"] ["self" "Marked by you"]])

(defn filter-rows
  "The rows the filters keep (§3.7): `query` in the label or the origin text,
   `source` among the idea's sources, and only due ideas when `due-only?`."
  [rows query source due-only?]
  (let [q (some-> query str/trim not-empty str/lower-case)]
    (filterv (fn [{:keys [label from sources due?]}]
               (and (or (nil? q)
                        (str/includes? (str/lower-case (str label)) q)
                        (str/includes? (str/lower-case (str from)) q))
                    (or (str/blank? source) (some #{source} sources))
                    (or (not due-only?) due?)))
      rows)))

(defn- open-idea! [navigate! entity-id]
  (navigate! :knowledge {:type :knowledge-known-idea :entity-id entity-id}))

(defonce ^:private !scroll-to-cards?
  ;; Set by a list row's Cards link, read once by the idea page's cards
  ;; section (§3.6).
  (atom false))

(defn- open-idea-cards! [navigate! entity-id]
  (reset! !scroll-to-cards? true)
  (open-idea! navigate! entity-id))

(defn- scroll-cards-into-view!
  "Scroll `node` into view once, when a Cards link asked for it."
  [node]
  #?(:cljs (when (compare-and-set! !scroll-to-cards? true false)
             (js/requestAnimationFrame #(.scrollIntoView node #js {:block "start"})))
     :clj nil)
  nil)

(defn- follow-idea-command!
  "What the client does after a command succeeds: a check opens the Quiz on
   the cards (§4.1), and Open the chat opens its document (§5.3.2)."
  [kind result navigate!]
  (case kind
    :check (when (seq (:card-ids result))
             (reset! quiz-page/!pending-preset {:card-ids (:card-ids result)})
             (navigate! :quiz))
    :open-chat (navigate! :viewer (nav/nav-topic (:topic-id result) :knowledge))
    nil)
  nil)

(defn- open-file-on-map!
  "Open `topic-id` in the viewer with the Map tab showing (§5.4)."
  [navigate! topic-id]
  (pin-state/show-view! :map)
  (navigate! :viewer (nav/nav-topic topic-id :knowledge)))

;; ── Components ─────────────────────────────────────────────────────

(e/defn IdeaAction
  "One button that runs one idea command (§3.8, §5.3) and handles its own
   result, as the map view's rows do: the command never leaves this frame.
   `tip` sits on a wrapper, because a disabled button takes no hover. A
   success that leaves the page follows it once the token is spent."
  [user-id navigate! command label class disabled? tip]
  (e/client
    (dom/span
      (dom/props {:class "known-ideas__tip" :data-tooltip tip})
      (e/for [[t clicked] (e/diff-by first
                            (e/as-vec (forms/Button! command :class class :label label
                                        :disabled disabled?)))]
        (let [[kind arg] clicked
              result (e/server (e/Offload #(run-idea-command!* user-id kind arg)))]
          (case result
            (if (:error result)
              (t (:error result))
              (do (e/on-unmount #(follow-idea-command! kind result navigate!))
                  (t)))))))))

(e/defn CheckButton
  "Check me on `entity-ids`, disabled with `Make cards first` when they have
   no cards (§4.2)."
  [user-id navigate! entity-ids card-count label]
  (e/client
    (IdeaAction user-id navigate! [:check (vec entity-ids)] label "btn btn-sm btn-primary"
      (zero? card-count) (when (zero? card-count) "Make cards first"))))

(e/defn MakeCardsButton
  "Make cards from an idea, disabled when it has no passage (§2.1)."
  [user-id navigate! entity-id material? label]
  (e/client
    (IdeaAction user-id navigate! [:make entity-id] label "btn btn-sm btn-secondary"
      (not material?) (when-not material? "No passage to make cards from"))))

(e/defn RowMore
  "The row's overflow actions (§3.8), opened in place: a menu that drops out
   of a row would be clipped by the scroll box. Keyed on the idea, so a
   recycled row never shows another idea's open menu."
  [user-id navigate! {:keys [entity_id card_count material? self?]}]
  (e/client
    (e/for-by identity [id [entity_id]]
      (let [!open? (atom false) open? (e/watch !open?)]
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary"
                      :aria-expanded (str open?)})
          (dom/text (if open? "Less" "More"))
          (dom/On "click" (fn [_] (swap! !open? not)) nil))
        (when open?
          (when (pos? card_count)
            (MakeCardsButton user-id navigate! id material? "Make more"))
          (when self?
            (IdeaAction user-id navigate! [:undo id] "Undo my mark" "btn btn-sm btn-secondary"
              false nil))
          (IdeaAction user-id navigate! [:unlearn id] "Not learned yet" "btn btn-sm btn-secondary"
            false nil))))))

(e/defn IdeaRow
  "One known idea (§3.2). Each action runs in its own button, so a row that
   scrolls away takes its pending action with it."
  [user-id {:keys [entity_id label how from checked due? card_count material?] :as row}
   i navigate!]
  (e/client
    (dom/tr
      (dom/props {:class (str (when (even? i) "row-alt") (when due? " known-ideas__row--due"))
                  :style {:--order i}})
      (dom/td
        (dom/button
          (dom/props {:type "button" :class "known-ideas__idea tape-cell-text" :dir "auto"
                      :data-tooltip label})
          (dom/text label)
          (dom/On "click" (fn [_] (open-idea! navigate! entity_id)) nil)))
      (dom/td (dom/props {:class "tape-cell-meta"})
        (dom/span (dom/props {:class "tape-cell-text" :data-tooltip how}) (dom/text how)))
      (dom/td (dom/props {:class "tape-cell-meta"})
        (dom/span (dom/props {:class "tape-cell-text" :data-tooltip from}) (dom/text from)))
      (dom/td (dom/props {:class (if due? "known-ideas__checked--due" "tape-cell-meta")})
        (dom/text checked))
      (dom/td
        (if (pos? card_count)
          (dom/button
            (dom/props {:type "button" :class "known-ideas__cards"})
            (dom/text (str card_count (if (= 1 card_count) " card" " cards")))
            (dom/On "click" (fn [_] (open-idea-cards! navigate! entity_id)) nil))
          (MakeCardsButton user-id navigate! entity_id material? "Make cards")))
      (dom/td (dom/props {:class "tape-cell-actions"})
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
          (dom/text "Open")
          (dom/On "click" (fn [_] (open-idea! navigate! entity_id)) nil))
        (CheckButton user-id navigate! [entity_id] card_count "Check me")
        (RowMore user-id navigate! row)))))

(e/defn DueBand
  "The ideas due for a check, above the table, hidden when none are (§3.4)."
  [user-id due-labels due-ids navigate!]
  (e/client
    (when (seq due-ids)
      (dom/div
        (dom/props {:class "known-ideas__due" :role "status"})
        (dom/div
          (dom/props {:class "known-ideas__due-text"})
          (dom/text (str (count due-ids) (if (= 1 (count due-ids)) " idea is" " ideas are")
                      " due for a check: " (str/join ", " due-labels))))
        ;; A due idea has a scheduled card, so it always has cards.
        (CheckButton user-id navigate! due-ids 1
          (str "Check me on " (if (= 1 (count due-ids)) "this one" (str "these " (count due-ids)))))))))

(def ^:private list-grid
  "minmax(180px,1.4fr) minmax(170px,1fr) minmax(150px,1fr) 130px 100px minmax(230px,max-content)")

(e/defn KnownIdeasView
  "The Known ideas list (§3): a due band, filters and the table. Every read
   watches the channels of §1.5.4, so an action changes its row with no
   reload."
  [user-id navigate!]
  (e/client
    (let [!query (atom "") query (e/watch !query)
          !query-timer (atom nil)
          !source (atom "") source (e/watch !source)
          !due-only? (atom false) due-only? (e/watch !due-only?)
          rows (e/server
                 (let [rev (+ (e/watch (us/get-atom user-id :question-mutations))
                             (e/watch (us/get-atom user-id :card-mutations))
                             (e/watch (us/get-atom user-id :kg-mutations)))]
                   (e/Offload #(list-rows* rev user-id))))
          shown (e/server (filter-rows rows query source due-only?))
          item-count (e/server (count shown))
          total (e/server (count rows))
          due-labels (e/server (into [] (comp (filter :due?) (map :label)) rows))
          due-ids (e/server (into [] (comp (filter :due?) (map :entity_id)) rows))]
      (dom/div
        (dom/props {:class "known-ideas"})
        (DueBand user-id due-labels due-ids navigate!)
        (dom/div
          (dom/props {:class "known-ideas__filters"})
          (dom/input
            (dom/props {:placeholder "Search ideas and files…" :class "input known-ideas__search"
                        :aria-label "Search known ideas"})
            (dom/On "input"
              (fn [ev] (util/debounce! !query-timer !query (.. ev -target -value) 300))
              nil))
          (e/for [[t {picked :source}] (forms-ext/Select! :source source source-options
                                         :class "input" :aria-label "Filter by source")]
            (case (reset! !source picked) (t)))
          (dom/button
            (dom/props {:type "button"
                        :class (str "filter-chip" (when due-only? " filter-chip-active"))
                        :aria-pressed (boolean due-only?)})
            (dom/text "Due only")
            (dom/On "click" (fn [_] (swap! !due-only? not)) nil))
          (dom/span (dom/props {:class "known-ideas__count"})
            (dom/text (str item-count (if (= 1 item-count) " idea" " ideas")))))
        (TapeTable ["Idea" "How you know it" "From" "Last checked" "Cards" ""]
          list-grid item-count [query source due-only?]
          (if (zero? total)
            "No known ideas yet. Pin questions on a repository file and choose Ask, or mark an idea in a question map."
            "No ideas match the filters.")
          (e/fn [i]
            (when-let [row (e/server (nth shown i nil))]
              (IdeaRow user-id row i navigate!))))))))

;; ── The idea page ──────────────────────────────────────────────────

(defn- rating-word [rating]
  (case rating 1 "again" 2 "hard" 3 "good" 4 "easy" nil))

(defn- mark-text [{:keys [source date]}]
  (str (if (= "self" source) "Marked by you" "Tutor confirmed") (when date (str " (" date ")"))))

(defn- root-text [{:keys [status learned]}]
  (case status
    "learned" (str "learned here" (when learned (str " (" learned ")")))
    "next" "being taught now"
    "on the map"))

(e/defn IdeaCards
  "Cards from this idea, with their sync state, and Make more (§5.3.4)."
  [user-id navigate! {:keys [entity_id cards material?]}]
  (e/client
    (dom/section
      (dom/props {:class "idea-page__card" :id "idea-cards" :aria-labelledby "idea-cards-h"})
      (e/snapshot (scroll-cards-into-view! dom/node))
      (dom/div
        (dom/props {:class "idea-page__head"})
        (dom/h2 (dom/props {:id "idea-cards-h"}) (dom/text "Cards from this idea"))
        (MakeCardsButton user-id navigate! entity_id material?
          (if (seq cards) "Make more" "Make cards")))
      (dom/ul
        (dom/props {:class "idea-page__cards"})
        (e/for [card (e/diff-by :id cards)]
          (dom/li
            (dom/span (dom/props {:class (str "type-badge type-badge-" (:kind card))})
              (dom/text (:kind card)))
            (dom/span (dom/props {:class "idea-page__card-text" :dir "auto"})
              (dom/text (:text card)))
            (dom/span (dom/props {:class (if (:synced? card)
                                           "idea-page__sync"
                                           "idea-page__sync idea-page__sync--pending")})
              (dom/text (if (:synced? card) "In Anki" "Not synced"))))))
      (when (empty? cards)
        (dom/p (dom/props {:class "idea-page__muted"}) (dom/text "No cards yet."))))))

(e/defn IdeaChecks
  "Last check, its result, the next check and Check me again (§5.3.5)."
  [user-id navigate! {:keys [entity_id cards last-check next-check last-rating due?]}]
  (e/client
    (dom/section
      (dom/props {:class "idea-page__card" :aria-labelledby "idea-checks-h"})
      (dom/h2 (dom/props {:id "idea-checks-h"}) (dom/text "Checks"))
      (dom/p
        (dom/text
          (if last-check
            (str "Last checked " last-check
              (when-let [w (rating-word last-rating)] (str ", rated " w)) ". "
              (if next-check (str "Next check " next-check (when due? ", due now") ".") ""))
            "Never checked.")))
      (CheckButton user-id navigate! [entity_id] (count cards) "Check me again"))))

(e/defn IdeaBody
  "The sections of one idea's page (§5.3)."
  [user-id idea navigate!]
  (e/client
    (let [{:keys [entity_id label marks roots proof events]} idea
          self? (some #(= "self" (:source %)) marks)]
      (dom/div
        (dom/props {:class "idea-page__title-row"})
        (dom/h1 (dom/props {:dir "auto"}) (dom/text label))
        (e/for [m (e/diff-by :source marks)]
          (dom/span (dom/props {:class (str "idea-page__badge idea-page__badge--" (:source m))})
            (dom/text (mark-text m)))))
      (dom/div
        (dom/props {:class "idea-page__grid"})
        (dom/div
          (dom/props {:class "idea-page__main"})
          (dom/section
            (dom/props {:class "idea-page__card" :aria-labelledby "idea-proof-h"})
            (dom/h2 (dom/props {:id "idea-proof-h"}) (dom/text "The answer that showed it"))
            (if proof
              (do (dom/blockquote (dom/props {:class "idea-page__proof" :dir "auto"})
                    (dom/text (:answer proof)))
                  (IdeaAction user-id navigate! [:open-chat entity_id] "Open the chat"
                    "btn btn-sm btn-secondary" false nil))
              (dom/p (dom/props {:class "idea-page__muted"})
                (dom/text "No answer is stored for this idea."))))
          (dom/section
            (dom/props {:class "idea-page__card" :aria-labelledby "idea-history-h"})
            (dom/h2 (dom/props {:id "idea-history-h"}) (dom/text "How you got here"))
            (dom/ol
              (dom/props {:class "idea-page__events"})
              (e/for [[_ {:keys [date text]}] (e/diff-by first (map-indexed vector events))]
                (dom/li
                  (dom/span (dom/props {:class "idea-page__event-date"}) (dom/text date))
                  (dom/span (dom/text text))))))
          (IdeaCards user-id navigate! idea))
        (dom/aside
          (dom/props {:class "idea-page__side"})
          (dom/section
            (dom/props {:class "idea-page__card idea-page__actions" :aria-label "Actions"})
            (when self?
              (IdeaAction user-id navigate! [:undo entity_id] "Undo my mark"
                "btn btn-sm btn-secondary" false nil))
            (IdeaAction user-id navigate! [:unlearn entity_id] "Not learned yet"
              "btn btn-sm btn-danger" false nil)
            (dom/p (dom/props {:class "idea-page__muted"})
              (dom/text "Not learned yet puts the idea back on every map that has it, and the tutor teaches it again. Its cards stay.")))
          (IdeaChecks user-id navigate! idea)
          (dom/section
            (dom/props {:class "idea-page__card" :aria-labelledby "idea-where-h"})
            (dom/h2 (dom/props {:id "idea-where-h"}) (dom/text "Where it appears"))
            (dom/ul
              (dom/props {:class "idea-page__where"})
              (e/for [root (e/diff-by :id roots)]
                (dom/li
                  (if-let [file-id (:file_topic_id root)]
                    (dom/button
                      (dom/props {:type "button" :class "known-ideas__idea"})
                      (dom/text (str (:repo_path root) " (" (:repo_title root) ")"))
                      (dom/On "click" (fn [_] (open-file-on-map! navigate! file-id)) nil))
                    (dom/span (dom/text (str (:repo_path root) " (" (:repo_title root) ")"))))
                  (dom/span (dom/props {:class "idea-page__muted"})
                    (dom/text (str " " (root-text root)))))))
            (when (empty? roots)
              (dom/p (dom/props {:class "idea-page__muted"})
                (dom/text (str "Not on any question map."
                            (when-let [repos (seq (keep :repo_title marks))]
                              (str " Marked while reading " (str/join ", " (distinct repos)) "."))))))))))))

(e/defn IdeaPage
  "One known idea (§5). An entity the user does not own reads Not found."
  [user-id entity-id navigate!]
  (e/client
    (let [idea (e/server
                 (let [rev (+ (e/watch (us/get-atom user-id :question-mutations))
                             (e/watch (us/get-atom user-id :card-mutations))
                             (e/watch (us/get-atom user-id :kg-mutations)))]
                   (e/Offload #(idea-page* rev user-id entity-id))))]
      (dom/div
        (dom/props {:class "idea-page"})
        (dom/nav
          (dom/props {:class "idea-page__crumbs" :aria-label "Breadcrumb"})
          (dom/a
            (dom/props {:href "#"})
            (dom/text "Known ideas")
            (dom/On "click" (fn [e] (.preventDefault e) (navigate! :knowledge {:type :knowledge-known}))
              nil)))
        (if idea
          (IdeaBody user-id idea navigate!)
          (dom/p (dom/props {:class "idea-page__muted"}) (dom/text "Not found.")))))))
