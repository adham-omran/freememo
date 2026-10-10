(ns freememo.assistant-chat-history
  "The assistant panel's chat bar and chat history: the bar names the active
   chat, and the history view over the panel lists, finds, renames, pins and
   deletes the document's chats (plans/assistant-chat-history.md §3).

   Its own namespace, entered through one call from AssistantPanel, because that
   component sits at the 64KB method limit; see freememo.assistant-panel-rows.

   State ownership. `!active`, `!history-open?`, `!renaming-active?` and
   `!chat-action` are created by freememo.right-side-panel/RightSidePanel, which
   outlives a panel collapse and a poem tab switch, and they arrive here as
   positional parameters — never inside a map, because an atom cannot be
   serialized. Every server write goes through `!chat-action`, whose effect
   (`ChatActionEffect`) RightSidePanel mounts, so closing this view or the
   panel never tears down a write in flight."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.assistant-chat-lineage :as lineage]
   [freememo.assistant-chat-tree :as tree]
   [freememo.assistant-composer :as composer]
   [freememo.command-bus :as bus]
   [freememo.icons :as icons]
   [freememo.interaction-trail :as trail]
   [freememo.modal-shell :as modal-shell]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])))

;; ── Pure helpers ───────────────────────────────────────────────────────────

(defn current-time-ms
  "The client clock, read once per history open."
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn- local-day-start-ms
  "Epoch ms of local midnight `days-back` days before the day of `now-ms`.
   Calendar arithmetic, not a multiple of 24 h, so a DST change cannot move a
   chat across a boundary."
  [now-ms days-back]
  #?(:clj (let [zone (java.time.ZoneId/systemDefault)]
            (-> (java.time.Instant/ofEpochMilli now-ms)
                (.atZone zone) (.toLocalDate) (.minusDays days-back)
                (.atStartOfDay zone) (.toInstant) (.toEpochMilli)))
     :cljs (let [d (js/Date. now-ms)]
             (.setHours d 0 0 0 0)
             (.setDate d (- (.getDate d) days-back))
             (.getTime d))))

(defn- title-matches?
  "Case-insensitive substring match of `query` against the chat's title.
   A blank query matches every chat."
  [query chat]
  (or (str/blank? query)
      (str/includes? (str/lower-case (or (:label chat) ""))
                     (str/lower-case (str/trim query)))))

(defn history-sections
  "Split `chats` into the four history sections, in display order, each a
   tree (plans/assistant-chat-forks.md §4.6.1).

   Pre: each chat carries :id, :label, :activity-ms and :pinned-ms (nil when
   unpinned), all epoch ms, and :parent-id (nil for a root); `now-ms` is the
   client clock.
   Post: {:pinned :today :week :older}, each a vector of rows with :depth.
   The sections place top-level chats: a pinned one is in :pinned only, most
   recently pinned first, and the rest keep their incoming order (newest
   activity first) and are bucketed by local calendar day. Each top-level
   chat is followed by its descendants, depth first, in fork order, so a fork
   always sits in its root's section and a pin on a fork marks it in place.
   Only chats whose title matches `query`, and their ancestors, appear
   (§4.6.4), so a matched fork still shows the path to it."
  [chats query now-ms]
  (let [today-start (local-day-start-ms now-ms 0)
        week-start (local-day-start-ms now-ms 7)
        by-id (into {} (map (juxt :id identity)) chats)
        shown-ids (into #{}
                    (comp (filter #(title-matches? query %))
                      (mapcat #(cons (:id %) (map :id (tree/lineage by-id (:id %))))))
                    chats)
        shown (filterv #(shown-ids (:id %)) chats)
        top-level (remove #(shown-ids (:parent-id %)) shown)
        children (tree/children-index shown)
        expand #(tree/with-descendants % children)
        unpinned (remove :pinned-ms top-level)]
    {:pinned (expand (sort-by :pinned-ms > (filter :pinned-ms top-level)))
     :today (expand (filter #(>= (:activity-ms %) today-start) unpinned))
     :week (expand (filter #(and (< (:activity-ms %) today-start)
                              (>= (:activity-ms %) week-start)) unpinned))
     :older (expand (filter #(< (:activity-ms %) week-start) unpinned))}))

(def ^:private section-labels
  [[:pinned "Pinned"] [:today "Today"] [:week "Previous 7 days"] [:older "Older"]])

(defn relative-time-label
  "A compact age for a history row: now, 5m, 3h, 2d, 4w, 6mo, 2y."
  [now-ms ms]
  (let [minutes (quot (max 0 (- now-ms ms)) 60000)
        hours (quot minutes 60)
        days (quot hours 24)]
    (cond
      (< minutes 1) "now"
      (< hours 1) (str minutes "m")
      (< days 1) (str hours "h")
      (< days 7) (str days "d")
      (< days 30) (str (quot days 7) "w")
      (< days 365) (str (quot days 30) "mo")
      :else (str (quot days 365) "y"))))

(defn chat-action
  "One request for ChatActionEffect. The nonce makes every request a distinct
   value, so repeating an action still opens a new token.
   Pre: op ∈ #{:open :rename :pin :delete :exclude}."
  [op chat-id & {:as more}]
  (merge {:op op :chat-id chat-id :nonce (str (random-uuid))} more))

(defn request-chat-action!
  "Queue one chat action. Returns nil."
  [!chat-action op chat-id & more]
  (reset! !chat-action (apply chat-action op chat-id more))
  nil)

(defn clear-active-chat!
  "Return the panel to \"no chat yet\": no active chat, history closed, no
   rename. The next message creates a chat."
  [!active !history-open? !renaming-active?]
  (reset! !active nil)
  (reset! !history-open? false)
  (reset! !renaming-active? false)
  nil)

(defn apply-last-chat-seed!
  "Open the seeded chat unless the user already chose one (§4.2.1).
   Post: `!active` is unchanged when it held a chat; `!seeded?` is true."
  [!active !seeded? seed-chat-id]
  (compare-and-set! !active nil seed-chat-id)
  (reset! !seeded? true)
  nil)

(defn drop-dead-active-chat!
  "Clear the active chat when the server says it no longer exists (§3.7.1).

   Pre: `liveness` is nil or [chat-id live?] from assistant/chat-liveness*.
   Post: `!active` becomes nil only on an explicit false for the chat it still
   holds. A pending read, a nil answer, or an answer about a chat the user has
   already left changes nothing."
  [!active !renaming-active? liveness]
  (when-let [[chat-id live?] liveness]
    (when (and (false? live?) (compare-and-set! !active chat-id nil))
      (reset! !renaming-active? false)))
  nil)

(defn- last-opened-chat-id*
  [user-id root-topic-id]
  #?(:clj (assistant/last-opened-chat-id* user-id root-topic-id)
     :cljs nil))

(defn- chat-liveness*
  [rev user-id chat-id]
  #?(:clj (assistant/chat-liveness* rev user-id chat-id)
     :cljs nil))

(defn- chat-branch-state*
  [rev user-id chat-id]
  #?(:clj (assistant/chat-branch-state* rev user-id chat-id)
     :cljs nil))

(defn- apply-chat-action!*
  "Run one queued chat action on the server."
  [user-id action]
  #?(:clj (assistant/apply-chat-history-action! user-id action)
     :cljs nil))

;; ── DOM helpers (client) ───────────────────────────────────────────────────

(def ^:private row-class "assistant-history__row")

#?(:cljs
   (defn- row-chat-id
     "The chat id a history row element carries, or nil for any other element."
     [el]
     (when (and el (.-classList el) (.contains (.-classList el) row-class))
       (js/parseInt (.getAttribute el "data-chat-id") 10))))

#?(:cljs
   (defn- history-rows [container]
     (array-seq (.querySelectorAll container (str "." row-class)))))

#?(:cljs
   (defn- focus-row-at!
     "Focus the row `step` positions from `el` in document order, if any."
     [container el step]
     (let [rows (vec (history-rows container))
           i (.indexOf (to-array rows) el)
           target (get rows (+ i step))]
       (when target (.focus target)))))

(defn focus-history-row!
  "Focus the row of `chat-id` inside `container`. Used when the row menu closes."
  [container chat-id]
  #?(:clj nil
     :cljs (when-let [row (and container
                               (.querySelector container
                                 (str "." row-class "[data-chat-id=\"" chat-id "\"]")))]
             (.focus row))))

(defn focus-past-history-row!
  "Move focus off the row of `chat-id` before that row goes away: to the next
   row, else the previous one, else the search input.
   Without this a delete leaves focus on <body>, outside the view, where
   Escape and the list keys no longer reach it."
  [container chat-id]
  #?(:clj nil
     :cljs (when container
             (let [rows (vec (history-rows container))
                   i (first (keep-indexed
                              (fn [i row] (when (= (str chat-id) (.getAttribute row "data-chat-id")) i))
                              rows))
                   target (or (and i (get rows (inc i)))
                              (and i (pos? i) (get rows (dec i)))
                              (.querySelector container ".assistant-history__search"))]
               (some-> target (.focus))))))

(defn focus-first-history-row!
  "Move focus from the search input to the first listed row (§3.6.3)."
  [container]
  #?(:clj nil
     :cljs (some-> (first (history-rows container)) (.focus))))

(defn history-list-keydown!
  "Keyboard for the history list (§3.6).

   Pre: `handlers` holds plain fns :open! :rename! :delete!, each taking a chat id.
   Post: a key acts only when the event target IS a row element. A key whose
   target is the search input or a rename input reaches no handler, so typing
   there never opens, renames or deletes a chat."
  [container event handlers]
  #?(:clj nil
     :cljs (when-let [chat-id (row-chat-id (.-target event))]
             (case (.-key event)
               "ArrowDown" (do (.preventDefault event) (focus-row-at! container (.-target event) 1))
               "ArrowUp" (do (.preventDefault event) (focus-row-at! container (.-target event) -1))
               "Enter" (do (.preventDefault event) ((:open! handlers) chat-id))
               "F2" (do (.preventDefault event) ((:rename! handlers) chat-id))
               ("Delete" "Backspace") (do (.preventDefault event)
                                          (focus-past-history-row! container chat-id)
                                          ((:delete! handlers) chat-id))
               nil))))

(defn menu-placement
  "Absolute position of the row menu inside the history view.

   `anchor` is {:x :y} in viewport coordinates: the ⋯ button's bottom-right
   corner, or the pointer for a right-click. The menu opens below the anchor,
   and above it when it would run past the view's bottom edge.
   Post: a style map with :left and :top in px, inside the view."
  [view anchor]
  #?(:clj nil
     :cljs (let [rect (.getBoundingClientRect view)
                 menu-w 168
                 menu-h 150
                 x (- (:x anchor) (.-left rect))
                 y (- (:y anchor) (.-top rect))
                 left (max 4 (min (- x (if (:right-aligned? anchor) menu-w 0))
                                  (- (.-width rect) menu-w 4)))
                 top (if (> (+ y menu-h) (.-height rect))
                       (max 4 (- y menu-h (:anchor-h anchor 0)))
                       y)]
             {:left (str left "px") :top (str top "px")})))

(defn menu-anchor-from-button
  "The ⋯ button's bottom-right corner as a menu anchor."
  [button]
  #?(:clj nil
     :cljs (let [r (.getBoundingClientRect button)]
             {:x (.-right r) :y (.-bottom r) :anchor-h (.-height r)
              :right-aligned? true})))

(defn menu-anchor-from-pointer
  "The pointer position of a right-click as a menu anchor."
  [event]
  #?(:clj nil
     :cljs {:x (.-clientX event) :y (.-clientY event)}))

(defn title-editor-keydown!
  "Keys inside a chat-title input.

   Enter blurs, which fires `change` (the commit) when the value changed and
   `focusout` (the close) always. Escape marks the input cancelled, so the
   `change` a removal may fire is ignored, and closes. Every key listed in
   §3.4.2 stops here, so the list handler and the view's Escape never see it."
  [event close!]
  #?(:clj nil
     :cljs (let [k (.-key event)]
             (when (#{"Enter" "Escape" "Backspace" "Delete" "F2" "ArrowUp" "ArrowDown"} k)
               (.stopPropagation event))
             (case k
               "Enter" (do (.preventDefault event) (.blur (.-target event)))
               "Escape" (do (.preventDefault event)
                            (aset (.-target event) "__cancelled" true)
                            (close!))
               nil))))

(defn title-editor-change!
  "The commit half of a chat-title input: hand a changed, non-blank title on.
   A cancelled input or a blank value commits nothing."
  [event original commit!]
  #?(:clj nil
     :cljs (let [input (.-target event)
                 value (str/trim (or (.-value input) ""))]
             (when (and (instance? js/HTMLInputElement input)
                        (not (aget input "__cancelled"))
                        (not (str/blank? value))
                        (not= value original))
               (commit! value)))))

(defn select-all-on-focus!
  "Select an input's whole value when it takes focus, so typing replaces it."
  [event]
  #?(:clj nil
     :cljs (let [n (.-target event)]
             (when (instance? js/HTMLInputElement n) (.select n)))))

;; ── Components ─────────────────────────────────────────────────────────────

(e/defn ChatActionEffect
  "Run each request written to `!chat-action` against the server.

   Pre: mounted where it outlives the controls that write `!chat-action` —
   RightSidePanel — so neither a closed view nor a collapsed panel cancels a
   write in flight.
   Post: one server call per request. The server reports a failure as a toast."
  [user-id !chat-action]
  (e/client
    (let [action (e/watch !chat-action)
          [t _] (e/Token action)]
      (when t
        (let [result (e/server (e/Offload #(apply-chat-action!* user-id action)))]
          (case result (t)))))))

(e/defn ChatTitleEditor
  "The rename input, shared by the bar title and a history row (§3.4).

   Pre: `commit!` takes a trimmed, changed, non-blank title; `close!` takes no
   argument. Both are plain client fns.
   Post: Enter or blur with a changed title calls `commit!` then `close!`.
   Escape, a blank value or an unchanged one calls `close!` only."
  [title commit! close!]
  (e/client
    (dom/span
      (dom/props {:class "assistant-chat-title-editor"})
      (dom/On "focusin" select-all-on-focus! nil)
      (dom/On "keydown" (fn [event] (title-editor-keydown! event close!)) nil)
      (dom/On "change" (fn [event] (title-editor-change! event title commit!)) nil)
      (dom/On "focusout" (fn [_] (close!)) nil)
      (dom/On "click" (fn [event] (.stopPropagation event)) nil)
      ;; The per-keystroke token stays unspent, as in knowledge_tree's rename:
      ;; the commit is the delegated `change` above, once per edit.
      (forms/Input! :title (or title "")
        :type "text" :maxlength "200" :autofocus true
        :aria-label "Chat title"
        :class "input assistant-chat-title-editor__input"))))

(e/defn HistoryRowMenu
  "Rename, Pin or Unpin, and Delete for one row (§3.5).

   Post: Escape closes the menu only and returns focus to the row. Each item
   closes the menu after queuing its effect."
  [menu view-node !menu !editing-id !chat-action]
  (e/client
    (let [{:keys [chat-id pinned? fork? excluded?]} menu
          close! (fn [] (reset! !menu nil) (focus-history-row! view-node chat-id))]
      (dom/div
        (dom/props {:class "assistant-history__menu" :role "menu"
                    :style (:style menu)})
        (dom/On "keydown"
          (fn [event]
            (when (= "Escape" (.-key event))
              (.preventDefault event)
              (.stopPropagation event)
              (close!)))
          nil)
        (dom/button
          (dom/props {:type "button" :role "menuitem"
                      :class "assistant-history__menu-item"})
          (e/snapshot (modal-shell/focus-on-mount! dom/node))
          (dom/text "Rename")
          (dom/On "click" (fn [_] (reset! !menu nil) (reset! !editing-id chat-id)) nil))
        (dom/button
          (dom/props {:type "button" :role "menuitem"
                      :class "assistant-history__menu-item"})
          (dom/text (if pinned? "Unpin" "Pin"))
          (dom/On "click"
            (fn [_]
              (request-chat-action! !chat-action :pin chat-id :pinned? (not pinned?))
              (close!))
            nil))
        ;; Always mounted and hidden for a root, so the items never shift
        ;; under a menu that is open (plans/assistant-chat-forks.md §4.6.3).
        (dom/button
          (dom/props {:type "button" :role "menuitem" :hidden (not fork?)
                      :class "assistant-history__menu-item"})
          (dom/text (if excluded? "Include in parent" "Exclude from parent"))
          (dom/On "click"
            (fn [_]
              (request-chat-action! !chat-action :exclude chat-id :excluded? (not excluded?))
              (close!))
            nil))
        (dom/button
          (dom/props {:type "button" :role "menuitem"
                      :class "assistant-history__menu-item assistant-history__menu-item--danger"})
          (dom/text "Delete")
          (dom/On "click"
            (fn [_]
              (focus-past-history-row! view-node chat-id)
              (request-chat-action! !chat-action :delete chat-id)
              (reset! !menu nil))
            nil))))))

(e/defn HistoryRow
  "One chat in the history list: title (or its rename input), pin mark,
   relative time and the ⋯ button. The row element itself is focusable and
   carries `data-chat-id`, which is all the list's key handler reads."
  [chat active now-ms editing-id view-node open! !menu !editing-id !chat-action]
  (e/client
    (let [chat-id (:id chat)
          active? (= active chat-id)
          pinned? (some? (:pinned-ms chat))
          fork? (some? (:parent-id chat))
          excluded? (:excluded? chat)
          menu-state (fn [style] {:chat-id chat-id :pinned? pinned? :fork? fork?
                                  :excluded? excluded? :style style})]
      (dom/div
        (dom/props {:class (str row-class (when active? " assistant-history__row--active")
                             (when excluded? " assistant-history__row--excluded"))
                    :tabindex "0" :role "button"
                    :data-chat-id (str chat-id)
                    :aria-current (when active? "true")
                    :style {:--depth (:depth chat 0)}})
        (if (= editing-id chat-id)
          (ChatTitleEditor (:label chat)
            (fn [title] (request-chat-action! !chat-action :rename chat-id :title title))
            (fn [] (reset! !editing-id nil) (focus-history-row! view-node chat-id)))
          (dom/span
            (dom/props {:class "assistant-history__title"})
            (dom/text (:label chat))))
        (when excluded?
          (dom/span
            (dom/props {:class "assistant-history__excluded"
                        :data-tooltip "Its parent chat does not read it"})
            (dom/text "excluded")))
        (when pinned?
          (dom/span
            (dom/props {:class "assistant-history__pin" :aria-label "Pinned"})
            (icons/Icon :pin :size 12)))
        (dom/span
          (dom/props {:class "assistant-history__time"})
          (dom/text (relative-time-label now-ms (:activity-ms chat))))
        (dom/button
          (dom/props {:type "button" :class "assistant-history__more"
                      :aria-label "Chat actions" :aria-haspopup "menu"})
          (dom/text "⋯")
          (dom/On "click"
            (fn [event]
              (.stopPropagation event)
              (reset! !menu (menu-state (menu-placement view-node
                                          (menu-anchor-from-button (.-currentTarget event))))))
            nil))
        (dom/On "click"
          (fn [event]
            (when-not (.closest (.-target event) "button, input")
              (open! chat-id)))
          nil)
        (dom/On "contextmenu"
          (fn [event]
            (.preventDefault event)
            (reset! !menu (menu-state (menu-placement view-node
                                        (menu-anchor-from-pointer event)))))
          nil)))))

(e/defn HistorySection
  "One date section. The section and its row element stay mounted however many
   rows it holds; an empty section is hidden by a class (§3.3.4)."
  [section-key label chats active now-ms editing-id view-node open!
   !menu !editing-id !chat-action]
  (e/client
    (dom/section
      (dom/props {:class (str "assistant-history__section"
                           (when (empty? chats) " assistant-history__section--empty"))
                  :data-section (name section-key)})
      (dom/div
        (dom/props {:class "assistant-history__section-label"})
        (dom/text label))
      (dom/div
        (dom/props {:class "assistant-history__rows"})
        (e/for [chat (e/diff-by :id chats)]
          (HistoryRow chat active now-ms editing-id view-node open!
            !menu !editing-id !chat-action))))))

(e/defn HistoryList
  "Search bar, sections and empty states. Mounted only while the view is open,
   so each open starts with an empty query and a fresh clock.
   Pre: called directly inside the history view's container, which is
   `dom/node` here."
  [chat-options active new-chat! !active !history-open? !error !chat-action]
  (e/client
    (let [view-node dom/node
          now-ms (e/snapshot (current-time-ms))
          !query (atom "")
          query (e/watch !query)
          !menu (atom nil)
          menu (e/watch !menu)
          !editing-id (atom nil)
          editing-id (e/watch !editing-id)
          sections (history-sections chat-options query now-ms)
          listed? (boolean (some seq (vals sections)))
          open! (fn [chat-id]
                  (trail/record! :assistant {:action :select-chat :chat-id chat-id})
                  (reset! !active chat-id)
                  (reset! !error nil)
                  (request-chat-action! !chat-action :open chat-id)
                  (reset! !history-open? false))
          handlers {:open! open!
                    :rename! (fn [chat-id] (reset! !editing-id chat-id))
                    :delete! (fn [chat-id] (request-chat-action! !chat-action :delete chat-id))}]
      ;; A press anywhere outside the open menu closes it.
      (dom/On "mousedown"
        (fn [event]
          (when (and @!menu (not (.closest (.-target event) ".assistant-history__menu")))
            (reset! !menu nil)))
        nil)
      (dom/div
        (dom/props {:class "assistant-panel__bar assistant-history__bar"})
        (dom/button
          (dom/props {:type "button" :class "btn btn-secondary assistant-history__back"
                      :aria-label "Back to chat" :data-tooltip "Back to chat"})
          (icons/Icon :arrow-left :size 14)
          (dom/On "click" (fn [_] (reset! !history-open? false)) nil))
        (dom/input
          (dom/props {:type "search" :class "input assistant-history__search"
                      :placeholder "Search chats…" :aria-label "Search chats"})
          (e/snapshot (modal-shell/focus-on-mount! dom/node))
          (dom/On "input" (fn [event] (reset! !query (.. event -target -value))) nil)
          (dom/On "keydown"
            (fn [event]
              (when (= "ArrowDown" (.-key event))
                (.preventDefault event)
                (focus-first-history-row! view-node)))
            nil))
        (dom/button
          (dom/props {:class "btn btn-secondary assistant-panel__new" :type "button"
                      :data-tooltip "Start a new chat about this page"})
          (dom/text "＋ New")
          (dom/On "click" (fn [_] (new-chat!)) nil)))
      (dom/div
        (dom/props {:class "assistant-history__list"})
        (dom/On "keydown" (fn [event] (history-list-keydown! view-node event handlers)) nil)
        (e/for [[section-key label] (e/diff-by first section-labels)]
          (HistorySection section-key label (get sections section-key) active now-ms
            editing-id view-node open! !menu !editing-id !chat-action))
        ;; Always mounted and hidden by a class, the tape_table shape: the
        ;; sections above never swap with it.
        (dom/div
          (dom/props {:class (str "assistant-history__empty"
                               (when listed? " assistant-history__empty--hidden"))})
          (dom/text (if (seq chat-options)
                      "No chats match."
                      "Chats about this document appear here."))))
      (when menu
        (HistoryRowMenu menu view-node !menu !editing-id !chat-action)))))

(e/defn ChatHistoryView
  "The history view over the assistant panel (§3.2).

   Its container stays mounted and a class shows it, so opening and closing it
   never remounts the transcript beside it. Escape closes it (§3.3.10); the
   rename input and the row menu stop their own Escape first."
  [chat-options active history-open? new-chat! !active !history-open? !error
   !chat-action]
  (e/client
    (dom/div
      (dom/props {:class (str "assistant-history"
                           (when history-open? " assistant-history--open"))
                  :aria-hidden (str (not history-open?))})
      (dom/On "keydown"
        (fn [event]
          (when (= "Escape" (.-key event))
            (.preventDefault event)
            (reset! !history-open? false)))
        nil)
      (when history-open?
        (HistoryList chat-options active new-chat! !active !history-open? !error
          !chat-action)))))

(e/defn ChatBar
  "The chat bar: `[history] [active chat title] [＋ New]` (§3.1).

   The title is the active chat's, and selecting it starts a rename in place.
   With no active chat it reads \"New chat\" and is inert: the chat does not
   exist until the first message creates it.

   Pre: `active-title` is the active chat's title, or nil with no active chat;
   `new-chat!` is the plain client fn \"＋ New\" runs.
   Post: writes `!history-open?`, `!renaming-active?` and `!chat-action` only."
  [active active-title renaming-active? new-chat! !history-open? !renaming-active?
   !chat-action]
  (e/client
    (dom/div
      (dom/props {:class "assistant-panel__bar"})
      (dom/button
        (dom/props {:type "button" :class "btn btn-secondary assistant-panel__history"
                    :aria-label "Chat history" :data-tooltip "Chat history"})
        (icons/Icon :history :size 14)
        (dom/On "click" (fn [_] (reset! !history-open? true)) nil))
      (if (and active renaming-active?)
        (ChatTitleEditor active-title
          (fn [title] (request-chat-action! !chat-action :rename active :title title))
          (fn [] (reset! !renaming-active? false)))
        (dom/button
          (dom/props {:type "button"
                      :class (str "assistant-panel__title"
                               (when-not active " assistant-panel__title--new"))
                      :disabled (nil? active)
                      :data-tooltip (when active "Rename this chat")})
          (dom/text (if active (or active-title "Untitled") "New chat"))
          (dom/On "click" (fn [_] (when active (reset! !renaming-active? true))) nil)))
      (dom/button
        (dom/props {:class "btn btn-secondary assistant-panel__new"
                    :type "button" :data-tooltip "Start a new chat about this page"})
        (dom/text "＋ New")
        (dom/On "click" (fn [_] (new-chat!)) nil)))))

(e/defn ChatBarWithHistory
  "The bar and the history view, as two siblings in the assistant panel.

   Pre: `!draft` and `!error` are AssistantPanel's; the other atoms are
   RightSidePanel's.
   Post: \"＋ New\" clears only `!active`, `!draft` and `!error` and writes no
   row (§4.1). While mounted, the composer half of that is published as
   :reset-assistant-composer!, which the palette's New assistant chat runs."
  [root-topic-id chat-options active active-title
   !active !history-open? !renaming-active? !chat-action !draft !error]
  (e/client
    (let [history-open? (e/watch !history-open?)
          renaming-active? (e/watch !renaming-active?)
          reset-composer! (fn []
                            (composer/replace-composer-draft! !draft "")
                            (reset! !error nil))
          new-chat! (fn []
                      (trail/record! :assistant
                        {:action :new-chat :topic-id root-topic-id})
                      (clear-active-chat! !active !history-open? !renaming-active?)
                      (reset-composer!))]
      (bus/publish-ctx! {:reset-assistant-composer! reset-composer!})
      (e/on-unmount #(bus/retract-ctx! [:reset-assistant-composer!]))
      (ChatBar active active-title renaming-active? new-chat!
        !history-open? !renaming-active? !chat-action)
      (lineage/ChatLineage chat-options active
        (fn [chat-id]
          (trail/record! :assistant {:action :select-chat :chat-id chat-id})
          (reset! !active chat-id)
          (reset! !error nil)
          (request-chat-action! !chat-action :open chat-id))
        (fn [chat-id excluded?]
          (request-chat-action! !chat-action :exclude chat-id :excluded? excluded?)))
      ;; Over the bar and the transcript while open; always mounted, so the
      ;; transcript beside it never remounts (§3.2).
      (ChatHistoryView chat-options active history-open? new-chat!
        !active !history-open? !error !chat-action))))

(e/defn AssistantChatEffects
  "Everything that writes the assistant chat state from outside the panel.
   RightSidePanel mounts it beside the atoms, so it runs with the panel
   collapsed and while الشرح shows.

   - The last-chat seed (§4.2): one read per mount, applied only to an empty
     `!active`. `!seeded?` turns true when it answers, and the transcript shows
     no starters before then, so a reload never flashes them.
   - The liveness rule (§3.7.1): a delete here or in another tab clears the
     active chat on the next :assistant-mutations bump.
   - The palette's chat commands (§5.1), published with :active-chat-id, and
     the fork commands (plans/assistant-chat-forks.md §4.7.1), published with
     :active-chat-branch. Each opens the assistant tab first where it shows
     UI; `show-right-pane!` is RightSidePanel's.
   - ChatActionEffect, which runs every chat write."
  [user-id root-topic-id show-right-pane!
   !active !history-open? !renaming-active? !chat-action !seeded? !pending-fork]
  (e/client
    (let [active (e/watch !active)
          seed-chat-id (e/server (e/snapshot (last-opened-chat-id* user-id root-topic-id)))
          assistant-rev (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          liveness (e/server (chat-liveness* assistant-rev user-id active))
          branch-state (e/server (chat-branch-state* assistant-rev user-id active))]
      (bus/publish-ctx!
        {:show-assistant-history!
         (fn []
           (show-right-pane! "assistant")
           (reset! !renaming-active? false)
           (reset! !history-open? true))
         :new-assistant-chat!
         (fn []
           (show-right-pane! "assistant")
           (clear-active-chat! !active !history-open? !renaming-active?))
         :rename-active-assistant-chat!
         (fn []
           (when @!active
             (show-right-pane! "assistant")
             (reset! !history-open? false)
             (reset! !renaming-active? true)))
         :delete-assistant-chat!
         (fn [chat-id] (request-chat-action! !chat-action :delete chat-id))
         ;; plans/assistant-chat-forks.md §4.7.1. Fork chat stages a fork at
         ;; the active chat's latest message, and the next send creates it.
         :fork-active-assistant-chat!
         (fn []
           (when-let [chat-id @!active]
             (show-right-pane! "assistant")
             (reset! !history-open? false)
             (reset! !pending-fork {:chat-id chat-id :message-id nil})))
         :set-assistant-chat-excluded!
         (fn [chat-id excluded?]
           (request-chat-action! !chat-action :exclude chat-id :excluded? excluded?))})
      (bus/publish-ctx! {:active-chat-id active})
      ;; [chat-id excluded?] while the active chat is a fork, else nil. The
      ;; palette's Exclude and Include read it to decide which one applies.
      (bus/publish-ctx! {:active-chat-branch branch-state})
      (e/on-unmount #(bus/retract-ctx! [:show-assistant-history! :new-assistant-chat!
                                        :rename-active-assistant-chat!
                                        :delete-assistant-chat! :active-chat-id
                                        :fork-active-assistant-chat!
                                        :set-assistant-chat-excluded!
                                        :active-chat-branch]))
      (apply-last-chat-seed! !active !seeded? seed-chat-id)
      (drop-dead-active-chat! !active !renaming-active? liveness)
      (ChatActionEffect user-id !chat-action))))
