(ns freememo.question-map-panel
  "The assistant panel's side of question pins (plans/anchored-questions.md
   §6.2.5, §6.5, plans/question-map-tab.md): the Chat and Map tabs, the map
   view, the Ask-N-pins row above the composer, and the effect that turns an
   Ask into a submit.

   Its own namespace for the 64KB method cap, as `assistant-panel` splits its
   other parts."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.assistant-composer :as composer]
   [freememo.command-bus :as bus]
   [freememo.question-pin-pane :as pin-pane]
   [freememo.question-map-view :as qmv]
   [freememo.question-pin-state :as pin-state]
   #?(:clj [freememo.document-ref :as docref])
   #?(:clj [freememo.question-map :as qm])
   #?(:clj [freememo.user-state :as us])))

(defn map-card* [_rev user-id spec]
  #?(:clj (qm/map-card user-id (docref/resolve-spec user-id spec)) :cljs nil))

(defn set-root-known!* [user-id root-id known? spec]
  #?(:clj (qm/set-root-known! user-id root-id known? (docref/resolve-spec user-id spec))
     :cljs nil))

(defn teach-root!* [user-id root-id]
  #?(:clj (qm/teach-root! user-id root-id) :cljs nil))

(defn- pin-list
  "`labels` as `pin 3` or `pins 3, 4`, or nil when there are none."
  [labels]
  (when (seq labels)
    (str (if (= 1 (count labels)) "pin " "pins ") (str/join ", " labels))))

(defn- state-text
  "The one line of text under a root's label (plans/question-map-tab.md
   §4.3.3)."
  [{:keys [state known_sources done-label pin-labels]}]
  (case state
    "learned" (if done-label (str "Learned on " done-label) "Learned")
    "known" (if (some #{"self"} known_sources) "You knew it" "Learned on another map")
    "next" (str "Learning now" (some->> (pin-list pin-labels) (str ", answers ")))
    (if-let [pins (pin-list pin-labels)] (str "Answers " pins) "Open")))

(defn- focus-composer!
  "Focus the composer once the Chat view is shown. The view's class changes
   in a later frame, and a hidden textarea takes no focus."
  []
  #?(:cljs (js/requestAnimationFrame
             #(some-> (.querySelector js/document ".assistant-panel__input") .focus))
     :clj nil))

(defn- continue-in-chat!
  "Show Chat and focus the composer (§4.4.4). Post: nil."
  []
  (pin-state/show-view! :chat)
  (focus-composer!)
  nil)

(defn- show-chat-with-draft!
  "Show Chat and put `text` in the composer when the draft is empty
   (§4.4.2). It sends nothing. Post: nil."
  [!draft text]
  (pin-state/show-view! :chat)
  (when (str/blank? @!draft)
    (composer/replace-composer-draft! !draft text))
  (focus-composer!)
  nil)

(defn- move-between-tabs!
  "Arrow keys, Home and End move between the two tabs of `e`'s tablist and
   show the tab they reach (§4.1.3)."
  [e]
  #?(:cljs
     (when (#{"ArrowLeft" "ArrowRight" "Home" "End"} (.-key e))
       (.preventDefault e)
       (let [tab (case (.-key e)
                   "Home" :chat
                   "End" :map
                   (if (= :chat @pin-state/!panel-view) :map :chat))]
         (pin-state/show-view! tab)
         (some-> (.querySelector (.-currentTarget e) (str "#assistant-tab-" (name tab)))
           .focus)))
     :clj nil))

(e/defn ViewTab
  "One tab of the panel's tablist. Its props read only the client atom, so a
   pending server read never strips them (§4.2.3)."
  [tab view label]
  (e/client
    (let [selected? (= tab view)]
      (dom/button
        (dom/props {:type "button" :role "tab"
                    :id (str "assistant-tab-" (name tab))
                    :aria-controls (if (= :map tab) "question-map-view" "assistant-transcript")
                    :aria-selected (str selected?)
                    :tabindex (if selected? "0" "-1")
                    :class (str "assistant-view-tab"
                             (when selected? " assistant-view-tab--selected"))})
        (dom/text label)
        (dom/On "click" (fn [_] (pin-state/show-view! tab)) nil)))))

(e/defn ViewTabs
  "Chat and `Map 2 of 4`, shown only while the file has a map (§4.1)."
  [roots view]
  (e/client
    (when (seq roots)
      (dom/div
        (dom/props {:class "assistant-view-tabs" :role "tablist"
                    :aria-label "Assistant views"})
        (dom/On "keydown" move-between-tabs! nil)
        (ViewTab :chat view "Chat")
        (ViewTab :map view (qmv/tab-label roots))))))

(e/defn RootCitation
  "One passage that explains a root, shown by its quote (§4.3.5). Pointing at
   it lights the passage up in the file, and a click scrolls there."
  [{:keys [quote anchor]} n]
  (e/client
    (dom/button
      (dom/props {:class "question-map-view__citation" :type "button" :dir "auto"
                  :aria-label (str "Passage " n ": " quote)})
      (dom/text (qmv/quote-label quote))
      (dom/On "mouseenter" (fn [_] (pin-state/point! {:quote quote :anchor anchor})) nil)
      (dom/On "mouseleave" (fn [_] (pin-state/point! nil)) nil)
      (dom/On "click" (fn [_] (pin-state/point! {:quote quote :anchor anchor :scroll? true}))
        nil))))

(e/defn ClientAction
  "A button that writes nothing: Teach again and Continue in chat (§4.4.3,
   §4.4.4). Post: no command."
  [label act!]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-secondary" :type "button"})
      (dom/text label)
      (dom/On "click" (fn [_] (act!)) nil))
    (e/amb)))

(e/defn RootActions
  "The actions of one root's state (§4.3.3). Post: its button commands."
  [{:keys [id label state known_sources]} !draft]
  (e/client
    (case state
      "learned" (ClientAction "Teach again"
                  #(show-chat-with-draft! !draft (str "Teach me " label " again.")))
      "known" (e/amb
                (forms/Button! [::teach id label]
                  :class "btn btn-sm btn-secondary" :label "Teach me")
                (if (some #{"self"} known_sources)
                  (forms/Button! [::known id false]
                    :class "btn btn-sm btn-secondary" :label "Undo")
                  (e/amb)))
      "next" (e/amb
               (ClientAction "Continue in chat" continue-in-chat!)
               (forms/Button! [::known id true]
                 :class "btn btn-sm btn-secondary" :label "I know this"))
      (e/amb
        (forms/Button! [::teach id label]
          :class "btn btn-sm btn-secondary" :label "Teach me now")
        (forms/Button! [::known id true]
          :class "btn btn-sm btn-secondary" :label "I know this")))))

(e/defn TrailRow
  "One root of the trail: a marker, its label, one line of state, its
   passages and its actions (§4.3.3). Post: its button commands."
  [root !draft]
  (e/client
    (dom/li
      (dom/props {:class (str "question-map-view__root question-map-view__root--"
                           (:state root))})
      (dom/span (dom/props {:class "question-map-view__marker" :aria-hidden "true"}))
      (dom/div
        (dom/props {:class "question-map-view__body"})
        (dom/div (dom/props {:class "question-map-view__label" :dir "auto"})
          (dom/text (:label root)))
        (dom/div (dom/props {:class "question-map-view__state"})
          (dom/text (state-text root))
          (when-let [closed (and (= "learned" (:state root))
                              (pin-list (:closed-pin-labels root)))]
            (dom/span (dom/props {:class "question-map-view__closed-pins"})
              (dom/text closed))))
        (when-not (:in_document root)
          (dom/div (dom/props {:class "question-map-view__outside"})
            (dom/text "Outside this file")))
        (dom/div
          (dom/props {:class "question-map-view__citations"})
          (e/for [[i c] (e/diff-by first (map-indexed vector
                                           (qmv/distinct-citations (:citations root))))]
            (RootCitation c (inc i))))
        (dom/div
          (dom/props {:class "question-map-view__actions"})
          (RootActions root !draft))))))

(e/defn UnplacedPins
  "The foot of the map view while the file has pins no Ask has placed
   (§4.3.6). Its button asks every open pin, as `AskPinsRow` does."
  [unplaced sending?]
  (e/client
    (let [open (or (:question-open-pins (e/watch bus/!ctx)) 0)]
      (when (pos? unplaced)
        (dom/div
          (dom/props {:class "question-map-view__unplaced"})
          (dom/text (str unplaced (if (= 1 unplaced) " new pin in the file is" " new pins in the file are")
                      " not on the map yet"))
          (dom/button
            (dom/props {:class "btn btn-sm btn-primary" :type "button"})
            (dom/text (str "Ask " open " pin" (when (not= 1 open) "s")))
            (dom/On "click" (fn [_] (when-not sending? (pin-pane/ask-open-pins!))) nil)))))))

(e/defn MapView
  "The map as a trail (§4.3). It stays mounted, and CSS hides it in the Chat
   view (§4.2.2). The row loop has an element of its own."
  [user-id spec card !draft sending?]
  (e/client
    (dom/div
      (dom/props {:class "question-map-view" :id "question-map-view" :role "tabpanel"
                  :aria-labelledby "assistant-tab-map"})
      (dom/ol
        (dom/props {:class "question-map-view__trail"})
        (let [commands (e/for [root (e/diff-by :id (:roots card))] (TrailRow root !draft))]
          (e/for [[t command] (e/diff-by first (e/as-vec commands))]
            (let [[kind root-id arg] command
                  result (e/server
                           (e/Offload
                             #(if (= ::known kind)
                                (set-root-known!* user-id root-id arg spec)
                                (teach-root!* user-id root-id))))]
              (case result
                (if (and (= ::teach kind) (:ok result))
                  (case (show-chat-with-draft! !draft (str "Teach me " arg "."))
                    (t))
                  (t)))))))
      (UnplacedPins (:unplaced card) sending?))))

(e/defn QuestionMapTabs
  "The Chat and Map tabs and the map view of the open file
   (plans/question-map-tab.md §4.1, §4.3). One read of the map feeds both
   (§4.3.1). The file is the one a mounted pin pane publishes, a walkthrough
   canvas included, else the page topic.
   Post: with no roots, the panel shows Chat once the read returns (§4.2.4)."
  [user-id page-topic-id !draft sending?]
  (e/client
    (let [spec (or (:question-pane-ref (e/watch bus/!ctx)) {:topic-id page-topic-id})
          rev (e/server (e/watch (us/get-atom user-id :question-mutations)))
          card (e/server (e/Offload #(map-card* rev user-id spec)))
          roots (:roots card)
          view (e/watch pin-state/!panel-view)]
      (dom/div
        (dom/props {:class "question-map-host"})
        (ViewTabs roots view)
        (MapView user-id spec card !draft sending?))
      ;; LAST, wrapping nothing (CLAUDE.md, "A `case` Remounts Its Branch").
      (when (and (empty? roots) (= :map view))
        (pin-state/show-view! :chat)))))

(e/defn AskPinsRow
  "\"Ask N pins\" above the composer while the rendered pane has open pins
   (§6.2.5). It reads the count the pane publishes, so it shows exactly when
   the command is available."
  [sending?]
  (e/client
    (let [open (or (:question-open-pins (e/watch bus/!ctx)) 0)]
      (dom/div
        (dom/props {:class "question-pin-ask-row"})
        (dom/props {:style {:display (if (pos? open) "flex" "none")}})
        (dom/button
          (dom/props {:class "btn btn-sm btn-primary" :type "button"
                      :aria-label "Ask the tutor about every open pin"})
          (dom/text (str "Ask " open " pin" (when (not= 1 open) "s")))
          (dom/On "click" (fn [_] (when-not sending? (pin-pane/ask-open-pins!))) nil))))))

(e/defn AskPinsEffect
  "Turn each :ask-question-pins invocation into a submit of the panel
   (§6.2.5.1). The submit carries no pin text: the server builds it."
  [!submit !active mode-id web-search?]
  (e/client
    (e/for [{:keys [invocation-id]} (bus/Requests :ask-question-pins)]
      (let [[t _] (e/Token invocation-id)]
        (when t
          ;; Chat first, so the reader sees the reply stream (§4.2.5).
          (case (pin-state/show-view! :chat)
            (case (reset! !submit {:id (str (random-uuid)) :text "" :question-pins? true
                                   :chat @!active :refs [] :mode mode-id
                                   :web-search? web-search?})
              (case (bus/consume! invocation-id)
                (t)))))))))

(e/defn WalkthroughAskEffect
  "Turn each :ask-walkthrough-tutor invocation into a submit of the panel
   with the invocation's text (plans/commit-range-walkthrough.md §6.7.1). The
   server reads the walkthrough's step from the screen's focus, so the text
   is an ordinary turn.
   Post: the turn starts a NEW chat, which the panel then shows. The opener
   tests the reader from nothing, so an earlier conversation must not be in
   its context."
  [!submit !active mode-id web-search?]
  (e/client
    (e/for [{:keys [invocation-id payload]} (bus/Requests :ask-walkthrough-tutor)]
      (let [[t _] (e/Token invocation-id)]
        (when t
          (case (reset! !active nil)
            (case (reset! !submit {:id (str (random-uuid)) :text (str (:text payload))
                                   :chat nil :refs [] :mode mode-id
                                   :web-search? web-search?})
              (case (bus/consume! invocation-id)
                (t)))))))))

(e/defn AskEffects
  "Both command-to-submit effects of the assistant panel, in one call."
  [!submit !active mode-id web-search?]
  (e/client
    (AskPinsEffect !submit !active mode-id web-search?)
    (WalkthroughAskEffect !submit !active mode-id web-search?)))
