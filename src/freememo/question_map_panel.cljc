(ns freememo.question-map-panel
  "The assistant panel's side of question pins (plans/anchored-questions.md
   §6.2.5, §6.3.7, §6.5): the question map card, the Ask-N-pins row above the
   composer, and the effect that turns an Ask into a submit.

   Its own namespace for the 64KB method cap, as `assistant-panel` splits its
   other parts."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.question-pin-pane :as pin-pane]
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

(defn- status-word [{:keys [status known_sources]}]
  (cond (= "next" status) "Start here"
        (= "learned" status) "Learned"
        (seq known_sources) "Known"
        :else nil))

(e/defn RootCitation
  "One passage that explains a root. Pointing at it lights the passage up in
   the file, and a click scrolls there (§6.5.1, §6.5.4)."
  [{:keys [quote anchor]} n]
  (e/client
    (dom/button
      (dom/props {:class "question-map__citation" :type "button"
                  :aria-label (str "Passage " n ": " quote)})
      (dom/text (str "§" (or anchor n)))
      (dom/On "mouseenter" (fn [_] (pin-state/point! {:quote quote :anchor anchor})) nil)
      (dom/On "mouseleave" (fn [_] (pin-state/point! nil)) nil)
      (dom/On "click" (fn [_] (pin-state/point! {:quote quote :anchor anchor :scroll? true}))
        nil))))

(e/defn RootRow
  "One root of the map. Post: its button commands."
  [root]
  (e/client
    (dom/li
      (dom/props {:class (str "question-map__root question-map__root--" (:status root))})
      (dom/div
        (dom/props {:class "question-map__root-head"})
        (dom/span (dom/props {:class "question-map__label" :dir "auto"})
          (dom/text (:label root)))
        (when-let [word (status-word root)]
          (dom/span (dom/props {:class "question-map__badge"}) (dom/text word))))
      (dom/div
        (dom/props {:class "question-map__meta"})
        (dom/text (str (if (:in_document root) "In this file" "Outside this file")
                    (when (seq (:pin-labels root))
                      (str " (pins " (str/join ", " (:pin-labels root)) ")")))))
      (dom/div
        (dom/props {:class "question-map__citations"})
        (e/for [[i c] (e/diff-by first (map-indexed vector (:citations root)))]
          (RootCitation c (inc i))))
      (dom/div
        (dom/props {:class "question-map__actions"})
        (e/amb
          (if (seq (:known_sources root))
            (forms/Button! [::known (:id root) false]
              :class "btn btn-sm btn-secondary" :label "Undo known")
            (forms/Button! [::known (:id root) true]
              :class "btn btn-sm btn-secondary" :label "I know it"))
          (forms/Button! [::teach (:id root)]
            :class "btn btn-sm btn-secondary" :label "Teach me"
            :disabled (= "next" (:status root))))))))

(e/defn QuestionMapCard
  "The map of the open file: each root, its pins and its passages, with I know
   it and Teach me (§6.3.7). Shows nothing while the file has no roots. The
   file is the one a mounted pin pane publishes, a walkthrough canvas
   included, else the page topic."
  [user-id page-topic-id]
  (e/client
    (let [spec (or (:question-pane-ref (e/watch bus/!ctx)) {:topic-id page-topic-id})
          rev (e/server (e/watch (us/get-atom user-id :question-mutations)))
          roots (e/server (e/Offload #(map-card* rev user-id spec)))]
      (dom/div
        (dom/props {:class "question-map-host"})
        (when (seq roots)
          (dom/element "details"
            (dom/props {:class "question-map" :open true})
            (dom/element "summary"
              (dom/text (str "Question map: " (count roots) " idea"
                          (when (not= 1 (count roots)) "s"))))
            (dom/ul
              (dom/props {:class "question-map__roots"})
              (let [commands (e/for [root (e/diff-by :id roots)] (RootRow root))]
                (e/for [[t command] (e/diff-by first (e/as-vec commands))]
                  (let [[kind root-id known?] command
                        result (e/server
                                 (e/Offload
                                   #(if (= ::known kind)
                                      (set-root-known!* user-id root-id known? spec)
                                      (teach-root!* user-id root-id))))]
                    (case result (t))))))))))))

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
          (case (reset! !submit {:id (str (random-uuid)) :text "" :question-pins? true
                                 :chat @!active :refs [] :mode mode-id
                                 :web-search? web-search?})
            (case (bus/consume! invocation-id)
              (t))))))))

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
