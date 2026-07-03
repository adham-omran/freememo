(ns freememo.command-palette
  "Cmd/Ctrl-K command palette — enumerates the freememo.commands registry,
   fuzzy-matches on :label, and invokes through command-bus/dispatch! (the
   same path as buttons and keyboard shortcuts).

   Mounted once in Main (always available). A persistent final row turns the
   typed text into a :search invocation, so Cmd-K doubles as search.

   Contract: palette-rows is the single row-list derivation — the render body
   and the keydown handler MUST both call it with the same inputs, or Enter
   would run a different row than the one highlighted."
  (:require [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.commands :as commands]
            [freememo.command-bus :as bus]
            [freememo.modal-shell :as modal-shell]))

(defonce !open? (atom false))

(defn- close! [] (reset! !open? false))

(bus/register-handler! :toggle-palette
  (fn [_ctx _payload] (swap! !open? not)))

;; ── Row derivation ─────────────────────────────────────────────────────────

(defn match-score
  "Rank of `label` against `query`: 0 prefix, 1 substring (or empty query),
   2 in-order subsequence, nil no match. Case-insensitive."
  [label query]
  (let [l (str/lower-case label)
        q (str/lower-case query)]
    (cond
      (= q "") 1
      (str/starts-with? l q) 0
      (str/includes? l q) 1
      :else (loop [li 0 qi 0]
              (cond
                (= qi (count q)) 2
                (= li (count l)) nil
                (= (nth l li) (nth q qi)) (recur (inc li) (inc qi))
                :else (recur (inc li) qi))))))

(defn palette-rows
  "Available commands on `active-tab` matching `query`, best first, capped,
   plus the persistent search row. Rows carry :idx for selection."
  [active-tab ctx query]
  (let [matches (->> (commands/palette-listed active-tab)
                  (remove #(= :search (:id %))) ; rendered as the search row below
                  (filter #(bus/available? (:id %) ctx))
                  (keep #(when-let [s (match-score (:label %) query)]
                           (assoc % :score s)))
                  (sort-by (juxt :score :label))
                  (take 12))
        search-row {:id :search :search-row true
                    :label (if (= query "") "Search…" (str "Search: " query))}]
    (vec (map-indexed (fn [i row] (assoc row :idx i))
           (concat matches [search-row])))))

(defn- run-row! [row query]
  (close!)
  (if (= :search (:id row))
    (bus/dispatch! :search {:text query})
    (bus/dispatch! (:id row))))

;; ── Platform-split helpers (top-level, frame-safe) ─────────────────────────

#?(:cljs
   (defn format-bind
     "\"meta+shift+e\" → \"⌘⇧E\" on macOS, \"Ctrl+Shift+E\" elsewhere."
     [bind]
     (when bind
       (let [mac? (boolean (re-find #"(?i)mac" (str js/navigator.platform)))
             part (fn [p] (case p
                            "meta" (if mac? "⌘" "Ctrl")
                            "ctrl" "Ctrl"
                            "shift" (if mac? "⇧" "Shift")
                            "alt" (if mac? "⌥" "Alt")
                            (str/upper-case p)))]
         (str/join (if mac? "" "+") (map part (str/split bind #"\+"))))))
   :clj
   (defn format-bind [_bind] nil))

#?(:cljs
   (defn- on-palette-key
     "Container keydown: arrows move selection, Enter runs the highlighted
      row, Escape closes. Reads atoms at event time and re-derives rows via
      palette-rows (see ns contract)."
     [active-tab !query !idx e]
     (let [rows (palette-rows active-tab @bus/!ctx @!query)
           n (count rows)]
       (case (.-key e)
         "Escape"    (do (.preventDefault e) (close!))
         "ArrowDown" (do (.preventDefault e) (swap! !idx #(min (dec n) (inc %))))
         "ArrowUp"   (do (.preventDefault e) (swap! !idx #(max 0 (dec %))))
         "Enter"     (do (.preventDefault e)
                       (let [i (min @!idx (dec n))]
                         (when-let [row (and (>= i 0) (nth rows i nil))]
                           (run-row! row @!query))))
         nil)))
   :clj
   (defn- on-palette-key [_active-tab _!query _!idx _e] nil))

;; ── Component ──────────────────────────────────────────────────────────────

(e/defn CommandPalette
  "The palette modal. Renders nothing while closed."
  [active-tab]
  (e/client
    (when (e/watch !open?)
      (let [!query (atom "")
            query (e/watch !query)
            !idx (atom 0)
            idx (e/watch !idx)
            rows (palette-rows active-tab @bus/!ctx query)]
        (dom/div
          (dom/props {:class "modal-backdrop"
                      :style {:align-items "flex-start" :padding-top "12vh"}})
          (dom/On "click" (fn [_] (close!)) nil)
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "min(560px, 92vw)" :padding "0"
                                :overflow "hidden" :display "flex"
                                :flex-direction "column"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/On "keydown" (fn [e] (on-palette-key active-tab !query !idx e)) nil)

            (dom/input
              (dom/props {:placeholder "Type a command or search…"
                          :aria-label "Command palette"
                          :style {:padding "14px 16px" :font-size "15px"
                                  :border "none" :outline "none"
                                  :border-bottom "0.5px solid var(--color-border-light)"
                                  :background "transparent"
                                  :color "var(--color-text-primary)"}})
              (modal-shell/focus-on-mount! dom/node)
              (dom/On "input"
                (fn [e]
                  (reset! !query (-> e .-target .-value))
                  (reset! !idx 0))
                nil))

            (dom/div
              (dom/props {:style {:max-height "50vh" :overflow "auto" :padding "6px"}})
              (e/for [row (e/diff-by :id rows)]
                (let [selected? (= (:idx row) idx)]
                  (dom/button
                    (dom/props {:style {:display "flex" :width "100%"
                                        :align-items "center"
                                        :justify-content "space-between"
                                        :gap "12px" :padding "8px 10px"
                                        :border "none" :cursor "pointer"
                                        :text-align "left" :font-size "14px"
                                        :border-radius "var(--radius-md)"
                                        :color "var(--color-text-primary)"
                                        :background (if selected?
                                                      "var(--color-bg-hover, rgba(127,127,127,.15))"
                                                      "transparent")}})
                    (dom/span (dom/text (:label row)))
                    (when-let [b (format-bind (:bind row))]
                      (dom/span
                        (dom/props {:style {:font-size "12px"
                                            :color "var(--color-text-hint)"}})
                        (dom/text b)))
                    (dom/On "click" (fn [_] (run-row! row @!query)) nil)))))))))))
