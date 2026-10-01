(ns freememo.command-palette
  "Cmd/Ctrl-K command palette — enumerates the freememo.commands registry,
   fuzzy-matches on :label, and invokes through command-bus/dispatch! (the
   same path as buttons and keyboard shortcuts).

   Mounted once in Main (always available). A persistent final row turns the
   typed text into a :search invocation, so Cmd-K doubles as search.

   Help rows (plans/searchable-help.md 4.1) follow the commands: one per
   freememo.help-content section that matches. A command a Help step documents
   carries :command-help-slug, which Shift+Enter and the row's ? button open.

   Contract: palette-rows is the single row-list derivation — the render body
   and the keydown handler MUST both call it with the same inputs, or Enter
   would run a different row than the one highlighted."
  (:require [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [hyperfiddle.electric-scroll0 :refer [Tape]]
            [freememo.commands :as commands]
            [freememo.command-bus :as bus]
            [freememo.help-content :as help]
            [freememo.navigation :as nav]
            [freememo.scroll :refer [Scroll-window]]
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

(def ^:private step-label-tier
  "A help row matched on a step label ranks after every title match, whose
   match-score tiers are 0..2 (D10)."
  3)

(defn help-rows
  "One row per Help section matching `query`, best first, in reading order
   within a tier. A section matches on its title by match-score, or on a
   resolved step label by substring. Rows carry :help-slug and no :id."
  [query]
  (->> help/sections
    (keep (fn [{:keys [slug title] :as section}]
            (when-let [score (or (match-score title query)
                               (when (help/step-label-hit? section query)
                                 step-label-tier))]
              {:help-slug slug :label title :score score})))
    (sort-by :score)))

(defn palette-rows
  "Every available command on `active-tab` that matches `query`, best first,
   then the matching Help sections, then the persistent search row. Rows
   carry :idx for selection."
  [active-tab ctx query]
  (let [matches (->> (commands/palette-listed active-tab)
                  (remove #(= :search (:id %))) ; rendered as the search row below
                  (filter #(bus/available? (:id %) ctx))
                  (keep #(when-let [s (match-score (:label %) query)]
                           (assoc % :score s
                                    :command-help-slug (help/slug-for-command (:id %)))))
                  (sort-by (juxt :score :label)))
        search-row {:id :search :search-row true
                    :label (if (= query "") "Search…" (str "Search: " query))}]
    (vec (map-indexed (fn [i row] (assoc row :idx i))
           (concat matches (help-rows query) [search-row])))))

(defn row-help-slug
  "The Help section a row opens on Shift+Enter: its own for a help row, the
   documenting section for a command row, else nil."
  [row]
  (or (:help-slug row) (:command-help-slug row)))

(defn- open-help!
  "Close the palette and open /help/<slug>. Runs at event time, so it reads
   the navigate! the shell last published."
  [slug]
  (close!)
  (when-let [navigate! (:navigate! @bus/!ctx)]
    (navigate! :help (nav/nav-help slug))))

(defn- run-row! [row query]
  (cond
    (:help-slug row) (open-help! (:help-slug row))
    (= :search (:id row)) (do (close!) (bus/dispatch! :search {:text query} :palette))
    :else (do (close!) (bus/dispatch! (:id row) nil :palette))))

;; ── Platform-split helpers (top-level, frame-safe) ─────────────────────────

(defn format-bind
  "Registry [id bind] → display chord for this platform+browser. Thin alias for
   commands/display-chord (the single formatter)."
  [id bind]
  (commands/display-chord id bind))

(defn scroll-selected-row-into-view!
  "Scroll the palette list until its selected row is visible."
  [node idx row-height]
  #?(:cljs
     (when (and node (>= idx 0))
       (let [top (* idx row-height)
             bottom (+ top row-height)
             viewport-top (.-scrollTop node)
             viewport-bottom (+ viewport-top (.-clientHeight node))]
         (cond
           (< top viewport-top) (set! (.-scrollTop node) top)
           (> bottom viewport-bottom) (set! (.-scrollTop node) (- bottom (.-clientHeight node))))))
     :clj nil)
  nil)

#?(:cljs
   (defn- on-palette-key
     "Container keydown: arrows — and Emacs-style Ctrl/Cmd-n / Ctrl/Cmd-p —
      move selection, Enter runs the highlighted row, Shift+Enter opens its
      Help section (and does nothing on a row with none), Escape closes, Tab
      cycles focus within the modal (modal-shell/trap-tab! on the content
      container). Reads atoms at event time and re-derives rows via
      palette-rows (see ns contract). Cmd-N never reaches the page in Chrome
      (browser-reserved: new window) — Ctrl-n is the binding that always
      works; Cmd-P is interceptable and preventDefault suppresses the print
      dialog."
     [active-tab !query !idx e]
     (let [rows (palette-rows active-tab @bus/!ctx @!query)
           n (count rows)
           k (.-key e)
           mod? (or (.-ctrlKey e) (.-metaKey e))
           next? (or (= k "ArrowDown") (and mod? (= k "n")))
           prev? (or (= k "ArrowUp") (and mod? (= k "p")))]
       (cond
         (= k "Escape") (do (.preventDefault e) (close!))
         next?          (do (.preventDefault e) (swap! !idx #(min (dec n) (inc %))))
         prev?          (do (.preventDefault e) (swap! !idx #(max 0 (dec %))))
         (= k "Enter")  (do (.preventDefault e)
                          (let [i (min @!idx (dec n))]
                            (when-let [row (and (>= i 0) (nth rows i nil))]
                              (if (.-shiftKey e)
                                (when-let [slug (row-help-slug row)] (open-help! slug))
                                (run-row! row @!query)))))
         (= k "Tab")    (modal-shell/trap-tab! (.-currentTarget e) e)
         :else nil)))
   :clj
   (defn- on-palette-key [_active-tab _!query _!idx _e] nil))

(e/defn PaletteRow
  "One palette row: the row button, and for a command a Help step documents,
   the ? button beside it. Its own e/defn so CommandPaletteRows stays shallow
   enough for Electric's macroexpander on a default JVM stack."
  [palette-item i row-count selection-index query-atom]
  (e/client
    (dom/tr
      (dom/props {:style {:--order i}})
      (dom/td
        (dom/props {:class "command-palette-cell"})
        (dom/button
          (dom/props {:class (if (= (:idx palette-item) selection-index)
                              "command-palette-row command-palette-row--selected"
                              "command-palette-row")
                      :type "button"
                      :aria-posinset (str (inc i))
                      :aria-setsize (str row-count)})
          (dom/span
            (dom/props {:class "command-palette-label"})
            (dom/text (:label palette-item)))
          (when-let [bind (format-bind (:id palette-item) (:bind palette-item))]
            (dom/span
              (dom/props {:class "command-palette-shortcut"})
              (dom/text bind)))
          (when (:help-slug palette-item)
            (dom/span
              (dom/props {:class "command-palette-shortcut"})
              (dom/text "Help")))
          (dom/On "click"
            (fn [_] (run-row! palette-item @query-atom))
            nil))
        ;; A sibling of the row button, never inside it, so its click cannot
        ;; reach the row's handler (4.3.2). Shift+Enter is the keyboard path,
        ;; so Tab skips it (4.3.3).
        (when-let [slug (:command-help-slug palette-item)]
          (dom/button
            (dom/props {:class "command-palette-help" :type "button"
                        :tabindex "-1"
                        :aria-label (str "Help: " (:title (help/section-by-slug slug)))
                        :data-tooltip "Help (Shift+Enter)"})
            (dom/text "?")
            (dom/On "click" (fn [_] (open-help! slug)) nil)))))))

(e/defn CommandPaletteRows [palette-items selection-index query-atom filter-key]
  "Render the command palette's virtualized result list."
  (e/client
    (let [row-height 40
          row-count (count palette-items)]
      (dom/div
        (dom/props {:class "command-palette-list tape-scroll"
                    :style {:--count row-count
                            :--row-height (str row-height "px")
                            :--grid-cols "1fr"}})
        (let [[offset limit] (Scroll-window row-height row-count dom/node
                               {:overquery-factor 1.5
                                :reset-key filter-key})]
          (case (scroll-selected-row-into-view! dom/node selection-index row-height) nil)
          (dom/table
            (dom/props {:role "presentation"})
            (e/for [i (Tape offset limit)]
              (let [palette-item (nth palette-items i nil)]
                (when palette-item
                  (PaletteRow palette-item i row-count selection-index query-atom))))))))))

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
            rows (palette-rows active-tab (e/watch bus/!ctx) query)]
        (dom/div
          (dom/props {:class "modal-backdrop"
                      :role "dialog" :aria-modal "true" :aria-label "Command palette"
                      :style {:align-items "flex-start" :padding-top "12vh"}})
          (modal-shell/FocusReturn)
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
                          :class "command-palette-input"})
              ;; rAF focus-on-mount: HTML :autofocus is inert on dynamically
              ;; inserted elements (see modal-shell/focus-on-mount!). e/snapshot
              ;; fires once at mount; the rAF defer lets FocusReturn's
              ;; synchronous opener-capture win before focus moves in.
              (e/snapshot (modal-shell/focus-on-mount! dom/node))
              (dom/On "input"
                (fn [e]
                  (reset! !query (-> e .-target .-value))
                  (reset! !idx 0))
                nil))

            (CommandPaletteRows rows idx !query query)))))))
