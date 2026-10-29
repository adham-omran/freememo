(ns freememo.library-card-row
  "Own display descendants and current delegated actions for one Library row."
  (:require [freememo.context-menu-surface :as menu]
            [freememo.icon-paths :as icons]
            [freememo.math :as math]
            [freememo.tooltip :as tooltip]))

(defn- element [tag class]
  (doto (.createElement js/document tag)
    (aset "className" class)))

(defn- text! [node value]
  (let [value (str (or value ""))]
    (when (not= value (.-textContent node)) (set! (.-textContent node) value))))

(defn- attr! [node key value]
  (if (some? value)
    (let [value (str value)]
      (when (not= value (.getAttribute node key)) (.setAttribute node key value)))
    (when (.hasAttribute node key) (.removeAttribute node key))))

(defn- shown! [node shown?]
  (set! (.. node -style -display) (if shown? "" "none")))

(defn- action-for [row actions event]
  (loop [node (.-target event)]
    (when (and node (not (identical? row node)))
      (or (.get actions node) (recur (.-parentElement node))))))

(defn- closest-target [event selector]
  (let [target (.-target event)
        target (if (.-closest target) target (.-parentElement target))]
    (some-> target (.closest selector))))

(defn- invoke! [state action event]
  (.stopPropagation event)
  (when-let [callback (get-in state [:actions (keyword action)])]
    (callback)))

(defn create!
  "Install one delegated listener set. Each event reads the current row model."
  [row]
  (let [!state (atom nil)
        !cells (atom {})
        actions (js/WeakMap.)
        guard (fn [event]
                (when-not (and (:visible? @!state) (not (.-hidden row)))
                  (.preventDefault event)
                  (.stopImmediatePropagation event)))
        click (fn [event]
                (when-not (closest-target event "input")
                  (let [action (or (action-for row actions event) "edit")]
                    (invoke! @!state (if (and (= action "diff") (not (:diff? @!state))) "edit" action) event))))
        keydown (fn [event]
                  (when (and (#{"Enter" " "} (.-key event))
                             (not (closest-target event "button,input")))
                    (when-let [action (.get actions (.-target event))]
                      (.preventDefault event)
                      (invoke! @!state action event))))
        events ["click" "keydown" "contextmenu" "focusin"]]
    (doseq [event events] (.addEventListener row event guard true))
    (.addEventListener row "click" click)
    (.addEventListener row "keydown" keydown)
    {:node row :!state !state :!cells !cells :actions actions
     :dispose! (fn []
                 (doseq [event events] (.removeEventListener row event guard true))
                 (.removeEventListener row "click" click)
                 (.removeEventListener row "keydown" keydown)
                 (menu/retract-focus-target! row)
                 (reset! !state nil)
                 (reset! !cells {}))}))

(defn sync-row! [{:keys [node !state]} model]
  (reset! !state model)
  (set! (.-className node) (:class model))
  (set! (.-hidden node) (not (:visible? model)))
  (attr! node "inert" (when-not (:visible? model) ""))
  (attr! node "data-row-key" (when (:visible? model) (pr-str (:row-key model))))
  (attr! node "data-enter" (when (:fresh? model) "1"))
  (.setProperty (.-style node) "--order" (str (:pos model)))
  (if (:visible? model)
    (when (.contains node (.-activeElement js/document))
      (menu/publish-focus-target! node (:target model)))
    (do
      (when (.contains node (.-activeElement js/document))
        (.blur (.-activeElement js/document)))
      (menu/retract-focus-target! node))))

(defn- build-cell! [role cell actions]
  (case role
    :diff
    (let [direction (element "span" "")
          marked (element "span" "card-cell-glyph")
          remarked (element "span" "card-cell-glyph card-cell-glyph-remarked")
          suspended (element "span" "card-cell-glyph")]
      (set! (.-className cell) "card-cell-diff")
      (.set actions cell "diff")
      (text! marked "★") (text! remarked "✎") (text! suspended "⏸")
      (set! (.. marked -style -color) "var(--color-primary-text)")
      (set! (.. suspended -style -color) "var(--color-text-secondary)")
      (tooltip/set-tooltip! marked "Marked in Anki")
      (tooltip/set-tooltip! remarked "Has remarks in Anki")
      (doseq [node [direction marked remarked suspended]] (.appendChild cell node))
      {:node cell :direction direction :marked marked :remarked remarked :suspended suspended})

    :kind
    (let [badge (element "span" "type-badge")]
      (set! (.-className cell) "card-cell-badge")
      (.appendChild cell badge)
      {:node cell :badge badge})

    :front
    (let [content (element "div" "card-row-html")]
      (.setAttribute cell "dir" "auto")
      (.setAttribute cell "tabindex" "0")
      (.setAttribute cell "aria-label" "Edit card")
      (.set actions cell "edit")
      (.appendChild cell content)
      {:node cell :content content :html (volatile! ::unset)})

    :back
    (let [content (element "div" "card-row-html")]
      (.setAttribute cell "dir" "auto")
      (.appendChild cell content)
      {:node cell :content content :html (volatile! ::unset)})

    :doc
    (let [label (element "span" "card-cell-doc-label")]
      (set! (.-className cell) "card-cell-doc")
      (.setAttribute cell "tabindex" "0")
      (.set actions cell "document")
      (.appendChild cell label)
      {:node cell :label label})

    :added
    (do (set! (.-className cell) "card-cell-added") {:node cell})

    :stats
    (let [button (element "button" "btn btn-sm")]
      (set! (.-className cell) "card-cell-center")
      (.setAttribute button "type" "button")
      (.setAttribute button "aria-label" "Lifetime statistics")
      (.set actions button "stats")
      (tooltip/set-tooltip! button "Statistics — schedule, review history, forgetting curve")
      (set! (.-innerHTML button) (icons/svg-markup :chart-column 14))
      (.appendChild cell button)
      {:node cell :button button})

    :delete
    (let [button (element "button" "btn-delete-x")]
      (set! (.-className cell) "card-cell-center")
      (.setAttribute button "type" "button")
      (.set actions button "delete")
      (tooltip/set-tooltip! button "Delete card" :aria? true)
      (text! button "×")
      (.appendChild cell button)
      {:node cell :button button})))

(defn sync-cell!
  "Update a cell's owned descendants without replacing its Electric-owned td."
  [{:keys [!cells actions]} role cell model]
  (let [view (or (get @!cells role)
                 (let [view (build-cell! role cell actions)]
                   (swap! !cells assoc role view)
                   view))]
    (case role
      :diff
      (let [{:keys [direction marked remarked suspended]} view
            flags (:flags model)
            susp (:suspended flags)
            n (or (:suspended susp) 0)]
        (tooltip/set-tooltip! cell (:direction-tooltip model))
        (attr! cell "tabindex" (when (:diff? model) "0"))
        (attr! cell "aria-label" (when (:diff? model) "Show Anki difference"))
        (text! direction (:direction-glyph model))
        (set! (.. direction -style -color) (:direction-color model))
        (shown! marked (:marked flags))
        (shown! remarked (:remarked flags))
        (shown! suspended (pos? n))
        (set! (.. suspended -style -opacity) (if (= n (:total susp)) "1" "0.45"))
        (tooltip/set-tooltip! suspended
          (str n " of " (:total susp) " card" (when (not= (:total susp) 1) "s") " suspended")))

      :kind
      (do (set! (.-className (:badge view)) (:badge-class model))
          (text! (:badge view) (:badge-label model)))

      :front
      (do (set! (.-className cell) (str "card-row-cell" (when (:span2? model) " card-cell-span2")))
          (when (not= @(:html view) (:front-html model))
            (vreset! (:html view) (:front-html model))
            (math/set-html! (:content view) (:front-html model))))

      :back
      (do (set! (.-className cell) (str "card-row-cell" (when (:span2? model) " card-cell-hidden")))
          (when (not= @(:html view) (:back-html model))
            (vreset! (:html view) (:back-html model))
            (math/set-html! (:content view) (:back-html model))))

      :doc
      (do (text! (:label view) (:doc-label model))
          (attr! cell "aria-label" (str "Open " (:doc-label model)))
          (tooltip/set-tooltip! (:label view) (:doc-label model)))

      :added (text! cell (:added model))
      :stats (do (shown! (:button view) (:stats? model))
                 (set! (.-disabled (:button view)) (not (and (:stats? model) (:visible? model)))))
      :delete (set! (.-disabled (:button view)) (not (:visible? model))))))
