(ns freememo.graph-page
  "Graph tab — an Obsidian-style WebGL view of the knowledge graph. Nodes are
   concepts (kg_entities); edges are approved entity→entity facts. Click a
   concept to highlight its neighbours and open its fact panel (reused from
   quiz-page/EntityCardPopover). Positions are server-computed (sfdp) and cached
   (freememo.kg-graph); the client only renders + filters (freememo.graph-render
   over sigma.js).

   All sigma calls route through the reader-conditioned plain defns below so no
   cljs-only alias leaks into shared/CLJ code and the reactive body keeps equal
   CLJ/CLJS signal counts (repo frame-mismatch rule)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.quiz-page :as quiz]
   #?(:clj [freememo.kg-graph :as kgg])
   #?(:clj [freememo.user-state :as us])
   #?(:cljs [freememo.graph-render :as render])))

;; ---------------------------------------------------------------------------
;; Server query — whole defn under #?(:clj …); called only inside e/server.
;; ---------------------------------------------------------------------------

#?(:clj
   (defn graph-payload*
     "Cached, positioned render payload for the user's whole graph. `version` is
      the :kg-mutations counter (reactive) — any KG mutation recomputes lazily on
      the next open. Post: {:nodes :edges :predicates :docs}."
     [version user-id]
     (kgg/get-or-compute user-id "all" version)))

;; ---------------------------------------------------------------------------
;; sigma bridge — reader conditionals live here, never in the e/defn body.
;; ---------------------------------------------------------------------------

(defn schedule-graph-init!
  "Build the sigma renderer on the next tick and publish its ctx to !ctx (so the
   filter controls in the reactive body can reach it)."
  [!ctx container payload on-select]
  #?(:cljs (js/setTimeout
             (fn [] (reset! !ctx (render/init! container payload on-select)))
             0)
     :clj nil))

(defn apply-filters! [ctx source pred-set min-degree]
  #?(:cljs (render/apply-filters! ctx source pred-set min-degree) :clj nil))

(defn focus-by-label! [ctx payload q]
  #?(:cljs (render/focus-node! ctx (render/find-node-id-by-label payload q)) :clj nil))

(defn destroy-graph! [ctx]
  #?(:cljs (render/destroy! ctx) :clj nil))

;; ---------------------------------------------------------------------------
;; Filters sidebar
;; ---------------------------------------------------------------------------

(defn- pred-checked? [pred-set idx]
  ;; nil pred-set = "all allowed".
  (or (nil? pred-set) (contains? pred-set idx)))

(defn- toggle-pred
  "Toggle predicate `idx` in `pred-set` (nil = all). Returns nil when the result
   is the full set, so the common 'everything on' case stays cheap (nil)."
  [pred-set idx n-preds]
  (let [full (set (range n-preds))
        cur  (or pred-set full)
        nxt  (if (contains? cur idx) (disj cur idx) (conj cur idx))]
    (when-not (= nxt full) nxt)))

(e/defn FiltersPanel
  [payload !ctx !source source !preds preds !min-degree min-degree !search]
  (e/client
    (let [docs (:docs payload)
          predicates (:predicates payload)
          n-preds (count predicates)
          max-deg (reduce (fn [m n] (max m (nth n 2 0))) 1 (:nodes payload))]
      (dom/div
        (dom/props {:style {:width "240px" :flex-shrink "0" :overflow-y "auto"
                            :border-right "1px solid var(--color-border)"
                            :padding "12px" :font-size "13px"
                            :display "flex" :flex-direction "column" :gap "16px"}})

        ;; Search → focus a concept
        (dom/div
          (dom/label (dom/props {:style {:font-weight "600" :display "block" :margin-bottom "4px"}})
            (dom/text "Find concept"))
          (dom/input
            (dom/props {:type "search" :placeholder "Search…"
                        :style {:width "100%" :padding "4px 6px"
                                :border "1px solid var(--color-border)" :border-radius "4px"}})
            (dom/On "input" (fn [e] (reset! !search (.. e -target -value)) nil) nil)
            (dom/On "keydown"
              (fn [e]
                (when (= "Enter" (.-key e))
                  (focus-by-label! @!ctx payload @!search))
                nil)
              nil)))

        ;; Source / document scope
        (dom/div
          (dom/label (dom/props {:style {:font-weight "600" :display "block" :margin-bottom "4px"}})
            (dom/text "Document"))
          (dom/select
            (dom/props {:style {:width "100%" :padding "4px"}})
            (dom/On "change"
              (fn [e]
                (let [v (.. e -target -value)
                      src (if (= v "all") :all (parse-long v))]
                  (reset! !source src)
                  (apply-filters! @!ctx src @!preds @!min-degree))
                nil)
              nil)
            (dom/option (dom/props {:value "all" :selected (= source :all)}) (dom/text "All documents"))
            (e/for [[id title] (e/diff-by first docs)]
              (dom/option (dom/props {:value (str id) :selected (= source id)})
                (dom/text (str title))))))

        ;; Degree threshold
        (dom/div
          (dom/label (dom/props {:style {:font-weight "600" :display "block" :margin-bottom "4px"}})
            (dom/text (str "Min connections: " min-degree)))
          (dom/input
            (dom/props {:type "range" :min "1" :max (str (max 1 max-deg)) :value (str min-degree)
                        :style {:width "100%"}})
            (dom/On "input"
              (fn [e]
                (let [v (or (parse-long (.. e -target -value)) 1)]
                  (reset! !min-degree v)
                  (apply-filters! @!ctx @!source @!preds v))
                nil)
              nil)))

        ;; Predicate filter
        (when (pos? n-preds)
          (dom/div
            (dom/div (dom/props {:style {:font-weight "600" :margin-bottom "4px"}})
              (dom/text "Relations"))
            (dom/div
              (dom/props {:style {:max-height "260px" :overflow-y "auto"
                                  :display "flex" :flex-direction "column" :gap "2px"}})
              (e/for [[idx label] (e/diff-by first (map-indexed vector (map second predicates)))]
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                                      :cursor "pointer"}})
                  (dom/input
                    (dom/props {:type "checkbox" :checked (pred-checked? preds idx)})
                    (dom/On "change"
                      (fn [_]
                        (let [nxt (toggle-pred @!preds idx n-preds)]
                          (reset! !preds nxt)
                          (apply-filters! @!ctx @!source nxt @!min-degree))
                        nil)
                      nil))
                  (dom/span (dom/text (str label))))))))))))

;; ---------------------------------------------------------------------------
;; Page
;; ---------------------------------------------------------------------------

(e/defn GraphPage [user-id navigate!]
  (e/client
    (let [kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))
          payload (e/server (e/Offload #(graph-payload* kg-bump user-id)))
          !ctx (atom nil)
          !entity-card (atom nil) entity-card (e/watch !entity-card)
          !source (atom :all) source (e/watch !source)
          !preds (atom nil) preds (e/watch !preds)
          !min-degree (atom 1) min-degree (e/watch !min-degree)
          !search (atom "")
          node-count (count (:nodes payload))]
      (dom/div
        (dom/props {:style {:display "flex" :height "100%" :min-height "0"}})
        (if (zero? node-count)
          ;; Empty state — no approved entity→entity facts yet.
          (dom/div
            (dom/props {:style {:margin "auto" :max-width "420px" :text-align "center"
                                :color "var(--color-text-secondary)" :padding "24px"}})
            (icons/Icon :share-2 :size 40)
            (dom/p (dom/props {:style {:font-size "15px" :margin "12px 0 4px"}})
              (dom/text "No concept connections yet"))
            (dom/p (dom/props {:style {:font-size "13px"}})
              (dom/text "Distill documents in the Knowledge tab to extract facts. Once concepts link to each other, they appear here as a graph."))
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary" :style {:margin-top "12px"}})
              (dom/text "Go to Knowledge")
              (dom/On "click" (fn [_] (navigate! :knowledge)) nil)))
          ;; Graph
          (e/client
            (FiltersPanel payload !ctx !source source !preds preds !min-degree min-degree !search)
            (dom/div
              (dom/props {:style {:position "relative" :flex "1" :min-width "0" :height "100%"}})
              ;; Remount sigma when the graph identity changes (mutation) — keyed
              ;; on the mutation counter + node count.
              (e/for-by identity [_k [[kg-bump node-count]]]
                (dom/div
                  (dom/props {:style {:position "absolute" :inset "0"}})
                  (schedule-graph-init! !ctx dom/node payload
                    (fn [id] (reset! !entity-card (when id {:id id}))))
                  (e/on-unmount
                    (fn [] (destroy-graph! @!ctx) (reset! !ctx nil)))))
              ;; Hint overlay
              (dom/div
                (dom/props {:style {:position "absolute" :bottom "8px" :left "12px"
                                    :font-size "12px" :color "var(--color-text-secondary)"
                                    :pointer-events "none"}})
                (dom/text (str node-count " concepts · click a node for its facts, scroll to zoom"))))))
        ;; Fact panel (reused) — mounts over everything when a node is selected.
        (quiz/EntityCardPopover user-id !entity-card entity-card)))))
