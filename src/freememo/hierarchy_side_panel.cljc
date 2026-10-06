(ns freememo.hierarchy-side-panel
  "Left side panel for the viewer. Hierarchy is the default tab. Pins is the
   second tab.

   Reactivity: subscribes to the per-user :tree-mutations atom (tree shape) and
   :meta-refresh (done-status, which drives the strike-through) — NOT :refresh.
   :tree-mutations is bumped only when the topic tree shape changes (extract
   create/delete, rename, document import). Decoupling from :refresh keeps the
   rich-text editor's `topic` re-fetch out of the side-panel update path so
   extracting does not destroy and recreate the Quill instance.

   Open/collapsed state is persisted per-topic via freememo.settings."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [contrib.data :refer [clamp-left]]
   [freememo.a11y :as a11y]
   [freememo.navigation :as nav]
   [freememo.pin-side-panel :refer [PinsBody]]
   [freememo.command-bus :as bus]
   [freememo.util :as util]
   [freememo.tree-dnd :as tree-dnd]
   [freememo.viewport :as viewport]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.topic-move :as topic-move])
   #?(:clj [freememo.user-state :as us])))

#?(:clj
   (defn- slim-topic [t]
     (dissoc t :topics/content :topics/content_text)))

(declare build-rows)

(defn build-hierarchy-rows*
  "Server pipeline — subtree query + group-by + depth-first flatten, all
   sited here so the full topic list never crosses the wire; the client
   reads row-count and the virtual-scroll window rows only.
   `expanded-set` is the client's manually-expanded node ids (bounded).
   `_tree-rev` is the watched value of (us/get-atom user-id :tree-mutations);
   bumping it re-runs this query. Returns a row vec or nil when the topic
   has no resolvable root."
  [_tree-rev user-id page-topic-id expanded-set]
  #?(:clj
     (when (and user-id page-topic-id)
       (when-let [root-id (db/get-root-topic-id page-topic-id)]
         (when-let [scope-root (db/get-topic root-id)]
           (let [items (mapv slim-topic (db/get-subtree user-id root-id))
                 children-map (group-by :topics/parent_id items)]
             (build-rows (slim-topic scope-root) page-topic-id
               children-map (set expanded-set) items)))))
     :cljs nil))

(defn side-panel-badge
  "Returns [text color] or nil to skip rendering. Page rows skip the badge
   because the row already shows 'p.<n>' which is more informative."
  [kind]
  (case kind
    "pdf"        ["PDF"     "var(--color-badge-pdf)"]
    "epub"       ["EPUB"    "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown"   ["MD"      "var(--color-badge-web)"]
    "audio"      ["Audio"   "var(--color-badge-web)"]
    "video"      ["Video"   "var(--color-badge-video)"]
    "video-playlist" ["Playlist" "var(--color-badge-video)"]
    "supermemo"  ["SuperMemo" "var(--color-badge-supermemo)"]
    "poem"       ["Poem"    "var(--color-badge-epub)"]
    "repo"       ["Repo"    "var(--color-badge-occlusion)"]
    "commit"     ["Commit"  "var(--color-badge-occlusion)"]
    "dir"        ["Folder"  "var(--color-badge-occlusion)"]
    "code"       ["Code"    "var(--color-badge-web)"]
    "basic"      ["E"       "var(--color-badge-epub)"]
    nil))

(defn build-rows
  "Pure: depth-first flatten of the subtree rooted at scope-root.
   Returns a vec of {:depth :topic :has-children :current? :expanded-display?}
   rows. :expanded-display? mirrors the arrow-glyph rule the panel renders
   (current row or manually expanded), which intentionally differs from the
   walk's descend rule (root / path-to-current / manually expanded).

   A node's children are rendered when the node is on the path from
   scope-root to current-id, or when the user explicitly expanded it.
   scope-root is always treated as expanded.

   Page nodes are flattened away at the root: for a pdf/epub/web/wikipedia
   scope-root, each `page` child is replaced by that page's own children (the
   extracts), so pages are not rendered and extracts appear directly under the
   root. Mirrors DocumentTreeView (knowledge_tree.cljc)."
  [scope-root current-id children-map expanded-set items]
  (let [id->parent (into {} (map (juxt :topics/id :topics/parent_id)) items)
        path-set   (loop [id current-id acc (transient #{})]
                     (if (or (nil? id) (contains? acc id))
                       (persistent! acc)
                       (recur (get id->parent id) (conj! acc id))))]
    (letfn [(child-nodes [node depth]
              (let [raw (vec (get children-map (:topics/id node)))]
                ;; No `poem` (poem-reader-spec row 2.4.2): this flattens a
                ;; root's `page` children away, and a poem has no children.
                (if (and (zero? depth)
                      (#{"pdf" "epub" "web" "wikipedia"} (:topics/kind node)))
                  (vec (mapcat (fn [c]
                                 (if (= "page" (:topics/kind c))
                                   (get children-map (:topics/id c))
                                   [c]))
                         raw))
                  raw)))
            (walk [node depth]
              (let [id   (:topics/id node)
                    kids (child-nodes node depth)
                    has? (boolean (seq kids))
                    row  {:depth depth
                          :topic node
                          :has-children has?
                          :current? (= id current-id)
                          :expanded-display? (or (= id current-id)
                                               (contains? expanded-set id))}
                    expanded? (or (zero? depth)
                                (contains? path-set id)
                                (contains? expanded-set id))]
                (if (and expanded? has?)
                  (cons row (mapcat #(walk % (inc depth)) kids))
                  [row])))]
      (vec (walk scope-root 0)))))

(def ^:private panel-motion-ms
  "The side panels' open and close animation, in ms. Matches the width
   transition on `[data-motion]` panels in index.css."
  160)

(defn install-panel-motion!
  "Animate a side panel's open and close (plans/nav-stage-rail.md 4.1–4.3).

   Pre : `panel` is the SidePanelShell root. `!open?` is the caller's atom,
         and `!body-open?` starts equal to it.
   Post: `!body-open?` follows `!open?` at once on open. On close it stays
         true until the width transition has run, so the body shrinks with
         the panel instead of leaving an empty box to animate (N4). It follows
         at once when no transition runs: under reduced motion, or before the
         motion is armed. Motion arms two frames after install, so a saved
         state applies on load with no animation.
         Returns a fn that uninstalls the watch and the timer.

   Only `!body-open?` gates the body, so a close never unmounts and remounts
   it: the atom changes once, when the hold ends."
  [panel !open? !body-open?]
  #?(:clj (fn [])
     :cljs
     (let [k (random-uuid)
           timer (atom nil)
           animates? (fn []
                       (and (.hasAttribute panel "data-motion")
                            (not= "0s" (.-transitionDuration (js/getComputedStyle panel)))))
           settle! (fn []
                     (some-> @timer js/clearTimeout)
                     (reset! timer nil)
                     (reset! !body-open? @!open?))]
       (add-watch !open? k
         (fn [_ _ was now]
           (if (and was (not now) (animates?))
             (do (some-> @timer js/clearTimeout)
                 (reset! timer (js/setTimeout settle! (+ panel-motion-ms 40))))
             (settle!))))
       (js/requestAnimationFrame
         (fn [] (js/requestAnimationFrame #(.setAttribute panel "data-motion" ""))))
       (fn []
         (remove-watch !open? k)
         (some-> @timer js/clearTimeout)))))

(defn mark-resizing!
  "Hold the panel's width transition off for one drag (4.2), so the width
   follows the pointer with no lag.
   Pre : `handle` is the resize handle, a direct child of the panel.
   Post: the panel carries `data-resizing` until the handle's pointerup or
         pointercancel."
  [handle]
  #?(:clj nil
     :cljs
     (when-let [panel (.-parentElement handle)]
       (.setAttribute panel "data-resizing" "")
       (let [done (fn done [_]
                    (.removeAttribute panel "data-resizing")
                    (.removeEventListener handle "pointerup" done)
                    (.removeEventListener handle "pointercancel" done))]
         (.addEventListener handle "pointerup" done)
         (.addEventListener handle "pointercancel" done)))))

(e/defn SidePanelShell
  "Shared side-panel chrome, factored out of this ns and freememo.right-side-panel
   (RightSidePanel), which independently grew the same header-toggle +
   resize-handle shell. Lives here rather than its own namespace because this
   dedup pass is scoped to the two files that duplicated it.

   Renders: an outer div (`class`, plus `class`--collapsed when closed) whose
   width tracks `width-px` while open; a `side-panel__header` row with a ☰
   toggle that flips `!open?` and stages the new value into `!save` for the
   caller's own persistence effect; a `side-panel__resize` handle (only when
   open) wired to util/start-drag-px!/key-resize-px!, staging the committed
   width into `!width-save`.

   `side` is the panel-relative edge the resize handle sits on — :right for
   the hierarchy panel (handle on its inner/right edge; content follows,
   :after) or :left for the right side-panel (handle on its inner/left edge;
   content precedes, :before). Drives the `side-panel__resize--<side>` class,
   drag `invert?` (true for :left — dragging right shrinks the panel), and
   the content-side passed to util/panel-resize-max.

   The caller owns querying its own persisted open?/width values, the
   settings namespace, and running the save-token effects (offload + :success
   case) — this component only renders chrome and mutates the atoms it's
   handed, so each panel's own (already-fixed) persistence behavior is
   untouched by this refactor.

   `Header-extra` is `(e/fn [] ...)`, rendered after the toggle when open (a
   title, a tab row, etc). `Body` is `(e/fn [] ...)`, rendered below the
   resize handle when open."
  [{:keys [side class toggle-aria-label resize-aria-label
           open? !open? !save
           width-px !width-px !width-save]}
   Header-extra Body]
  (e/client
    (let [[content-side invert?] (case side :left [:before true] :right [:after false])
          ;; Gates the header extra and the body. It lags `open?` on close only,
          ;; for the length of the width transition (install-panel-motion!).
          !body-open? (atom @!open?)
          body-open? (e/watch !body-open?)]
      (dom/div
        ;; The width is a custom property, not an inline width, so the phone
        ;; overlay rule in index.css can replace it. Position lives there too.
        (dom/props {:class (str class (when-not open? (str " " class "--collapsed")))
                    :style {:--side-panel-width (when open? (str width-px "px"))}})
        (let [uninstall-motion (install-panel-motion! dom/node !open? !body-open?)]
          (e/on-unmount uninstall-motion))

        ;; Header row — toggle (always visible) + Header-extra (only when
        ;; open). The row renders even when collapsed so the fixed gutter
        ;; still surfaces the hamburger.
        (dom/div
          (dom/props {:class "side-panel__header"})
          (dom/button
            (dom/props {:class "side-panel__toggle"
                        :aria-label toggle-aria-label
                        :aria-expanded (str (boolean open?))})
            (dom/text "☰")
            (dom/On "click"
              (fn [_]
                (let [next-open? (not @!open?)]
                  (reset! !open? next-open?)
                  (reset! !save next-open?)))
              nil))
          (when body-open? (Header-extra)))

        ;; Resize handle on the inner edge; only when open.
        (when open?
          (dom/div
            (dom/props {:class (str "side-panel__resize side-panel__resize--" (name side))
                        :title "Drag to resize"
                        :role "separator" :aria-orientation "vertical"
                        :aria-label resize-aria-label :tabindex "0"
                        :aria-valuenow (str (int (or width-px 0)))})
            (dom/On "pointerdown"
              (fn [e]
                (mark-resizing! (.-currentTarget e))
                (util/start-drag-px! e !width-px
                  {:min 180
                   :max (max 180 (util/panel-resize-max (.-currentTarget e) content-side 320))
                   :invert? invert?
                   :on-commit #(reset! !width-save %)}))
              nil)
            (dom/On "keydown"
              (fn [e]
                (util/key-resize-px! e !width-px
                  {:min 180
                   :max (max 180 (util/panel-resize-max (.-currentTarget e) content-side 320))
                   :invert? invert?
                   :on-commit #(reset! !width-save %)}))
              nil)))

        (when body-open? (Body))))))

;; ---------------------------------------------------------------------------
;; HierarchySidePanel is split into three e/defns (row, tree, panel). One
;; ~190-line deeply-nested e/defn overflowed the Electric compiler's
;; macroexpansion recursion on the default JVM stack; splitting caps each
;; form's nesting depth so `-M:dev` boots without a -Xss flag.
;; ---------------------------------------------------------------------------

(e/defn HierarchyRow
  "One topic row in the hierarchy tree. Extracted so the row's deep DOM subtree
   macroexpands in its own stack budget."
  [i row navigate! !nav-target
   !drag-src drag-src forbidden !drop-cmd
   !expanded-set !scroll-node]
  (e/client
    (let [{:keys [depth topic has-children current? expanded-display?]} row
          id (:topics/id topic)
          title (or (:topics/title topic) "(empty)")
          kind (or (:topics/kind topic) "basic")
          is-page (= kind "page")
          page-num (:topics/page_number topic)
          topic-status (or (:topics/status topic) "active")
          done? (= topic-status "done")
          expanded? expanded-display?
          badge (side-panel-badge kind)
          open-topic! (fn [_]
                        (when-not current?
                          (cond
                            (and is-page page-num !nav-target)
                            (reset! !nav-target {:topic-id (:topics/parent_id topic)
                                                 :page page-num})
                            navigate!
                            (navigate! :viewer (nav/nav-topic id nil)))))
          toggle-children! (fn [e]
                             (.stopPropagation e)
                             (when-not current?
                               (let [sn @!scroll-node
                                     st (when sn (.-scrollTop sn))]
                                 (swap! !expanded-set
                                   (fn [s] (if (contains? s id) (disj s id) (conj s id))))
                                 (util/restore-scroll-after-render! sn st))))]
      (dom/tr
        (dom/props {:style {:--order i}})
        (dom/td
          (dom/props {:style {:display "flex" :align-items "center" :gap "6px"
                              ;; Base 20 reserves the fixed left gutter for the
                              ;; absolutely positioned .drag-grip (see tree_dnd).
                              :padding-left (str (+ 26 (* depth 14)) "px")
                              :padding-right "8px"
                              :cursor (if current? "default" "pointer")
                              :background (when current? "var(--color-bg-card)")
                              :border-left "3px solid transparent"
                              :outline (when current? "2px solid var(--color-primary)")
                              :outline-offset (when current? "-2px")
                              :font-weight (if current? "600" "400")}})
          ;; DnD re-parenting on this cell (tr is display:contents → not
          ;; draggable). Page stubs are structural — not draggable.
          (tree-dnd/DragDropRow! id (not is-page)
            !drag-src drag-src forbidden !drop-cmd)
          (when-not current?
            (a11y/KeyActivate {:label (str "Open " title)} open-topic!))
          (dom/On "click" open-topic! nil)

          (if has-children
            (dom/span
              (dom/props {:style {:width "14px" :font-size "10px" :flex-shrink "0"
                                  :user-select "none" :text-align "center"
                                  :cursor "pointer"
                                  :color "var(--color-text-secondary)"}})
              (when-not current?
                (a11y/KeyActivate
                  {:role "button" :label (str (if expanded? "Collapse " "Expand ") title)}
                  toggle-children!))
              (dom/text (if expanded? "▼" "▶"))
              (dom/On "click" toggle-children! nil))
            (dom/span
              (dom/props {:style {:width "14px" :flex-shrink "0"}})))

          (when (and is-page page-num)
            (dom/span
              (dom/props {:style {:font-size "11px"
                                  :color "var(--color-text-secondary)"
                                  :flex-shrink "0" :font-variant-numeric "tabular-nums"}})
              (dom/text "p." page-num)))

          (when badge
            (let [[badge-text badge-color] badge]
              (dom/span
                (dom/props {:class "type-badge"
                            :style {:background badge-color}})
                (dom/text badge-text))))

          (dom/span
            (dom/props {:style {:overflow "hidden"
                                :text-overflow "ellipsis"
                                :white-space "nowrap"
                                :flex "1" :min-width "0"
                                :color "var(--color-text-primary)"
                                :text-decoration (when done? "line-through")}})
            (dom/text title)))))))

(e/defn HierarchyTree
  "Scrollable topic tree: rows query + virtual window + drag/drop. Extracted
   from HierarchySidePanel to cap macroexpansion depth."
  [user-id page-topic-id navigate! !nav-target]
  (e/client
    ;; :tree-mutations = tree shape; :meta-refresh = done-status (drives the
    ;; strike-through). Both re-run the rows query.
    (let [tree-rev   (e/server (+ (e/watch (us/get-atom user-id :tree-mutations))
                                 (e/watch (us/get-atom user-id :meta-refresh))))
          !expanded-set (atom #{})
          expanded-set (e/watch !expanded-set)
          ;; Server-form binding (sited-by-use): the subtree rows stay
          ;; server-side; only row-count and the window rows cross.
          rows (e/server (e/Offload #(build-hierarchy-rows* tree-rev user-id
                                       page-topic-id expanded-set)))]
      (when (e/server (some? rows))
        (let [row-count (e/server (count rows))
              row-height 36
              !scroll-node (atom nil)
              ;; Drag-and-drop nesting (no promote-to-root here — the
              ;; hierarchy tab has no root target by design).
              !drag-src (atom nil)
              drag-src (e/watch !drag-src)
              ;; The nil test lives INSIDE the thunk on purpose. e/Offload latches
              ;; the previous value while it recomputes, so `forbidden` holds the
              ;; old set (#{} before the first drag) instead of going empty and
              ;; blanking every row for the length of the query. A stale set only
              ;; costs a drop the user cannot make: db/reparent-topic! rejects a
              ;; cycle on the server.
              forbidden (e/server (e/Offload #(if drag-src (set (db/get-subtree-ids drag-src)) #{})))
              !drop-cmd (atom nil)
              drop-cmd (e/watch !drop-cmd)]

          (let [[t _] (e/Token drop-cmd)]
            (when t
              (let [{:keys [src dst]} drop-cmd
                    ok (e/server (e/Offload #(topic-move/move-topic! user-id src dst)))]
                (case ok
                  (case (e/client (reset! !drop-cmd nil)) (t))))))

          (dom/div
            (dom/props {:class "tape-scroll"
                        :style {:flex "1" :overflow-y "auto" :min-height "0"
                                :scrollbar-gutter "stable"}})
            (reset! !scroll-node dom/node)
            (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 2})]
              (dom/props {:style {:--count row-count
                                  :--grid-cols "1fr"
                                  :--row-height (str row-height "px")}})
              (dom/table
                (dom/props {:style {:width "100%"
                                    :font-size "13px"}})
                (if (pos? row-count)
                  (e/for [i (Tape offset limit)]
                    (let [row (e/server (nth rows i nil))]
                      (when row
                        (HierarchyRow i row navigate! !nav-target
                          !drag-src drag-src forbidden !drop-cmd
                          !expanded-set !scroll-node))))
                  (dom/tr
                    (dom/props {:style {:--order 1 :height "auto"}})
                    (dom/td
                      (dom/props {:style {:padding "16px 12px" :text-align "center"
                                          :color "var(--color-text-secondary)" :font-size "13px"}})
                      (dom/text "No items."))))))))))))

(e/defn HierarchySidePanel [user-id page-topic-id root-topic-id navigate! !nav-target]
  (e/client
    ;; Frame isolation — remounts only when root-topic-id changes (i.e. when
    ;; navigating between documents). Page navigation within one document
    ;; updates :current? reactively without remount, so !open? state and the
    ;; DOM subtree persist across page scrolls.
    (e/for-by identity [_k [root-topic-id]]
      (let [;; A saved state restores at every Viewer viewport, including a state
            ;; a command set. Only a document with no saved state starts closed on
            ;; a phone, where the open panel covers the document. A deref, not a
            ;; watch: rotating the phone must not re-create !open?.
            default-open?   (not @viewport/!phone?)
            persisted-open? (e/server (e/Offload #(settings/get-hierarchy-open
                                                    user-id root-topic-id default-open?)))
            initial-open?   persisted-open?
            !open?          (atom initial-open?)
            open?           (e/watch !open?)
            !save           (atom nil)
            save-val        (e/watch !save)
            [?save-token _] (e/Token save-val)
            persisted-width (e/server (e/Offload #(settings/get-hierarchy-width user-id root-topic-id)))
            !width-px       (atom persisted-width)
            width-px        (e/watch !width-px)
            !width-save     (atom nil)
            width-save      (e/watch !width-save)
            [?width-token _] (e/Token width-save)
            persisted-tab   (e/server (settings/get-left-panel-tab user-id root-topic-id))
            !tab             (atom persisted-tab)
            tab              (e/watch !tab)
            !tab-save        (atom nil)
            tab-save         (e/watch !tab-save)
            [?tab-token _]   (e/Token tab-save)
            toggle-left-pane!
            (e/snapshot
              (fn []
                (let [next-open? (not @!open?)]
                  (reset! !open? next-open?)
                  (reset! !save next-open?))))
            show-left-pane!
            (e/snapshot
              (fn [tab]
                (when (#{"hierarchy" "pins"} tab)
                  (reset! !open? true)
                  (reset! !save true)
                  (reset! !tab tab)
                  (reset! !tab-save tab))))]

        (bus/publish-ctx! {:toggle-left-pane! toggle-left-pane!
                           :show-left-pane! show-left-pane!})
        (e/on-unmount #(bus/retract-ctx! [:toggle-left-pane! :show-left-pane!]))

        (when-some [token ?save-token]
          (let [r (e/server (e/Offload #(settings/save-hierarchy-open user-id root-topic-id save-val)))]
            (case r (if (:success r) (token) (token (:error r))))))

        (when-some [token ?width-token]
          (let [r (e/server (e/Offload #(settings/save-hierarchy-width user-id root-topic-id width-save)))]
            (case r (if (:success r) (token) (token (:error r))))))

        (when-some [token ?tab-token]
          (let [r (e/server (e/Offload
                              #(settings/save-left-panel-tab user-id root-topic-id tab-save)))]
            (case r (if (:success r) (token) (token (:error r))))))

        (SidePanelShell
          {:side :right
           :class "hierarchy-side-panel"
           :toggle-aria-label "Toggle hierarchy panel"
           :resize-aria-label "Resize hierarchy panel"
           :open? open? :!open? !open? :!save !save
           :width-px width-px :!width-px !width-px :!width-save !width-save}
          (e/fn []
            (dom/div
              (dom/props {:class "side-panel__tabs" :role "tablist"
                          :aria-label "Left panel"})
              (e/for [entry (e/diff-by :id [{:id "hierarchy" :label "Hierarchy"}
                                              {:id "pins" :label "Pins"}])]
                (let [id (:id entry)
                      selected? (= tab id)]
                  (dom/button
                    (dom/props {:class (str "side-panel__tab"
                                         (when selected? " side-panel__tab--active"))
                                :type "button" :role "tab"
                                :aria-selected (str selected?)})
                    (dom/text (:label entry))
                    (dom/On "click"
                      (fn [_] (reset! !tab id) (reset! !tab-save id)) nil))))))
          (e/fn []
            (case tab
              "pins" (PinsBody page-topic-id user-id)
              (if (nil? page-topic-id)
                (dom/div
                  (dom/props {:style {:padding "16px 12px" :font-size "13px"
                                      :color "var(--color-text-secondary)"}})
                  (dom/text "No page selected."))
                (HierarchyTree user-id page-topic-id navigate! !nav-target)))))))))
