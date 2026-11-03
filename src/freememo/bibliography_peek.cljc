(ns freememo.bibliography-peek
  "Read a document's bibliography without opening its editor
   (plans/feedback-activity-biblio-youtube.md 3.2).

   One trigger in the toolbar: a book icon, with the citation beside it while
   the toolbar has room. Hover opens a card with every field and an Edit
   button; a click opens the editor. A device with no hover gets the card on a
   tap instead, since a tap that edited would leave it no way to peek. ⌘⇧B
   (`:peek-bibliography`) toggles the card from anywhere in the viewer.

   The card lives in the top layer (`popover=\"manual\"`). The toolbar measures
   its own width with containment, and a fixed element inside it would be
   placed against the toolbar rather than the viewport.

   The open state is one client atom per trigger, holding `{:key :via}` or nil.
   `:via` is how it opened — :hover closes when the pointer leaves, :tap and
   :key stay until dismissed — and `:key` is per opening, so a reopened card
   mounts and is placed again."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.bibliography-form :as bibform]
   [freememo.command-bus :as bus]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   #?(:clj [freememo.user-state :as us])
   #?(:cljs [goog.object :as gobj])
   #?(:cljs [freememo.vendor-libs :as vendor])))

(def open-delay-ms
  "Hover time before the card opens. The nav rail's value, so the two hover
   surfaces feel the same."
  150)

(def close-delay-ms
  "Time after the pointer leaves trigger and card before the card closes. Long
   enough to cross the gap between them."
  300)

;; ── What the card shows ─────────────────────────────────────────────────────

(defn author-names
  "`authors` as the form holds them, as one line: \"Given Family\", comma-joined.
   Blank names are left out. Post: a string, or nil when no name remains."
  [authors]
  (some->> authors
    (keep (fn [{:keys [given family]}]
            (not-empty (str/trim (str/join " " (remove str/blank? [given family]))))))
    seq
    (str/join ", ")))

(defn peek-lines
  "The card's content for one `load-source-for-topic*` result.

   Pre : `source` is that fn's result, or nil.
   Post: {:title :url :lines :inherited?}. `:lines` holds the non-blank fields
         in reading order — authors, container, issued, page — so the card
         renders no empty row. nil when `source` is nil."
  [source]
  (when-some [{:keys [form inherited?]} source]
    (let [blank->nil (fn [s] (when-not (str/blank? s) (str/trim s)))]
      {:title (blank->nil (:title form))
       :url (blank->nil (:url form))
       :lines (vec (keep identity
                     [(author-names (:authors form))
                      (blank->nil (:container-title form))
                      (some->> (blank->nil (:issued form)) (str "Published "))
                      (some->> (blank->nil (:page form)) (str "Page "))]))
       :inherited? (boolean inherited?)})))

;; ── Plain-DOM controller ────────────────────────────────────────────────────

#?(:cljs
   (defn- hover-capable? []
     (.-matches (.matchMedia js/window "(hover: hover)"))))

#?(:cljs
   (defn- open! [!open via]
     (reset! !open {:key (str (random-uuid)) :via via})))

#?(:cljs
   (defn- clear-timers! [timers]
     (doseq [t (vals @timers)] (js/clearTimeout t))
     (reset! timers {})))

#?(:cljs
   (defn- hover-enter!
     "The pointer reached the trigger or the card: keep an open card, and open
      a closed one after `open-delay-ms`."
     [{:keys [timers !open]} ^js e]
     (js/clearTimeout (:close @timers))
     (when (and (not= "touch" (.-pointerType e)) (hover-capable?) (nil? @!open))
       (js/clearTimeout (:open @timers))
       (swap! timers assoc :open
         (js/setTimeout #(when (nil? @!open) (open! !open :hover)) open-delay-ms)))))

#?(:cljs
   (defn- hover-leave!
     "The pointer left the trigger or the card: close a hover-opened card after
      `close-delay-ms`, unless it comes back first.
      Inv : the timer closes only the opening it was started for. A card closed
            and reopened by key or tap inside the delay is a new opening, and
            stays."
     [{:keys [timers !open]} ^js e]
     (js/clearTimeout (:open @timers))
     (let [opening @!open]
       (when (and (not= "touch" (.-pointerType e)) (= :hover (:via opening)))
         (swap! timers assoc :close
           (js/setTimeout #(when (= opening @!open) (reset! !open nil))
             close-delay-ms))))))

#?(:cljs
   (defn install-trigger!
     "Wire hover, click and tap on `trigger`.

      Pre : `trigger` is mounted; `!open` is this trigger's open atom;
            `edit!` opens the bibliography editor.
      Post: returns {:controller c :uninstall f}. A click on a hover-capable
            device closes the card and calls `edit!`; a tap, or a click on a
            device with no hover, toggles the card open via :tap."
     [^js trigger !open edit!]
     (let [c {:timers (atom {}) :!open !open}
           on-enter (fn [e] (hover-enter! c e))
           on-leave (fn [e] (hover-leave! c e))
           on-click (fn [^js e]
                      (clear-timers! (:timers c))
                      (if (and (not= "touch" (.-pointerType e)) (hover-capable?))
                        (do (reset! !open nil) (edit!))
                        (if (some? @!open) (reset! !open nil) (open! !open :tap))))]
       (.addEventListener trigger "pointerenter" on-enter)
       (.addEventListener trigger "pointerleave" on-leave)
       (.addEventListener trigger "click" on-click)
       {:controller c
        :uninstall (fn []
                     (clear-timers! (:timers c))
                     (.removeEventListener trigger "pointerenter" on-enter)
                     (.removeEventListener trigger "pointerleave" on-leave)
                     (.removeEventListener trigger "click" on-click))})))

#?(:clj (defn install-trigger! [_trigger _open _edit!] {:controller nil :uninstall (fn [])}))

#?(:cljs
   (defn- placement-anchor
     "The trigger while it is painted, else the toolbar that holds it. The
      trigger hides when the toolbar collapses, and ⌘⇧B still opens the card."
     [^js trigger]
     (if (pos? (.-length (.getClientRects trigger)))
       trigger
       (or (.closest trigger ".toolbar-container") trigger))))

#?(:cljs
   (defn- place-below! [^js card ^js anchor]
     (let [r (.getBoundingClientRect anchor)]
       (set! (.. card -style -top) (str (+ (.-bottom r) 6) "px"))
       (set! (.. card -style -left) (str (max 8 (.-left r)) "px"))
       (.setAttribute card "data-placed" "true"))))

#?(:cljs
   (defn- place-card!
     "Place a shown `card` below `trigger` with Floating UI, and keep it placed.

      Each update picks the anchor again (`placement-anchor`), so a trigger the
      toolbar hides while the card is open hands over to the toolbar instead of
      leaving a zero-size rect to place against. Falls back to `place-below!`
      when Floating UI does not load or a placement fails. Stores the
      autoUpdate stopper in `!stop`, and calls `placed!` once the card is
      placed. Does nothing after `!alive?` goes false."
     [^js card ^js trigger !alive? !stop placed!]
     (-> (vendor/ensure! :floating-ui)
       (.then (fn [_]
                (when @!alive?
                  (let [^js F (gobj/get js/window "FloatingUIDOM")
                        place (fn []
                                (-> (.computePosition F (placement-anchor trigger) card
                                      #js {:strategy "fixed"
                                           :placement "bottom-start"
                                           :middleware #js [(.offset F 6)
                                                            (.flip F)
                                                            (.shift F #js {:padding 8})]})
                                  (.then (fn [^js p]
                                           (set! (.. card -style -left) (str (.-x p) "px"))
                                           (set! (.. card -style -top) (str (.-y p) "px"))
                                           (.setAttribute card "data-placed" "true")))))]
                    (reset! !stop (.autoUpdate F (placement-anchor trigger) card place))
                    ;; Returned, so a rejected placement reaches the fallback.
                    (-> (place) (.then placed!))))))
       (.catch (fn [_]
                 (when @!alive?
                   (place-below! card (placement-anchor trigger))
                   (placed!)))))))

#?(:cljs
   (defn attach-card!
     "Show `card` in the top layer below the trigger and wire how it closes.

      Pre : `card` and `trigger` are mounted; `controller` is the trigger's;
            `via` is how the card opened.
      Post: the card is shown and placed, or, when the browser refuses to show
            it, closed again. Escape closes it; a pointer press outside card and
            trigger closes it; the pointer leaving a hover-opened card closes it
            after `close-delay-ms`. A card opened by tap or key focuses its Edit
            button, so Enter edits. Returns a fn that hides the card, removes
            every listener, and, when focus is inside the card, returns it to
            where it was when the card opened (the trigger when that is gone).
      Inv : Escape is the card's only when focus is in the card or on the
            trigger. Otherwise it still closes the card, and the focused
            surface keeps both its focus and the key."
     [^js card ^js trigger controller via]
     (let [!alive? (atom true)
           !stop (atom nil)
           !open (:!open controller)
           ;; Read before the card takes focus: `placed!` moves it later.
           ^js return-to (.-activeElement js/document)
           ;; Whether focus is inside the card, kept by events rather than read
           ;; at detach: Electric may remove the card before `detach!` runs,
           ;; and focus has then already fallen to <body>.
           !holds-focus? (atom false)
           on-focus-in (fn [_] (reset! !holds-focus? true))
           on-focus-out (fn [^js e]
                          ;; A null relatedTarget is focus falling away with the
                          ;; node, not the user moving it.
                          (let [to (.-relatedTarget e)]
                            (when (and to (not (.contains card to)))
                              (reset! !holds-focus? false))))
           close! (fn [] (reset! !open nil))
           owns-focus? (fn []
                         (let [a (.-activeElement js/document)]
                           (or (.contains card a) (= trigger a))))
           on-key (fn [^js e]
                    (when (= "Escape" (.-key e))
                      (when (owns-focus?)
                        (.preventDefault e)
                        (.stopPropagation e))
                      (close!)))
           on-press (fn [^js e]
                      (let [t (.-target e)]
                        (when-not (or (.contains card t) (.contains trigger t))
                          (close!))))
           on-enter (fn [e] (hover-enter! controller e))
           on-leave (fn [e] (hover-leave! controller e))
           placed! (fn []
                     (when (and @!alive? (not= :hover via))
                       (some-> (.querySelector card ".bib-peek-edit")
                         (.focus #js {:preventScroll true}))))]
       (.addEventListener card "pointerenter" on-enter)
       (.addEventListener card "pointerleave" on-leave)
       (.addEventListener card "focusin" on-focus-in)
       (.addEventListener card "focusout" on-focus-out)
       (.addEventListener js/document "keydown" on-key true)
       (.addEventListener js/document "pointerdown" on-press true)
       ;; Show on the next frame, not now. This call runs when Electric creates
       ;; the node, before it inserts the node and before it applies props, and
       ;; showPopover throws on a disconnected node or one with no popover
       ;; attribute. So the attribute is set here, not in props.
       (js/requestAnimationFrame
         (fn []
           (when (and @!alive? (.-isConnected card))
             (.setAttribute card "popover" "manual")
             (if (try (when (.-showPopover card) (.showPopover card)) true
                      (catch :default _ false))
               (place-card! card trigger !alive? !stop placed!)
               (close!)))))
       (fn detach! []
         (when (and @!holds-focus?
                    (let [a (.-activeElement js/document)]
                      (or (nil? a) (= a (.-body js/document)) (.contains card a))))
           (.focus (if (and return-to (.-isConnected return-to)
                            (not (.contains card return-to)))
                     return-to
                     trigger)
             #js {:preventScroll true}))
         (reset! !alive? false)
         (.removeEventListener card "pointerenter" on-enter)
         (.removeEventListener card "pointerleave" on-leave)
         (.removeEventListener card "focusin" on-focus-in)
         (.removeEventListener card "focusout" on-focus-out)
         (.removeEventListener js/document "keydown" on-key true)
         (.removeEventListener js/document "pointerdown" on-press true)
         (when-let [stop! @!stop] (stop!))
         (when (and (.-hidePopover card) (.matches card ":popover-open"))
           (.hidePopover card))))))

#?(:clj (defn attach-card! [_card _trigger _controller _via] (fn [] nil)))

#?(:cljs
   (defn- toggle-by-key!
     "What ⌘⇧B does: close an open card, else open one that holds focus."
     [!open]
     (if (some? @!open) (reset! !open nil) (open! !open :key))))

#?(:clj (defn- toggle-by-key! [_open] nil))

;; ── Components ──────────────────────────────────────────────────────────────

(e/defn PeekCard
  "The open card. Its source loads only while it is open, and again on each
   `:refresh` bump — the channel the citation beside the icon follows — so a
   card held open shows an edit made elsewhere.
   Pre : `opening` is the open atom's value, non-nil."
  [user-id topic-id trigger controller opening edit!]
  (e/client
    (let [;; nil while the read is in flight, then {:source s}. The wrapper
          ;; keeps a source that does not exist apart from one still loading.
          ;; A client e/as-vec: [] until the read crosses, so the card shows
          ;; its placeholder instead of going empty.
          loaded (peek (e/as-vec
                         (e/server
                           (let [refresh (e/watch (us/get-atom user-id :refresh))]
                             (e/Offload
                               #(hash-map :source
                                  (bibform/load-source-for-topic* refresh user-id topic-id)))))))
          content (peek-lines (:source loaded))]
      ;; Mounted under <body>, not beside the trigger. The toolbar hides its
      ;; meta group with display:none from collapse tier 2, and a top-layer
      ;; element inside a display:none ancestor is not rendered — so ⌘⇧B
      ;; opened a card nobody could see. Electric adds and removes <body>'s
      ;; children by identity, so a second child here is safe (.claude/rules/deployment-hook.md,
      ;; deployment hook section).
      (binding [dom/node js/document.body]
        (dom/div
          (dom/props {:class "bib-peek" :role "dialog"
                      :aria-label "Bibliography"})
          (let [detach! (e/snapshot (attach-card! dom/node trigger controller (:via opening)))]
            (e/on-unmount detach!))
          (if (nil? content)
            (dom/div (dom/props {:class "bib-peek-line bib-peek-muted"})
              (dom/text (if (nil? loaded) "Loading…" "No source recorded.")))
            (dom/div
              (dom/props {:class "bib-peek-body"})
              (if-some [url (:url content)]
                (dom/a
                  (dom/props {:class "bib-peek-title" :href url
                              :target "_blank" :rel "noopener noreferrer"})
                  (dom/text (or (:title content) url)))
                (dom/div (dom/props {:class "bib-peek-title"})
                  (dom/text (or (:title content) "Untitled source"))))
              ;; The loop has an element of its own, always mounted (docs/electric-patterns.md,
              ;; "An e/for Gets an Element of Its Own").
              (dom/div
                (dom/props {:class "bib-peek-lines"})
                (e/for [[_i line] (e/diff-by first (map-indexed vector (:lines content)))]
                  (dom/div (dom/props {:class "bib-peek-line"}) (dom/text line))))
              (when (:inherited? content)
                (dom/div (dom/props {:class "bib-peek-line bib-peek-muted"})
                  (dom/text "Inherited from the parent document")))))
          (dom/div
            (dom/props {:class "bib-peek-actions"})
            (dom/button
              (dom/props {:type "button" :class "btn btn-sm btn-secondary bib-peek-edit"})
              (dom/text "Edit")
              (dom/On "click" (fn [_] (edit!)) nil))))))))

;; Pre : mounted only in the :inline toolbar, while the document has a citation
;;       (`dctx/citation` non-nil).
;; Post: a trigger that peeks on hover and edits on click; ⌘⇧B is available
;;       while this is mounted and gone when it unmounts.
;; Inv : its own e/defn, so DocumentMetaGroup's JVM method stays under 64KB.
(e/defn BibliographyPeek
  []
  (e/client
    (let [user-id dctx/user-id
          topic-id dctx/bib-topic-id
          citation dctx/citation
          !show-bib dctx/!show-bib
          !open (atom nil)
          opening (e/watch !open)
          ;; {:trigger node :controller c} once the button has mounted and its
          ;; listeners are installed. The card reads both; a DOM node bound in a
          ;; `let` from the button's value never mounted the button.
          !wired (atom nil)
          wired (e/watch !wired)
          edit! (fn [] (reset! !open nil) (reset! !show-bib true))
          ;; One fn per trigger, so unmount can withdraw exactly what it published.
          toggle! (fn [] (toggle-by-key! !open))]
      ;; The card mounts under <body> (PeekCard); the anchor span holds only the
      ;; trigger.
      (dom/span
        (dom/props {:class "bib-peek-anchor"})
        (dom/button
          (dom/props {:type "button" :class "bib-peek-trigger"
                      :aria-label "Bibliography" :aria-haspopup "dialog"})
          (dom/props {:aria-expanded (str (some? opening))})
          (icons/Icon :book-open :size 14)
          ;; Hidden from collapse tier 1, leaving the icon.
          (dom/span (dom/props {:class "toolbar-collapse-citation"})
            (dom/text citation))
          (let [node dom/node
                {:keys [controller uninstall]} (e/snapshot (install-trigger! node !open edit!))]
            (e/on-unmount (fn [] (uninstall) (reset! !wired nil)))
            (reset! !wired {:trigger node :controller controller})))
        (when (and (some? opening) (some? wired))
          (e/for-by identity [_k [(:key opening)]]
            (PeekCard user-id topic-id (:trigger wired) (:controller wired) opening edit!)))
        (e/on-unmount #(bus/retract-ctx-value! :open-bibliography-peek! toggle!))
        (bus/publish-ctx! {:open-bibliography-peek! toggle!})))))

;; Pre : the toolbar overflow panel, where the inline trigger is hidden.
;; Post: a "Bibliography" item while a trigger publishes the peek, which opens
;;       the card below the toolbar; nothing otherwise.
(e/defn BibliographyPeekOverflowItem
  [close-overflow!]
  (e/client
    (when-some [toggle! (:open-bibliography-peek! (e/watch bus/!ctx))]
      (dom/button
        ;; Not `toolbar-overflow-panel-action`: that class is display:none until
        ;; a tier rule reveals it. The `.toolbar-overflow-docmeta` box around
        ;; this item is already revealed from tier 2, so the item needs nothing.
        (dom/props {:type "button" :class "btn btn-sm btn-secondary"})
        (icons/Icon :book-open :size 14)
        (dom/text "Bibliography")
        (dom/On "click" (fn [_] (close-overflow!) (toggle!)) nil)))))
