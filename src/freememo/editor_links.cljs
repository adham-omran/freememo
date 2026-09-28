(ns freememo.editor-links
  "One click listener for every anchor in the editor.

   Three behaviours used to coexist here: a Wikipedia link showed an Import
   popover, a `/viewer/topic/<id>` link navigated, and every other link did
   nothing — a click inside `contenteditable` does not follow an href, so 833
   of one account's 1046 external links were inert. Two listeners for one
   gesture is what this replaces.

   `freememo.link-target` decides WHAT a click can offer; this namespace owns
   only the DOM — finding the anchor, placing the card, scrolling to a heading.
   The split is not tidiness: `test/` is Clojure-only, so classification had to
   leave this file to be testable at all.

   **The only channel out is the atoms in `chans`.** Navigation, import and
   toasts all belong to Electric; a DOM listener that did them itself would be
   unable to reach the server, the router or the toast queue.

   Pre : the caller owns the atoms and Quill's lifecycle.
   Post: `install!` returns a teardown fn that removes every listener it added.
   Inv : an anchor lookup is scoped to the editor root, never
         `document.getElementById`. Imported content carries arbitrary ids and
         may collide with the app's own — `#main-content` is a real one."
  (:require [clojure.string :as str]
            [freememo.editor-popovers :as editor-popovers]
            [freememo.link-target :as lt]))

;; ── The card ───────────────────────────────────────────────────────

(def ^:private card-style
  (str "position:fixed;display:none;z-index:1000;align-items:center;gap:8px;"
    "padding:6px 8px;background:var(--color-bg-card);"
    "border:1px solid var(--color-border);border-radius:var(--radius-sm);"
    "box-shadow:0 2px 8px rgba(0,0,0,0.18);font-size:13px;max-width:380px;"))

(def ^:private label-style
  (str "overflow:hidden;text-overflow:ellipsis;white-space:nowrap;"
    "max-width:220px;color:var(--color-text-secondary);"))

(defn- readable-target
  "What the card shows: host and path, without the scheme's noise.
   Post: a string; the whole url when it does not parse as one."
  [url]
  (or (some-> (re-find #"^https?://(.+)$" (str url)) second
        (str/replace #"/$" ""))
    (str url)))

(defn- place-card!
  "Put `card` just below `anchor`, clamped into the viewport.
   Pre : `card` is displayed, so it has measurable width."
  [^js card ^js anchor]
  (let [r (.getBoundingClientRect anchor)
        w (.-offsetWidth card)
        left (max 8 (min (.-left r) (- (.-innerWidth js/window) w 8)))]
    (set! (.. card -style -top) (str (+ (.-bottom r) 4) "px"))
    (set! (.. card -style -left) (str left "px"))))

;; ── Scrolling to a heading ─────────────────────────────────────────

(def ^:private highlight-class "anchor-target-flash")
(def ^:private highlight-ms 1600)

(defn- scroll-to-anchor!
  "Scroll the editor to the element carrying `id` and flash it.

   Pre : `root` is the `.ql-editor` element; `id` is a fragment with no `#`.
   Post: true when a target existed and was scrolled to; false otherwise — the
         caller toasts on false rather than leaving the click silent.
   Inv : scoped to `root`. A document-wide lookup would match the app's own
         elements, which imported content can name."
  [^js root id]
  (let [escaped (if (exists? js/CSS.escape) (js/CSS.escape id) id)
        target (or (.querySelector root (str "#" escaped))
                 ;; `[id=…]` also matches an id a selector cannot start with,
                 ;; e.g. one beginning with a digit, which Wikipedia emits.
                 (.querySelector root (str "[id=\"" escaped "\"]")))]
    (boolean
      (when target
        (.scrollIntoView target #js {:behavior "smooth" :block "start"})
        (.add (.-classList target) highlight-class)
        (js/setTimeout #(.remove (.-classList target) highlight-class) highlight-ms)
        true))))

(defn flush-pending-anchor!
  "Land the anchor a cross-document click left behind.

   Pre : `root` is the `.ql-editor` of the document just loaded; `!pending` is
         the atom holding the fragment, or nil.
   Post: the atom is nil afterwards, whether or not a target was found — a
         fragment that outlived its navigation must not fire on the next
         document too.
   Inv : deferred a frame. Quill's `setContents` has written the DOM by the
         time the caller runs, but layout has not settled, so an immediate
         `scrollIntoView` lands at the wrong offset."
  [^js root !pending]
  (when-let [id (some-> @!pending not-empty)]
    (reset! !pending nil)
    (js/requestAnimationFrame #(scroll-to-anchor! root id))))

;; ── Install ────────────────────────────────────────────────────────

(defn install!
  "Wire every anchor in `editor` to one click listener.

   Pre : `editor` is a live Quill instance. `chans` = {:!import :!navigate
         :!toast :!base-url :!import-status}, all client atoms the caller owns.
   Post: a teardown fn removing every listener and the card element.
   Inv : publishes to `chans` and does nothing else server-shaped."
  [editor chans]
  (let [{:keys [!import !navigate !toast !base-url !import-status]} chans
        ^js ed editor
        ^js root (.-root ed)
        ^js card (js/document.createElement "div")
        ^js label (js/document.createElement "span")
        ^js import-btn (js/document.createElement "button")
        ^js view-btn (js/document.createElement "button")
        !target (atom nil)                 ; the resolved link the card is for
        visible? (fn [] (not= "none" (.. card -style -display)))
        hide! (fn [] (set! (.. card -style -display) "none"))
        toast! (fn [level message]
                 (reset! !toast {:level level :message message
                                 :ts (js/Date.now)}))

        anchor-at (fn [^js node]
                    (when (and node (.-closest node)) (.closest node "a")))

        show-card! (fn [^js a target]
                     (reset! !target target)
                     (set! (.-textContent label) (readable-target (:url target)))
                     (set! (.-textContent import-btn) "Import")
                     (.remove (.-classList import-btn) "importing")
                     (set! (.. card -style -display) "flex")
                     (place-card! card a))

        act! (fn [^js a href modified?]
               (let [{:keys [kind topic-id fragment url view-url] :as target}
                     (lt/resolve-href href (deref !base-url))]
                 (case kind
                   :external (if modified?
                               (js/window.open view-url "_blank" "noopener")
                               (show-card! a target))
                   :in-app (do (hide!)
                               (if modified?
                                 (js/window.open
                                   (str "/viewer/topic/" topic-id
                                     (when fragment (str "#" fragment)))
                                   "_blank" "noopener")
                                 (reset! !navigate {:topic-id topic-id
                                                    :fragment fragment
                                                    :ts (js/Date.now)})))
                   :fragment (do (hide!)
                                 (when-not (scroll-to-anchor! root fragment)
                                   (toast! :info
                                     (str "No heading named \"" fragment
                                       "\" in this document."))))
                   :no-base (do (hide!)
                                (toast! :info
                                  (str "This link is relative and the document "
                                    "has no source address to resolve it against.")))
                   ;; mailto:/tel: — the OS takes it. `contenteditable`
                   ;; suppresses the browser's own handling, so it needs saying.
                   :handler (do (hide!) (set! (.. js/window -location -href) url))
                   ;; :inert and :inert-citation show nothing at all.
                   (hide!))))

        on-click (fn [e]
                   (when-let [^js a (anchor-at (.-target e))]
                     (.preventDefault e)
                     (act! a (.getAttribute a "href")
                       (or (.-metaKey e) (.-ctrlKey e)))))
        ;; Middle click fires auxclick, not click.
        on-aux (fn [e]
                 (when (= 1 (.-button e))
                   (when-let [^js a (anchor-at (.-target e))]
                     (.preventDefault e)
                     (act! a (.getAttribute a "href") true))))
        on-doc-click (fn [e]
                       (when (and (visible?)
                               (not (.contains card (.-target e)))
                               (not (anchor-at (.-target e))))
                         (hide!)))
        on-key (fn [e] (when (and (= "Escape" (.-key e)) (visible?)) (hide!)))
        on-scroll (fn [_] (when (visible?) (hide!)))
        unregister-popover! (editor-popovers/register! visible? hide!)]

    (set! (.-className card) "link-actions-popover")
    (.setAttribute card "style" card-style)
    (.setAttribute label "style" label-style)
    (doseq [[^js b text cls] [[import-btn "Import" "btn btn-sm btn-primary"]
                              [view-btn "View" "btn btn-sm"]]]
      (set! (.-className b) cls)
      (set! (.-type b) "button")
      (set! (.-textContent b) text))
    (.appendChild card label)
    (.appendChild card import-btn)
    (.appendChild card view-btn)
    (.appendChild (.-body js/document) card)

    (.addEventListener view-btn "click"
      (fn [e]
        (.preventDefault e)
        (when-let [u (:view-url @!target)]
          (js/window.open u "_blank" "noopener"))
        (hide!)))

    (.addEventListener import-btn "click"
      (fn [e]
        (.preventDefault e)
        (when-not (= @!import-status :importing)
          (when-let [u (:url @!target)]
            (reset! !import-status :importing)
            (set! (.-textContent import-btn) "Importing…")
            (.add (.-classList import-btn) "importing")
            (reset! !import {:url u :ts (js/Date.now)})))))

    ;; Status → button text. The Electric side owns the transitions.
    (add-watch !import-status ::link-card-button
      (fn [_ _ _ st]
        (case st
          :importing (do (set! (.-textContent import-btn) "Importing…")
                         (.add (.-classList import-btn) "importing"))
          :queued (do (set! (.-textContent import-btn) "Queued")
                      (.remove (.-classList import-btn) "importing")
                      (js/setTimeout (fn [] (hide!) (reset! !import-status :idle)) 1200))
          :done (do (set! (.-textContent import-btn) "Imported")
                    (.remove (.-classList import-btn) "importing")
                    (js/setTimeout (fn [] (hide!) (reset! !import-status :idle)) 1500))
          :already-exists (do (set! (.-textContent import-btn) "Already imported")
                              (.remove (.-classList import-btn) "importing")
                              (js/setTimeout (fn [] (hide!) (reset! !import-status :idle)) 1500))
          :error (do (set! (.-textContent import-btn) "Error")
                     (.remove (.-classList import-btn) "importing")
                     (js/setTimeout (fn [] (reset! !import-status :idle)) 2000))
          nil)))

    (.addEventListener root "click" on-click)
    (.addEventListener root "auxclick" on-aux)
    (.addEventListener js/document "click" on-doc-click)
    (.addEventListener js/document "keydown" on-key)
    ;; capture: the editor pane scrolls, not the window
    (.addEventListener js/document "scroll" on-scroll true)
    (.addEventListener js/window "resize" on-scroll)

    (fn teardown []
      (remove-watch !import-status ::link-card-button)
      (.removeEventListener root "click" on-click)
      (.removeEventListener root "auxclick" on-aux)
      (.removeEventListener js/document "click" on-doc-click)
      (.removeEventListener js/document "keydown" on-key)
      (.removeEventListener js/document "scroll" on-scroll true)
      (.removeEventListener js/window "resize" on-scroll)
      (unregister-popover!)
      (when (.-parentNode card) (.remove card)))))
