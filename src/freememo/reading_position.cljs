(ns freememo.reading-position
  "The browser half of reading-position memory
   (plans/reading-position-memory.md §4.3 to §4.5).

   THREE INPUTS DRIVE A TRACKER, and none of them is a reactive value.
   1. `installed!` — a content writer reports, from inside its own plain fn and
      after its write, that `surface` now holds `topic-id`'s content in `node`
      (§4 3.6). A restore runs only against an install of its own topic (3.7).
   2. `stored!` — the keyed `ReadingPosition` frame hands over the server's
      row, or nil when there is none (§4 3.5.2).
   3. Document-level listeners, installed once (§4 3.4, 4.4): `scroll` and the
      user-input events in the capture phase, `pagehide`, and
      `visibilitychange`. `scroll` does not bubble, but a capture-phase listener
      on the document sees every element's scroll, so a tracker adds no
      listener of its own and the editor never has to know at attach time
      whether `.ql-editor` or `.ql-container` scrolls (§4 3.2, 4.8).

   A tracker captures only after user input on its own install (§4 3.3), so a
   restore's own scroll and a layout shift before the reader touches anything
   are never saved."
  (:require
   [freememo.reading-anchor :as ra]))

(def ^:private debounce-ms
  "Quiet time after the last scroll before a position is sent (§4 4.3)."
  1000)

(def ^:private settle-ms
  "How long a restore keeps re-applying while layout settles (§4 5.5)."
  3000)

(def ^:private endpoint "/api/reading-position")

;; The last install per surface: {surface {:topic-id :node :seq :fragment?}}.
;; A tracker whose topic is not the last install's is inactive (§4 3.7).
(defonce ^:private !installs (atom {}))
(defonce ^:private !install-seq (atom 0))

;; Every live tracker. A tracker is an atom; see `tracker!` for its shape.
(defonce ^:private !live (atom #{}))

;; §4 4.7 — the latest position captured in this tab, per [topic-id surface].
;; It is newer than the server's row whenever both exist, so a restore reads
;; it first (§4 5.4).
(defonce ^:private !latest (atom {}))

;; ── Surface adapters (§4 3.2) ──────────────────────────────────────

(defn- scrolls? [^js el]
  (and el (> (.-scrollHeight el) (.-clientHeight el))))

(defn- scroller
  "The element that scrolls `surface`, given the node its install reported.
   The editor's is decided now, never at attach: `.ql-editor` and
   `.ql-container` can both scroll, and which one does depends on the layout."
  [surface ^js node]
  (case surface
    "editor" (let [container (.-parentElement node)]
               (if (and (not (scrolls? node)) (scrolls? container)) container node))
    "code-source" (.-parentElement node)
    node))

(defn- own-scroller?
  "Whether `target` of a scroll event is the scroller of `surface`. A table or
   a fence that scrolls in its own box is not."
  [surface ^js node ^js target]
  (case surface
    "editor" (or (identical? target node) (identical? target (.-parentElement node)))
    "code-source" (identical? target (.-parentElement node))
    (identical? target node)))

(defn- input-region
  "The element user input must land in to count for `surface` (§4 3.3)."
  [surface ^js node]
  (case surface
    "editor" (or (.closest node ".quill-editor-wrapper") (.-parentElement node))
    "code-source" (.-parentElement node)
    node))

(defn- source-lines
  "The lines of a code-source install, split once per install. The source of a
   code topic never changes (`topic_code` is insert-only)."
  [tracker ^js node]
  (let [{:keys [lines-seq lines]} @tracker
        seq-now (get-in @tracker [:install :seq])]
    (if (= lines-seq seq-now)
      lines
      (let [ls (.split (or (.-textContent node) "") "\n")]
        (swap! tracker assoc :lines ls :lines-seq seq-now)
        ls))))

(defn- block-view
  "The blocks of `surface` as the accessors `reading-anchor` takes:
   {:n :top-of :height-of :text-of}, tops in `sc`'s content coordinates."
  [tracker surface ^js node ^js sc]
  (let [srect (.getBoundingClientRect sc)
        origin (- (+ (.-top srect) (.-clientTop sc)) (.-scrollTop sc))]
    (if (= "code-source" surface)
      (let [lines (source-lines tracker node)
            style (js/getComputedStyle node)
            lh (js/parseFloat (.-lineHeight style))
            lh (if (js/isNaN lh) (* 1.5 (js/parseFloat (.-fontSize style))) lh)
            top0 (+ (- (.-top (.getBoundingClientRect node)) origin)
                   (js/parseFloat (.-paddingTop style)))]
        {:n (.-length lines)
         :top-of (fn [i] (+ top0 (* i lh)))
         :height-of (fn [_] lh)
         :text-of (fn [i] (aget lines i))})
      (let [els (case surface
                  "poem" (.querySelectorAll node ".poem-verse")
                  (.-children node))
            rect (fn [i] (.getBoundingClientRect (aget els i)))]
        {:n (.-length els)
         :top-of (fn [i] (- (.-top (rect i)) origin))
         :height-of (fn [i] (.-height (rect i)))
         :text-of (fn [i] (.-textContent (aget els i)))}))))

;; ── Tracker state ──────────────────────────────────────────────────

(defn- active?
  "§4 3.7: the last install on the tracker's surface named its own topic, and
   that content is still in the document."
  [tracker]
  (let [{:keys [surface install]} @tracker
        last-install (get @!installs surface)]
    (boolean (and install
               (= (:seq install) (:seq last-install))
               (.-isConnected ^js (:node install))))))

(defn- input-since-install? [tracker]
  (let [{:keys [install input-seq]} @tracker]
    (and install (= input-seq (:seq install)))))

(defn- cancel-settle! [tracker]
  (when-let [id (:settle-raf @tracker)]
    (js/cancelAnimationFrame id)
    (swap! tracker dissoc :settle-raf)))

;; ── Save (§4 4) ────────────────────────────────────────────────────

(defn- beacon!
  "§4 4.2. Form-encoded, so the route reads it with the form parser every other
   beacon here uses."
  [topic-id surface {:keys [anchor fraction]}]
  (let [body (js/URLSearchParams.
               #js {"topic_id" (str topic-id) "surface" surface
                    "i" (str (:i anchor)) "fp" (:fp anchor)
                    "frac" (str (:frac anchor)) "fraction" (str fraction)})]
    (when-not (and (.-sendBeacon js/navigator) (.sendBeacon js/navigator endpoint body))
      (-> (js/fetch endpoint #js {:method "POST" :body body :keepalive true
                                  :credentials "same-origin"})
        (.catch (fn [_] nil))))))

(defn- send! [tracker]
  (let [{:keys [topic-id surface pending timer]} @tracker]
    (when timer (js/clearTimeout timer))
    (swap! tracker dissoc :pending :timer)
    (when pending (beacon! topic-id surface pending))))

(defn- capture!
  "Record where the reader is now. Pre: the tracker is active and the reader
   gave input since the install."
  [tracker]
  (let [{:keys [topic-id surface install]} @tracker
        node (:node install)
        sc (scroller surface node)
        {:keys [n top-of height-of text-of]} (block-view tracker surface node sc)]
    (when-let [pos (ra/capture* n top-of height-of text-of
                     (.-scrollTop sc) (.-clientHeight sc) (.-scrollHeight sc))]
      (swap! !latest assoc [topic-id surface] pos)
      (swap! tracker assoc :pending pos)
      pos)))

(defn- schedule-capture!
  "One capture per animation frame while the reader scrolls, then a send after
   `debounce-ms` of quiet (§4 4.3)."
  [tracker]
  (when-not (:capture-raf @tracker)
    (swap! tracker assoc :capture-raf
      (js/requestAnimationFrame
        (fn []
          (swap! tracker dissoc :capture-raf)
          (when (and (active? tracker) (input-since-install? tracker))
            (when (capture! tracker)
              (when-let [t (:timer @tracker)] (js/clearTimeout t))
              (swap! tracker assoc :timer
                (js/setTimeout #(send! tracker) debounce-ms)))))))))

(defn- flush!
  "Send at once: the frame unmounts, the page hides, or the tab closes (§4 4.4).
   The final capture runs only while the tracker is active, so a flush after
   another topic's content replaced this one sends this topic's last capture
   and never the other topic's layout."
  [tracker]
  (when-let [id (:capture-raf @tracker)]
    (js/cancelAnimationFrame id)
    (swap! tracker dissoc :capture-raf))
  (when (and (active? tracker) (input-since-install? tracker))
    (capture! tracker))
  (send! tracker))

;; ── Restore (§4 5) ─────────────────────────────────────────────────

(defn- apply-position!
  "Scroll `surface` to `pos`, or to the top when `pos` is nil or names no block
   (§4 5.6)."
  [tracker pos]
  (let [{:keys [surface install]} @tracker
        node (:node install)
        sc (scroller surface node)
        {:keys [n top-of height-of text-of]} (block-view tracker surface node sc)
        anchor (:anchor pos)
        i (when anchor (ra/resolve-index* n text-of anchor))]
    (set! (.-scrollTop sc)
      (if i (ra/target-scroll-top* i (top-of i) (height-of i) (:frac anchor)) 0))))

(defn- settle!
  "§4 5.5: apply `pos` now, and again whenever the content's height or block
   count changes, until `settle-ms` pass or the reader gives input. A frame
   poll rather than a ResizeObserver, because a scroller's own box does not
   change size when its content grows."
  [tracker pos]
  (cancel-settle! tracker)
  (let [started (js/performance.now)
        seq-now (get-in @tracker [:install :seq])
        signature (fn []
                    (let [{:keys [surface install]} @tracker
                          node (:node install)
                          sc (scroller surface node)]
                      [(.-scrollHeight sc) (.-clientHeight sc)
                       (if (= "poem" surface)
                         (.-length (.querySelectorAll node ".poem-verse"))
                         (.-childElementCount node))]))
        step (fn step [last-sig]
               (swap! tracker dissoc :settle-raf)
               (when (and (active? tracker)
                       (= seq-now (get-in @tracker [:install :seq]))
                       (not (input-since-install? tracker))
                       (< (- (js/performance.now) started) settle-ms))
                 (let [sig (signature)]
                   (when (not= sig last-sig) (apply-position! tracker pos))
                   (swap! tracker assoc :settle-raf
                     (js/requestAnimationFrame #(step sig))))))]
    (step nil)))

(defn- maybe-restore!
  "§4 5.2, 5.4, 5.7, 5.8: restore once per install of the tracker's own topic,
   when a position is known."
  [tracker]
  (let [{:keys [topic-id surface install stored restored-seq]} @tracker]
    (when (and install (not= restored-seq (:seq install)) (active? tracker))
      (let [mem (get @!latest [topic-id surface])
            done! #(swap! tracker assoc :restored-seq (:seq install))]
        (cond
          (input-since-install? tracker) (done!)
          (:fragment? install) (done!)
          mem (do (done!) (settle! tracker mem))
          (= ::pending stored) nil
          :else (do (done!) (settle! tracker stored)))))))

(defn- on-install! [tracker install]
  (cancel-settle! tracker)
  (swap! tracker assoc :install install)
  (maybe-restore! tracker))

;; ── Document listeners (§4 3.4, 4.4) ───────────────────────────────

(defn- on-scroll [^js e]
  (let [target (.-target e)]
    (doseq [t @!live]
      (let [{:keys [surface install]} @t]
        (when (and install (own-scroller? surface (:node install) target)
                (active? t) (input-since-install? t))
          (schedule-capture! t))))))

(defn- on-input [^js e]
  (let [target (.-target e)
        doc-level? (or (identical? target js/document)
                     (identical? target (.-body js/document))
                     (identical? target (.-documentElement js/document)))]
    (doseq [t @!live]
      (let [{:keys [surface install]} @t]
        (when (and install
                (or doc-level?
                  (when-let [region (and (instance? js/Node target)
                                      (input-region surface (:node install)))]
                    (.contains region target))))
          (swap! t assoc :input-seq (:seq install))
          (cancel-settle! t))))))

(defn- flush-all! [] (run! flush! @!live))

(defonce ^:private listeners-installed?
  (do
    (.addEventListener js/document "scroll" on-scroll #js {:capture true :passive true})
    (doseq [type ["wheel" "touchstart" "keydown" "pointerdown"]]
      (.addEventListener js/document type on-input #js {:capture true :passive true}))
    (.addEventListener js/window "pagehide" flush-all!)
    (.addEventListener js/document "visibilitychange"
      (fn [] (when (= "hidden" (.-visibilityState js/document)) (flush-all!))))
    true))

;; ── Public API ─────────────────────────────────────────────────────

(defn installed!
  "§4 3.6: `surface` now holds `topic-id`'s content in `node`.
   Pre : called by the plain fn that wrote the content, after the write.
         `node` is `.ql-editor` (editor), `.poem-reader` (poem), the rendered
         `div` (code-rendered) or the source `code` element (code-source).
   Post: this install is the surface's last. The tracker of `topic-id` on
         `surface`, when one is live, restores against it; every other
         tracker on `surface` is inactive until its own topic installs again.
         With `fragment?`, a link fragment owns this install's scroll and no
         restore runs for it (§4 5.3)."
  ([surface topic-id node] (installed! surface topic-id node false))
  ([surface topic-id node fragment?]
   (when (and node topic-id)
     (let [install {:topic-id topic-id :node node :fragment? (boolean fragment?)
                    :seq (swap! !install-seq inc)}]
       (swap! !installs assoc surface install)
       (doseq [t @!live]
         (when (and (= surface (:surface @t)) (= topic-id (:topic-id @t)))
           (on-install! t install)))))
   nil))

(defn tracker!
  "§4 3.1: a tracker for `topic-id` on `surface`. Both are fixed for its life.
   Post: registered live. When the surface's last install already names
         `topic-id`, the tracker takes it, so a frame that mounts after its
         content is written still restores."
  [topic-id surface]
  (let [t (atom {:topic-id topic-id :surface surface :stored ::pending})
        last-install (get @!installs surface)]
    (swap! !live conj t)
    (when (and listeners-installed?
            (= topic-id (:topic-id last-install))
            (.-isConnected ^js (:node last-install)))
      (on-install! t last-install))
    t))

(defn stored!
  "§4 3.5.2: the server's position for the tracker, or nil when it has none."
  [tracker pos]
  (swap! tracker assoc :stored pos)
  (maybe-restore! tracker)
  nil)

(defn dispose!
  "§4 4.8: stop the tracker, then send what it holds."
  [tracker]
  (swap! !live disj tracker)
  (cancel-settle! tracker)
  (flush! tracker)
  nil)
