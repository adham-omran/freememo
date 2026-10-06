(ns freememo.walkthrough-canvas
  "The canvas and the rail of one walkthrough step
   (plans/commit-range-walkthrough.md §6.6.3, §6.6.4, §6.6.6).

   The canvas shows one file whole, at the step's commit. The server renders
   it (`walkthrough/canvas`): Markdown as the code pane renders it, each block
   marked by the change that wrote it, and code as one span per line with
   unchanged runs folded. Electric owns the canvas element but none of its
   children, which are written with `innerHTML`, so question pins work on it
   exactly as on the code pane (`question-pin-pane/PinLayer`).

   The rail lists the step's files with their change counts. A file the
   tutor cites lights up there, and a click swaps it into the canvas. Each
   file opens to its changes, and a change can move to another step
   (§6.5.7.1).

   Its own namespace for the 64KB method cap."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.forms-ext :as forms-ext]
   [freememo.math :as math]
   [freememo.question-pin-pane :as pin-pane]
   [freememo.question-pin-state :as pin-state]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.walkthrough :as wt])))

;; ── Server wrappers ────────────────────────────────────────────────

(defn canvas* [_rev user-id wid position path mode]
  #?(:clj (when (and wid position path) (wt/canvas user-id wid position path mode)) :cljs nil))

(defn move-hunk!* [user-id hunk-id concept-id]
  #?(:clj (wt/move-hunk! user-id hunk-id concept-id) :cljs nil))

;; ── Client helpers ─────────────────────────────────────────────────

(defn show-canvas!
  "Write the canvas `html` into `node`. Markdown is typeset as the code pane
   typesets it. Post: nil."
  [node html markdown?]
  #?(:cljs (if markdown?
             (math/set-html! node (or html ""))
             (set! (.-innerHTML node) (or html "")))
     :clj nil)
  nil)

(defn toggle-fold!
  "Open or close the folded run a click landed on. The run's text stays in
   the DOM either way, so no pin offset moves (§6.3.7)."
  [e]
  #?(:cljs (when-let [fold (some-> (.-target e) (.closest ".wt-fold"))]
             (when (identical? (.-target e) fold)
               (.toggle (.-classList fold) "wt-fold--open")))
     :clj nil)
  nil)

(defn- short-sha [sha] (subs (str sha) 0 (min 7 (count (str sha)))))

;; ── Canvas ─────────────────────────────────────────────────────────

(e/defn Canvas
  "The file at `path` in step `position`, colored for `mode`, and its pins.
   A read that fails, such as a forge that is down, shows its message in
   the notice instead of the file.
   Post: the canvas element, empty while the read is in flight."
  [user-id wid position path mode list-open? !list-open?]
  (e/client
    (let [rev (e/server (e/watch (us/get-atom user-id :walkthrough-mutations)))
          ;; SERVER-FORM binding: the HTML crosses once, through the read
          ;; below, and the rest crosses as short values.
          c (e/server (e/Offload #(canvas* rev user-id wid position path mode)))
          html (e/server (:html c))
          markdown? (e/server (boolean (:markdown? c)))
          spec (e/server (:ref-spec c))
          error (e/server (:error c))
          plain? (e/server (and (some? c) (nil? (:error c)) (not (:attributed? c))))
          [node _] (dom/div
                     (dom/props {:class "wt-canvas code-pane-rendered" :tabindex "0" :dir "ltr"
                                 :data-testid "walkthrough-canvas"})
                     (let [node dom/node]
                       (dom/On "click" (fn [e] (toggle-fold! e)) nil)
                       [node (show-canvas! node html markdown?)]))]
      (dom/props node {:data-kind (if markdown? "markdown" "code")})
      ;; One notice, always mounted, for both messages: a failed read and a
      ;; file shown plainly cannot happen together.
      (dom/div
        (dom/props {:class (if error "wt-canvas__notice wt-canvas__notice--error" "wt-canvas__notice")
                    :role (if error "alert" "status")
                    :data-testid "walkthrough-canvas-notice"})
        (dom/props {:style {:display (if (or plain? error) "block" "none")}})
        (dom/text (or error
                    (str "FreeMemo could not tell which change wrote each line of this "
                      "file, so it shows the file plainly."))))
      (when spec
        (pin-pane/PinLayer user-id spec node (if markdown? :blocks :source)
          list-open? !list-open?)))))

;; ── Rail ───────────────────────────────────────────────────────────

(e/defn HunkRow
  "One change of a rail file, with the step it belongs to (§6.5.7.1).
   A pick moves the change at once."
  [user-id hunk concept-id concept-options]
  (e/client
    (dom/li
      (dom/props {:class "wt-rail__hunk"})
      (dom/span
        (dom/props {:class "wt-rail__hunk-head" :dir "ltr"})
        (dom/text (str (short-sha (:commit-sha hunk)) ", +" (:added hunk) " −" (:removed hunk)
                    ", " (:header hunk))))
      (e/for [[t {:keys [concept]}] (forms-ext/Select! :concept (str concept-id) concept-options
                                       :class "input wt-rail__move"
                                       :aria-label "Move this change to another step")]
        (let [r (e/server (e/Offload #(move-hunk!* user-id (:id hunk) (parse-long concept))))]
          (case r (if (:ok r) (t) (t (:error r)))))))))

(e/defn RailFile
  "One file of the step: its path, its change counts, and its changes when
   open. The file the canvas shows is current; a file a tutor citation
   points at lights up (§6.6.4)."
  [user-id file current? lit? concept-id concept-options pick-path!]
  (e/client
    (let [!open? (atom false)
          open? (e/watch !open?)
          n (count (:hunks file))]
      (dom/li
        (dom/props {:class (str "wt-rail__file"
                             (when current? " wt-rail__file--current")
                             (when lit? " wt-rail__file--lit"))
                    :data-path (:path file)})
        (dom/button
          (dom/props {:type "button" :class "wt-rail__open" :dir "ltr"
                      :aria-current (if current? "true" "false")})
          (dom/span (dom/props {:class "wt-rail__path"}) (dom/text (:path file)))
          (dom/span (dom/props {:class "wt-rail__count"})
            (dom/text (str n " change" (when (not= 1 n) "s")
                        (when (:deleted? file) ", deleted"))))
          (dom/On "click" (fn [_] (pick-path! (:path file))) nil))
        (dom/button
          (dom/props {:type "button" :class "btn btn-sm btn-secondary wt-rail__toggle"
                      :aria-expanded (if open? "true" "false")
                      :aria-label (str "Show the changes to " (:path file))})
          (dom/text (if open? "Hide changes" "Changes"))
          (dom/On "click" (fn [_] (swap! !open? not)) nil))
        (dom/ul
          (dom/props {:class "wt-rail__hunks"})
          (when open?
            (e/for [hunk (e/diff-by :id (:hunks file))]
              (HunkRow user-id hunk concept-id concept-options))))))))

(e/defn Rail
  "The files of the step (§6.6.4). `files` is a differential collection the
   caller diffed by path on the server, so each row crosses once."
  [user-id repo-id files path concept-id concept-options pick-path!]
  (e/client
    (let [finger (e/watch pin-state/!finger)
          lit-path (let [[r p] (:source-doc finger)] (when (= r repo-id) p))]
      (dom/aside
        (dom/props {:class "wt-rail" :aria-label "Files of this step"})
        (dom/div (dom/props {:class "wt-rail__title"}) (dom/text "Files"))
        (dom/ul
          (dom/props {:class "wt-rail__files"})
          (e/for [file files]
            (RailFile user-id file (= path (:path file)) (= lit-path (:path file))
              concept-id concept-options pick-path!)))))))
