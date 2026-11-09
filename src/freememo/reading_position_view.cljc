(ns freememo.reading-position-view
  "The Electric half of reading-position memory
   (plans/reading-position-memory.md §4 3.5).

   Its own namespace, because Electric inlines same-namespace e/defn calls into
   the caller's method, and every pane that mounts `ReadingPosition` is already
   near the 64KB cap.

   The wrappers below exist so a reactive body names a var that resolves on
   BOTH peers. `freememo.reading-position` is a `.cljs` file, so its vars exist
   on the client only, and a bare call to one from an e/defn body would leave
   the CLJ peer without the symbol (docs/electric-patterns.md, server-only functions)."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.doc-context :as dctx]
   #?(:cljs [freememo.reading-position :as rp])
   #?(:clj [freememo.db :as db])))

(defn get-reading-position*
  "The server's row for `topic-id` on `surface`, tagged with the key it was
   read for.
   Post: {:key [topic-id surface] :pos pos}, where `pos` is
         {:anchor {:i :fp :frac} :fraction} as plain data (§4 1.5), or nil when
         there is no row or no topic. A nil topic reads nothing. The key is
         what keeps a row of one topic from reaching another topic's tracker
         (§4 3.5.3), because `ReadingPosition` keeps one frame across topics."
  [user-id topic-id surface]
  #?(:clj {:key [topic-id surface]
           :pos (when topic-id (db/get-reading-position user-id topic-id surface))}
     :cljs nil))

(defn own-row?
  "True when `row` (a `get-reading-position*` answer) was read for
   `tracker-key` ([topic-id surface]). nil, an answer still in flight, is
   nobody's row."
  [tracker-key row]
  (and (some? row) (= tracker-key (:key row))))

(defn installed!
  "`rp/installed!` for a writer whose write is an Electric render rather than a
   plain fn — the poem reader is the one."
  [surface topic-id node]
  #?(:cljs (rp/installed! surface topic-id node)
     :clj nil))

(defn- retarget!
  "Keep `!current` on the tracker for [topic-id surface].
   Post: when the key changed, the old tracker is disposed (it flushes under
         its own topic) and a new one is made, or none for a nil topic.
         Returns {:key [topic-id surface] :tracker t-or-nil}. An equal key
         returns the current value and changes nothing."
  [!current topic-id surface]
  #?(:cljs (let [k [topic-id surface]
                 cur @!current]
             (if (and cur (= k (:key cur)))
               cur
               (do (some-> (:tracker cur) rp/dispose!)
                   (reset! !current {:key k
                                     :tracker (when topic-id (rp/tracker! topic-id surface))}))))
     :clj nil))

(defn- stored!
  "Hand `row` to the current tracker, only when it was read for that
   tracker's key (§4 3.5.2, 3.5.3)."
  [current row]
  #?(:cljs (when (and (:tracker current) (own-row? (:key current) row))
             (rp/stored! (:tracker current) (:pos row)))
     :clj nil))

(defn- dispose! [!current]
  #?(:cljs (do (some-> (:tracker @!current) rp/dispose!)
               (reset! !current nil))
     :clj nil))

(e/defn ReadingPosition
  "Keep the reader's place in `surface` of `topic-id` (§4 3.5).
   Pre : `surface` is one of `reading-anchor/surfaces`; the pane that mounts
         this reports its installs through `rp/installed!` (§4 3.6).
   Post: one tracker per [topic-id surface]. A change of either disposes the
         old tracker, so it flushes under its own topic, and makes a new one
         whose stored row starts pending. A server row reaches only the
         tracker of the key it was read for, so the old topic's row never
         latches onto the new one (§4 3.5.1, 3.5.3). Renders nothing.
   The frame stays mounted across a topic change. A Learn Session Next does
   not build it again (plans/learn-next-speed/issues/05)."
  [surface topic-id]
  (e/client
    (let [user-id dctx/user-id
          !current (atom nil)
          current (retarget! !current topic-id surface)
          ;; A client e/as-vec: a new thunk on each topic, so the bare read
          ;; goes empty while it runs (docs/electric-patterns.md, "An Empty
          ;; `e/Offload` Empties Everything Downstream"). nil is no answer yet.
          row (peek (e/as-vec
                      (e/server (e/Offload #(get-reading-position* user-id topic-id surface)))))]
      (e/on-unmount #(dispose! !current))
      (stored! current row))))
