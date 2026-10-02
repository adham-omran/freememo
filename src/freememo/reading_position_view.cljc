(ns freememo.reading-position-view
  "The Electric half of reading-position memory
   (plans/reading-position-memory.md §4 3.5).

   Its own namespace, because Electric inlines same-namespace e/defn calls into
   the caller's method, and every pane that mounts `ReadingPosition` is already
   near the 64KB cap.

   The wrappers below exist so a reactive body names a var that resolves on
   BOTH peers. `freememo.reading-position` is a `.cljs` file, so its vars exist
   on the client only, and a bare call to one from an e/defn body would leave
   the CLJ peer without the symbol (CLAUDE.md, server-only functions)."
  (:require
   [hyperfiddle.electric3 :as e]
   [freememo.doc-context :as dctx]
   #?(:cljs [freememo.reading-position :as rp])
   #?(:clj [freememo.db :as db])))

(defn get-reading-position*
  "The server's row for `topic-id` on `surface`, or nil.
   Post: {:anchor {:i :fp :frac} :fraction} as plain data (§4 1.5). The topic
         id is not echoed: the keyed frame in `ReadingPosition` is what keeps a
         value for one topic from reaching another's tracker (§4 3.5.3)."
  [user-id topic-id surface]
  #?(:clj (db/get-reading-position user-id topic-id surface)
     :cljs nil))

(defn installed!
  "`rp/installed!` for a writer whose write is an Electric render rather than a
   plain fn — the poem reader is the one."
  [surface topic-id node]
  #?(:cljs (rp/installed! surface topic-id node)
     :clj nil))

(defn- tracker! [topic-id surface]
  #?(:cljs (rp/tracker! topic-id surface)
     :clj nil))

(defn- stored! [tracker pos]
  #?(:cljs (rp/stored! tracker pos)
     :clj nil))

(defn- dispose! [tracker]
  #?(:cljs (rp/dispose! tracker)
     :clj nil))

(e/defn ReadingPosition
  "Keep the reader's place in `surface` of `topic-id` (§4 3.5).
   Pre : `surface` is one of `reading-anchor/surfaces`; the pane that mounts
         this reports its installs through `rp/installed!` (§4 3.6).
   Post: one tracker per [topic-id surface]. A change of either is an honest
         unmount and mount, so the old tracker flushes under its own topic
         and the new frame's server read starts empty instead of latching the
         old topic's row (§4 3.5.1, 3.5.3). Renders nothing."
  [surface topic-id]
  (e/client
    (let [user-id dctx/user-id]
      (e/for-by identity [k (if topic-id [[topic-id surface]] [])]
        (let [[tid surf] k
              tr (tracker! tid surf)
              stored (e/server (e/Offload #(get-reading-position* user-id tid surf)))]
          (e/on-unmount #(dispose! tr))
          (stored! tr stored))))))
