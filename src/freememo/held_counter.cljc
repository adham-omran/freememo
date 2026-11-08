(ns freememo.held-counter
  "Hold a value while a kept page is hidden. A kept page stays mounted behind
   another tab (plans/low-end-tablet-performance/issues/17-keep-library-alive.md).
   Its server watches of a user-state counter must not query again for each
   review that runs behind it, and its sub-route must not follow the route of
   the visible tab.

   `shown?` is an ambient value, bound by the kept page. An e/fn that closes
   over a changing value is a new e/fn on every change, so a region would
   remount its loaded body on every hide and show. A read of the ambient var
   does not do that."
  (:require
   [hyperfiddle.electric3 :as e]
   #?(:clj [freememo.user-state :as us])))

(e/declare shown?)

(defn held-value
  "The value a kept page reads: `live` while shown, else the last value it read.
   A plain function, so a flip of `shown?` that keeps the value equal emits
   nothing downstream.
   Pre : held is ::none before the first read.
   Post: live when shown? or when nothing is held yet, else held."
  [shown? held live]
  (if (or shown? (= ::none held)) live held))

(e/defn HeldCounter
  "The user-state counter `k` of `user-id`, held while `shown?` is false.
   Site: server."
  [user-id k]
  (e/server
    (let [live (e/watch (us/get-atom user-id k))]
      (e/with-cycle [held ::none]
        (held-value shown? held live)))))
