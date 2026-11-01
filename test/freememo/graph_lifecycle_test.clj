(ns freememo.graph-lifecycle-test
  (:require [clojure.test :refer [deftest is]]
            [freememo.graph-lifecycle :as lifecycle]))

(deftest a-late-vendor-callback-cannot-create-an-abandoned-renderer
  (let [ctx (atom nil) ready (atom nil) initialized (atom 0) canceled (atom 0)
        cancel! (lifecycle/start! ctx #(do (reset! ready %) (fn [] (swap! canceled inc)))
                  #(swap! initialized inc) (fn [_]))]
    (cancel!)
    (@ready)
    (cancel!)
    (is (zero? @initialized))
    (is (= 1 @canceled))
    (is (nil? @ctx))))

(deftest teardown-destroys-only-its-own-renderer
  (let [ctx (atom nil) ready (atom nil) destroyed (atom [])
        old (Object.) replacement (Object.)
        cancel! (lifecycle/start! ctx #(do (reset! ready %) (fn []))
                  (constantly old) #(swap! destroyed conj %))]
    (@ready)
    (is (identical? old @ctx))
    (reset! ctx replacement)
    (cancel!)
    (cancel!)
    (is (= [old] @destroyed))
    (is (identical? replacement @ctx))))

(deftest duplicate-ready-callbacks-do-not-create-two-renderers
  (let [ctx (atom nil) ready (atom nil) initialized (atom 0)
        cancel! (lifecycle/start! ctx #(do (reset! ready %) (fn []))
                  #(do (swap! initialized inc) (Object.)) (fn [_]))]
    (@ready)
    (@ready)
    (is (= 1 @initialized))
    (cancel!)
    (is (nil? @ctx))))

(deftest teardown-during-initialization-destroys-the-result
  (let [ctx (atom nil) ready (atom nil) dispose (atom nil) destroyed (atom [])
        renderer (Object.)
        cancel! (lifecycle/start! ctx #(do (reset! ready %) (fn []))
                  #(do (@dispose) renderer) #(swap! destroyed conj %))]
    (reset! dispose cancel!)
    (@ready)
    (is (= [renderer] @destroyed))
    (is (nil? @ctx))))
