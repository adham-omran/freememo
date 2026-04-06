(ns hyperfiddle.router5 ; Copied from Hyperfiddle monorepo, modified for FreeMemo:
  ;; - Removed dustingetz.sexpr-router, hyperfiddle.ednish, dustingetz.data deps
  ;; - Inlined pad function
  ;; - Custom simple path encode/decode for FreeMemo's route shapes
  "A reactive tree router. navigation and replacestate are synchronous. Doesn't support navigation cancellation."
  (:refer-clojure :exclude [set pop])
  (:require
   [clojure.core :as cc]
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e :refer [$]]
   [hyperfiddle.electric3-contrib :as ex]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.token-zoo0 :refer [TokenNofail]]
   [hyperfiddle.rcf :refer [tests]]
   [hyperfiddle.history4 :as h]
   [missionary.core :as m])
  #?(:cljs (:import [hyperfiddle.history4.HTML5History]))
  #?(:cljs (:require-macros hyperfiddle.router5)))

;;; Inlined from dustingetz.data (pad only)
(defn- pad* [zero coll] (concat coll (repeat zero)))
(defn- pad-to [n coll] (vec (take (max n (count coll)) (pad* nil coll))))

;;; Minimal Lenses implementation

(defn lens [getter-f setter-f]
  (fn [next-step-f]
    (fn ([value] (next-step-f (getter-f value)))
      ([value f] (setter-f value (fn [value] (next-step-f value f)))))))

(defn view [lens structure] ((lens identity) structure))
(defn identity-setter [structure f] (f structure))
(defn over [lens f structure] ((lens identity-setter) structure f))
(defn set [lens value structure] (over lens (constantly value) structure))
(def id-lens (lens identity identity-setter))

(defn key-lens [key]
  (lens
    (fn [structure] (get structure key))
    (fn [structure f] (update structure key f))))

(defn rest-lens
  "Return a lens focusing on and setting the rest of a seq (cdr)."
  []
  (lens rest (fn [value f] (cons (first value) (f (rest value))))))

(defn adaptive-key
  [key]
  (if (= ::rest key)
    (rest-lens)
    (lens
      (fn [value] ((if (seq? value) nth get) value key nil))
      (fn [value f]
        (cond
          (map? value)    (update value key f)
          (vector? value) (update (pad-to key value) key f)
          (seq? value)    (seq (update (pad-to key value) key f))
          (nil? value)    (if (nat-int? key)
                            (update (pad-to key []) key f)
                            {key (f nil)})
          :else           (if (nat-int? key)
                            (update (pad-to key [value]) key f)
                            {value (f nil)}))))))

(defn path-lens
  "Given a sequence of keys, return a lens focusing on the value at the end of the path."
  [path]
  (cond
    (nil? path)    id-lens
    (vector? path) (reduce comp (map adaptive-key path))
    :else          (adaptive-key path)))

;;; Relative and absolute paths

(defn safe-pop [coll] (if (empty? coll) coll (cc/pop coll)))

(defn resolve-path
  ([path]
   (if (seq path)
     (resolve-path [] path)
     []))
  ([stack [x & xs]]
   (if (seq xs)
     (case x
       .  (resolve-path stack xs)
       .. (resolve-path (safe-pop stack) xs)
       /  (resolve-path [] xs)
       (resolve-path (conj stack x) xs))
     (case x
       .  stack
       .. (safe-pop stack)
       /  []
       (conj stack x)))))

;;; human-friendly representation

(defn simplify [route]
  (cond
    (nil? route)                      ()
    (and (map? route) (empty? route)) ()
    (and (map? route) (= 1 (count route)))
    (let [k (key (first route))
          head (if k [k] [])
          child-route (simplify (val (first route)))]
      (if ((some-fn seq? vector?) child-route)
        (concat head child-route)
        (list k child-route)))
    (map? route) route
    ((some-fn seq? vector? set?) route) route
    :else (vector route)))

(defn normalize [x]
  (cond
    (nil? x) nil
    (map? x) (not-empty (update-vals x (fn [x]
                                         (if (seq? x)
                                           {x nil}
                                           (normalize x)))))
    (seq? x) (cond
               (empty? x)       nil
               (map? (first x)) (normalize (first x))
               :else            {(first x) (normalize (next x))})
    :else    {x nil}))

;;; URL encoding — simple path-based for FreeMemo routes
;; Routes are flat lists: (library), (viewer browse-pdf 42), etc.
;; URLs: /library, /viewer/browse-pdf/42, etc.

(defn encode* [route]
  (let [s (simplify route)]
    (if (or (nil? s) (and (seqable? s) (empty? s)))
      "/"
      (str "/" (str/join "/" (map (fn [x]
                                   (cond
                                     (symbol? x) (str x)
                                     (keyword? x) (subs (str x) 1)
                                     :else (str x)))
                            s))))))

(defn decode* [path]
  (when (and (string? path) (not= path "/"))
    (let [segments (->> (str/split (str/replace-first path #"^/+" "") #"/")
                        (remove str/blank?))]
      (when (seq segments)
        (map (fn [s]
               (if-let [n (parse-long s)]
                 n
                 (symbol s)))
          segments)))))

;;; Electric

(e/declare ^{:doc "The router base path."} basis)
(e/declare ^{:doc "A stack of paths"} paths)
(e/declare ^{:doc "The current path"} path)
(e/declare ^{:doc "Top level route"} root-route)
(e/declare ^{:doc "Current route in the scope of a router"} route)

(e/declare ^:dynamic *encode*)
(e/declare ^:dynamic *decode*)

(defn current-path [paths] (into [] cat paths))

(defn as-vec [x] (if (vector? x) x [x]))

(e/defn Focus [path Body-fn]
  (let [paths (conj hyperfiddle.router5/paths (as-vec path))
        path (resolve-path (current-path paths))]
    (binding [hyperfiddle.router5/paths paths
              hyperfiddle.router5/path  path
              route (view (path-lens path) root-route)]
      ($ Body-fn))))

(defmacro focus [path & body] `($ Focus ~path (e/fn [] ~@body)))
(defmacro pop [& body] `(focus [::rest] ~@body))

(defn split-link-path [path]
  (cond (not (vector? path)) [['.] path]
        (= 1 (count path))   [['.] (first path)]
        :else                [(vec (butlast path)) (last path)]))

(e/defn Route-for
  ([path] (e/apply Route-for (split-link-path path)))
  ([path value]
   (over (path-lens (resolve-path (into hyperfiddle.router5/path path))) (constantly value) root-route)))

(e/defn Route-at [path]
  (view (path-lens (resolve-path (into hyperfiddle.router5/path path))) root-route))

(e/defn Current-route? [target-route]
  (= (resolve-path (current-path paths))
    (resolve-path (current-path (conj paths (as-vec target-route))))))

;;; Link

#?(:cljs
   (defn on-link-click [next-route ^js node ^js e]
     (let [target (.getAttribute node "target")]
       (set! (.-hyperfiddle_router_route e) next-route)
       (set! (.-hyperfiddle_router_external_nav e)
         (or (some? (.getAttribute node "download"))
           (and (some? target) (not= "_self" target))
           (.-ctrlKey e)
           (.-metaKey e)))
       nil)))

#?(:cljs
   (defn link-click-handler [node next-route]
     (m/reductions {} nil
       (dom/listen-some node "click"
         (fn [e]
           (on-link-click next-route node e))))))

(e/declare current-route?)

(defn normalize-route-value [x]
  (if (seq? x)
    (normalize {x nil})
    (normalize x)))

#?(:cljs (defn add-document-basis [basis path] (str basis (str/replace path #"^/" ""))))

(e/defn Link [path Body]
  (let [path&value (e/client (split-link-path path))
        path' (e/client (first path&value)), value (e/client (second path&value))]
    (dom/a
      (e/client (e/input (link-click-handler dom/node (into hyperfiddle.router5/path path))))
      (e/client (dom/props {::dom/href (add-document-basis basis (*encode* ($ Route-for path' value)))}))
      (binding [current-route? (e/client ($ Current-route? path'))]
        ($ Body)))))

(defmacro link [path & body]
  `($ Link ~path (e/fn [] ~@body)))

;;; History integration

#?(:cljs
   (defn- internal-nav-intent? [^js e]
     (and (some? (.-hyperfiddle_router_route e))
       (not (.-hyperfiddle_router_external_nav e)))))

#?(:cljs
   (defn -get-event-route [^js event]
     (.-hyperfiddle_router_route ^js event)))

#?(:cljs (defn as-directory [path] (-> path (str/replace #"/+$" "") (str "/"))))
#?(:cljs (defn path-name [url-str] (-> (new js/URL url-str) (.-pathname) (str/replace #"/[^\/]+$" ""))))

#?(:cljs
   (defn document-basis [current-node]
     (if (some? (.querySelector js/document "base"))
       (-> current-node .-baseURI path-name as-directory)
       "/")))

#?(:cljs
   (defn strip-document-basis [basis path]
     (if (str/starts-with? path basis)
       (str/replace-first path basis "/")
       path)))

(e/defn OnNavigate
  ([Callback] (e/client ($ OnNavigate (.-document js/window) Callback)))
  ([node Callback]
   (e/client
     (when-let [^js event (dom/On node "click" #(when (internal-nav-intent? %) (.preventDefault %) %) nil nil)]
       ($ Callback (-get-event-route event) event)))))

(e/defn Navigate!
  ([path]
   (h/navigate! h/history (add-document-basis basis (*encode* ($ Route-for path)))))
  ([path delay-ms]
   (case (e/Task (m/sleep delay-ms))
     (Navigate! path))))

(e/defn ReplaceState! [path]
  (e/client (h/replace-state! h/history (add-document-basis basis (*encode* ($ Route-for path))))))

(e/defn ReplaceState2!
  ([value] (ReplaceState2! ['.] value))
  ([path value] (ReplaceState! (conj path value))))

#?(:node nil
   :default
   (e/defn HTML5-History []
     (e/client
       (let [!history (h/html5-history)]
         (dom/On js/window "popstate" #(h/set-history! !history (h/html5-path) (.-timeStamp %)) nil nil)
         !history))))

(e/defn ForAll
  [x F]
  (e/for [x (e/diff-by (fn [_] (random-uuid)) (e/as-vec x))]
    (F x)))

(e/defn Router [history BodyFn]
  (binding [basis (e/Reconcile (or basis (document-basis dom/node)))
            *encode* (#(or *encode* encode*))
            *decode* (#(or *decode* decode*))]
    (binding [h/history  history
              root-route (*decode* (strip-document-basis basis (e/watch history)))
              path       []
              paths      []]
      ($ OnNavigate dom/node (e/fn [route e] (ForAll e (e/fn [_e] (Navigate! route)))))
      (focus '/
        ($ BodyFn)))))

(defmacro router [history & body]
  `($ Router ~history (e/fn [] ~@body)))
