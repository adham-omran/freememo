(ns freememo.knowledge-scope
  "What the Knowledge views share outside the table: the document scope in the
   path, the filter and sort state in the query string, and the search box
   (plans/knowledge-tables-selection-and-bulk.md §3).

   The scope is a path segment (`/knowledge/questions/<doc-id>`), written by
   `ReflectScopeInUrl`. Every other filter, and the sort, is a query parameter,
   written with `util/set-query-params!` — `replaceState`, so typing in search
   adds no history entry. Parameter grammar matches library_cards: `q` for
   search, `sort` as `col:dir`, and a parameter omitted at its default."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])))

;; ── Query-string grammar ───────────────────────────────────────────

(defn parse-sort
  "`\"col:dir\"` → [col dir] when col is one of `cols` and dir is asc or desc.
   Anything else → `default`, so no URL can break the view."
  [s cols default]
  (let [[c d] (str/split (or s "") #":")
        col (keyword c)
        dir (keyword d)]
    (if (and (contains? cols col) (#{:asc :desc} dir))
      [col dir]
      default)))

(defn sort-param
  "The `sort` value for [col dir], or nil at the default (a clean URL) and for
   anything that is not a keyword pair."
  [[col dir] default]
  (when (and (keyword? col) (keyword? dir) (not= [col dir] default))
    (str (name col) ":" (name dir))))

(defn flag-param [on?] (when on? "1"))

(defn flag-set? [params k] (= "1" (get params k)))

;; ── Server reads ───────────────────────────────────────────────────

;; Body-level reader conditionals: each name is called inside an (e/server …)
;; form of another namespace, which CLJS compiles too.

(defn list-source-docs*
  "Lean [{:id :title}] of root documents — options for the source filters."
  [_kg-bump user-id]
  #?(:clj (mapv (fn [t] {:id (:topics/id t) :title (:topics/title t)})
            (db/get-root-topic-titles user-id))
     :cljs nil))

;; ── Components ─────────────────────────────────────────────────────

(e/defn UrlState
  "Write `params` into the query string. The write is this body's value, so
   Electric never skips it; it runs again whenever a parameter changes."
  [params]
  (e/client (util/set-query-params! params)))

(e/defn SearchInput
  "The view's search box. Uncontrolled: seeded once from `query`, then the user's
   typing writes `!query` after a 300 ms pause. Bumping `epoch` remounts it with
   the current `query`, which is how Clear filters empties it.
   Pre:  `!query` is the view's search atom and `query` its watched value."
  [placeholder query !query epoch]
  (e/client
    (e/for-by identity [_k [epoch]]
      (let [!timer (atom nil)]
        (dom/input
          (dom/props {:type "search" :placeholder placeholder
                      :aria-label (str/replace placeholder #"…$" "")
                      :class "input knowledge-search"})
          (set! (.-value dom/node) (e/snapshot query))
          (dom/On "input"
            (fn [ev] (util/debounce! !timer !query (.. ev -target -value) 300))
            nil))))))

(e/defn ReflectScopeInUrl
  "Write the source filter into the address bar. One way only: the route seeds the
   filter atom when the view mounts, and the atom drives the URL thereafter.

   Navigating updates `route-doc-id`, which makes the guard false and remounts the
   view against the new route — so this converges instead of looping.
   Pre:  `nav-type` is the sub-view's navigate! :type; `route-doc-id` is the id in
         the URL now (nil = unscoped).
   Post: the URL's doc-id equals `source-id` after any filter change, including a
         clear."
  [navigate! nav-type route-doc-id source-id]
  (e/client
    ;; A map, not the bare id: e/Token ignores nil emissions, and clearing the
    ;; filter to nil is precisely the transition that must still fire.
    (let [pending (when (not= source-id route-doc-id) {:doc-id source-id})
          [t _] (e/Token pending)]
      (when t
        (case (navigate! :knowledge {:type nav-type :doc-id (:doc-id pending)})
          (t))))))

(e/defn FilterToggle
  "A filter that is on or off, as a chip. `aria-pressed` carries the state."
  [label on? !on?]
  (e/client
    (dom/button
      (dom/props {:type "button" :aria-pressed (str (boolean on?))
                  :class (if on? "filter-chip filter-chip-active" "filter-chip")})
      (dom/text label)
      (dom/On "click" (fn [_] (swap! !on? not)) nil))))

(defn action
  "A new action map for a view's action atom. The fresh :id makes two clicks of
   the same action two distinct values, so the view's token fires for each."
  [kind ids bulk?]
  {:id (str (random-uuid)) :kind kind :ids (vec ids) :bulk? bulk?})
