(ns freememo.search-page
  "Search tab — global content search with fuzzy/exact modes.
   Mirrors Library's filter UI; results are server-queried via pg_trgm."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [hyperfiddle.router5 :as r]
   [contrib.data :refer [clamp-left]]
   [clojure.string :as str]
   [freememo.navigation :as nav]
   #?(:clj [freememo.search :as search])))

;; Server wrapper — plain defn with reader conditional, so :refer works from cljs
(defn run-search* [user-id query mode kind-filter]
  #?(:clj (search/search-content user-id query mode kind-filter)
     :cljs []))

(def ^:private kind-options
  [["all" "All kinds"]
   ["topics" "Topics"]
   ["pages" "PDF pages"]
   ["articles" "Articles"]
   ["epub" "EPUB"]
   ["markdown" "Markdown"]])

(defn kind-badge [kind]
  (case kind
    "pdf" ["PDF" "var(--color-badge-pdf)"]
    "page" ["Page" "var(--color-badge-pdf)"]
    "epub" ["EPUB" "var(--color-badge-epub)"]
    ("web" "wikipedia") ["Web" "var(--color-badge-web)"]
    "markdown" ["MD" "var(--color-badge-web)"]
    ["Topic" "var(--color-badge-epub)"]))

(defn encode-query [q]
  #?(:cljs (js/encodeURIComponent (or q ""))
     :clj nil))

(defn decode-query [q-sym]
  #?(:cljs (when q-sym
             (try (js/decodeURIComponent (str q-sym))
               (catch :default _ (str q-sym))))
     :clj nil))

(defn update-url! [mode kind q]
  #?(:cljs
     (let [path (if (or (nil? q) (= "" q))
                  "/search"
                  (str "/search/" (name mode) "/" kind "/" (encode-query q)))]
       (.replaceState js/window.history nil "" path)
       nil)
     :clj nil))

(defn snippet-center!
  "Scroll the snippet div so its first <mark> descendant is visually centered.
   Direction-neutral (works for LTR and RTL content) via getBoundingClientRect.
   Deferred via requestAnimationFrame so layout is settled. No-op when no
   <mark> is present or the div hasn't been laid out."
  [div]
  #?(:cljs
     (js/requestAnimationFrame
       (fn []
         (when-let [mark (.querySelector div "mark")]
           (let [mr (.getBoundingClientRect mark)
                 dr (.getBoundingClientRect div)
                 mark-center (+ (.-left mr) (/ (.-width mr) 2))
                 div-center (+ (.-left dr) (/ (.-width dr) 2))
                 delta (- mark-center div-center)]
             (.scrollBy div delta 0)))))
     :clj nil))

(defn click-nav! [navigate! row]
  (let [id (:id row)
        kind (:kind row)]
    (case kind
      "pdf" (navigate! :viewer (nav/nav-browse-pdf id nil :search))
      "page" (navigate! :viewer (nav/nav-browse-pdf (:parent-id row) (:page-number row) :search))
      ;; basic, markdown, web, wikipedia, epub, anything else
      (navigate! :viewer (nav/nav-browse-topic id :search)))))

(e/defn ResultRow [row i row-height navigate!]
  (e/client
    (let [{:keys [title kind snippet source-title]} row
          [badge-text badge-color] (kind-badge kind)]
      (dom/tr
        (dom/props {:style {:border-bottom "1px solid var(--color-bg-subtle)"
                            :height (str row-height "px")
                            :cursor "pointer"
                            :--order (inc i)}})
        (dom/On "click" (fn [_] (click-nav! navigate! row)) nil)
        ;; Column 1: kind badge + title
        (dom/td
          (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                              :padding "4px 10px" :overflow "hidden"}})
          (dom/span
            (dom/props {:class "type-badge" :style {:background badge-color}})
            (dom/text badge-text))
          (dom/span
            (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                :font-size "13px" :font-weight "500"
                                :color "var(--color-text-primary)"}
                        :data-tooltip (or title "(untitled)")})
            (dom/text (or title "(untitled)"))))
        ;; Column 2: source (root topic's title)
        (dom/td
          (dom/props {:style {:padding "4px 10px" :overflow "hidden"
                              :display "flex" :align-items "center"
                              :font-size "12px" :color "var(--color-text-secondary)"
                              :white-space "nowrap" :text-overflow "ellipsis"}})
          (dom/span
            (dom/props {:style {:overflow "hidden" :text-overflow "ellipsis" :white-space "nowrap"
                                :width "100%"}
                        :data-tooltip (or source-title "")})
            (dom/text (or source-title ""))))
        ;; Column 3: snippet (HTML with <mark>; scroll-centered on mark)
        (dom/td
          (dom/props {:style {:padding "4px 10px" :overflow "hidden"
                              :display "flex" :align-items "center"
                              :font-size "12px" :color "var(--color-text-secondary)"
                              :white-space "nowrap"}})
          (dom/div
            (dom/props {:dir "auto"
                        :style {:overflow "hidden" :white-space "nowrap"
                                :width "100%"}})
            (set! (.-innerHTML dom/node) (or snippet ""))
            (snippet-center! dom/node)))))))

(e/defn SearchResultsTable [results navigate!]
  (e/client
    (let [row-count (count results)
          row-height 36
          grid-cols "minmax(160px, 1fr) minmax(120px, 0.7fr) 2.5fr"]
      (dom/div
        (dom/props {:style {:flex "1" :display "flex" :flex-direction "column" :min-height "0"}})

        ;; Header
        (dom/table
          (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :flex-shrink "0"}})
          (dom/thead
            (dom/props {:style {:display "contents"}})
            (dom/tr
              (dom/props {:style {:display "contents"}})
              (let [th-style {:padding "8px 10px" :border-bottom "2px solid var(--color-border)"
                              :font-weight "600" :font-size "13px"
                              :color "var(--color-text-primary)" :text-align "left"}]
                (dom/th (dom/props {:style th-style}) (dom/text "Document"))
                (dom/th (dom/props {:style th-style}) (dom/text "Source"))
                (dom/th (dom/props {:style th-style}) (dom/text "Snippet"))))))

        ;; Scrollable body
        (dom/div
          (dom/props {:style {:flex "1" :overflow-y "auto" :min-height "0" :scrollbar-gutter "stable"}})
          (let [[offset limit] (Scroll-window row-height row-count dom/node {:overquery-factor 1})
                occluded-height (clamp-left (* row-height (- row-count limit)) 0)]
            (dom/props {:class "tape-scroll"
                        :style {:--offset offset :--row-height (str row-height "px")}})
            (dom/table
              (dom/props {:style {:width "100%" :display "grid" :grid-template-columns grid-cols :font-size "13px"}})
              (e/for [i (Tape offset limit)]
                (let [row (nth results i nil)]
                  (when row
                    (ResultRow row i row-height navigate!)))))
            (dom/div (dom/props {:style {:height (str occluded-height "px")}}))))))))

(e/defn SearchPage [user-id navigate!]
  (e/client
    (let [;; Seed initial state from URL segments (post r/pop in main.cljc)
          [mode-sym kind-sym q-sym] r/route
          initial-mode (if mode-sym (keyword (str mode-sym)) :fuzzy)
          initial-kind (if kind-sym (str kind-sym) "all")
          initial-q (or (decode-query q-sym) "")

          !query (atom initial-q)
          query (e/watch !query)
          !mode (atom initial-mode)
          mode (e/watch !mode)
          !kind (atom initial-kind)
          kind (e/watch !kind)

          !last-results (atom [])
          last-results (e/watch !last-results)

          ;; URL sync — side effect during binding evaluation
          url-synced (do (update-url! mode kind query) true)

          ;; Derived
          q-trimmed (str/trim (or query ""))
          valid? (>= (count q-trimmed) 2)

          ;; Server search (e/Offload latest-wins cancel)
          results (when valid?
                    (e/server (e/Offload #(run-search* user-id q-trimmed mode kind))))

          loading? (and valid? (nil? results))

          ;; Stale-while-revalidate: only update !last-results on non-nil results
          swr-state (cond
                      (not valid?) (do (reset! !last-results []) :cleared)
                      (some? results) (do (reset! !last-results results) :updated)
                      :else :pending)]

      ;; Reference bindings so Electric doesn't optimize their side effects away
      (when url-synced nil)
      (when swr-state nil)

      (dom/div
        (dom/props {:class "page-container"
                    :style {:height "100%" :display "flex" :flex-direction "column"}})

        ;; Filter row
        (dom/div
          (dom/props {:style {:display "flex" :align-items "center" :gap "12px"
                              :margin-bottom "12px" :flex-wrap "wrap"}})

          (dom/input
            (dom/props {:type "text" :placeholder "Search all content..."
                        :class "input" :style {:flex "1" :min-width "200px"}})
            (set! (.-value dom/node) query)
            (dom/On "input" (fn [e] (reset! !query (-> e .-target .-value))) nil))

          (dom/select
            (dom/props {:class "input"})
            (dom/option (dom/props {:value "fuzzy"}) (dom/text "Fuzzy"))
            (dom/option (dom/props {:value "exact"}) (dom/text "Exact"))
            (set! (.-value dom/node) (name mode))
            (dom/On "change" (fn [e] (reset! !mode (keyword (-> e .-target .-value)))) nil))

          (dom/select
            (dom/props {:class "input"})
            (e/for [[v label] (e/diff-by first kind-options)]
              (dom/option (dom/props {:value v}) (dom/text label)))
            (set! (.-value dom/node) kind)
            (dom/On "change" (fn [e] (reset! !kind (-> e .-target .-value))) nil))

          (when loading?
            (dom/span
              (dom/props {:style {:display "inline-flex" :align-items "center"
                                  :color "var(--color-text-secondary)" :font-size "13px"}})
              (dom/span (dom/props {:class "spinner"}))
              (dom/text "Searching..."))))

        ;; Results area
        (cond
          (not valid?)
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :align-items "center" :justify-content "center"
                                :color "var(--color-text-secondary)" :font-size "13px"}})
            (dom/text "Type at least 2 characters to search"))

          (and (empty? last-results) (not loading?))
          (dom/div
            (dom/props {:style {:flex "1" :display "flex" :align-items "center" :justify-content "center"
                                :color "var(--color-text-secondary)" :font-size "13px"}})
            (dom/text (str "No matches for \"" q-trimmed "\"")))

          :else
          (SearchResultsTable last-results navigate!))))))
