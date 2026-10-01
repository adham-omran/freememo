(ns freememo.help-page
  "Help tab. Renders freememo.help-content.

   /help/<slug> scrolls to one section and marks it (plans/searchable-help.md
   2.2). An unknown slug shows the whole page with a notice (2.3). A query
   filters sections and steps (3.1) through help-content/filter-sections, the
   one matching rule."
  (:require
   [freememo.command-bus :as bus]
   [freememo.help-content :as help]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.router5 :as r]))

(def search-input-id "help-search-input")

(defn section-dom-id
  "The DOM id of section `slug`'s element, which scroll-to-section! finds."
  [slug]
  (str "help-" slug))

(defn scroll-to-section!
  "Scroll section `slug` into view, one frame after the render that mounted it.
   Pre : `slug` is nil or a string.
   Post: nil. No-op when `slug` is nil or its section is not in the DOM — an
         unknown slug, or a section the query hides."
  [slug]
  #?(:cljs (when slug
             (js/requestAnimationFrame
               (fn [_]
                 (when-let [node (.getElementById js/document (section-dom-id slug))]
                   (.scrollIntoView node #js {:block "start"})))))
     :clj nil)
  nil)

(defn clear-query!
  "Empty the search box and the query. The box is uncontrolled, so its value
   is cleared by hand. Post: @!query is \"\"."
  [!query]
  #?(:cljs (when-let [node (.getElementById js/document search-input-id)]
             (set! (.-value node) ""))
     :clj nil)
  (reset! !query ""))

(e/defn HelpSection
  "One section. `target?` marks the section /help/<slug> names."
  [{:keys [slug title intro steps]} target?]
  (e/client
    (dom/section
      (dom/props {:id (section-dom-id slug)
                  :class (if target? "help-section help-section--target" "help-section")})
      (dom/h2
        (dom/props {:style {:font-size "18px" :font-weight "600" :margin-bottom "4px"
                            :color "var(--color-text-primary)"}})
        (dom/text title))
      (dom/p
        (dom/props {:style {:font-size "14px" :color "var(--color-text-secondary)"
                            :margin-bottom "10px"}})
        (dom/text (help/resolve-chords intro)))
      (dom/ol
        (dom/props {:style {:padding-left "20px" :display "flex"
                            :flex-direction "column" :gap "8px"}})
        (e/for-by :label [{:keys [label detail]} steps]
          (dom/li
            (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"
                                :line-height "1.5"}})
            (dom/strong (dom/text (help/resolve-chords label)))
            (dom/text " — ")
            (dom/text (help/resolve-chords detail))))))))

(e/defn HelpPage []
  (e/client
    (let [[sub] r/route
          slug (some-> sub str)
          unknown-slug? (and (some? slug) (nil? (help/section-by-slug slug)))
          !query (atom "")
          query (e/watch !query)
          shown (help/filter-sections query)]
      (dom/div
        (dom/props {:style {:max-width "820px" :margin "0 auto" :padding "32px 24px"}})
        (dom/h1
          (dom/props {:style {:font-size "26px" :font-weight "700" :margin-bottom "6px"
                              :color "var(--color-text-primary)"}})
          (dom/text "Help"))
        (dom/p
          (dom/props {:style {:font-size "15px" :color "var(--color-text-secondary)"
                              :margin-bottom "28px"}})
          (dom/text "FreeMemo runs three review systems over what you read: a priority queue for reading, FSRS-6 for quiz questions, and your own Anki for cards. Here's how to use it, feature by feature."))
        ;; plans/getting-started-checklist.md 5.2 — the same command as the palette.
        (dom/p
          (dom/props {:style {:font-size "14px" :margin "-16px 0 28px"}})
          (dom/a
            (dom/props {:href "/"
                        :style {:color "var(--color-primary-text)" :font-weight "600"
                                :text-decoration "underline" :cursor "pointer"}})
            (dom/text "Show the getting-started checklist")
            (dom/On "click" (fn [e] (.preventDefault e) (bus/dispatch! :show-getting-started)) nil)))
        (dom/input
          (dom/props {:id search-input-id :type "search" :class "input help-search"
                      :placeholder "Search Help" :aria-label "Search Help"})
          (dom/On "input" (fn [e] (reset! !query (-> e .-target .-value))) nil))
        (when unknown-slug?
          (dom/p
            (dom/props {:class "help-notice" :role "status"})
            (dom/text (str "Help has no section called \u201c" slug "\u201d. Showing all of Help."))))
        (when (empty? shown)
          (dom/p
            (dom/props {:class "help-notice" :role "status"})
            (dom/text "No help matches. ")
            (dom/button
              (dom/props {:type "button" :class "help-clear"})
              (dom/text "Clear the search")
              (dom/On "click" (fn [_] (clear-query! !query)) nil))))
        (e/for-by :slug [section shown]
          (HelpSection section (= slug (:slug section))))
        ;; Last form: its value is the body's, so it is never skipped, and it
        ;; gates no frame. Re-runs on every slug change (2.2).
        (scroll-to-section! slug)))))
