(ns freememo.knowledge-page
  "Knowledge tab — the fact-graph workbench (plans/knowledge-graph-quizzes.md M2),
   and the known ideas a reader has learned (plans/known-ideas.md).

   This namespace holds the sub-view tabs and the dispatch. Each sub-view lives in
   its own namespace for the 64KB method cap: Documents, Facts, Questions and
   Entities in `freememo.knowledge-*`, Known ideas in `freememo.knowledge-known`.
   All five share `freememo.tape-table`'s selection, sort and batch bar and
   `freememo.row-menu` (plans/knowledge-tables-selection-and-bulk.md).

   All queries re-run on the :kg-mutations channel; distill progress is the
   :distilling-docs set."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.router5 :as r]
   [freememo.knowledge-documents :as documents]
   [freememo.knowledge-entities :as entities]
   [freememo.knowledge-facts :as facts]
   [freememo.knowledge-known :as known]
   [freememo.knowledge-questions :as questions]
   [freememo.page-title :refer [PageTitle]]
   #?(:clj [freememo.user-state :as us])))

(e/defn SubViewTabs
  "Settings-style horizontal nav (see settings-page/SettingsNav). Copies the
   .settings-nav markup + CSS rather than sharing the component, which is bound
   to settings-specific module state. Driven by the /knowledge/<sub>[/<doc-id>]
   URL segments via navigate!; the facts drill-down keeps Documents highlighted.

   `doc-id` is taken as its OWN parameter, never derived from the sub-view value:
   Electric keeps a branch's argument exprs live during the switch frame, so
   seq-ing a value that has flipped to a keyword throws (see the dispatch below).
   Pre:  sub-kw ∈ #{:documents :entities :questions :facts :known}; doc-id may
         be nil.
   Post: Entities/Questions preserve the current doc-id; Documents drops it — it
         is the unscoped index, where a scope has no meaning. Known ideas has no
         document scope (plans/known-ideas.md §3.1)."
  [navigate! sub-kw doc-id]
  (e/client
    (dom/nav
      (dom/props {:class "settings-nav"})
      (e/for [[k label] (e/diff-by first [[:documents "Documents"] [:entities "Entities"]
                                          [:questions "Questions"] [:known "Known ideas"]])]
        (let [active? (or (= sub-kw k) (and (= sub-kw :facts) (= k :documents)))]
          (dom/a
            (dom/props {:href "#"
                        :class (if active?
                                 "settings-nav__item settings-nav__item--active"
                                 "settings-nav__item")})
            (dom/text label)
            (dom/On "click"
              (fn [e]
                (.preventDefault e)
                (case k
                  :documents (navigate! :knowledge)
                  :entities (navigate! :knowledge {:type :knowledge-entities
                                                   :doc-id doc-id})
                  :questions (navigate! :knowledge {:type :knowledge-questions
                                                    :doc-id doc-id})
                  :known (navigate! :knowledge {:type :knowledge-known})))
              nil)))))))

(e/defn KnowledgePage [user-id navigate!]
  (e/client
    ;; Sub-view is the URL segment under /knowledge (router5 r/pop'd by main), so
    ;; tabs are deep-linkable and survive refresh/back. `arg` carries the doc-id
    ;; for the facts drill-down (/knowledge/facts/<id>).
    ;; `sub-kw` and `arg` are kept as SEPARATE scalars rather than folded into a
    ;; [sub doc-id] vector: the old shape was a keyword for two sub-views and a
    ;; vector for the third, which forced a `vector?` test in the tabs and an
    ;; `arg`-not-`(second view)` workaround here (Electric keeps a branch's
    ;; argument exprs live during the switch frame, so seq-ing the value after it
    ;; flips to a keyword throws not-ISeqable). Two scalars remove both.
    (let [[sub arg] r/route
          sub-kw (case sub
                   entities :entities
                   questions :questions
                   facts :facts
                   known :known
                   :documents)               ; nil / documents / unknown → documents
          kg-bump (e/server (e/watch (us/get-atom user-id :kg-mutations)))]
      (dom/div
        (dom/props {:style {:max-width "960px" :margin "0 auto" :padding-bottom "32px"}})
        (SubViewTabs navigate! sub-kw arg)
        (cond
          (= sub-kw :entities) (entities/EntitiesView user-id kg-bump arg navigate!)
          (= sub-kw :questions) (questions/QuestionsView user-id kg-bump arg navigate!)
          (= sub-kw :facts) (facts/FactsView user-id arg navigate! kg-bump)
          ;; /knowledge/known lists the ideas, /knowledge/known/<id> is one idea
          ;; (plans/known-ideas.md §3.1, §5.1).
          (= sub-kw :known) (if (some? arg)
                              (known/IdeaPage user-id arg navigate!)
                              (known/KnownIdeasView user-id navigate!))
          :else (documents/DocumentsView user-id navigate! kg-bump)))
      ;; A document the user does not own reads Not found here, although the
      ;; views show an empty list for it (plans/reactive-page-title.md D16).
      ;; A known idea is named after its entity (plans/known-ideas.md §5.2).
      (PageTitle user-id
        (cond
          (and (= sub-kw :known) (some? arg)) [:entity arg]
          (and (not= sub-kw :documents) (not= sub-kw :known) (some? arg)) [:topic arg]
          :else [:label :knowledge])))))
