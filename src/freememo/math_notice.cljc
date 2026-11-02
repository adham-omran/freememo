(ns freememo.math-notice
  "The user-visible consequence of the KaTeX gate: one log line per gate event,
   and a notice above the document editor while its formulas show as TeX.

   `freememo.math` records the outcome in `math/!katex-outcome` and cannot report
   it itself: server-only `.clj` namespaces load `math`, and `client-errors`
   requires Electric. This namespace is the client-side owner of both halves.
   See `plans/katex-timeout-notice.md`."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.client-errors :as ce]
   [freememo.math :as math]
   [freememo.rich-text-editor :as editor]))

(defn- report-outcome!
  "Ship the log entry for `outcome`, if it has one. Never toasts: the two-arity
   `ce/report!` carries no notify text."
  [outcome]
  (when-let [[op message] (math/outcome-log-entry outcome)]
    (ce/report! op message)))

#?(:cljs
   (defonce ^{:private true
              :doc "Installed once per page load. Reports each gate transition
                    exactly once: the gate writes only on a change, and this
                    compares again because `reset!` notifies on any write.
                    The initial report covers a gate that settled before this
                    namespace loaded."}
     katex-outcome-reporter
     (do (add-watch math/!katex-outcome ::report
           (fn [_ _ before after]
             (when (not= before after)
               (report-outcome! after))))
         (report-outcome! @math/!katex-outcome)
         true)))

(defn unsaved-edit?
  "Whether the editor holds an edit that auto-save has not acknowledged.

   pre : `dirty` is `editor/!dirty-html`'s value, `saved-edit-id` is
         `editor/!saved-edit-id`'s.
   post: true iff an edit exists and its id is not the last saved one."
  [dirty saved-edit-id]
  (boolean (and dirty (not= (:edit-id dirty) saved-edit-id))))

(defn reload-page!
  "Reload the tab. Defined on both peers because a reactive body names it."
  [_event]
  #?(:cljs (.reload js/location)
     :clj nil))

(e/defn MathNotice
  "The notice above the document editor while its formulas show as TeX.

   Pre : `html` is the stored body the editor shows (`dctx/static-content`).
         The caller mounts this OUTSIDE `.quill-editor-wrapper`, whose children
         Quill owns.
   Post: the slot element always mounts, so the host's child count never
         changes. Inside it, a notice shows iff `math/notice-for` returns one:
         the gate timed out, failed, or arrived late, and `html` can hold math.
   Inv : Reload is disabled while an edit is unsaved, so it cannot discard one."
  [html]
  (e/client
    (dom/div
      (dom/props {:class "math-notice-slot"})
      (let [notice (math/notice-for (e/watch math/!katex-outcome) (math/has-tex? html))]
        (when notice
          (dom/div
            (dom/props {:class (if (= :ready (:tone notice))
                                 "math-notice math-notice--ready"
                                 "math-notice")
                        :role "status" :aria-live "polite"})
            (dom/span (dom/props {:class "math-notice__text"})
              (dom/text (:text notice)))
            (let [unsaved? (unsaved-edit? (e/watch editor/!dirty-html)
                             (e/watch editor/!saved-edit-id))]
              (dom/button
                (dom/props {:class "btn btn-sm" :type "button" :disabled unsaved?})
                (dom/text (if unsaved? "Saving…" "Reload page"))
                (dom/On "click" reload-page! nil)))))))))
