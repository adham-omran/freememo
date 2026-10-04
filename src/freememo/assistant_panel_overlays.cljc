(ns freememo.assistant-panel-overlays
  "Two sibling groups of the assistant panel, in their own `e/defn`s so the
   panel's method stays under the 64KB bytecode limit (CLAUDE.md, \"Diagnosing
   'Method code too large!'\"). The panel keeps every `(e/server …)` form
   binding: values arrive here as positional arguments, which does not
   transfer them, and atoms arrive positionally, never inside a map."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.assistant-diagnostics :as diagnostics]
   [freememo.assistant-evidence :as evidence]
   [freememo.assistant-run-details :as run-details]
   [freememo.assistant-side-question :as side-question]))

(e/defn PanelOverlays
  "The answer details dialog, the evidence dialog, the side-question panel and
   the error line, in that DOM order. It watches `!side` and `!error` itself,
   which keeps two bindings out of the panel's method."
  [details user-id root-topic-id page-topic-id
   !details-run !evidence !side !error]
  (e/client
    (run-details/RunDetailsModal !details-run details user-id)
    (evidence/EvidenceModal !evidence root-topic-id)
    (side-question/SideQuestionHost (e/watch !side) user-id page-topic-id !side !evidence)
    (when-let [error (e/watch !error)]
      (dom/div
        (dom/props {:class "assistant-panel__error"})
        (dom/text error)))))

(e/defn PanelDiagnostics
  "Keep the browser-local diagnostic state in step with the panel.

   Call it as the LAST form of the panel body, wrapping nothing. It used to be
   `(case (replace-state! …) (do … (dom/div …)))`, which put the whole panel
   inside a case branch keyed on a map that changes at exactly the two moments
   a run starts and settles. Re-selecting the branch remounts it, so both
   transitions tore down and rebuilt the panel: every (e/server …) read went
   cold at once while the panel's atoms survived and put the same chat back a
   round trip later. That was the blank on send and the blank on settle.
   As the last form, its value is the body's value, so it cannot be skipped,
   and it gates no frame."
  [root-topic-id page-topic-id active active-run sending?]
  (e/client
    (e/on-unmount #(diagnostics/replace-state! {}))
    (diagnostics/replace-state!
      {:topic-id root-topic-id
       :page-topic-id page-topic-id
       :chat-id active
       :run-uuid (:run_uuid active-run)
       :run-kind (:kind active-run)
       :run-status (:status active-run)
       :render-phase (if sending? "streaming" "terminal")})))
