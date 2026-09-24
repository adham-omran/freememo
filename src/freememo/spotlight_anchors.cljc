(ns freememo.spotlight-anchors
  "Every control a spotlight may point at (plans/getting-started-checklist.md
   3.2). The key is what callers of `freememo.spotlight/show!` pass. The value
   is the literal `data-tour` attribute the control carries.

   The sites write the literal string, never a lookup, so that
   `spotlight_anchors_test` can grep `src/` for each one. A value with no site
   fails that test instead of leaving a \"Show me\" that points at nothing.

   No requires: the test reads this namespace with no browser.")

(def anchors
  {;; nav_rail.cljc — the rail's Import item.
   :import-cta "import-cta"
   ;; nav_rail.cljc — the ☰ that opens the rail at 600px and below, where the
   ;; closed drawer hides Import.
   :rail-menu "rail-menu"
   :learn-start "learn-start"
   :extract "extract"
   :generate "generate"
   :quiz-start "quiz-start"
   :pdf-copy-text "pdf-copy-text"
   ;; ai_features_section.cljc — the "Use AI features" row, not the whole tab,
   ;; so the highlight is the switch the card asks for.
   :settings-ai "settings-ai"
   :anki-connect "anki-connect"
   :anki-sync "anki-sync"})
