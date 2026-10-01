(ns freememo.settings-group
  "The heading of a group inside a Settings tab.

   A tab has one frame, and `settings-page/SettingsTab` owns it. Inside that
   frame, each group is a `section.settings-group` that starts with this
   heading. Groups separate by a rule and a title, not by a box of their own.
   See plans/scannable-settings-toolbar-accent.md 1.1."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn GroupHeading
  "Pre : the first child of a `section.settings-group`.
   Post: the group's `h4` title, above the heading rule."
  [title]
  (e/client
    (dom/div
      (dom/props {:class "settings-group__heading"})
      (dom/h4 (dom/props {:class "settings-group__title"}) (dom/text title)))))
