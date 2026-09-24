(ns freememo.settings-tab
  "Which Settings tab shows. Its own namespace so that a page outside Settings
   can pick a tab before it navigates there: requiring `freememo.settings-page`
   from Home closes a cycle through `ai-features-section`.

   `settings-page` seeds the atom with its first tab and owns every other
   read and write. No requires.")

(defonce ^{:doc "The id of the Settings tab that shows, one of
   `freememo.settings-page/section-defs`."}
  !active-section
  (atom nil))

(defn select!
  "Make `section-id` the tab Settings shows, with no URL change. For a caller
   that navigates to Settings right after: a mount with no hash keeps it.
   Pre : `section-id` is an id in `settings-page/section-defs` (caller bug
         otherwise).
   Post: returns nil."
  [section-id]
  (reset! !active-section section-id)
  nil)
