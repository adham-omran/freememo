(ns freememo.topic-kinds
  "The topic kinds, as one plain-data set (plans/help-for-viewer-topic-kind.md
   2.1). `topics.kind` is a string with no CHECK constraint, so this set is the
   only list of the kinds in code.

   A new kind joins this set. help_content_test then fails until Help maps the
   kind to a section.

   The four kind-badge switches (search_page, subset_review,
   hierarchy_side_panel, bibliography_form) do not read this set yet, and they
   already disagree with it: only bibliography_form lists \"score\".")

(def all
  #{"basic" "pdf" "page" "epub" "web" "wikipedia" "markdown" "supermemo"
    "video" "video-playlist" "audio" "score" "poem" "code" "repo" "commit" "dir"})
