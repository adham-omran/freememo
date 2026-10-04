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

(defn pdf-root-id
  "The id the PDF viewer and the toolbar's card generation treat as the document
   root, or nil when the topic is not part of a PDF.

   A pdf or score topic is its own root: a score rides the PDF viewer, because
   its main blob is the sheet-music PDF. A page under a pdf root has that root.
   Anything else has none, and callers fall back to the tree root.

   Pre:  `root-kind` is the kind of `root-id`, the topic's tree root."
  [kind topic-id root-kind root-id]
  (cond
    (= kind "pdf") topic-id
    (= kind "score") topic-id
    (and (= kind "page") (= root-kind "pdf")) root-id
    :else nil))
