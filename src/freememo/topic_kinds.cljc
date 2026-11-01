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
    "video" "video-playlist" "youtube" "audio" "score" "poem" "code" "repo" "commit" "dir"
    "gdoc" "gdoc-snapshot"})

(def home-kinds
  "Kinds whose viewer page is a home rather than a document: no notes editor,
   no document toolbar and no card table. The forge structure kinds
   (`forge-kinds/structure-kinds`) and a Google Doc root, whose home lists its
   snapshots and holds Pull (plans/google-docs-pull.md)."
  #{"repo" "commit" "dir" "gdoc"})

(defn home-kind?
  "True when a topic of `kind` shows a home instead of a document."
  [kind]
  (contains? home-kinds kind))

(def read-only-content-kinds
  "Kinds whose `content` the user reads and never edits. The viewer mounts a
   reader for them instead of the editor, and `db/update-topic-content!`
   refuses to write their `content`. A poem's content is a fallback rendered
   from its package. A Google Doc snapshot's is the Doc as one Pull exported it
   (plans/google-docs-pull.md)."
  #{"poem" "gdoc-snapshot"})

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
