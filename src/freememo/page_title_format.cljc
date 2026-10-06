(ns freememo.page-title-format
  "The words and the format of the browser tab title
   (plans/reactive-page-title.md §1).

   NO requires. Both peers read the same plain literals, and a CLJ test loads
   this namespace with PostgreSQL stopped.")

(def brand "FreeMemo")

(def tab-labels
  "Tab keyword → the word its tab title uses.

   `:knowledge` reads \"Knowledge\", not the nav text \"Facts\". A keyword absent
   here is a tab `Main` does not know, and its title reads Not found."
  {:home "Home"
   :viewer "Viewer"
   :library "Library"
   :learn "Learn"
   :schedule "Schedule"
   :curriculum "Curriculum"
   :quiz "Quiz"
   :search "Search"
   :knowledge "Knowledge"
   :graph "Graph"
   :import "Import"
   :help "Help"
   :settings "Settings"
   :credits "Credits"})

(def self-titled-tabs
  "Tabs whose page writes its own title, because a route under them names an
   item. `Main` writes the tab label for every other tab, so exactly one writer
   owns the title on any route."
  #{:viewer :schedule :curriculum :knowledge})

(defn walkthrough-name
  "The name of a walkthrough of the repository titled `repo-title`
   (plans/commit-range-walkthrough.md §6.6.7.3), or nil when there is none."
  [repo-title]
  (when repo-title (str "Walkthrough · " repo-title)))

(defn page-title
  "Pre: item-name is a page's name as a string, or nil when the page's item
   does not exist.
   Post: `<item-name> | FreeMemo`. A name with no visible character reads
   Untitled, and nil reads Not found."
  [item-name]
  (str (cond
         (nil? item-name) "Not found"
         (re-find #"\S" item-name) item-name
         :else "Untitled")
       " | " brand))
