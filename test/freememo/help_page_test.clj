(ns freememo.help-page-test
  "Content contracts for recent user-facing workflows in the Help page."
  (:require
   [clojure.set :as set]
   [clojure.test :refer [deftest is testing]]
   [freememo.help-content :as help]))

(defn- workflow-step-labels [title]
  (some->> help/sections
    (some #(when (= title (:title %)) %))
    :steps
    (map :label)
    set))

(deftest recent-feature-workflows-are-covered
  (let [required-labels
        {"Asking the AI assistant"
         #{"Set the panel height" "Reference another document"
           "Check source evidence" "Manage an answer"}
         "Planning a reading schedule"
         #{"Create a document schedule" "Read the dashboard"
           "Follow the reading pill" "Inspect the plan"
           "Handle a past target" "Archive or restore"}
         "Building a curriculum"
         #{"Create it" "Add blocks and materials" "Order the work"
           "Set the focus" "Track readiness" "Edit or delete"}
         "Taking curriculum exams"
         #{"Generate an exam" "Review and approve" "Take or resume it"
           "Grade it" "Cancel or archive" "Understand credits"}
         "Finding & organizing"
         #{"Use the Library on a phone"}
         ;; plans/help-import-sources.md 4.3 — each import source's first step.
         "Importing content"                         #{"Drop or browse" "Which source should I use?"}
         "Import a PDF, EPUB, HTML or Markdown file" #{"Pick the file"}
         "Import a SuperMemo collection"             #{"Zip the collection"}
         "Import a Sphinx documentation site"        #{"Zip the built site"}
         "Move your account to another FreeMemo"     #{"Export"}
         "Import from a link"                        #{"Fetch"}
         "Import audio"                              #{"Upload"}
         "Import video"                              #{"Add the files"}
         "Write a new topic"                         #{"Create"}
         "Paste HTML or Markdown"                    #{"Open Paste"}
         "Sheet-music (Score) cards"                 #{"Create it"}
         "Import from Zotero"                        #{"Set it up once"}
         "Import code"                               #{"From a forge"}
         "Import a poem"                             #{"Get a package"}
         "Live Document"                             #{"Start one"}
         "Control FreeMemo with the keyboard"
         #{"Start with focus" "Move between controls" "Activate controls"
           "Open any tab" "Use the command palette ({toggle-palette})"
           "Read documents" "Make cards" "Sync Anki"
           "Use tables and lists" "Use the topic tree" "Move a topic"
           "Use the graph" "Use rich-text editors" "Use dialogs"
           "Resize panes" "Spatial editors"}}]
    (doseq [[title expected] required-labels]
      (testing title
        (let [actual (workflow-step-labels title)]
          (is (some? actual) (str "Help is missing the workflow: " title))
          (is (set/subset? expected actual)
            (str "Help is missing steps from " title ": "
              (pr-str (set/difference expected (or actual #{}))))))))))
