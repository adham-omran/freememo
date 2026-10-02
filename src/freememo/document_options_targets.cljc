(ns freememo.document-options-targets
  "The topics that Document options acts on, computed from the viewer's ids.
   One definition, read by the content toolbar and by the modal mount in
   DocumentToolbars (plans/document-options-autosave-sheet.md 1.8).

   No requires: the content toolbar and a plain Clojure test both load it.")

(defn targets
  "The four topics Document options acts on.

   Pre:  topic-id is the viewed topic (a PDF root, a PDF page, an extract or a
         standalone topic). context-mode is :page for a PDF root or page, else
         :extract. extract-status is non-nil only for an extract.
         root-topic-id is the document root, or nil when topic-id is the root.
   Post: {:root-id  the document root — document-scoped settings
          :bib-id   the topic that owns the bibliography. A PDF page owns none,
                    so it resolves to the root
          :review-id the review unit that Priority acts on. An extract is its
                    own unit. Anything else is reviewed as its document
          :item-id  the viewed topic — the Custom prompt attaches here}"
  [{:keys [context-mode extract-status root-topic-id topic-id]}]
  (let [root-id (or root-topic-id topic-id)]
    {:root-id   root-id
     :bib-id    (if (= context-mode :page) root-id topic-id)
     :review-id (if extract-status topic-id root-id)
     :item-id   topic-id}))
