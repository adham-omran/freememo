(ns freememo.account-transfer-section
  "Account export group (plans/user-data-export-import.md §5.7.1). Extracted from
   settings_page so each e/defn stays under the JVM 64KB bytecode limit, matching
   `storage-section` and `credits-section`.

   The download is a plain anchor, not a button with a handler. `/api/account/export`
   is a GET the browser performs itself, so there is no server effect to sequence
   and therefore no `e/Token` — the same reasoning the project applies to a
   browser-local selection side effect.

   No progress indicator, deliberately. The response cannot declare a
   `Content-Length` (§5.3.8): its size is unknown until the archive is written.
   The hint says the download has started rather than showing a bar that would
   sit at zero."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.settings-group :as group]
   #?(:clj [freememo.account-export :as account-export])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; Server-only wrapper — visible to both compilers, plain CLJ body. Named in a
;; reactive body via (e/server …), so the var must exist on CLJS too.
(defn entity-counts* [_refresh user-id]
  #?(:clj (account-export/entity-counts db/ds user-id)
     :cljs nil))

(defn export-summary*
  "One line naming what the archive will carry, from the same counts the
   manifest records. Formatted server-side: `format` is CLJ-only, and calling it
   inside an e/defn body captures the var for Electric to serialize."
  [_refresh user-id]
  #?(:clj (let [c (account-export/entity-counts db/ds user-id)]
            (format "%d topics, %d cards, %d facts, %d images"
              (:topics c 0) (:flashcards c 0) (:kg_facts c 0) (:media c 0)))
     :cljs ""))

(e/defn AccountTransferSection [user-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          summary (e/server (e/Offload #(export-summary* refresh user-id)))]
      (dom/section
        (dom/props {:class "settings-group"})
        (group/GroupHeading "Export your data")
        (dom/div
          (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)"
                              :margin-bottom "10px"}})
          (dom/text summary))
        (dom/a
          (dom/props {:class "btn btn-secondary"
                      :href "/api/account/export"
                      ;; `download` keeps the browser from navigating away from
                      ;; the app to render the response.
                      :download ""
                      :style {:display "inline-block" :text-decoration "none"}})
          (dom/text "Download export (.zip)"))
        (dom/div
          (dom/props {:class "hint"})
          (dom/text (str "The archive holds your whole account: documents and their files, "
                      "cards and their review schedule, the knowledge graph, and your "
                      "settings. To load it into another FreeMemo instance, open that "
                      "instance's Import page and drop the .zip anywhere on it. "
                      "Large accounts take a while to start downloading, and the browser "
                      "cannot show progress because the size is not known in advance.")))))))
