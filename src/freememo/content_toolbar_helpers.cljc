(ns freememo.content-toolbar-helpers
  "Pure helper functions and background card-generation processor for ContentToolbar.
   Extracted to avoid circular dependencies between content_toolbar and content_toolbar_actions."
  (:require
   [clojure.string :as str]
   [freememo.logging :as log]
   [freememo.commands :as commands]
   [freememo.pre-prompt-mode :as ppm]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.cards-from-facts :as cff])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.page-merge :as page-merge])
   #?(:clj [freememo.repo-markdown :as repo-md])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.work-run :as run])
   #?(:clj [freememo.generation-queue :as queue])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled])))

;; Unsynced card count — single topic-id
(defn get-unsynced-count* [_refresh topic-id]
  #?(:clj (db/get-unsynced-card-count topic-id)
     :cljs 0))

;; ── Generate action labels ────────────────────────────────────────────────
;; Pure, both-platform. The pre-prompt dialog's title reads it.
(defn card-type-label
  "Display label for a card-type string. Capitalizes all three kinds
   (basic / cloze / overlapping); unknown values pass through verbatim."
  [card-type]
  (case card-type
    "basic" "Basic"
    "cloze" "Cloze"
    "overlapping" "Overlapping"
    (str card-type)))

;; Save generated cards to DB. Routes through cards/save-cards so bake-card-html
;; runs and topic pins land in :q / :a (basic) or :c (cloze) per P3d.
#?(:clj
   (defn save-cards-for-topic
     "Save generated cards. With `entity-id` they are linked to that known idea
      (plans/known-ideas.md §2.4)."
     ([user-id topic-id root-topic-id kind generated-cards]
      (save-cards-for-topic user-id topic-id root-topic-id kind generated-cards nil))
     ([user-id topic-id root-topic-id kind generated-cards entity-id]
      (save-cards-for-topic user-id topic-id root-topic-id kind generated-cards entity-id nil))
     ([user-id topic-id root-topic-id kind generated-cards entity-id commit!]
      (cards/save-cards user-id topic-id root-topic-id kind generated-cards true entity-id
        (or commit! (fn [write!] (write!)))))))

;; Get context from previous pages (for page-mode card generation)
(defn get-context-pages* [root-topic-id page-number context-window]
  #?(:clj
     (let [start-page (max 1 (- page-number context-window))
           end-page (dec page-number)]
       (when (>= end-page start-page)
         (let [pages (db/get-context-pages root-topic-id start-page end-page)]
           (when (seq pages)
             (->> pages
               (map :topics/content)
               (str/join "\n\n---\n\n"))))))
     :cljs nil))

;; Parent topic content for extract-mode context (moved here from
;; the old toolbar Generate so build-gen-context* can share it).
#?(:clj
   (defn get-parent-content* [topic-id]
     (when-let [pid (:topics/parent_id (db/get-topic topic-id))]
       (or (:topics/content (db/get-topic pid)) ""))))

;; Build {:content :context} for one card-generation run. Single source for the
;; Generate / Generate-with-Prompt token blocks and the compare-models modal —
;; the logic was previously duplicated at each token site.
;; :content = cleaned selection HTML, else the full content-text.
;; :context = prior-page (page mode) or parent (extract mode) content, or nil
;;            when use-context is false.
;;
;; A code topic (`:is-code?`, plans/git-forge-integration.md §4.4 4.6) takes its
;; subject in this order: a pane or editor selection — the caller has already
;; picked between the two — else the file's source. Never the notes alone: the
;; editor of a code topic holds notes ABOUT the file, and generating from them
;; with nothing selected would make cards about the notes. With the source as
;; the subject, the notes become its context, the way a selection's context is
;; the text around it. `user-id` scopes the source read to its owner.
#?(:clj
   (defn- code-source-html
     "Post: the file's source as an escaped code block, or nil for an empty leaf."
     [user-id topic-id]
     (when-let [source (:source (db/get-topic-code user-id topic-id))]
       (str "<pre><code>" (str/escape source {\& "&amp;" \< "&lt;" \> "&gt;"}) "</code></pre>"))))

(defn build-gen-context* [{:keys [selection-html content-text context-mode use-context
                                  topic-id root-topic-id page-number context-window
                                  is-code? user-id]}]
  #?(:clj
     (let [sel-html (cleaner/clean-html-fragment selection-html)
           subject-html (or sel-html
                          (when (and is-code? user-id) (code-source-html user-id topic-id)))
           context (when use-context
                     (case context-mode
                       :extract (if subject-html content-text (or (get-parent-content* topic-id) ""))
                       :page (let [prev (get-context-pages* root-topic-id page-number context-window)]
                               (if subject-html
                                 (if prev (str prev "\n\n---\n\n" content-text) content-text)
                                 prev))))]
       {:content (or subject-html content-text) :context context})
     :cljs nil))

;; Create an extract child topic — wrapped in try/catch outside e/defn.
;; Sanitize content: a manual extract creates a new topic from selected HTML;
;; the selection comes from the editor but may include pasted external content.
(defn create-extract-topic-safe! [parent-id user-id content title]
  #?(:clj (let [invocation-id (str (java.util.UUID/randomUUID))]
            (log/log-info (str "[CREATE-EXTRACT " invocation-id "] ENTER parent=" parent-id
                            " user=" user-id " title=" (pr-str title)
                            " content-len=" (count content)
                            " stack=" (->> (.getStackTrace (Thread/currentThread))
                                        (drop 1) (take 8) (map #(.toString %)) vec)))
            (try
              (let [inherited-src (db/clone-source! user-id
                                    (db/resolve-effective-source-id parent-id))
                    created (db/create-topic! {:parent-id parent-id
                                               :user-id user-id
                                               ;; A fragment, so it keeps no
                                               ;; heading ids — the anchors that
                                               ;; pointed at them stayed in the
                                               ;; parent.
                                               ;;
                                               ;; `strip-markers` FIRST, before
                                               ;; the sanitizer. A selection in
                                               ;; focus reader's merged mode can
                                               ;; span a page boundary, and
                                               ;; `clean-html-fragment` strips
                                               ;; the `id` that identifies the
                                               ;; boundary heading while keeping
                                               ;; its text — so afterwards the
                                               ;; literal "Page 7" is
                                               ;; indistinguishable from prose.
                                               ;; Same ordering trap
                                               ;; `sphinx_import/strip-headerlinks!`
                                               ;; records.
                                               :content (-> content
                                                          page-merge/strip-markers
                                                          cleaner/clean-html-fragment)
                                               :kind "basic"
                                               :title title
                                               :source-id inherited-src
                                               :extract? true})]
                (log/log-info (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-OK new-id=" (:topics/id created)
                                " inherited-source=" inherited-src))
                (db/copy-pins-to-child! parent-id (:topics/id created))
                {:success true})
              (catch Exception e
                (log/log-error (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-FAILED " (.getMessage e)))
                {:success false :error (.getMessage e)})))
     :cljs nil))

(defn create-code-pane-extract!
  "`create-extract-topic-safe!` for a selection in the code pane, either view
   (plans/markdown-rendered-code-pane.md §4.2, plans/editor-pane-toggle.md §5.4).
   Pre : `html` is that selection, from `selected-passage/current` — rendered
         Markdown, or the source view's escaped code block.
   Post: as `create-extract-topic-safe!`. The extract holds the selection with
         its tables lowered and each `pre` a Quill code block, so the first
         edit in Quill does not flatten it. A source-view block has no
         language and reads `plain`."
  [parent-id user-id html title]
  #?(:clj (create-extract-topic-safe! parent-id user-id
            (repo-md/selection->extract-html html) title)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Missionary card generation processor
;; ---------------------------------------------------------------------------

;; card-gen-status is per-user via (us/get-atom user-id :card-gen-status)
;; Shape: {topic-id {:active-id nil, :error nil, :pending 0}}

;; Fact-aware routing — the KG→cards bridge. When the document has approved KG
;; facts, Generate sources cards from the facts the passage supports, rendered
;; to statement text; otherwise (no facts, or none relevant) it falls back to
;; the passage's raw text — today's behavior, unchanged.
#?(:clj
   (defn resolve-gen-content
     "Decide the text a Generate run feeds the card generator.
      Pre:  `content` is the passage (cleaned selection HTML, else page text).
      Post: {:content text :from-facts? bool} — :from-facts? true when the text
            is rendered KG facts (context is then dropped); or a
            {:success false :error … :error-type kw?} passthrough when fact
            selection fails (e.g. out of credits)."
     [content user-id root-topic-id]
     (if-not (settings/effective-use-facts-for-card-generation user-id root-topic-id)
       {:content content :from-facts? false}
       (let [approved (db/get-kg-facts user-id root-topic-id "approved" nil)]
         (if (empty? approved)
         (do (log/log-info (str "Card gen: no KG facts for root=" root-topic-id "; raw-text path"))
           {:content content :from-facts? false})
         (let [sel (cff/select-relevant-facts user-id root-topic-id content approved)]
           (cond
             (not (:success sel))
             (do (log/log-info (str "Card gen: fact selection failed root=" root-topic-id
                                 " error=" (:error sel) "; no cards"))
               sel)
             (empty? (:facts sel))
             (do (log/log-info (str "Card gen: " (count approved) " fact(s) present but none matched"
                                 " passage root=" root-topic-id "; raw-text fallback"))
               {:content content :from-facts? false})
             :else
             (let [rendered (cff/render-facts (:facts sel))]
               (log/log-info (str "Card gen: sourcing cards from " (count (:facts sel)) "/"
                               (count approved) " fact(s) root=" root-topic-id
                               " ids=" (mapv :id (:facts sel)) "\n" rendered))
               {:content rendered :from-facts? true}))))))))

#?(:clj
   (defn generate-and-save! [item]
     (try
       (let [{:keys [content context card-type card-count user-id enc-key
                     topic-id root-topic-id pre-prompt pre-prompt-mode entity-id]} item
             ;; A strict pre-prompt IS the question, so the card count collapses
             ;; to 1. Forced here, where opts is owned — never written back to
             ;; the user's saved count, which still governs plain Generate.
             one-card? (ppm/single-card? pre-prompt-mode card-type pre-prompt)
             ;; The Flashcards level's model, or the user's own setting. Documents
             ;; do not override it (removed 2026-09-23).
             model (settings/get-model user-id)
             ;; Per-document learning goal (root-topic); nil/"" leaves gen unchanged.
             goal (settings/get-learning-goal user-id root-topic-id)
             ;; Cards from a known idea are made from its quotes, which are the
             ;; material; facts would replace them (plans/known-ideas.md §2.3).
             resolved (if entity-id
                        {:content content :from-facts? false}
                        (resolve-gen-content content user-id root-topic-id))]
         (if (false? (:success resolved))
           resolved ;; fact-selection failure — carries :error-type for the toast
           (let [gen-content (:content resolved)
                 ;; Facts are self-contained; page/parent context would dilute
                 ;; the facts-only source, so drop it on the fact path.
                 gen-context (when-not (:from-facts? resolved) context)
                 opts (cond-> {:content gen-content :context gen-context
                               :card-count (if one-card? 1 card-count)
                               :user-id user-id :enc-key enc-key
                               :topic-id topic-id :model model :goal goal}
                        pre-prompt (assoc :pre-prompt pre-prompt
                                     :pre-prompt-mode pre-prompt-mode))
                 gen-result (case card-type
                              "basic" (cards/generate-basic-cards opts)
                              "overlapping" (cards/generate-overlapping-cards opts)
                              (cards/generate-cloze-cards opts))]
             (cond
               ;; The HTTP request can outlive cancellation.
               ;; Reject it here and at the insert after image preparation.
               (and (:run item) (= :cancelled (run/state (:run item))))
               (do (log/log-info (str "Card gen cancelled before save topic=" topic-id))
                 {:success false :cancelled? true :error "Card generation cancelled"})

               (not (:success gen-result)) gen-result

               :else
               (save-cards-for-topic user-id topic-id root-topic-id card-type
                 (:cards gen-result) entity-id
                 (when-let [attempt (:run item)] #(run/commit! attempt %)))))))
       (catch Exception e
         (log/log-error (str "generate-and-save! failed: " (.getMessage e)))
         {:success false :error (.getMessage e)}))))

#?(:clj
   (defn- capacity-limits []
     (into {}
       (for [[key env] [[:items "CARD_GEN_QUEUE_ITEMS"]
                       [:user-items "CARD_GEN_QUEUE_USER_ITEMS"]
                       [:bytes "CARD_GEN_QUEUE_BYTES"]
                       [:user-bytes "CARD_GEN_QUEUE_USER_BYTES"]
                       [:item-bytes "CARD_GEN_QUEUE_ITEM_BYTES"]]
             :let [value (System/getenv env)] :when value]
         (let [limit (parse-long value)]
           (when-not (and limit (pos? limit))
             (throw (ex-info "Generation capacity must be a positive integer" {:setting env})))
           [key limit])))))

#?(:clj (defonce card-gen-queue (queue/create (capacity-limits))))

#?(:clj
   (defn- sync-topic-status! [scheduler uid tid]
     (locking (:state scheduler)
       (let [state (queue/topic-state scheduler uid tid)]
         (swap! (us/get-atom uid :card-gen-status) update tid
           #(merge {:error nil} % state))
         (when (zero? (:pending state))
           (swap! (us/get-atom uid :card-gen-cancellers) dissoc tid)
           (when-not (some #(and (= uid (:user-id (:item %)))
                                (= tid (:topic-id (:item %))))
                          (vals (:items @(:state scheduler))))
             (swap! (us/get-atom uid :card-gen-cancelled) disj tid)))
         state))))

#?(:clj
   (defn cancel-card-generation! [scheduler uid tid]
     (locking (:state scheduler)
       (queue/cancel-topic! scheduler uid tid)
       (swap! (us/get-atom uid :card-gen-cancelled) conj tid)
       (sync-topic-status! scheduler uid tid))
     nil))

(defn enqueue-card-gen! [item]
  #?(:clj
     (let [attempt (run/start)
           item (assoc item :request-id (:id item) :id (:id attempt) :run attempt)
           tid (:topic-id item) uid (:user-id item)
           outcome (queue/admit! card-gen-queue item
                     (fn []
                       (log/log-info (str "Card gen enqueued topic=" tid " run=" (:id attempt)))
                       (swap! (us/get-atom uid :card-gen-cancelled) disj tid)
                       (swap! (us/get-atom uid :card-gen-status) update tid assoc :error nil)
                       (swap! (us/get-atom uid :card-gen-cancellers) assoc tid
                         #(cancel-card-generation! card-gen-queue uid tid))
                       (sync-topic-status! card-gen-queue uid tid)))]
       (if (:ok outcome)
         (do (us/work-started! uid) :enqueued)
         (do (toasts/push! uid {:level :error :message (:error outcome)}) outcome)))
     :cljs nil))

#?(:clj
   (defn- card-generation-task [item]
     (m/via m/blk (generate-and-save! item))))

#?(:clj
   (defn- settle-generation! [scheduler item result]
     (let [uid (:user-id item) tid (:topic-id item)
           result (if (= :cancelled (run/state (:run item)))
                    {:success false :cancelled? true}
                    result)]
       (locking (:state scheduler)
         (queue/finish! scheduler (:id item))
         (sync-topic-status! scheduler uid tid)
         (cond
           (:success result)
           (do (commands/bump! uid :generate)
               (when (:success-toast? item)
                 (let [n (count (:ids result))]
                   (toasts/push! uid
                     {:level :success
                      :message (if (zero? n)
                                 "No new cards: they were all duplicates."
                                 (str "Added " n " card" (when (not= 1 n) "s") "."))})))
               (log/log-info (str "Card gen complete topic=" tid)))
           (:cancelled? result)
           (log/log-info (str "Card gen cancelled topic=" tid))
           :else
           (do (log/log-warn (str "Card gen failed topic=" tid " error=" (:error result)))
               (swap! (us/get-atom uid :card-gen-status) update tid assoc :error (:error result))
               (toasts/push! uid
                 {:level :error :message (:error result)
                  :actions (if (= :insufficient-credits (:error-type result))
                             [{:label "Top up credits" :nav :settings}] [])})))))))

#?(:clj
   (defn- generation-worker [scheduler]
     (m/sp
       (loop []
         (let [item (m/? (queue/take-task scheduler))]
           (sync-topic-status! scheduler (:user-id item) (:topic-id item))
           (let [result (if (= :cancelled (run/state (:run item)))
                          (do (log/log-info (str "Card gen skipped (cancelled) topic=" (:topic-id item)))
                              {:success false :cancelled? true})
                          (try
                            (m/? (card-generation-task item))
                            (catch Cancelled e (throw e))
                            (catch Exception e {:success false :error (.getMessage e)})))]
             (settle-generation! scheduler item result))
           (recur))))))

#?(:clj
   (defn start-generation-processor! [scheduler]
     ((m/join vector (generation-worker scheduler) (generation-worker scheduler)
        (generation-worker scheduler))
      (fn [_] nil)
      (fn [e] (log/log-error (str "Card gen processor crashed: " e))))))

#?(:clj
   (defonce card-gen-processor-cancel (start-generation-processor! card-gen-queue)))
