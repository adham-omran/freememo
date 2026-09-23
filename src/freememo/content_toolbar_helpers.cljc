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
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled])))

;; Unsynced card count — single topic-id
(defn get-unsynced-count* [_refresh topic-id]
  #?(:clj (db/get-unsynced-card-count topic-id)
     :cljs 0))

;; ── Generate action labels ────────────────────────────────────────────────
;; Pure, both-platform (used client-side in the Generate trigger + tooltip).
;; Single owner for card-type capitalization + the count/type summary so the
;; label reads identically wherever it appears (GenerateDropdown trigger,
;; ToolbarGenerate aria/tooltip).
(defn card-type-label
  "Display label for a card-type string. Capitalizes all three kinds
   (basic / cloze / overlapping); unknown values pass through verbatim."
  [card-type]
  (case card-type
    "basic" "Basic"
    "cloze" "Cloze"
    "overlapping" "Overlapping"
    (str card-type)))

(defn gen-label
  "Short action label: `Generate <n> <Type>` — no context clause. Shown as the
   split trigger's primary text."
  [card-count card-type]
  (str "Generate " card-count " " (card-type-label card-type)))

(defn gen-summary
  "Full action summary — `gen-label` plus a context clause when context is on.
   Used for the trigger tooltip / aria-label (not the visible primary text)."
  [card-count card-type use-context context-window]
  (str (gen-label card-count card-type)
    (when use-context
      (str " · " context-window " page" (when (not= 1 context-window) "s") " context"))))

;; Save generated cards to DB. Routes through cards/save-cards so bake-card-html
;; runs and topic pins land in :q / :a (basic) or :c (cloze) per P3d.
#?(:clj
   (defn save-cards-for-topic [user-id topic-id root-topic-id kind generated-cards]
     (cards/save-cards user-id topic-id root-topic-id kind generated-cards)))

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
;; content_toolbar_generate so build-gen-context* can share it).
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
                                               :source-id inherited-src})]
                (log/log-info (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-OK new-id=" (:topics/id created)
                                " inherited-source=" inherited-src))
                (db/copy-pins-to-child! parent-id (:topics/id created))
                {:success true})
              (catch Exception e
                (log/log-error (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-FAILED " (.getMessage e)))
                {:success false :error (.getMessage e)})))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Missionary card generation processor
;; ---------------------------------------------------------------------------

#?(:clj (defonce card-gen-mbx (m/mbx)))
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
                     topic-id root-topic-id pre-prompt pre-prompt-mode]} item
             ;; A strict pre-prompt IS the question, so the card count collapses
             ;; to 1. Forced here, where opts is owned — never written back to
             ;; the user's saved count, which still governs plain Generate.
             one-card? (ppm/single-card? pre-prompt-mode card-type pre-prompt)
             ;; The Flashcards level's model, or the user's own setting. Documents
             ;; do not override it (removed 2026-09-23).
             model (settings/get-model user-id)
             ;; Per-document learning goal (root-topic); nil/"" leaves gen unchanged.
             goal (settings/get-learning-goal user-id root-topic-id)
             resolved (resolve-gen-content content user-id root-topic-id)]
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
               ;; Checked between the model call and the insert, because that is
               ;; the only point left where cancelling can be honoured. The call
               ;; cannot be aborted — it goes through clj-http, whose socket read
               ;; ignores Thread.interrupt(), and this m/ap branch hands out no
               ;; per-item cancel handle anyway. So cancelling an in-flight
               ;; generation means: the call is paid for, and no cards land.
               (contains? @(us/get-atom user-id :card-gen-cancelled) topic-id)
               (do (log/log-info (str "Card gen cancelled before save topic=" topic-id))
                 {:success false :cancelled? true :error "Card generation cancelled"})

               (not (:success gen-result)) gen-result

               :else
               (save-cards-for-topic user-id topic-id root-topic-id card-type
                 (:cards gen-result))))))
       (catch Exception e
         (log/log-error (str "generate-and-save! failed: " (.getMessage e)))
         {:success false :error (.getMessage e)}))))

(defn enqueue-card-gen! [item]
  #?(:clj
     (let [tid (:topic-id item)
           uid (:user-id item)]
       (log/log-info (str "Card gen enqueued topic=" tid))
       ;; A fresh enqueue clears any standing cancellation for this topic —
       ;; otherwise a cancel would silently swallow every future run.
       (swap! (us/get-atom uid :card-gen-cancelled) disj tid)
       (swap! (us/get-atom uid :card-gen-status) update tid
         (fn [s] (update (or s {:active-id nil :error nil :pending 0}) :pending inc)))
       (swap! (us/get-atom uid :card-gen-cancellers) assoc tid
         (fn [] (swap! (us/get-atom uid :card-gen-cancelled) conj tid) nil))
       ;; After the status swap, so the Activity monitor opens this row now
       ;; rather than up to one reporter tick later — a generation shorter than
       ;; a tick was reaching the panel already reading "Done".
       (us/work-started! uid)
       (card-gen-mbx item)
       :enqueued)
     :cljs nil))

#?(:clj
   (defn- forget-card-gen-item!
     "Drop one outstanding item from a topic's counters, clearing the topic's
      cancellation state once nothing of it is left.

      Pre : called exactly once per dequeued item, run or skipped.
      Post: `:pending` decremented; at zero the topic's canceller and its
            standing cancellation are removed, so the next enqueue starts clean."
     [uid tid]
     (let [remaining (-> (swap! (us/get-atom uid :card-gen-status) update tid
                           (fn [s] (-> s
                                     (assoc :active-id nil)
                                     (update :pending #(max 0 (dec (or % 0)))))))
                       (get tid) :pending)]
       (when (zero? (or remaining 0))
         (swap! (us/get-atom uid :card-gen-cancellers) dissoc tid)
         (swap! (us/get-atom uid :card-gen-cancelled) disj tid))
       remaining)))

;; Infinite flow that reads from the mailbox one item at a time
#?(:clj
   (defn mbx-flow []
     (m/ap (loop []
             (let [item (m/? card-gen-mbx)]
               (m/amb item (recur)))))))

#?(:clj
   (defn- card-generation-task
     "Run one queued card-generation item.
      Pre:  `item` satisfies `generate-and-save!`.
      Post: a Missionary task returns the exact generation result.
      Inv:  the generation runs on the blocking executor, and no outer deadline
            can replace a result after `generate-and-save!` saves cards."
     [item]
     (m/via m/blk (generate-and-save! item))))

#?(:clj
   (defonce card-gen-processor-cancel
     ((m/reduce
        (fn [_ _] nil) nil
        (m/ap
          (let [item (m/?> 3 (mbx-flow))
                tid (:topic-id item)
                uid (:user-id item)
                !status (us/get-atom uid :card-gen-status)]
            ;; Checked at dequeue, and again inside `generate-and-save!` just
            ;; before the insert. Here it is free and complete — an item still in
            ;; the mailbox has spent nothing. There it is cooperative: the model
            ;; call cannot be recalled, but the cards need not land.
            (if (contains? @(us/get-atom uid :card-gen-cancelled) tid)
              (do (log/log-info (str "Card gen skipped (cancelled) topic=" tid))
                (forget-card-gen-item! uid tid))
              (do
                (swap! !status update tid assoc :active-id (:id item) :error nil)
                ;; Apache HttpClient ignores Missionary's worker interruption.
                ;; An outer timeout can therefore expire, wait while this call
                ;; saves cards, and then replace its success with a timeout. The
                ;; worker result is the only truthful completion signal.
                (let [result (m/? (card-generation-task item))]
                  (forget-card-gen-item! uid tid)
                  (cond
                    (:success result)
                    (do (commands/bump! uid :generate)
                      (log/log-info (str "Card gen complete topic=" tid)))

                    ;; A cancelled run is not a failure. Recording it in `:error`
                    ;; would make the Activity monitor close the run as one, and
                    ;; toast an error for something the user chose.
                    (:cancelled? result)
                    (log/log-info (str "Card gen cancelled topic=" tid))

                    :else
                    (do (log/log-warn (str "Card gen failed topic=" tid " error=" (:error result)))
                      (swap! !status update tid assoc :error (:error result))
                      (toasts/push! uid
                        {:level :error
                         :message (:error result)
                         :actions (if (= :insufficient-credits (:error-type result))
                                    [{:label "Top up credits" :nav :settings}]
                                    [])})))))))))
      (fn [_] nil)
      (fn [e] (log/log-error (str "Card gen processor crashed: " e))))))
