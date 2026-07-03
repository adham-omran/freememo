(ns freememo.content-toolbar-helpers
  "Pure helper functions and background card-generation processor for ContentToolbar.
   Extracted to avoid circular dependencies between content_toolbar and content_toolbar_actions."
  (:require
   [clojure.string :as str]
   [freememo.logging :as log]
   [freememo.commands :as commands]
   #?(:clj [freememo.cards :as cards])
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled])))

;; Unsynced card count — single topic-id
(defn get-unsynced-count* [_refresh topic-id]
  #?(:clj (db/get-unsynced-card-count topic-id)
     :cljs 0))

;; Save generated cards to DB. Routes through cards/save-cards so bake-card-html
;; runs and topic pins land in :q / :a (basic) or :c (cloze) per P3d.
#?(:clj
   (defn save-cards-for-topic [topic-id root-topic-id kind generated-cards]
     (cards/save-cards topic-id root-topic-id kind generated-cards)))

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
                                               :content (cleaner/clean-html content)
                                               :kind "basic"
                                               :title title
                                               :source-id inherited-src})]
                (log/log-info (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-OK new-id=" (:topics/id created)
                                " inherited-source=" inherited-src))
                (db/copy-pins-to-child! parent-id (:topics/id created))
                {:success true})
              (catch Exception e
                (log/log-info (str "[CREATE-EXTRACT " invocation-id "] DB-INSERT-FAILED " (.getMessage e)))
                {:success false :error (.getMessage e)})))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Missionary card generation processor
;; ---------------------------------------------------------------------------

#?(:clj (defonce card-gen-mbx (m/mbx)))
;; card-gen-status is per-user via (us/get-atom user-id :card-gen-status)
;; Shape: {topic-id {:active-id nil, :error nil, :pending 0}}

#?(:clj
   (defn generate-and-save! [item]
     (try
       (let [{:keys [content context card-type card-count user-id enc-key
                     topic-id root-topic-id pre-prompt]} item
             gen-result (if (= card-type "basic")
                          (cards/generate-basic-cards
                            (cond-> {:content content :context context
                                     :card-count card-count :user-id user-id :enc-key enc-key}
                              pre-prompt (assoc :pre-prompt pre-prompt)))
                          (cards/generate-cloze-cards
                            (cond-> {:content content :context context
                                     :card-count card-count :user-id user-id :enc-key enc-key}
                              pre-prompt (assoc :pre-prompt pre-prompt))))]
         (if-not (:success gen-result)
           gen-result
           (save-cards-for-topic topic-id root-topic-id card-type (:cards gen-result))))
       (catch Exception e
         {:success false :error (.getMessage e)}))))

(defn enqueue-card-gen! [item]
  #?(:clj
     (let [tid (:topic-id item)
           uid (:user-id item)]
       (log/log-info (str "Card gen enqueued topic=" tid))
       (swap! (us/get-atom uid :card-gen-status) update tid
         (fn [s] (update (or s {:active-id nil :error nil :pending 0}) :pending inc)))
       (card-gen-mbx item)
       :enqueued)
     :cljs nil))

;; Infinite flow that reads from the mailbox one item at a time
#?(:clj
   (defn mbx-flow []
     (m/ap (loop []
             (let [item (m/? card-gen-mbx)]
               (m/amb item (recur)))))))

#?(:clj
   (defonce card-gen-processor-cancel
     ((m/reduce
        (fn [_ _] nil) nil
        (m/ap
          (let [item (m/?> 3 (mbx-flow))
                tid (:topic-id item)
                uid (:user-id item)
                !status (us/get-atom uid :card-gen-status)]
            (swap! !status update tid assoc :active-id (:id item) :error nil)
            (let [result (m/? (m/timeout
                                (m/via m/blk (generate-and-save! item))
                                60000
                                {:success false :error "Card generation timed out"}))]
              (swap! !status update tid
                (fn [s] (-> s
                          (assoc :active-id nil)
                          (update :pending #(max 0 (dec (or % 0)))))))
              (if (:success result)
                (do (commands/bump! uid :generate)
                  (log/log-info (str "Card gen complete topic=" tid)))
                (do (log/log-info (str "Card gen failed topic=" tid " error=" (:error result)))
                  (swap! !status update tid assoc :error (:error result))
                  (toasts/push! uid
                    {:level :error
                     :message (:error result)
                     :actions (if (= :insufficient-credits (:error-type result))
                                [{:label "Top up credits" :nav :settings}]
                                [])})))))))
      (fn [_] nil)
      (fn [e] (log/log-error (str "Card gen processor crashed: " e))))))
