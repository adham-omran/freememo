(ns freememo.target-commands
  "Server effects of the queued target commands (plans/custom-context-menu.md
   §2). Each method reads its subjects from the payload a context-menu row or a
   focus target sent, and re-reads every fact it acts on from the database: a
   subject's :topic-kind or :root? only decided which rows the menu offered.

   Ownership is checked per subject, here, before any write. A payload is
   client data, and the menu makes every row of every surface a target.

   The toolbar buttons call the same server functions these methods call
   (done, stage-deletion!, enqueue-card-gen!, start-ocr-scan!), so a menu row
   and a button cannot differ in what they do to the data."
  (:require [clojure.string :as str]
            [freememo.commands :as commands]
            [freememo.content-toolbar-helpers :as helpers]
            [freememo.crypto :as crypto]
            [freememo.db :as db]
            [freememo.logging :as log]
            [freememo.optimistic :as opt]
            [freememo.pdf-action-dropdowns :as pdf-actions]
            [freememo.priority :as priority]
            [freememo.settings :as settings]
            [freememo.staged-delete :as staged-delete]
            [freememo.toasts :as toasts]
            [freememo.topic-move :as topic-move]
            [freememo.user-state :as us]))

(defn- subject-ids [payload]
  (mapv :topic-id (:subjects payload)))

(defn- owned-topic
  "The topic row for `id` when `user-id` owns it, else nil."
  [user-id id]
  (when (and (integer? id) (db/owns-topic? user-id id))
    (db/get-topic id)))

(defn- refuse!
  "Toast `message` and return the failure outcome."
  [user-id message]
  (toasts/push! user-id {:level :error :message message})
  {:ok? false :error message})

(defn- each-owned!
  "Run `f` on each subject topic `user-id` owns. A subject it does not own
   refuses the whole command before anything runs.
   Post: {:ok? true :results [...]} or a refusal."
  [user-id payload f]
  (let [ids (subject-ids payload)
        topics (mapv #(owned-topic user-id %) ids)]
    (if (or (empty? ids) (some nil? topics))
      (refuse! user-id "That topic is not available.")
      {:ok? true :results (mapv f topics)})))

(defn- enc-key-for
  "The key that decrypts `user-id`'s stored API keys — the value the session
   derives at sign-in (main/reconstruct-session*). Nil for a password account."
  [user-id]
  (when-let [google-id (:users/google_id (db/get-user-by-id user-id))]
    (crypto/derive-key-for-oauth-user google-id)))

;; ---------------------------------------------------------------------------
;; Topic
;; ---------------------------------------------------------------------------

(defmethod opt/run-command! :rename-topic [user-id {:keys [payload]}]
  (let [id (first (subject-ids payload))]
    (try
      (if (= :renamed (db/rename-topic! user-id id (:input payload)))
        (do (log/audit! {:id ::rename-topic :user-id user-id :action :update
                         :entity :document :entity-id id})
            {:ok? true})
        (refuse! user-id "That topic is not available."))
      (catch clojure.lang.ExceptionInfo e
        (refuse! user-id (str "Could not rename: " (ex-message e)))))))

(defmethod opt/run-command! :move-topic-to-top-level [user-id {:keys [payload]}]
  (each-owned! user-id payload
    (fn [topic] (topic-move/move-topic! user-id (:topics/id topic) nil))))

(defn- review-unit-id
  "The topic a priority belongs to: a PDF page's priority is its document's."
  [topic]
  (if (= "page" (:topics/kind topic))
    (:topics/parent_id topic)
    (:topics/id topic)))

(defmethod opt/run-command! :set-topic-priority [user-id {:keys [payload]}]
  (let [value (priority/normalize (:input payload))]
    (each-owned! user-id payload
      (fn [topic]
        (db/update-topic-priority! user-id (review-unit-id topic) value)))))

(defmethod opt/run-command! :mark-topic-done [user-id {:keys [payload]}]
  (each-owned! user-id payload
    (fn [topic]
      (if (= "page" (:topics/kind topic))
        ;; A page's completion lives on its PDF, keyed by page number: the
        ;; write the PDF toolbar's :set-page-done makes.
        (db/set-page-done! user-id (:topics/parent_id topic) (:topics/page_number topic) true)
        (db/done-topic! user-id (:topics/id topic))))))

(defn- set-dismissed! [user-id payload dismissed?]
  (each-owned! user-id payload
    (fn [topic]
      (let [id (:topics/id topic)]
        (if dismissed?
          (db/dismiss-topic! user-id id)
          (db/undismiss-topic! user-id id))
        (log/audit! {:id (if dismissed? ::topic-dismissed ::topic-undismissed)
                     :user-id user-id :action :update :entity :document :entity-id id})))))

(defmethod opt/run-command! :dismiss [user-id {:keys [payload]}]
  (set-dismissed! user-id payload true))

(defmethod opt/run-command! :undismiss [user-id {:keys [payload]}]
  (set-dismissed! user-id payload false))

;; The client finishes a delete in its ack (freememo.target-handlers): it
;; removes the deleted cards' Anki notes and the PDFs' cached bytes, which only
;; the browser can reach.
(defmethod opt/run-command! :delete-topic [user-id {:keys [payload]}]
  (let [outcome (each-owned! user-id payload
                  (fn [topic]
                    (when-let [r (staged-delete/stage-deletion! user-id (:topics/id topic) true)]
                      (assoc r :topic-id (:topics/id topic)))))]
    (if (:ok? outcome)
      (let [staged (remove nil? (:results outcome))]
        {:ok? (boolean (seq staged))
         :anki-note-ids (vec (mapcat :note-ids staged))
         :deleted-topic-ids (mapv :topic-id staged)})
      outcome)))

;; ---------------------------------------------------------------------------
;; Card generation
;; ---------------------------------------------------------------------------

(defn- enqueue-generation!
  "Queue card generation for `topic` with the user's saved settings — the
   values the toolbar seeds its own controls from (content_toolbar.cljc).

   Pre : `user-id` owns `topic`.
   Post: :enqueued, or a refusal when the topic has nothing to generate from."
  [user-id topic]
  (let [id (:topics/id topic)
        page? (= "page" (:topics/kind topic))
        code? (some? (:topics/repo_path topic))
        content-text (or (:topics/content_text topic) "")
        root-id (if page? (:topics/parent_id topic) (db/get-root-topic-id id))]
    (if (and (str/blank? content-text) (not code?))
      (refuse! user-id "Extract text first to generate flashcards.")
      (let [{:keys [content context]}
            (helpers/build-gen-context*
              {:selection-html nil
               :content-text content-text
               :context-mode (if page? :page :extract)
               :use-context (settings/get-context-enabled user-id)
               :context-window (settings/get-context-pages user-id)
               :topic-id id :root-topic-id root-id
               :page-number (:topics/page_number topic)
               :is-code? code? :user-id user-id})]
        (let [outcome (helpers/enqueue-card-gen!
                        {:id (str (random-uuid))
                         :content content :context context
                         :card-type (settings/get-card-type user-id)
                         :card-count (settings/get-card-count user-id)
                         :user-id user-id :enc-key (enc-key-for user-id)
                         :topic-id id :root-topic-id root-id
                         :pre-prompt nil :pre-prompt-mode nil})]
          (if (= :enqueued outcome) {:ok? true}
              {:ok? false :error (:error outcome)}))))))

(defmethod opt/run-command! :generate-cards-for-topic [user-id {:keys [payload]}]
  (let [topic (owned-topic user-id (first (subject-ids payload)))]
    (cond
      (nil? topic) (refuse! user-id "That topic is not available.")
      (and (nil? (:topics/parent_id topic)) (= "pdf" (:topics/kind topic)))
      (refuse! user-id "Generate cards from a page of this PDF.")
      :else (enqueue-generation! user-id topic))))

;; ---------------------------------------------------------------------------
;; PDF page
;; ---------------------------------------------------------------------------

(defn- page-topic
  "The page topic of page `page-number` in the PDF `pdf-root-id`, or nil."
  [pdf-root-id page-number]
  (some #(when (= page-number (:topics/page_number %)) %) (db/list-pages pdf-root-id)))

(defn- owned-pdf-page
  "[pdf-root-id page-number] of the first subject when `user-id` owns the PDF."
  [user-id payload]
  (let [{:keys [pdf-root-id page-number]} (first (:subjects payload))]
    (when (and (integer? page-number) (owned-topic user-id pdf-root-id))
      [pdf-root-id page-number])))

(defmethod opt/run-command! :scan-pdf-page [user-id {:keys [payload]}]
  (if-let [[doc page] (owned-pdf-page user-id payload)]
    (do (when-not (contains? @(us/get-atom user-id :scanning-pages) [doc page])
          (pdf-actions/start-ocr-scan! user-id doc page (enc-key-for user-id)
            (settings/get-scan-dpi user-id)))
        {:ok? true})
    (refuse! user-id "That page is not available.")))

(defmethod opt/run-command! :generate-cards-for-page [user-id {:keys [payload]}]
  (if-let [topic (some->> (owned-pdf-page user-id payload) (apply page-topic))]
    (enqueue-generation! user-id topic)
    (refuse! user-id "That page is not available.")))

;; ---------------------------------------------------------------------------
;; Image pins and error toasts
;; ---------------------------------------------------------------------------

(defmethod opt/run-command! :add-pin [user-id {:keys [payload]}]
  (let [{:keys [topic-id media-id placement]} payload]
    (if-not (owned-topic user-id topic-id)
      (refuse! user-id "That topic is not available.")
      (try
        (db/set-pin! {:topic-id topic-id :media-id media-id :placement placement})
        (log/audit! {:id ::add-pin :user-id user-id :action :create
                     :entity :pin :entity-id media-id})
        {:ok? true}
        (catch clojure.lang.ExceptionInfo e
          (refuse! user-id (if (= :pin-cap-exceeded (:reason (ex-data e)))
                             "This topic already has 2 pins. Remove one first."
                             "Couldn't pin this image.")))))))

;; A client-side failure (a clipboard write, an upload) has no server scope to
;; toast from. The handler queues this instead.
(defmethod opt/run-command! :show-error-toast [user-id {:keys [payload]}]
  (toasts/push! user-id {:level :error :message (str (:message payload))})
  {:ok? true})

(comment
  ;; Every queued target command needs a method here, or the pump toasts
  ;; "Unknown action".
  (->> commands/registry
    (filter (fn [[_ e]] (and (:target e) (= :queue (commands/exec-mode e)))))
    (map first)
    (remove (set (keys (methods opt/run-command!))))))
