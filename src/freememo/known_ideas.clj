(ns freememo.known-ideas
  "Known ideas (plans/known-ideas.md): the list of what a reader knows, one
   idea's page, the cards made from an idea, and the writes that change what
   counts as known.

   `freememo.db` owns every statement and its ownership clause. This namespace
   shapes rows for the wire, with dates as text, and ends every write in
   `commands/bump!`."
  (:require
   [clojure.string :as str]
   [freememo.commands :as commands]
   [freememo.content-toolbar-helpers :as card-gen]
   [freememo.db :as db]
   [freememo.html-cleaner :as cleaner]
   [freememo.text :as text]
   [freememo.util :as util]))

;; ── Reads ──────────────────────────────────────────────────────────

(defn- how-known
  "`Tutor confirmed (Oct 12), marked by you (Oct 10)`, from an idea's sources."
  [{:keys [tutor_at self_at]}]
  (str/join ", "
    (cond-> []
      tutor_at (conj (str "Tutor confirmed (" (util/format-date-short tutor_at) ")"))
      self_at (conj (str (if tutor_at "marked" "Marked") " by you ("
                      (util/format-date-short self_at) ")")))))

(defn- origin-text
  "The files an idea came from, joined with commas, else its repositories."
  [{:keys [origin-files origin-repos]}]
  (if (seq origin-files)
    (str/join ", " (map #(str (:repo_path %) " (" (:repo_title %) ")") origin-files))
    (str/join ", " (map :repo_title origin-repos))))

(defn- check-text
  "`Never checked`, or the last check's date, with `(due)` on a due idea."
  [{:keys [last_check due?]}]
  (str (if last_check (util/format-date-short last_check) "Never checked")
    (when due? " (due)")))

(defn list-rows
  "The rows of the known ideas list (known-ideas §3.2), due ideas first.
   Post: [{:entity_id :label :how :from :checked :due? :card_count :material?
           :self? :sources}], no timestamps."
  [user-id]
  (mapv (fn [idea]
          {:entity_id (:entity_id idea)
           :label (:label idea)
           :sources (:sources idea)
           :self? (boolean (some #{"self"} (:sources idea)))
           :how (how-known idea)
           :from (origin-text idea)
           :checked (check-text idea)
           :due? (:due? idea)
           :card_count (:card_count idea)
           :material? (:material? idea)})
    (db/known-ideas user-id)))

(defn idea-page
  "One idea for its page (known-ideas §5), dates as text.
   Post: nil when the entity is not the user's. Otherwise `db/known-idea`'s
         map with every time formatted, plus `:events` for How you got here
         (§5.3.3): only times a row stores, oldest first."
  [user-id entity-id]
  (when-let [idea (db/known-idea user-id entity-id)]
    (let [fmt util/format-date-short
          events (->> (concat
                        (for [{:keys [repo_path pins]} (:roots idea)
                              {:keys [label question created_at]} pins]
                          [created_at (str "You pinned passage " label " in " repo_path
                                        (when-not (str/blank? question)
                                          (str ": " question)))])
                        (for [{:keys [repo_path learned_at]} (:roots idea)
                              :when learned_at]
                          [learned_at (str "The tutor confirmed it in " repo_path ".")])
                        ;; A tutor mark with a learned root is that root's event
                        ;; above. A walkthrough's mark has no root, so it is its own.
                        (for [{:keys [source created_at]} (:marks idea)
                              :when (or (= "self" source)
                                      (not-any? :learned_at (:roots idea)))]
                          [created_at (if (= "self" source)
                                        "You marked it known."
                                        "The tutor confirmed it.")])
                        (for [{:keys [reviewed_at rating]} (:reviews idea)]
                          [reviewed_at (str "You checked a card: "
                                         (case (long rating) 1 "again" 2 "hard" 3 "good" "easy")
                                         ".")]))
                   (sort-by first)
                   (mapv (fn [[at text]] {:date (fmt at) :text text})))
          due? (some #(when-let [d (:due %)] (.before ^java.util.Date d (java.util.Date.)))
                 (:cards idea))]
      (-> idea
        (update :marks (fn [ms] (mapv #(-> % (assoc :date (fmt (:created_at %)))
                                           (dissoc :created_at :proof_run_id))
                                  ms)))
        (update :roots (fn [rs] (mapv #(-> % (assoc :learned (fmt (:learned_at %)))
                                           (dissoc :learned_at :pins))
                                  rs)))
        (update :cards (fn [cs] (mapv #(-> % (assoc :text (text/strip-html
                                                           (or (:question %) (:cloze %)))
                                                  :checked (fmt (:last_review %))
                                                  :next (fmt (:due %)))
                                           (dissoc :last_review :due :question :cloze))
                                  cs)))
        (assoc :events events
               :last-check (some->> (:cards idea) (keep :last_review) seq (apply max-key #(.getTime ^java.util.Date %)) fmt)
               :next-check (some->> (:cards idea) (keep :due) seq (apply min-key #(.getTime ^java.util.Date %)) fmt)
               :last-rating (some-> (first (:reviews idea)) :rating long)
               :due? (boolean due?)
               :material? (some? (db/idea-material user-id entity-id)))
        (dissoc :reviews)))))

;; ── Writes ─────────────────────────────────────────────────────────

(defn not-learned!
  "Not learned yet on `user-id`'s idea (known-ideas §1.7).
   Post: {:ok true}, or {:error} when the idea is not theirs."
  [user-id entity-id]
  (if (db/unlearn-entity! user-id entity-id)
    (do (commands/bump! user-id :mark-idea-not-learned) {:ok true})
    {:error "That idea could not be changed."}))

(defn undo-self-mark!
  "Undo my mark on `user-id`'s idea: the `self` mark only (§1.8).
   Post: {:ok true}, or {:error} when there was no such mark."
  [user-id entity-id]
  (if (pos? (or (db/delete-known-mark! user-id entity-id "self") 0))
    (do (commands/bump! user-id :undo-idea-self-mark) {:ok true})
    {:error "There was no mark of yours to undo."}))

(defn open-proof-chat!
  "Make the chat of `user-id`'s proof the one the panel opens on its document
   (§5.3.2). Post: the chat's document topic id, or nil."
  [user-id entity-id]
  (when-let [{:keys [chat_id root_topic_id]} (:proof (db/known-idea user-id entity-id))]
    (when (db/mark-assistant-chat-opened! user-id chat_id)
      root_topic_id)))

(def idea-card-count 3)

(defn- material-html
  "The content of a generation from `material`: each quote as a paragraph."
  [{:keys [quotes]}]
  (cleaner/clean-html-fragment
    (str/join "" (map #(str "<p>" (str/escape % {\< "&lt;" \> "&gt;" \& "&amp;"}) "</p>")
                   quotes))))

(defn- material-context
  "The context of a generation: the idea's label and the reader's questions."
  [{:keys [label questions]}]
  (str "The idea: " label "."
    (when (seq questions)
      (str "\nThe reader asked about these passages:\n"
        (str/join "\n" (map #(str "- " %) questions))))))

(defn make-cards!
  "Queue 3 basic cards from `user-id`'s idea (known-ideas §2): its material,
   on its target topic, linked to the idea when saved. The run has the
   `:card-gen` Activity row, Cancel and a toast, and the `:cards.basic` charge.
   Post: {:ok true} once queued, or {:error} when the idea has no material."
  [user-id entity-id]
  (if-let [material (db/idea-material user-id entity-id)]
    (do (card-gen/enqueue-card-gen!
          {:id (str (random-uuid))
           :content (material-html material)
           :context (material-context material)
           :card-type "basic"
           :card-count idea-card-count
           :user-id user-id
           :enc-key nil
           :topic-id (:topic-id material)
           :root-topic-id (:root-topic-id material)
           :entity-id entity-id
           :pre-prompt nil
           :pre-prompt-mode nil
           :success-toast? true})
        {:ok true})
    {:error "There is no passage to make cards from."}))

(defn check-card-ids
  "The cards a check of `user-id`'s ideas `entity-ids` reviews (§4.1)."
  [user-id entity-ids]
  (db/entity-card-ids user-id entity-ids))
