(ns freememo.comprehension-ocr
  "Answer-specific OCR. Original scans never replace learning material."
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [freememo.answer-sheet :as answer-sheet]
            [freememo.cards :as cards]
            [freememo.commands :as commands]
            [freememo.card-models :as card-models]
            [freememo.comprehension :as comprehension]
            [freememo.credits :as credits]
            [freememo.db :as db]
            [freememo.kg-llm :as llm]
            [freememo.llm-json :as llm-json]
            [freememo.reply-schema :as reply-schema
             :refer [strict-object array-of json-string json-boolean]]
            [freememo.ocr :as ocr]
            [freememo.ocr-models :as ocr-models]
            [freememo.openrouter :as openrouter]
            [freememo.settings :as settings]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import [java.io ByteArrayInputStream]
           [javax.imageio ImageIO]
           [java.util Base64]
           [org.postgresql.util PGobject]))

(def ^:private opts {:builder-fn rs/as-unqualified-maps})
(def max-upload-bytes (* 20 1024 1024))
(def max-pages 20)

(defn- encoded [value]
  (doto (PGobject.) (.setType "jsonb") (.setValue (json/generate-string value))))

(defn- decode-row [row]
  (when row (update-vals row #(if (instance? PGobject %)
                               (json/parse-string-strict (.getValue ^PGobject %) true) %))))

(defn- result [f]
  (try (f) (catch Exception e {:ok false :error (or (ex-message e) "The answer scan failed.")})))

(defn- require-cycle!
  ([user-id cycle-id] (require-cycle! db/ds user-id cycle-id))
  ([connectable user-id cycle-id]
   (or (jdbc/execute-one! connectable
         ["SELECT id, form, draft_answers FROM comprehension_cycles WHERE id = ? AND user_id = ? FOR UPDATE"
          cycle-id user-id] opts)
       (throw (ex-info "This study cycle is not available." {})))))

(defn scan-detail
  "Return scan metadata only when the learner owns its cycle."
  ([user-id scan-id] (scan-detail db/ds user-id scan-id))
  ([connectable user-id scan-id]
   (decode-row
    (jdbc/execute-one! connectable
      ["SELECT s.id, s.cycle_id, s.filename, s.content_type, s.page_count, s.status,
          s.suggestions, s.uncertainties, s.accepted_assignments, s.accepted_at, s.error
         FROM comprehension_scans s JOIN comprehension_cycles c ON c.id = s.cycle_id
        WHERE s.id = ? AND c.user_id = ?" scan-id user-id] opts))))

(defn scan-original
  "Return the unchanged authorized upload. The HTTP caller must set private cache headers."
  [user-id scan-id]
  (when-let [row (jdbc/execute-one! db/ds
                  ["SELECT s.original_bytes AS bytes, s.content_type, s.filename
                     FROM comprehension_scans s JOIN comprehension_cycles c ON c.id = s.cycle_id
                    WHERE s.id = ? AND c.user_id = ?" scan-id user-id] opts)]
    row))

(defn delete-scan!
  "Remove an unaccepted scan. Submitted answer evidence cannot be removed through this action."
  [user-id cycle-id scan-id]
  (result
    #(let [deleted (jdbc/execute-one! db/ds
                     ["DELETE FROM comprehension_scans s USING comprehension_cycles c
                        WHERE s.id = ? AND s.cycle_id = ? AND c.id = s.cycle_id
                          AND c.user_id = ? AND s.status IN ('uploaded','failed','review')
                          AND NOT EXISTS (
                            SELECT 1 FROM comprehension_submissions a,
                              jsonb_array_elements(a.scan_assignments) assignment
                             WHERE a.cycle_id = c.id AND assignment->>'id' = CAST(s.id AS text))
                        RETURNING s.id" scan-id cycle-id user-id] opts)]
       (when-not deleted
         (throw (ex-info "Only an unaccepted scan outside an active transcription can be removed." {})))
       {:ok true})))

(defn- image-page-count! [bytes content-type]
  (with-open [stream (ImageIO/createImageInputStream (ByteArrayInputStream. bytes))]
    (let [readers (ImageIO/getImageReaders stream)]
      (when-not (.hasNext readers)
        (throw (ex-info "Upload a readable PNG or JPEG image." {})))
      (let [reader (.next readers)]
        (try
          (.setInput reader stream)
          (let [format (str/lower-case (.getFormatName reader))
                expected (if (= content-type "image/png") "png" "jpeg")]
            (when-not (= expected format)
              (throw (ex-info "The image does not match its declared file type." {})))
            (when (> (* (long (.getWidth reader 0)) (long (.getHeight reader 0))) 40000000)
              (throw (ex-info "Use an image with at most 40 million pixels." {}))))
          1
          (finally (.dispose reader)))))))

(defn stage-scan!
  "Validate and save original bytes before OCR. Pre: learner owns the target cycle."
  [user-id cycle-id bytes content-type filename]
  (result
    #(do
       (require-cycle! user-id cycle-id)
       (when-not (and (bytes? bytes) (pos? (alength ^bytes bytes))
                   (<= (alength ^bytes bytes) max-upload-bytes))
         (throw (ex-info "Upload a nonempty file of at most 20 MB." {})))
       (when-not (contains? #{"application/pdf" "image/png" "image/jpeg"} content-type)
         (throw (ex-info "Upload a PDF, PNG, or JPEG answer scan." {})))
       (let [page-count (if (= "application/pdf" content-type)
                          (ocr/get-page-count bytes) (image-page-count! bytes content-type))
             filename (-> (or filename "answers") (str/split #"[/\\]") last
                        (str/replace #"[\p{Cntrl}]" ""))
             filename (subs filename 0 (min 160 (count filename)))]
         (when-not (<= 1 page-count max-pages)
           (throw (ex-info "Upload at most 20 answer pages per file." {})))
         (jdbc/with-transaction [tx db/ds]
           (jdbc/execute-one! tx ["SELECT pg_advisory_xact_lock(?)" (long user-id)])
           (let [used (:bytes (jdbc/execute-one! tx
                                ["SELECT COALESCE(sum(octet_length(s.original_bytes)),0) AS bytes
                                   FROM comprehension_scans s JOIN comprehension_cycles c ON c.id = s.cycle_id
                                  WHERE c.user_id = ?" user-id] opts))]
             (when (> (+ used (alength ^bytes bytes)) (* 100 1024 1024))
               (throw (ex-info "Answer scans can use at most 100 MB per account." {})))
             (let [row (jdbc/execute-one! tx
                         ["INSERT INTO comprehension_scans
                            (cycle_id, filename, content_type, original_bytes, page_count)
                          VALUES (?, ?, ?, ?, ?) RETURNING id"
                          cycle-id filename content-type bytes page-count] opts)]
               {:ok true :scan-id (:id row)})))))))

(def transcript-reply
  "The reply contract of comprehension-answer-ocr.md. validate-suggestions!
   still rejects a question id that is not on the form."
  (reply-schema/contract "answer_transcript" :transcript
    (strict-object {:answers (array-of (strict-object {:question_id json-string
                                                       :text json-string
                                                       :uncertain json-boolean}))
                    :uncertainties (array-of json-string)})))

(defn validate-suggestions!
  "Reject invented question ids. Page numbers always come from the scanned page."
  [question-ids page-number transcript]
  (when-not (and (vector? (:answers transcript)) (vector? (:uncertainties transcript)))
    (throw (ex-info "The OCR response omitted its answer assignments." {})))
  (mapv (fn [{:keys [question_id text uncertain] :as answer}]
          (when-not (and (contains? question-ids question_id) (string? text)
                      (<= (count text) 50000) (boolean? uncertain))
            (throw (ex-info "The OCR response contains an invalid answer assignment." {})))
          (assoc (select-keys answer [:question_id :text])
            :page_numbers [page-number] :uncertain uncertain))
    (:answers transcript)))

(defn ocr-question-list
  "The question list the OCR prompt reads, in printed order.
   Post: each entry has the printed :number, the stable :id and the :prompt.
   A question with a row on the bubble page also has :bubble_letters, the
   letters printed in its circles, so a filled circle reads as an option id."
  [questions]
  (mapv (fn [index q]
          (cond-> {:number (inc index) :id (:id q) :prompt (:prompt q)}
            (answer-sheet/bubbled? q) (assoc :bubble_letters (mapv :id (:options q)))))
    (range) questions))

(defn- page-image [bytes content-type page-index]
  (if (= "application/pdf" content-type)
    (:image-url (ocr/prepare-ocr-page bytes page-index 130 #{:chat}))
    (str "data:" content-type ";base64," (.encodeToString (Base64/getEncoder) bytes))))

(defn- answer-ocr-model! [user-id]
  (let [api-key (settings/get-openrouter-api-key user-id)
        gate (credits/check-cost-billed-balance! user-id)
        selected (ocr/effective-ocr-model user-id nil)
        allowed (set (ocr/allowed-ocr-model-ids))
        entry (if (= :chat (:shape selected)) selected
                (or (some #(when (and (= :chat (:shape %)) (contains? allowed (:id %))) %)
                      (sort-by #(if (= ocr-models/default-id (:id %)) 0 1) ocr-models/registry))
                    (throw (ex-info "Answer transcripts need an allowed vision OCR model." {}))))
        capabilities (card-models/resolve-model (:id entry))]
    (when (str/blank? api-key)
      (throw (ex-info "Configure an OpenRouter API key before transcribing answers." {})))
    (when-not (:ok gate) (throw (ex-info (:error gate) {})))
    (when-not capabilities
      (throw (ex-info "The selected OCR model has no structured answer-transcription capability." {})))
    ;; :entry is the card-models entry of the model the request names: chat!
    ;; reads its :supported-parameters, and resolve-model names it as the model
    ;; to bill.
    {:api-key api-key :entry capabilities
     :model-fields (openrouter/structured-chat-completion-fields capabilities {})}))

(defn transcribe-scan!
  "Suggest literal transcripts. Post: only explicit learner acceptance creates exam answers.
   on-claimed, when given, runs once this run owns the scan's `transcribing` status."
  ([user-id cycle-id scan-id] (transcribe-scan! user-id cycle-id scan-id nil))
  ([user-id cycle-id scan-id on-claimed]
    (comprehension/with-work! user-id [:ocr scan-id]
      #(let [cycle (decode-row (require-cycle! user-id cycle-id))
             scan (or (scan-detail user-id scan-id)
                      (throw (ex-info "This answer scan is not available." {})))]
         (when-not (= cycle-id (:cycle_id scan))
           (throw (ex-info "This scan belongs to a different study cycle." {})))
         (when (= "accepted" (:status scan))
           (throw (ex-info "This transcript is already accepted. Upload another scan for a later answer." {})))
         (let [token (random-uuid)
               claimed (jdbc/execute-one! db/ds
                         ["UPDATE comprehension_scans SET status = 'transcribing', error = NULL,
                            transcription_token = ?, transcription_started_at = now()
                            WHERE id = ? AND (status IN ('uploaded','failed','review') OR
                              (status = 'transcribing' AND (transcription_started_at IS NULL OR
                                transcription_started_at < now() - interval '20 minutes'))) RETURNING id"
                          token scan-id] opts)]
           (when-not claimed
             (throw (ex-info "This scan is already being transcribed." {})))
           (when on-claimed (on-claimed))
           (try
             (let [{:keys [bytes content_type]} (scan-original user-id scan-id)
                   {:keys [entry] :as resolved} (answer-ocr-model! user-id)
                   questions (ocr-question-list (get-in cycle [:form :questions]))
                   ids (set (map :id questions))
                   pages (mapv (fn [page-index]
                                 (llm/interrupt-checkpoint!)
                                 (let [page-number (inc page-index)
                                       {:keys [parsed cost tokens]}
                                       (llm/chat! resolved
                                         (cards/load-prompt-template "comprehension-answer-ocr.md")
                                         [{:type "text" :text (llm-json/write-prompt-input
                                                                {:questions questions :page_number page-number})}
                                          {:type "image_url"
                                           :image_url {:url (page-image bytes content_type page-index)}}]
                                         transcript-reply {:feature :comprehension-ocr :user-id user-id})]
                                   (credits/record-cost-charge! user-id :comprehension.answer-ocr (:id entry) cost tokens)
                                   {:answers (validate-suggestions! ids page-number parsed)
                                    :uncertainties (mapv (fn [text] {:page_number page-number :reason (str text)})
                                                     (:uncertainties parsed))}))
                           (range (:page_count scan)))
                   suggestions (->> pages (mapcat :answers) (group-by :question_id)
                                 (mapv (fn [[question-id segments]]
                                         {:question_id question-id
                                          :text (str/join "\n\n" (map :text segments))
                                          :page_numbers (vec (distinct (mapcat :page_numbers segments)))
                                          :uncertain (boolean (some :uncertain segments))})))
                   uncertainties (vec (mapcat :uncertainties pages))]
               (when-not (jdbc/execute-one! db/ds
                           ["UPDATE comprehension_scans SET suggestions = ?, uncertainties = ?, status = 'review'
                              WHERE id = ? AND status = 'transcribing' AND transcription_token = ? RETURNING id"
                            (encoded suggestions) (encoded uncertainties) scan-id token] opts)
                 (throw (ex-info "A newer transcription run owns this scan. Refresh to inspect its result." {})))
               {:ok true :suggestions suggestions :uncertainties uncertainties})
             (catch Exception e
               (jdbc/execute-one! db/ds
                 ["UPDATE comprehension_scans SET status = 'failed', error = ?
                    WHERE id = ? AND transcription_token = ? AND status = 'transcribing'"
                  (ex-message e) scan-id token])
               (throw e))))))))

(def claim-wait-ms
  "How long an upload response waits for its transcription to claim the scan."
  10000)

(defn start-transcription!
  "Transcribe a newly staged scan on a server thread, so OCR does not depend on the tab.
   Pre: the learner owns the cycle, and stage-scan! saved scan-id in it.
   Post: returns after the run claims the scan, ends, or claim-wait-ms passes.
   A page that reads after the return sees `transcribing`, not `uploaded`.
   When the run ends, `:comprehension-mutations` bumps so the page reads the result.
   A failed run records its error on the scan row. It never throws into the caller."
  [user-id cycle-id scan-id]
  (let [claimed (promise)]
    (future
      (try
        (transcribe-scan! user-id cycle-id scan-id #(deliver claimed true))
        (catch Throwable _ nil)
        (finally
          (deliver claimed false)
          (commands/bump-channels! user-id #{:comprehension-mutations}))))
    (deref claimed claim-wait-ms false)
    nil))

(defn normalize-assignments!
  "Accept only known questions and pages. Unclear text remains explicit evidence uncertainty."
  [question-ids page-count assignments]
  (when-not (and (vector? assignments) (seq assignments))
    (throw (ex-info "Assign at least one answer before accepting this transcript." {})))
  (mapv (fn [{:keys [question_id text page_numbers uncertain]}]
          (when-not (and (contains? question-ids question_id) (string? text)
                      (<= (count text) 50000) (vector? page_numbers) (seq page_numbers)
                      (every? #(and (integer? %) (<= 1 % page-count)) page_numbers))
            (throw (ex-info "Each transcript needs a valid question, answer text, and page assignment." {})))
          {:question_id question_id :text text :page_numbers (vec (sort (distinct page_numbers)))
           :uncertain (boolean (or uncertain (re-find #"(?i)\[\s*(unclear|illegible|unreadable)[^\]]*\]" text)))})
    assignments))

(defn accept-scan!
  "Save reviewed assignments and literal corrections.
   Pre: the learner owns this cycle and has reviewed this scan.
   Post: only this scan's old and new question assignments update draft answers.
   Invariant: unrelated saved answers and original evidence stay unchanged.
   This function owns lost draft edits."
  [user-id cycle-id scan-id assignments]
  (result
    (fn [] (jdbc/with-transaction [tx db/ds]
       (let [cycle (decode-row (require-cycle! tx user-id cycle-id))
             scan (or (scan-detail tx user-id scan-id)
                      (throw (ex-info "This answer scan is not available." {})))]
         (when-not (and (= cycle-id (:cycle_id scan))
                     (contains? #{"review" "accepted"} (:status scan)))
           (throw (ex-info "Transcribe this cycle's scan before accepting its answers." {})))
         (let [ids (set (map :id (get-in cycle [:form :questions])))
               assignments (normalize-assignments! ids (:page_count scan) assignments)
               accepted-scans (mapv decode-row (jdbc/execute! tx
                                                 ["SELECT id, accepted_assignments FROM comprehension_scans
                                                    WHERE cycle_id = ? AND status = 'accepted' ORDER BY id"
                                                  cycle-id] opts))
               affected-questions (into (set (map :question_id (:accepted_assignments scan)))
                                    (map :question_id assignments))
               updated-scans (sort-by :id
                               (conj (filterv (fn [row] (not= scan-id (:id row))) accepted-scans)
                                 {:id scan-id :accepted_assignments assignments}))
               transcripts (comprehension/accepted-scan-answers updated-scans)
               current (into {} (map (fn [[k v]] [(name k) v])) (:draft_answers cycle))
               answers (reduce (fn [answers question-id]
                                 (assoc answers question-id (get transcripts question-id "")))
                         current affected-questions)]
           (jdbc/execute-one! tx
             ["UPDATE comprehension_scans SET accepted_assignments = ?, accepted_at = now(), status = 'accepted'
                WHERE id = ?" (encoded assignments) scan-id])
           (jdbc/execute-one! tx
             ["UPDATE comprehension_cycles SET draft_answers = ?, draft_kind = 'transcription-correction'
                WHERE id = ? AND user_id = ?" (encoded answers) cycle-id user-id])
           {:ok true :answers answers}))))))
