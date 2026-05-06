(ns freememo.pdf
  "Business logic for PDF document management."
  (:require
    [freememo.db :as db]
    [freememo.ocr :as ocr]
    [freememo.quota :as quota]
    [freememo.user-state :as us]
    [taoensso.telemere :as tel]))

(defn pdf-magic-bytes?
  "True iff bytes start with %PDF- (the PDF file header)."
  [^bytes b]
  (and (>= (alength b) 5)
    (= (byte 0x25) (aget b 0))   ;; %
    (= (byte 0x50) (aget b 1))   ;; P
    (= (byte 0x44) (aget b 2))   ;; D
    (= (byte 0x46) (aget b 3))   ;; F
    (= (byte 0x2D) (aget b 4)))) ;; -

(defn save-pdf [user-id filename file-bytes]
  (try
    (let [file-size (alength file-bytes)]
      (cond
        (not (pdf-magic-bytes? file-bytes))
        {:success false :error "Not a valid PDF file" :code "invalid-file-type"}

        ;; Legacy backstop — should never trigger if the Ring layer is present.
        (> file-size (* 100 1024 1024))
        {:success false :error "File exceeds 100 MB limit"
         :code "file-too-large" :limit (* 100 1024 1024) :incoming file-size}

        :else
        (let [check (quota/check-quota user-id file-size)]
          (if-not (:ok check)
            (case (:reason check)
              :file-too-large {:success false
                               :error (str "File exceeds per-file limit (" (:limit check) " bytes)")
                               :code "file-too-large"
                               :limit (:limit check) :incoming (:incoming check)}
              :over-quota {:success false
                           :error "Storage quota exceeded — delete documents to free space"
                           :code "over-quota"
                           :used (:used check) :limit (:limit check) :incoming (:incoming check)})
            (let [page-count (ocr/get-page-count file-bytes)
                  topic (db/create-pdf-topic! user-id filename file-bytes file-size page-count)
                  topic-id (:topics/id topic)]
              (swap! (us/get-atom user-id :refresh) inc)
              {:success true :id topic-id})))))
    (catch Exception e
      (tel/error! {:id ::save-pdf} e)
      {:success false :error (str "Failed to save PDF: " (.getMessage e))})))

(defn list-pdfs [user-id]
  (try
    (let [docs (db/get-root-topics user-id)]
      {:success true :documents docs})
    (catch Exception e
      (tel/error! {:id ::list-pdfs} e)
      {:success false :error "Failed to load PDFs"})))

(defn delete-pdf [user-id id]
  (try
    (db/delete-topic-for-user! user-id id)
    {:success true}
    (catch Exception e
      (tel/error! {:id ::delete-pdf} e)
      {:success false :error "Failed to delete PDF"})))
