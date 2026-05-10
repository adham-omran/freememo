(ns freememo.import-page
  "Import tab — Upload PDF, paste articles, or import from URL."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.wikipedia :as wiki])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.extractor :as extractor])
   #?(:clj [freememo.markdown :as md])))

;; Client-visible upload cap. Must match `api/upload-byte-cap` (server-side).
;; Client-side guard short-circuits oversized files before fetch — avoids the
;; mid-upload connection drop that the server's 413 path triggers in the browser.
(def upload-max-bytes 104857600) ;; 100 MB
(def upload-max-label "100 MB")

#?(:cljs
   (defn- format-mb
     "Render a byte count as e.g. \"137 MB\" for error messages."
     [bytes]
     (let [mb (/ (double (or bytes 0)) 1048576.0)]
       (if (< mb 10)
         (str (.toFixed mb 1) " MB")
         (str (Math/round mb) " MB")))))

;; Server-side wrappers
(defn format-timestamp* [ts]
  #?(:clj (when ts
            (let [fmt (java.time.format.DateTimeFormatter/ofPattern "MMM d, yyyy")
                  instant (.toInstant ts)
                  ldt (java.time.LocalDateTime/ofInstant instant (java.time.ZoneId/systemDefault))]
              (.format ldt fmt)))
     :cljs nil))

(defn create-web-topic* [user-id title html source-url]
  #?(:clj (db/create-web-topic! user-id title (cleaner/clean-html html) source-url)
     :cljs nil))

(defn clean-html* [html]
  #?(:clj (cleaner/clean-html html)
     :cljs nil))

(defn fetch-url* [url]
  #?(:clj (wiki/fetch-url url)
     :cljs nil))

(defn try-auto-extract*
  "Best-effort auto-extract: segments HTML into topics, highlights section headings
   in the topic HTML, and batch-inserts child topics. Returns nil.
   Logs and swallows errors so the import still succeeds."
  [topic-id html]
  #?(:clj (try
            (let [start (System/currentTimeMillis)
                  result (extractor/extract-and-annotate html)
                  elapsed (- (System/currentTimeMillis) start)]
              (when result
                (let [{:keys [sections annotated-html]} result
                      clean-sections (mapv #(update % :content cleaner/clean-html) sections)]
                  (log/log-info (str "Auto-extract complete section-count=" (count clean-sections) " elapsed-ms=" elapsed))
                  ;; Save child topics
                  (when (seq clean-sections)
                    (db/batch-create-topics! topic-id clean-sections))
                  ;; Update topic content with highlighted HTML.
                  ;; Sanitize: cleaner now permits style="background-color" so the
                  ;; extract-highlight spans survive while scripts/handlers don't.
                  (when annotated-html
                    (db/update-topic-content! topic-id (cleaner/clean-html annotated-html)))))
              nil)
            (catch Exception e
              (log/log-warn (str "Auto-extract failed: " (.getMessage e)))
              nil))
     :cljs nil))

(defn parse-markdown* [markdown-string]
  #?(:clj (md/parse-markdown markdown-string)
     :cljs nil))

(defn extract-frontmatter-title* [markdown-string]
  #?(:clj (md/extract-frontmatter-title markdown-string)
     :cljs nil))

(defn create-markdown-topic* [user-id title html-content]
  #?(:clj (db/create-markdown-topic! user-id title html-content)
     :cljs nil))

(defn markdown-default-title []
  #?(:clj (let [now (java.time.LocalDateTime/now)
                fmt (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
            (str "Markdown " (.format now fmt)))
     :cljs nil))

;; Paste Import Modal
(e/defn PasteImportModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          !title (atom "")
          !url (atom "")
          !html (atom nil)
          !auto-extract (atom true)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg" :style {:max-height "80vh" :display "flex" :flex-direction "column"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import Web Article"))

          ;; Title
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Title"))
          (dom/input
            (dom/props {:type "text" :placeholder "Article title"
                        :value (e/watch !title)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

          ;; Source URL (optional)
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Source URL (optional)"))
          (dom/input
            (dom/props {:type "text" :placeholder "https://..."
                        :value (e/watch !url)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))

          ;; Paste area
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Content (paste from browser with Ctrl+V)"))
          (dom/div
            (dom/props {:contenteditable "true"
                        :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                                :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                :font-size "14px" :line-height "1.6" :margin-bottom "var(--sp-4)"
                                :background "var(--color-bg-subtle)"}})
            (dom/On "paste"
              (fn [e]
                (let [cd (.-clipboardData e)
                      html-data (.getData cd "text/html")
                      text-data (.getData cd "text/plain")
                      content (if (seq html-data) html-data (str "<p>" text-data "</p>"))]
                  (reset! !html content)
                  ;; Auto-fill title from first heading if empty
                  #?(:cljs
                     (when (empty? @!title)
                       (let [tmp (js/document.createElement "div")]
                         (set! (.-innerHTML tmp) content)
                         (when-let [h (or (.querySelector tmp "h1")
                                        (.querySelector tmp "h2")
                                        (.querySelector tmp "h3"))]
                           (reset! !title (.-textContent h))))))))
              nil))

          ;; Options
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "8px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
              (dom/input (dom/props {:type "checkbox"})
                (set! (.-checked dom/node) (e/watch !auto-extract))
                (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
              (dom/text "Auto-extract into topics")))

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :html @!html :url @!url
                                     :auto-extract @!auto-extract})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:disabled importing?
                            :style {:cursor (if importing? "not-allowed" "pointer")}})
                (dom/text (if importing? "Processing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [title-val (:title event)
                        html-val (:html event)
                        url-val (:url event)
                        ae? (:auto-extract event)]
                    (if (and (seq title-val) (seq html-val))
                      (let [topic-id (e/server
                                       (let [tid (create-web-topic* user-id title-val html-val
                                                   (when (seq url-val) url-val))]
                                         (when (and ae? tid)
                                           (try-auto-extract*
                                             tid
                                             (clean-html* html-val)))
                                         tid))]
                        (if topic-id
                          (do (swap! !mutations inc)
                            (token)
                            (reset! !show false)
                            (navigate! :viewer (nav/nav-browse-topic topic-id nil)))
                          (token "Failed to save article")))
                      (token "Title and content are required"))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; URL Import Modal — accepts any URL, auto-detects Wikipedia
(e/defn URLImportModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          !url (atom "")]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import from URL"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text "Paste a Wikipedia or any web page URL. Wikipedia articles are fetched via API for best quality."))
          (dom/input
            (dom/props {:type "text" :placeholder "https://en.wikipedia.org/wiki/..."
                        :value (e/watch !url)
                        :class "input input-full" :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil))

          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (let [event (dom/On "click"
                            (fn [_] {:url @!url})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (dom/props {:class "btn btn-primary"
                            :style {:font-weight "600" :order "1"
                                    :cursor (if importing? "not-allowed" "pointer")}
                            :disabled importing?})
                (dom/text (if importing? "Processing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [url-val (:url event)]
                    (if (seq url-val)
                      (let [result (e/server
                                     (e/Offload
                                       #(do
                                          (log/log-info (str "URL import attempt url=" url-val))
                                          (let [fetch-result (fetch-url* url-val)]
                                            (if (false? (:success fetch-result))
                                              (do
                                                (log/log-warn (str "URL import fetch failed url=" url-val " error=" (:error fetch-result)))
                                                (throw (ex-info (:error fetch-result) {:type :url-import-failed})))
                                              (do
                                                (log/log-info (str "URL import fetch ok url=" url-val " title=" (:title fetch-result) " source-type=" (:source-type fetch-result)))
                                                (let [topic-id (create-web-topic* user-id
                                                                 (:title fetch-result)
                                                                 (:html fetch-result)
                                                                 (:url fetch-result))]
                                                  (if topic-id
                                                    (do (log/log-info (str "URL import topic created topic-id=" topic-id " url=" url-val))
                                                        {:topic-id topic-id})
                                                    (do (log/log-warn (str "URL import topic creation returned nil url=" url-val))
                                                        (throw (ex-info "Failed to save article" {:type :url-import-failed})))))))))))]
                        ;; Guard: e/Offload returns nil while pending. Without this,
                        ;; side effects fire immediately on click (modal closes
                        ;; before fetch completes, navigate! receives nil topic-id).
                        ;; Pattern matches extract_page.cljc:139.
                        (when (some? result)
                          (swap! !mutations inc)
                          (token)
                          (reset! !url "")
                          (reset! !show false)
                          (navigate! :viewer (nav/nav-browse-topic (:topic-id result) nil))))
                      (token "Please enter a URL"))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

(e/defn UploadPDFModal [!show navigate!]
  (e/client
    (let [!file-input (atom nil)
          !uploading (atom false)
          !error-msg (atom nil)
          !quota-error? (atom false)
          uploading (e/watch !uploading)
          error-msg (e/watch !error-msg)
          quota-error? (e/watch !quota-error?)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Upload PDF"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text (str "Select a PDF file from your device. Maximum " upload-max-label ".")))

          ;; Shared upload fn
          (let [do-upload!
                (fn [file]
                  (when (and file (not @!uploading))
                    (cond
                      (> (.-size file) upload-max-bytes)
                      (do (reset! !quota-error? false)
                          (reset! !error-msg
                            (str "File is " (format-mb (.-size file))
                              " — limit is " upload-max-label
                              ". Try a smaller file.")))

                      :else
                      (do
                        (reset! !uploading true)
                        (reset! !error-msg nil)
                        (reset! !quota-error? false)
                        (let [form-data (js/FormData.)]
                          (.append form-data "file" file)
                          (-> (js/fetch "/api/upload-pdf" (clj->js {:method "POST" :body form-data}))
                            (.then (fn [resp] (.json resp)))
                            (.then (fn [^js data]
                                     (reset! !uploading false)
                                     (if (.-success data)
                                       (do (reset! !show false)
                                         (navigate! :viewer (nav/nav-browse-pdf (or (.-topic_id data) (.-doc_id data)) nil nil)))
                                       (let [code (.-code data)]
                                         (reset! !quota-error? (or (= code "over-quota") (= code "file-too-large")))
                                         (reset! !error-msg (or (.-error data) "PDF upload failed"))))))
                            (.catch (fn [err]
                                      (reset! !uploading false)
                                      (reset! !error-msg "Upload failed — please try again")
                                      (js/console.error "PDF upload failed:" err)))))))))]

            ;; Drop zone / file picker
            (dom/div
              (dom/props {:style {:border "2px dashed var(--color-border)" :border-radius "var(--radius-md)"
                                  :padding "32px" :text-align "center" :cursor "pointer"
                                  :margin-bottom "var(--sp-3)" :transition "border-color 0.15s, background 0.15s"}})
              (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil)
              (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")) nil)
              (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                     (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "dragover" (fn [e] (.preventDefault e)
                                   (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-background (.-style (.-currentTarget e))) "var(--color-bg-subtle)")) nil)
              (dom/On "dragleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                    (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "drop" (fn [e] (.preventDefault e)
                               (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                               (set! (.-background (.-style (.-currentTarget e))) "")
                               (do-upload! (-> e .-dataTransfer .-files (aget 0)))) nil)
              (dom/div
                (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                (dom/text "Drop a PDF here or click to browse")))

            ;; Hidden file input
            (dom/input
              (dom/props {:type "file" :accept "application/pdf" :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change" (fn [e] (do-upload! (-> e .-target .-files (aget 0)))) nil))

            ;; Error display — quota errors get a Library link.
            (when error-msg
              (dom/div
                (dom/props {:style {:padding "10px 12px" :margin-bottom "var(--sp-3)"
                                    :background "var(--color-danger-bg, #fee)"
                                    :border-radius "var(--radius-sm)"
                                    :color "var(--color-danger)" :font-size "13px"}})
                (dom/text error-msg)
                (when quota-error?
                  (dom/div
                    (dom/props {:style {:margin-top "8px"}})
                    (dom/button
                      (dom/props {:style {:padding "4px 12px" :font-size "12px"
                                          :background "transparent" :border "1px solid var(--color-danger)"
                                          :color "var(--color-danger)" :border-radius "3px" :cursor "pointer"}})
                      (dom/text "Manage Library")
                      (dom/On "click" (fn [_] (reset! !show false) (navigate! :library)) nil))))))

            ;; Buttons
            (dom/div
              (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
              (dom/button
                (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}
                            :disabled uploading})
                (dom/text (if uploading "Uploading..." "Upload"))
                (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil))
              (dom/button
                (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !show false)) nil)))))))))

(e/defn UploadEPUBModal [!show navigate!]
  (e/client
    (let [!file-input (atom nil)
          !auto-extract (atom false)
          !image-mode (atom "reduce")
          !uploading (atom false)
          !error-msg (atom nil)
          !quota-error? (atom false)
          uploading (e/watch !uploading)
          error-msg (e/watch !error-msg)
          quota-error? (e/watch !quota-error?)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Upload EPUB"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text (str "Select an EPUB ebook file. Text and images will be extracted automatically. Maximum " upload-max-label ".")))

          ;; Shared upload fn
          (let [do-upload!
                (fn [file]
                  (when (and file (not @!uploading))
                    (cond
                      (> (.-size file) upload-max-bytes)
                      (do (reset! !quota-error? false)
                          (reset! !error-msg
                            (str "File is " (format-mb (.-size file))
                              " — limit is " upload-max-label
                              ". Try a smaller file.")))

                      :else
                      (do
                        (reset! !uploading true)
                        (reset! !error-msg nil)
                        (reset! !quota-error? false)
                        (let [form-data (js/FormData.)]
                          (.append form-data "file" file)
                          (.append form-data "auto_extract" (str @!auto-extract))
                          (.append form-data "image_mode" @!image-mode)
                          (-> (js/fetch "/api/upload-epub" (clj->js {:method "POST" :body form-data}))
                            (.then (fn [resp] (.json resp)))
                            (.then (fn [^js data]
                                     (reset! !uploading false)
                                     (if (.-success data)
                                       (do (reset! !show false)
                                         (navigate! :library))
                                       (let [code (.-code data)]
                                         (reset! !quota-error? (or (= code "over-quota") (= code "file-too-large")))
                                         (reset! !error-msg (or (.-error data) "EPUB import failed"))))))
                            (.catch (fn [err]
                                      (reset! !uploading false)
                                      (reset! !error-msg "Upload failed — please try again")
                                      (js/console.error "EPUB upload failed:" err)))))))))]

            ;; Drop zone / file picker
            (dom/div
              (dom/props {:style {:border "2px dashed var(--color-border)" :border-radius "var(--radius-md)"
                                  :padding "32px" :text-align "center" :cursor "pointer"
                                  :margin-bottom "var(--sp-3)" :transition "border-color 0.15s, background 0.15s"}})
              (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil)
              (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")) nil)
              (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                     (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "dragover" (fn [e] (.preventDefault e)
                                   (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-background (.-style (.-currentTarget e))) "var(--color-bg-subtle)")) nil)
              (dom/On "dragleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                    (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "drop" (fn [e] (.preventDefault e)
                               (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                               (set! (.-background (.-style (.-currentTarget e))) "")
                               (do-upload! (-> e .-dataTransfer .-files (aget 0)))) nil)
              (dom/div
                (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                (dom/text "Drop an EPUB here or click to browse")))

            ;; Hidden file input
            (dom/input
              (dom/props {:type "file" :accept ".epub,application/epub+zip" :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change" (fn [e] (do-upload! (-> e .-target .-files (aget 0)))) nil))

            ;; Options
            (dom/div
              (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "10px"}})

              ;; Image handling
              (dom/div
                (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"}})
                (dom/span
                  (dom/props {:style {:font-size "13px" :font-weight "500" :color "var(--color-text-label)"}})
                  (dom/text "Images"))
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
                  (dom/input (dom/props {:type "radio" :name "image_mode" :value "reduce" :checked true})
                    (dom/On "change" (fn [_] (reset! !image-mode "reduce")) nil))
                  (dom/text "Reduce size — shrink for faster loading"))
                (dom/label
                  (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
                  (dom/input (dom/props {:type "radio" :name "image_mode" :value "strip"})
                    (dom/On "change" (fn [_] (reset! !image-mode "strip")) nil))
                  (dom/text "Strip images — text only")))

              ;; Auto-extract
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
                (dom/input (dom/props {:type "checkbox"})
                  (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
                (dom/text "Auto-extract into topics")))

            ;; Error display
            (when error-msg
              (dom/div
                (dom/props {:style {:padding "10px 12px" :margin-bottom "var(--sp-3)"
                                    :background "var(--color-danger-bg, #fee)"
                                    :border-radius "var(--radius-sm)"
                                    :color "var(--color-danger)" :font-size "13px"}})
                (dom/text error-msg)
                (when quota-error?
                  (dom/div
                    (dom/props {:style {:margin-top "8px"}})
                    (dom/button
                      (dom/props {:style {:padding "4px 12px" :font-size "12px"
                                          :background "transparent" :border "1px solid var(--color-danger)"
                                          :color "var(--color-danger)" :border-radius "3px" :cursor "pointer"}})
                      (dom/text "Manage Library")
                      (dom/On "click" (fn [_] (reset! !show false) (navigate! :library)) nil))))))

            ;; Buttons
            (dom/div
              (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
              (dom/button
                (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}
                            :disabled uploading})
                (dom/text (if uploading "Processing..." "Upload"))
                (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil))
              (dom/button
                (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !show false)) nil)))))))))

;; Markdown File Upload Modal
(e/defn UploadMarkdownModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          !file-input (atom nil)
          !title (atom "")
          !md-text (atom nil)
          !auto-extract (atom true)
          !uploading (atom false)
          uploading (e/watch !uploading)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Upload Markdown"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text "Select a .md file. Tables, strikethrough, and task lists are supported."))

          ;; File drop zone
          (let [handle-file!
                (fn [file]
                  (when file
                    (let [reader (js/FileReader.)]
                      (set! (.-onload reader)
                        (fn [e]
                          (let [text (-> e .-target .-result)]
                            (reset! !md-text text)
                            ;; Pre-fill title: frontmatter > filename
                            (when (empty? @!title)
                              (let [fname (.-name file)
                                    base (subs fname 0 (max 0 (- (count fname) 3)))]
                                (reset! !title base))))))
                      (.readAsText reader file))))]

            (dom/div
              (dom/props {:style {:border "2px dashed var(--color-border)" :border-radius "var(--radius-md)"
                                  :padding "32px" :text-align "center" :cursor "pointer"
                                  :margin-bottom "var(--sp-3)" :transition "border-color 0.15s, background 0.15s"}})
              (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil)
              (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")) nil)
              (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                     (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "dragover" (fn [e] (.preventDefault e)
                                   (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-background (.-style (.-currentTarget e))) "var(--color-bg-subtle)")) nil)
              (dom/On "dragleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                    (set! (.-background (.-style (.-currentTarget e))) "")) nil)
              (dom/On "drop" (fn [e] (.preventDefault e)
                               (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                               (set! (.-background (.-style (.-currentTarget e))) "")
                               (handle-file! (-> e .-dataTransfer .-files (aget 0)))) nil)
              (dom/div
                (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
                (if (some? (e/watch !md-text))
                  (dom/text "File loaded. Ready to import.")
                  (dom/text "Drop a .md file here or click to browse"))))

            ;; Hidden file input
            (dom/input
              (dom/props {:type "file" :accept ".md,text/markdown" :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change" (fn [e] (handle-file! (-> e .-target .-files (aget 0)))) nil)))

          ;; Title
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Title"))
          (dom/input
            (dom/props {:type "text" :placeholder "Document title"
                        :value (e/watch !title)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

          ;; Options
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "8px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
              (dom/input (dom/props {:type "checkbox"})
                (set! (.-checked dom/node) (e/watch !auto-extract))
                (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
              (dom/text "Auto-extract into topics")))

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :md-text @!md-text
                                     :auto-extract @!auto-extract})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (let [no-title? (empty? (e/watch !title))]
                  (dom/props {:disabled (or importing? (nil? (e/watch !md-text)) no-title?)
                              :style {:cursor (if (or importing? no-title?) "not-allowed" "pointer")}})
                  (dom/text (if importing? "Processing..." "Import"))
                  (when ?error
                    (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                      (dom/text ?error)))
                  (when-some [token ?token]
                    (let [md-val (:md-text event)
                          title-val (:title event)
                          ae? (:auto-extract event)]
                      (if (seq md-val)
                        (let [topic-id (e/server
                                         (let [html (parse-markdown* md-val)
                                               tid (create-markdown-topic* user-id title-val html)]
                                           (when (and ae? tid)
                                             (try-auto-extract* tid html))
                                           tid))]
                          (if topic-id
                            (do (swap! !mutations inc)
                              (token)
                              (reset! !show false)
                              (navigate! :viewer (nav/nav-browse-topic topic-id nil)))
                            (token "Failed to import")))
                        (token "No file loaded")))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; Paste Markdown Modal
(e/defn PasteMarkdownModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          default-title (e/server (markdown-default-title))
          !title (atom (or default-title ""))
          !md-text (atom "")
          !auto-extract (atom true)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content modal-lg" :style {:max-height "80vh" :display "flex" :flex-direction "column"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "Import Markdown"))

          ;; Title
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Title"))
          (dom/input
            (dom/props {:type "text" :placeholder "Document title"
                        :value (e/watch !title)
                        :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

          ;; Markdown textarea
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Markdown content"))
          (dom/textarea
            (dom/props {:placeholder "Paste your Markdown here..."
                        :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                                :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                :font-size "13px" :font-family "monospace" :line-height "1.5" :margin-bottom "var(--sp-4)"
                                :background "var(--color-bg-subtle)" :resize "vertical"}})
            (dom/On "input" (fn [e] (reset! !md-text (-> e .-target .-value))) nil))

          ;; Options
          (dom/div
            (dom/props {:style {:margin-bottom "var(--sp-3)" :display "flex" :flex-direction "column" :gap "8px"}})
            (dom/label
              (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
              (dom/input (dom/props {:type "checkbox"})
                (set! (.-checked dom/node) (e/watch !auto-extract))
                (dom/On "change" (fn [e] (reset! !auto-extract (-> e .-target .-checked))) nil))
              (dom/text "Auto-extract into topics")))

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
              (let [event (dom/On "click"
                            (fn [_] {:title @!title :md-text @!md-text
                                     :auto-extract @!auto-extract})
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)]
                (let [no-title? (empty? (e/watch !title))]
                  (dom/props {:disabled (or importing? no-title?)
                              :style {:cursor (if (or importing? no-title?) "not-allowed" "pointer")}})
                  (dom/text (if importing? "Processing..." "Import"))
                  (when ?error
                    (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                      (dom/text ?error)))
                  (when-some [token ?token]
                    (let [md-val (:md-text event)
                          title-val (:title event)
                          ae? (:auto-extract event)]
                      (if (seq md-val)
                        (let [topic-id (e/server
                                         (let [html (parse-markdown* md-val)
                                               tid (create-markdown-topic* user-id title-val html)]
                                           (when (and ae? tid)
                                             (try-auto-extract* tid html))
                                           tid))]
                          (if topic-id
                            (do (swap! !mutations inc)
                              (token)
                              (reset! !show false)
                              (navigate! :viewer (nav/nav-browse-topic topic-id nil)))
                            (token "Failed to import")))
                        (token "Please enter some Markdown content")))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

(e/defn NewTopicModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          !title (atom "")]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class "modal-content" :style {:width "500px" :max-width "90%"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text "New Topic"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text "Create a blank topic. You can add content later."))
          (dom/input
            (dom/props {:type "text" :placeholder "Topic title..."
                        :class "input input-full" :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"}})
            (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
              (let [event (dom/On "click" (fn [_] @!title) nil)
                    [?token ?error] (e/Token event)
                    creating? (some? ?token)]
                (dom/props {:disabled creating?
                            :style {:cursor (if creating? "not-allowed" "pointer")}})
                (dom/text (if creating? "Creating..." "Create"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (let [title-val (if (seq event) event "New Topic")
                        result (e/server (db/create-standalone-topic! user-id title-val))]
                    (if (:topic-id result)
                      (do (swap! !mutations inc)
                        (token)
                        (reset! !show false)
                        (navigate! :viewer (nav/nav-browse-topic (:topic-id result) nil)))
                      (token "Failed to create topic"))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; Main Import page component
(e/defn ImportPage [user-id navigate! enc-key llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})

      ;; Import cards
      (dom/div
        (dom/props {:style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr))"
                            :gap "12px"}})

        ;; Upload PDF card
        (let [!show-pdf (atom false)
              show-pdf (e/watch !show-pdf)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-pdf true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCC4"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Upload PDF"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Select a PDF file from your device")))
          (when show-pdf
            (UploadPDFModal !show-pdf navigate!)))

        ;; Paste Article card
        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-paste true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCCB"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Paste Article"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Paste HTML content from your clipboard")))
          (when show-paste
            (PasteImportModal !show-paste user-id navigate!)))

        ;; Import URL card
        (let [!show-url (atom false)
              show-url (e/watch !show-url)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-url true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83C\uDF10"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Import URL"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Fetch from any web page or Wikipedia")))
          (when show-url
            (URLImportModal !show-url user-id navigate!)))

        ;; Upload EPUB card
        (let [!show-epub (atom false)
              show-epub (e/watch !show-epub)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-epub true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCD6"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Upload EPUB"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Import an EPUB ebook file")))
          (when show-epub
            (UploadEPUBModal !show-epub navigate!)))

        ;; Upload Markdown card
        (let [!show-md-upload (atom false)
              show-md-upload (e/watch !show-md-upload)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-md-upload true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\u2B07\uFE0F"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Upload Markdown"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Import a .md file with GFM support")))
          (when show-md-upload
            (UploadMarkdownModal !show-md-upload user-id navigate!)))

        ;; Paste Markdown card
        (let [!show-md-paste (atom false)
              show-md-paste (e/watch !show-md-paste)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-md-paste true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\uD83D\uDCDD"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "Paste Markdown"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Paste raw Markdown text")))
          (when show-md-paste
            (PasteMarkdownModal !show-md-paste user-id navigate!)))

        ;; New Topic card
        (let [!show-topic (atom false)
              show-topic (e/watch !show-topic)]
          (dom/div
            (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                                :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                                :background "var(--color-bg-surface)"}})
            (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
            (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                                   (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
            (dom/On "click" (fn [_] (reset! !show-topic true)) nil)
            (dom/div
              (dom/props {:style {:font-size "24px" :margin-bottom "8px"}})
              (dom/text "\u270F\uFE0F"))
            (dom/div
              (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}})
              (dom/text "New Topic"))
            (dom/div
              (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}})
              (dom/text "Create a blank topic to write in")))
          (when show-topic
            (NewTopicModal !show-topic user-id navigate!)))))))

