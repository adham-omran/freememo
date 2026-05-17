(ns freememo.import-page
  "Import tab — Link, Upload, Paste, New Topic."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [freememo.navigation :as nav]
   [freememo.util :as util]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.wikipedia :as wiki])
   #?(:clj [freememo.html-cleaner :as cleaner])
   #?(:clj [freememo.extractor :as extractor])
   #?(:clj [freememo.markdown :as md])))

;; Server-side accessor for the per-file upload cap. Returns 0 when unlimited
;; (env STORAGE_PER_FILE_MAX_BYTES=0). The client pulls this via e/server and
;; uses it both to short-circuit oversized files and to render the "Maximum N"
;; label.
(defn upload-cap-bytes* []
  #?(:clj (long quota/per-file-max-bytes)
     :cljs 0))

#?(:cljs
   (defn- format-mb [bytes]
     (let [mb (/ (double (or bytes 0)) 1048576.0)]
       (if (< mb 10)
         (str (.toFixed mb 1) " MB")
         (str (Math/round mb) " MB")))))

#?(:cljs
   (defn- file-ext [^js file]
     (when file
       (let [n (.-name file)
             dot (.lastIndexOf n ".")]
         (when (pos? dot)
           (.toLowerCase (subs n (inc dot))))))))

#?(:cljs
   (defn- file-stem [^js file]
     (let [n (.-name file)
           dot (.lastIndexOf n ".")]
       (if (pos? dot) (subs n 0 dot) n))))

#?(:cljs
   (defn- ext->flow [ext]
     (case ext
       "pdf" :pdf
       "epub" :epub
       ("html" "htm") :html
       ("md" "markdown") :markdown
       nil)))

#?(:cljs
   (defn- unsupported-ext-msg [ext]
     (if ext
       (str "Can't import ." ext " files. Supported: PDF, EPUB, HTML, Markdown.")
       "Can't import files without an extension. Supported: PDF, EPUB, HTML, Markdown.")))

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

;; Preserved for future re-enable; no call sites in import flows.
(defn try-auto-extract* [topic-id html]
  #?(:clj (try
            (let [start (System/currentTimeMillis)
                  result (extractor/extract-and-annotate html)
                  elapsed (- (System/currentTimeMillis) start)]
              (when result
                (let [{:keys [sections annotated-html]} result
                      clean-sections (mapv #(update % :content cleaner/clean-html) sections)]
                  (log/log-info (str "Auto-extract complete section-count=" (count clean-sections) " elapsed-ms=" elapsed))
                  (when (seq clean-sections)
                    (db/batch-create-topics! topic-id clean-sections))
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
                                                {:err (:error fetch-result)})
                                              (do
                                                (log/log-info (str "URL import fetch ok url=" url-val " title=" (:title fetch-result) " source-type=" (:source-type fetch-result)))
                                                (let [topic-id (create-web-topic* user-id
                                                                 (:title fetch-result)
                                                                 (:html fetch-result)
                                                                 (:url fetch-result))]
                                                  (if topic-id
                                                    (do (log/log-info (str "URL import topic created topic-id=" topic-id " url=" url-val))
                                                        {:ok topic-id})
                                                    (do (log/log-warn (str "URL import topic creation returned nil url=" url-val))
                                                        {:err "Failed to save article"})))))))))]
                        ;; e/Offload returns nil while pending — gate side effects.
                        ;; Failures return {:err msg} so the reactor stays alive
                        ;; (throwing inside e/Offload crashes the WS).
                        (when (some? result)
                          (if-let [tid (:ok result)]
                            (do (swap! !mutations inc)
                                (token)
                                (reset! !url "")
                                (reset! !show false)
                                (navigate! :viewer (nav/nav-topic tid nil)))
                            (token (:err result)))))
                      (token "Please enter a URL"))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; Unified Upload Modal — file extension picks the flow (PDF/EPUB/HTML/Markdown).
;; Pre: user picks a file via click or drop; size <= cap-bytes (or cap-bytes = 0 → unlimited).
;; Post: PDF/EPUB POST to /api/upload-{pdf,epub}; HTML/MD go through e/server.
;; Invariant: server failures route to ?error via (token msg) — never throw
;; inside e/Offload (would crash the reactor).
(e/defn UploadModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          !file-input (atom nil)
          !file (atom nil)
          !title (atom "")
          !html-text (atom nil)
          !md-text (atom nil)
          !image-mode (atom "reduce")
          !advanced-open (atom false)
          !uploading (atom false)
          !error-msg (atom nil)
          !quota-error? (atom false)
          file (e/watch !file)
          uploading (e/watch !uploading)
          error-msg (e/watch !error-msg)
          quota-error? (e/watch !quota-error?)
          title (e/watch !title)
          ext (when file (file-ext file))
          flow (when file (ext->flow ext))
          cap-bytes (e/server (upload-cap-bytes*))
          cap-label (when (pos? cap-bytes) (format-mb cap-bytes))]
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
            (dom/text "Upload"))
          (dom/p
            (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
            (dom/text (if cap-label
                        (str "PDF, EPUB, HTML, or Markdown — file extension picks the flow. Maximum " cap-label ".")
                        "PDF, EPUB, HTML, or Markdown — file extension picks the flow.")))

          (let [handle-file!
                (fn [f]
                  (when f
                    (reset! !error-msg nil)
                    (reset! !quota-error? false)
                    (reset! !file nil)
                    (reset! !html-text nil)
                    (reset! !md-text nil)
                    (reset! !title "")
                    (reset! !advanced-open false)
                    (cond
                      (and (pos? cap-bytes) (> (.-size f) cap-bytes))
                      (reset! !error-msg
                        (str "File is " (format-mb (.-size f))
                          " — limit is " cap-label
                          ". Try a smaller file."))

                      :else
                      (let [e (file-ext f)
                            fw (ext->flow e)]
                        (case fw
                          :pdf  (reset! !file f)
                          :epub (reset! !file f)
                          :html (let [reader (js/FileReader.)]
                                  (reset! !file f)
                                  (set! (.-onload reader)
                                    (fn [ev]
                                      (let [text (-> ev .-target .-result)]
                                        (reset! !html-text text)
                                        (when (empty? @!title)
                                          (let [doc (.parseFromString (js/DOMParser.) text "text/html")
                                                title-el (.querySelector doc "title")
                                                h (or (.querySelector doc "h1")
                                                    (.querySelector doc "h2")
                                                    (.querySelector doc "h3"))
                                                from-title (some-> title-el .-textContent .trim)
                                                from-heading (some-> h .-textContent .trim)
                                                stem (file-stem f)]
                                            (reset! !title
                                              (cond
                                                (seq from-title)   from-title
                                                (seq from-heading) from-heading
                                                :else              stem)))))))
                                  (.readAsText reader f))
                          :markdown (let [reader (js/FileReader.)]
                                      (reset! !file f)
                                      (set! (.-onload reader)
                                        (fn [ev]
                                          (let [text (-> ev .-target .-result)]
                                            (reset! !md-text text)
                                            (when (empty? @!title)
                                              (reset! !title (file-stem f))))))
                                      (.readAsText reader f))
                          (reset! !error-msg (unsupported-ext-msg e)))))))

                start-upload!
                (fn [fw f]
                  (when (and f (not @!uploading))
                    (reset! !uploading true)
                    (reset! !error-msg nil)
                    (reset! !quota-error? false)
                    (let [form-data (js/FormData.)
                          endpoint (case fw :pdf "/api/upload-pdf" :epub "/api/upload-epub")]
                      (.append form-data "file" f)
                      (when (= fw :epub)
                        (.append form-data "auto_extract" "false")
                        (.append form-data "image_mode" @!image-mode))
                      (-> (js/fetch endpoint (clj->js {:method "POST" :body form-data}))
                        (.then (fn [resp] (.json resp)))
                        (.then (fn [^js data]
                                 (reset! !uploading false)
                                 (if (.-success data)
                                   (do (reset! !show false)
                                       (case fw
                                         :pdf  (navigate! :viewer (nav/nav-topic (or (.-topic_id data) (.-doc_id data)) nil))
                                         :epub (navigate! :library)))
                                   (let [code (.-code data)]
                                     (reset! !quota-error? (or (= code "over-quota") (= code "file-too-large")))
                                     (reset! !error-msg (or (.-error data)
                                                            (case fw :pdf "PDF upload failed" :epub "EPUB import failed")))))))
                        (.catch (fn [err]
                                  (reset! !uploading false)
                                  (reset! !error-msg "Upload failed — please try again")
                                  (js/console.error "Upload failed:" err)))))))]

            ;; Drop zone
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
                (if file
                  (dom/text (str "Selected: " (.-name file)))
                  (dom/text "Drop a file here or click to browse"))))

            ;; Hidden file input
            (dom/input
              (dom/props {:type "file"
                          :accept ".pdf,.epub,.html,.htm,.md,.markdown"
                          :style {:display "none"}})
              (reset! !file-input dom/node)
              (dom/On "change" (fn [e] (handle-file! (-> e .-target .-files (aget 0)))) nil))

            ;; Title field — HTML and Markdown only
            (when (or (= flow :html) (= flow :markdown))
              (dom/label
                (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
                (dom/text "Title"))
              (dom/input
                (dom/props {:type "text" :placeholder "Document title"
                            :value title
                            :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
                (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil)))

            ;; EPUB Advanced disclosure
            (when (= flow :epub)
              (let [advanced-open (e/watch !advanced-open)
                    image-mode (e/watch !image-mode)]
                (dom/div
                  (dom/props {:style {:margin-bottom "var(--sp-3)"}})
                  (dom/button
                    (dom/props {:type "button"
                                :style {:background "transparent" :border "none" :color "var(--color-text-secondary)"
                                        :font-size "13px" :cursor "pointer" :padding "0"
                                        :display "flex" :align-items "center" :gap "4px"}})
                    (dom/text (if advanced-open "▾ Advanced" "▸ Advanced"))
                    (dom/On "click" (fn [_] (swap! !advanced-open not)) nil))
                  (when advanced-open
                    (dom/div
                      (dom/props {:style {:margin-top "8px" :padding "10px"
                                          :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                          :background "var(--color-bg-subtle)"
                                          :display "flex" :flex-direction "column" :gap "6px"}})
                      (dom/span
                        (dom/props {:style {:font-size "12px" :font-weight "500" :color "var(--color-text-label)"}})
                        (dom/text "Images"))
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
                        (dom/input (dom/props {:type "radio" :name "image_mode" :value "reduce"})
                          (set! (.-checked dom/node) (= image-mode "reduce"))
                          (dom/On "change" (fn [_] (reset! !image-mode "reduce")) nil))
                        (dom/text "Reduce size — shrink for faster loading"))
                      (dom/label
                        (dom/props {:style {:display "flex" :align-items "center" :gap "8px" :font-size "13px" :cursor "pointer"}})
                        (dom/input (dom/props {:type "radio" :name "image_mode" :value "strip"})
                          (set! (.-checked dom/node) (= image-mode "strip"))
                          (dom/On "change" (fn [_] (reset! !image-mode "strip")) nil))
                        (dom/text "Strip images — text only")))))))

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

            ;; Buttons — branch by flow (PDF/EPUB imperative, HTML/MD reactive token)
            (dom/div
              (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
              (case flow
                (:pdf :epub)
                (dom/button
                  (dom/props {:class "btn btn-primary"
                              :style {:font-weight "600" :order "1"
                                      :cursor (if uploading "not-allowed" "pointer")}
                              :disabled uploading})
                  (dom/text (if uploading "Uploading..." "Import"))
                  (dom/On "click" (fn [_] (start-upload! flow @!file)) nil))

                (:html :markdown)
                (dom/button
                  (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
                  (let [event (dom/On "click"
                                (fn [_] {:flow flow :title @!title
                                         :html-text @!html-text :md-text @!md-text})
                                nil)
                        [?token ?error] (e/Token event)
                        importing? (some? ?token)
                        no-title? (empty? title)]
                    (dom/props {:disabled (or importing? no-title?)
                                :style {:cursor (if (or importing? no-title?) "not-allowed" "pointer")}})
                    (dom/text (if importing? "Processing..." "Import"))
                    (when ?error
                      (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                        (dom/text ?error)))
                    (when-some [token ?token]
                      (case (:flow event)
                        :html
                        (let [{title-val :title html-val :html-text} event]
                          (if (and (seq title-val) (seq html-val))
                            (let [result (e/server
                                           (e/Offload
                                             #(let [tid (create-web-topic* user-id title-val html-val nil)]
                                                (if tid {:ok tid} {:err "Failed to save"}))))]
                              ;; e/Offload returns nil while pending — gate side effects.
                              (when (some? result)
                                (if-let [tid (:ok result)]
                                  (do (swap! !mutations inc)
                                      (token)
                                      (reset! !show false)
                                      (navigate! :viewer (nav/nav-topic tid nil)))
                                  (token (:err result)))))
                            (token "Title and content are required")))
                        :markdown
                        (let [{title-val :title md-val :md-text} event]
                          (if (and (seq title-val) (seq md-val))
                            (let [topic-id (e/server
                                             (let [html (parse-markdown* md-val)]
                                               (create-markdown-topic* user-id title-val html)))]
                              (if topic-id
                                (do (swap! !mutations inc)
                                    (token)
                                    (reset! !show false)
                                    (navigate! :viewer (nav/nav-topic topic-id nil)))
                                (token "Failed to import")))
                            (token "Title and content are required")))))))

                ;; nil flow — no file or unsupported
                (dom/button
                  (dom/props {:class "btn btn-primary"
                              :style {:font-weight "600" :order "1" :cursor "not-allowed"}
                              :disabled true})
                  (dom/text "Import")))

              (dom/button
                (dom/props {:class "btn btn-secondary"})
                (dom/text "Cancel")
                (dom/On "click" (fn [_] (reset! !show false)) nil))))))
      (e/watch !mutations))))

;; Unified Paste Modal — dropdown picks HTML vs Markdown; atoms persist per-format.
(e/defn PasteModal [!show user-id navigate!]
  (e/client
    (let [!mutations (atom 0)
          default-md-title (e/server (markdown-default-title))
          !format (atom :markdown)
          !html-title (atom "")
          !md-title (atom (or default-md-title ""))
          !html (atom nil)
          !md-text (atom "")
          !url (atom "")
          format (e/watch !format)
          html-title (e/watch !html-title)
          md-title (e/watch !md-title)
          md-text (e/watch !md-text)
          url (e/watch !url)
          title (case format :html html-title :markdown md-title)]
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
            (dom/text "Paste"))

          ;; Format selector
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Format"))
          (dom/select
            (dom/props {:value (name format)
                        :class "input" :style {:margin-bottom "var(--sp-3)" :width "200px"}})
            (dom/On "change" (fn [e] (reset! !format (keyword (-> e .-target .-value)))) nil)
            (dom/option (dom/props {:value "markdown"}) (dom/text "Markdown"))
            (dom/option (dom/props {:value "html"}) (dom/text "HTML")))

          ;; Title — bound atom swaps by format
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (dom/text "Title"))
          (case format
            :html (dom/input
                    (dom/props {:type "text" :placeholder "Article title"
                                :value html-title
                                :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
                    (dom/On "input" (fn [e] (reset! !html-title (-> e .-target .-value))) nil))
            :markdown (dom/input
                        (dom/props {:type "text" :placeholder "Document title"
                                    :value md-title
                                    :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
                        (dom/On "input" (fn [e] (reset! !md-title (-> e .-target .-value))) nil)))

          ;; Source URL — HTML only
          (when (= format :html)
            (dom/label
              (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
              (dom/text "Source URL (optional)"))
            (dom/input
              (dom/props {:type "text" :placeholder "https://..."
                          :value url
                          :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
              (dom/On "input" (fn [e] (reset! !url (-> e .-target .-value))) nil)))

          ;; Content area swaps by format
          (dom/label
            (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
            (case format
              :html (dom/text "Content (paste from browser with Ctrl+V)")
              :markdown (dom/text "Markdown content")))
          (case format
            :html (dom/div
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
                          #?(:cljs
                             (when (empty? @!html-title)
                               (let [tmp (js/document.createElement "div")]
                                 (set! (.-innerHTML tmp) content)
                                 (when-let [h (or (.querySelector tmp "h1")
                                                (.querySelector tmp "h2")
                                                (.querySelector tmp "h3"))]
                                   (reset! !html-title (.-textContent h))))))))
                      nil))
            :markdown (dom/textarea
                        (dom/props {:placeholder "Paste your Markdown here..."
                                    :value md-text
                                    :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                                            :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                                            :font-size "13px" :font-family "monospace" :line-height "1.5" :margin-bottom "var(--sp-4)"
                                            :background "var(--color-bg-subtle)" :resize "vertical"}})
                        (dom/On "input" (fn [e] (reset! !md-text (-> e .-target .-value))) nil)))

          ;; Buttons
          (dom/div
            (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
              (let [event (dom/On "click"
                            (fn [_]
                              (case @!format
                                :html {:format :html :title @!html-title :html @!html :url @!url}
                                :markdown {:format :markdown :title @!md-title :md-text @!md-text}))
                            nil)
                    [?token ?error] (e/Token event)
                    importing? (some? ?token)
                    no-title? (empty? title)]
                (dom/props {:disabled (or importing? no-title?)
                            :style {:cursor (if (or importing? no-title?) "not-allowed" "pointer")}})
                (dom/text (if importing? "Processing..." "Import"))
                (when ?error
                  (dom/div (dom/props {:style {:color "var(--color-danger)" :font-size "12px" :margin-top "var(--sp-1)"}})
                    (dom/text ?error)))
                (when-some [token ?token]
                  (case (:format event)
                    :html
                    (let [{title-val :title html-val :html url-val :url} event]
                      (if (and (seq title-val) (seq html-val))
                        (let [result (e/server
                                       (e/Offload
                                         #(let [tid (create-web-topic* user-id title-val html-val
                                                      (when (seq url-val) url-val))]
                                            (if tid {:ok tid} {:err "Failed to save article"}))))]
                          (when (some? result)
                            (if-let [tid (:ok result)]
                              (do (swap! !mutations inc)
                                  (token)
                                  (reset! !show false)
                                  (navigate! :viewer (nav/nav-topic tid nil)))
                              (token (:err result)))))
                        (token "Title and content are required")))
                    :markdown
                    (let [{title-val :title md-val :md-text} event]
                      (if (and (seq title-val) (seq md-val))
                        (let [topic-id (e/server
                                         (let [html (parse-markdown* md-val)]
                                           (create-markdown-topic* user-id title-val html)))]
                          (if topic-id
                            (do (swap! !mutations inc)
                                (token)
                                (reset! !show false)
                                (navigate! :viewer (nav/nav-topic topic-id nil)))
                            (token "Failed to import")))
                        (token "Please enter some Markdown content")))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; New Topic Modal — create a blank topic
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
                          (navigate! :viewer (nav/nav-topic (:topic-id result) nil)))
                      (token "Failed to create topic"))))))
            (dom/button
              (dom/props {:class "btn btn-secondary"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (reset! !show false)) nil)))))
      (e/watch !mutations))))

;; ImportCard — single tile that opens its modal on click
(e/defn ImportCard [emoji label desc !show]
  (dom/div
    (dom/props {:style {:border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                        :padding "20px" :cursor "pointer" :transition "border-color 0.15s, box-shadow 0.15s"
                        :background "var(--color-bg-surface)"}})
    (dom/On "mouseenter" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                           (set! (.-boxShadow (.-style (.-currentTarget e))) "0 0 0 1px var(--color-primary)")) nil)
    (dom/On "mouseleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                           (set! (.-boxShadow (.-style (.-currentTarget e))) "none")) nil)
    (dom/On "click" (fn [_] (reset! !show true)) nil)
    (dom/div (dom/props {:style {:font-size "24px" :margin-bottom "8px"}}) (dom/text emoji))
    (dom/div (dom/props {:style {:font-size "14px" :font-weight "600" :margin-bottom "4px"}}) (dom/text label))
    (dom/div (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"}}) (dom/text desc))))

;; Main Import page — Link, Upload, Paste, New Topic
(e/defn ImportPage [user-id navigate! enc-key llm-enabled?]
  (e/client
    (dom/div
      (dom/props {:class "page-container"})
      (dom/div
        (dom/props {:style {:display "grid" :grid-template-columns "repeat(auto-fill, minmax(200px, 1fr))"
                            :gap "12px"}})

        (let [!show-link (atom false)
              show-link (e/watch !show-link)]
          (ImportCard "🌐" "Link" "Fetch from any web page or Wikipedia URL" !show-link)
          (when show-link
            (URLImportModal !show-link user-id navigate!)))

        (let [!show-upload (atom false)
              show-upload (e/watch !show-upload)]
          (ImportCard "⬆️" "Upload" "PDF, EPUB, HTML, or Markdown — chosen by file extension" !show-upload)
          (when show-upload
            (UploadModal !show-upload user-id navigate!)))

        (let [!show-paste (atom false)
              show-paste (e/watch !show-paste)]
          (ImportCard "📋" "Paste" "Paste HTML or Markdown content" !show-paste)
          (when show-paste
            (PasteModal !show-paste user-id navigate!)))

        (let [!show-topic (atom false)
              show-topic (e/watch !show-topic)]
          (ImportCard "✏️" "New Topic" "Create a blank topic to write in" !show-topic)
          (when show-topic
            (NewTopicModal !show-topic user-id navigate!)))))))
