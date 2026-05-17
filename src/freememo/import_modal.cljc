(ns freememo.import-modal
  "Unified Import modal. Replaces URLImportModal, UploadModal, PasteModal,
   NewTopicModal. Bytes and content flow through HTTP endpoints; Electric
   drives the reactive state machine only.

   Entry points: each card in ImportPage opens this modal with a
   `:source-preset` of :url | :file | :paste | :new-topic. The modal walks
   through stages :collecting → :fetching → :confirming → :importing → :done
   (or → :error)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.navigation :as nav]
   #?(:clj [freememo.quota :as quota])))

;; ── Server helpers ─────────────────────────────────────────────────

(defn upload-cap-bytes* []
  #?(:clj (long quota/per-file-max-bytes)
     :cljs 0))

;; ── CLJS helpers ───────────────────────────────────────────────────

#?(:cljs
   (defn- format-mb [bytes]
     (let [mb (/ (double (or bytes 0)) 1048576.0)]
       (if (< mb 10)
         (str (.toFixed mb 1) " MB")
         (str (Math/round mb) " MB")))))

#?(:cljs
   (defn- format-bytes [n]
     (cond
       (nil? n) ""
       (< n 1024) (str n " B")
       (< n 1048576) (str (.toFixed (/ n 1024.0) 1) " KB")
       :else (format-mb n))))

#?(:cljs
   (defn- file-ext [^js file]
     (when file
       (let [n (.-name file)
             dot (.lastIndexOf n ".")]
         (when (pos? dot)
           (.toLowerCase (subs n (inc dot))))))))

#?(:cljs
   (defn- flow-label [flow]
     (case flow
       "pdf" "PDF"
       "epub" "EPUB"
       "html" "HTML"
       "markdown" "Markdown"
       "web" "Web Article"
       "Item")))

;; ── HTTP helpers ───────────────────────────────────────────────────

#?(:cljs
   (defn- form-data
     "Build a FormData from a Clojure map of string keys → values."
     [m]
     (let [fd (js/FormData.)]
       (doseq [[k v] m]
         (when (some? v)
           (.append fd (name k) (str v))))
       fd)))

#?(:cljs
   (defn- post-form! [url params on-success on-error]
     (-> (js/fetch url (clj->js {:method "POST" :body (form-data params)}))
         (.then (fn [resp] (.json resp)))
         (.then (fn [^js data] (on-success data)))
         (.catch (fn [err]
                   (js/console.error "POST failed:" err)
                   (on-error "Request failed — please try again."))))))

#?(:cljs
   (defn- post-multipart! [url file extra-params on-success on-error]
     (let [fd (js/FormData.)]
       (.append fd "file" file)
       (doseq [[k v] extra-params]
         (when (some? v) (.append fd (name k) (str v))))
       (-> (js/fetch url (clj->js {:method "POST" :body fd}))
           (.then (fn [resp] (.json resp)))
           (.then (fn [^js data] (on-success data)))
           (.catch (fn [err]
                     (js/console.error "Upload failed:" err)
                     (on-error "Upload failed — please try again.")))))))

;; ── Atoms reset ────────────────────────────────────────────────────

#?(:cljs
   (defn- reset-error! [!error !quota-error?]
     (reset! !error nil)
     (reset! !quota-error? false)))

;; ── Result handlers ────────────────────────────────────────────────
;; A response from any upload-* endpoint has one of three shapes:
;;   {:success true :doc_id N :flow "web|html|markdown"}     — committed
;;   {:success true :upload_id "..." :flow "pdf|epub" ...}    — staged
;;   {:success true :flow "html|markdown" :content :default_title}  — inline file
;;   {:success false :error <msg> :code <...>}                — error

#?(:cljs
   (defn- handle-upload-response
     [^js data
      {:keys [!stage !flow !staged !filename !error !quota-error?
              !title !html-text !md-text
              navigate-to-viewer!]}]
     (cond
       (.-success data)
       (cond
         ;; Inline text content (HTML/MD file upload) — go to text-confirming
         (and (.-content data) (.-flow data))
         (let [flow-str (.-flow data)
               content (.-content data)
               default-title (.-default_title data)]
           (reset! !flow flow-str)
           (reset! !filename (.-filename data))
           (reset! !title default-title)
           (if (= flow-str "html")
             (reset! !html-text content)
             (reset! !md-text content))
           (reset! !stage :text-confirming))

         ;; Staged binary — go to confirming
         (.-upload_id data)
         (do (reset! !staged {:upload-id (.-upload_id data)
                              :filename (.-filename data)
                              :size (.-size data)
                              :flow (.-flow data)})
             (reset! !flow (.-flow data))
             (reset! !stage :confirming))

         ;; Committed topic — navigate
         (.-doc_id data)
         (do (navigate-to-viewer! (.-doc_id data))
             (reset! !stage :done))

         :else
         (do (reset! !error "Unexpected response from server.")
             (reset! !stage :error)))

       :else
       (do (reset! !error (or (.-error data) "Import failed."))
           (reset! !quota-error? (or (= (.-code data) "over-quota")
                                     (= (.-code data) "file-too-large")))
           (reset! !stage :error)))))

;; ── Sub-components ─────────────────────────────────────────────────

;; Pre:  `on-fetch` is a 1-arg fn taking a non-blank url string.
;; Post: on submit (button click or Enter inside the Form's <form>), `(on-fetch url)`
;;       fires once and the form's token is spent.
;; Invariant: :required + HTML validation blocks submit on empty.
(e/defn UrlInput [on-fetch]
  (e/client
    (let [commits (forms/Form! {:url ""}
                    (e/fn Fields [{:keys [url]}]
                      (e/amb
                        (dom/p
                          (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"
                                              :color "var(--color-text-secondary)"}})
                          (dom/text "Paste a Wikipedia or any web page URL. PDFs and EPUBs are recognized and offered for import."))
                        (forms/Input! :url (or url "")
                          :type "text"
                          :placeholder "https://en.wikipedia.org/wiki/..."
                          :required true
                          :class "input input-full"
                          :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"})
                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                              :justify-content "flex-end"}})
                          (forms/SubmitButton! :label "Fetch"
                            :class "btn btn-primary"
                            :style {:font-weight "600" :order "1"}))))
                    :type :command
                    :show-buttons false
                    :Parse (e/fn [{:keys [url]} _tempid]
                             [`Fetch-url url]))]
      (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
        (do (on-fetch (nth cmd 1)) (token))))))

(e/defn FilePicker [!file !file-input on-file cap-bytes]
  (e/client
    (let [file (e/watch !file)
          cap-label (when (pos? cap-bytes) (format-mb cap-bytes))]
      (dom/p
        (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
        (dom/text (if cap-label
                    (str "PDF, EPUB, HTML, or Markdown — file extension picks the flow. Maximum " cap-label ".")
                    "PDF, EPUB, HTML, or Markdown — file extension picks the flow.")))
      (dom/div
        (dom/props {:style {:border "2px dashed var(--color-border)" :border-radius "var(--radius-md)"
                            :padding "32px" :text-align "center" :cursor "pointer"
                            :margin-bottom "var(--sp-3)" :transition "border-color 0.15s, background 0.15s"}})
        (dom/On "click" (fn [_] (when-some [inp @!file-input] (.click inp))) nil)
        (dom/On "dragover" (fn [e] (.preventDefault e)
                             (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-primary)")
                             (set! (.-background (.-style (.-currentTarget e))) "var(--color-bg-subtle)")) nil)
        (dom/On "dragleave" (fn [e] (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                              (set! (.-background (.-style (.-currentTarget e))) "")) nil)
        (dom/On "drop" (fn [e] (.preventDefault e)
                         (set! (.-borderColor (.-style (.-currentTarget e))) "var(--color-border)")
                         (set! (.-background (.-style (.-currentTarget e))) "")
                         (when-some [f (-> e .-dataTransfer .-files (aget 0))]
                           (on-file f))) nil)
        (dom/div
          (dom/props {:style {:font-size "13px" :color "var(--color-text-secondary)"}})
          (if file
            (dom/text (str "Selected: " (.-name file)))
            (dom/text "Drop a file here or click to browse"))))
      (dom/input
        (dom/props {:type "file"
                    :accept ".pdf,.epub,.html,.htm,.md,.markdown"
                    :style {:display "none"}})
        (reset! !file-input dom/node)
        (dom/On "change" (fn [e] (when-some [f (-> e .-target .-files (aget 0))]
                                   (on-file f))) nil)))))

(e/defn PasteEditor [!format !title !paste-url !html-text !md-text on-import]
  (e/client
    (let [format (e/watch !format)
          title (e/watch !title)]
      (dom/label
        (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
        (dom/text "Format"))
      (dom/select
        (dom/props {:value format
                    :class "input" :style {:margin-bottom "var(--sp-3)" :width "200px"}})
        (dom/On "change" (fn [e] (reset! !format (-> e .-target .-value))) nil)
        (dom/option (dom/props {:value "markdown"}) (dom/text "Markdown"))
        (dom/option (dom/props {:value "html"}) (dom/text "HTML")))

      (dom/label
        (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
        (dom/text "Title"))
      (dom/input
        (dom/props {:type "text" :placeholder "Document title"
                    :value title
                    :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
        (dom/On "input" (fn [e] (reset! !title (-> e .-target .-value))) nil))

      (when (= format "html")
        (dom/label
          (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
          (dom/text "Source URL (optional)"))
        (dom/input
          (dom/props {:type "text" :placeholder "https://..."
                      :value (e/watch !paste-url)
                      :class "input input-full" :style {:margin-bottom "var(--sp-3)"}})
          (dom/On "input" (fn [e] (reset! !paste-url (-> e .-target .-value))) nil)))

      (dom/label
        (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
        (case format
          "html" (dom/text "Content (paste from browser with Ctrl+V)")
          "markdown" (dom/text "Markdown content")))
      (case format
        "html"
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
                (reset! !html-text content)
                #?(:cljs
                   (when (empty? @!title)
                     (let [tmp (js/document.createElement "div")]
                       (set! (.-innerHTML tmp) content)
                       (when-let [h (or (.querySelector tmp "h1")
                                      (.querySelector tmp "h2")
                                      (.querySelector tmp "h3"))]
                         (reset! !title (.-textContent h))))))))
            nil)
          (dom/On "input"
            (fn [e] (reset! !html-text (.-innerHTML (.-currentTarget e))))
            nil))

        "markdown"
        (dom/textarea
          (dom/props {:placeholder "Paste your Markdown here..."
                      :value (or (e/watch !md-text) "")
                      :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                              :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                              :font-size "13px" :font-family "monospace" :line-height "1.5" :margin-bottom "var(--sp-4)"
                              :background "var(--color-bg-subtle)" :resize "vertical"}})
          (dom/On "input" (fn [e] (reset! !md-text (-> e .-target .-value))) nil)))

      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}
                      :disabled (empty? title)})
          (dom/text "Import")
          (dom/On "click" (fn [_] (on-import)) nil))))))

;; Pre:  `!flow`, `!filename`, `!title` are atoms; !title may carry a
;;       file-extracted seed value. `on-confirm` is a 1-arg fn taking the
;;       edited title (non-blank by :required). `on-cancel` is a 0-arg fn.
;; Post: on submit → `(on-confirm title)`; on cancel → `(on-cancel)`. Token spent.
;; Invariant: :required blocks submit on empty title. The Form! owns the title
;;            field after mount; !title atom is no longer mutated by edits here.
(e/defn TextFileReviewCancelButton [on-cancel]
  (dom/button
    (dom/props {:class "btn btn-secondary" :type "button"})
    (dom/text "Cancel")
    (dom/On "click" (fn [_] (on-cancel)) nil))
  (e/amb))

(e/defn TextFileReviewStage [!flow !filename !title on-confirm on-cancel]
  (e/client
    (let [flow (e/watch !flow)
          filename (e/watch !filename)
          title (e/watch !title)
          label (flow-label flow)
          commits (forms/Form! {:title (or title "")}
                    (e/fn Fields [{:keys [title]}]
                      (e/amb
                        (dom/p
                          (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"}})
                          (dom/text (str "This is an " label " file.")))
                        (dom/div
                          (dom/props {:style {:padding "12px" :margin-bottom "var(--sp-3)"
                                              :background "var(--color-bg-subtle)"
                                              :border-radius "var(--radius-sm)"
                                              :font-size "13px" :font-weight "500"}})
                          (dom/text (or filename "")))
                        (dom/label
                          (dom/props {:class "label" :style {:color "var(--color-text-secondary)"}})
                          (dom/text "Title"))
                        (forms/Input! :title (or title "")
                          :type "text"
                          :placeholder "Document title"
                          :required true
                          :class "input input-full"
                          :style {:margin-bottom "var(--sp-3)"})
                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                              :justify-content "flex-end"}})
                          (e/amb
                            (forms/SubmitButton! :label "Import"
                              :class "btn btn-primary"
                              :style {:font-weight "600" :order "1"})
                            (TextFileReviewCancelButton on-cancel)))))
                    :type :command
                    :show-buttons false
                    :Parse (e/fn [{:keys [title]} _tempid]
                             [`Text-confirm title]))]
      (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
        (do (on-confirm (nth cmd 1)) (token))))))

;; Pre:  `on-create` is a 1-arg fn taking a title string (possibly empty).
;; Post: on submit, `(on-create title)` fires once and the form's token is spent.
;; Invariant: empty title is allowed — the parent's callback defaults to "New Topic".
(e/defn NewTopicInput [on-create]
  (e/client
    (let [commits (forms/Form! {:title ""}
                    (e/fn Fields [{:keys [title]}]
                      (e/amb
                        (dom/p
                          (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"
                                              :color "var(--color-text-secondary)"}})
                          (dom/text "Create a blank topic. You can add content later."))
                        (forms/Input! :title (or title "")
                          :type "text"
                          :placeholder "Topic title..."
                          :class "input input-full"
                          :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"})
                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                              :justify-content "flex-end"}})
                          (forms/SubmitButton! :label "Create"
                            :class "btn btn-primary"
                            :style {:font-weight "600" :order "1"}))))
                    :type :command
                    :show-buttons false
                    :Parse (e/fn [{:keys [title]} _tempid]
                             [`Create-topic title]))]
      (e/for [[token cmd] (e/diff-by first (e/as-vec commits))]
        (do (on-create (nth cmd 1)) (token))))))

(e/defn ConfirmingStage [!staged !flow !image-mode !advanced-open on-confirm on-cancel]
  (e/client
    (let [staged (e/watch !staged)
          flow (e/watch !flow)
          filename (:filename staged)
          size (:size staged)
          label (flow-label flow)
          advanced-open (e/watch !advanced-open)]
      (dom/p
        (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"}})
        (dom/text (str "This is a " label ". Import as " label "?")))
      (dom/div
        (dom/props {:style {:padding "12px" :margin-bottom "var(--sp-3)"
                            :background "var(--color-bg-subtle)"
                            :border-radius "var(--radius-sm)"
                            :font-size "13px"}})
        (dom/div (dom/props {:style {:font-weight "500"}}) (dom/text (or filename "")))
        (dom/div (dom/props {:style {:color "var(--color-text-secondary)" :margin-top "4px"}})
          (dom/text (format-bytes size))))

      (when (= flow "epub")
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
            (let [image-mode (e/watch !image-mode)]
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

      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
          (dom/text (str "Import as " label))
          (dom/On "click" (fn [_] (on-confirm)) nil))
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (on-cancel)) nil))))))

(e/defn StatusStage [message]
  (e/client
    (dom/div
      (dom/props {:style {:padding "24px" :text-align "center"
                          :color "var(--color-text-secondary)"}})
      (dom/text message))))

(e/defn ErrorStage [!error !quota-error? on-try-again on-manage-library]
  (e/client
    (let [error-msg (e/watch !error)
          quota-error? (e/watch !quota-error?)]
      (dom/div
        (dom/props {:style {:padding "10px 12px" :margin-bottom "var(--sp-3)"
                            :background "var(--color-danger-bg, #fee)"
                            :border-radius "var(--radius-sm)"
                            :color "var(--color-danger)" :font-size "13px"}})
        (dom/text (or error-msg "Something went wrong.")))
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
        (when quota-error?
          (dom/button
            (dom/props {:style {:padding "4px 12px" :font-size "12px"
                                :background "transparent" :border "1px solid var(--color-danger)"
                                :color "var(--color-danger)" :border-radius "3px" :cursor "pointer"}})
            (dom/text "Manage Library")
            (dom/On "click" (fn [_] (on-manage-library)) nil)))
        (dom/button
          (dom/props {:class "btn btn-primary" :style {:font-weight "600" :order "1"}})
          (dom/text "Try again")
          (dom/On "click" (fn [_] (on-try-again)) nil))))))

;; ── Top-level ImportModal ──────────────────────────────────────────

(e/defn ImportModal [!show user-id source-preset navigate!]
  (e/client
    (let [!source (atom (or source-preset :url))
          !stage (atom :collecting)
          !flow (atom nil)
          !staged (atom nil)
          !filename (atom nil)
          !error (atom nil)
          !quota-error? (atom false)
          !file (atom nil)
          !file-input (atom nil)
          !title (atom "")
          !html-text (atom "")
          !md-text (atom "")
          !format (atom "markdown")
          !paste-url (atom "")
          !image-mode (atom "reduce")
          !advanced-open (atom false)
          source (e/watch !source)
          stage (e/watch !stage)
          cap-bytes (e/server (upload-cap-bytes*))
          navigate-to-viewer! (fn [doc-id]
                                (reset! !show false)
                                (navigate! :viewer (nav/nav-topic doc-id nil)))
          ctx {:!stage !stage :!flow !flow :!staged !staged :!filename !filename
               :!error !error :!quota-error? !quota-error?
               :!title !title :!html-text !html-text :!md-text !md-text
               :navigate-to-viewer! navigate-to-viewer!}
          handle-resp (fn [^js data] (handle-upload-response data ctx))
          handle-fetch-err (fn [err]
                             (reset! !error err)
                             (reset! !stage :error))
          handle-file (fn [^js f]
                        (reset-error! !error !quota-error?)
                        (reset! !file f)
                        (cond
                          (and (pos? cap-bytes) (> (.-size f) cap-bytes))
                          (do (reset! !error (str "File is " (format-mb (.-size f))
                                               " — limit is " (format-mb cap-bytes) "."))
                              (reset! !quota-error? true)
                              (reset! !stage :error))

                          :else
                          (do (reset! !stage :fetching)
                              (post-multipart! "/api/upload-file" f {}
                                handle-resp
                                handle-fetch-err))))
          ;; Pre: url is a non-blank string (UrlInput's :required enforces non-empty).
          on-fetch-url (fn [url]
                         (reset-error! !error !quota-error?)
                         (reset! !stage :fetching)
                         (post-form! "/api/upload-url" {"url" url}
                           handle-resp
                           handle-fetch-err))
          on-paste-import (fn []
                            (reset-error! !error !quota-error?)
                            (let [format-v @!format
                                  title-v @!title
                                  content-v (if (= format-v "html") @!html-text @!md-text)]
                              (cond
                                (empty? title-v)
                                (do (reset! !error "Title is required.")
                                    (reset! !stage :error))

                                (empty? content-v)
                                (do (reset! !error "Content is required.")
                                    (reset! !stage :error))

                                :else
                                (do (reset! !stage :importing)
                                    (post-form! "/api/upload-paste"
                                      (cond-> {"format" format-v
                                               "title" title-v
                                               "content" content-v}
                                        (and (= format-v "html") (seq @!paste-url))
                                        (assoc "url" @!paste-url))
                                      handle-resp
                                      handle-fetch-err)))))
          ;; Pre: title may be empty (defaults to "New Topic"). No Forms5 validation.
          on-new-topic (fn [title]
                         (reset-error! !error !quota-error?)
                         (let [title-v (if (seq title) title "New Topic")]
                           (reset! !stage :importing)
                           (post-form! "/api/create-topic" {"title" title-v}
                             handle-resp
                             handle-fetch-err)))
          on-confirm (fn []
                       (reset-error! !error !quota-error?)
                       (reset! !stage :importing)
                       (post-form! "/api/upload-staged"
                         {"upload_id" (:upload-id @!staged)
                          "image_mode" @!image-mode}
                         handle-resp
                         handle-fetch-err))
          on-cancel-confirm (fn []
                              (reset! !staged nil)
                              (reset! !flow nil)
                              (reset! !stage :collecting))
          ;; Pre: title is non-blank (TextFileReviewStage's :required enforces).
          on-text-confirm (fn [title]
                            (reset-error! !error !quota-error?)
                            (let [flow-v @!flow
                                  content-v (if (= flow-v "html") @!html-text @!md-text)]
                              (reset! !stage :importing)
                              (post-form! "/api/upload-paste"
                                {"format" flow-v
                                 "title" title
                                 "content" content-v}
                                handle-resp
                                handle-fetch-err)))
          on-cancel-text (fn []
                           (reset! !html-text "")
                           (reset! !md-text "")
                           (reset! !title "")
                           (reset! !filename nil)
                           (reset! !flow nil)
                           (reset! !stage :collecting))
          on-try-again (fn []
                         (reset-error! !error !quota-error?)
                         (reset! !stage :collecting))
          on-manage-library (fn []
                              (reset! !show false)
                              (navigate! :library))
          ;; Universal drop handler — switches to file source on any drop
          on-modal-drop (fn [e]
                          (.preventDefault e)
                          (when-some [f (-> e .-dataTransfer .-files (aget 0))]
                            (reset! !source :file)
                            (handle-file f)))
          title (case source
                  :url "Import from URL"
                  :file "Upload"
                  :paste "Paste"
                  :new-topic "New Topic"
                  "Import")]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class (case source
                               :paste "modal-content modal-lg"
                               "modal-content")
                      :style (case source
                               :paste {:max-height "85vh" :display "flex" :flex-direction "column"}
                               {:width "500px" :max-width "90%"})})
          (dom/On "dragover" (fn [e] (.preventDefault e)) nil)
          (dom/On "drop" on-modal-drop nil)

          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text title))

          (case stage
            :fetching (StatusStage "Fetching…")
            :importing (StatusStage "Importing…")
            :confirming (ConfirmingStage !staged !flow !image-mode !advanced-open
                                         on-confirm on-cancel-confirm)
            :text-confirming (TextFileReviewStage !flow !filename !title
                                                  on-text-confirm on-cancel-text)
            :error (ErrorStage !error !quota-error? on-try-again on-manage-library)
            :done nil
            ;; :collecting (default)
            (case source
              :url (UrlInput on-fetch-url)
              :file (FilePicker !file !file-input handle-file cap-bytes)
              :paste (PasteEditor !format !title !paste-url !html-text !md-text
                                  on-paste-import)
              :new-topic (NewTopicInput on-new-topic)
              nil)))))))
