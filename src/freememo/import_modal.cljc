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
   [clojure.string :as str]
   [freememo.loading :as loading]
   [freememo.modal-shell :as modal]
   [freememo.navigation :as nav]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.web-import :as web-import])))

;; ── Server helpers ─────────────────────────────────────────────────

(defn upload-cap-bytes* [user-id]
  #?(:clj (long (quota/get-user-upload-max db/ds user-id))
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
       "audio" "Audio"
       "score" "Score"
       "repo" "Code Repository"
       "Item")))

;; ── Paste-source detection ─────────────────────────────────────────
;;
;; The HTML pane accepts two kinds of paste:
;;   rendered copy — the text/html flavor carries real markup; the browser's
;;     default insertion renders it.
;;   raw source — the HTML is literal text (copied from an editor or a code
;;     block); without parsing it lands as escaped text and imports literally.
;; Source is detected when the plain-text flavor reads as markup AND the
;; text/html flavor is absent or is only code-block chrome around it.

(defn- html-source?
  "True iff `s` reads as raw HTML source: after leading whitespace it opens
   with a tag (`<` + letter or `!`) and a closing tag appears later.
   Mid-text fragments without a leading tag intentionally fail."
  [s]
  (boolean
    (and s
         (re-find #"^\s*<[a-zA-Z!]" s)
         (str/includes? s "</"))))

#?(:cljs
   (defn- parse-inert
     "Parse `html` into an inert Document — DOMParser documents have no
      browsing context, so nothing executes and no resources load."
     [html]
     (.parseFromString (js/DOMParser.) html "text/html")))

#?(:cljs
   (defn- code-block-wrapper?
     "True iff the text/html clipboard flavor contains no content elements —
      only pre/code/span/div/br (code-block chrome wrapping escaped source).
      A flavor with any real element (p, h1, ul, …) is a rendered copy."
     [html]
     (every? #(contains? #{"PRE" "CODE" "SPAN" "DIV" "BR"} (.-tagName ^js %))
             (array-seq (.querySelectorAll (.-body (parse-inert html)) "*")))))

#?(:cljs
   (defn- strip-active-content
     "Preview-grade sanitization of raw HTML source before it is assigned to
      the pane's innerHTML: drops script/iframe/style elements, on* attributes,
      and javascript: href/src values. Persistence-grade sanitization stays
      server-side (freememo.html-cleaner/clean-html).
      Post: the returned HTML assigns to innerHTML without executing anything."
     [source]
     (let [body (.-body (parse-inert source))]
       (doseq [^js el (array-seq (.querySelectorAll body "script, iframe, style"))]
         (.remove el))
       (doseq [^js el (array-seq (.querySelectorAll body "*"))
               ;; snapshot names first — removing while iterating the live
               ;; NamedNodeMap skips entries
               attr-name (mapv #(.-name ^js %) (array-seq (.-attributes el)))]
         (when (or (str/starts-with? attr-name "on")
                   (and (contains? #{"href" "src"} attr-name)
                        (some-> (.getAttribute el attr-name)
                          str/trim str/lower-case
                          (str/starts-with? "javascript:"))))
           (.removeAttribute el attr-name)))
       (.-innerHTML body))))

#?(:cljs
   (defn- first-heading-text
     "Text of the first h1 in `html`, else first h2, else first h3 — or nil."
     [html]
     (let [body (.-body (parse-inert html))]
       (some #(some-> (.querySelector body %) .-textContent) ["h1" "h2" "h3"]))))

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

;; ── State-update helpers for the URL/Confirm Forms5 services ───────
;; Bundle synchronous atom mutations into single fns so the calling
;; Electric `case` chains stay one-line per phase. Returning nil is fine —
;; `case` only needs the test to resolve before the default-branch fires.

#?(:cljs
   (defn- url-set-staged!
     "Transition to the :confirming stage with staged binary data."
     [!staged !flow !stage staged-result]
     (reset! !staged {:upload-id (:upload-id staged-result)
                      :filename (:filename staged-result)
                      :size 0
                      :flow (name (:dispatch staged-result))})
     (reset! !flow (name (:dispatch staged-result)))
     (reset! !stage :confirming)))

#?(:cljs
   (defn- url-set-error!
     "Transition to the :error stage with a message."
     [!error !quota-error? !stage msg]
     (reset! !error (or msg "Import failed."))
     (reset! !quota-error? false)
     (reset! !stage :error)))

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

;; Pre:  user-id is a logged-in user; navigate-to-viewer! is a 1-arg fn.
;; Post: on submit, dispatches `[Import-wiki-url url]` to web-import/import-url!*
;;       via e/server, then either navigates (HTML topic) or transitions to
;;       :confirming (binary). Token spends after server returns.
;; Invariant: :required blocks submit on empty.
(e/defn UrlInput [user-id navigate-to-viewer! !stage !staged !flow !error !quota-error? !busy-msg]
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
                             [`Import-wiki-url (str/trim (or url ""))]))]
      (e/for [[token [_head url]] (e/diff-by first (e/as-vec commits))]
        (when token
          ;; Busy overlay: set while this commit processes, clear when (token)
          ;; spends. `case` forces the reset! to evaluate; e/on-unmount fires on
          ;; spend. Both are siblings of the offload — never wrap it (that would
          ;; unmount the in-flight e/Offload).
          (e/on-unmount #(reset! !busy-msg nil))
          (case (reset! !busy-msg "Fetching…")
            (case (reset-error! !error !quota-error?)
              (let [r (e/server (e/Offload #(web-import/import-url!* user-id url)))]
                (case r
                  (if (:ok r)
                    (case (:flow r)
                      :imported       (case (navigate-to-viewer! (:topic-id r)) (token))
                      :already-exists (case (navigate-to-viewer! (:topic-id r)) (token))
                      :staged         (case (url-set-staged! !staged !flow !stage r) (token))
                      (case (url-set-error! !error !quota-error? !stage
                              (str "Unexpected flow: " (:flow r))) (token)))
                    (case (url-set-error! !error !quota-error? !stage (:error r)) (token))))))))))))

(e/defn FilePicker [!file !file-input on-file accept blurb]
  (e/client
    (let [file (e/watch !file)]
      (dom/p
        (dom/props {:style {:margin "0 0 12px 0" :font-size "13px" :color "var(--color-text-secondary)"}})
        (dom/text blurb))
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
                    :accept accept
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
          "html" (dom/text "Content — paste a rendered page or raw HTML source")
          "markdown" (dom/text "Markdown content")))
      (case format
        "html"
        (dom/div
          (dom/props {:contenteditable "true"
                      :style {:flex "1" :min-height "200px" :max-height "400px" :overflow-y "auto"
                              :padding "var(--sp-3)" :border "1px solid var(--color-border)" :border-radius "var(--radius-sm)"
                              :font-size "14px" :line-height "1.6" :margin-bottom "var(--sp-4)"
                              :background "var(--color-bg-subtle)"}})
          ;; Invariant: once a paste settles, !html-text equals the pane's
          ;; innerHTML. Rich/prose pastes keep the browser default insertion
          ;; and the input listener captures it; raw-source pastes suppress
          ;; the default (it would insert the tags as literal text), render
          ;; the parsed source, and set !html-text by hand — programmatic
          ;; innerHTML assignment fires no input event.
          (dom/On "paste"
            (fn [e]
              #?(:cljs
                 (let [cd (.-clipboardData e)
                       html-data (.getData cd "text/html")
                       text-data (.getData cd "text/plain")
                       ;; raw source travels in text/plain; with only a
                       ;; text/html flavor, its text content is the source
                       source-text (if (seq text-data)
                                     text-data
                                     (when (seq html-data)
                                       (.-textContent (.-body (parse-inert html-data)))))
                       source-paste? (and (html-source? source-text)
                                          (or (empty? html-data)
                                              (code-block-wrapper? html-data)))
                       ;; effective markup of this paste on either path
                       markup (cond
                                source-paste? (strip-active-content source-text)
                                (seq html-data) html-data
                                :else nil)]
                   (when source-paste?
                     ;; replaces any prior pane content
                     (.preventDefault e)
                     (set! (.-innerHTML (.-currentTarget e)) markup)
                     (reset! !html-text (.-innerHTML (.-currentTarget e))))
                   (when (and (seq markup) (empty? @!title))
                     (when-some [t (first-heading-text markup)]
                       (reset! !title t))))))
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

;; Pre:  !staged holds {:upload-id :filename :flow}; user-id owns the upload.
;; Post: on submit, a repo flow dispatches `[Confirm-repo-upload upload-id
;;       extract-facts?] to web-import/confirm-repo-upload!*; every other flow
;;       dispatches `[Confirm-staged-upload upload-id image-mode] to
;;       web-import/confirm-staged-upload!*. Both via e/server, navigate on ok.
;; Cancel button: locally resets staged state without claiming the upload.
(e/defn ConfirmingStage [user-id navigate-to-viewer! !stage !staged !flow
                         !image-mode !advanced-open !extract-facts? !error !quota-error? !busy-msg]
  (e/client
    (let [staged (e/watch !staged)
          flow (e/watch !flow)
          filename (:filename staged)
          size (:size staged)
          label (flow-label flow)
          advanced-open (e/watch !advanced-open)
          image-mode (e/watch !image-mode)
          extract-facts? (e/watch !extract-facts?)
          upload-id (:upload-id staged)
          commits (forms/Form! {}
                    (e/fn Fields [_]
                      (e/amb
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
                                  (dom/text "Strip images — text only"))))))
                        (when (= flow "repo")
                          (dom/div
                            (dom/props {:style {:margin-bottom "var(--sp-3)"}})
                            (dom/label
                              (dom/props {:style {:display "flex" :align-items "center" :gap "8px"
                                                  :font-size "13px" :cursor "pointer"}})
                              (dom/input (dom/props {:type "checkbox"})
                                (set! (.-checked dom/node) extract-facts?)
                                (dom/On "change" (fn [_] (swap! !extract-facts? not)) nil))
                              (dom/text "Extract facts into the knowledge graph"))
                            (dom/p
                              (dom/props {:style {:margin "4px 0 0 24px" :font-size "12px"
                                                  :color "var(--color-text-secondary)"}})
                              (dom/text "Static analysis, runs in the background. You can also do this later from the Knowledge tab."))))
                        (dom/div
                          (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
                          (forms/SubmitButton! :label (str "Import as " label)
                            :class "btn btn-primary"
                            :style {:font-weight "600" :order "1"}))))
                    :type :command
                    :show-buttons false
                    :Parse (e/fn [_ _tempid]
                             (if (= flow "repo")
                               [`Confirm-repo-upload upload-id extract-facts?]
                               [`Confirm-staged-upload upload-id (keyword image-mode)])))]
      ;; Cancel button — local-only reset, not part of the form's commit stream.
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"
                            :margin-top "var(--sp-2)"}})
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Cancel")
          (dom/On "click" (fn [_]
                            (reset! !staged nil)
                            (reset! !flow nil)
                            (reset! !extract-facts? false)
                            (reset! !stage :collecting))
            nil)))
      ;; `Confirm-repo-upload carries the extract-facts? flag and gates async
      ;; fact distillation; every other flow carries the image-mode keyword.
      (e/for [[token [head u-id arg]] (e/diff-by first (e/as-vec commits))]
        (when token
          ;; Busy overlay while the staged upload commits (sibling of the
          ;; offload; cleared on token spend via e/on-unmount).
          (e/on-unmount #(reset! !busy-msg nil))
          (case (reset! !busy-msg "Importing…")
            (case (reset-error! !error !quota-error?)
              (let [r (e/server (e/Offload #(if (= head `Confirm-repo-upload)
                                              (web-import/confirm-repo-upload!* user-id u-id arg)
                                              (web-import/confirm-staged-upload!* user-id u-id arg))))]
                (case r
                  (if (:ok r)
                    (case (navigate-to-viewer! (:topic-id r)) (token))
                    (case (url-set-error! !error !quota-error? !stage (:error r)) (token))))))))))))

;; Pre:  !score-pdf / !score-audio hold nil or {:upload-id :filename :size} —
;;       each slot uploads on pick (audio with purpose=score, lifting the
;;       Whisper cap) and stores its staged entry.
;; Post: once BOTH slots are staged, a Forms5 command form mounts; submit
;;       dispatches [Confirm-score pdf-id audio-id] to
;;       web-import/confirm-score-upload!* via e/server and navigates on success.
;; Invariant: a slot rejects a file whose classified flow ≠ its expected flow —
;;            the staged entry never holds the wrong media type.
(e/defn ScoreStage [user-id navigate-to-viewer! !stage !score-pdf !score-audio
                    !error !quota-error? !busy-msg]
  (e/client
    (let [score-pdf (e/watch !score-pdf)
          score-audio (e/watch !score-audio)
          !pdf-file (atom nil)
          !pdf-input (atom nil)
          !audio-file (atom nil)
          !audio-input (atom nil)
          slot-error! (fn [msg]
                        (reset! !error (or msg "Upload failed."))
                        (reset! !quota-error? false)
                        (reset! !stage :error))
          slot-response (fn [!slot expected ^js data]
                          (if (.-success data)
                            (if (and (.-upload_id data) (= expected (.-flow data)))
                              (reset! !slot {:upload-id (.-upload_id data)
                                             :filename (.-filename data)
                                             :size (.-size data)})
                              (slot-error! (str "That file was recognized as "
                                             (flow-label (.-flow data))
                                             " — expected " (flow-label expected) ".")))
                            (do (reset! !error (or (.-error data) "Upload failed."))
                                (reset! !quota-error? (or (= (.-code data) "over-quota")
                                                        (= (.-code data) "file-too-large")))
                                (reset! !stage :error))))
          on-pdf (fn [^js f]
                   (post-multipart! "/api/upload-file" f {}
                     (fn [data] (slot-response !score-pdf "pdf" data))
                     slot-error!))
          on-audio (fn [^js f]
                     (post-multipart! "/api/upload-file" f {"purpose" "score"}
                       (fn [data] (slot-response !score-audio "audio" data))
                       slot-error!))
          staged-style {:font-size "12px" :color "var(--color-success, #16a34a)"
                        :margin "-8px 0 var(--sp-3) 0"}]
      (FilePicker !pdf-file !pdf-input on-pdf ".pdf" "Sheet music (PDF)")
      (when score-pdf
        (dom/div (dom/props {:style staged-style})
          (dom/text (str "✓ " (:filename score-pdf) " (" (format-bytes (:size score-pdf)) ")"))))
      (FilePicker !audio-file !audio-input on-audio
        ".mp3,.m4a,.mp4,.wav,.webm,.ogg,.oga,.flac,.mpeg,.mpga"
        "Recording (mp3, m4a, wav, webm, ogg, flac)")
      (when score-audio
        (dom/div (dom/props {:style staged-style})
          (dom/text (str "✓ " (:filename score-audio) " (" (format-bytes (:size score-audio)) ")"))))
      (if-not (and score-pdf score-audio)
        (dom/p
          (dom/props {:style {:margin "0" :font-size "12px"
                              :color "var(--color-text-secondary)" :text-align "right"}})
          (dom/text "Pick both files to import."))
        (let [commits (forms/Form! {}
                        (e/fn Fields [_]
                          (dom/div
                            (dom/props {:style {:display "flex" :gap "var(--sp-2)"
                                                :justify-content "flex-end"}})
                            (forms/SubmitButton! :label "Import Score"
                              :class "btn btn-primary"
                              :style {:font-weight "600" :order "1"})))
                        :type :command
                        :show-buttons false
                        :Parse (e/fn [_ _tempid]
                                 [`Confirm-score
                                  (:upload-id score-pdf)
                                  (:upload-id score-audio)]))]
          (e/for [[token [_head pdf-id audio-id]] (e/diff-by first (e/as-vec commits))]
            (when token
              ;; Busy overlay while both staged uploads commit into one topic
              ;; (sibling of the offload; cleared on token spend via e/on-unmount).
              (e/on-unmount #(reset! !busy-msg nil))
              (case (reset! !busy-msg "Importing…")
                (case (reset-error! !error !quota-error?)
                  (let [r (e/server (e/Offload #(web-import/confirm-score-upload!*
                                                  user-id pdf-id audio-id)))]
                    (case r
                      (if (:ok r)
                        (case (navigate-to-viewer! (:topic-id r)) (token))
                        (case (url-set-error! !error !quota-error? !stage (:error r)) (token))))))))))))))

(e/defn StatusStage [message]
  (e/client
    (loading/Spinner message)))

(e/defn ErrorStage [!error !quota-error? on-try-again on-manage-library]
  (e/client
    (let [error-msg (e/watch !error)
          quota-error? (e/watch !quota-error?)]
      (dom/div
        (dom/props {:style {:padding "10px 12px" :margin-bottom "var(--sp-3)"
                            :background "var(--color-danger-bg, #fee)"
                            :border-radius "var(--radius-sm)"
                            :color "var(--color-danger-text)" :font-size "13px"}})
        (dom/text (or error-msg "Something went wrong.")))
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
        (when quota-error?
          (dom/button
            (dom/props {:style {:padding "4px 12px" :font-size "12px"
                                :background "transparent" :border "1px solid var(--color-danger)"
                                :color "var(--color-danger-text)" :border-radius "3px" :cursor "pointer"}})
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
          ;; repo confirm: whether to distill knowledge-graph facts on import
          ;; (opt-in; skippable later via the Knowledge tab's Distill action)
          !extract-facts? (atom false)
          ;; Score preset: the two staged slots ({:upload-id :filename :size} or nil)
          !score-pdf (atom nil)
          !score-audio (atom nil)
          ;; nil = idle; a string = an e/Offload import is in flight, shown as a
          ;; spinner overlay. Set/cleared by UrlInput and ConfirmingStage (the
          ;; two flows whose offload can't swap to StatusStage without being
          ;; unmounted). HTTP paths use the :fetching/:importing StatusStage.
          !busy-msg (atom nil)
          source (e/watch !source)
          stage (e/watch !stage)
          cap-bytes (e/server (e/Offload #(upload-cap-bytes* user-id)))
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
          ;; Universal drop handler — switches to file source on any drop.
          ;; Score has two typed dropzones of its own; hijacking those drops
          ;; into the single-file flow would discard the slot pairing.
          on-modal-drop (fn [e]
                          (.preventDefault e)
                          (when (not= @!source :score)
                            (when-some [f (-> e .-dataTransfer .-files (aget 0))]
                              (reset! !source :file)
                              (handle-file f))))
          title (case source
                  :url "Import from URL"
                  :file "Upload"
                  :audio "Upload Audio"
                  :score "Import Score"
                  :paste "Paste"
                  :new-topic "New Topic"
                  "Import")]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
        (modal/ModalEscape (fn [] (reset! !show false)) "Import")
        (dom/On "click" (fn [e]
                          (when (= (.-target e) (.-currentTarget e))
                            (reset! !show false)))
          nil)
        (dom/div
          (dom/props {:class (case source
                               :paste "modal-content modal-lg"
                               "modal-content")
                      ;; position:relative anchors the busy-overlay (inset:0).
                      :style (case source
                               :paste {:max-height "85vh" :display "flex" :flex-direction "column"
                                       :position "relative"}
                               {:width "500px" :max-width "90%" :position "relative"})})
          (dom/On "dragover" (fn [e] (.preventDefault e)) nil)
          (dom/On "drop" on-modal-drop nil)

          (dom/h3
            (dom/props {:style {:margin "0 0 16px 0" :font-size "16px"}})
            (dom/text title))

          (case stage
            :fetching (StatusStage "Fetching…")
            :importing (StatusStage "Importing…")
            :confirming (ConfirmingStage user-id navigate-to-viewer!
                                         !stage !staged !flow
                                         !image-mode !advanced-open !extract-facts?
                                         !error !quota-error? !busy-msg)
            :text-confirming (TextFileReviewStage !flow !filename !title
                                                  on-text-confirm on-cancel-text)
            :error (ErrorStage !error !quota-error? on-try-again on-manage-library)
            :done nil
            ;; :collecting (default)
            (case source
              :url (UrlInput user-id navigate-to-viewer!
                             !stage !staged !flow !error !quota-error? !busy-msg)
              :file (let [cap-label (when (pos? cap-bytes) (format-mb cap-bytes))]
                      (FilePicker !file !file-input handle-file
                                  ".pdf,.epub,.html,.htm,.md,.markdown,.zip"
                                  (if cap-label
                                    (str "PDF, EPUB, HTML, Markdown, or a code repo (.zip) — file extension picks the flow. Maximum " cap-label ".")
                                    "PDF, EPUB, HTML, Markdown, or a code repo (.zip) — file extension picks the flow.")))
              :audio (FilePicker !file !file-input handle-file
                                 ".mp3,.m4a,.mp4,.wav,.webm,.ogg,.oga,.flac,.mpeg,.mpga"
                                 "Audio file (mp3, m4a, wav, webm, ogg, flac). Maximum 25 MB.")
              :score (ScoreStage user-id navigate-to-viewer! !stage
                                 !score-pdf !score-audio
                                 !error !quota-error? !busy-msg)
              :paste (PasteEditor !format !title !paste-url !html-text !md-text
                                  on-paste-import)
              :new-topic (NewTopicInput on-new-topic)
              nil))

          ;; Busy overlay for the e/Offload flows (URL / staged-confirm): covers
          ;; the still-mounted form with a centered spinner so the offload keeps
          ;; running underneath. Cleared by the owning component on token spend.
          (when-some [msg (e/watch !busy-msg)]
            (dom/div
              (dom/props {:style {:position "absolute" :inset "0"
                                  :background "var(--color-bg-card)"
                                  :border-radius "var(--radius-lg)"
                                  :display "flex" :align-items "center" :justify-content "center"
                                  :z-index "1"}})
              (loading/Spinner msg))))))))
