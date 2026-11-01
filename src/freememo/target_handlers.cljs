(ns freememo.target-handlers
  "Client handlers of the target commands (plans/custom-context-menu.md §2).

   Registered at namespace load, as freememo.command-bus registers the shell's.
   Each handler takes the bus context and the target. It reads plain facts from
   `:subjects` and client-only handles from `:surface`: a Quill instance, an
   anchor element, a callback the surface lent. A handler never asks the
   server what it may do: a server effect goes through a :queue command, whose
   method in freememo.target-commands checks ownership.

   A failure only the browser sees (a clipboard write, an upload) is reported
   through :show-error-toast, because the toast stack is server state."
  (:require [freememo.anki-subtree-request :as anki-subtree]
            [freememo.card-components :as card-components]
            [freememo.card-export-modal :as card-export]
            [freememo.command-bus :as bus]
            [freememo.editor-actions :as editor-actions]
            [freememo.formula-ui :as formula-ui]
            [freememo.image-actions :as image]
            [freememo.navigation :as nav]
            [freememo.neoreader-picker :as neoreader]
            [freememo.optimistic :as opt]
            [freememo.pdf-cache :as pdf-cache]
            [freememo.rich-text-editor :as rich-text-editor]
            [freememo.target-modals :as modals]))

(defn- report!
  "Toast `message` to the user. Returns nil."
  [message]
  (opt/enqueue! {:type :show-error-toast :payload {:message message}})
  nil)

(defn- subject [target] (first (:subjects target)))

(defn- navigate! [ctx & args]
  (when-let [navigate! (:navigate! ctx)]
    (apply navigate! args)))

;; ---------------------------------------------------------------------------
;; Topic
;; ---------------------------------------------------------------------------

(bus/register-handler! :open-topic-in-new-tab
  (fn [_ctx target]
    (.open js/window (str "/viewer/topic/" (:topic-id (subject target))) "_blank" "noopener")))

(bus/register-handler! :review-topic
  (fn [ctx target]
    (let [{:keys [topic-id title]} (subject target)]
      (navigate! ctx :viewer (nav/nav-subset-review topic-id title)))))

(bus/register-handler! :sync-topic-with-anki
  (fn [_ctx target]
    (anki-subtree/request-subtree-sync! (:topic-id (subject target)))))

;; Every outcome the user should see arrives as a server-pushed toast, because
;; the server holds the counts. A cancelled Picker and a Picker that will not
;; load reach the console only — the gap library_row_actions recorded when it
;; owned this call.
(bus/register-handler! :sync-topic-with-neoreader
  (fn [_ctx target]
    (neoreader/sync-document! (:topic-id (subject target))
      (str (.-pathname js/location) (.-search js/location))
      (fn [status]
        (case (:phase status)
          :error (js/console.error "NeoReader sync:" (:message status))
          :cancelled (js/console.info "NeoReader sync cancelled")
          nil)))))

(bus/register-handler! :open-document-options
  (fn [_ctx target]
    (let [{:keys [topic-id topic-kind]} (subject target)]
      (modals/open-document-options! topic-id (= "pdf" topic-kind)))))

(bus/register-handler! :export-cards-for-topic
  (fn [_ctx target]
    (card-export/open! {:scope :topic :topic-id (:topic-id (subject target))})))

;; The palette entry acts on the topic on screen, which DocumentMainColumn
;; publishes. No topic, no entry.
(bus/register-preparer! :export-topic-cards
  (fn [ctx]
    (when-let [topic-id (:export-topic-id ctx)]
      {:topic-id topic-id})))

(bus/register-handler! :export-topic-cards
  (fn [_ctx {:keys [topic-id]}]
    (card-export/open! {:scope :topic :topic-id topic-id})))

;; A topic delete finishes in the browser: the deleted cards' Anki notes go
;; through AnkiConnect on localhost, and the cached PDF bytes live in
;; IndexedDB. Both are best effort, as in the library's delete dialog.
(opt/register-outcome-handler! :delete-topic
  (fn [_command result]
    (when (:ok? result)
      (card-components/try-delete-anki-notes! (:anki-note-ids result))
      (doseq [id (:deleted-topic-ids result)]
        (pdf-cache/cache-delete id)))))

;; ---------------------------------------------------------------------------
;; Card
;; ---------------------------------------------------------------------------

;; The edit modals belong to the surface that lists the card, so the surface
;; lends its opener in the target's :surface.
(bus/register-handler! :open-card-editor
  (fn [_ctx target]
    (when-let [edit! (-> target :surface :edit-card!)]
      (edit! (subject target)))))

(bus/register-handler! :card-history
  (fn [_ctx target]
    (modals/open-card-history! (:history-scope (subject target)))))

(bus/register-handler! :open-card-source-topic
  (fn [ctx target]
    (navigate! ctx :viewer (nav/nav-topic (:topic-id (subject target))
                             (-> target :surface :origin)))))

(bus/register-handler! :delete-cards
  (fn [_ctx target]
    (doseq [{:keys [member-ids]} (:subjects target)]
      (opt/enqueue-delete-card! member-ids))))

;; ---------------------------------------------------------------------------
;; Image
;; ---------------------------------------------------------------------------

(bus/register-handler! :copy-image
  (fn [_ctx target] (image/copy-image! (:src (subject target)) report!)))

(bus/register-handler! :save-image
  (fn [_ctx target] (image/save-image! (:src (subject target)) report!)))

(bus/register-handler! :copy-image-address
  (fn [_ctx target] (image/copy-image-address! (:src (subject target)) report!)))

(defn- with-media-id!
  "Resolve the image's media id (uploading its bytes when it has none), then
   call `f` with it. A failure toasts `failure`."
  [target failure f]
  (-> (image/resolve-src->media-id! (:src (subject target)))
    (.then f)
    (.catch (fn [err]
              (js/console.warn "[context-menu] media id:" (str err))
              (report! failure)))))

(defn- pin-image! [target placement]
  (with-media-id! target "Couldn't pin this image."
    (fn [media-id]
      (opt/enqueue! {:type :add-pin
                     :payload {:topic-id (:topic-id (subject target))
                               :media-id media-id
                               :placement placement}}))))

(bus/register-handler! :pin-image-front (fn [_ctx target] (pin-image! target "front")))
(bus/register-handler! :pin-image-back (fn [_ctx target] (pin-image! target "back")))

(bus/register-handler! :open-image-occlusion
  (fn [_ctx target]
    (with-media-id! target "Couldn't open image occlusion for this image."
      (fn [media-id]
        (let [{:keys [topic-id root-topic-id]} (subject target)]
          (reset! modals/!occlusion-request
            {:mode :create :image-media-id media-id
             :topic-id topic-id :root-topic-id (or root-topic-id topic-id)}))))))

;; ---------------------------------------------------------------------------
;; Audio clip
;; ---------------------------------------------------------------------------

(defn- toggle-loop! [_ctx target]
  (let [{:keys [quill node]} (:surface target)]
    (editor-actions/toggle-clip-loop! quill node)))

(bus/register-handler! :loop-audio-clip toggle-loop!)
(bus/register-handler! :stop-audio-clip-loop toggle-loop!)

;; ---------------------------------------------------------------------------
;; Caret — Quill edits, source "user", so the editor's own save persists them
;; ---------------------------------------------------------------------------

(defn- clipboard-blocked
  "Nil when the async clipboard API can write, else a message naming why not.
   The API exists only in a secure context (https or localhost)."
  []
  (cond
    (not (.-isSecureContext js/window))
    "Copying needs a secure connection. Open the app over https or on localhost."
    (nil? (.-clipboard js/navigator))
    "This browser cannot copy from a menu. Use Ctrl+C or Cmd+C."
    :else nil))

(defn- write-selection!
  "Write the subject's selection in `ed` to the clipboard, as the HTML and the
   text Quill's own copy writes (`getSemanticHTML`, `getText`).
   Pre : the subject has a :selection-length above 0.
   Post: a promise that resolves once the clipboard holds the selection and
         rejects when it could not be written. Without ClipboardItem only the
         text is written."
  [^js ed {:keys [selection-index selection-length]}]
  (let [text (.getText ed selection-index selection-length)]
    (if (exists? js/ClipboardItem)
      (let [html (.getSemanticHTML ed selection-index selection-length)]
        (.write js/navigator.clipboard
          #js [(js/ClipboardItem.
                 #js {"text/html" (js/Blob. #js [html] #js {:type "text/html"})
                      "text/plain" (js/Blob. #js [text] #js {:type "text/plain"})})]))
      (.writeText js/navigator.clipboard text))))

(defn- copy-selection!
  "Copy, then run `after!` with the editor and the subject once the write
   succeeded. A failure toasts and runs nothing, so Cut never deletes text the
   clipboard did not take."
  [target after!]
  (let [^js ed (-> target :surface :quill)
        s (subject target)]
    (if-let [blocked (clipboard-blocked)]
      (report! blocked)
      (-> (write-selection! ed s)
        (.then (fn [_] (after! ed s)))
        (.catch (fn [_] (report! "Couldn't copy the selected text.")))))))

(bus/register-handler! :copy-editor-selection
  (fn [_ctx target] (copy-selection! target (fn [_ _] nil))))

(bus/register-handler! :cut-editor-selection
  (fn [_ctx target]
    (copy-selection! target
      (fn [^js ed {:keys [selection-index selection-length]}]
        (.deleteText ed selection-index selection-length "user")
        (.focus ed)
        (.setSelection ed selection-index 0 "user")))))

;; The document editor's card actions. Both owners live in
;; freememo.card-actions-host; these rows only reach them.
;;
;; Pre : the target is the document editor's caret (:document-editor).
;; Post: Add new card opens AddCardModal, with the selection the right-click
;;       kept in the front field, or empty with none: the range is restored
;;       or cleared first, because the owner reads the editor's selection. Generate cards opens the
;;       whole-document form, or toasts when no owner is mounted.
(bus/register-handler! :add-new-card-at-caret
  (fn [_ctx target]
    (let [^js ed (-> target :surface :quill)
          {:keys [selection-index selection-length]} (subject target)]
      ;; The editor caches its last range for card actions, so with nothing
      ;; selected the cache must go too, or an old passage fills the card.
      (if (pos? selection-length)
        (.setSelection ed selection-index selection-length "silent")
        (rich-text-editor/release-selection!))
      (bus/dispatch! :add-new))))

(bus/register-handler! :generate-document-cards
  (fn [_ctx _target]
    (when-not (bus/run-invoker! :open-document-generate-form)
      (report! "Card generation is not available here."))))

(defn- at-caret!
  "Focus the target's editor with the caret at the subject's index, then call
   `f` with the editor and that index (clamped to the live document).
   Pre : the target's :surface holds :quill."
  [target f]
  (let [^js ed (-> target :surface :quill)
        index (min (:index (subject target)) (max 0 (dec (.getLength ed))))]
    (.focus ed)
    (.setSelection ed index 0 "user")
    (f ed index)))

(bus/register-handler! :insert-formula
  (fn [_ctx target]
    (at-caret! target
      (fn [^js ed index]
        (.insertEmbed ed index "formula" "" "user")
        ;; The new embed is the leaf just after `index`. Open the popover on
        ;; it: an empty commit deletes it again, so Cancel leaves nothing.
        (let [[^js blot] (.getLeaf ed (inc index))]
          (when (and blot (.-domNode blot))
            (formula-ui/open-formula! ed (.-domNode blot))))))))

(bus/register-handler! :insert-table
  (fn [_ctx target]
    (at-caret! target
      (fn [^js ed _index]
        (when-let [^js table (.getModule ed "table")]
          (.insertTable table 3 3))))))

(bus/register-handler! :insert-image
  (fn [_ctx target]
    (at-caret! target (fn [ed index] (editor-actions/insert-image! ed index)))))

(bus/register-handler! :insert-code-block
  (fn [_ctx target]
    (at-caret! target
      (fn [^js ed index] (.formatLine ed index 1 "code-block" true "user")))))

(bus/register-handler! :insert-divider
  (fn [_ctx target]
    (at-caret! target
      (fn [^js ed index]
        (.insertEmbed ed index "divider" true "user")
        (.setSelection ed (inc index) 0 "user")))))

;; ---------------------------------------------------------------------------
;; PDF page
;; ---------------------------------------------------------------------------

;; The selection was read when the menu opened: the menu takes focus, and the
;; text the user right-clicked must be what is copied.
(bus/register-handler! :copy-pdf-selection
  (fn [_ctx target]
    (let [text (:selection-text (subject target))]
      (if (and (.-isSecureContext js/window) (.-clipboard js/navigator))
        (-> (.writeText js/navigator.clipboard text)
          (.catch (fn [_] (report! "Couldn't copy the selected text."))))
        (report! "Copying needs a secure connection. Open the app over https or on localhost.")))))
