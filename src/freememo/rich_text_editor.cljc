(ns freememo.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.logging :as log]
   [clojure.string :as str]))

;; Global singleton — mirrors pdf_viewer.cljc pattern
(defonce !editor-state (atom nil))

;; Set to the current HTML on every user edit (immediate, no debounce).
;; nil means "no pending user edits" — the auto-save guard checks for non-nil.
;; Reset to nil on init/page-switch so navigating doesn't trigger a save.
;; Shape: {:html "..." :topic-id N}
(defonce !dirty-html (atom nil))


(defn destroy-editor!
  "Destroy the current global editor instance (if any)."
  []
  #?(:clj nil
     :cljs
     (do
       (when-let [{:keys [editor container]} @!editor-state]
         (let [^js ed editor
               ^js ct container]
           (.off ed "text-change")
           (when ct
             ;; Remove Quill's toolbar (sibling before container, created by snow theme)
             (when-let [toolbar (.querySelector (.-parentNode ct) ".ql-toolbar")]
               (.remove toolbar))
             (set! (.-innerHTML ct) "")))
         (reset! !editor-state nil)))))

(defn- add-toolbar-tooltips!
  "Add title attributes to Quill toolbar buttons for hover tooltips."
  [editor]
  #?(:cljs
     (when-let [^js toolbar-el (some-> ^js editor .-container .-parentNode
                                  (.querySelector ".ql-toolbar"))]
       (doseq [[selector title] [[".ql-bold" "Bold"]
                                  [".ql-italic" "Italic"]
                                  [".ql-underline" "Underline"]
                                  [".ql-strike" "Strikethrough"]
                                  [".ql-header[value=\"1\"]" "Heading 1"]
                                  [".ql-header[value=\"2\"]" "Heading 2"]
                                  [".ql-header[value=\"3\"]" "Heading 3"]
                                  [".ql-list[value=\"ordered\"]" "Numbered List"]
                                  [".ql-list[value=\"bullet\"]" "Bullet List"]
                                  [".ql-align .ql-picker-label" "Alignment"]
                                  [".ql-color .ql-picker-label" "Text Color"]
                                  [".ql-background .ql-picker-label" "Background Color"]
                                  [".ql-size .ql-picker-label" "Font Size"]
                                  [".ql-direction" "Text Direction (RTL)"]
                                  [".ql-clean" "Clear Formatting"]]]
         (when-let [^js el (.querySelector toolbar-el selector)]
           (.setAttribute el "title" title))))
     :clj nil))

(defn- setup-mobile-keyboard-suppression!
  "On touch devices, suppress virtual keyboard until double-tap.
   Sets inputmode=\"none\" on .ql-editor; double-tap removes it and
   refocuses to trigger keyboard. Blur re-adds the attribute."
  [editor]
  #?(:cljs
     (let [mq (.matchMedia js/window "(pointer: coarse)")]
       (when (.-matches mq)
         (let [^js root        (.-root editor)
               !last-tap       (atom 0)
               !refocusing     (atom false)
               suppress!       (fn [] (.setAttribute root "inputmode" "none"))
               enter-edit!     (fn []
                                 (.removeAttribute root "inputmode")
                                 (reset! !refocusing true)
                                 (.blur root)
                                 (js/requestAnimationFrame
                                   (fn []
                                     (.focus root)
                                     (reset! !refocusing false))))]
           ;; Double-tap detection via touchend timing
           (.addEventListener root "touchend"
             (fn [^js e]
               (when (zero? (.-length (.-touches e)))
                 (let [now     (js/Date.now)
                       elapsed (- now @!last-tap)]
                   (reset! !last-tap now)
                   (when (< elapsed 300)
                     (enter-edit!)))))
             #js {:passive true})
           ;; Re-suppress on blur (tap outside, back button, app switch)
           (.addEventListener root "blur"
             (fn [_] (when-not @!refocusing (suppress!))))
           ;; Initial suppression
           (suppress!))))
     :clj nil))

(defn init-editor!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton).
   Wires an immediate text-change listener that sets !dirty-html on user edits.
   topic-id tags dirty-html so auto-save targets the correct topic."
  [container initial-html topic-id]
  #?(:clj nil
     :cljs
     (when (and container (.-Quill js/window))
       (log/log-debug (str "init-editor! topic-id=" topic-id " html-len=" (count initial-html)))
       ;; Destroy previous editor if any
       (destroy-editor!)
       ;; Clear dirty flag — fresh content, no pending saves
       (reset! !dirty-html nil)
       (let [Quill (.-Quill js/window)
             cleaned-html (-> (or initial-html "")
                            (str/replace #"^```html\s*\n?" "")
                            (str/replace #"^```\s*\n?" "")
                            (str/replace #"\n?```\s*$" "")
                            str/trim)
             t0 (when js/goog.DEBUG (js/performance.now))
             ^js editor (new Quill container
                          (clj->js {:theme "snow"
                                    :modules {:toolbar [["bold" "italic" "underline" "strike"]
                                                        [{"header" 1} {"header" 2} {"header" 3}]
                                                        [{"size" ["small" false "large" "huge"]}]
                                                        [{"color" []} {"background" []}]
                                                        [{"list" "ordered"} {"list" "bullet"}]
                                                        [{"align" []}]
                                                        [{"direction" "rtl"}]
                                                        ["clean"]]}
                                    :placeholder "Enter text..."}))
             ^js clipboard (.-clipboard editor)
             delta (.convert clipboard cleaned-html)]
         (when (seq cleaned-html)
           (.setContents editor delta))
         (when (and js/goog.DEBUG t0)
           (js/console.log "[Editor perf] init TOTAL:" (.toFixed (- (js/performance.now) t0) 1) "ms, html-len:" (count cleaned-html)))
         ;; Immediate text-change: sets !dirty-html on every user edit
         ;; e/Offload-latch in callers handles rapid updates via "latest-wins" semantics
         ;; Text-change listener reads topic-id from !editor-state (mutable ref)
         ;; so it stays correct across page navigations without reinit
         (.on editor "text-change"
           (fn [_delta _oldDelta source]
             (when (= source "user")
               (let [^js root (.-root editor)
                     current-topic-id (:topic-id @!editor-state)]
                 (reset! !dirty-html {:html (.-innerHTML root)
                                      :topic-id current-topic-id})))))
         (reset! !editor-state {:editor editor :container container
                                :topic-id topic-id})
         (add-toolbar-tooltips! editor)
         (setup-mobile-keyboard-suppression! editor)
         editor))))

(defn set-content!
  "Update Quill content without destroying the editor.
   Uses source 'api' so text-change listener does NOT fire.
   Does NOT set !dirty-html — caller decides persistence."
  [html]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js clipboard (.-clipboard ed)
             cleaned (-> (or html "")
                       (str/replace #"^```html\s*\n?" "")
                       (str/replace #"^```\s*\n?" "")
                       (str/replace #"\n?```\s*$" "")
                       str/trim)
             current-html (.-innerHTML (.-root ed))]
         ;; Skip if content hasn't changed (preserves scroll + highlights)
         (when (not= (count cleaned) (count current-html))
           (let [delta (.convert clipboard cleaned)]
             (.setText ed "" "api")
             (.setContents ed delta "api")
             (.clear (.-history ed))))
         true))))

(defn update-topic-id!
  "Update the topic-id in editor state without reinitializing Quill.
   The text-change listener reads from !editor-state, so this ensures
   dirty-html is tagged with the correct topic-id after page navigation."
  [new-topic-id]
  #?(:cljs (when @!editor-state
             (swap! !editor-state assoc :topic-id new-topic-id))
     :clj nil))

(defn get-current-html!
  "Read innerHTML from the current editor. Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js root (.-root ed)]
         (.-innerHTML root)))))

(defn get-selected-text!
  "Get selected text from Quill editor. Returns nil if no selection.
   Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js selection (.getSelection ed)]
         (when (and selection (> (.-length selection) 0))
           (.getText ed (.-index selection) (.-length selection)))))))

(defn get-selection!
  "Get selected text and its range from Quill. Returns nil if no selection."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js sel (.getSelection ed)]
         (when (and sel (> (.-length sel) 0))
           {:text (.getText ed (.-index sel) (.-length sel))
            :index (.-index sel)
            :length (.-length sel)})))))

(defn get-selection-html!
  "Get selected HTML and range from Quill. Returns nil if no selection.
   Creates a temporary Quill instance to convert Delta -> HTML."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [^js ed editor
             ^js sel (.getSelection ed)]
         (when (and sel (> (.-length sel) 0))
           (let [idx (.-index sel)
                 len (.-length sel)
                 ^js delta (.getContents ed idx len)
                 ^js temp-div (js/document.createElement "div")
                 ^js temp-quill (new (.-Quill js/window) temp-div)
                 _ (.setContents temp-quill delta)
                 html (.-innerHTML (.-root temp-quill))]
             {:html html
              :text (.getText ed idx len)
              :index idx
              :length len}))))))

(defn highlight-range!
  "Apply highlight formatting to a range in the Quill editor.
   Default color is extract blue (#44C2FF). Pass :color for card-generation gold (#FBBF24) etc.
   Also pushes the updated HTML to !dirty-html so the highlight is persisted."
  [index length & {:keys [color] :or {color "var(--color-highlight-extract)"}}]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor topic-id]} @!editor-state]
       (.formatText ^js editor index length
         (clj->js {:background color})
         "api")
       ;; Push updated HTML so the auto-save pipeline persists the highlight
       (let [^js root (.-root editor)]
         (reset! !dirty-html {:html (.-innerHTML root)
                              :topic-id topic-id})))))
