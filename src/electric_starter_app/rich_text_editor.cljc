(ns electric-starter-app.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [electric-starter-app.logging :as log]
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
         ;; Immediate text-change: sets !dirty-html on every user edit
         ;; e/Offload-latch in callers handles rapid updates via "latest-wins" semantics
         (.on editor "text-change"
           (fn [_delta _oldDelta source]
             (when (= source "user")
               (let [^js root (.-root editor)]
                 (reset! !dirty-html {:html (.-innerHTML root)
                                      :topic-id topic-id})))))
         (reset! !editor-state {:editor editor :container container
                                :topic-id topic-id})
         editor))))

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
  [index length & {:keys [color] :or {color "#44C2FF"}}]
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
