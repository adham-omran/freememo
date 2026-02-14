(ns electric-starter-app.rich-text-editor
  "Quill rich text editor integration — global singleton."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

;; Global singleton — mirrors pdf_viewer.cljc pattern
(defonce !editor-state (atom nil))

;; Set to the current HTML only after a user edit (debounced).
;; nil means "no pending user edits" — the auto-save guard checks for non-nil.
;; Reset to nil on init/page-switch so navigating doesn't trigger a save.
(defonce !dirty-html (atom nil))

#?(:cljs (defonce !debounce-timer (atom nil)))

(defn destroy-editor!
  "Destroy the current global editor instance (if any)."
  []
  #?(:clj nil
     :cljs
     (do
       (when-let [timer @!debounce-timer]
         (js/clearTimeout timer)
         (reset! !debounce-timer nil))
       (when-let [{:keys [editor container]} @!editor-state]
         (.off editor "text-change")
         (when container
           ;; Remove Quill's toolbar (sibling before container, created by snow theme)
           (when-let [toolbar (.querySelector (.-parentNode container) ".ql-toolbar")]
             (.remove toolbar))
           (set! (.-innerHTML container) ""))
         (reset! !editor-state nil)))))

(defn init-editor!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton).
   Wires a debounced text-change listener that sets !dirty-html on user edits.
   page-number and doc-id are tagged onto dirty-html so auto-save targets the correct page."
  [container initial-html page-number doc-id]
  #?(:clj nil
     :cljs
     (when (and container (.-Quill js/window))
       ;; Destroy previous editor if any
       (destroy-editor!)
       ;; Clear dirty flag — fresh content, no pending saves
       (reset! !dirty-html nil)
       (let [Quill (.-Quill js/window)
             cleaned-html (-> (or initial-html "")
                              (clojure.string/replace #"^```html\s*\n?" "")
                              (clojure.string/replace #"^```\s*\n?" "")
                              (clojure.string/replace #"\n?```\s*$" "")
                              clojure.string/trim)
             editor (new Quill container
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
             delta (.clipboard.convert editor cleaned-html)]
         (when (seq cleaned-html)
           (.setContents editor delta))
         ;; Debounced text-change: only on user edits, sets !dirty-html after 500ms
         ;; Stores a map with :html, :page, :doc-id so auto-save targets the correct page
         (.on editor "text-change"
              (fn [_delta _oldDelta source]
                (when (= source "user")
                  (when-let [timer @!debounce-timer]
                    (js/clearTimeout timer))
                  (reset! !debounce-timer
                    (js/setTimeout
                      (fn []
                        (reset! !dirty-html {:html (.-innerHTML (.-root editor))
                                             :page page-number
                                             :doc-id doc-id}))
                      500)))))
         (reset! !editor-state {:editor editor :container container})
         editor))))

(defn get-current-html!
  "Read innerHTML from the current editor. Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (.-innerHTML (.-root editor)))))

(defn get-selected-text!
  "Get selected text from Quill editor. Returns nil if no selection.
   Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [selection (.getSelection editor)]
         (when (and selection (not= (.-index selection) (.-length selection)))
           (.getText editor (.-index selection) (.-length selection)))))))
