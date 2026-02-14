(ns electric-starter-app.rich-text-editor
  "Quill rich text editor integration utilities."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

;; Global editor state (similar to pdf-viewer pattern)
(defonce !editor-state (atom nil))

(defn init-editor!
  "Initialize Quill editor with given container and HTML content.
   Calls on-change callback when content changes with HTML string."
  [container initial-html on-change]
  #?(:clj nil
     :cljs
     (do
       (println "[Quill] Initializing with content:" initial-html)
       (println "[Quill] Container:" container)
       (println "[Quill] window.Quill available?" (some? (.-Quill js/window)))

       (when (and container (.-Quill js/window))
         (let [Quill (.-Quill js/window)
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
               delta (.clipboard.convert editor initial-html)]
           ;; Set initial content
           (when initial-html
             (.setContents editor delta))

           ;; Listen for changes
           (.on editor "text-change"
                (fn [_delta _oldDelta _source]
                  (let [html (.getSemanticHTML editor)]
                    (when on-change (on-change html)))))

           (println "[Quill] Editor initialized successfully")
           (reset! !editor-state {:editor editor})
           editor)))))

(defn set-content!
  "Update editor content with new HTML."
  [html]
  #?(:clj nil
     :cljs (when-let [{:keys [editor]} @!editor-state]
             (let [delta (.clipboard.convert editor html)]
               (.setContents editor delta)))))

(defn get-html!
  "Get current editor content as HTML string."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [editor]} @!editor-state]
             (.getSemanticHTML editor))))

(defn destroy-editor!
  "Destroy editor instance (cleanup)."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [editor]} @!editor-state]
             (reset! !editor-state nil))))
