(ns electric-starter-app.rich-text-editor
  "Quill rich text editor integration — global singleton, no reactive callbacks."
  (:require
    [hyperfiddle.electric3 :as e]
    [hyperfiddle.electric-dom3 :as dom]))

;; Global singleton — mirrors pdf_viewer.cljc pattern
(defonce !editor-state (atom nil))

(defn destroy-editor!
  "Destroy the current global editor instance (if any)."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor container]} @!editor-state]
       (.off editor "text-change")
       (when container
         (set! (.-innerHTML container) ""))
       (reset! !editor-state nil))))

(defn init-editor!
  "Initialize Quill editor in the given container with initial HTML.
   Destroys any existing editor first (singleton). No callbacks."
  [container initial-html]
  #?(:clj nil
     :cljs
     (when (and container (.-Quill js/window))
       ;; Destroy previous editor if any
       (destroy-editor!)
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
         (reset! !editor-state {:editor editor :container container})
         editor))))

(defn set-content!
  "Replace the editor's content with new HTML, without creating a new Quill instance."
  [html]
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (let [cleaned (-> (or html "")
                         (clojure.string/replace #"^```html\s*\n?" "")
                         (clojure.string/replace #"^```\s*\n?" "")
                         (clojure.string/replace #"\n?```\s*$" "")
                         clojure.string/trim)
             delta (.clipboard.convert editor cleaned)]
         (.setContents editor delta)))))

(defn get-current-html!
  "Read innerHTML from the current editor. Client-side only, never enters Electric reactive graph."
  []
  #?(:clj nil
     :cljs
     (when-let [{:keys [editor]} @!editor-state]
       (.-innerHTML (.-root editor)))))
