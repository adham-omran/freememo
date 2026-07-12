(ns freememo.jump-to-source-button
  "Toolbar button: from a PDF-page extract, jump to its source page in the PDF.
   Shown only when the extract's parent is a PDF page. Own e/defn/ns to stay
   under the JVM 64KB bytecode limit."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.navigation :as nav]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])))

(defn get-extract-source-page* [extract-id]
  #?(:clj (db/get-extract-source-page extract-id)
     :cljs nil))

(e/defn JumpToSourcePageButton [topic-id navigate!]
  (e/client
    (let [src (e/server (get-extract-source-page* topic-id))]
      (when src
        (let [root (:root src)
              page (:page src)]
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :style {:font-weight "500"}
                        :aria-label "Go to source page"})
            (tooltip/Tooltip! "Open the source page in the PDF")
            (icons/Icon :link :size 16)
            (dom/span (dom/props {:class "icon-label"})
              (dom/text (str "Go to page " page)))
            (dom/On "click"
              (fn [_]
                (reset! nav/!pending-page-jump {:root root :page page})
                (navigate! :viewer (nav/nav-topic root nil)))
              nil)))))))
