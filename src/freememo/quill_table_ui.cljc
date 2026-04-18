(ns freememo.quill-table-ui
  "Quill table authoring UI — toolbar handler + contextual action bar.")

(defn- build-action-bar!
  "Create the floating div with buttons wired to the Quill table module."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           bar (js/document.createElement "div")
           actions [["Row Above" "insertRowAbove"]
                    ["Row Below" "insertRowBelow"]
                    ["Col Left" "insertColumnLeft"]
                    ["Col Right" "insertColumnRight"]
                    ["Del Row" "deleteRow"]
                    ["Del Col" "deleteColumn"]
                    ["Del Table" "deleteTable"]]]
       (set! (.-className bar) "ql-table-actions")
       (doseq [[label method] actions]
         (let [btn (js/document.createElement "button")]
           (set! (.-type btn) "button")
           (set! (.-textContent btn) label)
           ;; mousedown.preventDefault keeps editor focus so table module sees the cell
           (.addEventListener btn "mousedown"
             (fn [^js e] (.preventDefault e)))
           (.addEventListener btn "click"
             (fn [^js e]
               (.preventDefault e)
               (when-let [^js table-mod (.getModule ed "table")]
                 (js-invoke table-mod method))))
           (.appendChild bar btn)))
       bar)))

(defn init!
  "Wire the Quill table toolbar handler and the contextual action bar."
  [editor]
  #?(:clj nil
     :cljs
     (let [^js ed editor
           ^js toolbar (.getModule ed "toolbar")
           ^js table-mod (.getModule ed "table")
           ;; host is the editor container (our .quill-editor-wrapper div, which
           ;; Quill mutates in place to also carry .ql-container.ql-snow). CSS
           ;; gives it position: relative so the absolutely-positioned action
           ;; bar anchors to the editor, not the page viewport.
           ^js host (.-container ed)]
       (when (and host toolbar table-mod)
         (when-let [^js old (.querySelector host ".ql-table-actions")]
           (.remove old))
         (.addHandler toolbar "table"
           (fn [_] (.insertTable table-mod 3 3)))
         (let [bar (build-action-bar! ed)]
           (.appendChild host bar)
           (.on ed "selection-change"
             (fn [^js range _old _source]
               (let [^js info (when range (.getTable table-mod range))]
                 (if (and info (aget info 0))
                   (.add (.-classList bar) "visible")
                   (.remove (.-classList bar) "visible")))))
           nil)))))
