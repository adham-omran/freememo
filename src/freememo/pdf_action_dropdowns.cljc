(ns freememo.pdf-action-dropdowns
  "PDF page-action dropdowns (C1/C2) — two collapse-into-one controls for the
   PDF sub-toolbar, mirroring `toolbar_sync_dropdown.cljc`:

     ScanDropdown — Scan Page + Compare OCR
     CopyDropdown — Copy text + Copy all text

   Each dropdown renders its real source buttons (hidden, in a
   `toolbar-dropdown-sources` wrapper) so their tokens / modals / keyboard
   shortcut stay live; menu items dispatch their commands through
   `freememo.command-bus`. Owns `start-ocr-scan!` (relocated from pdf_toolbar
   so that pdf_toolbar can require this ns without a cycle)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.logging :as log]
   [freememo.icons :as icons]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.copy-text :as copy]
   [freememo.ocr-compare :as ocr-compare]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.kg-extract :as kg])
   #?(:clj [freememo.kg-questions :as kgq])
   #?(:clj [freememo.toasts :as toasts])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [missionary.core :as m]))
  #?(:clj (:import [missionary Cancelled]
                   [java.util.concurrent Executors])))

;; ---------------------------------------------------------------------------
;; OCR scan infrastructure (relocated from pdf_toolbar.cljc — ScanDropdown is
;; now the sole caller). Submits an OCR scan as a Missionary task on a bounded
;; executor; timeout after 30s; cancel fn stored in per-user :scan-cancellers.
;; ---------------------------------------------------------------------------

#?(:clj (defonce ocr-executor (Executors/newFixedThreadPool 3)))

(defn start-ocr-scan!
  [uid doc page ek dpi]
  #?(:clj
     (do
       (swap! (us/get-atom uid :scanning-pages) conj [doc page])
       (swap! (us/get-atom uid :ocr-errors) dissoc [doc page])
       (log/log-info (str "OCR scan started topic-id=" doc " page=" page))
       (let [cancel-fn
             ((m/timeout
                (m/via ocr-executor (page/extract-page-text uid doc page ek dpi))
                30000
                {:success false :error "Scan timed out after 30 seconds"})
              (fn [result]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (if (:success result)
                  (do (log/log-info (str "OCR scan complete topic-id=" doc " page=" page))
                    (commands/bump! uid :scan))
                  (do (log/log-info (str "OCR scan failed topic-id=" doc " page=" page " error=" (:error result)))
                    (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (:error result))
                    (toasts/push! uid
                      {:level :error
                       :message (:error result)
                       :actions (if (= :insufficient-credits (:error-type result))
                                  [{:label "Top up credits" :nav :settings}]
                                  [])})))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page]))
              (fn [e]
                (swap! (us/get-atom uid :scan-cancellers) dissoc [doc page])
                (when-not (instance? Cancelled e)
                  (log/log-info (str "OCR scan error topic-id=" doc " page=" page " ex=" (.getMessage e)))
                  (swap! (us/get-atom uid :ocr-errors) assoc [doc page] (.getMessage e))
                  (toasts/push! uid {:level :error :message (.getMessage e)}))
                (swap! (us/get-atom uid :scanning-pages) disj [doc page])))]
         (swap! (us/get-atom uid :scan-cancellers) assoc [doc page] cancel-fn))
       nil)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Knowledge-graph command sources — hidden invokable buttons giving the
;; palette/keyboard a per-open-document target: :distill and
;; :generate-questions act on the PDF open in the viewer (plan doc, palette
;; integration, design C). No visible UI — the Knowledge tab keeps the
;; browsing buttons. Guards live server-side: start-distill! no-ops on an
;; in-flight run; exhausted question generation toasts.
;; ---------------------------------------------------------------------------

(e/defn KgCommandSources []
  (e/client
    (let [user-id dctx/user-id
          doc dctx/pdf-root-id]
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (dom/button
          (dom/props {:aria-hidden "true" :tabindex "-1"})
          (let [node dom/node]
            (bus/publish-invoker! :distill (fn [] (.click node)))
            (e/on-unmount (fn [] (bus/retract-invoker! :distill))))
          (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
                [t _] (e/Token click)]
            (when t
              (case (e/server (kg/start-distill! user-id doc))
                (t)))))
        (dom/button
          (dom/props {:aria-hidden "true" :tabindex "-1"})
          (let [node dom/node]
            (bus/publish-invoker! :generate-questions (fn [] (.click node)))
            (e/on-unmount (fn [] (bus/retract-invoker! :generate-questions))))
          (let [click (dom/On "click" (fn [_] {:id (str (random-uuid))}) nil)
                [t _] (e/Token click)]
            (when t
              (case (e/server (kgq/start-atomic-generation! user-id doc))
                (t)))))))))

;; ---------------------------------------------------------------------------
;; Click-outside / Escape listener for an open menu. Plain defn (not in an
;; e/defn body); see toolbar_sync_dropdown.cljc for rationale.
;; ---------------------------------------------------------------------------

(defn install-dropdown-listeners! [!open trigger-class menu-class]
  #?(:cljs
     (let [trigger-sel (str "." trigger-class)
           menu-sel (str "." menu-class)
           on-key (fn [e] (when (= (.-key e) "Escape") (reset! !open false)))
           on-mouse (fn [e]
                      (let [target (.-target e)]
                        (when-not (or (.closest target menu-sel)
                                    (.closest target trigger-sel))
                          (reset! !open false))))]
       (.addEventListener js/document "keydown" on-key)
       (.addEventListener js/document "mousedown" on-mouse)
       (fn []
         (.removeEventListener js/document "keydown" on-key)
         (.removeEventListener js/document "mousedown" on-mouse)))
     :clj (fn [] nil)))

(defn- dispatch-item!
  "Dispatch a menu item's command through the bus, then close the menu.
   No-op (but still closes) when the command is unavailable — same semantics
   as the old unset-ref / disabled-button check."
  [command-id !open]
  #?(:cljs
     (do (bus/dispatch! command-id)
         (reset! !open false))
     :clj nil))

;; ---------------------------------------------------------------------------
;; ScanDropdown (C1) — Scan Page + Compare OCR.
;; ---------------------------------------------------------------------------

(e/defn ScanDropdown
  "Pre:  llm-enabled? true (caller gates); cfg carries the scan target.
   Post: trigger toggles a menu of [Scan Page, Compare OCR]; each dispatches the
         hidden source button. Scan keeps !scan-btn-ref (keyboard shortcut)."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key pdf-root-id dctx/pdf-root-id
          page-number dctx/page-number scan-dpi dctx/scan-dpi scanning? dctx/scanning?
          !open (atom false)
          open (e/watch !open)]

      ;; Hidden sources — kept mounted so tokens/modals/shortcut stay live.
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :aria-label "Scan page"
                      :disabled scanning?})
          (let [node dom/node]
            (bus/publish-invoker! :scan (fn [] (.click node)))
            (e/on-unmount (fn [] (bus/retract-invoker! :scan))))
          (let [click-event (dom/On "click"
                              (fn [_] {:id (str (random-uuid)) :page page-number})
                              nil)
                [t _] (e/Token click-event)]
            (when t
              (case (e/server
                      (let [pg (:page click-event)
                            doc pdf-root-id
                            uid user-id
                            ek enc-key]
                        (if (contains? @(us/get-atom uid :scanning-pages) [doc pg])
                          (log/log-info (str "OCR scan already in progress topic-id=" doc " page=" pg))
                          (start-ocr-scan! uid doc pg ek scan-dpi))))
                (t)))))
        (ocr-compare/OcrCompareButton user-id pdf-root-id page-number scan-dpi))

      ;; Visible dropdown.
      (dom/div
        (dom/props {:class "toolbar-dropdown toolbar-scan-dropdown"})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-dropdown-trigger toolbar-scan-trigger"
                      :aria-haspopup "menu"
                      :aria-expanded (if open "true" "false")
                      :aria-label "Scan menu"})
          (tooltip/Tooltip! "Scan this page with AI OCR, or compare OCR models")
          (if scanning?
            (icons/Icon :loader-2 :size 16 :class "spin")
            (icons/Icon :sparkles :size 16))
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if scanning? "Scanning…" "Scan Page")))
          (icons/Icon :chevron-down :size 14)
          (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) (swap! !open not)) nil))

        (when open
          (let [cleanup (install-dropdown-listeners! !open "toolbar-scan-trigger" "toolbar-scan-menu")]
            (e/on-unmount cleanup)
            (dom/div
              (dom/props {:class "toolbar-dropdown-menu toolbar-scan-menu" :role "menu"})
              (dom/button
                (dom/props {:class "toolbar-dropdown-item" :role "menuitem" :aria-label "Scan page"})
                (icons/Icon :sparkles :size 16)
                (dom/span (dom/text (if scanning? "Scanning…" "Scan Page")))
                (dom/On "click" (fn [_] (dispatch-item! :scan !open)) nil))
              (dom/button
                (dom/props {:class "toolbar-dropdown-item" :role "menuitem" :aria-label "Compare OCR"})
                (icons/Icon :scan-text :size 16)
                (dom/span (dom/text "Compare OCR"))
                (dom/On "click" (fn [_] (dispatch-item! :compare-ocr !open)) nil)))))))))

;; ---------------------------------------------------------------------------
;; CopyDropdown (C2) — Copy text + Copy all text.
;; ---------------------------------------------------------------------------

(e/defn CopyDropdown
  "Pre:  cfg carries the copy scope + per-PDF extract style.
   Post: trigger toggles a menu of [Copy text, Copy all text]; each dispatches
         the hidden source button (which owns busy-state + the compare modal)."
  []
  (e/client
    (let [user-id dctx/user-id pdf-root-id dctx/pdf-root-id page-number dctx/page-number
          extract-style dctx/extract-style
          !open (atom false)
          open (e/watch !open)]

      ;; Hidden sources.
      (dom/div
        (dom/props {:class "toolbar-dropdown-sources"})
        (copy/CopyTextButton user-id pdf-root-id page-number extract-style)
        (copy/CopyAllTextButton user-id pdf-root-id extract-style))

      ;; Visible dropdown.
      (dom/div
        (dom/props {:class "toolbar-dropdown toolbar-copy-dropdown"})
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary toolbar-dropdown-trigger toolbar-copy-trigger"
                      :aria-haspopup "menu"
                      :aria-expanded (if open "true" "false")
                      :aria-label "Copy menu"})
          (tooltip/Tooltip! "Copy the PDF's own text (no AI)")
          (icons/Icon :clipboard :size 16)
          (dom/span (dom/props {:class "icon-label"}) (dom/text "Copy text"))
          (icons/Icon :chevron-down :size 14)
          (dom/On "click" (fn [e] #?(:cljs (.stopPropagation e)) (swap! !open not)) nil))

        (when open
          (let [cleanup (install-dropdown-listeners! !open "toolbar-copy-trigger" "toolbar-copy-menu")]
            (e/on-unmount cleanup)
            (dom/div
              (dom/props {:class "toolbar-dropdown-menu toolbar-copy-menu" :role "menu"})
              (dom/button
                (dom/props {:class "toolbar-dropdown-item" :role "menuitem" :aria-label "Copy text"})
                (icons/Icon :clipboard :size 16)
                (dom/span (dom/text "Copy text"))
                (dom/On "click" (fn [_] (dispatch-item! :copy-text !open)) nil))
              (dom/button
                (dom/props {:class "toolbar-dropdown-item" :role "menuitem" :aria-label "Copy all text"})
                (icons/Icon :library :size 16)
                (dom/span (dom/text "Copy all text"))
                (dom/On "click" (fn [_] (dispatch-item! :copy-all !open)) nil)))))))))
