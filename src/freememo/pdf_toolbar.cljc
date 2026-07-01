(ns freememo.pdf-toolbar
  "Second command bar, rendered under the main ContentToolbar and shown only
   when a PDF item is in view. Hosts every PDF-scoped control that previously
   lived split across PdfViewerComponent (page-nav, zoom, layout-toggle) and
   EditorPane's page-header (mark-page-done + Scan / Compare-OCR / Copy-text /
   Copy-all + the Live-Document add-photos affordance).

   Styling: every control is a ghost .btn-secondary inside a .toolbar-group, so
   the main toolbar's transparent-at-rest / hover-fill rule applies — the whole
   row matches the main ContentToolbar (S2).

   Two layouts:
     desktop/tablet — all groups inline; action buttons collapse to icon-only
       under the content-aware overflow mechanism (install-overflow-detector!;
       tier-1 hides .icon-label). Page-nav and zoom glyphs carry no .icon-label.
     phone (C3) — two primary controls + a ⋮ overflow dropdown holding the rest.
       Live docs get the add-photos buttons as primaries; other PDFs get
       page-nav. Everything else (page-nav for live, zoom, mark-done, scan/copy)
       drops into .pdf-overflow-panel, one tap away."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.doc-context :as dctx]
   [freememo.icons :as icons]
   [freememo.keyboard :as keyboard]
   [freememo.copy-text :as copy]
   [freememo.pdf-action-dropdowns :refer [ScanDropdown CopyDropdown]]
   [freememo.pdf-viewer :as viewer]
   [freememo.pdf-viewer-component :refer [LiveDocAddPhotos]]
   [freememo.toolbar-overflow :refer [install-overflow-detector!]]
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.db :as db])))

;; ---------------------------------------------------------------------------
;; Page-done status read. Wraps the DB call with a _refresh-first arg so the
;; reactive call site re-queries when :meta-refresh bumps — the done button
;; reflects a toggle immediately (a native checkbox got this free from the
;; browser; a button must be driven by the reactive value).
;; ---------------------------------------------------------------------------

(defn page-done?*
  [_refresh pdf-root-id page-number]
  #?(:clj (boolean (when (and pdf-root-id page-number)
                     (db/get-page-done-status pdf-root-id page-number)))
     :cljs nil))

;; ---------------------------------------------------------------------------
;; Control groups — extracted so both the desktop (inline) and phone (overflow
;; panel) layouts render them once each, without duplicating their logic.
;; ---------------------------------------------------------------------------

(e/defn PdfPageNav
  "◀ [page] ▶ navigation. Owns its own input-val/focus atoms so it can mount in
   either the inline bar or the overflow panel. Syncs the input to the live page
   when the user isn't typing."
  []
  (e/client
    (let [page-number dctx/page-number total dctx/total on-page-change! dctx/on-page-change!
          !input-val   (atom (str page-number))
          input-val    (e/watch !input-val)
          !inp-focused (atom false)
          inp-focused  (e/watch !inp-focused)
          nav! (fn [p]
                 (viewer/go-to-page! p)
                 (when on-page-change! (on-page-change! p)))]
      (when (and (not inp-focused) (not= input-val (str page-number)))
        (reset! !input-val (str page-number)))
      (dom/div
        (dom/props {:class "toolbar-group"
                    :style {:display "flex" :align-items "center" :gap "4px"}})
        (let [prev-disabled? (or (= page-number 1) (= total 0))]
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :title "Previous Page" :disabled prev-disabled?})
            (dom/text "◀")
            (dom/On "click"
              (fn [_] (when (and (> page-number 1) (> total 0))
                        (nav! (dec page-number)))) nil)))
        (dom/input
          (dom/props {:type "text" :value input-val
                      :style {:width "40px" :text-align "center" :padding "4px"
                              :border "1px solid var(--color-border)"
                              :border-radius "3px" :font-size "14px"}})
          (dom/On "focus" (fn [_] (reset! !inp-focused true)) nil)
          (dom/On "blur"
            (fn [e]
              (let [raw (-> e .-target .-value)]
                (reset! !inp-focused false)
                (let [n (js/parseInt raw)]
                  (when (and (not (js/isNaN n)) (>= n 1) (<= n total))
                    (nav! (max 1 (min total n)))))))
            nil)
          (dom/On "keydown"
            (fn [e] (when (= "Enter" (.-key e)) (.blur (.-target e)))) nil)
          (let [v (dom/On "input" (fn [e] (-> e .-target .-value)) nil)]
            (when (some? v) (reset! !input-val v))))
        (dom/span
          (dom/props {:style {:color "var(--color-text-primary)" :padding "0 4px"}})
          (dom/text "of " total))
        (let [next-disabled? (or (>= page-number total) (= total 0))]
          (dom/button
            (dom/props {:class "btn btn-sm btn-secondary"
                        :title "Next Page" :disabled next-disabled?})
            (dom/text "▶")
            (dom/On "click"
              (fn [_] (when (and (< page-number total) (> total 0))
                        (nav! (inc page-number)))) nil)))))))

(e/defn PdfZoom
  "Zoom −/+ + preset select + layout toggle."
  []
  (e/client
    (let [layout dctx/layout on-layout-toggle! dctx/on-layout-toggle!]
    (dom/div
      (dom/props {:class "toolbar-group"
                  :style {:display "flex" :align-items "center" :gap "4px"
                          :padding-left "12px"
                          :border-left "1px solid var(--color-border)"}})
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom Out"})
        (dom/text "−")
        (e/for [_ (dom/On-all "click")] (viewer/zoom! 0.9)))
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary" :title "Zoom In"})
        (dom/text "+")
        (e/for [_ (dom/On-all "click")] (viewer/zoom! 1.1)))
      (dom/select
        (dom/props {:style {:padding "6px 8px" :cursor "pointer"
                            :background "transparent"
                            :color "var(--color-text-label)"
                            :border "1px solid transparent"
                            :border-radius "3px"}})
        (dom/option (dom/props {:value "page-width"}) (dom/text "Page Width"))
        (dom/option (dom/props {:value "page-fit"}) (dom/text "Page Fit"))
        (dom/option (dom/props {:value "0.5"}) (dom/text "50%"))
        (dom/option (dom/props {:value "0.75"}) (dom/text "75%"))
        (dom/option (dom/props {:value "1.0"}) (dom/text "100%"))
        (dom/option (dom/props {:value "1.25"}) (dom/text "125%"))
        (dom/option (dom/props {:value "1.5"}) (dom/text "150%"))
        (dom/option (dom/props {:value "2.0"}) (dom/text "200%"))
        (e/for [[t e] (dom/On-all "change")]
          (when e
            (let [v (-> e .-target .-value)]
              (case v
                "page-width" (viewer/set-zoom-fit!)
                "page-fit" (viewer/set-zoom-page-fit!)
                (let [scale (js/parseFloat v)]
                  (when-not (js/isNaN scale) (viewer/set-zoom! scale))))))
          (t)))
      (when on-layout-toggle!
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :style {:font-size "14px"}
                      :data-tooltip (if (= layout "top-bottom")
                                      "Switch to side-by-side layout"
                                      "Switch to stacked layout")})
          (dom/text (if (= layout "top-bottom") "⇅" "⇄"))
          (dom/On "click" (fn [_] (on-layout-toggle!)) nil)))))))

(e/defn PdfMarkDone
  "Mark-page-done toggle — plain ghost secondary button, check/rotate icon,
   Done/Restore label. Registers the keyboard ref so the Done shortcut works."
  []
  (e/client
    (let [user-id dctx/user-id pdf-root-id dctx/pdf-root-id page-number dctx/page-number]
    (dom/div
      (dom/props {:class "toolbar-group"
                  :style {:padding-left "12px"
                          :border-left "1px solid var(--color-border)"}})
      (let [refresh    (e/server (e/watch (us/get-atom user-id :meta-refresh)))
            page-done? (e/server (page-done?* refresh pdf-root-id page-number))]
        (dom/button
          (dom/props {:class "btn btn-sm btn-secondary"
                      :aria-label (if page-done?
                                    (str "Restore page " page-number)
                                    (str "Mark page " page-number " as done"))
                      :data-tooltip "Mark this page as completed to track your extraction progress"})
          (icons/Icon (if page-done? :rotate-ccw :check) :size 16)
          (dom/span (dom/props {:class "icon-label"})
            (dom/text (if page-done?
                        (str "Restore page " page-number)
                        (str "Mark page " page-number " as done"))))
          (reset! keyboard/!done-btn-ref dom/node)
          (e/on-unmount (fn [] (reset! keyboard/!done-btn-ref nil)))
          (let [click-event (dom/On "click"
                              (fn [_] {:id (str (random-uuid)) :page page-number})
                              nil)
                [t _] (e/Token click-event)]
            (when t
              (case (e/server (db/toggle-page-done! pdf-root-id (:page click-event)))
                (case (e/server (swap! (us/get-atom user-id :meta-refresh) inc))
                  (t)))))))))))

(e/defn PdfScanCopy
  "Scan (+Compare OCR) / Copy (+Copy all) dropdowns, plus the Live-Document
   add-photos buttons when `is-live?` (the inline-bar use; on phone the photos
   are surfaced as primaries instead, so callers pass is-live? false here)."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key pdf-root-id dctx/pdf-root-id
          page-number dctx/page-number scan-dpi dctx/scan-dpi llm-enabled? dctx/llm-enabled?
          scanning-pages dctx/scanning-pages extract-style dctx/extract-style is-live? dctx/is-live?]
      (dom/div
        (dom/props {:class "toolbar-group"})
        (when llm-enabled?
          (binding [dctx/scanning? (contains? scanning-pages [pdf-root-id page-number])]
            (ScanDropdown)))
        (CopyDropdown)
        (when is-live?
          (binding [dctx/document-id pdf-root-id dctx/compact? true]
            (LiveDocAddPhotos)))))))

(e/defn PdfOcrError
  "OCR error display — auto-dismiss after 3s."
  []
  (e/client
    (let [pdf-root-id dctx/pdf-root-id page-number dctx/page-number ocr-errors dctx/ocr-errors]
    (when-let [ocr-err (get ocr-errors [pdf-root-id page-number])]
      (let [!show (atom true)
            show (e/watch !show)]
        (dom/div
          (dom/props {:style {:padding "6px 10px"
                              :background "var(--color-danger-bg)"
                              :border "1px solid var(--color-danger-light)"
                              :border-radius "var(--radius-sm)"
                              :font-size "13px"
                              :color "var(--color-danger-dark)"
                              :opacity (if show "1" "0")
                              :transition "opacity 0.5s ease-out"}})
          (dom/text ocr-err)
          (e/client
            (js/setTimeout (fn [] (reset! !show false)) 3000))))))))

(e/defn PdfToolbar
  "props: {:user-id :enc-key :pdf-root-id :page-number :total :layout :is-live?
           :scan-dpi :llm-enabled? :scanning-pages :ocr-errors :phone?
           :on-page-change! :on-layout-toggle!}
   `pdf-root-id` is the document root — it serves as document-id (LiveDoc),
   done/scan target, and copy-extract scope (all the same value upstream).
   `phone?` selects the compact C3 layout (two primaries + ⋮ overflow)."
  []
  (e/client
    (let [user-id dctx/user-id enc-key dctx/enc-key pdf-root-id dctx/pdf-root-id
          page-number dctx/page-number total dctx/total layout dctx/layout is-live? dctx/is-live?
          scan-dpi dctx/scan-dpi llm-enabled? dctx/llm-enabled? scanning-pages dctx/scanning-pages
          ocr-errors dctx/ocr-errors phone? dctx/phone?
          on-page-change! dctx/on-page-change! on-layout-toggle! dctx/on-layout-toggle!
          ;; Reactive per-PDF extraction style (drives Copy text / Copy all).
          settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          extract-style (e/server (copy/get-extract-style* settings-refresh user-id pdf-root-id))]
      (if phone?
        ;; ── Phone (C3): two primaries + ⋮ overflow dropdown ──────────────
        (let [!overflow-open (atom false)
              overflow-open  (e/watch !overflow-open)]
          (dom/div
            (dom/props {:class "toolbar-container pdf-toolbar-container"})
            (dom/div
              (dom/props {:class "toolbar pdf-toolbar-bar"})
              ;; Primary controls — live doc: add-photos; otherwise page-nav.
              (if is-live?
                (dom/div
                  (dom/props {:class "toolbar-group"})
                  (binding [dctx/document-id pdf-root-id dctx/compact? true]
                    (LiveDocAddPhotos)))
                (PdfPageNav))
              ;; ⋮ trigger (pushed right).
              (dom/div
                (dom/props {:class "toolbar-overflow-trigger"
                            :style {:display "flex" :margin-left "auto"}})
                (dom/button
                  (dom/props {:class "btn btn-sm btn-secondary"
                              :aria-label "More PDF controls" :data-tooltip "More"
                              :style {:font-weight "bold" :font-size "16px" :line-height "1"}})
                  (icons/Icon :more-vertical :size 16)
                  (dom/On "click" (fn [_] (swap! !overflow-open not)) nil)))
              ;; Overflow panel — secondary controls, one tap away.
              (when overflow-open
                (dom/div
                  (dom/props {:class "pdf-overflow-panel"})
                  (when is-live? (PdfPageNav))
                  (PdfZoom)
                  (PdfMarkDone)
                  (binding [dctx/extract-style extract-style dctx/is-live? false]
                    (PdfScanCopy))))
              (PdfOcrError))
            (when overflow-open
              (dom/div
                (dom/props {:class "pdf-overflow-backdrop"})
                (dom/On "click" (fn [_] (reset! !overflow-open false)) nil)))))

        ;; ── Desktop / tablet: all groups inline + content-aware collapse ──
        (let [!tier (atom 0)
              _tier (e/watch !tier)
              !overflow-open (atom false)]
          (dom/div
            ;; pdf-toolbar-container: ranks this bar's stacking context below the
            ;; main ContentToolbar and above EditorToolbar, so dropped tooltips
            ;; and the modal backdrop layer correctly across the stacked bars.
            (dom/props {:class "toolbar-container pdf-toolbar-container"})
            (let [container-node dom/node]
              (dom/div
                (dom/props {:class "toolbar pdf-toolbar-bar"})
                (let [toolbar-node dom/node
                      cleanup (install-overflow-detector!
                                container-node toolbar-node !tier !overflow-open)]
                  (e/on-unmount cleanup)
                  (PdfPageNav)
                  (PdfZoom)
                  (PdfMarkDone)
                  (binding [dctx/extract-style extract-style]
                    (PdfScanCopy))
                  (PdfOcrError))))))))))
