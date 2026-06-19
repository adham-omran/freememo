(ns freememo.copy-text
  "\"Copy text\" — the single native text-extraction button (replaces the old
   Extract(PDFBox)/Extract(PDF.js) pair). Reads the per-PDF extraction style:
   set → run that engine and save silently; unset → run BOTH and show a compare
   modal so the user picks the better output and optionally remembers it.

   client = PDF.js (browser), remote = PDFBox (server). Both normalize to the
   same paragraph HTML, so the modal compares like-for-like."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   [freememo.pdf-viewer :as pdfviewer]
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.user-state :as us])))

;; ── Server bridges (plain defns; CLJS gets a no-op) ────────────────────────

(defn remote-extract-save!* [user-id root page]
  #?(:clj (let [r (page/extract-page-text-pdfbox root page)]
            (when (:success r) (swap! (us/get-atom user-id :refresh) inc))
            r)
     :cljs nil))

(defn client-save!* [user-id root page raw]
  #?(:clj (let [r (page/save-pdfjs-text! root page raw)]
            (when (:success r) (swap! (us/get-atom user-id :refresh) inc))
            r)
     :cljs nil))

(defn preview-remote* [root page]
  #?(:clj (page/preview-pdfbox-html root page) :cljs nil))

(defn preview-client* [raw]
  #?(:clj (page/preview-pdfjs-html raw) :cljs nil))

(defn commit-html!* [user-id root page html]
  #?(:clj (let [r (page/save-page-html-impl root page html)]
            (when (:success r) (swap! (us/get-atom user-id :refresh) inc))
            r)
     :cljs nil))

(defn get-extract-style*
  "Reactive read of the per-PDF style; `_refresh` (a :settings-refresh value)
   forces re-read after a save."
  [_refresh user-id root]
  #?(:clj (settings/get-extract-style user-id root) :cljs nil))

(defn save-style!* [user-id root style]
  #?(:clj (let [r (settings/save-extract-style user-id root style)]
            (swap! (us/get-atom user-id :settings-refresh) inc)
            r)
     :cljs nil))

;; ── Compare modal ──────────────────────────────────────────────────────────

(e/defn ResultPanel [title result]
  (e/client
    (dom/div
      (dom/props {:style {:flex "1" :min-width "0" :display "flex" :flex-direction "column"
                          :border "1px solid var(--color-border)" :border-radius "var(--radius-md)"
                          :overflow "hidden"}})
      (dom/div
        (dom/props {:style {:padding "6px 10px" :font-size "12px" :font-weight "600"
                            :background "var(--color-bg-subtle)"
                            :border-bottom "1px solid var(--color-border)"}})
        (dom/text title))
      (dom/div
        (dom/props {:style {:padding "10px" :overflow "auto" :max-height "40vh" :font-size "13px"}})
        (cond
          (nil? result) (dom/text "Extracting…")
          (:success result) (set! (.-innerHTML dom/node) (:text result))
          :else (dom/span (dom/props {:style {:color "var(--color-text-hint)"}})
                  (dom/text (or (:error result) "No text"))))))))

(e/defn PickButton
  "Commit the chosen text to the page; if Remember is checked, also persist the
   style. Closes the modal on success."
  [label user-id root-topic-id page html style enabled? !remember !compare]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :disabled (not enabled?)
                  :style {:width "100%"}})
      (dom/text label)
      (let [click (dom/On "click" (fn [_] @!remember) nil)
            [t _] (e/Token click)]
        (when t
          (e/on-unmount #(reset! !compare nil))
          (case (e/server (e/Offload #(commit-html!* user-id root-topic-id page html)))
            (case (if click
                    (e/server (save-style!* user-id root-topic-id style))
                    :skip)
              (t))))))))

(e/defn CompareModal
  "Runs both previews on mount, shows them side-by-side, commits the picked text
   to the current page, and (if Remember is checked) saves the per-PDF style."
  [user-id root-topic-id state !compare]
  (e/client
    (let [{:keys [page raw]} state
          a (e/server (preview-client* raw))
          b (e/server (e/Offload #(preview-remote* root-topic-id page)))
          a-ok (boolean (:success a))
          b-ok (boolean (:success b))
          both-done (and (some? a) (some? b))
          both-fail (and both-done (not a-ok) (not b-ok))
          !remember (atom false)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [_] (reset! !compare nil)) nil)
        (dom/div
          (dom/props {:class "modal-content"
                      :style {:width "min(880px, 95vw)" :max-height "85vh"
                              :display "flex" :flex-direction "column" :gap "12px" :padding "16px 20px"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin "0" :font-size "16px" :font-weight "500"}})
            (dom/text (str "Copy text — page " page)))

          (if both-fail
            (dom/div
              (dom/props {:style {:padding "20px 4px" :color "var(--color-text-secondary)" :font-size "13px"}})
              (dom/text "No extractable text in this PDF — use Scan Page."))
            (dom/div
              (dom/props {:style {:display "flex" :flex-direction "column" :gap "12px"}})
              ;; Two options side-by-side: each preview with its own pick button
              ;; directly beneath. Labelled A/B (no engine jargon) so it reads
              ;; clearly for non-technical users.
              (dom/div
                (dom/props {:style {:display "flex" :gap "12px" :align-items "stretch"}})
                (dom/div
                  (dom/props {:style {:flex "1" :min-width "0" :display "flex" :flex-direction "column" :gap "8px"}})
                  (ResultPanel "Option A" a)
                  (PickButton "Use A" user-id root-topic-id page (:text a) "client" a-ok !remember !compare))
                (dom/div
                  (dom/props {:style {:flex "1" :min-width "0" :display "flex" :flex-direction "column" :gap "8px"}})
                  (ResultPanel "Option B" b)
                  (PickButton "Use B" user-id root-topic-id page (:text b) "remote" b-ok !remember !compare)))
              ;; Remember
              (dom/label
                (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
                (dom/input
                  (dom/props {:type "checkbox"})
                  (dom/On "change" (fn [e] (reset! !remember (-> e .-target .-checked))) nil))
                (dom/text "Remember for this PDF"))))

          ;; Close
          (dom/div
            (dom/props {:style {:display "flex" :justify-content "flex-end"}})
            (dom/button
              (dom/props {:class "btn btn-sm btn-secondary"})
              (dom/text "Close")
              (dom/On "click" (fn [_] (reset! !compare nil)) nil))))))))

;; ── Button ───────────────────────────────────────────────────────────────

(defn- kick-client!
  "Run the client PDF.js extraction (needs the loaded viewer) and stash the raw
   text into `!sink` keyed by id/page. Errors resolve to empty text (handled as
   no-text downstream)."
  [!sink id page]
  #?(:cljs (-> (pdfviewer/get-page-text-content! page)
             (.then (fn [t] (reset! !sink {:id id :page page :raw t})))
             (.catch (fn [_] (reset! !sink {:id id :page page :raw ""}))))
     :clj nil))

(e/defn CopyTextButton
  "Single native-extract button. `extract-style` is the per-PDF setting
   (\"client\"|\"remote\" run directly; \"ask\" or nil → compare modal)."
  [user-id root-topic-id page-number extract-style]
  (e/client
    (let [!remote-run (atom nil) remote-run (e/watch !remote-run)
          !client-save (atom nil) client-save (e/watch !client-save)
          !compare (atom nil) compare (e/watch !compare)
          busy? (or (some? remote-run) (some? client-save) (some? compare))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Copy text"
                    :data-tooltip "Copy the PDF's own text (no AI)"
                    :disabled busy?})
        (icons/Icon :clipboard :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text (if busy? "Copying…" "Copy text")))
        (dom/On "click"
          (fn [_]
            (let [pg page-number id (str (random-uuid)) style extract-style]
              (case style
                "remote" (reset! !remote-run {:id id :page pg})
                "client" (kick-client! !client-save id pg)
                (kick-client! !compare id pg))))
          nil))

      ;; Remote direct path: extract + save on the server.
      (when remote-run
        (let [[t _] (e/Token remote-run)]
          (when t
            (e/on-unmount #(reset! !remote-run nil))
            (case (e/server (e/Offload #(remote-extract-save!* user-id root-topic-id (:page remote-run))))
              (t)))))

      ;; Client direct path: save the client-extracted raw text.
      (when client-save
        (let [[t _] (e/Token client-save)]
          (when t
            (e/on-unmount #(reset! !client-save nil))
            (case (e/server (e/Offload #(client-save!* user-id root-topic-id (:page client-save) (:raw client-save))))
              (t)))))

      ;; Compare path (style unset): run both previews in a modal.
      (when compare
        (CompareModal user-id root-topic-id compare !compare)))))
