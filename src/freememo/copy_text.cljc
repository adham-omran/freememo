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
   [taoensso.telemere :as tel]
   [freememo.icons :as icons]
   [freememo.modal-shell :as modal]
   [freememo.commands :as commands]
   [freememo.command-bus :as bus]
   [freememo.loading :as loading]
   [freememo.pdf-viewer :as pdfviewer]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.page-ocr :as page])
   #?(:clj [freememo.settings :as settings])
   #?(:clj [freememo.toasts :as toasts])))

;; ── Server bridges (plain defns; CLJS gets a no-op) ────────────────────────

(defn remote-extract-save!* [user-id root page]
  #?(:clj (let [r (page/extract-page-text-pdfbox root page)]
            (when (:success r) (commands/bump! user-id :copy-text))
            r)
     :cljs nil))

(defn client-save!* [user-id root page raw]
  #?(:clj (let [r (page/save-pdfjs-text! root page raw)]
            (when (:success r) (commands/bump! user-id :copy-text))
            r)
     :cljs nil))

(defn preview-remote* [root page]
  #?(:clj (page/preview-pdfbox-html root page) :cljs nil))

(defn preview-client* [raw]
  #?(:clj (page/preview-pdfjs-html raw) :cljs nil))

(defn commit-html!* [user-id root page html]
  #?(:clj (let [r (page/save-page-html-impl root page html)]
            (when (:success r) (commands/bump! user-id :copy-text))
            r)
     :cljs nil))

(defn get-extract-style*
  "Reactive read of the per-PDF style; `_refresh` (a :settings-refresh value)
   forces re-read after a save."
  [_refresh user-id root]
  #?(:clj (settings/get-extract-style user-id root) :cljs nil))

(defn save-style!* [user-id root style]
  #?(:clj (let [r (settings/save-extract-style user-id root style)]
            (commands/bump! user-id :set-setting)
            r)
     :cljs nil))

(defn push-toast!*
  "Push a toast to the user's queue. Errors auto-sticky (toasts/push! default)."
  [user-id level message]
  #?(:clj (do (toasts/push! user-id {:level level :message message}) nil)
     :cljs nil))

(defn- summarize [results]
  (let [ok (count (filter :success results))
        failed (count (remove :success results))]
    {:ok ok :failed failed}))

(defn remote-extract-all!*
  "Extract+saves text via PDFBox for every page under `root`. Overwrites
   existing page text. Bumps :refresh once. Returns {:ok n :failed m}."
  [user-id root]
  #?(:clj (let [pages (db/list-pages root)
                results (doall
                          (for [p pages
                                :let [pn (:topics/page_number p)]]
                            (page/extract-page-text-pdfbox root pn)))]
            (commands/bump! user-id :copy-all)
            (summarize results))
     :cljs nil))

(defn client-save-all!*
  "Save client-extracted raw text for a batch of pages. `page->raw` maps
   page-number (Long) -> raw String. Overwrites the existing page row under
   `root` for each page-number that exists under `root` (save-page-text!
   keys on (root, page-number)); unknown page-numbers are skipped. Bumps
   :refresh once. Returns {:ok n :failed m}."
  [user-id root page->raw]
  #?(:clj (let [valid (into #{} (keep :topics/page_number) (db/list-pages root))
                results (doall
                          (for [[pn raw] page->raw
                                :when (contains? valid pn)]
                            (page/save-pdfjs-text! root pn raw)))]
            (commands/bump! user-id :copy-all)
            (summarize results))
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
   style. Closes the modal on success; on failure toasts the error and leaves
   the modal open so the user isn't left thinking the pick was saved."
  [label user-id root-topic-id page html style enabled? !remember !compare]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm btn-primary" :disabled (not enabled?)
                  :style {:width "100%"}})
      (dom/text label)
      (let [click (dom/On "click" (fn [_] @!remember) nil)
            [t _] (e/Token click)]
        (when t
          (let [commit-r (e/server (e/Offload #(commit-html!* user-id root-topic-id page html)))]
            (case commit-r
              (if (:success commit-r)
                (let [style-r (if click
                                (e/server (save-style!* user-id root-topic-id style))
                                {:success true})]
                  (case style-r
                    (if (:success style-r)
                      (do (e/on-unmount #(reset! !compare nil)) (t))
                      (case (e/server (e/Offload #(push-toast!* user-id :warning
                                                    (str "Saved the text, but couldn't remember the style: " (:error style-r)))))
                        (do (e/on-unmount #(reset! !compare nil)) (t))))))
                (case (e/server (e/Offload #(push-toast!* user-id :error
                                              (str "Couldn't save text: " (:error commit-r)))))
                  (t (:error commit-r)))))))))))

(e/defn CompareModal
  "Runs both previews on mount, shows them side-by-side, commits the picked text
   to the current page, and (if Remember is checked) saves the per-PDF style."
  [user-id root-topic-id page raw !compare]
  (e/client
    (let [!remember (atom false)]
      (dom/div
        (dom/props {:class "modal-backdrop"})
        (dom/On "click" (fn [_] (reset! !compare nil)) nil)
        (modal/ModalEscape (fn [] (reset! !compare nil)) "Copy text")
        (dom/div
          (dom/props {:class "modal-content"
                      :style {:width "min(880px, 95vw)" :max-height "85vh"
                              :display "flex" :flex-direction "column" :gap "12px" :padding "16px 20px"}})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/props {:style {:margin "0" :font-size "16px" :font-weight "500"}})
            (dom/text (str "Copy text — page " page)))

          ;; Resolve both previews before rendering — the both-fail hint needs
          ;; both results, and a pending e/server value is empty-amb (not nil),
          ;; which would otherwise blank the body. WithLoading shows the spinner.
          (loading/WithLoading
            (e/fn [] (e/server (preview-client* raw)))
            (e/fn [a]
              (loading/WithLoading
                (e/fn [] (e/server (e/Offload #(preview-remote* root-topic-id page))))
                (e/fn [b]
                  (let [a-ok (boolean (:success a))
                        b-ok (boolean (:success b))]
                    (if (and (not a-ok) (not b-ok))
                      (dom/div
                        (dom/props {:style {:padding "20px 4px" :color "var(--color-text-secondary)" :font-size "13px"}})
                        (dom/text "No extractable text in this PDF — use Scan Page."))
                      (dom/div
                        (dom/props {:style {:display "flex" :flex-direction "column" :gap "12px"}})
                        ;; Two options side-by-side: each preview with its own pick
                        ;; button beneath. Labelled A/B (no engine jargon).
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
                        (dom/label
                          (dom/props {:style {:display "flex" :align-items "center" :gap "6px" :font-size "13px"}})
                          (dom/input
                            (dom/props {:type "checkbox"})
                            (dom/On "change" (fn [e] (reset! !remember (-> e .-target .-checked))) nil))
                          (dom/text "Remember for this PDF")))))))))

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
                    :disabled busy?})
        (tooltip/Tooltip! "Copy the PDF's own text (no AI)")
        (icons/Icon :clipboard :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text (if busy? "Copying…" "Copy text")))
        (let [node dom/node]
          (bus/publish-invoker! :copy-text (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :copy-text))))
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
            (let [r (e/server (e/Offload #(remote-extract-save!* user-id root-topic-id (:page remote-run))))]
              (case r
                (if (:success r)
                  (t)
                  (case (e/server (e/Offload #(push-toast!* user-id :error (str "Couldn't copy text: " (:error r)))))
                    (t (:error r)))))))))

      ;; Client direct path: save the client-extracted raw text.
      (when client-save
        (let [[t _] (e/Token client-save)]
          (when t
            (e/on-unmount #(reset! !client-save nil))
            (let [r (e/server (e/Offload #(client-save!* user-id root-topic-id (:page client-save) (:raw client-save))))]
              (case r
                (if (:success r)
                  (t)
                  (case (e/server (e/Offload #(push-toast!* user-id :error (str "Couldn't save text: " (:error r)))))
                    (t (:error r)))))))))

      ;; Compare path (style unset): run both previews in a modal.
      (when compare
        (CompareModal user-id root-topic-id (:page compare) (:raw compare) !compare)))))

;; ── Copy-all button ───────────────────────────────────────────────────────

(def ^:private client-batch-limit 50)

(defn- kick-client-all!
  "Extract text for EVERY page (1..numPages) client-side via PDF.js and stash
   the {page-number -> raw-text} map into `!sink`. Pages are processed in
   sequential chunks of `client-batch-limit`, so no more than that many PDF.js
   fetches are in flight at once. A failed chunk resolves its pages to \"\" and
   the run continues. `id` is a correlation key. No-op on the server."
  [!sink id]
  #?(:cljs (let [^js doc @pdfviewer/!pdf-doc]
             (if-not doc
               (do (tel/log! {:level :debug :id ::copy-all-no-doc} "no doc loaded; nothing to extract")
                   (reset! !sink {:id id :results {}}))
               (let [num-pages (.-numPages doc)
                     chunks (vec (partition-all client-batch-limit (range 1 (inc num-pages))))
                     nchunks (count chunks)]
                 ;; DEBUG (hung-vs-slow hunt): logs each chunk as it begins and on
                 ;; completion. Stall at "chunk k" = a getTextContent never settling;
                 ;; reaching "extraction complete" while the button stays "Copying…"
                 ;; = the stall is server-side (client-save-all!*). Remove once confirmed.
                 (tel/log! {:level :debug :id ::copy-all-start :data {:pages num-pages :chunks nchunks}} "start")
                 (-> (reduce
                       (fn [p [i chunk]]
                         (.then p
                           (fn [acc]
                             (tel/log! {:level :debug :id ::copy-all-chunk :data {:chunk (inc i) :of nchunks :from (first chunk) :to (last chunk)}} "chunk")
                             (-> (.all js/Promise
                                   (mapv #(pdfviewer/get-page-text-content! %) chunk))
                               (.then (fn [texts] (merge acc (zipmap chunk texts))))
                               (.catch (fn [_] (merge acc (zipmap chunk (repeat "")))))))))
                       (.resolve js/Promise {})
                       (map-indexed vector chunks))
                   (.then (fn [results]
                            (tel/log! {:level :debug :id ::copy-all-done :data {:pages (count results)}} "extraction complete; handing to server save")
                            (reset! !sink {:id id :results results})))))))
     :clj nil))

(e/defn CopyAllTextButton
  "Copy the PDF's own text for ALL pages. Dispatches on `extract-style`
   (the per-PDF setting):
     \"remote\" → extract+saves every page server-side via PDFBox.
     \"client\" → extract every page client-side via PDF.js, in sequential
                 chunks of `client-batch-limit`.
     \"ask\"/nil → toasts a warning asking the user to set a default method
                  first (via Copy text's compare modal or Document options)."
  [user-id root-topic-id extract-style]
  (e/client
    (let [!remote-run (atom nil) remote-run (e/watch !remote-run)
          !client-run (atom nil) client-run (e/watch !client-run)
          !client-all (atom nil) client-all (e/watch !client-all)
          !warned (atom nil) warned (e/watch !warned)
          busy? (or (some? remote-run) (some? client-run) (some? client-all))]
      (dom/button
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Copy text for all pages"
                    :disabled busy?})
        (tooltip/Tooltip! "Copy the PDF's own text for all pages (no AI)")
        (icons/Icon :library :size 16)
        (dom/span (dom/props {:class "icon-label"})
          (dom/text (if busy? "Copying…" "Copy all text")))
        (let [node dom/node]
          (bus/publish-invoker! :copy-all (fn [] (.click node)))
          (e/on-unmount (fn [] (bus/retract-invoker! :copy-all))))
        (dom/On "click"
          (fn [_]
            (let [id (str (random-uuid)) style extract-style]
              (case style
                "remote" (reset! !remote-run {:id id})
                "client" (do
                           (reset! !client-run {:id id})
                           (kick-client-all! !client-all id))
                (reset! !warned {:id id}))))
          nil))

      ;; No preferred method → warn the user once per click.
      (when warned
        (let [[t _] (e/Token warned)]
          (when t
            (e/on-unmount #(reset! !warned nil))
            (case (e/server
                    (e/Offload
                      (fn []
                        (push-toast!*
                          user-id :warning
                          (str "Set a default text-extraction method first — "
                            "click 'Copy text' on a single page and pick one "
                            "(or choose one in Document options).")))))
              (t)))))

      ;; Remote batch path: extract+saves every page on the server, then toast.
      (when remote-run
        (let [[t _] (e/Token remote-run)]
          (when t
            (e/on-unmount #(reset! !remote-run nil))
            (let [summary (e/server (e/Offload #(remote-extract-all!* user-id root-topic-id)))]
              (case (e/server
                      (e/Offload
                        (fn []
                          (push-toast!*
                            user-id
                            (if (pos? (:failed summary)) :warning :success)
                            (str "Copied text for " (:ok summary) " page(s)"
                              (when (pos? (:failed summary))
                                (str " (" (:failed summary) " failed)"))
                              ".")))))
                (t))))))

      ;; Client batch path: save the client-extracted raw text per page, then toast.
      (when client-all
        (let [[t _] (e/Token client-all)]
          (when t
            (e/on-unmount #(do (reset! !client-run nil) (reset! !client-all nil)))
            (let [results (:results client-all)
                  summary (e/server (e/Offload #(client-save-all!* user-id root-topic-id results)))]
              (case (e/server
                      (e/Offload
                        (fn []
                          (push-toast!*
                            user-id
                            (if (pos? (:failed summary)) :warning :success)
                            (str "Copied text for " (:ok summary) " page(s) (client)"
                              (when (pos? (:failed summary))
                                (str " (" (:failed summary) " failed)"))
                              ".")))))
                (t)))))))))
