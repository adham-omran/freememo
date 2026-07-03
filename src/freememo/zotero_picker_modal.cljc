(ns freememo.zotero-picker-modal
  "Pick an item from the user's local Zotero library and import its PDF.

   Loads the entire user library once on open, filters client-side, and
   renders with virtual scroll. The server fetches CSL-JSON only at
   import time, so the bulk listing payload stays small.

   Stages:
     :loading                 — initial library fetch
     :browsing                — list visible; filter active
     :fetching-attachments    — server resolving PDFs for chosen item
     :choosing-attachment     — ≥2 PDFs; user disambiguates
     :importing               — staged bytes; committing
     :done                    — navigated away
     :error                   — terminal; retry returns to :loading"
  (:require
   [clojure.string :as str]
   [contrib.data :refer [clamp-left]]
   [freememo.modal-shell :as modal]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-scroll0 :refer [Scroll-window Tape]]
   [freememo.navigation :as nav]
   [freememo.zotero-client :as zc]))

(def ^:private row-height 52)

(def ^:private blocked-item-types
  "Zotero item_type values dropped from the picker. Standalone attachments
   and notes pass Zotero's onlyTopLevel filter (no parent), but the picker's
   import path requires a parent with PDF children. PDF annotations occupy
   the same non-bibliographic shape."
  #{"attachment" "note" "annotation"})

#?(:cljs
   (defn- bib-item?
     "Pre: item is a Zotero item map carrying :item-type or :item_type.
      Post: true iff item-type ∉ blocked-item-types."
     [item]
     (not (contains? blocked-item-types
            (or (:item-type item) (:item_type item))))))

#?(:cljs
   (defn- format-meta [item]
     ;; The plugin returns :creators_summary / :item_type with snake_case
     ;; keys; the legacy server endpoint returned :creators-summary etc.
     ;; Accept either to keep the helper resilient during the migration.
     (->> [(or (:creators-summary item) (:creators_summary item))
           (:year item)
           (or (:item-type item) (:item_type item))]
       (remove (fn [s] (or (nil? s) (= "" s))))
       (str/join " · "))))

#?(:cljs
   (defn- matches?
     "Case-insensitive substring search over the user-visible fields of a
      Zotero item. Pre: q is non-blank lowercase string. Post: boolean."
     [q-lower item]
     (let [hay (str/lower-case
                 (str (:title item) " "
                   (or (:creators-summary item) (:creators_summary item)) " "
                   (:year item) " "
                   (or (:item-type item) (:item_type item))))]
       (str/includes? hay q-lower))))

#?(:cljs
   (defn- filter-items
     "Pre: items is the full vector; q may be blank.
      Post: items when q is blank, else only items matching q (sub-string,
            case-insensitive) across title + creators + year + item-type.
      Invariant: order preserved."
     [items q]
     (let [q (some-> q str/trim)]
       (if (str/blank? q)
         items
         (let [ql (str/lower-case q)]
           (filterv #(matches? ql %) items))))))

#?(:cljs
   (defn- fetch-all-items!
     "Drain the user's Zotero library through the FreeMemo plugin
      (zc/list-all-items!), then drop non-bibliographic rows.
      On success call (on-ok bib-items raw-total) where bib-items is the
      block-list-filtered vector and raw-total is the count Zotero returned.
      On failure call (on-err msg)."
     [on-ok on-err]
     (-> (zc/list-all-items!)
       (.then (fn [result]
                (if (:ok? result)
                  (let [items (get-in result [:data :items] [])
                        bib-items (filterv bib-item? items)]
                    (on-ok bib-items (count items)))
                  (on-err (or (:error result)
                            "Failed to reach Zotero. Is the FreeMemo plugin installed and Zotero running?"))))))))

#?(:cljs
   (defn- post-form!
     "POST application/x-www-form-urlencoded to the FreeMemo server.
      Ring's form middleware reads this into :params under string keys."
     [url params on-data on-err]
     (let [fd (js/URLSearchParams.)]
       (doseq [[k v] params]
         (when (some? v) (.append fd (name k) (str v))))
       (-> (js/fetch url
             (clj->js {:method "POST" :body fd}))
         (.then (fn [resp] (.json resp)))
         (.then on-data)
         (.catch (fn [_err] (on-err "Failed to reach the server.")))))))

#?(:cljs
   (defn- post-multipart-stage!
     "POST PDF bytes + optional CSL-JSON to /api/zotero/stage as
      multipart/form-data. Pre: bytes is a Uint8Array; filename is a
      non-blank string; csljson is a Clojure map or nil. Post: invokes
      (on-data {:success :upload_id ...}) on success or (on-err msg)."
     [bytes filename csljson on-data on-err]
     (let [fd (js/FormData.)
           blob (js/Blob. #js [bytes] #js {:type "application/pdf"})]
       (.append fd "file" blob filename)
       (.append fd "filename" filename)
       (when csljson
         (.append fd "csljson" (js/JSON.stringify (clj->js csljson))))
       (-> (js/fetch "/api/zotero/stage"
             (clj->js {:method "POST" :body fd}))
         (.then (fn [resp] (.json resp)))
         (.then on-data)
         (.catch (fn [_err] (on-err "Failed to upload to FreeMemo.")))))))

#?(:cljs
   (defn- fetch-and-stage!
     "Fetch attachment bytes + CSL-JSON from the plugin in parallel, then
      POST to /api/zotero/stage. Pre: item-key is the parent's Zotero key,
      attachment-key is a PDF child's key. Post: on success calls
      (on-data {:success true :upload_id ...}); on any failure step calls
      (on-err msg)."
     [item-key attachment-key on-data on-err]
     (let [bytes-promise (zc/fetch-attachment-bytes! attachment-key)
           csl-promise (zc/get-csljson! item-key)]
       (-> (js/Promise.all #js [bytes-promise csl-promise])
         (.then (fn [results]
                  (let [bytes-result (aget results 0)
                        csl-result (aget results 1)]
                    (cond
                      (not (:ok? bytes-result))
                      (on-err (cond
                                (= 413 (:status bytes-result))
                                "PDF exceeds the per-file size cap."
                                (= 404 (:status bytes-result))
                                "Attachment file is missing from Zotero storage."
                                :else
                                (or (get-in bytes-result [:data :error])
                                  (:error bytes-result)
                                  "Failed to fetch the PDF from Zotero.")))

                      :else
                      (let [{:keys [filename bytes]} (:data bytes-result)
                            csljson (when (:ok? csl-result)
                                      (get-in csl-result [:data :csl]))]
                        (post-multipart-stage! bytes filename csljson on-data on-err))))))
         (.catch (fn [_err] (on-err "Unexpected error during import.")))))))

;; ── Sub-components ─────────────────────────────────────────────────

(e/defn SearchBar [!query]
  (e/client
    (let [q (e/watch !query)]
      (dom/input
        (dom/props {:type "text"
                    :placeholder "Filter your Zotero library…"
                    :value q
                    :class "input input-full"
                    :style {:padding "10px 12px" :margin-bottom "var(--sp-3)"}})
        (dom/On "input" (fn [e] (reset! !query (-> e .-target .-value))) nil)))))

(e/defn ItemCell [item i on-pick]
  (e/client
    (dom/td
      (dom/props {:style {:padding "6px 12px"
                          :cursor "pointer"
                          :display "flex" :flex-direction "column"
                          :justify-content "center"
                          :overflow "hidden"}})
      (dom/On "click" (fn [_] (on-pick item)) nil)
      (dom/div
        (dom/props {:style {:font-size "14px" :font-weight "500"
                            :color "var(--color-text-primary)"
                            :white-space "nowrap" :overflow "hidden"
                            :text-overflow "ellipsis"}})
        (dom/text (or (:title item) "(Untitled)")))
      (dom/div
        (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                            :white-space "nowrap" :overflow "hidden"
                            :text-overflow "ellipsis"
                            :margin-top "2px"}})
        (dom/text (format-meta item))))))

(e/defn VirtualItemList [filtered on-pick empty-message]
  (e/client
    (let [row-count (count filtered)]
      (dom/div
        (dom/props {:style {:flex "1" :min-height "0" :overflow-y "auto"
                            :border "1px solid var(--color-border)"
                            :border-radius "var(--radius-sm)"}})
        (let [[offset limit] (Scroll-window row-height row-count dom/node
                               {:overquery-factor 1})
              occluded (clamp-left (* row-height (- row-count limit)) 0)]
          (dom/props {:class "tape-scroll"
                      :style {:--offset offset :--row-height (str row-height "px")}})
          (dom/table
            (dom/props {:style {:width "100%" :display "grid"
                                :grid-template-columns "1fr"}})
            (if (pos? row-count)
              (e/for [i (Tape offset limit)]
                (let [item (nth filtered i nil)]
                  (when item
                    (dom/tr
                      (dom/props {:style {:--order (inc i)
                                          :height (str row-height "px")
                                          :border-bottom "1px solid var(--color-bg-subtle)"}})
                      (ItemCell item i on-pick)))))
              (dom/tr
                (dom/td
                  (dom/props {:style {:padding "24px 12px" :text-align "center"
                                      :color "var(--color-text-secondary)"
                                      :font-size "13px"}})
                  (dom/text empty-message)))))
          (dom/div (dom/props {:style {:height (str occluded "px")}})))))))

(e/defn AttachmentChooser [candidates on-pick on-cancel]
  (e/client
    (dom/div
      (dom/props {:style {:padding "12px 0"}})
      (dom/p
        (dom/props {:style {:margin "0 0 12px 0" :font-size "13px"
                            :color "var(--color-text-secondary)"}})
        (dom/text "This item has multiple PDFs. Pick one to import."))
      (dom/div
        (dom/props {:style {:display "flex" :flex-direction "column" :gap "6px"
                            :margin-bottom "var(--sp-3)"}})
        (e/for [c (e/diff-by :key candidates)]
          (dom/button
            (dom/props {:class "btn btn-secondary"
                        :style {:text-align "left" :padding "8px 12px"}})
            (dom/text (:filename c))
            (dom/On "click" (fn [_] (on-pick c)) nil))))
      (dom/div
        (dom/props {:style {:display "flex" :justify-content "flex-end"}})
        (dom/button
          (dom/props {:class "btn btn-secondary"})
          (dom/text "Cancel")
          (dom/On "click" (fn [_] (on-cancel)) nil))))))

(e/defn StatusStage [message]
  (e/client
    (dom/div
      (dom/props {:style {:padding "24px" :text-align "center"
                          :color "var(--color-text-secondary)"}})
      (dom/text message))))

(e/defn ErrorStage [!error on-retry on-close]
  (e/client
    (let [msg (e/watch !error)]
      (dom/div
        (dom/props {:style {:padding "10px 12px" :margin-bottom "var(--sp-3)"
                            :background "var(--color-danger-bg, #fee)"
                            :border-radius "var(--radius-sm)"
                            :color "var(--color-danger-text)"
                            :font-size "13px"}})
        (dom/text (or msg "Something went wrong.")))
      (dom/div
        (dom/props {:style {:display "flex" :gap "var(--sp-2)" :justify-content "flex-end"}})
        (dom/button (dom/props {:class "btn btn-secondary"})
          (dom/text "Close")
          (dom/On "click" (fn [_] (on-close)) nil))
        (dom/button (dom/props {:class "btn btn-primary"})
          (dom/text "Try again")
          (dom/On "click" (fn [_] (on-retry)) nil))))))

;; ── Top-level ZoteroPickerModal ────────────────────────────────────

(e/defn ZoteroPickerModal [!show user-id navigate!]
  (e/client
    (let [!stage (atom :loading)
          !all-items (atom [])
          !raw-total (atom 0)
          !query (atom "")
          !error (atom nil)
          !candidates (atom nil)
          !pending-item (atom nil)
          stage (e/watch !stage)
          query (e/watch !query)
          all-items (e/watch !all-items)
          raw-total (e/watch !raw-total)
          candidates (e/watch !candidates)
          filtered (filter-items all-items query)
          total (count all-items)
          shown (count filtered)

          close-modal! (fn [] (reset! !show false))

          navigate-to-viewer!
          (fn [doc-id]
            (reset! !stage :done)
            (reset! !show false)
            (navigate! :viewer (nav/nav-topic doc-id nil)))

          on-error!
          (fn [msg]
            (reset! !error msg)
            (reset! !stage :error))

          load-all!
          (fn []
            (reset! !stage :loading)
            (reset! !error nil)
            (fetch-all-items!
              (fn [items raw]
                (reset! !all-items (vec items))
                (reset! !raw-total raw)
                (reset! !stage :browsing))
              on-error!))

          commit-staged!
          (fn [upload-id]
            (post-form! "/api/upload-staged" {"upload_id" upload-id}
              (fn [^js data]
                (cond
                  (.-doc_id data) (navigate-to-viewer! (.-doc_id data))
                  (.-success data) (on-error! "Unexpected response from server.")
                  :else (on-error! (or (.-error data) "Import failed."))))
              on-error!))

          handle-stage-response
          (fn [^js data]
            (cond
              (and (.-success data) (.-upload_id data))
              (commit-staged! (.-upload_id data))

              (.-error data) (on-error! (.-error data))

              :else (on-error! "Unexpected response from server.")))

          pick-item!
          (fn [item]
            (reset! !stage :fetching-attachments)
            (reset! !error nil)
            (-> (zc/list-pdf-attachments! (:key item))
              (.then
                (fn [result]
                  (cond
                    (not (:ok? result))
                    (on-error! (or (:error result)
                                 "Failed to list attachments for this item."))

                    :else
                    (let [pdfs (get-in result [:data :pdfs] [])]
                      (cond
                        (empty? pdfs)
                        (on-error! "No PDF attached to this Zotero item.")

                        (= 1 (count pdfs))
                        (do (reset! !stage :importing)
                          (fetch-and-stage! (:key item) (:key (first pdfs))
                            handle-stage-response on-error!))

                        :else
                        (do (reset! !candidates pdfs)
                          (reset! !pending-item item)
                          (reset! !stage :choosing-attachment)))))))))

          pick-attachment!
          (fn [chosen]
            (let [item @!pending-item]
              (reset! !stage :importing)
              (fetch-and-stage! (:key item) (:key chosen)
                handle-stage-response on-error!)))

          cancel-chooser!
          (fn []
            (reset! !candidates nil)
            (reset! !pending-item nil)
            (reset! !stage :browsing))

          retry! (fn [] (load-all!))]

      ;; Single fetch on mount. e/Token on a constant key fires exactly
      ;; once; we release the token after kicking off the fetch.
      (let [[t _] (e/Token :zotero-picker-mount)]
        (when t
          (load-all!)
          (t)))

      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1" :autofocus true})
        (modal/ModalEscape close-modal! "Zotero picker")
        (dom/On "click"
          (fn [e] (when (= (.-target e) (.-currentTarget e)) (close-modal!)))
          nil)
        (dom/div
          (dom/props {:class "modal-content"
                      :style {:width "640px" :max-width "92%"
                              :height "75vh"
                              :display "flex" :flex-direction "column"}})
          (dom/h3
            (dom/props {:style {:margin "0 0 4px 0" :font-size "16px"}})
            (dom/text "Import from Zotero"))
          (dom/div
            (dom/props {:style {:font-size "12px" :color "var(--color-text-secondary)"
                                :margin-bottom "var(--sp-3)"}})
            (dom/text
              (str "Library — " total " item" (when (not= total 1) "s")
                (when (and (pos? raw-total) (not= raw-total total))
                  (str " (filtered from " raw-total ")"))
                (when (and (seq query) (not= shown total))
                  (str " (" shown " shown)")))))

          (case stage
            :loading (StatusStage "Loading your Zotero library…")
            :fetching-attachments (StatusStage "Finding the PDF…")
            :importing (StatusStage "Importing…")
            :choosing-attachment (AttachmentChooser (or candidates [])
                                   pick-attachment! cancel-chooser!)
            :error (ErrorStage !error retry! close-modal!)
            :done nil
            ;; :browsing (default)
            (do
              (SearchBar !query)
              (VirtualItemList filtered pick-item!
                (if (zero? total)
                  "No bibliographic items in your Zotero library."
                  "No items match your filter.")))))))))
