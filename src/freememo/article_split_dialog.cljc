(ns freememo.article-split-dialog
  "Preview stored article sections and create a selected batch of topics."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.forms-ext :as fields]
   [freememo.modal-shell :as modal]
   [freememo.rich-text-editor :as editor]
   [freememo.tooltip :as tooltip]
   #?(:clj [freememo.article-split :as article-split])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.undo :as undo])))

(def boundary-modes
  [["headings" "Headings"]
   ["lines" "Horizontal lines"]
   ["splitmark" "Custom splitmark"]
   ["blocks" "Groups of four blocks"]])

(defn initial-state []
  {:options {:mode :headings :splitmark ""}
   :highlight-source? false
   :preview nil :selected #{} :result nil :error nil :busy? false})

(defn preview* [user-id topic-id options]
  #?(:clj (article-split/preview user-id topic-id options) :cljs nil))

(defn create-topics!* [user-id topic-id request]
  #?(:clj (article-split/create-topics! user-id topic-id request) :cljs nil))

(defn undo-topics!* [user-id undo-id]
  #?(:clj (undo/undo-entry! user-id undo-id) :cljs nil))

(defn section-depth
  "Pre: sections use preceding parent indices. Post: return this section's depth."
  [sections index]
  (loop [parent (:parent-index (nth sections index nil)) depth 0]
    (if (and (integer? parent) (<= 0 parent) (< parent index))
      (recur (:parent-index (nth sections parent nil)) (inc depth))
      depth)))

(defn preview-document
  "Pre: html is sanitized. Post: return an isolated, readable preview document."
  [html]
  (str "<!doctype html><html><head><meta charset='utf-8'>"
    "<meta name='color-scheme' content='light dark'>"
    "<style>body{font:14px/1.6 system-ui,sans-serif;margin:12px;overflow-wrap:anywhere}"
    "img,video{max-width:100%;height:auto}pre{white-space:pre-wrap}"
    "table{max-width:100%}</style></head><body>" html "</body></html>"))

(defn change-options
  "Post: clear the preview when a boundary option changes. Preserve no stale selection."
  [state edit]
  (let [options (cond-> (:options state)
                  (contains? edit :mode) (assoc :mode (keyword (:mode edit)))
                  (contains? edit :splitmark) (assoc :splitmark (:splitmark edit)))]
    (assoc state :options options :preview nil :selected #{} :result nil :error nil)))

(e/defn SplitRules [state]
  (dom/div
    (dom/props {:class "article-split__rules"})
    (e/amb
      (dom/label
        (dom/props {:class "article-split__field"})
        (e/amb
          (do (dom/span (dom/text "Split at")) (e/amb))
          (fields/Select! :mode (name (get-in state [:options :mode])) boundary-modes
            :class "input" :aria-label "Split boundary")))
      (dom/label
        (dom/props {:class "article-split__field"
                    :hidden (not= :splitmark (get-in state [:options :mode]))})
        (e/amb
          (do (dom/span (dom/text "Splitmark")) (e/amb))
          (forms/Input! :splitmark (get-in state [:options :splitmark])
            :class "input" :aria-label "Splitmark"
            :placeholder "A whole paragraph, such as ***")))
      (dom/label
        (dom/props {:class "article-split__selection"})
        (e/amb
          (forms/Checkbox! :highlight-source? (:highlight-source? state)
            :aria-label "Highlight created sections in source"
            :disabled (false? (get-in state [:preview :can-highlight?])))
          (do (dom/span (dom/text "Highlight created sections in source")) (e/amb))))
      (forms/Button! [::preview]
        :class "btn btn-secondary" :label "Preview sections"))))

(e/defn SplitSection [sections section selected? close!]
  (let [{:keys [index title content]} section]
    (dom/article
      (dom/props {:class "article-split__section"
                  :style {:--section-depth (min 6 (section-depth sections index))}})
      (e/amb
        (dom/label
          (dom/props {:class "article-split__selection"})
          (e/amb
            (forms/Checkbox! index selected? :aria-label (str "Include " title))
            (do (dom/span (dom/props {:dir "auto"}) (dom/text title)) (e/amb))))
        (do
          (dom/details
            (dom/summary (dom/text "Inspect section"))
            (dom/iframe
              ;; Media requires session cookies. Keep scripts disabled.
              (dom/props {:class "article-split__content"
                          :sandbox "allow-same-origin" :srcdoc (preview-document content)})
              (tooltip/Tooltip! (str "Preview: " title) :aria? true)
              (modal/IframeEscape close!)))
          (e/amb))))))

(e/defn SplitSections [state close!]
  (let [sections (get-in state [:preview :sections] [])]
    (dom/div
      (dom/props {:class "article-split__sections"})
      (e/for [section (e/diff-by :index sections)]
        (SplitSection sections section (contains? (:selected state) (:index section)) close!)))))

(e/defn SplitActions [state]
  (dom/div
    (dom/props {:class "article-split__actions"})
    (e/amb
      (forms/Button! [::create] :class "btn btn-primary" :label "Create topics")
      (forms/Button! [::undo] :class "btn btn-secondary" :label "Undo created topics"
        :hidden (nil? (get-in state [:result :undo-id])))
      (forms/Button! [::close] :class "btn btn-secondary"
        :label (if (:result state) "Close" "Cancel")))))

(e/defn PreviewSections [token user-id topic-id !state snapshot]
  (if (and (= :splitmark (get-in snapshot [:options :mode]))
        (str/blank? (get-in snapshot [:options :splitmark])))
    (case (swap! !state assoc :error "Enter a splitmark paragraph.")
      (token "Enter a splitmark paragraph."))
    (case (swap! !state assoc :busy? true :error nil)
      (let [result (e/server (e/Offload #(preview* user-id topic-id (:options snapshot))))]
        (e/on-unmount #(swap! !state assoc :busy? false))
        (if (:success result)
          (case (swap! !state assoc :preview result :result nil
                  :selected (set (map :index (:sections result)))
                  :highlight-source? (and (:highlight-source? snapshot)
                                       (not (false? (:can-highlight? result)))))
            (token))
          (case (swap! !state assoc :error (:error result))
            (token (:error result))))))))

(e/defn CreateSections [token user-id topic-id !state snapshot]
  (let [preview (:preview snapshot)
        selected (:selected snapshot)
        error (cond
                (:result snapshot) "Preview the article again before creating more topics."
                (nil? preview) "Preview the article before creating topics."
                (empty? selected) "Select at least one section.")]
    (if error
      (case (swap! !state assoc :error error) (token error))
      (case (swap! !state assoc :busy? true :error nil)
        (let [request {:source-version (:source-version preview)
                       :options (:options snapshot) :selected-indices (vec (sort selected))
                       :highlight-source? (boolean (:highlight-source? snapshot))}
              result (e/server (e/Offload #(create-topics!* user-id topic-id request)))]
          (e/on-unmount #(swap! !state assoc :busy? false))
          (if (:success result)
            (case (swap! !state assoc :result result) (token))
            (case (swap! !state
                    (fn [state]
                      (cond-> (assoc state :error (:error result))
                        (:stale? result) (assoc :preview nil :selected #{}))))
              (token (:error result)))))))))

(e/defn UndoSections
  "Pre: snapshot names this batch's undo entry.
   Post: clear the result only after undo succeeds. Show failures in the dialog.
   Invariant: a failed undo retains the batch's undo entry."
  [token user-id !state snapshot]
  (if-let [undo-id (get-in snapshot [:result :undo-id])]
    (case (swap! !state assoc :busy? true :error nil)
      (let [result (e/server (e/Offload #(undo-topics!* user-id undo-id)))]
        (e/on-unmount #(swap! !state assoc :busy? false))
        (if (:success result)
          (case (reset! !state (assoc (initial-state) :options (:options snapshot)
                                :highlight-source? (:highlight-source? snapshot)))
            (token))
          (case (swap! !state assoc :error (:error result))
            (token (:error result))))))
    (token)))

(e/defn SplitCommand [token command user-id topic-id !state !open]
  (let [snapshot (e/snapshot (deref !state))]
    (cond
      (map? command)
      (case (swap! !state
              (fn [state]
                (cond
                  (contains? command :highlight-source?)
                  (assoc state :highlight-source? (boolean (:highlight-source? command)))

                  (or (contains? command :mode) (contains? command :splitmark))
                  (change-options state command)

                  :else
                  (update state :selected
                    (fn [selected]
                      (reduce-kv (fn [indices index include?]
                                   ((if include? conj disj) indices index)) selected command))))))
        (token))

      (= [::preview] command) (PreviewSections token user-id topic-id !state snapshot)
      (= [::create] command) (CreateSections token user-id topic-id !state snapshot)

      (= [::undo] command) (UndoSections token user-id !state snapshot)

      (= [::close] command)
      (do (e/on-unmount #(reset! !open false)) (token)))))

(e/defn SplitDialog [user-id topic-id !open]
  (let [!state (atom (initial-state))
        state (e/watch !state)
        close! (fn [] (when-not (:busy? @!state) (reset! !open false)))]
    (dom/div
      (dom/props {:class "modal-backdrop"})
      (modal/ModalEscape close! "Split article")
      (dom/div
        (dom/props {:class "modal-content article-split"})
        (dom/h3 (dom/text "Split article"))
        (dom/p
          (dom/props {:class "hint"})
          (dom/text
            (if (:highlight-source? state)
              "Preview saved content. Newly created sections receive blue highlights in the source article."
              "Preview saved content. The source article stays unchanged unless you enable highlighting.")))
        (when (= :splitmark (get-in state [:options :mode]))
          (dom/p (dom/props {:class "hint"})
            (dom/text "Use a whole paragraph. Matching is case-sensitive. Surrounding whitespace is ignored.")))
        (when (false? (get-in state [:preview :can-highlight?]))
          (dom/p (dom/props {:class "hint"})
            (dom/text "Source highlighting is unavailable for this topic type.")))
        (when (:error state)
          (dom/p (dom/props {:class "article-split__error" :role "alert"})
            (dom/text (:error state))))
        (when (:busy? state)
          (dom/p (dom/props {:role "status"}) (dom/text "Please wait…")))
        (when-let [result (:result state)]
          (dom/p (dom/props {:role "status"})
            (dom/text (str "Created " (:created result) " topics. Skipped " (:skipped result)
                       " sections already created."))))
        (when (:preview state)
          (dom/p
            (dom/text (str (count (:selected state)) " of "
                       (count (get-in state [:preview :sections])) " sections selected."))))
        (dom/fieldset
          (dom/props {:class "article-split__fieldset" :disabled (:busy? state)})
          (let [commands (e/amb (SplitRules state) (SplitSections state close!) (SplitActions state))]
            (e/for [[token command] (e/diff-by first (e/as-vec commands))]
              (SplitCommand token command user-id topic-id !state !open))))))))

(e/defn ArticleSplitOwner
  "Pre: mount once per topic. Post: publish a viewer command and own its dialog."
  [user-id topic-id]
  (e/client
    (let [!open (atom false)
          source-update (e/server (e/watch (us/get-atom user-id :article-source-update)))]
      (e/for [update (e/diff-by :event-id (if source-update [source-update] []))]
        (editor/install-saved-content! (:topic-id update)
          (:before-content update) (:content update)))
      (bus/publish-invoker! :split-article #(reset! !open true))
      (e/on-unmount #(bus/retract-invoker! :split-article))
      (when (e/watch !open)
        (SplitDialog user-id topic-id !open)))))

(e/defn SplitArticleButton
  "Post: open the same dialog as the palette command. This button writes no topics."
  []
  (e/client
    (e/for [[token _] (forms/Button! [::open]
                      :class "btn btn-sm btn-secondary" :label "Split article…")]
      (case (bus/dispatch! :split-article) (token)))))
