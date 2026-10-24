(ns freememo.extract-invoker
  "The :extract command's owner: creates a child topic from the reader's
   selection. It renders no control. The selection popover, the compact
   toolbar pair, the palette and ⌘E all reach it through the command bus
   (plans/toolbar-card-actions-fold.md §2)."
  (:require
   [hyperfiddle.electric3 :as e]
   [clojure.string :as str]
   [freememo.selected-passage :as selected-passage]
   [freememo.content-toolbar-helpers :as helpers]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.logging :as log]
   #?(:clj [freememo.toasts :as toasts])))

(defn create-extract!*
  "Save `html` as a child topic of `topic-id`.
   Pre : `html` is a selection from `selected-passage/current`; `code-pane?`
         says it came from a read-only surface rather than the editor.
   Post: as `helpers/create-extract-topic-safe!`. A failure is also toasted,
         because no control is on screen to show it."
  [topic-id user-id html title code-pane?]
  #?(:clj (let [r (if code-pane?
                    (helpers/create-code-pane-extract! topic-id user-id html title)
                    (helpers/create-extract-topic-safe! topic-id user-id html title))]
            (when-not (:success r)
              (toasts/push! user-id {:level :error
                                     :message (str "Couldn't save the extract: "
                                                (or (:error r) "unknown error"))}))
            r)
     :cljs nil))

(defn- extract-title
  "The first 80 characters of the selection's text, else \"Extract\"."
  [html]
  (let [raw (str/replace (or html "") #"<[^>]+>" "")]
    (if (str/blank? raw) "Extract" (subs raw 0 (min 80 (count raw))))))

(defn- take-selection!
  "Read and mark the reader's selection.
   Post: nil when no surface holds one. Otherwise a fresh request map, so two
         extracts of the same text are two requests."
  []
  #?(:clj nil
     :cljs (when-let [{:keys [html surface] :as passage} (selected-passage/current)]
             (selected-passage/mark! passage)
             (selected-passage/flash! passage)
             {:request-id (str (random-uuid))
              :html html
              :code-pane? (not= :editor surface)})))

(e/defn ExtractInvoker
  "Pre : mounted once per viewed topic, by CardActionsHost.
   Post: while mounted, :extract is available. Each invocation with a
         selection saves one extract, then bumps the :extract views.
   Inv : the request atom is local, so a token fires only on an invocation."
  [user-id topic-id]
  (e/client
    (let [!request (atom nil)
          request (e/watch !request)
          [t _] (e/Token request)]
      (bus/publish-invoker! :extract
        (fn [] (when-let [r (take-selection!)] (reset! !request r))))
      (e/on-unmount (fn [] (bus/retract-invoker! :extract)))
      (when t
        (let [{:keys [html code-pane?]} request
              title (extract-title html)
              result (e/server (e/Offload #(create-extract!* topic-id user-id html title code-pane?)))]
          ;; e/Offload answers nil while the save runs.
          (when (some? result)
            (if (:success result)
              (case (e/server (commands/bump! user-id :extract))
                (t))
              (do (log/log-debug (str "[EXTRACT-ERROR] " (pr-str (:error result))))
                  (t (or (:error result) "Failed to save extract"))))))))))
