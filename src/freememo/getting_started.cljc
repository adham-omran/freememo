(ns freememo.getting-started
  "The getting-started checklist on Home (plans/getting-started-checklist.md
   §5, §7). Progress is read from existing data (S4). The only stored state is
   the dismissed flag in `settings`.

   Home's summary query carries this namespace's input, so the checklist has
   no loading state of its own and reads fresh data on every Home mount (2.7).
   `freememo.getting-started-steps` decides what shows. This namespace draws
   it and runs each \"Show me\"."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.command-bus :as bus]
   [freememo.commands :as commands]
   [freememo.icons :as icons]
   [freememo.navigation :as nav]
   [freememo.settings-tab :as settings-tab]
   [freememo.spotlight :as spotlight]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.settings :as settings])))

;; ── Server ─────────────────────────────────────────────────────────────────

#?(:clj
   (defn checklist-input
     "The input `getting-started-steps/checklist` needs, plus what each
      \"Show me\" opens. Skips both queries once the checklist is dismissed.
      Pre : `ai-configured?` is the operator key's status.
      Post: {:dismissed? :ai-configured? :llm-enabled? :progress :targets}."
     [user-id ai-configured?]
     (if (settings/get-getting-started-dismissed user-id)
       {:dismissed? true}
       {:dismissed? false
        :ai-configured? (boolean ai-configured?)
        :llm-enabled? (settings/get-llm-enabled user-id)
        :progress (db/get-getting-started-progress user-id)
        :targets (db/get-getting-started-targets user-id)})))

(defn dismiss!* [user-id]
  #?(:clj (settings/dismiss-getting-started! user-id)
     :cljs nil))

(defn reopen!* [user-id]
  #?(:clj (settings/reopen-getting-started! user-id)
     :cljs nil))

;; ── Show me (§7) ───────────────────────────────────────────────────────────

(defn show-me!
  "Open the spotlight for step `step-id`, navigating first when the step's
   control lives on another page.
   Pre : `targets` is the :targets map of `checklist-input`.
   Post: a spotlight is open. Returns nil."
  [navigate! step-id {:keys [text-topic-id extract-id pdf-id]} llm-enabled?]
  (let [open-topic! (fn [topic-id] (navigate! :viewer (nav/nav-topic topic-id :home)))
        open-settings! (fn [section-id]
                         (settings-tab/select! section-id)
                         (navigate! :settings))]
    (case step-id
      :import
      (spotlight/show! {:anchors [:import-cta :rail-menu]
                        :title "Import a document"
                        :body "Start here to add a document. On a phone, open the menu first."})

      :learn
      (do (navigate! :learn)
          (spotlight/show! {:anchors [:learn-start] :navigates? true
                            :title "Read it in Learn"
                            :body "Start a session. Press Next when you finish reading."}))

      :extract
      (cond
        text-topic-id
        (do (open-topic! text-topic-id)
            (spotlight/show! {:anchors [:extract] :navigates? true
                              :title "Extract a passage"
                              :body "Select a passage in the editor, then press Extract."}))
        pdf-id
        (do (open-topic! pdf-id)
            (spotlight/show! {:anchors [:pdf-copy-text] :navigates? true
                              :title "Get the page text"
                              :body "This page has no text yet. Copy text gets it from the PDF."}))
        :else
        (spotlight/show! {:anchors []
                          :title "Extract a passage"
                          :body "Import a web page or a PDF first. Then select a passage and press Extract."}))

      :cards
      (cond
        (not llm-enabled?)
        (do (open-settings! "settings-ai")
            (spotlight/show! {:anchors [:settings-ai] :navigates? true
                              :title "Turn on AI features"
                              :body "Turn on AI features to generate cards."}))
        (or extract-id text-topic-id)
        (do (open-topic! (or extract-id text-topic-id))
            (spotlight/show! {:anchors [:generate] :navigates? true
                              :title "Make flashcards"
                              :body "Generate flashcards from this text."}))
        :else
        (spotlight/show! {:anchors []
                          :title "Make flashcards"
                          :body "Import a document with text first. Then press Generate in its toolbar."}))

      :quiz
      (do (navigate! :quiz)
          (spotlight/show! {:anchors [:quiz-start] :navigates? true
                            :title "Review a card"
                            :body "Review your new cards here."}))

      :anki
      (do (open-settings! "settings-anki")
          (spotlight/show! {:anchors [:anki-connect] :navigates? true
                            :title "Send cards to Anki"
                            :body "Connect FreeMemo to Anki on this computer."})))))

;; ── Opening it again (5.1) ─────────────────────────────────────────────────

(defonce ^{:doc "nil, or a uuid per \"Show getting started\" request. Home's
   `ReopenRequests` spends it and sets it back to nil."}
  !reopen-request
  (atom nil))

;; A client handler cannot write the dismissed row, so it records the request
;; and goes Home, where `ReopenRequests` owns the server write.
(bus/register-handler! :show-getting-started
  (fn [ctx _payload]
    (reset! !reopen-request (random-uuid))
    (when-let [navigate! (:navigate! ctx)]
      (navigate! :home))))

(e/defn ReopenRequests
  "Spend a \"Show getting started\" request: delete the dismissed row, then
   bump `:settings-refresh` so Home's summary reads it again. Mount on Home.
   The request goes back to nil when its token is spent, so a later Home
   mount cannot replay it over a newer Dismiss."
  [user-id]
  (e/client
    (let [request (e/watch !reopen-request)
          [t _error] (e/Token request)]
      (when t
        (e/on-unmount #(reset! !reopen-request nil))
        (let [r (e/server (e/Offload #(reopen!* user-id)))]
          (case r
            (if (:success r)
              (case (e/server (commands/bump! user-id :set-setting))
                (t))
              (t (:error r)))))))))

;; ── Components ─────────────────────────────────────────────────────────────

(e/defn DismissButton [user-id]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm getting-started__dismiss"})
      (dom/text "Dismiss")
      (let [click (dom/On "click" (fn [_] (random-uuid)) nil)
            [t _error] (e/Token click)]
        (when t
          (let [r (e/server (e/Offload #(dismiss!* user-id)))]
            (case r
              (if (:success r)
                (case (e/server (commands/bump! user-id :set-setting))
                  (t))
                (t (:error r))))))))))

(e/defn StepRow
  "One step: a mark, the label and text, and \"Show me\" while it is open."
  [navigate! {:keys [id label text state optional?]} targets llm-enabled?]
  (e/client
    (dom/li
      (dom/props {:class (str "getting-started__step is-" (name state))})
      (dom/span
        (dom/props {:class "getting-started__mark" :aria-hidden "true"})
        (when (= state :done) (icons/Icon :circle-check :size 18)))
      (dom/div
        (dom/props {:class "getting-started__words"})
        (dom/span
          (dom/props {:class "getting-started__label"})
          (dom/text label)
          (when (= state :done)
            (dom/span (dom/props {:class "nav-rail__sr"}) (dom/text " (done)")))
          (when optional?
            (dom/span (dom/props {:class "getting-started__optional"}) (dom/text "optional"))))
        (dom/span
          (dom/props {:class "getting-started__text"})
          (dom/text text)))
      (when (#{:next :pending} state)
        (dom/button
          (dom/props {:class (if (= state :next) "btn btn-primary btn-sm" "btn btn-sm")})
          (dom/text "Show me")
          (dom/On "click" (fn [_] (show-me! navigate! id targets llm-enabled?)) nil))))))

(e/defn GettingStarted
  "The checklist block. `steps` is the non-nil result of
   `getting-started-steps/checklist`, and `input` is `checklist-input`."
  [navigate! user-id steps {:keys [targets llm-enabled?]}]
  (e/client
    (let [required (remove :optional? steps)
          done (count (filter #(= :done (:state %)) required))]
      (dom/section
        (dom/props {:class "getting-started" :aria-labelledby "getting-started-title"})
        (dom/div
          (dom/props {:class "getting-started__head"})
          (dom/h2
            (dom/props {:id "getting-started-title" :class "getting-started__title"})
            (dom/text "Get started"))
          (dom/span
            (dom/props {:class "getting-started__count"})
            (dom/text (str done " of " (count required) " done")))
          (DismissButton user-id))
        (dom/ol
          (dom/props {:class "getting-started__steps"})
          (e/for-by :id [step steps]
            (StepRow navigate! step targets llm-enabled?)))))))
