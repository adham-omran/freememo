(ns freememo.anki-connect-guide
  "Getting Anki to accept FreeMemo (plans/getting-started-checklist.md §9).

   A hosted FreeMemo is not in AnkiConnect's default `webCorsOriginList`, so
   every sync from it failed until a user edited that list by hand.
   `requestPermission` is the one action AnkiConnect answers for any origin:
   Anki asks the user, and Yes adds this site to the list. Connect sends it.

   Nothing here contacts AnkiConnect until the user presses Connect (8.2.2).
   From a public site, Chrome 142 and later asks for local-network access on
   the first request, and that prompt must follow a press, not a page load.

   Settings → Anki Sync mounts the guide with no `on-granted`. The sync
   modal's error panel passes its Retry, so a granted Connect goes straight on
   to loading decks."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.anki-connect-status :as status]
   [freememo.command-bus :as bus]
   [freememo.spotlight :as spotlight]
   #?(:cljs [freememo.anki-sync-helpers :as helpers])
   #?(:clj [freememo.db :as db])))

(def addon-code "2055492159")

(def app-nap-commands
  "The AnkiConnect README's fix for App Nap on macOS (8.3.5)."
  (str "defaults write net.ankiweb.dtop NSAppSleepDisabled -bool true\n"
       "defaults write net.ichi2.anki NSAppSleepDisabled -bool true\n"
       "defaults write org.qt-project.Qt.QtWebEngineCore NSAppSleepDisabled -bool true"))

(defn newest-carded-root-id* [user-id]
  #?(:clj (:carded-root-id (db/get-getting-started-targets user-id))
     :cljs nil))

(defn copy-text!
  "Put `text` on the clipboard. A refusal (no permission, no secure context)
   leaves the text on screen for the user to copy by hand."
  [text]
  #?(:cljs (some-> js/navigator .-clipboard (.writeText text) (.catch (fn [_] nil)))
     :clj nil)
  nil)

(defn connect!
  "Send `requestPermission` and record what came back in `!state`.
   Pre : `on-granted` is nil or a fn of no arguments.
   Post: `!state` is {:phase :connecting} at once, then {:phase outcome
         :version v} with an outcome of `anki-connect-status/classify`.
         `on-granted` runs once for a usable grant. Returns nil."
  [!state on-granted]
  #?(:cljs
     (do (reset! !state {:phase :connecting})
         ((helpers/anki-call! "requestPermission" nil {:timeout-ms status/connect-timeout-ms})
          (fn [result]
            (let [phase (status/classify {:result result})]
              (reset! !state {:phase phase :version (:version result)})
              (when (and on-granted (= phase :granted) (not (status/outdated? result)))
                (on-granted))))
          (fn [^js err]
            (reset! !state {:phase (status/classify
                                     {:failure (if (= "TimeoutError" (.-name err))
                                                 :timeout
                                                 :unreachable)})}))))
     :clj nil)
  nil)

(defn show-sync-trigger!
  "Open the newest document with cards and point at its Anki Sync menu
   (8.3.1, 8.5.4). With no cards yet, say so in a centered card."
  [root-id]
  (if root-id
    (do (bus/dispatch! :open-topic {:topic-id root-id})
        (spotlight/show! {:anchors [:anki-sync]
                          :route-key (spotlight/route-key :viewer root-id)
                          :title "Send cards to Anki"
                          :body "Open this menu and choose Sync with Anki."}))
    (spotlight/show! {:anchors []
                      :title "Send cards to Anki"
                      :body "Make flashcards in a document first. Then choose Sync with Anki in its toolbar."})))

;; ── Components ─────────────────────────────────────────────────────────────

(e/defn CopyButton [label text]
  (e/client
    (dom/button
      (dom/props {:class "btn btn-sm"})
      (dom/text label)
      (dom/On "click" (fn [_] (copy-text! text)) nil))))

(e/defn GrantedPanel
  "Connected. Settings adds the way to the first push; the modal goes on by
   itself, so it passes `show-next?` false."
  [user-id version show-next?]
  (e/client
    (dom/div
      (dom/props {:class "anki-connect-guide__outcome is-ok" :role "status"})
      (if (status/outdated? {:version version})
        (dom/p (dom/text "Anki answered, but its AnkiConnect is too old. In Anki, choose Tools → Add-ons, select AnkiConnect and press Check for Updates."))
        (dom/p (dom/text "Connected to Anki.")))
      (when show-next?
        (let [root-id (e/server (e/Offload #(newest-carded-root-id* user-id)))]
          (dom/p
            (dom/text "Open a document with cards and choose Sync with Anki. ")
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"})
              (dom/text "Show me")
              (dom/On "click" (fn [_] (show-sync-trigger! root-id)) nil))))))))

(e/defn UnreachablePanel []
  (e/client
    (dom/div
      (dom/props {:class "anki-connect-guide__outcome is-problem" :role "alert"})
      (dom/p (dom/text "FreeMemo could not reach Anki. Check these in order:"))
      (dom/ol
        (dom/li (dom/text "Anki is open on this computer."))
        (dom/li (dom/text "AnkiConnect is installed, and you restarted Anki after you installed it."))
        (dom/li (dom/text "Your browser allows this site to reach your computer. In Chrome and Edge, open site settings and allow Local network access. In Brave, allow this site at brave://settings/content/localhostAccess."))
        (dom/li (dom/text "On Windows, you accepted the firewall prompt when Anki started.")))
      (dom/p
        (dom/text "To test Anki alone, open ")
        (dom/a (dom/props {:href "http://localhost:8765" :target "_blank" :rel "noopener noreferrer"})
          (dom/text "http://localhost:8765"))
        (dom/text " in a new tab. It shows the word AnkiConnect when Anki is ready.")))))

(e/defn TimeoutPanel []
  (e/client
    (dom/div
      (dom/props {:class "anki-connect-guide__outcome is-problem" :role "alert"})
      (dom/p (dom/text "Anki did not answer in time. On a Mac, App Nap pauses Anki while it is in the back. Run these three commands in Terminal, then restart Anki:"))
      (dom/pre (dom/props {:class "anki-connect-guide__code"}) (dom/text app-nap-commands))
      (CopyButton "Copy commands" app-nap-commands))))

(e/defn OutcomePanel [user-id {:keys [phase version]} show-next?]
  (e/client
    (case phase
      :idle nil
      :connecting (dom/p (dom/props {:class "anki-connect-guide__outcome" :role "status"})
                    (dom/text "Waiting for Anki…"))
      :granted (GrantedPanel user-id version show-next?)
      :denied (dom/div
                (dom/props {:class "anki-connect-guide__outcome is-problem" :role "alert"})
                (dom/p (dom/text "Anki said No. Press Connect again and choose Yes."))
                (dom/p (dom/text "If Anki showed no question, this site is on its ignore list. In Anki, choose Tools → Add-ons → AnkiConnect → Config and remove this site from ignoreOriginList.")))
      :needs-key (dom/div
                   (dom/props {:class "anki-connect-guide__outcome is-problem" :role "alert"})
                   (dom/p (dom/text "Your AnkiConnect asks for an API key, and FreeMemo sends none. In Anki, choose Tools → Add-ons → AnkiConnect → Config and set apiKey to null.")))
      :unreachable (UnreachablePanel)
      :timeout (TimeoutPanel))))

(e/defn AnkiConnectGuide
  "The three setup steps, Connect, and what Connect found (8.1).
   `on-granted` is nil in Settings, where a grant shows the way to the first
   push. The sync modal passes its Retry instead."
  [user-id on-granted]
  (e/client
    (let [!state (atom {:phase :idle})
          state (e/watch !state)]
      (dom/div
        (dom/props {:class "anki-connect-guide"})
        (dom/h4 (dom/props {:class "anki-connect-guide__title"}) (dom/text "Connect to Anki"))
        (dom/ol
          (dom/props {:class "anki-connect-guide__steps"})
          (dom/li
            (dom/text "Install Anki 23.10 or later from ")
            (dom/a (dom/props {:href "https://apps.ankiweb.net" :target "_blank" :rel "noopener noreferrer"})
              (dom/text "apps.ankiweb.net"))
            (dom/text "."))
          (dom/li
            (dom/text (str "In Anki, choose Tools → Add-ons → Get Add-ons, enter " addon-code
                        ", then restart Anki. "))
            (CopyButton "Copy code" addon-code))
          (dom/li
            (dom/text "Keep Anki open, then press Connect. Anki asks whether to allow this site. Choose Yes. ")
            (dom/button
              (dom/props {:class "btn btn-sm btn-primary"
                          :disabled (= :connecting (:phase state))})
              (dom/props {:data-tour "anki-connect"})
              (dom/text "Connect")
              (dom/On "click" (fn [_] (connect! !state on-granted)) nil))))
        (dom/p
          (dom/props {:class "anki-connect-guide__note"})
          (dom/text "Anki must run on this computer. Phones and tablets cannot connect."))
        (OutcomePanel user-id state (nil? on-granted))))))
