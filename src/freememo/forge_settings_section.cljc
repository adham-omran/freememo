(ns freememo.forge-settings-section
  "Settings → Git Forges: the personal access tokens a repository import reads
   with (plans/git-forge-integration.md §4.2 2.1, D4).

   Saving checks the token against the forge first. A 401 refuses it, because a
   token the forge rejects would fail every later import with the same message.
   A 403 saves it with a note: a token scoped to reading repository contents may
   not list repositories, and that costs only the suggestions in the import
   form.

   A token never returns to the client. The list shows the forge and the host."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.forge-kinds :as kinds]
   #?(:clj [freememo.commands :as commands])
   #?(:clj [freememo.forge :as forge])
   #?(:clj [freememo.forge-tokens :as tokens])
   #?(:clj [freememo.user-state :as us])))

(defn saved-origins*
  "Post: [{:forge :host} …] for every saved token. Carries no token."
  [_refresh user-id]
  #?(:clj (tokens/saved-origins user-id)
     :cljs []))

(defn save-token!*
  "Check `token` against the forge, then store it encrypted.
   Post: {:ok true} or {:ok true :warning S} when it is stored, and
         :settings-refresh is bumped; {:ok false :error S} with nothing stored."
  [user-id forge host token]
  #?(:clj
     (let [origin (forge/parse-origin (or (some-> host str/trim not-empty)
                                        (kinds/default-host forge)))
           token (some-> token str/trim)
           label (kinds/forge-label forge)]
       (cond
         (not (contains? kinds/forge-ids forge))
         {:ok false :error "Choose GitHub, GitLab or Forgejo."}

         (nil? origin)
         {:ok false :error "Enter the forge host, for example gitlab.com."}

         (str/blank? token)
         {:ok false :error "Paste a token."}

         :else
         (let [check (try
                       (forge/list-repositories forge origin token)
                       {:ok true}
                       (catch clojure.lang.ExceptionInfo ex
                         (if-not (forge/failure? ex)
                           (throw ex)
                           (case (:status (ex-data ex))
                             401 {:ok false :error (str label " refused this token. Check it and paste it again.")}
                             403 {:ok true
                                  :warning (str "Token saved. " label " does not let it list repositories, "
                                             "so the import form cannot suggest them.")}
                             {:ok false :error (.getMessage ex)}))))]
           (when (:ok check)
             (tokens/save-token! user-id forge (:host origin) token)
             (commands/bump! user-id :set-setting))
           check)))
     :cljs nil))

(defn delete-token!*
  "Post: no token is stored for (`forge`, `host`); :settings-refresh is bumped."
  [user-id forge host]
  #?(:clj (do (tokens/delete-token! user-id forge host)
              (commands/bump! user-id :set-setting)
              {:ok true})
     :cljs nil))

(e/defn FieldLabel [text]
  (dom/label (dom/props {:class "label"}) (dom/text text))
  (e/amb))

(e/defn ForgeKindSelect
  "A1-fallback select (CLAUDE.md, Forms5 → Select dropdowns)."
  [!forge]
  (let [forge (e/watch !forge)]
    (dom/select
      (dom/props {:class "input input-full"})
      (dom/On "change" (fn [e] (reset! !forge (-> e .-target .-value))) nil)
      (e/for [{:keys [id label]} (e/diff-by :id kinds/forges)]
        (dom/option
          (dom/props {:value id :selected (= id forge)})
          (dom/text label)))))
  (e/amb))

(e/defn ForgeTokensSection
  "Pre : `user-id` is signed in.
   Post: the saved tokens are listed by forge and host, each removable, and a
         form saves a new one."
  [user-id]
  (e/client
    (let [settings-refresh (e/server (e/watch (us/get-atom user-id :settings-refresh)))
          saved (e/server (e/Offload #(saved-origins* settings-refresh user-id)))
          !forge (atom "github")
          forge (e/watch !forge)
          ;; Bumped after a save, to remount the form empty: the token field
          ;; must not keep a secret on screen after it is stored.
          !generation (atom 0)
          generation (e/watch !generation)
          !outcome (atom nil)
          outcome (e/watch !outcome)]
      (dom/div
        (dom/props {:class "forge-tokens-section"})
        (dom/p
          (dom/props {:class "hint"})
          (dom/text (str "A token lets FreeMemo download private repositories. FreeMemo only reads. "
                      "A public repository needs no token. Scopes: a GitHub fine-grained token with "
                      "Contents: Read, a GitLab token with read_api, or a Forgejo token with "
                      "read:repository.")))

        (if (empty? saved)
          (dom/p (dom/props {:class "hint"}) (dom/text "No tokens are saved."))
          (dom/ul
            (dom/props {:class "forge-tokens"})
            (e/for [{:keys [forge host]} (e/diff-by (juxt :forge :host) saved)]
              (dom/li
                (dom/props {:class "forge-tokens__row"})
                (dom/span (dom/props {:dir "ltr"}) (dom/text (str (kinds/forge-label forge) " (" host ")")))
                (e/for [[t _] (forms/Button! [::remove] :class "btn btn-sm btn-danger" :label "Remove")]
                  (case (e/server (e/Offload #(delete-token!* user-id forge host)))
                    (t)))))))

        (e/for-by identity [_generation [generation]]
          (let [commits (forms/Form! {:host "" :token ""}
                          (e/fn Fields [{:keys [host token]}]
                            (e/amb
                              (FieldLabel "Forge")
                              (ForgeKindSelect !forge)
                              (FieldLabel "Host")
                              (forms/Input! :host (or host "")
                                :class "input input-full" :autocomplete "off"
                                :placeholder (kinds/default-host forge))
                              (FieldLabel "Token")
                              (forms/Input! :token (or token "")
                                :type "password" :class "input input-full" :autocomplete "off")
                              (dom/div
                                (dom/props {:class "forge-tokens__actions"})
                                (forms/SubmitButton! :label "Save token" :class "btn btn-primary"))))
                          :Parse (e/fn [fields _tempid]
                                   [`SaveForgeToken (e/watch !forge) (:host fields) (:token fields)])
                          :type :command
                          :show-buttons false)]
            (e/for [[t [_ forge-id host secret]] (e/diff-by first (e/as-vec commits))]
              (let [r (e/server (e/Offload #(save-token!* user-id forge-id host secret)))]
                (case (reset! !outcome r)
                  (if (:ok r)
                    (do (e/on-unmount #(swap! !generation inc))
                        (t))
                    (t (:error r))))))))

        ;; Success only. A failed save spends its token with the error, and
        ;; Forms5 prints that error inside the form (`p[data-role=errormessage]`)
        ;; — a second copy here printed every failure twice.
        (when (:ok outcome)
          (dom/p
            (dom/props {:class "forge-tokens__outcome" :role "status"})
            (dom/text (or (:warning outcome) "Token saved."))))))))
