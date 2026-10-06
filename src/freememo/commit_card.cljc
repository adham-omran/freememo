(ns freememo.commit-card
  "The commit snapshot node's card (plans/repository-home.md §4.4): the
   commit's message, author, date, sha, ref and changed-file count, read from
   the repository's clone, and \"Walk through this commit\". It stands in for
   the notes editor, which a commit node does not have.

   Its own namespace for the 64KB method cap."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [hyperfiddle.electric-forms5 :as forms]
   [freememo.command-bus :as bus]
   [freememo.doc-context :as dctx]
   [freememo.loading :as loading]
   [freememo.navigation :as nav]
   [freememo.walkthrough-entry :as entry]
   #?(:clj [freememo.repo-commits :as commits])))

(defn card*
  "Post: `repo-commits/commit-card`'s result, never nil: `WithLoading` reads
   nil as still loading."
  [user-id topic-id]
  #?(:clj (or (commits/commit-card user-id topic-id)
              {:error "This commit is not part of an imported repository."})
     :cljs nil))

(defn copy-text!
  "Put `text` on the clipboard. A refusal leaves the clipboard as it was."
  [text]
  #?(:cljs (some-> js/navigator .-clipboard (.writeText text) (.catch (fn [_] nil)))
     :clj nil)
  nil)

(defn click-walk!
  "Click the card's walk button, as the palette command does.
   Post: true when the button was there."
  []
  #?(:cljs (if-let [b (.querySelector js/document ".commit-card__walk:not([disabled])")]
             (do (.click b) true)
             false)
     :clj false))

(defn- shown-date [iso] (str/replace (str iso) "T" " "))

(e/defn WalkButton
  "\"Walk through this commit\": a walkthrough whose first and last commit
   are this one (§4.4.3). Disabled, with the reason, when the range is
   refused (§4.4.4)."
  [user-id {:keys [repo-id sha refusal]} navigate!]
  (e/client
    (let [!error (atom nil)
          error (e/watch !error)]
      (when-not refusal
        (bus/publish-invoker! :walk-through-commit click-walk!))
      (e/on-unmount #(bus/retract-invoker! :walk-through-commit))
      (e/for [[t _] (forms/Button! [::walk] :class "btn btn-primary commit-card__walk"
                      :label "Walk through this commit"
                      :disabled (some? refusal))]
        (let [r (e/server (e/Offload #(entry/start!* user-id repo-id sha sha)))]
          (case r
            (if (:ok r)
              (case (navigate! :viewer (nav/nav-walkthrough repo-id (:walkthrough-id r))) (t))
              (case (reset! !error (:error r)) (t (:error r)))))))
      (dom/p
        (dom/props {:class "wt-panel__hint commit-card__refusal" :role "status"})
        (dom/props {:style {:display (if (or refusal error) "block" "none")}})
        (dom/text (or error refusal ""))))))

(e/defn CommitCardBody
  "The card for a read that landed. `card` is `repo-commits/commit-card`'s
   result."
  [user-id card navigate!]
  (e/client
    (if (:error card)
      (dom/p (dom/props {:class "wt-picker__error" :role "alert"})
        (dom/text (:error card)))
      (dom/article
        (dom/props {:class "commit-card" :data-testid "commit-card"})
        (dom/h2 (dom/props {:class "commit-card__subject" :dir "auto"})
          (dom/text (:subject card)))
        (dom/pre (dom/props {:class "commit-card__body" :dir "auto"
                             :style {:display (if (str/blank? (:body card)) "none" "block")}})
          (dom/text (:body card)))
        (dom/dl
          (dom/props {:class "commit-card__facts"})
          (dom/dt (dom/text "Author"))
          (dom/dd (dom/props {:class "commit-card__author"}) (dom/text (:author card)))
          (dom/dt (dom/text "Date"))
          (dom/dd (dom/props {:class "commit-card__date"}) (dom/text (shown-date (:date card))))
          (dom/dt (dom/text "Commit"))
          (dom/dd
            (dom/code (dom/props {:class "commit-card__sha" :dir "ltr"}) (dom/text (:sha card)))
            (dom/button
              (dom/props {:type "button" :class "btn btn-sm btn-secondary commit-card__copy"})
              (dom/text "Copy")
              (dom/On "click" (fn [_] (copy-text! (:sha card))) nil)))
          (dom/dt (dom/text "Branch or tag"))
          (dom/dd (dom/props {:class "commit-card__ref" :dir "ltr"}) (dom/text (or (:ref card) "")))
          (dom/dt (dom/text "Changed files"))
          (dom/dd (dom/props {:class "commit-card__files"}) (dom/text (str (:changed-files card)))))
        (WalkButton user-id card navigate!)))))

(e/defn CommitCard
  "The card of the commit node the viewer shows. The read runs once per
   node: the key remounts it when the viewer moves to another commit."
  []
  (e/client
    (let [user-id dctx/user-id
          navigate! dctx/navigate!]
      (dom/div
        (dom/props {:class "commit-card-host"})
        (e/for-by identity [topic-id [dctx/page-topic-id]]
          (loading/WithLoading
            (e/fn [] (e/server (e/Offload #(card* user-id topic-id))))
            (e/fn [card] (CommitCardBody user-id card navigate!))
            (e/fn [] (loading/Spinner "Reading the commit…"))))))))
