(ns freememo.bibliography-button
  "Bibliography toolbar button — re-runs identifier resolution and merge for
   a topic that already has a source row. Disabled when topic.source_id is
   nil (per spec: 'refetch an existing source' only)."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.icons :as icons]
   #?(:clj [freememo.biblio-import :as biblio-import])
   #?(:clj [freememo.db :as db])))

(defn refetch-biblio!*
  "Server wrapper for biblio-import/refetch-biblio!, callable from .cljc.
   Pre:  user-id and topic-id non-nil.
   Post: {:ok true :source-id N} or {:ok false :error msg-or-kw}.
   Blame: caller bug if user-id or topic-id is nil."
  [user-id topic-id]
  #?(:clj (biblio-import/refetch-biblio! user-id topic-id)
     :cljs nil))

(defn has-source?*
  "Server fn — true iff the topic has a sources row attached.
   `_refresh` creates a reactive dep so the button re-evaluates when the
   :refresh channel bumps after a save/attach."
  [_refresh user-id topic-id]
  #?(:clj (boolean (and user-id topic-id
                     (some-> (db/get-topic-for-user user-id topic-id)
                       :topics/source_id)))
     :cljs false))

;; !overflow-open: optional atom. When non-nil, reset to false in
;; `e/on-unmount` inside `(when t …)`, so the overflow dropdown closes
;; only AFTER the server effect resolves (success or error). The inline
;; toolbar mount passes nil since there is no dropdown to close.
;;
;; Pre:  user-id and topic-id non-nil; has-source? is a boolean derived
;;       reactively from the topic's source_id.
;; Post: A toolbar button is mounted. Disabled when (not has-source?)
;;       or while a server roundtrip is in flight; aria-busy during the
;;       roundtrip. On click, runs refetch-biblio!*; spends the token on
;;       success or error. On error, the error string is displayed inside
;;       the button until the next token spend.
(e/defn BibliographyButton [user-id topic-id has-source? !overflow-open]
  (e/client
    (dom/button
      ;; !flash-success: stable per dom/button mount (constant seed, no
      ;; reactive flicker). Set true via e/on-unmount on a successful token
      ;; spend, cleared ~1.2s later via setTimeout. Green flash = "click
      ;; completed without error" regardless of whether a resolver actually
      ;; fetched new data (no-op refetches still flash green).
      (let [!flash-success (atom false)
            flash-success? (e/watch !flash-success)
            click-event (dom/On "click" (fn [e] e) nil)
            [t ?error] (e/Token click-event)]
        (dom/props {:class "btn btn-sm btn-secondary"
                    :aria-label "Refetch bibliography"
                    :data-tooltip "Refetch bibliography"
                    :disabled (or (not has-source?) (some? t))
                    :aria-busy (some? t)
                    :style (when flash-success?
                             {:background "var(--color-success-light)"
                              :border-color "var(--color-success)"
                              :color "var(--color-success-dark)"
                              :transition "background-color 0.2s ease, border-color 0.2s ease, color 0.2s ease"})})
        (icons/Icon :library :size 16)
        (dom/span (dom/props {:class "icon-label"}) (dom/text "Refetch bibliography"))
        (when ?error
          (dom/div
            (dom/props {:style {:color "var(--color-danger)" :font-size "11px"
                                :margin-top "4px"}})
            (dom/text ?error)))
        (when t
          (when !overflow-open
            (e/on-unmount #(reset! !overflow-open false)))
          (let [result (e/server (e/Offload #(refetch-biblio!* user-id topic-id)))]
            (when (some? result)
              (if (:ok result)
                ;; e/on-unmount inside the success branch — fires when (t)
                ;; unmounts (when t …). Schedules a CLJS-side timer to clear
                ;; the flash after 1.2s. Reader-conditional is inside a plain
                ;; fn body (safe — callbacks aren't reactively compiled).
                (do (e/on-unmount
                      (fn []
                        (reset! !flash-success true)
                        #?(:cljs (js/setTimeout
                                   #(reset! !flash-success false)
                                   1200))))
                    (t))
                (t (str (:error result)))))))))))
