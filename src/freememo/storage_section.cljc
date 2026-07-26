(ns freememo.storage-section
  "Storage usage card. Extracted from settings_page so each e/defn stays under
   the JVM 64KB bytecode limit. Reactive on the user's :refresh atom — bumps
   on upload/delete propagate here."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
   #?(:clj [freememo.storage-meter :as meter])
   #?(:clj [freememo.user-state :as us])))

;; Server-only wrappers — visible to both compilers, plain CLJ body.
(defn get-usage* [_refresh user-id]
  #?(:clj (quota/get-user-usage db/ds user-id)
     :cljs 0))

(defn get-quota* [_refresh user-id]
  #?(:clj (quota/get-user-quota db/ds user-id)
     :cljs 0))

;; Server-only formatters — `format` is CLJ-only; calling it inside an e/defn
;; body captures the var, which Electric tries to serialize.
(defn format-mb* [_refresh bytes]
  #?(:clj (let [mb (/ (double (or bytes 0)) 1048576.0)]
            (if (< mb 10) (format "%.2f MB" mb) (format "%.1f MB" mb)))
     :cljs ""))

(defn format-pct* [_refresh pct]
  #?(:clj (format "%.0f" (double (or pct 0)))
     :cljs ""))

(defn storage-rate-label*
  "The storage price, or nil when metering is off (self-host, or no rate
   configured). Formatted by `storage-meter`, which owns the number — the
   Credits panel renders the same string.

   Takes `_refresh` like every other wrapper here. The value is boot-constant
   (config.edn is read once), so the argument buys no reactivity — it exists
   because a zero-arity server call in this position has no reactive input for
   Electric to key the node on."
  [_refresh]
  #?(:clj (meter/storage-rate-label)
     :cljs nil))

(e/defn StorageSection [user-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          usage (e/server (get-usage* refresh user-id))
          cap (e/server (get-quota* refresh user-id))
          pct (if (and cap (pos? cap)) (* 100.0 (/ (double usage) cap)) 0.0)
          used-mb (e/server (format-mb* refresh usage))
          cap-mb (e/server (format-mb* refresh cap))
          pct-str (e/server (format-pct* refresh pct))
          rate-label (e/server (storage-rate-label* refresh))
          bar-color (cond (>= pct 100) "var(--color-danger)"
                          (>= pct 80)  "var(--color-warning, #c98a00)"
                          :else        "var(--color-primary-text)")
          bar-width-pct (if (>= pct 100) 100 pct)]
      (dom/div
        (dom/props {:class "card"})
        (dom/h3 (dom/props {:class "section-title"}) (dom/text "Storage"))
        (dom/div
          (dom/props {:style {:font-size "14px" :color "var(--color-text-primary)" :margin-bottom "8px"}})
          (dom/text (str used-mb " / " cap-mb " used"))
          (dom/span
            (dom/props {:style {:margin-left "8px" :color "var(--color-text-secondary)" :font-size "13px"}})
            (dom/text (str "(" pct-str "%)"))))
        (dom/div
          (dom/props {:style {:height "8px" :background "var(--color-bg-subtle)"
                              :border-radius "4px" :overflow "hidden"
                              :margin-bottom "8px"}})
          (dom/div
            (dom/props {:style {:height "100%"
                                :width (str bar-width-pct "%")
                                :background bar-color
                                :transition "width 0.2s"}})))
        ;; The rate, when metering is on. Shown against the usage figure above
        ;; so the two together answer "what is this costing me" without the user
        ;; having to multiply in their head on another tab.
        (when rate-label
          (dom/div
            (dom/props {:style {:display "flex" :align-items "center" :justify-content "space-between"
                                :padding "8px 10px" :margin-bottom "8px"
                                :background "var(--color-bg-subtle)"
                                :border-radius "var(--radius-md)" :font-size "13px"}})
            (dom/span
              (dom/props {:style {:color "var(--color-text-label)"}})
              (dom/text "Storage rate"))
            (dom/span
              (dom/props {:style {:font-weight "600" :color "var(--color-text-primary)"}})
              (dom/text rate-label))))
        (dom/div
          (dom/props {:class "hint"})
          (dom/text (str "Storage is consumed by uploaded PDFs, EPUBs, audio files, and videos. "
                      (if rate-label
                        (str "Stored videos are billed at this rate for as long as they are kept; "
                          "everything else is free. ")
                        "")
                      "Delete documents from the Library to free space.")))))))
