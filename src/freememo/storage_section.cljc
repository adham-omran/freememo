(ns freememo.storage-section
  "Storage usage card. Extracted from settings_page so each e/defn stays under
   the JVM 64KB bytecode limit. Reactive on the user's :refresh atom — bumps
   on upload/delete propagate here."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.quota :as quota])
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

(e/defn StorageSection [user-id]
  (e/client
    (let [refresh (e/server (e/watch (us/get-atom user-id :refresh)))
          usage (e/server (get-usage* refresh user-id))
          cap (e/server (get-quota* refresh user-id))
          pct (if (and cap (pos? cap)) (* 100.0 (/ (double usage) cap)) 0.0)
          used-mb (e/server (format-mb* refresh usage))
          cap-mb (e/server (format-mb* refresh cap))
          pct-str (e/server (format-pct* refresh pct))
          bar-color (cond (>= pct 100) "var(--color-danger)"
                          (>= pct 80)  "var(--color-warning, #c98a00)"
                          :else        "var(--color-primary)")
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
        (dom/div
          (dom/props {:class "hint"})
          (dom/text "Storage is consumed by uploaded PDFs and EPUBs. Delete documents from the Library to free space."))))))
