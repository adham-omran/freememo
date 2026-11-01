(ns freememo.document-picker
  "Server-backed source, schedule, curriculum, and playlist pickers."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.typeahead :as typeahead]
            #?(:clj [freememo.option-queries :as queries])))

(defn search* [_refresh user-id domain kind request]
  #?(:clj (queries/search user-id domain kind request) :cljs nil))

(defn selected* [_refresh user-id domain id]
  #?(:clj (queries/selected user-id domain id) :cljs nil))

(e/defn DocumentPicker [!id refresh user-id domain kind placeholder]
  (typeahead/SearchTypeahead !id
                             (e/fn Search [request]
                               (e/server (e/Offload #(search* refresh user-id domain kind request))))
                             (e/fn Selected [id]
                               (e/server (e/Offload #(selected* refresh user-id domain id))))
                             [refresh user-id domain kind] placeholder nil false)
  (e/amb))
