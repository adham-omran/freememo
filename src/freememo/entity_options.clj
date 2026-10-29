(ns freememo.entity-options
  "Bounded entity suggestions and independent selected-label reads."
  (:require [freememo.db :as db]))

(def option-limit 50)

(defn search-options
  "Read identified suggestions for a query and optional source document."
  [user-id source-id query]
  (mapv #(select-keys % [:id :label])
    (db/list-kg-entities user-id query option-limit source-id)))

(defn selected-option
  "Resolve one owned entity independently of the suggestion query."
  [user-id entity-id]
  (when (some? entity-id)
    (when-let [label (db/get-kg-entity-label user-id entity-id)]
      {:id entity-id :label label})))
