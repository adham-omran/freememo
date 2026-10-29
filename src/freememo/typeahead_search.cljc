(ns freememo.typeahead-search
  "Response checks for server-backed Typeahead requests.")

(defn matching-options
  "Return options only when the response belongs to the current request."
  [request response]
  (if (and request (= request (:request response)))
    (vec (:options response))
    []))

(defn matching-label
  "Return a selected label only when its response identifies the current ID."
  [selected-id response]
  (when (and (some? selected-id) (= selected-id (:id response)))
    (:label response)))
