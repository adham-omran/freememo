(ns freememo.model-context
  "The context length of each OpenRouter model, cached from OpenRouter's model
   catalog (plans/assistant-chat-forks.md §4.5.1).

   The model registry stores no context length, and a length recorded by hand
   goes stale when a provider changes it. The catalog reports
   `top_provider.context_length` for each slug.

   A send never waits for the catalog. `context-length` answers from the cache
   and starts a refresh in the background when the cache is older than a day.
   Until the first refresh lands it answers nil, and then only the overflow
   fallback protects a send (§4.5.6)."
  (:require [freememo.openrouter :as openrouter]
            [taoensso.telemere :as tel]))

(def ^:private refresh-interval-ms
  "A catalog younger than this is not fetched again."
  (* 24 60 60 1000))

(def ^:private retry-interval-ms
  "After a failed fetch, the next attempt waits this long, so a network
   outage does not start a fetch on every send."
  (* 10 60 1000))

(defonce ^:private !catalog
  ;; :lengths maps an OpenRouter slug to its context length in tokens.
  (atom {:lengths {} :refreshed-at-ms nil :attempted-at-ms nil}))

(defonce ^:private !refreshing? (atom false))

(defn catalog-lengths
  "slug → context length, from the catalog's `models`.
   Post: only entries with an id and a positive integer length. The top
         provider's length wins, and the model's own length is the fallback."
  [models]
  (into {}
    (keep (fn [model]
            (let [length (or (get-in model [:top_provider :context_length])
                           (:context_length model))]
              (when (and (string? (:id model)) (integer? length) (pos? length))
                [(:id model) (long length)]))))
    models))

(defn refresh!
  "Fetch the catalog and replace the cache. Blocks for the fetch.
   Post: on success the cache holds the new lengths. On failure the cache
         keeps the lengths it had, and the failure is logged."
  []
  (let [now (System/currentTimeMillis)]
    (try
      (let [lengths (catalog-lengths (openrouter/list-models!))]
        (swap! !catalog assoc :lengths lengths :refreshed-at-ms now :attempted-at-ms now)
        (tel/log! {:level :info :id ::refreshed :data {:models (count lengths)}}
          "Model context lengths refreshed"))
      (catch Exception error
        (swap! !catalog assoc :attempted-at-ms now)
        (tel/error! {:id ::refresh} error)))
    nil))

(defn- stale?
  [{:keys [refreshed-at-ms attempted-at-ms]} now]
  (and (or (nil? refreshed-at-ms) (> (- now refreshed-at-ms) refresh-interval-ms))
    (or (nil? attempted-at-ms) (> (- now attempted-at-ms) retry-interval-ms))))

(defn- refresh-in-background! []
  (when (compare-and-set! !refreshing? false true)
    (future
      (try (refresh!)
        (finally (reset! !refreshing? false))))))

(defn context-length
  "The context length in tokens of the model with slug `openrouter-model`, or
   nil when the cache does not hold it.
   Post: returns without a network call. A stale cache starts one background
         refresh."
  [openrouter-model]
  (let [catalog @!catalog]
    (when (stale? catalog (System/currentTimeMillis))
      (refresh-in-background!))
    (get-in catalog [:lengths openrouter-model])))
