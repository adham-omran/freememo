(ns freememo.walkthrough-model-stub
  "A stand-in for the model behind a walkthrough's grouping, stubbed at
   `openrouter/chat-completion!` so `kg-llm/chat!` and its JSON parser run as
   they do in production (plans/commit-range-walkthrough.md §6.5).

   Pass 1 names \"split hold\" for the commit whose subject starts with feat,
   and \"rename cycle to loop\" for the one that starts with refactor. Pass 2
   puts a hunk in the concept of its commit's subject."
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn- reply-object [system user]
  (cond
    (str/starts-with? system "You read the commits")
    {:concepts [{:label "rename cycle to loop" :summary "Cycle is now loop."
                 :commits (keep second [(re-find #"## Commit (\w+)\nrefactor" user)])
                 :depends_on ["split hold"]}
                {:label "split hold" :summary "A hold op splits."
                 :commits (keep second [(re-find #"## Commit (\w+)\nfeat" user)])}]}

    (str/starts-with? system "You place each hunk")
    {:assignments (into {} (map (fn [[_ id subject]]
                                  [id (if (str/starts-with? subject "feat")
                                        "split hold" "rename cycle to loop")]))
                    (re-seq #"### (H\d+) · commit \w+ \"([^\"]*)\"" user))}

    :else (throw (ex-info "stub: no reply for this prompt" {:system (subs system 0 40)}))))

(defn chat-completion!
  "`openrouter/chat-completion!`'s contract: a 200 body whose message content
   is the reply as JSON text."
  ([api-key body] (chat-completion! api-key body nil))
  ([_api-key body _ctx]
   (let [[system user] (map :content (:messages body))]
     {:choices [{:message {:content (json/generate-string (reply-object system user))}
                 :finish_reason "stop"}]
      :usage {:cost 0.0 :prompt_tokens 10 :completion_tokens 10}})))
