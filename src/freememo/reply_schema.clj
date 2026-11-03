(ns freememo.reply-schema
  "The reply contract of a structured AI call: the JSON Schema its reply must
   match, in the form OpenRouter's strict structured outputs accept.

   A contract is {:name :root :schema}. `:schema` describes the value under the
   reply's one root key. `kg-llm/chat!` and the card loop send the wrapped
   schema to every model whose registry entry lists :structured_outputs, and JSON
   mode to the others. Every feature still runs its own checks: a schema cannot
   check a quote against a source, and some models get no schema at all.

   Strict mode needs three things of every object: each property is required,
   `additionalProperties` is false, and the property names are fixed. So an
   optional field is (nullable …), and a map whose keys the model chooses is an
   array of entries that the feature's reader turns back into a map.
   See plans/structured-reply-schemas.md."
  (:require [clojure.string :as str]
            [freememo.openrouter :as openrouter]))

(defn strict-object
  "An object schema that strict mode accepts: every property required, no other
   property. Write an optional field as (nullable …)."
  [properties]
  {:type "object" :properties properties
   :required (mapv name (keys properties)) :additionalProperties false})

(defn nullable
  "Allow null by a type list. All eight registry models accepted this form on
   2026-11-03. A JSON null reaches the feature as an absent field, because
   kg-llm/chat! drops null fields."
  [schema]
  (assoc schema :type [(:type schema) "null"]))

(defn array-of [items] {:type "array" :items items})
(defn enum-of [values] {:type "string" :enum (vec values)})
(def json-string {:type "string"})
(def json-integer {:type "integer"})
(def json-boolean {:type "boolean"})

(defn- base-type
  "The non-null type of a schema whose :type may be a [type \"null\"] list."
  [schema]
  (let [t (:type schema)]
    (if (sequential? t) (first (remove #{"null"} t)) t)))

(defn strict-problems
  "Why strict mode would refuse `schema`, or an empty vector.
   Post: one string per problem, each naming the path of the schema it is in.
   Checks every object and array at every depth."
  ([schema] (strict-problems schema "$"))
  ([schema path]
   (case (base-type schema)
     "object"
     (let [properties (:properties schema)]
       (if-not (map? properties)
         [(str path ": an object without fixed properties is an open map")]
         (into (cond-> []
                 (not= (set (map name (keys properties))) (set (:required schema)))
                 (conj (str path ": every property must be required"))
                 (not (false? (:additionalProperties schema)))
                 (conj (str path ": additionalProperties must be false")))
           (mapcat (fn [[k v]] (strict-problems v (str path "." (name k))))) properties)))
     "array"
     (if (map? (:items schema))
       (strict-problems (:items schema) (str path "[]"))
       [(str path ": an array needs an items schema")])
     [])))

(defn contract
  "A reply contract.
   Pre:  name matches [A-Za-z0-9_-]+, root is a keyword, and schema is an
         object or array schema with no strict-problems.
   Post: {:name :root :schema}.
   Throws at the call, so a contract defined with `def` fails when its
   namespace loads, before any model call."
  [name root schema]
  (let [problems (cond-> (strict-problems schema)
                   (not (re-matches #"[A-Za-z0-9_-]+" (str name))) (conj (str "bad contract name " (pr-str name)))
                   (not (keyword? root)) (conj "the root must be a keyword")
                   (not (#{"object" "array"} (:type schema))) (conj "the root value must be an object or an array"))]
    (when (seq problems)
      (throw (ex-info (str "Invalid reply contract " name ": " (str/join "; " problems))
               {:type ::invalid-contract :name name :problems problems})))
    {:name name :root root :schema schema}))

(defn require-contract!
  "Pre for kg-llm/chat! and the card loop. Throws ::invalid-contract unless
   `reply` is a contract that `contract` would build."
  [reply]
  (if (and (map? reply) (:name reply) (:root reply) (:schema reply))
    (contract (:name reply) (:root reply) (:schema reply))
    (throw (ex-info "A structured AI call needs a reply contract {:name :root :schema}."
             {:type ::invalid-contract :reply reply}))))

(defn root-type
  "The root type llm-json/parse-response-field checks: :map or :vector."
  [reply]
  (if (= "array" (:type (:schema reply))) :vector :map))

(defn wire-schema
  "The schema sent to the provider: an object whose one property is the root."
  [reply]
  (strict-object {(:root reply) (:schema reply)}))

(defn request-fields
  "Model fields for one call under `reply`.
   Pre:  model-fields come from structured-chat-completion-fields in
         freememo.openrouter.
   Post: model-fields with a json_schema response_format when `entry` lists
         :structured_outputs; otherwise model-fields unchanged (JSON mode).
   Invariant: routing and provider.require_parameters are unchanged."
  [entry model-fields reply]
  (openrouter/with-response-schema entry model-fields (:name reply) (wire-schema reply)))

(defn drop-null-fields
  "Read a JSON null in a model reply as an absent field.
   Post: the value with every map entry whose value is nil removed, at every
   depth. Invariant: vectors keep their length and order."
  [value]
  (cond
    (map? value) (into {} (keep (fn [[k v]] (when (some? v) [k (drop-null-fields v)]))) value)
    (vector? value) (mapv drop-null-fields value)
    :else value))
