(ns freememo.gen-settings
  "The settings a Generate action reads: how many cards, which kind, and
   whether to include context and from how many pages
   (plans/toolbar-card-actions-fold.md §1).

   One client-global atom, so the plain-CLJS selection popover
   (freememo.format-menu) and the Electric invokers in freememo.card-actions-host
   read and write the SAME value. Defined on both peers (the keyboard.cljc
   idiom) so the var can be named inside an e/defn body without a frame-signal
   mismatch.

   Lifecycle: nil until `CardActionsHost` seeds it from the user's saved
   settings. A writer changes it with `assoc-setting!` and then runs the
   :persist-gen-settings invoker, which saves the whole map.

   Leaf ns (no requires): both the Electric and the plain-CLJS sides depend on
   it without a cycle.")

(def count-min 1)
(def count-max 20)
(def context-pages-min 1)
(def context-pages-max 10)

(def kinds
  "The card kinds a Generate action makes, in display order."
  ["basic" "cloze" "overlapping"])

(def kind-labels
  {"basic" "Basic" "cloze" "Cloze" "overlapping" "Overlapping"})

(def defaults
  "What `settings/get-*` answers for a user who never saved a value."
  {:count 2 :kind "basic" :context? true :context-pages 1})

(defn clamp [lo hi n] (max lo (min hi n)))

(defn normalize
  "Post: `v` coerced into the legal range of setting `k`. An unknown kind
   becomes the default kind."
  [k v]
  (case k
    :count (clamp count-min count-max v)
    :context-pages (clamp context-pages-min context-pages-max v)
    :kind (if (some #{v} kinds) v (:kind defaults))
    :context? (boolean v)
    v))

(defn- whole-number
  "`v` as an integer: a number as is, a numeric string parsed, else nil."
  [v]
  (cond (number? v) (long v)
        (string? v) (parse-long v)
        :else nil))

(defn from-form
  "Settings from a form's fields, whose numbers arrive as strings.
   Post: every key present and normalized. A field missing or unparseable
         takes its value from `fallback`, a full settings map."
  [fields fallback]
  {:count (normalize :count (or (whole-number (:count fields)) (:count fallback)))
   :kind (normalize :kind (or (:kind fields) (:kind fallback)))
   :context? (boolean (if (contains? fields :context?) (:context? fields) (:context? fallback)))
   :context-pages (normalize :context-pages (or (whole-number (:context-pages fields))
                                                (:context-pages fallback)))})

;; nil until CardActionsHost seeds it.
(defonce !gen-settings (atom nil))

(defn effective
  "Post: `settings` with every key present. A nil map reads as `defaults`."
  [settings]
  (merge defaults settings))

(defn current
  "The settings as plain CLJS reads them, every key present."
  []
  (effective @!gen-settings))

(defn assoc-setting!
  "Set one setting, normalized. Returns the new map.
   Post: the atom holds every key, so an unseeded atom becomes a full map."
  [k v]
  (swap! !gen-settings (fn [m] (assoc (effective m) k (normalize k v)))))
