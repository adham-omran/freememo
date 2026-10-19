(ns freememo.command-targets
  "What a target command acts on, and whether it can act on a given target.

   A TARGET is the thing a right-click or a focused row hands a command:

     {:kind     one of `target-kinds`
      :subjects [subject …]   ; at least one, plain data
      :input    any           ; the value a :menu-input step collected
      :surface  {…}}          ; client-only handles: a Quill instance, an anchor
                              ; element, the surface's own callbacks

   `:surface` never crosses the wire. command-bus strips it before a :queue
   command is enqueued, so everything a server effect reads lives in
   `:subjects` or `:input`.

   A registry entry opts in with `:target` (a kind), `:arity` (`:one` by
   default, or `:many`), and two sets of SUBJECT TESTS: `:target-if` hides the
   item when a subject fails a test, and `:enable-if` shows it disabled with the
   test's reason. The tests are named here, so the registry stays pure data.

   Pure: no requires beyond clojure.string, so a clj test reaches all of it."
  (:require [clojure.string :as str]))

(def target-kinds
  "The kinds of thing a target command can act on."
  #{:topic :card :image :audio-clip :caret :pdf-page})

(def arities #{:one :many})

(defn- same-origin-src?
  "True when the image's bytes come from this app or are inline. Copy image
   works there without the remote origin's CORS headers."
  [src]
  (let [src (str src)]
    (or (str/starts-with? src "data:")
        (str/starts-with? src "/api/media/"))))

(def subject-tests
  "Named facts a subject must hold. A registry entry names them in
   `:target-if` and `:enable-if`.

   Each test reads keys a surface puts on its subjects. A key the surface does
   not know is absent, and every hide test reads an absent key as the safe
   answer: the item is hidden, never offered on a guess. :pin-room is the one
   enable test, and it says why it differs."
  {:root               (fn [s] (true? (:root? s)))
   :not-root           (fn [s] (false? (:root? s)))
   :has-children       (fn [s] (true? (:has-children? s)))
   :dismissed          (fn [s] (true? (:dismissed? s)))
   :not-dismissed      (fn [s] (false? (:dismissed? s)))
   :not-done           (fn [s] (and (string? (:status s)) (not= "done" (:status s))))
   ;; A PDF whose root? is unknown counts as a root: nearly every PDF is one.
   :not-pdf-root       (fn [s] (and (some? (:topic-kind s))
                                    (not (and (= "pdf" (:topic-kind s))
                                              (not (false? (:root? s)))))))
   :deletable          (fn [s] (true? (:deletable? s)))
   :neoreader-syncable (fn [s] (true? (:neoreader-syncable? s)))
   :has-topic          (fn [s] (some? (:topic-id s)))
   :has-history        (fn [s] (some? (:history-scope s)))
   :cross-origin-src   (fn [s] (and (string? (:src s)) (not (same-origin-src? (:src s)))))
   ;; An enable test, never a hide test. A card field cannot count its topic's
   ;; pins, so an unknown count leaves the row enabled: :add-pin re-checks the
   ;; cap in its transaction and toasts a refusal.
   :pin-room           (fn [s] (let [n (:pin-count s)] (or (nil? n) (< n 2))))
   :looping            (fn [s] (true? (:looping? s)))
   :not-looping        (fn [s] (false? (:looping? s)))
   :math-ready         (fn [s] (true? (:math-ready? s)))
   :has-selection      (fn [s] (not (str/blank? (:selection-text s))))})

(def enable-reasons
  "Why an item shows disabled, for each test an `:enable-if` may name."
  {:pin-room "Max 2 pins. Remove one first."})

(defn- holds? [test-key subjects]
  (let [test (get subject-tests test-key)]
    (every? test subjects)))

(defn applicable?
  "True when `entry` can act on `target`.

   Pre : `entry` is a registry entry. `target` is a target map or nil.
   Post: true iff the entry declares the target's kind, the subject count fits
         its arity, and every subject holds every `:target-if` test.
         `:enable-if` does not count here: a disabled item still applies."
  [entry target]
  (let [subjects (:subjects target)]
    (boolean
      (and (some? (:target entry))
           (= (:target entry) (:kind target))
           (seq subjects)
           (or (= :many (:arity entry)) (= 1 (count subjects)))
           (every? #(holds? % subjects) (:target-if entry))))))

(defn disabled-reason
  "The reason `entry` is shown disabled for `target`, or nil when it is enabled.

   Pre : (applicable? entry target).
   Post: nil, or the reason of the first `:enable-if` test a subject fails."
  [entry target]
  (some (fn [k] (when-not (holds? k (:subjects target)) (get enable-reasons k)))
    (:enable-if entry)))

(defn menu-label
  "The text a menu row shows: `:menu-label`, else the palette `:label`."
  [entry]
  (or (:menu-label entry) (:label entry)))

(defn menu-items
  "The menu rows for `target`, in display order.

   Pre : `registry` maps command id → entry.
   Post: a vector of {:id :label :group :danger? :disabled-reason}, one per
         entry that applies to `target`, sorted by the entry's `:menu`
         [group rank]. A change of `:group` between two rows is where the menu
         draws a divider."
  [registry target]
  (->> registry
    (keep (fn [[id entry]]
            (when (applicable? entry target)
              {:id id
               :label (menu-label entry)
               :group (first (:menu entry))
               :rank (second (:menu entry))
               :danger? (true? (:danger? entry))
               :disabled-reason (disabled-reason entry target)})))
    (sort-by (juxt :group :rank :label))
    (mapv #(dissoc % :rank))))

(defn wire-payload
  "The part of `target` a :queue command may send to the server.
   Post: `target` without `:surface`."
  [target]
  (dissoc target :surface))

(def menu-input-types #{:text :number :confirm})

(defn validate-entry
  "Violation strings for one registry entry's target keys; empty = valid."
  [id {:keys [target arity target-if enable-if menu menu-input] :as entry}]
  (let [declares? (some #(contains? entry %)
                    [:arity :target-if :enable-if :menu :menu-input :menu-label])]
    (concat
      (when (and declares? (nil? target))
        [(str id " has target keys but no :target")])
      (when (and (some? target) (not (target-kinds target)))
        [(str id " has unknown :target " target)])
      (when (and (some? arity) (not (arities arity)))
        [(str id " has unknown :arity " arity)])
      (for [k (concat target-if enable-if)
            :when (not (contains? subject-tests k))]
        (str id " names unknown subject test " k))
      (for [k enable-if
            :when (not (contains? enable-reasons k))]
        (str id " :enable-if " k " has no reason in enable-reasons"))
      (when (and (some? target)
                 (not (and (vector? menu) (= 2 (count menu)) (every? number? menu))))
        [(str id " has :target but no [group rank] :menu")])
      (when (and (some? menu-input) (not (menu-input-types (:type menu-input))))
        [(str id " has unknown :menu-input type " (:type menu-input))]))))
