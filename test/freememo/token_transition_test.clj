(ns freememo.token-transition-test
  "A token transition must wait for every server value it acts on
   (docs/electric-token-transitions.md).

   The shape this catches: inside a token frame, `(e/on-unmount F)` sits beside
   the call that spends the token, and F reads a value bound from an
   `(e/server …)` form that no enclosing `case`, `if`, `when` or `cond` TEST has
   read. Nothing then makes the spend wait for that value. `(t)` runs, the frame
   unmounts before F can be built, and the transition is lost with no error.
   `9a7545a` fixed the shipped instance: every full Anki sync stopped after its
   plan.

   A static check, read from source with no Electric compile. It follows
   `let`-style bindings and the values derived from them, so a client value
   computed from a server value counts as that server value. It does not follow
   values passed into another `e/defn` as arguments, so a parameter is never
   treated as a server value. That keeps it free of false alarms on the shapes
   this codebase uses, at the cost of missing the defect across a call."
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

;; ---------------------------------------------------------------------------
;; Reading
;; ---------------------------------------------------------------------------

(def ^:private resolver
  "Reads `::kw`, `::alias/kw` and syntax-quote without loading any namespace.
   The check compares symbols by name, so what they resolve to does not matter."
  (reify clojure.lang.LispReader$Resolver
    (currentNS [_] 'user)
    (resolveClass [_ s] s)
    (resolveAlias [_ s] s)
    (resolveVar [_ s] s)))

(defn- read-forms
  "Every top-level form of `text`, CLJS branches of reader conditionals."
  [text]
  (binding [*reader-resolver* resolver
            *read-eval* false
            ;; `#js` and any other tagged literal: keep the form, drop the tag.
            *default-data-reader-fn* (fn [_tag v] v)]
    (let [rdr (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. text))
          opts {:read-cond :allow :features #{:cljs} :eof ::eof}]
      (loop [acc []]
        (let [form (read opts rdr)]
          (if (= ::eof form) acc (recur (conj acc form))))))))

;; ---------------------------------------------------------------------------
;; Symbols and bindings
;; ---------------------------------------------------------------------------

(defn- syms-in
  "Every symbol anywhere in `form`."
  [form]
  (into #{} (filter symbol?) (tree-seq coll? seq form)))

(defn- head= [form nm]
  (and (seq? form) (symbol? (first form)) (= nm (name (first form)))))

(defn- server-form? [form]
  (some #(head= % "server") (tree-seq coll? seq form)))

(defn- token-form? [form]
  (some #(head= % "Token") (tree-seq coll? seq form)))

(defn- pattern-syms
  "The symbols a destructuring pattern binds."
  [pat]
  (cond
    (symbol? pat) #{pat}
    (vector? pat) (into #{} (mapcat pattern-syms) (remove #{'& :as} pat))
    (map? pat) (into #{}
                 (mapcat (fn [[k v]]
                           (cond
                             (#{:keys :syms :strs} k) (map (comp symbol name) v)
                             (= :as k) [v]
                             (keyword? k) []
                             :else (pattern-syms k))))
                 pat)
    :else #{}))

(def ^:private binding-heads
  #{"let" "when-some" "if-some" "when-let" "if-let" "loop" "binding"})

(def ^:private test-heads
  "Conditionals whose first argument is a test that makes the body wait."
  #{"case" "if" "if-not" "when" "when-not" "when-some" "if-some" "when-let"
    "if-let"})

(defn- roots
  "The server bindings `syms` depend on, under `env`. A root is one BINDING,
   not a name: an inner `verdict` that shadows an outer, already tested
   `verdict` is a new root, and the outer test does not gate it. That shadowing
   is exactly the shape the shipped defect had."
  [env syms]
  (into #{} (mapcat #(get-in env [:deps %])) syms))

(defn- bind
  "Extend `env` with one binding pair."
  [env pat init]
  (let [server? (server-form? init)
        deps (when-not server? (roots env (syms-in init)))]
    (reduce (fn [e s]
              (if server?
                (let [root (gensym (str s "__"))]
                  (-> e
                    (assoc-in [:deps s] #{root})
                    (assoc-in [:names root] s)))
                (assoc-in e [:deps s] deps)))
      env (pattern-syms pat))))

(defn- bind-tokens
  "Record the token symbols one binding pair introduces."
  [env pat init]
  (cond-> env
    (token-form? init) (update :tokens into (if (vector? pat)
                                              (pattern-syms (first pat))
                                              #{}))
    (and (symbol? init) (contains? (:tokens env) init))
    (update :tokens into (pattern-syms pat))))

;; ---------------------------------------------------------------------------
;; The walk
;; ---------------------------------------------------------------------------

(defn- spends-token?
  "Whether `form` calls a token symbol in `env`, at any depth. A sibling
   `(case (e/server …) (t))` spends the token as surely as a bare `(t)`."
  [env form]
  (some #(and (seq? %) (symbol? (first %)) (contains? (:tokens env) (first %)))
    (tree-seq coll? seq form)))

(defn- unmount-closure [form]
  (when (head= form "on-unmount") (second form)))

(defn- check-siblings
  "Violations among the direct children of one body: an `on-unmount` whose
   closure reads an ungated server value, beside a token spend."
  [env children]
  (when (some #(and (not (unmount-closure %)) (spends-token? env %)) children)
    (for [child children
          :let [f (unmount-closure child)]
          :when f
          :let [ungated (set/difference (roots env (syms-in f)) (:gated env))]
          :when (seq ungated)]
      {:closure f :ungated (into #{} (map #(get-in env [:names %])) ungated)})))

(defn- walk
  "All violations in `form` under `env`."
  [env form]
  (cond
    (not (coll? form)) nil

    (map? form) (mapcat #(walk env %) (concat (keys form) (vals form)))
    (or (vector? form) (set? form)) (mapcat #(walk env %) form)

    (not (seq? form)) nil

    (and (symbol? (first form)) (binding-heads (name (first form)))
         (vector? (second form)))
    (let [[_ bvec & body] form
          pairs (partition 2 bvec)
          ;; Each init is walked under the bindings before it.
          [env' init-hits]
          (reduce (fn [[e hits] [pat init]]
                    [(-> e (bind-tokens pat init) (bind pat init))
                     (concat hits (walk e init))])
            [env nil] pairs)
          ;; A `when-some`-style test gates what it binds.
          env' (if (test-heads (name (first form)))
                 (update env' :gated into (roots env' (mapcat pattern-syms (map first pairs))))
                 env')]
      (concat init-hits
        (check-siblings env' body)
        (mapcat #(walk env' %) body)))

    (and (symbol? (first form)) (test-heads (name (first form))))
    (let [[_ test & body] form
          env' (update env :gated into (roots env (syms-in test)))]
      (concat (walk env test)
        (check-siblings env' body)
        (mapcat #(walk env' %) body)))

    (head= form "cond")
    (let [clauses (partition 2 (rest form))]
      ;; Each branch is gated by its own test and by every test before it.
      (first
        (reduce (fn [[hits e] [test branch]]
                  (let [e' (update e :gated into (roots e (syms-in test)))]
                    [(concat hits (walk e test) (check-siblings e' [branch]) (walk e' branch))
                     e']))
          [nil env] clauses)))

    :else
    (concat (check-siblings env (rest form))
      (mapcat #(walk env %) form))))

(def ^:private empty-env {:deps {} :names {} :gated #{} :tokens #{}})

(defn violations
  "Every violation in source `text`, as `{:def name :ungated #{sym}}`."
  [text]
  (for [form (read-forms text)
        hit (walk empty-env form)]
    {:def (when (seq? form) (second form))
     :ungated (:ungated hit)}))

;; ---------------------------------------------------------------------------
;; The checker catches the shape, and passes its correct form
;; ---------------------------------------------------------------------------

(def ^:private shipped-defect
  "The last branch of `ReconcilePlan` before `9a7545a`, reduced."
  "(e/defn ReconcilePlan [user-id recon]
     (let [[t _] (e/Token :reconcile-plan)]
       (when t
         (let [p (e/server (plan* user-id))
               total (count (:write-card-ids p))
               verdict (e/server (report-progress!* user-id total))]
           (case (reset! (:!plan recon) p)
             (do (e/on-unmount
                   (fn []
                     (cond
                       (not= :continue verdict) (halt! recon verdict)
                       (pos? total) (reset! (:!phase recon) :batch))))
                 (t)))))))")

(def ^:private shipped-fix
  "The same branch after `9a7545a`."
  "(e/defn ReconcilePlan [user-id recon]
     (let [[t _] (e/Token :reconcile-plan)]
       (when t
         (let [p (e/server (plan* user-id))
               total (count (:write-card-ids p))
               verdict (e/server (report-progress!* user-id total))]
           (case (reset! (:!plan recon) p)
             (if (not= :continue verdict)
               (do (e/on-unmount #(halt! recon verdict))
                   (t))
               (do (e/on-unmount #(reset! (:!phase recon) (if (pos? total) :batch :finishing)))
                   (t))))))))")

(deftest the-check-catches-the-shipped-defect
  (testing "the verdict was read only inside the unmount closure"
    (is (= [{:def 'ReconcilePlan :ungated #{'verdict}}]
          (vec (violations shipped-defect)))))
  (testing "an outer test of a SHADOWED name does not gate the inner binding,
            which is how the shipped file read"
    (is (= [{:def 'ReconcilePlan :ungated #{'verdict}}]
          (vec (violations
                 "(e/defn ReconcilePlan [user-id recon]
                    (let [[t _] (e/Token :reconcile-plan)]
                      (when t
                        (let [verdict (e/server (check-in!))]
                          (if (not= :continue verdict)
                            (do (e/on-unmount #(halt! recon verdict)) (t))
                            (let [verdict (e/server (report!))]
                              (case (reset! (:!plan recon) 1)
                                (do (e/on-unmount #(halt! recon verdict))
                                    (t)))))))))")))))
  (testing "an if on the verdict before the spend makes it wait"
    (is (empty? (violations shipped-fix)))))

(deftest a-client-value-is-never-flagged
  (is (empty? (violations
                "(e/defn Close [!open]
                   (let [[t _] (e/Token :x) n (inc 1)]
                     (when t (e/on-unmount #(reset! !open n)) (t))))"))))

(deftest a-value-derived-from-a-server-value-counts-as-one
  (is (= #{'r}
        (:ungated (first (violations
                           "(e/defn Save [!s]
                              (let [[t _] (e/Token :x)]
                                (when t
                                  (let [r (e/server (save!))
                                        ok? (:ok r)]
                                    (do (e/on-unmount #(reset! !s ok?)) (t))))))"))))))

;; ---------------------------------------------------------------------------
;; The source tree holds none
;; ---------------------------------------------------------------------------

(defn- cljc-files []
  (->> (file-seq (io/file "src"))
    (filter #(str/ends-with? (.getName ^java.io.File %) ".cljc"))
    (sort-by str)))

(deftest no-token-transition-waits-on-nothing
  (doseq [f (cljc-files)
          v (violations (slurp f))]
    (is false
      (str (.getPath ^java.io.File f) " " (:def v)
        ": an e/on-unmount beside a token spend reads " (sort (:ungated v))
        ", and no enclosing case/if/when/cond test reads it first."
        " See docs/electric-token-transitions.md."))))
