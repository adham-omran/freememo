(ns freememo.anki-reconcile-client
  "Client half of Anki reconcile — the AnkiConnect calls, as Missionary tasks.

   Every function here takes plain data and resolves to plain data, so the
   Electric component that drives them holds no AnkiConnect knowledge and these
   hold no reactive state.

   Phase order, and why it is this order:

     1. `scan!`        bounded notesInfo requests over known notes
     2. (server plans from the digests this produced)
     then, per batch of `anki-reconcile/batch-size` cards:
     3. `write!`       addNote / updateNote for the batch's creates and pushes
     4. `read-back!`   notesInfo again, over ONLY the notes step 3 touched
     5. (server records the batch — note ids and baselines)
     then once:
     6. `finish-io/finish!` commits bounded pull and deletion chunks
     7. (server applies pulls, backfill, conflicts and tombstones)

   Step 4 is the one that looks redundant and is not. Anki normalises HTML on
   write, so a baseline taken from the payload sent in step 3 would disagree
   with what step 1 of the NEXT run reads — the card would report an Anki-side
   change forever, and become a permanent conflict the moment the local side
   moved. It costs one notesInfo over the changed cards only, which is 0 notes
   on a run with nothing to do."
  (:require
   [missionary.core :as m]
   [freememo.anki-reads :as reads]
   [freememo.task-request :as request]
   [freememo.anki-finish-io :as finish-io]
   [freememo.anki-digest :as digest]
   [freememo.anki-sync-helpers :as helpers]
   [freememo.logging :as log]))

(defn- note-digest-entry
  "One notesInfo note reduced to {:note-id :digest :mod}, using the LOCAL card's
   kind to choose which fields count. `kind-by-note` comes from the server's
   manifest; a note the server does not know about is skipped by the caller."
  [note kind-by-note]
  (let [note-id (:noteId note)]
    {:note-id note-id
     :digest  (digest/anki-digest (:fields note) (get kind-by-note note-id))
     :mod     (:mod note)}))

(defn scan-notes!
  "Compute compact scan state and release each validated note batch."
  [call! note-ids kind-by-note]
  (reads/reduce-notes! call!
    (fn [result ids notes]
      (reduce (fn [acc [nid note]]
                (if (:noteId note)
                  (let [entry (note-digest-entry note kind-by-note)]
                    (-> acc
                      (assoc-in [:digests nid] (:digest entry))
                      (assoc-in [:mods nid] (:mod entry))))
                  (update acc :absent conj nid)))
        result (map vector ids notes)))
    {:digests {} :mods {} :absent []} note-ids))

#?(:cljs
   (defn scan! [note-ids kind-by-note]
     (m/sp
       (let [result (m/? (scan-notes! helpers/anki-call! note-ids kind-by-note))]
         (log/log-debug (str "[reconcile] scanned " (count note-ids) " notes"))
         result))))

#?(:cljs
   (defn write!
     "Create and update notes in Anki for one run.

      pre : `bundles` is `anki-write-bundles`' `:bundles` — cards grouped by
            root topic with that root's push settings. `do-anki-push!` splits new
            from existing by `anki_note_id`, which is exactly the `:create` /
            `:push` split the plan made, so the two need not be told apart here.
      post: task of {:pairs [{:card-id :anki-note-id}] :errors [...]}.

      Sequential over bundles (concurrency 1), matching `run-bulk-push!`: each
      bundle may create the same app-owned model, and two concurrent creates of
      one model race in AnkiConnect."
     [bundles]
     (if (empty? bundles)
       (m/sp {:pairs [] :errors []})
       (m/sp
         (let [results (m/? (m/reduce conj []
                              (m/ap
                                (let [{:keys [cards settings]} (m/?> 1 (m/seed bundles))]
                                  (m/? (helpers/do-anki-push! cards settings))))))]
           {:pairs (vec (mapcat :pairs results))
            :errors (vec (mapcat :errors results))})))))

#?(:cljs
   (defn read-back!
     "Digest what Anki now holds for the notes a write touched.

      pre : `pairs` = [{:card-id n :anki-note-id n} …] from `write!`;
            `kind-by-card` maps card id to kind.
      post: task of [{:card-id :note-id :anki-digest :anki-mod} …], carrying only
            notes AnkiConnect reported. A note it does not report was written and
            then removed between the two calls; leaving it out means the card
            keeps no baseline and the next run classifies it afresh."
     [pairs kind-by-card]
     (if (empty? pairs)
       (m/sp [])
       (m/sp
         (let [note-ids (mapv :anki-note-id pairs)
               kind-by-note (into {} (map (juxt :anki-note-id
                                            #(get kind-by-card (:card-id %))))
                              pairs)
               card-by-note (into {} (map (juxt :anki-note-id :card-id)) pairs)
               notes (m/? (reads/read-notes! helpers/anki-call! note-ids))]
           (into []
             (keep (fn [n]
                     (when-let [nid (:noteId n)]
                       (let [{:keys [digest mod]} (note-digest-entry n kind-by-note)]
                         {:card-id (get card-by-note nid)
                          :note-id nid
                          :anki-digest digest
                          :anki-mod mod}))))
             notes))))))


;; ---------------------------------------------------------------------------
;; Drivers — plain fns that run a task and land its result in atoms.
;;
;; Electric bodies call these through `case`, so the reactive graph holds no
;; Missionary and no AnkiConnect. Each is generation-guarded: a second run
;; started before the first finished must not have its predecessor's reply
;; overwrite it.
;; ---------------------------------------------------------------------------

(defonce !run-gen (atom 0))

(defn fail!
  "End the run as failed with `message`. Post: phase `:error`; returns nil."
  [recon message]
  (reset! (:!error recon) message)
  (reset! (:!phase recon) :error)
  nil)

(defn- run-task!
  "Run `task`, landing its result through `on-ok` / `on-err` only while `g` is
   still the current generation, and keep its canceller in `:!task-cancel` so
   an unmounting executor can stop it (`cancel-task!`)."
  [recon task g on-ok on-err]
  #?(:cljs
     (let [cancel (task
                    (fn [v]
                      (when (= g @!run-gen)
                        (reset! (:!task-cancel recon) nil)
                        (on-ok v)))
                    (fn [err]
                      (when (= g @!run-gen)
                        (reset! (:!task-cancel recon) nil)
                        (on-err err))))]
       (reset! (:!task-cancel recon) cancel)
       nil)
     :clj nil))

(defn cancel-task!
  "Stop the in-flight AnkiConnect task, if any. A request already sent can still
   land in Anki; this only stops the ones not sent yet. Post: nil."
  [recon]
  (when-some [cancel @(:!task-cancel recon)]
    (reset! (:!task-cancel recon) nil)
    (cancel))
  nil)

(defn run-scan!
  "Phase 1. Read every known note from Anki and move to `:planning`.
   pre : `targets` is `scan-targets`' result. post (async): !scan set."
  [targets recon]
  #?(:cljs
     (let [g (swap! !run-gen inc)]
       (run-task! recon
         (scan! (:note-ids targets) (:kind-by-note targets))
         g
         (fn [result]
           (reset! (:!scan recon) result)
           (reset! (:!phase recon) :planning))
         (fn [err] (fail! recon (str "Could not read Anki: " (.-message err))))))
     :clj nil))

(defn run-batch!
  "One batch: create and update its notes, then re-read what was written.

   pre : `bundles` from `anki-write-bundles` for this batch only; `kind-by-card`
         from the plan.
   post (async): `:!batch-out` ← {:pairs n :errors [...] :readback [{:card-id
         :note-id :anki-digest :anki-mod} …]}, or `:!error` and phase `:error`.
         The phase stays `:batch`: the caller records the batch from
         `:!batch-out` in the same frame that holds the batch's server values."
  [bundles kind-by-card recon]
  #?(:cljs
     (let [g (swap! !run-gen inc)]
       (run-task! recon
         (m/sp
           (let [writes (m/? (write! bundles))
                 readback (m/? (read-back! (:pairs writes) kind-by-card))]
             {:pairs (count (:pairs writes))
              :errors (:errors writes)
              :readback readback}))
         g
         (fn [out] (reset! (:!batch-out recon) out))
         (fn [err] (fail! recon (str "Anki write failed: " (.-message err)))))
       nil)
     :clj nil))


(defn run-finish-io!
  "Read and commit pulls and deletes through bounded acknowledged requests."
  [plan scan recon]
  #?(:cljs
     (let [g (swap! !run-gen inc)]
       (run-task! recon
         (finish-io/finish! helpers/anki-call!
           #(request/request! (:!io-request recon) %) plan scan)
         g
         (fn [result]
           (if-let [verdict (:verdict result)]
             (if (= :cancel verdict)
               (reset! (:!phase recon) :cancelled)
               (fail! recon "Another tab took over this sync."))
             (do (reset! (:!finish-io recon) result)
                 (reset! (:!finished-note-ids recon) (:finished-note-ids result)))))
         (fn [err] (fail! recon (str "Anki finish failed: " (.-message err))))))
     :clj nil))
