(ns freememo.card-rows
  "Display-row model for the card tables: one row per AUTHORING GROUP, never
   one row per flashcards row.

   An occlusion group fans out to one flashcards row per mask group
   (freememo.occlusion-ordinals) and a score group to one row per direction
   (db/insert-score-group!). Both render as a SINGLE row standing for all of
   their members, so the table shows what the user authored rather than what
   Anki stores.

   Vocabulary, and who owes what:
     :row-key     identity of the row — the SELECTION unit. Callers key
                  selection sets, select-all and DOM ids off this and nothing
                  else.
     :member-ids  the flashcards ids the row stands for — the DELETE, PUSH and
                  PULL unit. Always complete: a row deletes every mask of its
                  group, never the subset that happened to pass a filter.
     :members     the full member rows. SERVER-SIDE ONLY — every projection to
                  the wire MUST drop this key, or the row payload grows by the
                  whole group.

   The three scope keywords match card_versions.scope_type ('card',
   'occlusion_group', 'score_group'): the same three-way split, one layer down.

   Pure and platform-free — no requires, so both peers see plain literals
   (CLAUDE.md, transcribe_language precedent).")

(defn row-key
  "Identity of the display row that `row` belongs to.
   Pre:  row is a flashcard row with :flashcards/id.
   Post: [:occlusion-group id] | [:score-group id] | [:card id]. Two rows of
         one group return `=` keys; rows of different groups never collide,
         because the scope keyword disambiguates equal ids across tables."
  [row]
  (if-let [og (:flashcards/occlusion_group_id row)]
    [:occlusion-group og]
    (if-let [sg (:flashcards/score_group_id row)]
      [:score-group sg]
      [:card (:flashcards/id row)])))

(defn group-row?
  "True when `display-row` stands for more than the one card it looks like."
  [display-row]
  (not= :card (first (:row-key display-row))))

(defn collapse-groups
  "Collapse flashcard rows into display rows, one per row-key.
   Pre:  rows are flashcard rows; a group's members may arrive in any order and
         need not be adjacent.
   Post: one display row per row-key, ordered by each group's FIRST appearance
         in `rows` (so a caller's sort of the input survives). Members are
         ordered by :flashcards/id ascending, which is mask-ordinal order for
         occlusion and directions order for score — both are inserted in that
         order inside one transaction. The display row IS its first member plus
         :row-key, :member-ids and :members, so every existing field reader
         keeps working and reads the representative's value.
   Invariant: (count :member-ids) = the number of Anki notes the row stands for,
         and (apply + (map (comp count :member-ids) result)) = (count rows)."
  [rows]
  (let [keyed (mapv (fn [r] [(row-key r) r]) rows)
        order (into [] (comp (map first) (distinct)) keyed)
        by-key (reduce (fn [m [k r]] (update m k (fnil conj []) r)) {} keyed)]
    (mapv (fn [k]
            (let [members (vec (sort-by :flashcards/id (get by-key k)))]
              (assoc (first members)
                :row-key k
                :member-ids (mapv :flashcards/id members)
                :members members)))
      order)))

(defn card-count
  "How many flashcards the display rows stand for — the Anki-facing number,
   which is not (count rows) once groups collapse."
  [display-rows]
  (transduce (map (comp count :member-ids)) + 0 display-rows))

(defn any-member?
  "True when `pred` holds for any member row. The matching rule for every
   filter that reads a per-card property: a group is shown when any of its
   cards qualifies, and it still deletes and pushes as a whole."
  [pred display-row]
  (boolean (some pred (:members display-row))))

(defn aggregate-state
  "Fold per-member states into one row state.
   Pre:  state-fn maps a member row to a state value.
   Post: the shared state when every member agrees, else :mixed. A row with one
         member always returns that member's own state, so ungrouped cards are
         unaffected by this whole model."
  [state-fn display-row]
  (let [states (into #{} (map state-fn) (:members display-row))]
    (if (= 1 (count states)) (first states) :mixed)))
