(ns freememo.anki-sync-helpers
  "Plain helper functions for Anki sync — no e/defn, no reactive frame slots."
  (:require
   [missionary.core :as m]
   [clojure.string :as str]
   [freememo.logging :as log]))

;; ---------------------------------------------------------------------------
;; Client-side AnkiConnect wrapper
;; ---------------------------------------------------------------------------

(defn prepend-header
  "Prepend optional HTML header to note front text.
   Tagged with fm-header class so pull can strip it without contaminating the question."
  [front use-header header-text]
  (let [base (or front "")]
    (if (and use-header (not (str/blank? header-text)))
      (str "<p class=\"fm-header\">" header-text "</p>" base)
      base)))

(defn wrap-p
  "Wrap text in <p> tags if not already wrapped in HTML block tags."
  [text]
  (if (or (str/blank? text)
        (str/starts-with? (str/trim text) "<"))
    text
    (str "<p>" text "</p>")))

(defn strip-html
  "Remove all HTML tags and push-time decorations from Anki field text.
   Strips: fm-header tagged headers, source/bibliography suffix (<hr>...),
   then all remaining HTML tags."
  [text]
  (if (str/blank? text)
    text
    (-> text
      (str/replace #"<p[^>]*class=\"fm-header\"[^>]*>.*?</p>" "") ;; strip tagged header
      (str/replace #"\n?<hr[^>]*class=\"fm-(?:source|bibliography)\"[^>]*>[\s\S]*$" "") ;; strip tagged source/bib block
      (str/replace #"<br\s*/?>" "\n") ;; preserve line breaks
      (str/replace #"<[^>]*>" "") ;; strip all remaining tags
      str/trim)))

(defn append-source
  "Append source reference HTML to card content when source-display-mode is 'append'."
  [content source-ref]
  (if (and (not (str/blank? source-ref)))
    (str content "\n<hr class=\"fm-source\"><small style='color:#999'>Source: " source-ref "</small>")
    content))

(defn wrap-fm-source
  "Wrap source-anchor HTML in <p class=\"fm-source\"> for separate-field emission. No-op on blank input."
  [source-html]
  (if (str/blank? source-html)
    source-html
    (str "<p class=\"fm-source\">" source-html "</p>")))

(defn wrap-fm-bibliography
  "Wrap bibliography body HTML in <p class=\"fm-bibliography\"> for separate-field emission. No-op on blank input."
  [bib-html]
  (if (str/blank? bib-html)
    bib-html
    (str "<p class=\"fm-bibliography\">" bib-html "</p>")))

(declare html-escape)

(defn format-bibliography-html
  "Render a CSL map as the bibliography body HTML — title link + author + year.
   Returns nil for a blank citation. No presentation wrapper, no
   \"Bibliography: \" prefix — callers add those when appending. Mirrors the
   source/append-source split: field mode writes the raw body, append mode
   wraps it. Server-side only — title/author/URL are HTML-escaped here, so
   callers must NOT double-escape."
  [csl]
  #?(:clj
     (when (map? csl)
       (let [title (some-> (:title csl) str/trim not-empty)
             url (some-> (:URL csl) str/trim not-empty)
             first-auth (first (:author csl))
             author (or (some-> (:family first-auth) str/trim not-empty)
                      (some-> (:given first-auth) str/trim not-empty))
             year (some-> csl :issued :date-parts first first)
             title-html (when title
                          (if url
                            (str "<a href=\"" (html-escape url) "\">"
                              (html-escape title) "</a>")
                            (html-escape title)))
             author-html (when author (html-escape author))
             year-html (when year (str (when author " ") "(" year ")"))
             dash (when (and title-html (or author-html year-html)) " — ")
             body (str (or title-html "")
                    (or dash "")
                    (or author-html "")
                    (or year-html ""))]
         (when (seq body) body)))
     :cljs nil))

(defn append-bibliography
  "Append bibliography body to card content when bibliography-display-mode is
   'append'. Wraps with the fm-bibliography <hr> divider and the
   <small>Bibliography: …</small> presentation. Pull-side strip-html removes
   this same wrapper."
  [content bib-body]
  (if (str/blank? bib-body)
    content
    (str content "\n<hr class=\"fm-bibliography\"><small style='color:#999'>Bibliography: " bib-body "</small>")))

#?(:cljs
   (defn to-future! [token task]
     (js/Promise.
       (fn [resolve reject]
         (.finally token (task resolve reject))))))

#?(:cljs
   (defn from-future
     ([f]
      (fn [success failure]
        (let [box (volatile! nil)
              token (js/Promise.
                      (fn [resolve _]
                        (vreset! box resolve)))
              resolve @box]
          (.then (f token)
            (fn [x]
              (resolve nil)
              (success x))
            (fn [e]
              (resolve nil)
              (failure e)))
          #(resolve nil))))
     ([f & args]
      (from-future #(apply f % args)))))

#?(:cljs
   (defn anki-call!
     "Call AnkiConnect API. Returns a Promise resolving to the result value."
     [action params]
     (from-future
       (fn [token]
         (let [ctrl (js/AbortController.)]
           (.finally token #(.abort ctrl))
           (-> (js/fetch "http://127.0.0.1:8765"
                 (clj->js {:method "POST"
                           :headers {"Content-Type" "application/json"}
                           :signal (.-signal ctrl)
                           :body (js/JSON.stringify
                                   (clj->js (cond-> {:action action :version 6}
                                              params (assoc :params params))))}))
             (.then (fn [resp]
                      (when-not (.-ok resp)
                        (throw (js/Error. (str "HTTP " (.-status resp)))))
                      (.json resp)))
             (.then (fn [json]
                      (let [result (js->clj json :keywordize-keys true)]
                        (when (:error result)
                          (throw (js/Error. (str (:error result)))))
                        (:result result))))))))))

#?(:cljs
   (defn mime->ext
     "Derive a file extension from a MIME type string (client-side, mirrors server)."
     [mime]
     (case (str/lower-case (or mime ""))
       "image/png" "png"
       "image/jpeg" "jpg"
       "image/jpg" "jpg"
       "image/gif" "gif"
       "image/webp" "webp"
       "image/svg+xml" "svg"
       "image/bmp" "bmp"
       "image/tiff" "tiff"
       "bin")))

#?(:cljs
   (defn fetch-and-store-media!
     "Fetch bytes for a media item from the local app server (/api/media/<id>),
      derive the filename from the response Content-Type header, call AnkiConnect
      storeMediaFile, and return the filename string (e.g. \"42.png\").
      Concurrency-safe: each call is independent; dedup is handled by the caller."
     [media-id]
     (from-future
       (fn [token]
         (let [ctrl (js/AbortController.)
               filename-atom (atom nil)]
           (.finally token #(.abort ctrl))
           (-> (js/fetch (str "/api/media/" media-id)
                 (clj->js {:signal (.-signal ctrl)}))
             (.then (fn [resp]
                      (when-not (.-ok resp)
                        (throw (js/Error. (str "HTTP " (.-status resp)
                                            " fetching /api/media/" media-id))))
                      (let [ct (.get (.-headers resp) "content-type")
                            ext (mime->ext (when ct (first (str/split ct #";"))))
                            fn (str media-id "." ext)]
                        (reset! filename-atom fn)
                        (.arrayBuffer resp))))
             (.then (fn [buf]
                      ;; Convert ArrayBuffer to base64. Chunked to dodge
                      ;; "Maximum call stack" on large files; each chunk uses
                      ;; String.fromCharCode.apply(null, slice) — applying the
                      ;; byte array as arguments to fromCharCode (NOT to String).
                      (let [bytes (js/Uint8Array. buf)
                            byte-len (.-length bytes)
                            chunks (js/Array.)
                            chunk-sz 8192]
                        (doseq [i (range 0 byte-len chunk-sz)]
                          (let [slice (.slice bytes i (+ i chunk-sz))]
                            (.push chunks (.apply (.-fromCharCode js/String) nil slice))))
                        (let [b64 (.btoa js/window (.join chunks ""))]
                          ;; anki-call! returns a missionary task fn (2-arity),
                          ;; NOT a Promise. Wrap it so .then can await the upload.
                          (js/Promise.
                            (fn [resolve reject]
                              (let [task (anki-call! "storeMediaFile"
                                           {:filename @filename-atom :data b64})]
                                (task resolve reject))))))))
             (.then (fn [_] @filename-atom))))))))

#?(:cljs
   (defn fetch-anki-config! []
     (m/join (fn [decks models tags]
               {:decks (vec (js->clj decks))
                :models (vec (js->clj models))
                :tags (vec (js->clj tags))})
       (anki-call! "deckNames" nil)
       (anki-call! "modelNames" nil)
       (anki-call! "getTags" nil))))

;; ---------------------------------------------------------------------------
;; Media image handling for Anki push
;; ---------------------------------------------------------------------------

(defn extract-media-ids
  "Return a set of media ID strings found in /api/media/<id> src attributes
   within the given HTML string. Works on both CLJ and CLJS (no numeric parsing)."
  [html]
  (when-not (str/blank? html)
    (->> (re-seq #"/api/media/(\d+)" html)
      (map second)
      set)))

(defn rewrite-media-srcs
  "Replace every /api/media/<id> occurrence in html with the filename
   from filename-map (a map of id-string -> filename string).
   IDs not present in filename-map are left unchanged."
  [html filename-map]
  (if (or (str/blank? html) (empty? filename-map))
    html
    (str/replace html #"/api/media/(\d+)"
      (fn [[match id-str]]
        (or (get filename-map id-str) match)))))

(defn collect-card-media-ids
  "Return a set of all media ID strings referenced in any HTML field of a card."
  [card]
  (reduce into #{}
    (keep extract-media-ids
      [(:flashcards/question card)
       (:flashcards/answer card)
       (:flashcards/cloze card)])))

(defn extract-img-tags
  "Return a string of all <img ...> tags extracted from html, preserving order.
   Used in field-mode to move images into a dedicated Anki field."
  [html]
  (if (str/blank? html)
    ""
    (let [matches (re-seq #"<img[^>]*/?>|<img[^>]*></img>" html)]
      (str/join "" matches))))

(defn remove-img-tags
  "Remove all <img ...> tags from html, returning the text-only remainder."
  [html]
  (if (str/blank? html)
    html
    (-> html
      (str/replace #"<img[^>]*/?>|<img[^>]*></img>" "")
      str/trim)))

(def ^:private default-app-base-url
  "Fallback when the caller's settings map omits :app-base-url. Hosted default;
   self-host supplies the value via `freememo.settings/app-base-url` (env-driven)."
  "https://freememo.net")

(defn html-escape
  "Escape HTML-special characters so titles/URLs can be embedded safely."
  [s]
  (-> (str s)
    (str/replace "&" "&amp;")
    (str/replace "<" "&lt;")
    (str/replace ">" "&gt;")
    (str/replace "\"" "&quot;")
    (str/replace "'" "&#39;")))

(defn build-source-anchor
  "Build an HTML anchor linking back to the source item in FreeMemo.
   Links unconditionally to <app-base-url>/viewer/topic/<topic-id>.
   Anchor text is '<title> - <page>' when the card has a :page_number, else '<title>'.
   Returns nil when title is blank.
   Pre: caller supplies `:app-base-url` in `settings`; falls back to default if absent."
  [card settings]
  (let [{:keys [topic-kind root-topic-id topic-title app-base-url]} settings
        base (or app-base-url default-app-base-url)
        title topic-title
        page (:page_number card)
        url (str base "/viewer/topic/" (:flashcards/topic_id card))
        anchor-text (if page
                      (str title " - " page)
                      title)]
    (when-not (str/blank? title)
      (str "<a href=\"" (html-escape url) "\">" (html-escape anchor-text) "</a>"))))

#?(:cljs
   (defn build-note
     "Build an AnkiConnect note map for addNote.
      settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
                  :allow-dupes :use-header :header-text :tags :source-display-mode
                  :bibliography-display-mode :bibliography-field-name :bibliography-html
                  :images-front-field :images-back-field :image-display-mode}
      filename-map = {media-id -> \"<id>.<ext>\"} — pre-uploaded filenames for src rewrite."
     [card settings filename-map]
     (let [{:keys [deck basic-model cloze-model basic-fields cloze-fields
                   allow-dupes use-header header-text tags source-display-mode
                   bibliography-display-mode bibliography-html
                   images-front-field images-back-field image-display-mode]} settings
           kind (:flashcards/kind card)
           basic? (= kind "basic")
           model (if basic? basic-model cloze-model)
           fields (if basic? basic-fields cloze-fields)
           source-ref (build-source-anchor card settings)
           append-source? (and (= source-display-mode "append") (not (str/blank? source-ref)))
           field-source? (and (= source-display-mode "field") (not (str/blank? source-ref)))
           append-bib? (and (= bibliography-display-mode "append") (not (str/blank? bibliography-html)))
           field-bib? (and (= bibliography-display-mode "field") (not (str/blank? bibliography-html)))
           src-field-key (or (:source-field settings) "Source")
           bib-field-name (or (:bibliography-field-name settings) "Bibliography")
           same-field? (and field-source? field-bib?
                         (= (str/lower-case (str/trim src-field-key))
                           (str/lower-case (str/trim bib-field-name))))
           field-mode? (= image-display-mode "field")
           ;; Rewrite /api/media/<id> → filename in all field HTML
           raw-front (wrap-p (or (:flashcards/question card) ""))
           raw-back (wrap-p (or (:flashcards/answer card) ""))
           raw-cloze (or (:flashcards/cloze card) "")
           rw-front (rewrite-media-srcs raw-front filename-map)
           rw-back (rewrite-media-srcs raw-back filename-map)
           rw-cloze (rewrite-media-srcs raw-cloze filename-map)
           ;; Basic field map (with optional header, source, bibliography, and field-mode image split)
           field-map
           (if basic?
             (let [front-html (prepend-header rw-front use-header header-text)
                   ;; Source-first, Bibliography-second append order (footnote convention).
                   back-html (cond-> rw-back
                               append-source? (append-source source-ref)
                               append-bib? (append-bibliography bibliography-html))
                   ;; Field mode: extract <img> from front/back, put in dedicated fields
                   main-front (if field-mode? (remove-img-tags front-html) front-html)
                   main-back (if field-mode? (remove-img-tags back-html) back-html)
                   imgs-front (when field-mode? (extract-img-tags front-html))
                   imgs-back (when field-mode? (extract-img-tags back-html))
                   src-field-html (wrap-p source-ref)]
               (cond-> {(first fields) main-front
                        (second fields) main-back}
                 (and field-source? (not same-field?)) (assoc src-field-key (wrap-fm-source source-ref))
                 (and field-bib? (not same-field?)) (assoc bib-field-name (wrap-fm-bibliography bibliography-html))
                 same-field? (assoc src-field-key
                               (append-bibliography src-field-html bibliography-html))
                 (and field-mode? (seq imgs-front) (seq images-front-field))
                 (assoc images-front-field imgs-front)
                 (and field-mode? (seq imgs-back) (seq images-back-field))
                 (assoc images-back-field imgs-back)))
             ;; Cloze — F2-uniform: all <img> stay in the Text (cloze) field
             (let [cloze-html (cond-> (prepend-header rw-cloze use-header header-text)
                                append-source? (append-source source-ref)
                                append-bib? (append-bibliography bibliography-html))]
               (cond-> {(first fields) cloze-html}
                 (and field-source? (not same-field?)) (assoc src-field-key (wrap-fm-source source-ref))
                 (and field-bib? (not same-field?)) (assoc bib-field-name (wrap-fm-bibliography bibliography-html))
                 same-field? (assoc src-field-key
                               (append-bibliography source-ref bibliography-html)))))]
       (cond-> {:deckName deck
                :modelName model
                :fields field-map
                :tags tags}
         (not allow-dupes) (assoc :options {:allowDuplicate false
                                            :duplicateScope "deck"})
         allow-dupes (assoc :options {:allowDuplicate true})))))

#?(:cljs
   (defn build-update-fields
     "Build Anki field map for updating an existing note.
      filename-map = {media-id -> \"<id>.<ext>\"} — pre-uploaded filenames for src rewrite."
     [card settings filename-map]
     (let [{:keys [basic-fields cloze-fields use-header header-text source-display-mode
                   bibliography-display-mode bibliography-html
                   images-front-field images-back-field image-display-mode]} settings
           kind (:flashcards/kind card)
           basic? (= kind "basic")
           fields (if basic? basic-fields cloze-fields)
           source-ref (build-source-anchor card settings)
           append-src? (and (= source-display-mode "append") (not (str/blank? source-ref)))
           field-src? (and (= source-display-mode "field") (not (str/blank? source-ref)))
           append-bib? (and (= bibliography-display-mode "append") (not (str/blank? bibliography-html)))
           field-bib? (and (= bibliography-display-mode "field") (not (str/blank? bibliography-html)))
           src-field-key (or (:source-field settings) "Source")
           bib-field-name (or (:bibliography-field-name settings) "Bibliography")
           same-field? (and field-src? field-bib?
                         (= (str/lower-case (str/trim src-field-key))
                           (str/lower-case (str/trim bib-field-name))))
           field-mode? (= image-display-mode "field")
           rw-front (rewrite-media-srcs (wrap-p (or (:flashcards/question card) "")) filename-map)
           rw-back (rewrite-media-srcs (wrap-p (or (:flashcards/answer card) "")) filename-map)
           rw-cloze (rewrite-media-srcs (or (:flashcards/cloze card) "") filename-map)]
       (if basic?
         (let [front-html (prepend-header rw-front use-header header-text)
               back-html (cond-> rw-back
                           append-src? (append-source source-ref)
                           append-bib? (append-bibliography bibliography-html))
               main-front (if field-mode? (remove-img-tags front-html) front-html)
               main-back (if field-mode? (remove-img-tags back-html) back-html)
               imgs-front (when field-mode? (extract-img-tags front-html))
               imgs-back (when field-mode? (extract-img-tags back-html))
               src-field-html (wrap-p source-ref)]
           (cond-> {(first fields) main-front
                    (second fields) main-back}
             (and field-src? (not same-field?)) (assoc src-field-key (wrap-fm-source source-ref))
             (and field-bib? (not same-field?)) (assoc bib-field-name (wrap-fm-bibliography bibliography-html))
             same-field? (assoc src-field-key
                           (append-bibliography src-field-html bibliography-html))
             (and field-mode? (seq imgs-front) (seq images-front-field))
             (assoc images-front-field imgs-front)
             (and field-mode? (seq imgs-back) (seq images-back-field))
             (assoc images-back-field imgs-back)))
         (let [cloze-html (cond-> (prepend-header rw-cloze use-header header-text)
                            append-src? (append-source source-ref)
                            append-bib? (append-bibliography bibliography-html))]
           (cond-> {(first fields) cloze-html}
             (and field-src? (not same-field?)) (assoc src-field-key (wrap-fm-source source-ref))
             (and field-bib? (not same-field?)) (assoc bib-field-name (wrap-fm-bibliography bibliography-html))
             same-field? (assoc src-field-key
                           (append-bibliography source-ref bibliography-html))))))))

#?(:cljs
   (defn do-anki-push!
     "Push cards to Anki. Returns Missionary task resolving to result map.
      settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
                  :allow-dupes :use-header :header-text :tags
                  :images-front-field :images-back-field :image-display-mode}"
     [cards settings]
     (let [new-cards (vec (filter #(nil? (:flashcards/anki_note_id %)) cards))
           ;; Re-push every previously-synced card on every push so changes to
           ;; header text / use-header / source mode / tags propagate even when
           ;; card body content was not edited. Anki-side timestamp checks
           ;; can't see these settings, so optimizing here would silently drop
           ;; legitimate updates.
           changed-cards (vec (filter #(some? (:flashcards/anki_note_id %)) cards))]
       (log/log-debug (str "[anki-push] do-anki-push! starting"
                        " new=" (count new-cards)
                        " update=" (count changed-cards)
                        " mode=" (:source-display-mode settings)
                        " image-display-mode=" (:image-display-mode settings)
                        " topic-title=" (pr-str (:topic-title settings))
                        " topic-kind=" (pr-str (:topic-kind settings))))
       (m/sp
         ;; Phase 0: collect all unique media IDs across every card, upload each once,
         ;; build filename-map {id -> "<id>.<ext>"} for src rewriting.
         (let [all-media-ids (reduce (fn [acc card] (into acc (collect-card-media-ids card)))
                               #{}
                               (concat new-cards changed-cards))
               filename-map
               (m/? (m/reduce
                      (fn [acc [id filename]] (assoc acc id filename))
                      {}
                      (m/ap
                        (let [media-id (m/?> 3 (m/seed (vec all-media-ids)))]
                          (try
                            (let [filename (m/? (fetch-and-store-media! media-id))]
                              [media-id filename])
                            (catch :default err
                              (log/log-error (str "[anki-push] storeMediaFile FAILED id=" media-id
                                               " error=" (.-message err)))
                              [media-id (str "/api/media/" media-id)]))))))
               ;; Phase 1: add new cards (sequential, concurrency 1)
               add-result
               (m/? (m/reduce
                      (fn [acc item] (merge-with into acc item))
                      {:pairs [] :skipped [] :errors []}
                      (m/ap
                        (let [card (m/?> 1 (m/seed new-cards))]
                          (try
                            (let [note (build-note card settings filename-map)
                                  note-id (m/? (anki-call! "addNote" {:note note}))]
                              (if note-id
                                {:pairs [{:card-id (:flashcards/id card) :anki-note-id note-id}]}
                                {:skipped [{:card-id (:flashcards/id card) :reason "No note ID returned"}]}))
                            (catch :default err
                              {:errors [{:card-id (:flashcards/id card) :error (.-message err)}]}))))))
               ;; Phase 2: update changed cards (concurrency 10)
               update-result
               (m/? (m/reduce
                      (fn [acc item] (merge-with into acc item))
                      {:pairs [] :updated [] :errors []}
                      (m/ap
                        (let [card (m/?> 10 (m/seed changed-cards))]
                          (try
                            (let [field-map (build-update-fields card settings filename-map)]
                              (log/log-debug (str "[anki-push] updateNote card-id=" (:flashcards/id card)
                                               " note-id=" (:flashcards/anki_note_id card)
                                               " field-keys=" (vec (keys field-map))
                                               " field-map=" (pr-str field-map)))
                              (m/? (anki-call! "updateNote"
                                     {:note {:id (:flashcards/anki_note_id card)
                                             :fields field-map
                                             :tags (:tags settings)}}))
                              {:pairs [{:card-id (:flashcards/id card)
                                        :anki-note-id (:flashcards/anki_note_id card)}]
                               :updated [1]})
                            (catch :default err
                              (log/log-error (str "[anki-push] updateNote FAILED card-id=" (:flashcards/id card)
                                               " error=" (.-message err)))
                              {:errors [{:card-id (:flashcards/id card) :error (.-message err)}]}))))))]
           ;; Merge both phases
           (-> (merge-with into add-result update-result)
             (assoc :updated (count (:updated update-result)))))))))

(defn ordered-field-values
  "Field values of an Anki notesInfo :fields map, sorted by Anki's :order.
   Shared by pull (field-update detection) and the library cards Anki
   overlay (anki-modified diff) — one definition of 'Anki's field order'."
  [fields-map]
  (->> fields-map
    (sort-by (fn [[_ v]] (or (:order v) 0)))
    (mapv (fn [[_ v]] (:value v)))))

#?(:cljs
   (defn do-anki-pull!
     "Pull edits from Anki for previously-synced cards. Returns Missionary task of updates + deleted IDs.

      Field selection is derived from Anki's notesInfo :order — basic cards read
      the first two fields by order (question, answer); cloze cards read the
      first. This matches Anki's own field ordering and removes any modal-side
      configuration dependency."
     [cards]
     (let [synced (filter #(some? (:flashcards/anki_note_id %)) cards)
           note-ids (mapv :flashcards/anki_note_id synced)]
       (if (empty? note-ids)
         (m/sp {:updates [] :deleted []})
         (m/sp
           (let [notes (m/? (anki-call! "notesInfo" {:notes note-ids}))
                 notes-js (js->clj notes)
                 pairs (map vector synced notes-js)
                 deleted (reduce
                           (fn [acc [card anki-note]]
                             (let [anki-note (if (map? anki-note) anki-note
                                               (js->clj anki-note :keywordize-keys true))]
                               (if (or (empty? anki-note) (nil? (:noteId anki-note)))
                                 (conj acc (:flashcards/id card))
                                 acc)))
                           []
                           pairs)
                 id->card (into {} (keep (fn [[card anki-note]]
                                           (let [anki-note (if (map? anki-note) anki-note
                                                             (js->clj anki-note :keywordize-keys true))]
                                             (when-let [nid (:noteId anki-note)]
                                               [nid card])))
                                     pairs))
                 updates (reduce
                           (fn [acc anki-note]
                             (let [anki-note (if (map? anki-note) anki-note
                                               (js->clj anki-note :keywordize-keys true))
                                   note-id (:noteId anki-note)
                                   card (get id->card note-id)]
                               (if-not card
                                 acc
                                 (let [kind (:flashcards/kind card)
                                       basic? (= kind "basic")
                                       ordered-vals (ordered-field-values (:fields anki-note))
                                       update-map
                                       (if basic?
                                         (let [q (strip-html (first ordered-vals))
                                               a (strip-html (second ordered-vals))
                                               local-q (or (:flashcards/question card) "")
                                               local-a (or (:flashcards/answer card) "")]
                                           (when (or (not= q local-q) (not= a local-a))
                                             {:card-id (:flashcards/id card)
                                              :question q :answer a}))
                                         (let [c (strip-html (first ordered-vals))
                                               local-c (or (:flashcards/cloze card) "")]
                                           (when (not= c local-c)
                                             {:card-id (:flashcards/id card)
                                              :cloze c})))]
                                   (if update-map (conj acc update-map) acc)))))
                           []
                           notes-js)]
             {:updates updates :deleted deleted}))))))

;; ---------------------------------------------------------------------------
;; Cross-platform wrappers — avoid #?(:cljs) inside e/defn bodies.
;; Top-level defn with #?(:cljs/:clj) body is safe (no frame slot impact).
;; ---------------------------------------------------------------------------


(defn run-fetch-config! [conn]
  (let [{:keys [!status !error !decks !models !selected-deck !basic-model !cloze-model !all-tags]} conn]
    #?(:cljs
       (do ((fetch-anki-config!)
            (fn [{:keys [decks models tags]}]
              (reset! !decks decks)
              (reset! !models models)
              (reset! !all-tags tags)
              (when (seq decks) (reset! !selected-deck (first decks)))
              (when (seq models)
                (reset! !basic-model (first models))
                (reset! !cloze-model (first models)))
              (reset! !status :connected))
            (fn [err]
              (reset! !error (str "Cannot connect to Anki: " (.-message err)))
              (reset! !status :error)))
         nil)
       :clj nil)))


(defn run-fetch-fields!
  "Fetch model field names from Anki and store in atom.
   Atom values: :loading while in flight, vector of field names on success, [] on error.

   `preferred-fields` (optional) — vector of user-saved field ordering. When
   every preferred name exists in the model's fetched fields, the atom is set
   to `preferred-fields` instead of the fetched order; otherwise fetched wins.
   Resolved once at modal open from per-doc preset → user-level setting; see
   anki_sync_server/resolve-preferred-fields."
  ([model-name !fields-atom]
   (run-fetch-fields! model-name !fields-atom nil))
  ([model-name !fields-atom preferred-fields]
   #?(:cljs
      (when model-name
        (reset! !fields-atom :loading)
        (do ((anki-call! "modelFieldNames" {:modelName model-name})
             (fn [fields]
               (let [fetched (vec (js->clj fields))
                     pref (vec (or preferred-fields []))
                     valid? (and (seq pref)
                              (every? (fn [f] (some #{f} fetched)) pref))]
                 (reset! !fields-atom (if valid? pref fetched))))
             (fn [_] (reset! !fields-atom [])))
          nil))
      :clj nil)))

(defn run-fetch-models!
  "Fetch the list of model names from AnkiConnect and update status/models atoms.
   Used by the Settings page field-defaults UI to check AnkiConnect reachability
   and populate the model pickers. Sets :connecting → :connected | :error."
  [!status !models !error]
  #?(:cljs
     (do (reset! !status :connecting)
       (reset! !error nil)
       ((anki-call! "modelNames" nil)
        (fn [models]
          (reset! !models (vec (js->clj models)))
          (reset! !status :connected))
        (fn [err]
          (reset! !error (.-message err))
          (reset! !status :error)))
       nil)
     :clj nil))

(defn run-push!
  "Execute push to Anki and update state atoms with results.
   Always transitions to :recording — finalize-push! must run even when
   there are no new pairs, so settings/per-item preset still persist.
   settings = {:deck :basic-model :cloze-model :basic-fields :cloze-fields
               :allow-dupes :use-header :header-text :tags}
   sync     = {:!phase :!result :!error :!push-pairs}"
  [cards settings sync]
  (let [{:keys [!phase !result !error !push-pairs]} sync]
    #?(:cljs
       (do ((do-anki-push! cards settings)
            (fn [result]
              (let [pairs (or (:pairs result) [])
                    updated (or (:updated result) 0)
                    added (max 0 (- (count pairs) updated))
                    skipped (count (or (:skipped result) []))
                    errors (count (or (:errors result) []))]
                (log/log-debug (str "[anki-push] complete added=" added
                                 " updated=" updated
                                 " skipped=" skipped
                                 " errors=" errors)))
              (reset! !result result)
              (reset! !push-pairs (vec (:pairs result)))
              (reset! !phase :recording))
            (fn [err]
              (reset! !error (.-message err))
              (reset! !phase :error)))
         nil)
       :clj nil)))

(defn run-pull!
  "Execute pull from Anki and update state atoms with results.
   sync = {:!phase :!result :!error :!pull-updates}"
  [cards sync]
  (let [{:keys [!phase !result !error !pull-updates]} sync]
    #?(:cljs
       (do ((do-anki-pull! cards)
            (fn [result]
              (if (and (empty? (:updates result)) (empty? (:deleted result)))
                (do (reset! !result result)
                  (reset! !phase :done))
                (do (reset! !result result)
                  (reset! !pull-updates {:updates (:updates result)
                                         :deleted (:deleted result)})
                  (reset! !phase :recording))))
            (fn [err]
              (reset! !error (.-message err))
              (reset! !phase :error)))
         nil)
       :clj nil)))

(defn escape-key?
  "Cross-platform Escape key check for DOM key events."
  [e]
  #?(:cljs (= (.-key e) "Escape")
     :clj false))
