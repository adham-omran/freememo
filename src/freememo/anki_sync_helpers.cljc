(ns freememo.anki-sync-helpers
  "Plain helper functions for Anki sync — no e/defn, no reactive frame slots."
  (:require
   [missionary.core :as m]
   [clojure.string :as str]
   [freememo.occlusion-svg :as osvg]
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

(declare html-escape)

#?(:clj
   (defn format-bibliography-html
     "Render a CSL map as the bibliography body HTML — title link + author + year.
      Returns nil for a blank citation. No presentation wrapper. Server-side
      only — title/author/URL are HTML-escaped here, so callers must NOT
      double-escape."
     [csl]
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
         (when (seq body) body)))))

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
       "audio/mpeg" "mp3"
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
;; FreeMemo IO note type — canonical definition + ensure-and-enforce.
;; The app owns this model: its styling is the served freememo-anki.css and
;; its single template is the constant below (cloned from Image Occlusion
;; Enhanced's "IO Card"). Any Anki-side drift is overwritten on the next IO
;; push — user styling tweaks belong in the source CSS file, not in Anki.
;; ---------------------------------------------------------------------------

(def io-model-name "FreeMemo IO")
(def io-template-name "IO Card")

(def io-field-names
  ["ID (hidden)" "Header" "Image" "Question Mask" "Footer" "Remarks"
   "Sources" "Extra 1" "Extra 2" "Answer Mask" "Original Mask"])

(def io-pull-field-indexes
  "io_fields key -> index into the model's field order. The pull-side diff
   reads notesInfo values by Anki's :order; safe because this app owns the
   model's field list (io-field-names)."
  {:header 1 :footer 4 :remarks 5 :sources 6 :extra1 7 :extra2 8})

(def io-card-front
  "{{#Image}}\n<div id=\"io-header\">{{Header}}</div>\n<div id=\"io-wrapper\">\n  <div id=\"io-overlay\">{{Question Mask}}</div>\n  <div id=\"io-original\">{{Image}}</div>\n</div>\n<div id=\"io-footer\">{{Footer}}</div>\n\n<script>\n// Prevent original image from loading before mask\naFade = 50, qFade = 0;\nvar mask = document.querySelector('#io-overlay>img');\nfunction loaded() {\n    var original = document.querySelector('#io-original');\n    original.style.visibility = \"visible\";\n}\nif (mask === null || mask.complete) {\n    loaded();\n} else {\n    mask.addEventListener('load', loaded);\n}\n</script>\n{{/Image}}\n")

(def io-card-back
  "{{#Image}}\n<div id=\"io-header\">{{Header}}</div>\n<div id=\"io-wrapper\">\n  <div id=\"io-overlay\">{{Answer Mask}}</div>\n  <div id=\"io-original\">{{Image}}</div>\n</div>\n{{#Footer}}<div id=\"io-footer\">{{Footer}}</div>{{/Footer}}\n<button id=\"io-revl-btn\" onclick=\"toggle();\">Toggle Masks</button>\n<div id=\"io-extra-wrapper\">\n  <div id=\"io-extra\">\n    {{#Remarks}}\n      <div class=\"io-extra-entry\">\n        <div class=\"io-field-descr\">Remarks</div>{{Remarks}}\n      </div>\n    {{/Remarks}}\n    {{#Sources}}\n      <div class=\"io-extra-entry\">\n        <div class=\"io-field-descr\">Sources</div>{{Sources}}\n      </div>\n    {{/Sources}}\n    {{#Extra 1}}\n      <div class=\"io-extra-entry\">\n        <div class=\"io-field-descr\">Extra 1</div>{{Extra 1}}\n      </div>\n    {{/Extra 1}}\n    {{#Extra 2}}\n      <div class=\"io-extra-entry\">\n        <div class=\"io-field-descr\">Extra 2</div>{{Extra 2}}\n      </div>\n    {{/Extra 2}}\n  </div>\n</div>\n\n<script>\n// Toggle answer mask on clicking the image\nvar toggle = function() {\n  var amask = document.getElementById('io-overlay');\n  if (amask.style.display === 'block' || amask.style.display === '')\n    amask.style.display = 'none';\n  else\n    amask.style.display = 'block'\n}\n\n// Prevent original image from loading before mask\naFade = 50, qFade = 0;\nvar mask = document.querySelector('#io-overlay>img');\nfunction loaded() {\n    var original = document.querySelector('#io-original');\n    original.style.visibility = \"visible\";\n}\nif (mask === null || mask.complete) {\n    loaded();\n} else {\n    mask.addEventListener('load', loaded);\n}\n</script>\n{{/Image}}\n")

(def app-anki-css-url "/freememo/freememo-anki.css")

(defn occlusion-card? [card]
  (= "occlusion" (:flashcards/kind card)))

;; ---------------------------------------------------------------------------
;; FreeMemo Score note type — app-owned like FreeMemo IO. One template; the
;; direction is baked into which field carries the [sound:...] clip vs the
;; stacked notation crops, so "Both" is two independent notes.
;; Field ownership: ID/Front/Back/Links are FM-generated and overwritten on
;; every push; Remarks is the user's Anki-side annotation space — addNote seeds
;; it empty, updateNote never touches it.
;; ---------------------------------------------------------------------------

(def score-model-name "FreeMemo Score")
(def score-template-name "Score Card")

(def score-field-names
  ["ID (hidden)" "Front" "Back" "Links" "Remarks"])

(def score-card-front
  "<div class=\"score-front\">{{Front}}</div>\n")

(def score-card-back
  "{{FrontSide}}\n<hr id=answer>\n<div class=\"score-back\">{{Back}}</div>\n{{#Remarks}}<div class=\"score-extra\"><div class=\"score-field-descr\">Remarks</div>{{Remarks}}</div>{{/Remarks}}\n{{#Links}}<div class=\"score-extra\">{{Links}}</div>{{/Links}}\n")

(defn score-card? [card]
  (= "score" (:flashcards/kind card)))

(defn score-media-ids
  "Media id strings of a score card's clip + every notation crop.
   Pre: card carries :score-group (attached by get-cards-for-sync)."
  [card]
  (let [{:keys [clip-media-id geometry]} (:score-group card)]
    (into #{(str clip-media-id)}
      (map (comp str :media-id))
      (mapcat :rects (:pages geometry)))))

;; ---------------------------------------------------------------------------
;; Basic & FreeMemo Cloze note types — app-owned like IO/Score. The app owns
;; the whole pipeline: field list, templates, styling. Field order mirrors
;; Anki (Front/Back at index 0/1, Text at index 0) so the order-based pull path
;; reads identically for these and any pre-ownership notes. Source/Bibliography
;; are FM-generated; Remarks (both) and Back Extra (cloze) are the user's
;; Anki-side space — preserved on update, never generated by FreeMemo.
;; ---------------------------------------------------------------------------

(def basic-model-name "FreeMemo Basic")
(def basic-template-name "Basic Card")
(def basic-field-names ["Front" "Back" "Links" "Remarks"])

(def cloze-model-name "FreeMemo Cloze")
(def cloze-template-name "Cloze Card")
(def cloze-field-names ["Text" "Back Extra" "Links" "Remarks"])

(def basic-card-front
  "<div class=\"fm-front\">{{Front}}</div>\n")

(def basic-card-back
  "{{FrontSide}}\n<hr id=answer>\n<div class=\"fm-back\">{{Back}}</div>\n{{#Links}}<div class=\"fm-extra fm-links\">{{Links}}</div>{{/Links}}\n{{#Remarks}}<div class=\"fm-extra\"><div class=\"fm-field-descr\">Remarks</div>{{Remarks}}</div>{{/Remarks}}\n")

(def cloze-card-front
  "<div class=\"fm-front\">{{cloze:Text}}</div>\n")

(def cloze-card-back
  "<div class=\"fm-front\">{{cloze:Text}}</div>\n{{#Back Extra}}<div class=\"fm-back\">{{Back Extra}}</div>{{/Back Extra}}\n{{#Links}}<div class=\"fm-extra fm-links\">{{Links}}</div>{{/Links}}\n{{#Remarks}}<div class=\"fm-extra\"><div class=\"fm-field-descr\">Remarks</div>{{Remarks}}</div>{{/Remarks}}\n")

;; ---------------------------------------------------------------------------
;; FreeMemo Overlapping Cloze note type — app-owned, cloze-typed. One authored
;; list is one note; Anki fans it into one card per item (Text1..TextN, item k
;; is the active {{ck::}}) plus a reveal-all card (Full = c21). The question and
;; the items blocks each carry dir="auto", so direction is detected per block
;; from its own content — an English question stays LTR while an Arabic list
;; goes RTL — with no direction field or control. Question/Text*/Full/Original/
;; Links are FM-generated and overwritten every push; Remarks is the user's
;; Anki-side space — seeded empty, never clobbered.
;; ---------------------------------------------------------------------------

(def overlapping-model-name "FreeMemo Overlapping Cloze")
(def overlapping-template-name "Overlapping Card")

(def overlapping-field-names
  (into ["Question"]
    (concat (map #(str "Text" %) (range 1 21))
      ["Full" "Original" "Links" "Remarks"])))

(def ^:private overlapping-cloze-refs
  "The cloze-field references, in order, shared by front and back templates.
   Anki renders {{cloze:TextK}} empty unless TextK holds the current card's
   cloze number, so exactly one block shows per card."
  (str/join "\n    "
    (concat (map #(str "{{cloze:Text" % "}}") (range 1 21))
      ["{{cloze:Full}}"])))

(def overlapping-card-front
  (str "<div class=\"fm-ol\">\n"
    "  {{#Question}}<div class=\"fm-ol-question\" dir=\"auto\">{{Question}}</div>{{/Question}}\n"
    "  <div class=\"fm-ol-text\" dir=\"auto\">\n    " overlapping-cloze-refs "\n  </div>\n"
    "</div>\n"))

(def overlapping-card-back
  (str "<div class=\"fm-ol\">\n"
    "  {{#Question}}<div class=\"fm-ol-question\" dir=\"auto\">{{Question}}</div>{{/Question}}\n"
    "  <div class=\"fm-ol-text\" dir=\"auto\">\n    " overlapping-cloze-refs "\n  </div>\n"
    "  {{#Original}}<div class=\"fm-ol-original\" dir=\"auto\"><hr>{{Original}}</div>{{/Original}}\n"
    "  {{#Links}}<div class=\"fm-ol-extra fm-links\">{{Links}}</div>{{/Links}}\n"
    "  {{#Remarks}}<div class=\"fm-ol-extra\"><div class=\"fm-ol-field-descr\">Remarks</div>{{Remarks}}</div>{{/Remarks}}\n"
    "</div>\n"))

(defn overlapping-card? [card]
  (= "overlapping" (:flashcards/kind card)))

#?(:cljs
   (defn fetch-text!
     "Missionary task: GET a same-origin URL, resolve to the body text.

      Uses cache: no-store so the request always hits the network. The sole
      caller fetches the app-owned stylesheet for the ensure-*-model! drift
      check; that check is only correct against the CURRENT served CSS. The
      asset is served with Cache-Control max-age, so a plain fetch would read
      a stale browser-cached copy — which the check would then push to Anki,
      silently reverting a freshly deployed stylesheet."
     [url]
     (from-future
       (fn [token]
         (let [ctrl (js/AbortController.)]
           (.finally token #(.abort ctrl))
           (-> (js/fetch url (clj->js {:signal (.-signal ctrl) :cache "no-store"}))
             (.then (fn [resp]
                      (when-not (.-ok resp)
                        (throw (js/Error. (str "HTTP " (.-status resp) " fetching " url))))
                      (.text resp)))))))))

#?(:cljs
   (defn migrate-fields!
     "Missionary task: reconcile an app-owned model's field NAMES toward canon.
      Renames then removes, each op guarded so it is idempotent and safe to run
      on every push: a rename fires only when the old field is present and the
      new absent; a removal only when the field is present. Name-based — robust
      to the field-order drift seen across historical model versions.
      pre:  renames = [[old new] …]; removals = [name …].
      post: for each [old new] the model has new and lacks old (when old existed);
            for each name in removals the model lacks name.
      invariant: MUST run before the missing-field append step — appending the
      new field first makes the rename guard skip and strands the old field."
     [model-name renames removals]
     (m/sp
       ;; current-fields! returns a TASK yielding the model's current field-name
       ;; set — it must return a task, not the set directly. m/? only parks in
       ;; the enclosing m/sp body; missionary does NOT rewrite ? across a fn
       ;; boundary, so a bare (fn [] … (m/? …) …) never awaits the anki-call!
       ;; task, and the un-awaited task flows into js->clj/set and throws
       ;; "[object Object] is not ISeqable". Await with (m/? (current-fields!)).
       (let [current-fields! (fn [] (m/sp (set (js->clj (m/? (anki-call! "modelFieldNames"
                                                               {:modelName model-name}))))))]
         (loop [[[old new] & more] (seq renames)]
           (when old
             (let [fields (m/? (current-fields!))]
               (when (and (fields old) (not (fields new)))
                 (log/log-debug (str "[anki-push] " model-name ": renaming field " old " → " new))
                 (m/? (anki-call! "modelFieldRename"
                        {:modelName model-name :oldFieldName old :newFieldName new}))))
             (recur more)))
         (loop [[nm & more] (seq removals)]
           (when nm
             (when ((m/? (current-fields!)) nm)
               (log/log-debug (str "[anki-push] " model-name ": removing field " nm))
               (m/? (anki-call! "modelFieldRemove"
                      {:modelName model-name :fieldName nm})))
             (recur more)))
         true))))

#?(:cljs
   (defn- ensure-owned-model!
     "Create an app-owned model if absent, else reconcile it toward canon:
      apply field-name migrations (renames/removals), append any missing owned
      fields, then restore styling + template on strict-equality drift (same
      policy as ensure-io-model!/ensure-score-model!). Migrations run BEFORE the
      append so a rename lands before the new field would otherwise be added
      empty. Field appends land after the existing fields, so Front/Back (index
      0/1) and Text (index 0) — created first via :inOrderFields — keep their
      order-based-pull positions. Resolves to true; throws on AnkiConnect error."
     [{:keys [model-name template-name field-names is-cloze front back renames removals]} css]
     (m/sp
       (let [models (vec (js->clj (m/? (anki-call! "modelNames" nil))))]
         (if (some #{model-name} models)
           (do
             (m/? (migrate-fields! model-name renames removals))
             (let [fields (set (js->clj (m/? (anki-call! "modelFieldNames"
                                               {:modelName model-name}))))
                   missing (vec (remove fields field-names))]
               (loop [[f & more] missing]
                 (when f
                   (log/log-debug (str "[anki-push] " model-name ": adding field " f))
                   (m/? (anki-call! "modelFieldAdd"
                          {:modelName model-name :fieldName f}))
                   (recur more))))
             (let [styling (js->clj (m/? (anki-call! "modelStyling" {:modelName model-name})))]
               (when (not= (get styling "css") css)
                 (log/log-debug (str "[anki-push] " model-name " styling drifted — restoring"))
                 (m/? (anki-call! "updateModelStyling"
                        {:model {:name model-name :css css}}))))
             (let [templates (js->clj (m/? (anki-call! "modelTemplates" {:modelName model-name})))
                   tmpl (get templates template-name)]
               (when (or (not= (get tmpl "Front") front)
                       (not= (get tmpl "Back") back))
                 (log/log-debug (str "[anki-push] " model-name " templates drifted — restoring"))
                 (m/? (anki-call! "updateModelTemplates"
                        {:model {:name model-name
                                 :templates {template-name {:Front front :Back back}}}})))))
           (m/? (anki-call! "createModel"
                  {:modelName model-name
                   :inOrderFields field-names
                   :css css
                   :isCloze is-cloze
                   :cardTemplates [{:Name template-name :Front front :Back back}]})))
         true))))

#?(:cljs
   (defn ensure-io-model!
     "Missionary task: create-or-restore the FreeMemo IO model via
      ensure-owned-model! — no field migrations for this model (renames/removals
      empty; io-field-names has been stable since the model was cloned from
      Image Occlusion Enhanced's \"IO Card\"). Drift check is strict string
      equality — any Anki-side edit is restored. Resolves to true; throws on
      AnkiConnect errors (the caller's whole push fails with that message)."
     []
     (m/sp
       (let [css (m/? (fetch-text! app-anki-css-url))]
         (m/? (ensure-owned-model!
                {:model-name io-model-name :template-name io-template-name
                 :field-names io-field-names :is-cloze false
                 :front io-card-front :back io-card-back
                 :renames [] :removals []}
                css))))))

#?(:cljs
   (defn ensure-score-model!
     "Missionary task: create-or-restore the FreeMemo Score model via
      ensure-owned-model!, migrating its FIELDS toward canon: legacy
      'Bibliography' renames to 'Links', legacy 'Source'/'Sources' are removed,
      and any canonical field the model lacks is appended. Existing Remarks
      content survives."
     []
     (m/sp
       (let [css (m/? (fetch-text! app-anki-css-url))]
         (m/? (ensure-owned-model!
                {:model-name score-model-name :template-name score-template-name
                 :field-names score-field-names :is-cloze false
                 :front score-card-front :back score-card-back
                 :renames [["Bibliography" "Links"]] :removals ["Source" "Sources"]}
                css))))))

#?(:cljs
   (defn ensure-basic-model!
     "Missionary task: create-or-restore the FreeMemo Basic model. Mandatory
      before any basic note lands — a failure fails the whole push."
     []
     (m/sp
       (let [css (m/? (fetch-text! app-anki-css-url))]
         (m/? (ensure-owned-model!
                {:model-name basic-model-name :template-name basic-template-name
                 :field-names basic-field-names :is-cloze false
                 :front basic-card-front :back basic-card-back
                 :renames [["Bibliography" "Links"]] :removals ["Source"]}
                css))))))

#?(:cljs
   (defn ensure-cloze-model!
     "Missionary task: create-or-restore the FreeMemo Cloze model (cloze-typed
      via :isCloze true). Mandatory before any cloze note lands."
     []
     (m/sp
       (let [css (m/? (fetch-text! app-anki-css-url))]
         (m/? (ensure-owned-model!
                {:model-name cloze-model-name :template-name cloze-template-name
                 :field-names cloze-field-names :is-cloze true
                 :front cloze-card-front :back cloze-card-back
                 :renames [["Bibliography" "Links"]] :removals ["Source"]}
                css))))))

#?(:cljs
   (defn ensure-overlapping-model!
     "Missionary task: create-or-restore the FreeMemo Overlapping Cloze model
      (cloze-typed via :isCloze true). Mandatory before any overlapping note
      lands — a failure fails the whole push."
     []
     (m/sp
       (let [css (m/? (fetch-text! app-anki-css-url))]
         (m/? (ensure-owned-model!
                {:model-name overlapping-model-name :template-name overlapping-template-name
                 :field-names overlapping-field-names :is-cloze true
                 :front overlapping-card-front :back overlapping-card-back
                 :renames [["Title" "Question"]] :removals ["Direction"]}
                css))))))

(defn io-mask-uploads
  "[{:filename :svg} ...] — the Q and A masks of one occlusion card.
   Pre: card carries :occlusion-group (attached by get-cards-for-sync)."
  [card]
  (let [{:keys [anki-key mode geometry]} (:occlusion-group card)
        ordinal (:flashcards/mask_ordinal card)
        names (osvg/media-filenames anki-key ordinal)]
    [{:filename (:q names)
      :svg (osvg/question-mask-svg anki-key geometry mode ordinal)}
     {:filename (:a names)
      :svg (osvg/answer-mask-svg anki-key geometry mode ordinal)}]))

(defn io-mask-upload-plan
  "Q/A uploads for every occlusion card plus ONE shared O mask per group.
   Re-pushing overwrites the same filenames — that is how geometry edits
   propagate to already-pushed sibling notes."
  [io-cards]
  (let [per-group (into {}
                    (map (fn [card]
                           (let [{:keys [anki-key geometry]} (:occlusion-group card)]
                             [anki-key
                              {:filename (:o (osvg/media-filenames anki-key 0))
                               :svg (osvg/original-mask-svg anki-key geometry)}])))
                    io-cards)]
    (vec (concat (mapcat io-mask-uploads io-cards) (vals per-group)))))

#?(:cljs
   (defn store-io-mask!
     "Missionary task: upload one generated mask SVG to Anki's media folder.
      Generated SVGs are pure ASCII, so btoa is safe."
     [{:keys [filename svg]}]
     (anki-call! "storeMediaFile" {:filename filename :data (js/btoa svg)})))

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
  "Return a set of all media ID strings referenced in any HTML field of a card.
   Occlusion cards contribute their io_fields HTML plus the group's image;
   score cards contribute their clip + notation crops."
  [card]
  (let [html-ids (reduce into #{}
                   (keep extract-media-ids
                     (concat [(:flashcards/question card)
                              (:flashcards/answer card)
                              (:flashcards/cloze card)]
                       (vals (or (:flashcards/io_fields card) {})))))]
    (cond-> html-ids
      (:occlusion-group card)
      (conj (str (get-in card [:occlusion-group :image-media-id])))
      (:score-group card)
      (into (score-media-ids card)))))

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

(defn build-links-field
  "FM-owned 'Links' field for a FreeMemo Basic/Cloze/Score note: the resolved
   citation (linked iff the CSL carried a URL) stacked over an 'Open in FreeMemo'
   deep-link to <app-base-url>/viewer/topic/<topic-id>. FreeMemo owns this field
   unconditionally and overwrites it on every push. Returns {\"Links\" html}.
   pre:  citation pre-resolved in :fm/bibliography-html (card) or
         :bibliography-html (settings); already HTML-escaped by
         format-bibliography-html, so it is NOT re-escaped here. :app-base-url
         in settings, else default-app-base-url.
   post: the FreeMemo link renders whenever :flashcards/topic_id is present
         (independent of topic title); value is \"\" only when topic_id is nil
         AND citation is blank."
  [card settings]
  (let [citation (or (:fm/bibliography-html card) (:bibliography-html settings))
        topic-id (:flashcards/topic_id card)
        base (or (:app-base-url settings) default-app-base-url)
        fm-link (when topic-id
                  (str "<a href=\"" (html-escape (str base "/viewer/topic/" topic-id))
                    "\">Open in FreeMemo</a>"))
        body (cond
               (and (not (str/blank? citation)) fm-link) (str citation "<br>" fm-link)
               fm-link fm-link
               (not (str/blank? citation)) citation
               :else nil)]
    {"Links" (if body (str "<p class=\"fm-links\">" body "</p>") "")}))

(defn build-io-fields
  "Field map for an FreeMemo IO note: generated media references plus the six
   io_fields text values (with /api/media srcs rewritten to uploaded names).
   Pre: card carries :occlusion-group and :flashcards/mask_ordinal."
  [card filename-map]
  (let [{:keys [anki-key image-media-id]} (:occlusion-group card)
        ordinal (:flashcards/mask_ordinal card)
        io (or (:flashcards/io_fields card) {})
        names (osvg/media-filenames anki-key ordinal)
        image-filename (get filename-map (str image-media-id)
                         (str "/api/media/" image-media-id))
        rw (fn [k] (rewrite-media-srcs (or (get io k) "") filename-map))]
    {"ID (hidden)" (osvg/note-hidden-id anki-key ordinal)
     "Header" (rw :header)
     "Image" (str "<img src=\"" image-filename "\" />")
     "Question Mask" (str "<img src=\"" (:q names) "\" />")
     "Footer" (rw :footer)
     "Remarks" (rw :remarks)
     "Sources" (rw :sources)
     "Extra 1" (rw :extra1)
     "Extra 2" (rw :extra2)
     "Answer Mask" (str "<img src=\"" (:a names) "\" />")
     "Original Mask" (str "<img src=\"" (:o names) "\" />")}))

(defn build-io-note
  "AnkiConnect note map for an occlusion card. The 'ID (hidden)' first field
   is unique per note, so Anki's duplicate check never trips."
  [card settings filename-map]
  {:deckName (:deck settings)
   :modelName io-model-name
   :fields (build-io-fields card filename-map)
   :tags (:tags settings)
   :options {:allowDuplicate false :duplicateScope "deck"}})

(defn- score-sound-html
  "[sound:...] tag for the group's clip, via the uploaded filename."
  [clip-media-id filename-map]
  (str "[sound:" (get filename-map (str clip-media-id)
                   (str clip-media-id ".mp3")) "]"))

(defn- score-sheet-html
  "Notation crops stacked vertically in ordinal order (page order — a phrase
   wrapping across systems reads top to bottom)."
  [geometry filename-map]
  (->> (:pages geometry)
    (mapcat :rects)
    (sort-by :ordinal)
    (map (fn [{:keys [media-id]}]
           (str "<img src=\""
             (get filename-map (str media-id) (str "/api/media/" media-id))
             "\" />")))
    (str/join "<br/>")))

(defn build-score-fields
  "FM-owned fields for a score note, direction-baked: audio-front puts the
   clip on the Front and the crops on the Back; sheet-front mirrors. Links
   fills exactly like basic cards (citation + FreeMemo deep-link). Remarks is
   deliberately absent — it is the user's Anki-side annotation space and
   updateNote must never clobber it.
   Pre: card carries :score-group and :flashcards/score_direction; settings
   carries the link inputs (:app-base-url :bibliography-html ...)."
  [card settings filename-map]
  (let [{:keys [anki-key clip-media-id geometry]} (:score-group card)
        direction (:flashcards/score_direction card)
        sound (score-sound-html clip-media-id filename-map)
        sheet (score-sheet-html geometry filename-map)]
    (merge
      {"ID (hidden)" (str "fm-score-" anki-key "-" direction)
       "Front" (if (= direction "audio-front") sound sheet)
       "Back" (if (= direction "audio-front") sheet sound)}
      (build-links-field card settings))))

(defn build-score-note
  "AnkiConnect note map for a score card. Remarks seeded empty on create;
   the unique 'ID (hidden)' first field defeats the duplicate check (the
   Both pair shares Front/Back media)."
  [card settings filename-map]
  {:deckName (:deck settings)
   :modelName score-model-name
   :fields (merge {"Remarks" ""}
             (build-score-fields card settings filename-map))
   :tags (:tags settings)
   :options {:allowDuplicate false :duplicateScope "deck"}})

(defn- note-dupe-options
  "addNote :options for owned basic/cloze notes — deck-scoped duplicate check
   unless the user opted into duplicates."
  [settings]
  (if (:allow-dupes settings)
    {:allowDuplicate true}
    {:allowDuplicate false :duplicateScope "deck"}))

(defn build-basic-content-fields
  "FM-owned content fields for a FreeMemo Basic note: Front, Back, Links.
   Excludes Remarks (user Anki-side space). Header prepends to Front. Images
   stay inline in Front/Back (no field split). Shared by add + update."
  [card settings filename-map]
  (let [{:keys [use-header header-text]} settings
        rw-front (rewrite-media-srcs (wrap-p (or (:flashcards/question card) "")) filename-map)
        rw-back (rewrite-media-srcs (wrap-p (or (:flashcards/answer card) "")) filename-map)
        front-html (prepend-header rw-front use-header header-text)]
    (merge {"Front" front-html "Back" rw-back}
      (build-links-field card settings))))

(defn build-cloze-content-fields
  "FM-owned content fields for a FreeMemo Cloze note: Text, Links. Excludes
   Back Extra + Remarks (user Anki-side space). All <img> stay in the Text
   field. Shared by add + update."
  [card settings filename-map]
  (let [{:keys [use-header header-text]} settings
        rw-cloze (rewrite-media-srcs (or (:flashcards/cloze card) "") filename-map)
        cloze-html (prepend-header rw-cloze use-header header-text)]
    (merge {"Text" cloze-html}
      (build-links-field card settings))))

(defn build-basic-note
  "AnkiConnect addNote map for a basic card → FreeMemo Basic. Remarks seeded
   empty; the app owns the model, so no field/model config is read from settings."
  [card settings filename-map]
  {:deckName (:deck settings)
   :modelName basic-model-name
   :fields (merge {"Remarks" ""} (build-basic-content-fields card settings filename-map))
   :tags (:tags settings)
   :options (note-dupe-options settings)})

(defn build-cloze-note
  "AnkiConnect addNote map for a cloze card → FreeMemo Cloze. Back Extra +
   Remarks seeded empty (user Anki-side space)."
  [card settings filename-map]
  {:deckName (:deck settings)
   :modelName cloze-model-name
   :fields (merge {"Back Extra" "" "Remarks" ""}
             (build-cloze-content-fields card settings filename-map))
   :tags (:tags settings)
   :options (note-dupe-options settings)})

(defn build-overlapping-content-fields
  "FM-owned content fields for an FreeMemo Overlapping Cloze note: Question, the
   materialized Text1..Text20 (items beyond the list blank), Full, Original,
   Links. Excludes Remarks (user Anki-side space). Shared by add +
   update. Pre: card carries :flashcards/overlapping (parsed JSONB w/ :fields)."
  [card settings filename-map]
  (let [ol (:flashcards/overlapping card)
        fields (:fields ol)
        rw (fn [s] (rewrite-media-srcs (or s "") filename-map))
        text-fields (into {}
                      (map (fn [k]
                             [(str "Text" k)
                              (rw (get fields (keyword (str "Text" k))))]))
                      (range 1 21))]
    (merge
      {"Question" (rw (:question ol))
       "Full" (rw (:Full fields))
       "Original" (rw (:Original fields))}
      text-fields
      (build-links-field card settings))))

(defn build-overlapping-note
  "AnkiConnect addNote map for an overlapping card → FreeMemo Overlapping Cloze.
   Remarks seeded empty (user Anki-side space)."
  [card settings filename-map]
  {:deckName (:deck settings)
   :modelName overlapping-model-name
   :fields (merge {"Remarks" ""} (build-overlapping-content-fields card settings filename-map))
   :tags (:tags settings)
   :options (note-dupe-options settings)})

#?(:cljs
   (defn build-note
     "Build an AnkiConnect addNote map, routing by card kind to the app-owned
      model builder. filename-map = {media-id -> \"<id>.<ext>\"} for src rewrite.
      Every kind is app-owned: occlusion→IO, score→Score, cloze→FreeMemo Cloze,
      else→FreeMemo Basic."
     [card settings filename-map]
     (cond
       (occlusion-card? card) (build-io-note card settings filename-map)
       (score-card? card) (build-score-note card settings filename-map)
       (overlapping-card? card) (build-overlapping-note card settings filename-map)
       (= "cloze" (:flashcards/kind card)) (build-cloze-note card settings filename-map)
       :else (build-basic-note card settings filename-map))))

#?(:cljs
   (defn build-update-fields
     "FM-owned field map for updating an existing note (content fields only —
      never Remarks/Back Extra, the user's Anki-side space). Occlusion→IO
      fields, score→score fields, cloze→cloze content, else→basic content.
      filename-map = {media-id -> \"<id>.<ext>\"} for src rewrite."
     [card settings filename-map]
     (cond
       (occlusion-card? card) (build-io-fields card filename-map)
       (score-card? card) (build-score-fields card settings filename-map)
       (overlapping-card? card) (build-overlapping-content-fields card settings filename-map)
       (= "cloze" (:flashcards/kind card)) (build-cloze-content-fields card settings filename-map)
       :else (build-basic-content-fields card settings filename-map))))

(defn owned-model-name
  "The app-owned Anki model name for a card kind. Every kind is app-owned."
  [kind]
  (case kind
    "occlusion"   io-model-name
    "score"       score-model-name
    "cloze"       cloze-model-name
    "overlapping" overlapping-model-name
    basic-model-name))

(def ^:private migrate-carry-fields
  "Per kind, the field names an updateNoteModel migration must re-supply from the
   note's CURRENT Anki values. build-update-fields omits these (user Anki-side
   space) and updateNoteModel wipes omitted fields, so without re-supplying them
   the migration would blank them. Occlusion carries nothing — build-io-fields is
   already complete."
  {"basic"       ["Remarks"]
   "cloze"       ["Remarks" "Back Extra"]
   "overlapping" ["Remarks"]
   "score"       ["Remarks"]
   "occlusion"   []})

(defn migrate-note-fields
  "Field map for an updateNoteModel migration: FM content (field-map) plus each
   carried user-owned field read from current-fields — the note's notesInfo
   :fields map, {:FieldName {:value ...}}. A key absent from current-fields
   (e.g. migrating from a foreign model that never had it) defaults to \"\".
   pre:  field-map = build-update-fields output; current-fields may be nil.
   post: result = field-map plus every name in (migrate-carry-fields kind)."
  [kind field-map current-fields]
  (reduce (fn [m field-name]
            (assoc m field-name (get-in current-fields [(keyword field-name) :value] "")))
    field-map
    (get migrate-carry-fields kind)))

#?(:cljs
   (defn do-anki-push!
     "Push cards to Anki. Returns Missionary task resolving to result map.
      All card kinds are app-owned models (occlusion→IO, score→Score,
      cloze→FreeMemo Cloze, basic→FreeMemo Basic); no note-type/field config is
      read from settings.
      settings = {:deck :allow-dupes :use-header :header-text :tags
                  :bibliography-html :app-base-url}"
     [cards settings]
     (let [new-cards (vec (filter #(nil? (:flashcards/anki_note_id %)) cards))
           ;; Re-push every previously-synced card on every push so changes to
           ;; header text / use-header / tags propagate even when card body
           ;; content was not edited. Anki-side timestamp checks can't see these
           ;; settings, so optimizing here would silently drop legitimate updates.
           changed-cards (vec (filter #(some? (:flashcards/anki_note_id %)) cards))
           ;; Occlusion cards that actually carry group data (attached by
           ;; get-cards-for-sync); the same re-push-everything rule regenerates
           ;; every pushed group's masks, which is what makes geometry
           ;; dirtiness group-scoped.
           io-cards (vec (filter :occlusion-group cards))
           score-cards (vec (filter :score-group cards))
           has-basic? (some #(= "basic" (:flashcards/kind %)) cards)
           has-cloze? (some #(= "cloze" (:flashcards/kind %)) cards)
           has-overlapping? (some #(= "overlapping" (:flashcards/kind %)) cards)]
       (log/log-debug (str "[anki-push] do-anki-push! starting"
                        " new=" (count new-cards)
                        " update=" (count changed-cards)
                        " occlusion=" (count io-cards)
                        " score=" (count score-cards)
                        " topic-title=" (pr-str (:topic-title settings))
                        " topic-kind=" (pr-str (:topic-kind settings))))
       (m/sp
         ;; Phase -1: any card of a kind ⇒ guarantee its app-owned model exists
         ;; and matches canon BEFORE any note lands. A failure here fails the
         ;; whole push with the AnkiConnect error — by design: "mandatory" means
         ;; no note is ever created against a missing or drifted model.
         (when (seq io-cards)
           (m/? (ensure-io-model!)))
         (when (seq score-cards)
           (m/? (ensure-score-model!)))
         (when has-basic?
           (m/? (ensure-basic-model!)))
         (when has-cloze?
           (m/? (ensure-cloze-model!)))
         (when has-overlapping?
           (m/? (ensure-overlapping-model!)))
         ;; Phase 0: collect all unique media IDs across every card, upload each once,
         ;; build filename-map {id -> "<id>.<ext>"} for src rewriting.
         (let [all-media-ids (reduce (fn [acc card] (into acc (collect-card-media-ids card)))
                               #{}
                               (concat new-cards changed-cards))
               ;; Phase 0.5: generate + store the mask SVGs (Q/A per card, one
               ;; shared O per group). Overwrites the same filenames on every
               ;; push. Failures don't abort the push; they surface on the
               ;; :errors channel below.
               mask-errors
               (if (empty? io-cards)
                 []
                 (m/? (m/reduce into []
                        (m/ap
                          (let [upload (m/?> 3 (m/seed (io-mask-upload-plan io-cards)))]
                            (try
                              (m/? (store-io-mask! upload))
                              []
                              (catch :default err
                                (log/log-error (str "[anki-push] mask store FAILED "
                                                 (:filename upload)
                                                 " error=" (.-message err)))
                                [{:card-id nil
                                  :error (str "mask " (:filename upload) ": "
                                           (.-message err))}])))))))
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
               ;; Phase 1.9: read every changed note's current model + fields.
               ;; Phase 2 field-updates (updateNote) notes already on the owned
               ;; model and migrates (updateNoteModel) notes on any other model —
               ;; legacy foreign note types AND the pre-rename owned names.
               ;; updateNoteModel wipes omitted fields, so the migrate map re-reads
               ;; the user-owned fields (Remarks / Back Extra) from these values.
               changed-notes-info
               (if (empty? changed-cards)
                 []
                 (m/? (anki-call! "notesInfo"
                        {:notes (mapv :flashcards/anki_note_id changed-cards)})))
               model-by-note (into {} (map (fn [n] [(:noteId n) (:modelName n)])) changed-notes-info)
               fields-by-note (into {} (map (fn [n] [(:noteId n) (:fields n)])) changed-notes-info)
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
                        (let [card (m/?> 10 (m/seed changed-cards))
                              kind (:flashcards/kind card)
                              note-id (:flashcards/anki_note_id card)]
                          (try
                            (let [field-map (build-update-fields card settings filename-map)
                                  owned-model (owned-model-name kind)
                                  current-model (get model-by-note note-id)]
                              (log/log-debug (str "[anki-push] update card-id=" (:flashcards/id card)
                                               " kind=" kind " note-id=" note-id
                                               " model=" (pr-str current-model) " → " (pr-str owned-model)
                                               " field-keys=" (vec (keys field-map))))
                              (if (= current-model owned-model)
                                ;; Already on the owned model — field-only update
                                ;; preserves user-owned fields (updateNote leaves
                                ;; omitted fields intact).
                                (m/? (anki-call! "updateNote"
                                       {:note {:id note-id :fields field-map
                                               :tags (:tags settings)}}))
                                ;; On another model — migrate to the owned model.
                                ;; Covers legacy foreign note types AND the pre-rename
                                ;; owned names. updateNoteModel wipes omitted fields,
                                ;; so re-supply the user-owned fields from current values.
                                (let [migrate-map (migrate-note-fields kind field-map
                                                    (get fields-by-note note-id))]
                                  (m/? (anki-call! "updateNoteModel"
                                         {:note {:id note-id :modelName owned-model
                                                 :fields migrate-map
                                                 :tags (:tags settings)}}))))
                              {:pairs [{:card-id (:flashcards/id card)
                                        :anki-note-id note-id}]
                               :updated [1]})
                            (catch :default err
                              (log/log-error (str "[anki-push] update FAILED card-id=" (:flashcards/id card)
                                               " error=" (.-message err)))
                              {:errors [{:card-id (:flashcards/id card) :error (.-message err)}]}))))))]
           ;; Merge both phases (+ mask-upload errors)
           (-> (merge-with into add-result update-result)
             (update :errors into mask-errors)
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
                                       occlusion? (= kind "occlusion")
                                       score? (= kind "score")
                                       overlapping? (= kind "overlapping")
                                       ordered-vals (ordered-field-values (:fields anki-note))
                                       update-map
                                       (cond
                                         ;; Score/overlapping: fields are FM-derived
                                         ;; (media refs, or the expanded list) — the
                                         ;; Anki→FM inverse is lossy, so nothing is
                                         ;; pulled.
                                         (or score? overlapping?) nil
                                         ;; Occlusion: diff the six text fields by the
                                         ;; app-owned field order; masks/Image/ID are
                                         ;; FM-owned and ignored. Both sides stripped so
                                         ;; local Quill markup doesn't read as a diff.
                                         occlusion?
                                         (let [io (or (:flashcards/io_fields card) {})
                                               changed (reduce
                                                         (fn [acc [k idx]]
                                                           (let [anki-v (strip-html (or (nth ordered-vals idx nil) ""))
                                                                 local-v (strip-html (or (get io k) ""))]
                                                             (if (= anki-v local-v)
                                                               acc
                                                               (assoc acc k anki-v))))
                                                         {} io-pull-field-indexes)]
                                           (when (seq changed)
                                             {:card-id (:flashcards/id card)
                                              :io-fields changed}))

                                         basic?
                                         (let [q (strip-html (first ordered-vals))
                                               a (strip-html (second ordered-vals))
                                               local-q (or (:flashcards/question card) "")
                                               local-a (or (:flashcards/answer card) "")]
                                           (when (or (not= q local-q) (not= a local-a))
                                             {:card-id (:flashcards/id card)
                                              :question q :answer a}))

                                         :else
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
  (let [{:keys [!status !error !decks !models !selected-deck !all-tags]} conn]
    #?(:cljs
       (do ((fetch-anki-config!)
            (fn [{:keys [decks models tags]}]
              (reset! !decks decks)
              (reset! !models models)
              (reset! !all-tags tags)
              ;; Deck selection is NOT defaulted here — apply-prefs! resolves it
              ;; (preset/Settings/first) once, so the form never paints a wrong
              ;; first value before the saved prefs load.
              (reset! !status :connected))
            (fn [err]
              (reset! !error (str "Cannot connect to Anki: " (.-message err)))
              (reset! !status :error)))
         nil)
       :clj nil)))


(defn run-push!
  "Execute push to Anki and update state atoms with results.
   Always transitions to :recording — finalize-push! must run even when
   there are no new pairs, so settings/per-item preset still persist.
   settings = {:deck :allow-dupes :use-header :header-text :tags
               :bibliography-html :app-base-url ...}
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
              (reset! !error {:message (.-message err)
                              :stack (.-stack err)
                              :source :client
                              :phase :push
                              :deck (:deck settings)
                              :card-count (count cards)
                              :topic-kind (:topic-kind settings)})
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
              (reset! !error {:message (.-message err)
                              :stack (.-stack err)
                              :source :client
                              :phase :pull
                              :card-count (count cards)})
              (reset! !phase :error)))
         nil)
       :clj nil)))

(defn escape-key?
  "Cross-platform Escape key check for DOM key events."
  [e]
  #?(:cljs (= (.-key e) "Escape")
     :clj false))

(defn pushable?
  "Gate for enabling Push / cmd-Enter: a deck is selected. Note types are
   app-owned, so no model selection is required."
  [deck]
  (boolean (not (str/blank? deck))))

(defn schedule-close!
  "Close the modal after ms — js/setTimeout kept out of the e/defn body per the
   file's platform-split rule. Token-guarded by the caller so it schedules once."
  [!show-modal ms]
  #?(:cljs (do (js/setTimeout (fn [] (reset! !show-modal false)) ms) nil)
     :clj nil))
