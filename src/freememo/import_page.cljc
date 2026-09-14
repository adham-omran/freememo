(ns freememo.import-page
  "Import tab — an intake area over a grid of source cards.

   plans/import-page-redesign.md is the specification. Two things about the shape
   are worth stating here, because both replaced something simpler that was
   wrong.

   The intake area is the page's primary control, and it owns NO transport. It
   classifies what arrived (`freememo.import-types`), decides which modal opens,
   and hands the file over. Every ceiling check and every upload protocol stays
   in the modal that already had it — a second copy of the archive-versus-
   multipart split is a second place for it to drift.

   Which modal is open is ONE fact, held in `!open-source` as the source's
   keyword. The modals close themselves by writing `false` to that same atom, so
   every read here tests for a keyword and both `nil` and `false` read as closed.
   Eight independent booleans made \"is anything open\" a question you had to ask
   eight times."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.a11y :as a11y]
   [freememo.icons :as icons]
   [freememo.import-types :as itypes]
   [freememo.navigation :as nav]
   [freememo.command-bus :as bus]
   [freememo.import-modal :refer [ImportModal]]
   [freememo.forge-import-modal :refer [ForgeImportModal]]
   [freememo.video-import-modal :refer [VideoImportModal]]
   [freememo.zotero-picker-modal :refer [ZoteroPickerModal]]
   [freememo.client-errors :as ce]))

;; Palette → Import handoff: the command handler records which entry modal to
;; open and navigates to :import; ImportPage consumes the intent on mount.
(defonce !pending-import (atom nil))

;; ── The source set ─────────────────────────────────────────────────

(def sources
  "Every source card, in display order within its group.

   `:id` is the card's identity everywhere — the palette intent, the open-modal
   atom, and the key mapping all use it. `:key` is the digit that selects the
   card (spec §11.2); Live Document carries none while §16.3 is open.

   There is NO Upload card. Uploading a document is what the intake area does,
   and a card two hundred pixels away that opens a file dialog was competing with
   the control the whole page is built around. The `:upload` source id survives
   without a card: the intake area's single-file route and the palette's
   `:import-upload` command both open that modal directly."
  [{:id :link      :group :primary   :key "1" :icon :link      :title "Link"
    :desc "Fetch a web page or a Wikipedia article"}
   {:id :audio     :group :primary   :key "2" :icon :mic       :title "Audio"
    :desc "Transcribe an audio file"}
   {:id :video     :group :primary   :key "3" :icon :video     :title "Video"
    :desc "Transcribe one video or a playlist"}
   {:id :new-topic :group :secondary :key "4" :icon :file-plus :title "New Topic"
    :desc "Write in a blank topic"}
   {:id :paste     :group :secondary :key "5" :icon :clipboard :title "Paste"
    :desc "Paste HTML or Markdown"}
   {:id :score     :group :secondary :key "6" :icon :music     :title "Score"
    :desc "Match sheet music to a recording"}
   {:id :zotero    :group :secondary :key "7" :icon :library   :title "Zotero"
    :desc "Select a PDF from your Zotero library"}
   ;; One card for both code paths. A forge import is what the card opens; the
   ;; archive upload is one link inside it (`:code-zip`), so the grid gains no
   ;; card for it (plans/git-forge-integration.md §4.3 3.1).
   {:id :code      :group :secondary :key "8" :icon :code      :title "Code"
    :desc "From GitHub, GitLab, Forgejo or a .zip file"}
   {:id :poem      :group :secondary :key "9" :icon :book-open :title "Poem"
    :desc "Import a poem package as a .zip file"}
   {:id :live-doc  :group :secondary          :icon :scan-text :title "Live Document"
    :desc "Photograph pages into a growing PDF"}])

(defn sources-in
  "The cards of one group, in order. Post: 3 cards for :primary, 7 for
   :secondary. The primary count divides by the three columns §7.3 asks for;
   the secondary one no longer does, so its last row holds one card. Poem was
   the seventh (poem-reader-spec row 2.6), and a filler card to square the grid
   would be a control that does nothing."
  [group]
  (filterv #(= group (:group %)) sources))

;; ── Live Document ──────────────────────────────────────────────────
;; Both platforms so the e/defn body below references a resolvable var on CLJ
;; (no #?(:cljs …) inside the reactive body — frame-safety convention).

#?(:cljs
   (defn- create-live-doc! [navigate!]
     (-> (js/fetch "/api/create-live-doc" (clj->js {:method "POST"}))
       (.then (fn [r] (.json r)))
       (.then (fn [^js d]
                (when (.-success d)
                  (navigate! :viewer (nav/nav-topic (.-doc_id d) nil)))))
       (.catch (fn [e]
                 (js/console.error "Create Live Document failed:" e)
                 (ce/report! :import/create-live-doc e)))))
   :clj
   (defn- create-live-doc! [_navigate!] nil))

#?(:cljs
   (defn- take-pending-import!
     "Open the source the palette asked for, once.

      Pre : `!pending` holds a source id or nil.
      Post: `!pending` holds nil, and `!open-source` holds the id it held.
      One plain fn rather than two side effects side by side in the reactive body:
      Electric may skip an expression whose value nothing uses, and `do` is
      concurrent, so a pair of bare `reset!`s there has neither guarantee."
     [!pending !open-source]
     (when-some [id @!pending]
       (reset! !open-source id)
       (reset! !pending nil)
       id))
   :clj
   (defn- take-pending-import! [_!pending _!open-source] nil))

#?(:cljs
   (defn- open-import! [intent]
     (fn [ctx _payload]
       (reset! !pending-import intent)
       (when-let [navigate! (:navigate! ctx)]
         (navigate! :import)))))

#?(:cljs
   (do (bus/register-handler! :import-link (open-import! :link))
       (bus/register-handler! :import-upload (open-import! :upload))
       (bus/register-handler! :import-audio (open-import! :audio))
       (bus/register-handler! :import-video (open-import! :video))
       (bus/register-handler! :import-score (open-import! :score))
       (bus/register-handler! :import-paste (open-import! :paste))
       (bus/register-handler! :new-topic (open-import! :new-topic))
       (bus/register-handler! :import-zotero (open-import! :zotero))
       (bus/register-handler! :import-poem (open-import! :poem))
       (bus/register-handler! :create-live-doc
         (fn [ctx _payload]
           (when-let [navigate! (:navigate! ctx)]
             (create-live-doc! navigate!))))))

;; ── Drop routing ───────────────────────────────────────────────────

#?(:cljs
   (defn file-vec
     "Post: a vector of the Files in `file-list` (possibly empty)."
     [file-list]
     (vec (array-seq (or file-list #js []))))
   :clj
   (defn file-vec [_file-list] []))

#?(:cljs (defn- file-name [^js f] (.-name f))
   :clj  (defn- file-name [_f] ""))

(defn plan-drop
  "What a dropped or picked set of files means. Pure — the reactive layer only
   applies the result.

   Pre : `files` is a vector of Files (possibly empty).
   Post: exactly one of
           {:action :none}
           {:action :video  :files [F …]}          — stage them all (§6.9.1)
           {:action :single :file F :message S?}   — import one, name the rest
           {:action :message :message S}           — import nothing (§6.8, §6.9.3)
   Invariant: `:single` never carries a video file, and `:video` never carries
         anything else — a mixed drop is a `:message`, because no flow can take
         two transports at once."
  [files]
  (let [named (mapv (fn [f] [f (file-name f)]) files)
        unknown (remove (fn [[_ n]] (itypes/group-for n)) named)
        transports (into #{} (map (fn [[_ n]] (itypes/transport-for n)) named))]
    (cond
      (empty? files)
      {:action :none}

      ;; §6.8 — one unreadable file refuses the whole drop. Importing the
      ;; readable part of a mixed selection would hide the refusal.
      (seq unknown)
      {:action :message :message (itypes/rejection-message (second (first unknown)))}

      ;; §6.9.3 — mixed transports. Naming the groups is what tells the user to
      ;; drop them separately.
      (> (count transports) 1)
      {:action :message
       :message (str "Drop one kind of file at a time. This selection holds "
                  (->> named
                    (map (comp :label itypes/group-for second))
                    distinct sort
                    (str/join " and "))
                  ".")}

      ;; §6.9.1 — every video in the selection stages together, because a
      ;; playlist is the one import that takes more than one file.
      (= #{:video-session} transports)
      {:action :video :files files}

      ;; §6.9.2 — one file imports, and the message names what the page skipped.
      :else
      (let [[f _] (first named)
            skipped (mapv second (rest named))]
        (cond-> {:action :single :file f}
          (seq skipped)
          (assoc :message (str "Imported the first file. FreeMemo did not use "
                            (str/join ", " skipped)
                            " — drop one file at a time for these.")))))))

;; ── Clipboard routing ──────────────────────────────────────────────

(def ^:private url-pattern
  "A pasted web address. Anchored and whitespace-free, so a sentence that merely
   mentions a link stays text and imports as a topic."
  #"^https?://[^\s]+$")

(defn- html-source?
  "True when `s` reads as HTML markup rather than prose."
  [s]
  (boolean (and s (re-find #"^\s*<[a-zA-Z!]" s) (str/includes? s "</"))))

(defn plan-paste
  "What the clipboard means (spec §6.10.1). Pure.

   Pre : `text` is the text/plain flavor, `html` the text/html flavor. Either may
         be nil or blank.
   Post: {:source :link :intake {:url S}}
       | {:source :paste :intake {:paste {:format S :content S}}}
       | nil — nothing identifiable, so the caller starts no import (§6.10.2)."
  [text html]
  (let [t (some-> text str/trim)]
    (cond
      (and t (re-find url-pattern t))
      {:source :link :intake {:url t}}

      ;; Raw markup typed or copied as source travels in text/plain. A rendered
      ;; copy travels in text/html. Both import as HTML.
      (html-source? t)
      {:source :paste :intake {:paste {:format "html" :content t}}}

      (not (str/blank? html))
      {:source :paste :intake {:paste {:format "html" :content html}}}

      (not (str/blank? t))
      {:source :paste :intake {:paste {:format "markdown" :content t}}}

      :else nil)))

;; ── Page-level listeners ───────────────────────────────────────────

#?(:cljs
   (defn install-page-drag!
     "Show the drag state while a file is over the page, and route a drop.

      Pre : `!dragging?` is an atom; `on-files!` takes a vector of Files.
      Post: returns a 0-arg teardown fn that removes every listener it added.
      Invariant: dragenter and dragleave fire once per element the pointer
            crosses, so a plain boolean flickers as the pointer moves over the
            cards. A depth counter is what keeps the state on for the whole drag.
      `dragover` must call preventDefault or the browser refuses the drop."
     [!dragging? on-files!]
     (let [!depth (atom 0)
           over (fn [^js e] (.preventDefault e))
           enter (fn [^js e]
                   (.preventDefault e)
                   (when (= 1 (swap! !depth inc)) (reset! !dragging? true)))
           leave (fn [_]
                   (when (zero? (swap! !depth #(max 0 (dec %))))
                     (reset! !dragging? false)))
           drop! (fn [^js e]
                   (.preventDefault e)
                   (reset! !depth 0)
                   (reset! !dragging? false)
                   (on-files! (file-vec (some-> e .-dataTransfer .-files))))]
       (.addEventListener js/document "dragenter" enter)
       (.addEventListener js/document "dragover" over)
       (.addEventListener js/document "dragleave" leave)
       (.addEventListener js/document "drop" drop!)
       (fn teardown []
         (.removeEventListener js/document "dragenter" enter)
         (.removeEventListener js/document "dragover" over)
         (.removeEventListener js/document "dragleave" leave)
         (.removeEventListener js/document "drop" drop!))))
   :clj
   (defn install-page-drag! [_!dragging? _on-files!] (fn [])))

#?(:cljs
   (defn install-page-paste!
     "Route a paste anywhere on the page.

      Pre : `on-clipboard!` takes [text html], either possibly nil.
      Post: returns a 0-arg teardown fn.
      Invariant: a paste into an editable element belongs to that element — the
            listener yields whenever the event target is one, so a future input on
            this page keeps working."
     [on-clipboard!]
     (let [editable? (fn [^js el]
                       (boolean (and el
                                  (or (.-isContentEditable el)
                                    (#{"INPUT" "TEXTAREA"} (.-tagName el))))))
           on-paste (fn [^js e]
                      (when-not (editable? (.-target e))
                        (when-some [cd (.-clipboardData e)]
                          (on-clipboard! (.getData cd "text/plain")
                            (.getData cd "text/html")))))]
       (.addEventListener js/document "paste" on-paste)
       (fn teardown [] (.removeEventListener js/document "paste" on-paste))))
   :clj
   (defn install-page-paste! [_on-clipboard!] (fn [])))

;; ── Components ─────────────────────────────────────────────────────

;; Pre:  `source` is an entry of `sources`; `activate!` takes the source id.
;; Post: a card that opens its source on click, on Enter and on Space.
;; The whole card is the control, so it carries the tabindex and the role rather
;; than nesting a button inside a div (a11y/KeyActivate applies both to the
;; enclosing element).
(e/defn SourceCard [source activate!]
  (e/client
    (let [{:keys [id icon title desc]} source]
      (dom/div
        (dom/props {:class "import-card"})
        (a11y/KeyActivate {:label (str title " — " desc) :role "button"}
          (fn [_] (activate! id)))
        (dom/On "click" (fn [_] (activate! id)) nil)
        (dom/div (dom/props {:class "import-card-icon"}) (icons/Icon icon :size 24))
        (dom/div (dom/props {:class "import-card-title"}) (dom/text title))
        (dom/div (dom/props {:class "import-card-desc"}) (dom/text desc))))))

;; The primary group carries a modifier class because its card count is odd: three
;; cards in the two-column band would leave a row of one, so that band gives it a
;; single column. The secondary group's six cards divide by both counts.
(e/defn SourceGrid [group activate!]
  (e/client
    (dom/div
      (dom/props {:class (if (= group :primary)
                           "import-grid import-grid-primary"
                           "import-grid")})
      (e/for-by :id [source (sources-in group)]
        (SourceCard source activate!)))))

;; Pre:  `chips` is an `itypes/chips` result; `open-file-dialog!` is 0-arg.
;; Post: the intake area, showing the drag state when `dragging?`.
;;
;; The Browse button is the accessible control, and the box around it is a mouse
;; convenience that opens the same dialog. Dragging is one of three ways in, and
;; the least discoverable: nothing about a dashed rectangle says a click also
;; works. The button says it.
;;
;; The box therefore carries NO role and NO tabindex. A real <button> inside an
;; element with role="button" is nested interactive content, which is invalid, and
;; it would put two stops in the tab order for one action.
(e/defn IntakeArea [dragging? chips open-file-dialog!]
  (e/client
    (dom/div
      (dom/props {:class (str "import-intake" (when dragging? " is-dragging"))})
      (dom/On "click" (fn [_] (open-file-dialog!)) nil)
      (dom/div
        (dom/props {:class "import-intake-prompt"})
        (dom/text (if dragging?
                    "Release to import"
                    "Drop a file, paste a link, or paste text")))
      (dom/button
        (dom/props {:class "btn btn-secondary import-browse" :type "button"})
        (dom/text "Browse files")
        ;; stopPropagation: the click would otherwise also reach the box's handler
        ;; and open a second file dialog.
        (dom/On "click"
          (fn [^js e] (.stopPropagation e) (open-file-dialog!))
          nil))
      (dom/div
        (dom/props {:class "import-chips"})
        (e/for-by :label [chip chips]
          (dom/span
            (dom/props {:class "import-chip"})
            (dom/text (:label chip))
            (when-some [limit (:limit chip)]
              (dom/span
                (dom/props {:class "import-chip-limit"})
                (dom/text (str " · " limit))))))))))

;; The hidden input behind the intake area's click. Its `accept` names every
;; group, because the intake area accepts everything the app imports; the drop
;; router is what actually holds the set (an OS file dialog lets the user switch
;; `accept` off).
(e/defn IntakeFileInput [!input on-files!]
  (e/client
    (dom/input
      (dom/props {:type "file" :multiple true :style {:display "none"}
                  :accept (itypes/accept-attr (map :label itypes/groups))})
      (reset! !input dom/node)
      (dom/On "change"
        (fn [^js e]
          (on-files! (file-vec (-> e .-target .-files)))
          ;; Clear the value so picking the same file twice fires again.
          (set! (-> e .-target .-value) ""))
        nil))))

;; Native <details> owns its own open state and its own keyboard operability, so
;; nothing here writes the `open` attribute: an Electric-managed `:open` prop
;; would fight the browser's toggle, and any string value at all opens the
;; element. Restoring the last state across visits (§7.2.1) and closing on Escape
;; (§7.2.2) are not wired yet.
(e/defn SecondarySources [activate!]
  (e/client
    (dom/details
      (dom/props {:class "import-disclosure"})
      (dom/summary (dom/text "More sources"))
      (SourceGrid :secondary activate!))))

;; ── Page ───────────────────────────────────────────────────────────

(e/defn ImportPage [user-id navigate! _enc-key _llm-enabled?]
  (e/client
    (let [;; Which source is open: a keyword, or falsey when nothing is. The
          ;; modals write `false` here to close themselves.
          !open-source (atom nil)
          open-source (e/watch !open-source)
          ;; What the intake area collected, for the modal that is about to
          ;; mount. Set before !open-source, read once at that modal's mount.
          !intake (atom nil)
          !intake-files (atom nil)
          !dragging? (atom false)
          !message (atom nil)
          !file-input (atom nil)
          ceilings (e/server (e/Offload #(itypes/ceilings* nil user-id)))
          activate! (fn [id]
                      (reset! !message nil)
                      (if (= id :live-doc)
                        (create-live-doc! navigate!)
                        (reset! !open-source id)))
          ;; One entry point for a drop and for the file dialog: `plan-drop` owns
          ;; the rules, and this fn only applies them.
          on-files! (fn [files]
                      (let [plan (plan-drop files)]
                        (reset! !message (:message plan))
                        (case (:action plan)
                          :video (do (reset! !intake-files (:files plan))
                                     (reset! !open-source :video))
                          :single (do (reset! !intake {:file (:file plan)})
                                      (reset! !open-source :upload))
                          nil)))
          on-clipboard! (fn [text html]
                          (when-some [plan (plan-paste text html)]
                            (reset! !message nil)
                            (reset! !intake (:intake plan))
                            (reset! !open-source (:source plan))))
          open-file-dialog! (fn [] (when-some [el @!file-input] (.click el)))
          pending (e/watch !pending-import)]

      ;; A palette-dispatched intent opens its modal once. The watch above is what
      ;; makes this re-run when the palette writes; the fn is what makes it a
      ;; single expression whose value the `when` uses.
      (when pending
        (take-pending-import! !pending-import !open-source))

      ;; Page-scoped document listeners. `e/snapshot` runs the install once per
      ;; mount and yields its teardown fn, which e/on-unmount then owns — the
      ;; listeners die with the page rather than surviving into other tabs.
      (e/on-unmount (e/snapshot (install-page-drag! !dragging? on-files!)))
      (e/on-unmount (e/snapshot (install-page-paste! on-clipboard!)))

      (dom/div
        (dom/props {:class "import-page"})
        (dom/div
          (dom/props {:class "import-page-body"})

          (dom/div
            (dom/h1 (dom/props {:class "import-title"}) (dom/text "Import"))
            (dom/p
              (dom/props {:class "import-intro"})
              (dom/text "Add material to your library. FreeMemo makes cards from it.")))

          ;; The chip row is derived server-side: `ceilings` is a server-form
          ;; binding, and formatting there ships the seven small chip maps instead
          ;; of pulling the ceilings map across to format them here.
          (IntakeArea (e/watch !dragging?) (e/server (itypes/chips ceilings))
            open-file-dialog!)
          (IntakeFileInput !file-input on-files!)

          (when-some [msg (e/watch !message)]
            (dom/div (dom/props {:class "import-message"}) (dom/text msg)))

          (SourceGrid :primary activate!)

          (SecondarySources activate!))

        ;; One mount point for every source's modal. `!intake` / `!intake-files`
        ;; carry what the intake area collected; a card click leaves both nil, so
        ;; the modal opens empty.
        (case open-source
          :upload    (ImportModal !open-source user-id :file navigate! !intake)
          :code      (ForgeImportModal !open-source user-id navigate!)
          ;; No card of its own: ForgeImportModal's "Upload a .zip file instead"
          ;; writes it, the way the intake area writes :upload.
          :code-zip  (ImportModal !open-source user-id :code navigate! !intake)
          :link      (ImportModal !open-source user-id :url navigate! !intake)
          :audio     (ImportModal !open-source user-id :audio navigate! !intake)
          :score     (ImportModal !open-source user-id :score navigate! !intake)
          :paste     (ImportModal !open-source user-id :paste navigate! !intake)
          :new-topic (ImportModal !open-source user-id :new-topic navigate! !intake)
          :video     (VideoImportModal !open-source user-id navigate! !intake-files)
          :poem      (ImportModal !open-source user-id :poem navigate! !intake)
          :zotero    (ZoteroPickerModal !open-source user-id navigate!)
          nil)))))
