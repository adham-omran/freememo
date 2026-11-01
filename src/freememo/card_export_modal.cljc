(ns freememo.card-export-modal
  "The Export cards dialog (plans/card-export-csv-tsv.md §6).

   One dialog for both entry points. A topic opens it with the topic's set,
   and Library > Cards opens it with the filter opts on screen. The dialog
   counts what the file will hold, then starts a browser download from
   `/api/cards/export`. Nothing is saved, so the dialog has no server commit
   and no token. Its controls only set client atoms, the CLAUDE.md pattern for
   a selection whose effect stays in the browser.

   The request atoms live here, not in `freememo.target-modals`, so Library
   can open the dialog without requiring every modal that namespace mounts."
  (:require [clojure.string :as str]
            [hyperfiddle.electric3 :as e]
            [hyperfiddle.electric-dom3 :as dom]
            [freememo.card-export-presets :as presets]
            [freememo.modal-shell :as modal]
            #?(:clj [freememo.card-export :as export])
            #?(:clj [freememo.card-export-http :as export-http])))

;; {:scope :topic :topic-id n} | {:scope :library :opts m} | nil
(defonce !request (atom nil))
(defonce !open? (atom false))

(defn open!
  "Open the dialog for `request`. Returns nil.
   Pre: `request` is {:scope :topic :topic-id n} or {:scope :library :opts m},
        where `m` is the Library query opts on screen."
  [request]
  (reset! !request request)
  (reset! !open? true)
  nil)

(defn export-summary*
  "What the file for `scope` will hold, and the topic title for a topic scope.
   Post: {:cards :rows :dropped :skipped {kind n} :title s|nil}, or
         {:error s} when the set cannot be read."
  [user-id scope preset-id images?]
  #?(:clj (if-let [{:keys [cards base]} (export-http/card-set user-id scope)]
            (assoc (export/summarize (export/resolve-preset preset-id images?) cards)
              :title (when (= :topic (:scope scope)) base))
            {:error "These cards could not be read."})
     :cljs nil))

(defn download!
  "Start the download of `scope` as `preset-id`. A hidden form does it, so the
   browser handles the attachment and the page stays where it is. The filter
   scope POSTs, because its opts can be larger than a URL.
   Returns nil."
  [scope preset-id images?]
  #?(:cljs
     (let [post? (= :filter (:scope scope))
           fields (cond-> {"scope" (name (:scope scope))
                           "preset" preset-id
                           "images" (if images? "1" "0")}
                    (:topic-id scope) (assoc "topic-id" (str (:topic-id scope)))
                    post? (assoc "opts" (pr-str (:opts scope))))
           form (.createElement js/document "form")]
       (set! (.-method form) (if post? "post" "get"))
       (set! (.-action form) "/api/cards/export")
       (set! (.. form -style -display) "none")
       (doseq [[k v] fields]
         (let [input (.createElement js/document "input")]
           (set! (.-type input) "hidden")
           (set! (.-name input) k)
           (set! (.-value input) v)
           (.appendChild form input)))
       (.appendChild (.-body js/document) form)
       (.submit form)
       (.remove form)
       nil)
     :clj nil))

(def ^:private kind-names
  {"occlusion" ["image occlusion card" "image occlusion cards"]
   "score" ["score card" "score cards"]})

(defn- counted [n [one many]] (str n " " (if (= 1 n) one many)))

(defn summary-lines
  "The dialog's sentences for `summary`.
   Post: a vector of strings, each one sentence."
  [{:keys [cards rows dropped skipped error]}]
  (if error
    [error]
    (cond-> [(str (counted cards ["card becomes" "cards become"]) " "
               (counted rows ["row" "rows"]) ".")]
      (seq skipped)
      (conj (str "Not exported: "
              (str/join ", " (map (fn [[k n]] (counted n (get kind-names k [(str k " card") (str k " cards")])))
                               (sort skipped)))
              ". A text file cannot hold them."))
      (pos? dropped)
      (conj (str (counted dropped ["row has" "rows have"])
              " no front text without images and " (if (= 1 dropped) "is" "are") " left out.")))))

(def ^:private import-help
  {"noji" "In Noji, open a deck, choose Import Cards, and upload the file."
   "mnemosyne" "In Mnemosyne, choose File, Import, then Tab-separated text files."
   "plain-csv" "The first row names the columns."
   "plain-tsv" "The first row names the columns."})

(e/defn PresetSelect [preset-id !preset]
  (e/client
    (dom/select
      (dom/props {:class "input" :aria-label "Format"})
      (e/for [id (e/diff-by identity presets/preset-order)]
        (dom/option
          (dom/props {:value id :selected (= id preset-id)})
          (dom/text (:label (get presets/presets id)))))
      (dom/On "change" (fn [e] (reset! !preset (-> e .-target .-value))) nil))))

(e/defn LibrarySetChoice [library-set !library-set]
  (e/client
    (dom/fieldset
      (dom/props {:class "card-export__set"})
      (dom/legend (dom/text "Cards"))
      (dom/label
        (dom/input
          (dom/props {:type "radio" :name "card-export-set" :checked (= :filter library-set)})
          (dom/On "change" (fn [_] (reset! !library-set :filter)) nil))
        (dom/text "The cards the filter shows"))
      (dom/label
        (dom/input
          (dom/props {:type "radio" :name "card-export-set" :checked (= :all library-set)})
          (dom/On "change" (fn [_] (reset! !library-set :all)) nil))
        (dom/text "All cards")))))

(e/defn CardExportDialog [user-id request]
  (e/client
    (let [!preset (atom "noji") preset-id (e/watch !preset)
          !images? (atom false) images-on? (e/watch !images?)
          !library-set (atom :filter) library-set (e/watch !library-set)
          library? (= :library (:scope request))
          scope (cond
                  (not library?) {:scope :topic :topic-id (:topic-id request)}
                  (= :all library-set) {:scope :all}
                  :else {:scope :filter :opts (:opts request)})
          images? (and images-on? (presets/images-allowed? preset-id))
          ;; A client e/as-vec reads the Offload as nil until it answers, so
          ;; nothing below unmounts while a new count runs (CLAUDE.md, "An
          ;; Empty e/Offload Empties Everything Downstream").
          summary (peek (e/as-vec
                          (e/server (e/Offload #(export-summary* user-id scope preset-id images?)))))
          close! (fn [] (reset! !open? false))]
      (dom/div
        (dom/props {:class "modal-backdrop" :tabindex "-1"})
        (modal/ModalEscape close! "Export cards")
        (dom/On "click" (fn [_] (close!)) nil)
        (dom/div
          (dom/props {:class "modal-content modal-md card-export"})
          (dom/On "click" (fn [e] (.stopPropagation e)) nil)
          (dom/h3 (dom/text (if-let [title (:title summary)]
                              (str "Export cards: " title)
                              "Export cards")))
          (when library?
            (LibrarySetChoice library-set !library-set))
          (dom/label
            (dom/props {:class "card-export__field"})
            (dom/span (dom/text "Format"))
            (PresetSelect preset-id !preset))
          (when (presets/images-allowed? preset-id)
            (dom/label
              (dom/props {:class "card-export__check"})
              (dom/input
                (dom/props {:type "checkbox" :checked images-on?})
                (dom/On "change" (fn [e] (reset! !images? (-> e .-target .-checked))) nil))
              (dom/text "Include images (zip)")))
          (dom/p
            (dom/props {:class "card-export__help"})
            (dom/text (str (get import-help preset-id)
                        (when images? " Unzip the file first, then import cards.txt from that folder."))))
          (dom/div
            (dom/props {:class "card-export__summary" :aria-live "polite"})
            ;; One loop, always mounted, never a branch beside another child
            ;; (CLAUDE.md, "An e/for Gets an Element of Its Own").
            (e/for [line (e/diff-by identity (if summary
                                               (summary-lines summary)
                                               ["Counting cards…"]))]
              (dom/p (dom/text line))))
          (dom/div
            (dom/props {:class "card-export__actions"})
            (dom/button
              (dom/props {:class "btn btn-secondary" :type "button"})
              (dom/text "Cancel")
              (dom/On "click" (fn [_] (close!)) nil))
            (dom/button
              (dom/props {:class "btn btn-primary" :type "button"
                          :disabled (not (pos? (or (:rows summary) 0)))})
              (dom/text "Download")
              (dom/On "click" (fn [_] (download! scope preset-id images?) (close!)) nil))))))))

(e/defn CardExportModal
  "Headless until a request arrives. Mounted once, in `TargetModals`."
  [user-id]
  (e/client
    (let [request (e/watch !request)]
      (when (and (e/watch !open?) request)
        (CardExportDialog user-id request)))))
