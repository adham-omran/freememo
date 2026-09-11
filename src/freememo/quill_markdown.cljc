(ns freememo.quill-markdown
  "Markdown shortcuts for Quill 2 editors.

   Block shortcuts run when a marker starts the current line.
   Inline shortcuts run when a closing marker reaches the caret. The extension
   changes the existing Quill content, so nested formats remain intact."
  (:require [clojure.string :as str]))

(defn block-shortcut
  "Return the block operation for `text`, or nil.

   `text` contains the current line from its start through the caret."
  [text]
  (let [text (str/replace text \u00a0 \space)]
    (cond
      (re-find #"^#{1,6} " text)
      (let [marker (re-find #"^#{1,6} " text)]
        {:marker-length (count marker)
         :format "header"
         :value (dec (count marker))})

      (str/starts-with? text "> ")
      {:marker-length 2 :format "blockquote" :value true}

      (or (str/starts-with? text "- ") (str/starts-with? text "* "))
      {:marker-length 2 :format "list" :value "bullet"}

      (re-find #"^[0-9]+\. " text)
      {:marker-length (count (re-find #"^[0-9]+\. " text))
       :format "list" :value "ordered"}

      (str/starts-with? text "[ ] ")
      {:marker-length 4 :format "list" :value "unchecked"}

      (or (str/starts-with? text "[x] ") (str/starts-with? text "[X] "))
      {:marker-length 4 :format "list" :value "checked"}

      (= "```" text)
      {:marker-length 3 :format "code-block" :value "plain"}

      :else nil)))

(defn- inline-match [pattern text kind]
  (when-let [match (re-find pattern text)]
    (let [whole (first match)
          marker (second match)
          content (nth match 2)]
      {:kind kind
       :start (str/index-of text whole)
       :marker-length (count marker)
       :content-length (count content)})))

(defn inline-shortcut
  "Return the first complete inline operation before the caret, or nil.

   The parser ignores blank content. It gives paired markers precedence over
   single emphasis markers."
  [text]
  (or
    (inline-match #"(\*\*)(\S(?:.*?\S)?)(?:\*\*)" text "bold")
    (inline-match #"(__)(\S(?:.*?\S)?)(?:__)" text "bold")
    (inline-match #"(~~)(\S(?:.*?\S)?)(?:~~)" text "strike")
    (when-let [[whole label url]
               (re-find #"\[([^\]\n]+)\]\(([^\s()\n]+)\)" text)]
      {:kind "link"
       :start (str/index-of text whole)
       :label-length (count label)
       :url url
       :tail-length (+ 3 (count url))})
    (inline-match #"(`)([^`\s](?:[^`\n]*?[^`\s])?)(?:`)" text "code")
    (when-let [[whole boundary marker content]
               (or (re-find #"(^|\s|\(|\[|\{)(\*)([^*\s](?:[^*\n]*?[^*\s])?)(?:\*)" text)
                   (re-find #"(^|\s|\(|\[|\{)(_)([^_\s](?:[^_\n]*?[^_\s])?)(?:_)" text))]
      (let [boundary-length (count boundary)]
        {:kind "italic"
         :start (+ (str/index-of text whole) boundary-length)
         :marker-length (count marker)
         :content-length (count content)}))))

(defn fenced-ranges
  "Return paired fence indexes from `lines`.

   An unmatched final fence stays as text."
  [lines]
  (loop [index 0 open nil result []]
    (if (= index (count lines))
      result
      (if (re-matches #"```[^`]*" (nth lines index))
        (if open
          (recur (inc index) nil (conj result [open index]))
          (recur (inc index) index result))
        (recur (inc index) open result)))))

#?(:cljs
   (defn- current-line-context [^js ed]
     (when-let [^js selection (.getSelection ed)]
       (when (zero? (.-length selection))
         (let [cursor (.-index selection)
               line-result (.getLine ed cursor)
               line (aget line-result 0)
               offset (aget line-result 1)]
           (when line
             {:cursor cursor
              :line-start (- cursor offset)
              :text (.getText ed (- cursor offset) offset)}))))))

#?(:cljs
   (defn- inserted-range [^js delta]
     (loop [ops (seq (array-seq (.-ops delta)))
            index 0
            start nil
            end nil]
       (if-let [op (first ops)]
         (let [retain (aget op "retain")
               insert (aget op "insert")]
           (cond
             retain (recur (next ops) (+ index retain) start end)
             (string? insert)
             (recur (next ops) (+ index (count insert))
               (or start index) (+ index (count insert)))
             insert (recur (next ops) (inc index) (or start index) (inc index))
             :else (recur (next ops) index start end)))
         (when (and start end (> (- end start) 1)) [start end])))))

#?(:cljs
   (defn- apply-block-shortcut! [^js ed {:keys [cursor line-start text]}]
     (when-let [{:keys [marker-length format value]} (block-shortcut text)]
       (.deleteText ed line-start marker-length "user")
       (.formatLine ed line-start 1 format value "user")
       (.setSelection ed (- cursor marker-length) 0 "silent")
       true)))

#?(:cljs
   (defn- apply-inline-shortcut! [^js ed {:keys [cursor line-start text]}]
     (when-let [{:keys [kind start marker-length content-length label-length
                        url tail-length]}
                (inline-shortcut text)]
       (let [absolute-start (+ line-start start)]
         (if (= "link" kind)
           (do
             ;; Remove the URL tail and brackets without replacing the label.
             ;; Existing formats on the label stay in the Delta.
             (.deleteText ed (+ absolute-start 1 label-length) tail-length "user")
             (.deleteText ed absolute-start 1 "user")
             (.formatText ed absolute-start label-length "link" url "user")
             (.setSelection ed (- cursor tail-length 1) 0 "silent")
             (.format ed "link" false "user"))
           (let [content-start (+ absolute-start marker-length)
                 closing-start (+ content-start content-length)]
             (.deleteText ed closing-start marker-length "user")
             (.deleteText ed absolute-start marker-length "user")
             (.formatText ed absolute-start content-length kind true "user")
             (.setSelection ed (- cursor (* 2 marker-length)) 0 "silent")
             (.format ed kind false "user")))
         true))))

#?(:cljs
   (defn- line-context-at [^js ed index cursor]
     (let [line-result (.getLine ed index)
           line (aget line-result 0)]
       (when line
         (let [line-start (.getIndex ed line)
               text-length (max 0 (dec (.length line)))]
           {:cursor cursor
            :line-start line-start
            :text (.getText ed line-start text-length)})))))

#?(:cljs
   (defn- line-entries [^js ed start end]
     (loop [index start result []]
       (if (> index end)
         result
         (let [line-result (.getLine ed index)
               line (aget line-result 0)]
           (if-not line
             result
             (let [line-start (.getIndex ed line)
                   line-length (.length line)]
               (if (some #(= line-start (:start %)) result)
                 result
                 (recur (+ line-start line-length)
                   (conj result {:start line-start
                                 :length line-length
                                 :text (.getText ed line-start
                                         (max 0 (dec line-length)))}))))))))))

#?(:cljs
   (defn- delete-preserve-caret! [^js ed index length]
     (when-let [^js selection (.getSelection ed)]
       (let [cursor (.-index selection)
             old-length (.getLength ed)]
         (.deleteText ed index length "user")
         (let [removed (- old-length (.getLength ed))]
           (.setSelection ed
             (if (> cursor index)
               (- cursor (min removed (- cursor index)))
               cursor)
             0 "silent"))))))

#?(:cljs
   (defn- convert-entry! [^js ed line-start]
     (when-let [^js selection (.getSelection ed)]
       (when-let [context (line-context-at ed line-start (.-index selection))]
         (let [format (.getFormat ed line-start 1)]
           (when-not (aget format "code-block")
             (apply-block-shortcut! ed context)
             (loop [attempt 0]
               (when (< attempt 50)
                 (when-let [^js current-selection (.getSelection ed)]
                   (when-let [current (line-context-at ed line-start
                                        (.-index current-selection))]
                     (when (apply-inline-shortcut! ed current)
                       (recur (inc attempt)))))))))))))

#?(:cljs
   (defn- convert-fence! [^js ed entries open-index close-index]
     (let [open-entry (nth entries open-index)
           close-entry (nth entries close-index)]
       (doseq [{:keys [start]} (subvec entries (inc open-index) close-index)]
         (.formatLine ed start 1 "code-block" "plain" "user"))
       (delete-preserve-caret! ed (:start close-entry) (:length close-entry))
       (delete-preserve-caret! ed (:start open-entry) (:length open-entry)))))

#?(:cljs
   (defn- convert-range! [^js ed start end]
     (let [entries (line-entries ed start end)
           fences (fenced-ranges (mapv :text entries))
           fenced-indexes (set (mapcat (fn [[from to]] (range from (inc to))) fences))]
       ;; Work from the bottom so edits do not invalidate an earlier line index.
       (doseq [[entry-index entry] (reverse (map-indexed vector entries))]
         (when-not (fenced-indexes entry-index)
           (convert-entry! ed (:start entry))))
       ;; Re-read indexes after the marker deletions above.
       (when-let [^js selection (.getSelection ed)]
         (let [current-entries (line-entries ed start (.-index selection))
               current-fences (fenced-ranges (mapv :text current-entries))]
           (doseq [[open-index close-index] (reverse current-fences)]
             (convert-fence! ed current-entries open-index close-index)))))))

(defn install!
  "Install Markdown shortcuts on `ed` and return an idempotent teardown.

   Quill reports text changes before it updates the selection. A zero-delay
   callback reads the final caret and applies the pending shortcuts. The guard
   prevents the extension from reading an editor after teardown."
  [ed]
  #?(:clj nil
     :cljs
     (let [^js ed ed
           !active? (atom true)
           !scheduled? (atom false)
           !inserted-range (atom nil)
           run! (fn []
                  (reset! !scheduled? false)
                  (when @!active?
                    (if-let [[start end] @!inserted-range]
                      (do (reset! !inserted-range nil)
                          (convert-range! ed start end))
                      (when-let [context (current-line-context ed)]
                        (let [format-index (max 0 (dec (:cursor context)))
                              format (.getFormat ed format-index 1)]
                          (when-not (or (aget format "code-block")
                                        (aget format "code"))
                            (or (apply-block-shortcut! ed context)
                                (apply-inline-shortcut! ed context))))))))
           on-text-change (fn [delta _old source]
                            (when (and @!active?
                                       (= source "user"))
                              (when-let [[start end] (inserted-range delta)]
                                (reset! !inserted-range [start end]))
                              (when (compare-and-set! !scheduled? false true)
                                (js/setTimeout run! 0))))]
       (.on ed "text-change" on-text-change)
       (fn []
         (when @!active?
           (reset! !active? false)
           (.off ed "text-change" on-text-change))))))
