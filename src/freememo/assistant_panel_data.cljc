(ns freememo.assistant-panel-data
  "Reactive data for the assistant panel."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [freememo.assistant-modes :as modes]
   [freememo.card-models :as card-models]
   #?(:clj [freememo.assistant :as assistant])
   #?(:clj [freememo.user-state :as us])
   #?(:clj [freememo.settings :as settings])))

(defn mention-query
  "Return the unfinished document mention at the end of a draft."
  [draft]
  (some-> (re-find #"(?:^|\s)@([^\s@]*)$" (or draft "")) second))

(defn active-document-mention?
  "Return true only while a draft ends with an unfinished `@` mention.

   Pre: `draft` is the current composer text.
   Post: false removes the document menu.
   Invariant: deleting `@` makes this false."
  [draft]
  (some? (mention-query draft)))

(defn- mention-text
  "The mention a reference to `title` writes into the draft."
  [title]
  (str "@" title))

(defn draft-with-reference
  "Replace the unfinished trailing `@` mention with a reference to `title`.

   The mention STAYS IN THE MESSAGE TEXT. Picking a document used to delete the
   `@…` the learner typed and show the attachment as a chip beside the composer,
   and chips are cleared on send — so the sent turn recorded nothing about what
   it was asked to read, in the transcript or after a reload.

   Pre:  `draft` is the composer text, ending in the mention being completed.
   Post: the returned draft ends with `@<title> `, so the next word starts a new
         word rather than extending the mention.
   Invariant: no regex replacement string is built from `title` — a title
   carrying `$` or a backslash is inserted verbatim."
  [draft title]
  (let [text (or draft "")
        partial (re-find #"@[^\s@]*$" text)
        base (if partial (subs text 0 (- (count text) (count partial))) text)]
    (str base (mention-text title) " ")))

(defn draft-without-reference
  "Remove the reference to `title` from `draft`.

   Removing the chip and leaving its `@<title>` in the text would name a
   document the send no longer attaches, so the two representations are written
   and cleared together.

   A match must end the text or be followed by a space, so removing `@Zig` from
   a draft that says `@Zigzag` leaves that word alone. It cannot separate `@Zig`
   from `@Zig Guide`: a title may contain spaces, so nothing in the text says
   which of the two a match belongs to, and the first whole match wins.

   Pre:  `draft` is the composer text.
   Post: the first whole `@<title>` is gone, with its trailing space if it had
         one.
   Invariant: an absent mention returns the draft unchanged."
  [draft title]
  (let [text (or draft "")
        mention (mention-text title)
        width (count mention)
        at (loop [from 0]
             (when-let [found (str/index-of text mention from)]
               (let [end (+ found width)]
                 (if (or (= end (count text)) (= \space (nth text end)))
                   found
                   (recur (inc found))))))]
    (if (nil? at)
      text
      (let [end (+ at width)
            end (if (and (< end (count text)) (= \space (nth text end)))
                  (inc end)
                  end)]
        (str (subs text 0 at) (subs text end))))))

(e/defn AssistantPanelData
  [user-id root-topic-id active details-run pending-mode pending-web-search?
   model-revision]
  (e/client
    (let [assistant-rev
          (e/server (e/watch (us/get-atom user-id :assistant-mutations)))
          stream-rev
          (e/server (e/watch (us/get-atom user-id :assistant-stream)))
          chat-options
          (e/server
            (mapv
              (fn [chat]
                {:id (:assistant_chats/id chat)
                 :label (or (:assistant_chats/title chat) "Untitled")
                 :mode (:assistant_chats/mode chat)
                 :web-search? (:assistant_chats/web_search chat)})
              (assistant/chats* assistant-rev user-id root-topic-id)))
          active-run
          (e/server
            (assistant/active-run* [assistant-rev stream-rev] user-id active))
          details
          (e/server
            (assistant/run-details* assistant-rev user-id details-run))
          mode-entry
          (modes/resolve-mode
            (or (some
                  (fn [chat]
                    (when (= active (:id chat)) (:mode chat)))
                  chat-options)
              pending-mode))
          active-chat
          (some #(when (= active (:id %)) %) chat-options)
          web-search?
          (if active-chat
            (:web-search? active-chat)
            pending-web-search?)
          model-state
          (e/server [(settings/effective-assistant-model user-id root-topic-id)
                     model-revision])
          model-id
          (first model-state)
          web-search-supported?
          (card-models/native-web-search-supported? model-id)]
      {:chat-options chat-options
       :active-run active-run
       :details details
       :sending? (some? active-run)
       :mode-entry mode-entry
       :web-search? (boolean web-search?)
       :web-search-supported? web-search-supported?})))
