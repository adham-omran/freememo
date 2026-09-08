(ns freememo.assistant-component-shape-test
  "Electric boundary contracts for the assistant panel."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]))

(defn- read-source-forms [path]
  (with-open [reader (java.io.PushbackReader. (io/reader path))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read {:eof ::eof :read-cond :allow :features #{:clj}}
                     reader)]
          (if (= ::eof form)
            forms
            (recur (conj forms form))))))))

(defn- calls-to [forms component]
  (filter
    #(and (seq? %) (= component (first %)))
    (tree-seq coll? seq forms)))

(defn- opaque-map-entry? [[key value]]
  (or (= :token key)
      (and (keyword? key) (str/starts-with? (name key) "!"))
      (and (symbol? value) (str/starts-with? (name value) "!"))))

(deftest assistant-components-pass-opaque-state-positionally
  (let [forms (read-source-forms "src/freememo/assistant_panel.cljc")]
    (doseq [component '[submit-effect/SubmitEffect
                        transcript/AssistantTranscript
                        composer/AssistantComposer]]
      (testing (str component " does not serialize atoms or tokens inside a map")
        (let [calls (calls-to forms component)
              config (second (first calls))]
          (is (= 1 (count calls)))
          (is (map? config))
          (is (empty? (filter opaque-map-entry? config))))))))

(deftest transcript-diffs-at-the-server-source
  (let [source (slurp "src/freememo/assistant_transcript.cljc")]
    (is (str/includes? source
          "(e/server\n              (e/diff-by :assistant_messages/id"))
    (is (str/includes? source
          "(e/server (assistant/transcript-message-view message))"))))

(deftest document-mention-list-is-windowed-at-the-server-source
  (let [panel-source (slurp "src/freememo/assistant_panel.cljc")
        composer-source (slurp "src/freememo/assistant_composer.cljc")
        row-source (slurp "src/freememo/assistant_panel_rows.cljc")
        data-source (slurp "src/freememo/assistant_panel_data.cljc")]
    (is (str/includes? panel-source
          "(e/server (assistant/referenceable-docs\n                      user-id root-topic-id mention-query))"))
    (is (str/includes? panel-source "(e/server (count reference-documents))"))
    (is (str/includes? composer-source
          "(rows/ReferenceMenu at-open? reference-documents reference-document-count"))
    (is (not (str/includes? composer-source
               "(when at-open?\n        (rows/ReferenceMenu"))
        "the virtual list stays mounted while Forms5 handles the input event")
    (is (str/includes? composer-source "close-reference-menu-after-input!"))
    (is (str/includes? panel-source ":at-open? at-open?"))
    (is (str/includes? row-source "[freememo.scroll :refer [Scroll-window]]"))
    (is (str/includes? row-source "assistant-panel__mention-menu--closed"))
    (is (str/includes? row-source "(e/server (nth reference-documents i nil))"))
    (is (str/includes? row-source ":reset-key mention-query"))
    (is (not (str/includes? row-source "(if (pos? reference-document-count)")))
    (is (not (str/includes? data-source "referenceable-docs")))))

(deftest rich-answer-is-an-unmanaged-leaf
  (let [source (slurp "src/freememo/assistant_message_content.cljc")]
    (is (str/includes? source "(math/set-html! node safe-html"))
    (is (str/includes? source "(let [node dom/node]")
      "the after-render callback closes over the element, not over dom/node")
    (is (not (str/includes? source "assistant-render-tree")))
    (is (not (str/includes? source "render-tree/RenderTree")))))

(deftest fatal-client-report-does-not-use-electric
  (let [source (slurp "src/freememo/client_boot.cljs")]
    (is (str/includes? source ".sendBeacon"))
    (is (str/includes? source ":keepalive true"))
    (is (str/includes? source "/api/client-reactor-failure"))))

(deftest stream-publication-uses-its-own-bounded-channel
  (let [assistant-source (slurp "src/freememo/assistant.clj")
        commands-source (slurp "src/freememo/commands.cljc")]
    (is (str/includes? assistant-source
          "(commands/bump-channels! user-id #{:assistant-stream})"))
    (is (str/includes? assistant-source "100000000"))
    (is (str/includes? commands-source ":assistant-stream"))))
