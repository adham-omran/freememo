(ns freememo.card-export-presets
  "The card export presets, as plain data both peers read.

   A `.cljc` with no requires and no reader conditional, so the export dialog
   (client) and `freememo.card-export` (server) name the same presets. A
   CLJ-only def read from client scope compiles with no warning and crashes the
   WebSocket at mount (CLAUDE.md, \"Server-Only Functions in .cljc Files\").")

(def presets
  "Preset id → what it writes. The id is the `preset` URL parameter.
   Images are not a preset of their own: the Mnemosyne preset takes an
   \"Include images\" option, and the route turns it into a zip."
  {"noji"      {:label "Noji (CSV)"       :encoding :csv :header? false
                :fields :text :math :noji :ext "csv"}
   "mnemosyne" {:label "Mnemosyne (text)" :encoding :mnemosyne-tsv :header? false
                :fields :html :math :mnemosyne :ext "txt"}
   "plain-csv" {:label "Plain CSV"        :encoding :csv :header? true
                :fields :text :math :keep :ext "csv"}
   "plain-tsv" {:label "Plain TSV"        :encoding :plain-tsv :header? true
                :fields :text :math :keep :ext "tsv"}})

(def preset-order
  "The order the dialog lists the presets in."
  ["noji" "mnemosyne" "plain-csv" "plain-tsv"])

(defn images-allowed?
  "True iff `preset-id` can carry images, which only the Mnemosyne zip does."
  [preset-id]
  (= "mnemosyne" preset-id))
