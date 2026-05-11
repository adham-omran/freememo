(ns freememo.navigation
  "Navigation state constructors and schema.
   All navigation mutations should use these constructors to ensure
   consistent data shapes. No ad-hoc map construction.")

;; Viewer nav types (stored in !viewer-nav atom in Main):
;; {:type :topic           :topic-id _ :origin _}
;; {:type :learn-session}
;; {:type :subset-review   :root-id _ :root-name _}

(defn nav-topic [topic-id origin]
  {:type :topic :topic-id topic-id :origin origin})

(defn nav-learn-session []
  {:type :learn-session})

(defn nav-subset-review [root-id root-name]
  {:type :subset-review :root-id root-id :root-name root-name})
