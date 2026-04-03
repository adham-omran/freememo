(ns freememo.navigation
  "Navigation state constructors and schema.
   All navigation mutations should use these constructors to ensure
   consistent data shapes. No ad-hoc map construction.")

;; Viewer nav types (stored in !viewer-nav atom in Main):
;; {:type :browse-pdf      :topic-id _ :page _ :origin _}
;; {:type :browse-topic    :topic-id _ :origin _}
;; {:type :learn-session}
;; {:type :subset-review   :root-id _ :root-name _}

(defn nav-browse-pdf [topic-id page origin]
  {:type :browse-pdf :topic-id topic-id :page page :origin origin})

(defn nav-browse-topic [topic-id origin]
  {:type :browse-topic :topic-id topic-id :origin origin})

(defn nav-learn-session []
  {:type :learn-session})

(defn nav-subset-review [root-id root-name]
  {:type :subset-review :root-id root-id :root-name root-name})
