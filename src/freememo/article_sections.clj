(ns freememo.article-sections
  "Split stored article HTML without changing the source."
  (:require [clojure.string :as str])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Element Node TextNode]))

(def ^:private heading-levels
  {"h1" 1 "h2" 2 "h3" 3 "h4" 4 "h5" 5 "h6" 6})

(def ^:private container-tags
  #{"article" "aside" "div" "footer" "header" "main" "nav" "section"})

(defn- boundary [^Node node mode splitmark]
  (when (instance? Element node)
    (let [element ^Element node
          tag (.normalName element)]
      (case mode
        :headings (when-let [level (heading-levels tag)]
                    {:level level :title (.text element)})
        :lines (when (= "hr" tag) {:delimiter? true})
        :splitmark (when (and (= "p" tag)
                              (= splitmark (str/trim (.wholeText element))))
                     {:delimiter? true})
        nil))))

(defn- boundary-tokens [^Node node ancestors mode splitmark]
  (if-let [mark (boundary node mode splitmark)]
    {:has-boundary? true
     :tokens [{:node node :ancestors ancestors :boundary mark}]}
    (let [children (mapv #(boundary-tokens % (conj ancestors node) mode splitmark)
                        (.childNodes node))]
      (if (some :has-boundary? children)
        {:has-boundary? true :tokens (into [] (mapcat :tokens) children)}
        {:has-boundary? false :tokens [{:node node :ancestors ancestors}]}))))

(defn- block-tokens [^Node node ancestors]
  (if (and (instance? Element node)
           (container-tags (.normalName ^Element node))
           (not (.hasClass ^Element node "ql-code-block-container"))
           (seq (.childNodes node)))
    (mapcat #(block-tokens % (conj ancestors node)) (.childNodes node))
    [{:node node :ancestors ancestors}]))

(defn- meaningful-token? [{:keys [node]}]
  (if (instance? TextNode node)
    (not (str/blank? (.getWholeText ^TextNode node)))
    (instance? Element node)))

(defn- render-tokens
  "Copy tokens and their ancestor wrappers into one HTML fragment.
   Pre: tokens occur in source order with unchanged ancestor paths.
   Post: each token appears once with its wrapper attributes.
   Invariant: the source DOM remains unchanged."
  [tokens]
  (let [document (Jsoup/parseBodyFragment "")
        _ (.prettyPrint (.outputSettings document) false)
        fragment (.body document)]
    (reduce
      (fn [copies {:keys [node ancestors]}]
        (let [[copies parent]
              (reduce (fn [[copies parent] ^Element ancestor]
                        (if-let [copy (get copies ancestor)]
                          [copies copy]
                          (let [copy (.shallowClone ancestor)]
                            (.appendChild ^Element parent copy)
                            [(assoc copies ancestor copy) copy])))
                      [copies fragment] ancestors)]
          (.appendChild ^Element parent (.clone ^Node node))
          copies))
      {} tokens)
    (.html fragment)))

(defn- section-title [html fallback]
  (let [text (-> (Jsoup/parseBodyFragment html) .body .text str/trim)]
    (cond
      (str/blank? text) fallback
      (> (count text) 80) (str (subs text 0 77) "...")
      :else text)))

(defn- append-section [result tokens title parent-index]
  (if (some meaningful-token? tokens)
    (let [content (render-tokens tokens)
          index (count result)]
      (conj result {:index index
                    :parent-index parent-index
                    :title (or (not-empty (some-> title str/trim))
                               (section-title content (str "Section " (inc index))))
                    :content content}))
    result))

(defn- heading-sections [tokens]
  (loop [[token & remaining] tokens
         pending []
         title "Introduction"
         parent-index nil
         stack []
         result []]
    (if-not token
      (append-section result pending title parent-index)
      (if-let [{:keys [level] heading-title :title} (:boundary token)]
        (let [result (append-section result pending title parent-index)
              stack (vec (take-while #(< (:level %) level) stack))
              parent-index (:index (peek stack))
              index (count result)]
          (recur remaining [token] heading-title parent-index
                 (conj stack {:level level :index index}) result))
        (recur remaining (conj pending token) title parent-index stack result)))))

(defn- delimiter-sections [tokens]
  (loop [[token & remaining] tokens
         pending []
         result []]
    (if-not token
      (append-section result pending nil nil)
      (if (:boundary token)
        (recur remaining [] (append-section result pending nil nil))
        (recur remaining (conj pending token) result)))))

(defn- content-blocks [tokens]
  (let [{:keys [blocks]}
        (reduce
          (fn [{:keys [blocks inline-ancestors] :as state} token]
            (let [node ^Node (:node token)
                  block? (and (instance? Element node) (.isBlock ^Element node))
                  inline? (and (not block?) (meaningful-token? token))
                  continue? (and inline? (= inline-ancestors (:ancestors token))
                                 (seq blocks))]
              (cond
                (not (meaningful-token? token))
                (if (seq blocks)
                  (update-in state [:blocks (dec (count blocks))] conj token)
                  state)

                continue?
                (update-in state [:blocks (dec (count blocks))] conj token)

                :else
                {:blocks (conj blocks [token])
                 :inline-ancestors (when inline? (:ancestors token))})))
          {:blocks [] :inline-ancestors nil} tokens)]
    blocks))

(defn sections
  "Return indexed HTML sections for a deterministic boundary mode.
   Pre: HTML is a string or nil. Mode is :headings, :lines, :splitmark, or :blocks.
   Pre: :splitmark requires a nonblank string. Invalid inputs throw ex-info.
   Post: indices are consecutive. Each parent index names an earlier section.
   Post: heading sections contain only their own content.
   Invariant: source HTML does not change. No model or database calls occur.
   The caller owns invalid inputs. This parser owns content and hierarchy errors."
  [html {:keys [mode splitmark] :or {mode :headings}}]
  (when-not (or (nil? html) (string? html))
    (throw (ex-info "Article content must be HTML text." {:type :invalid-content})))
  (when-not (#{:headings :lines :splitmark :blocks} mode)
    (throw (ex-info "Choose a supported article boundary mode." {:type :invalid-mode :mode mode})))
  (when (and (= :splitmark mode) (or (not (string? splitmark)) (str/blank? splitmark)))
    (throw (ex-info "Enter a nonblank custom splitmark." {:type :invalid-splitmark})))
  (if (str/blank? html)
    []
    (let [document (Jsoup/parseBodyFragment html)
          _ (.prettyPrint (.outputSettings document) false)
          body (.body document)
          single-section [{:index 0 :parent-index nil
                           :title (section-title html "Section 1") :content html}]]
      (if (= :blocks mode)
        (let [blocks (->> (.childNodes body)
                          (mapcat #(block-tokens % []))
                          content-blocks)]
          (if (<= (count blocks) 4)
            single-section
            (reduce (fn [result group]
                      (append-section result (mapcat identity group) nil nil))
                    [] (partition-all 4 blocks))))
        (let [walks (mapv #(boundary-tokens % [] mode (some-> splitmark str/trim))
                          (.childNodes body))]
          (if-not (some :has-boundary? walks)
            single-section
            (let [tokens (into [] (mapcat :tokens) walks)]
              (if (= :headings mode)
                (heading-sections tokens)
                (delimiter-sections tokens)))))))))
