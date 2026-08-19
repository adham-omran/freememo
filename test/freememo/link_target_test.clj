(ns freememo.link-target-test
  "One deftest per row of the affordance table in
   plans/uniform-anchor-links.md — the rows a pure function can decide. The
   popover, the scroll and the toast are browser checks.

   The hrefs are real shapes taken from the production census, not invented
   ones: exrx.net's `../Articulations/...`, SuperMemo's `book-partN.xhtml#...`,
   Wikipedia's `#cite_note-N`, and the `/wiki/Glossary:Extract` root-relative
   form the SuperMemo Help pages use.

   Needs no database: freememo.link-target requires only clojure.string."
  (:require [clojure.test :refer [deftest is testing]]
            [freememo.link-target :as lt]))

(def ^:private exrx "https://exrx.net/WeightExercises/PectoralSternal/BBBenchPress")
(def ^:private blender "https://docs.blender.org/manual/en/latest/interface/window_system/splash.html")

(defn- kind [href base] (:kind (lt/resolve-href href base)))

;; ── Nothing to offer ───────────────────────────────────────────────

(deftest empty-and-unusable-schemes-are-inert
  (testing "an anchor with no href is caret placement only"
    (is (= :inert (kind nil nil)))
    (is (= :inert (kind "" nil)))
    (is (= :inert (kind "   " nil))))
  (testing "schemes this app cannot serve"
    (is (= :inert (kind "ftp://h/f" exrx)))
    (is (= :inert (kind "javascript:alert(1)" exrx)))
    (is (= :inert (kind "data:text/html,x" exrx)))))

(deftest reference-plumbing-is-inert
  (testing "Wikipedia citation machinery — 93.6% of the corpus's fragments"
    (is (= :inert-citation (kind "#cite_note-1" nil)))
    (is (= :inert-citation (kind "#cite_note-smith-2004-14" nil)))
    (is (= :inert-citation (kind "#cite_ref-46" nil)))
    (is (= :inert-citation (kind "#CITEREFBrown1991" nil))))
  (testing "footnotes and numbered references"
    (is (= :inert-citation (kind "#fn1" nil)))
    (is (= :inert-citation (kind "#footnote-2" nil)))
    (is (= :inert-citation (kind "#endnote3" nil)))
    (is (= :inert-citation (kind "#ref-21" nil)))
    (is (= :inert-citation (kind "#n_en101_68" nil))))
  (testing "SuperMemo gap markers and the bare hash"
    (is (= :inert-citation (kind "#g:4" nil)))
    (is (= :inert-citation (kind "#" nil))))
  (testing "an empty fragment on a REAL link is still a real link"
    (let [r (lt/resolve-href "x.html#" exrx)]
      (is (= :external (:kind r)))
      (is (nil? (:fragment r)) "nothing to scroll to, but the page imports"))))

;; ── This document ──────────────────────────────────────────────────

(deftest a-named-fragment-scrolls
  (testing "the 4431 links a heading could actually satisfy"
    (is (= {:kind :fragment :fragment "References"} (lt/resolve-href "#References" nil)))
    (is (= {:kind :fragment :fragment "See_also"} (lt/resolve-href "#See_also" nil)))
    (is (= {:kind :fragment :fragment "intro"} (lt/resolve-href "#intro" exrx)))))

;; ── Inside FreeMemo ────────────────────────────────────────────────

(deftest a-topic-link-navigates-in-app
  (testing "the Sphinx rewrite, 13559 links in production"
    (is (= {:kind :in-app :topic-id 12 :fragment nil}
          (lt/resolve-href "/viewer/topic/12" nil))))
  (testing "with an anchor — what carrying the fragment buys"
    (is (= {:kind :in-app :topic-id 309 :fragment "key-features"}
          (lt/resolve-href "/viewer/topic/309#key-features" nil))))
  (testing "a citation fragment on a topic link still scrolls nowhere"
    (is (= {:kind :in-app :topic-id 309 :fragment nil}
          (lt/resolve-href "/viewer/topic/309#cite_note-2" nil))))
  (testing "a look-alike that is not a topic link"
    (is (= :external (kind "/viewer/topic/12/extra" exrx))))
  (testing "an id too large for the topics.id column is not a topic link"
    ;; `\\d+` is unbounded. Parsing it would overflow Long/parseLong on the JVM
    ;; and silently return 1e20 in the browser — two peers, two behaviours, on
    ;; the one function written to be portable.
    (is (= :external (kind "/viewer/topic/99999999999999999999" exrx)))
    (is (= :no-base (kind "/viewer/topic/99999999999999999999" nil)))
    (is (= :external (kind "/viewer/topic/2147483648" exrx))
      "one past the integer column's maximum")
    (is (= :in-app (kind "/viewer/topic/2147483647" exrx))
      "the maximum itself is still a topic")))

;; ── Out on the web ─────────────────────────────────────────────────

(deftest an-absolute-link-offers-import-and-view
  (let [r (lt/resolve-href "https://e.com/p#f" nil)]
    (is (= :external (:kind r)))
    (testing "Import takes the page, View takes the anchor"
      (is (= "https://e.com/p" (:url r)))
      (is (= "https://e.com/p#f" (:view-url r))))
    (testing ":fragment means \"scroll to this here\" and nothing else"
      (is (nil? (:fragment r))
        "an external page's anchor is not in this document; it rides in :view-url"))))

(deftest a-relative-link-resolves-against-the-document
  (testing "exrx.net's own shape — one level up from the page's directory,
            per RFC 3986 §5.4 (base http://a/b/c/d;p?q + ../g -> http://a/b/g)"
    (let [r (lt/resolve-href "../Articulations/Shoulder#TransverseFlexion" exrx)]
      (is (= :external (:kind r)))
      (is (= "https://exrx.net/WeightExercises/Articulations/Shoulder" (:url r)))
      (is (= "https://exrx.net/WeightExercises/Articulations/Shoulder#TransverseFlexion"
            (:view-url r))
        "View keeps the anchor; only :url is stripped")
      (is (nil? (:fragment r)) "it is not an anchor in THIS document")))
  (testing "a sibling page"
    (is (= "https://exrx.net/WeightExercises/PectoralSternal/Posture"
          (:url (lt/resolve-href "Posture#Protracted" exrx)))))
  (testing "a bare filename with an anchor — SuperMemo's EPUB-internal shape.
            The base is invented: in production those topics carry no source at
            all, which `a-relative-link-with-no-base-can-do-nothing` covers.
            This row exercises the resolver, not the scenario."
    (is (= "https://exrx.net/WeightExercises/PectoralSternal/book-part13.xhtml"
          (:url (lt/resolve-href "book-part13.xhtml#CN-bp-11" exrx)))))
  (testing "root-relative, as the SuperMemo Help pages use"
    (is (= "https://exrx.net/wiki/Glossary:Extract"
          (:url (lt/resolve-href "/wiki/Glossary:Extract" exrx)))))
  (testing "protocol-relative"
    (is (= "https://cdn.example/x.png"
          (:url (lt/resolve-href "//cdn.example/x.png" exrx)))))
  (testing "the Blender manual's cross-directory shape"
    (is (= "https://docs.blender.org/manual/en/latest/interface/regions.html"
          (:url (lt/resolve-href "../regions.html" blender))))))

(deftest a-relative-link-with-no-base-can-do-nothing
  (testing "9680 SuperMemo topics have no source URL at all"
    (is (= :no-base (kind "../Articulations/Shoulder" nil)))
    (is (= :no-base (kind "book-part13.xhtml#CN-bp-11" nil)))
    (is (= :no-base (kind "/wiki/Glossary:Extract" nil))))
  (testing "a non-http base is no base"
    (is (= :no-base (kind "x.html" "file:///home/a/b.html")))))

(deftest resolution-matches-rfc-3986-vectors
  ;; The reference table from RFC 3986 §5.4, against its own base. These are the
  ;; only external oracle this resolver has, and they caught a real defect: a
  ;; path ending in a bare "." or ".." lost its trailing slash.
  ;; Rows the editor never reaches are omitted: "g:h" (foreign scheme), "" and
  ;; "#s" (same document), which resolve-href classifies before resolving.
  (let [base "http://a/b/c/d;p?q"]
    (doseq [[ref want] [["g"           "http://a/b/c/g"]
                        ["./g"         "http://a/b/c/g"]
                        ["g/"          "http://a/b/c/g/"]
                        ["/g"          "http://a/g"]
                        ["//g"         "http://g"]
                        ["?y"          "http://a/b/c/d;p?y"]
                        ["g?y"         "http://a/b/c/g?y"]
                        ["g#s"         "http://a/b/c/g"]
                        [";x"          "http://a/b/c/;x"]
                        ["g;x"         "http://a/b/c/g;x"]
                        ["."           "http://a/b/c/"]
                        ["./"          "http://a/b/c/"]
                        [".."          "http://a/b/"]
                        ["../"         "http://a/b/"]
                        ["../g"        "http://a/b/g"]
                        ["../.."       "http://a/"]
                        ["../../"      "http://a/"]
                        ["../../g"     "http://a/g"]
                        ["../../../g"  "http://a/g"]
                        ["/./g"        "http://a/g"]
                        ["/../g"       "http://a/g"]
                        ["g."          "http://a/b/c/g."]
                        [".g"          "http://a/b/c/.g"]
                        ["g.."         "http://a/b/c/g.."]
                        ["..g"         "http://a/b/c/..g"]]]
      (testing (str "ref " (pr-str ref))
        (is (= want (:url (lt/resolve-href ref base))))))))

;; ── Handled by the OS ──────────────────────────────────────────────

(deftest mail-and-phone-go-to-the-handler
  (is (= {:kind :handler :url "mailto:a@b.c"} (lt/resolve-href "mailto:a@b.c" nil)))
  (is (= {:kind :handler :url "tel:+1234"} (lt/resolve-href "tel:+1234" nil))))

;; ── The dupe key ───────────────────────────────────────────────────

(deftest the-fragment-never-reaches-the-dupe-key
  ;; :url is the ONLY dupe key. There is deliberately no second public way to
  ;; derive one, so nothing can drift from this.
  (testing "page.html#a and page.html#b are one page"
    (is (= (:url (lt/resolve-href "https://e.com/p#a" nil))
          (:url (lt/resolve-href "https://e.com/p#b" nil))))
    (is (= "https://e.com/p" (:url (lt/resolve-href "https://e.com/p#a" nil)))))
  (testing "a link with no fragment gives the same key"
    (is (= "https://e.com/p" (:url (lt/resolve-href "https://e.com/p" nil)))))
  (testing "and through a relative href too"
    (is (= (:url (lt/resolve-href "x.html#a" exrx))
          (:url (lt/resolve-href "x.html#b" exrx))))))

;; ── It must never throw inside a click handler ─────────────────────

(deftest malformed-input-degrades-rather-than-throws
  (doseq [href ["://" "#" "http://" "///" "?q=1" ".." "%%%" "a b c"
                "mailto:" "https://e.com/a b/c#d e"
                "/viewer/topic/99999999999999999999"
                "/viewer/topic/0" "/viewer/topic/007"]]
    (testing (str "href " (pr-str href))
      (is (keyword? (kind href exrx)))
      (is (keyword? (kind href nil))))))
