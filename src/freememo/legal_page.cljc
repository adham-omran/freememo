(ns freememo.legal-page
  "Public legal documents for any visitor, signed in or not.

   **The words live in `freememo.legal-copy`, not here.** This namespace owns
   structure and markup only. `freememo.crawler-page` renders the same data to
   static HTML for crawlers, which cannot run this page at all — Googlebot's
   renderer has no WebSocket support, so a crawler sees an empty body without it.
   Unlike the landing page, this one keeps NO prose inline: every sentence
   composes from whole parts, so nothing needs a renderer-owned equivalent.

   `freememo.main/Main` dispatches here BEFORE it branches on sign-in, because a
   legal document does not depend on `user-id`. That placement also keeps these
   paths out of the router: the app's router lives inside the authenticated
   branch, and a legal path routed as a tab would produce an `active-tab` that
   matches no branch and render an empty page.

   See plans/legal-pages-and-ads-tag-removal.md."
  (:require
   [freememo.legal-copy :as legal]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(e/defn Fragments
  "One paragraph's fragments, where a fragment is {:t text} with optional :href.
   Pre: `frags` is a vector of fragment maps. Post: link fragments become
   anchors and plain fragments become text, in order.

   Every loop in this namespace is keyed by INDEX, not by content. The copy is
   static prose, so position is the stable identity, and two identical fragments
   in one paragraph are an ordinary edit that content-keying would collide on.
   `freememo.landing-page` keys its FAQ fragments by `:t` — that page's copy
   happens to have no repeats, which is a property of its text rather than a
   guarantee."
  [frags]
  (e/client
    (e/for-by first [entry (map-indexed vector frags)]
      (let [frag (second entry)]
        (if (:href frag)
          (dom/a (dom/props {:href (:href frag)}) (dom/text (:t frag)))
          (dom/text (:t frag)))))))

(e/defn Part
  "One body part, dispatched on `freememo.legal-copy/part-kind`.

   Pre: `part` is a map from a section body.
   Post: a `<p>` for `:text` and `:run`, a `<ul>` for `:list`, and a paragraph
   holding `legal/unrenderable-part-marker` for a part that classifies to nil.
   The shape is NOT re-derived from the part's keys here — `part-kind` is the
   one classifier, shared with `freememo.crawler-page/part-html`, so the two
   renderers cannot disagree about what a part is.
   `cond`, not `case`: Electric compiles `case` as its effect-sequencing
   primitive, and this is ordinary branching (see CLAUDE.md)."
  [part]
  (e/client
    (let [kind (legal/part-kind part)]
      (cond
        (= :text kind)
        (dom/p (dom/props {:class "legal-p"}) (dom/text (:p part)))

        (= :run kind)
        (dom/p (dom/props {:class "legal-p"}) (Fragments (:p part)))

        (= :list kind)
        (dom/ul
          (dom/props {:class "legal-list"})
          ;; Index-keyed, never content-keyed: two identical bullets are a
          ;; legitimate edit to prose, and `e/for-by identity` would collide
          ;; on them.
          (e/for-by first [entry (map-indexed vector (:ul part))]
            (dom/li (dom/text (second entry)))))

        :else
        (dom/p (dom/props {:class "legal-p"})
          (dom/text legal/unrenderable-part-marker))))))

(e/defn Section [section]
  (e/client
    (dom/section
      (dom/props {:class "legal-section"})
      (dom/h2
        (dom/props {:class "legal-section-title"})
        (dom/text (:heading section)))
      ;; Index-keyed for the reason the bullet list above states: two identical
      ;; sentences in one section must not collide.
      (e/for-by first [entry (map-indexed vector (:body section))]
        (Part (second entry))))))

(e/defn LegalPage
  "One legal document, rendered for a visitor.
   Pre: `doc` is a value from `freememo.legal-copy/documents`, never nil — the
   caller decides whether this path is a legal document.
   Post: the whole document, plus a link back to the site root."
  [doc]
  (e/client
    (dom/div
      (dom/props {:class "legal-layout"})
      (dom/main
        (dom/props {:class "legal-main"})
        (dom/h1
          (dom/props {:class "legal-title"})
          (dom/text (:title doc)))
        (dom/p
          (dom/props {:class "legal-updated"})
          (dom/text (str "Last updated " (:updated doc))))
        (e/for-by first [entry (map-indexed vector (:sections doc))]
          (Section (second entry))))
      (dom/footer
        (dom/props {:class "legal-footer"})
        (dom/a (dom/props {:href "/"}) (dom/text "Back to FreeMemo"))
        (dom/a (dom/props {:href (str "mailto:" legal/contact-email)})
          (dom/text "Contact"))))))
