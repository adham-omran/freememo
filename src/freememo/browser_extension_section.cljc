(ns freememo.browser-extension-section
  "Settings → Browser Extension. Install link and what the extension does.

   Its own namespace because `settings_page` is a shell that composes sections
   — the rule that file's own docstring states. `ZoteroSection` predates the
   rule and still lives inline.

   Read-only. Every preference the Chrome extension has lives in the extension,
   where the service worker can read it without a round trip. Nothing here
   writes.

   See `plans/chrome-capture-extension.md`."
  (:require
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]))

(def ^:private store-listing-url
  "The Chrome Web Store listing.

   A store URL, not a repository path. The Zotero plugin links to
   `/raw/main/extensions/zotero/dist/…`, which breaks whenever that directory
   moves — it did, in the commit that created `extensions/`. A store listing
   is a stable identifier the repository layout cannot invalidate."
  "https://chromewebstore.google.com/detail/freememo-web-capture")

(def ^:private capability-lines
  ["Captures the page as your browser rendered it, including content behind a login."
   "Captures a selection, when you pick Selection in the popup."
   "Falls back to a full-page screenshot where a page cannot be read."
   "Saves an open PDF as a real PDF document, with pages you can scan."
   "Reads the page's citation metadata and attaches it as a source."])

(e/defn BrowserExtensionSection
  "Settings → Browser Extension.

   Pre : `user-id` is non-nil. Unused today — the section is static, and the
         parameter is here because every other section takes one and a future
         per-user capture preference would need it.
   Post: renders an install link and a capability list. Writes nothing."
  [user-id]
  (e/client
    (dom/div
      (dom/props {:class "card"})
      (dom/h3 (dom/props {:class "section-title"}) (dom/text "Browser Extension"))

      (dom/p
        (dom/props {:style {:margin "0 0 12px" :color "var(--color-text-muted)"
                            :font-size "13px" :line-height "1.5"}})
        (dom/text
          (str "FreeMemo Web Capture saves a page into your library from the "
            "browser. The server cannot fetch a page that needs your session, "
            "so the extension captures what you are actually looking at.")))

      (dom/ul
        (dom/props {:style {:margin "0 0 16px" :padding-left "18px"
                            :color "var(--color-text-muted)" :font-size "13px"
                            :line-height "1.7"}})
        ;; `e/for` iterates a DIFFERENTIAL collection. A plain vector has to be
        ;; lifted first, which is what every other call site here does —
        ;; `assistant_panel_rows.cljc:178` over a static string vector is the
        ;; same shape.
        (e/for [line (e/diff-by identity capability-lines)]
          (dom/li (dom/text line))))

      (dom/div
        (dom/props {:style {:display "flex" :align-items "center" :gap "10px"}})
        (dom/a
          (dom/props {:href store-listing-url
                      :target "_blank"
                      :rel "noopener noreferrer"
                      :class "btn btn-secondary"
                      :style {:font-size "13px"}})
          (dom/text "Get the Chrome extension")))

      (dom/p
        (dom/props {:style {:margin "12px 0 0" :color "var(--color-text-muted)"
                            :font-size "12px"}})
        (dom/text
          (str "Sign in here first. The extension uses this browser's session, "
            "so it needs no key and no separate password."))))))
