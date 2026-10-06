(ns freememo.icon-paths
  "Lucide-derived SVG path data, and a string renderer for plain-DOM callers.
   All Lucide paths are copied verbatim from lucide.dev (ISC license). Entries
   added for the format menu come from lucide-static 1.52.0; earlier entries
   predate that pin.

   Two renderers read `paths`, which is why this namespace has no requires:
   `freememo.icons/Icon` (Electric, builds SVG nodes) and `svg-markup` below
   (a string, for `freememo.format-menu`, which builds its card with innerHTML).

   Every entry draws on a 24×24 grid under Lucide's stroke contract: no fill,
   currentColor stroke, width 2, round caps and joins. The few entries that are
   NOT Lucide say so, and keep that grid and contract so they sit at one weight
   beside the Lucide ones.")

(def paths
  "Icon keyword → vector of [tag attrs] child descriptors of the <svg>."
  {:sparkles
   [[:path {:d "M9.937 15.5A2 2 0 0 0 8.5 14.063l-6.135-1.582a.5.5 0 0 1 0-.962L8.5 9.936A2 2 0 0 0 9.937 8.5l1.582-6.135a.5.5 0 0 1 .963 0L14.063 8.5A2 2 0 0 0 15.5 9.937l6.135 1.581a.5.5 0 0 1 0 .964L15.5 14.063a2 2 0 0 0-1.437 1.437l-1.582 6.135a.5.5 0 0 1-.963 0z"}]
    [:path {:d "M20 3v4"}]
    [:path {:d "M22 5h-4"}]
    [:path {:d "M4 17v2"}]
    [:path {:d "M5 18H3"}]]

   :pen-line
   [[:path {:d "M12 20h9"}]
    [:path {:d "M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z"}]]

   ;; Composite: pen-line + small sparkle marks adjacent to the nib.
   ;; Signals "AI-assisted writing" — used for Generate-with-Prompt.
   :pen-sparkles
   [[:path {:d "M12 20h9"}]
    [:path {:d "M16.376 3.622a1 1 0 0 1 3.002 3.002L7.368 18.635a2 2 0 0 1-.855.506l-2.872.838a.5.5 0 0 1-.62-.62l.838-2.872a2 2 0 0 1 .506-.854z"}]
    [:path {:d "M5 3v3"}]
    [:path {:d "M3.5 4.5h3"}]
    [:path {:d "M22 9v3"}]
    [:path {:d "M20.5 10.5h3"}]]

   ;; Two nodes + crossing connectors — signals "compare / branch". Used for
   ;; the Compare-models menu item (was a duplicate :sparkles).
   :git-compare
   [[:circle {:cx "18" :cy "18" :r "3"}]
    [:circle {:cx "6" :cy "6" :r "3"}]
    [:path {:d "M13 6h3a2 2 0 0 1 2 2v7"}]
    [:path {:d "M11 18H8a2 2 0 0 1-2-2V9"}]]

   :scissors
   [[:circle {:cx "6" :cy "6" :r "3"}]
    [:path {:d "M8.12 8.12 12 12"}]
    [:path {:d "M20 4 8.12 15.88"}]
    [:circle {:cx "6" :cy "18" :r "3"}]
    [:path {:d "M14.8 14.8 20 20"}]]

   :plus
   [[:path {:d "M5 12h14"}]
    [:path {:d "M12 5v14"}]]

   ;; Question curation (quiz + Questions bank): flag = "revisit later", eye /
   ;; eye-off = drawn / withheld. Distinct silhouettes because the two axes are
   ;; independent and sit side by side.
   :flag
   [[:path {:d "M4 15s1-1 4-1 5 2 8 2 4-1 4-1V3s-1 1-4 1-5-2-8-2-4 1-4 1z"}]
    [:line {:x1 "4" :x2 "4" :y1 "22" :y2 "15"}]]

   :eye
   [[:path {:d "M2.062 12.348a1 1 0 0 1 0-.696 10.75 10.75 0 0 1 19.876 0 1 1 0 0 1 0 .696 10.75 10.75 0 0 1-19.876 0"}]
    [:circle {:cx "12" :cy "12" :r "3"}]]

   :eye-off
   [[:path {:d "M10.733 5.076a10.744 10.744 0 0 1 11.205 6.575 1 1 0 0 1 0 .696 10.747 10.747 0 0 1-1.444 2.49"}]
    [:path {:d "M14.084 14.158a3 3 0 0 1-4.242-4.242"}]
    [:path {:d "M17.479 17.499a10.75 10.75 0 0 1-15.417-5.151 1 1 0 0 1 0-.696 10.75 10.75 0 0 1 4.446-5.143"}]
    [:path {:d "m2 2 20 20"}]]

   :download
   [[:path {:d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"}]
    [:polyline {:points "7 10 12 15 17 10"}]
    [:line {:x1 "12" :x2 "12" :y1 "15" :y2 "3"}]]

   :upload
   [[:path {:d "M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"}]
    [:polyline {:points "17 8 12 3 7 8"}]
    [:line {:x1 "12" :x2 "12" :y1 "3" :y2 "15"}]]

   :mic
   [[:path {:d "M12 2a3 3 0 0 0-3 3v7a3 3 0 0 0 6 0V5a3 3 0 0 0-3-3Z"}]
    [:path {:d "M19 10v2a7 7 0 0 1-14 0v-2"}]
    [:line {:x1 "12" :x2 "12" :y1 "19" :y2 "22"}]]

   :music
   [[:path {:d "M9 18V5l12-2v13"}]
    [:circle {:cx "6" :cy "18" :r "3"}]
    [:circle {:cx "18" :cy "16" :r "3"}]]

   ;; Lucide `video` — the Import tile and the video kind badge.
   :video
   [[:path {:d "m16 13 5.223 3.482a.5.5 0 0 0 .777-.416V7.87a.5.5 0 0 0-.752-.432L16 10.5"}]
    [:rect {:x "2" :y "6" :width "14" :height "12" :rx "2"}]]

   :rect-select
   [[:path {:d "M5 3a2 2 0 0 0-2 2"}]
    [:path {:d "M19 3a2 2 0 0 1 2 2"}]
    [:path {:d "M21 19a2 2 0 0 1-2 2"}]
    [:path {:d "M5 21a2 2 0 0 1-2-2"}]
    [:path {:d "M9 3h1"}]
    [:path {:d "M14 3h1"}]
    [:path {:d "M9 21h1"}]
    [:path {:d "M14 21h1"}]
    [:path {:d "M3 9v1"}]
    [:path {:d "M3 14v1"}]
    [:path {:d "M21 9v1"}]
    [:path {:d "M21 14v1"}]]

   :cloud-download
   [[:path {:d "M12 13v8l-4-4"}]
    [:path {:d "m12 21 4-4"}]
    [:path {:d "M4.393 15.269A7 7 0 1 1 15.71 8h1.79a4.5 4.5 0 0 1 2.436 8.284"}]]

   :refresh-cw
   [[:path {:d "M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8"}]
    [:path {:d "M21 3v5h-5"}]
    [:path {:d "M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16"}]
    [:path {:d "M3 21v-5h5"}]]

   :check
   [[:path {:d "M20 6 9 17l-5-5"}]]

   :rotate-ccw
   [[:path {:d "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8"}]
    [:path {:d "M3 3v5h5"}]]

   :trash-2
   [[:path {:d "M3 6h18"}]
    [:path {:d "M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"}]
    [:path {:d "M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"}]
    [:line {:x1 "10" :x2 "10" :y1 "11" :y2 "17"}]
    [:line {:x1 "14" :x2 "14" :y1 "11" :y2 "17"}]]

   :x
   [[:path {:d "M18 6 6 18"}]
    [:path {:d "m6 6 12 12"}]]

   :more-vertical
   [[:circle {:cx "12" :cy "12" :r "1"}]
    [:circle {:cx "12" :cy "5" :r "1"}]
    [:circle {:cx "12" :cy "19" :r "1"}]]

   ;; The nav rail's Keep open control.
   :panel-left
   [[:rect {:width "18" :height "18" :x "3" :y "3" :rx "2"}]
    [:path {:d "M9 3v18"}]]

   ;; The phone bar's control that opens the nav rail as a drawer.
   :menu
   [[:line {:x1 "4" :x2 "20" :y1 "12" :y2 "12"}]
    [:line {:x1 "4" :x2 "20" :y1 "6" :y2 "6"}]
    [:line {:x1 "4" :x2 "20" :y1 "18" :y2 "18"}]]

   :link
   [[:path {:d "M10 13a5 5 0 0 0 7.54.54l3-3a5 5 0 0 0-7.07-7.07l-1.72 1.71"}]
    [:path {:d "M14 11a5 5 0 0 0-7.54-.54l-3 3a5 5 0 0 0 7.07 7.07l1.71-1.71"}]]

   :clipboard
   [[:rect {:width "8" :height "4" :x "8" :y "2" :rx "1" :ry "1"}]
    [:path {:d "M16 4h2a2 2 0 0 1 2 2v14a2 2 0 0 1-2 2H6a2 2 0 0 1-2-2V6a2 2 0 0 1 2-2h2"}]]

   :file-plus
   [[:path {:d "M15 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7Z"}]
    [:polyline {:points "14 2 14 8 20 8"}]
    [:line {:x1 "12" :x2 "12" :y1 "18" :y2 "12"}]
    [:line {:x1 "9" :x2 "15" :y1 "15" :y2 "15"}]]

   :loader-2
   [[:path {:d "M21 12a9 9 0 1 1-6.219-8.56"}]]

   :library
   [[:path {:d "m16 6 4 14"}]
    [:path {:d "M12 6v14"}]
    [:path {:d "M8 8v12"}]
    [:path {:d "M4 4v16"}]]

   :scan-text
   [[:path {:d "M3 7V5a2 2 0 0 1 2-2h2"}]
    [:path {:d "M17 3h2a2 2 0 0 1 2 2v2"}]
    [:path {:d "M21 17v2a2 2 0 0 1-2 2h-2"}]
    [:path {:d "M7 21H5a2 2 0 0 1-2-2v-2"}]
    [:path {:d "M7 8h8"}]
    [:path {:d "M7 12h10"}]
    [:path {:d "M7 16h6"}]]

   :code
   [[:path {:d "m16 18 6-6-6-6"}]
    [:path {:d "m8 6-6 6 6 6"}]]

   :chevron-down
   [[:path {:d "m6 9 6 6 6-6"}]]

   :chevron-up
   [[:path {:d "m18 15-6-6-6 6"}]]

   :arrow-left
   [[:path {:d "m12 19-7-7 7-7"}]
    [:path {:d "M19 12H5"}]]

   :route
   [[:circle {:cx "6" :cy "19" :r "2"}]
    [:circle {:cx "18" :cy "5" :r "2"}]
    [:path {:d "M6 17c0-7 12-5 12-10"}]]

   :history
   [[:path {:d "M3 12a9 9 0 1 0 3-6.7L3 8"}]
    [:path {:d "M3 3v5h5"}]
    [:path {:d "M12 7v5l4 2"}]]

   :pin
   [[:path {:d "M12 17v5"}]
    [:path {:d "M9 10.76a2 2 0 0 1-1.11 1.79l-1.78.9A2 2 0 0 0 5 15.24V16a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-.76a2 2 0 0 0-1.11-1.79l-1.78-.9A2 2 0 0 1 15 10.76V7a1 1 0 0 1 1-1 2 2 0 0 0 0-4H8a2 2 0 0 0 0 4 1 1 0 0 1 1 1z"}]]

   :clock
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:polyline {:points "12 6 12 12 16 14"}]]

   :home
   [[:path {:d "m3 9 9-7 9 7v11a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"}]
    [:polyline {:points "9 22 9 12 15 12 15 22"}]]

   :search
   [[:circle {:cx "11" :cy "11" :r "8"}]
    [:path {:d "m21 21-4.3-4.3"}]]

   :settings
   [[:path {:d "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"}]
    [:circle {:cx "12" :cy "12" :r "3"}]]

   :graduation-cap
   [[:path {:d "M22 10v6"}]
    [:path {:d "M2 10l10-5 10 5-10 5z"}]
    [:path {:d "M6 12v5c3 3 9 3 12 0v-5"}]]

   :book-open
   [[:path {:d "M2 3h6a4 4 0 0 1 4 4v14a3 3 0 0 0-3-3H2z"}]
    [:path {:d "M22 3h-6a4 4 0 0 0-4 4v14a3 3 0 0 1 3-3h7z"}]]

   ;; Toast level glyphs (lucide: circle-check / circle-alert / triangle-alert / info)
   :circle-check
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "m9 12 2 2 4-4"}]]

   :circle-alert
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:line {:x1 "12" :x2 "12" :y1 "8" :y2 "12"}]
    [:line {:x1 "12" :x2 "12.01" :y1 "16" :y2 "16"}]]

   :triangle-alert
   [[:path {:d "m21.73 18-8-14a2 2 0 0 0-3.48 0l-8 14A2 2 0 0 0 4 21h16a2 2 0 0 0 1.73-3Z"}]
    [:path {:d "M12 9v4"}]
    [:path {:d "M12 17h.01"}]]

   :info
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "M12 16v-4"}]
    [:path {:d "M12 8h.01"}]]

   :chart-column
   [[:path {:d "M3 3v16a2 2 0 0 0 2 2h16"}]
    [:path {:d "M18 17V9"}]
    [:path {:d "M13 17V5"}]
    [:path {:d "M8 17v-3"}]]

   :circle-help
   [[:circle {:cx "12" :cy "12" :r "10"}]
    [:path {:d "M9.09 9a3 3 0 0 1 5.83 1c0 2-3 3-3 3"}]
    [:path {:d "M12 17h.01"}]]

   :share-2
   [[:circle {:cx "18" :cy "5" :r "3"}]
    [:circle {:cx "6" :cy "12" :r "3"}]
    [:circle {:cx "18" :cy "19" :r "3"}]
    [:line {:x1 "8.59" :x2 "15.42" :y1 "13.51" :y2 "17.49"}]
    [:line {:x1 "15.41" :x2 "8.59" :y1 "6.51" :y2 "10.49"}]]

   ;; --- Format menu (lucide-static 1.52.0) ---------------------------------

   ;; The colour control: an A over a baseline, Notion's colour trigger.
   :baseline
   [[:path {:d "M4 20h16"}]
    [:path {:d "m6 16 6-12 6 12"}]
    [:path {:d "M8 12h8"}]]

   :bold
   [[:path {:d "M6 12h9a4 4 0 0 1 0 8H7a1 1 0 0 1-1-1V5a1 1 0 0 1 1-1h7a4 4 0 0 1 0 8"}]]

   :italic
   [[:line {:x1 "19" :x2 "10" :y1 "4" :y2 "4"}]
    [:line {:x1 "14" :x2 "5" :y1 "20" :y2 "20"}]
    [:line {:x1 "15" :x2 "9" :y1 "4" :y2 "20"}]]

   :underline
   [[:path {:d "M6 4v6a6 6 0 0 0 12 0V4"}]
    [:line {:x1 "4" :x2 "20" :y1 "20" :y2 "20"}]]

   :strikethrough
   [[:path {:d "M16 4H9a3 3 0 0 0-2.83 4"}]
    [:path {:d "M14 12a4 4 0 0 1 0 8H6"}]
    [:line {:x1 "4" :x2 "20" :y1 "12" :y2 "12"}]]

   :remove-formatting
   [[:path {:d "M4 7V4h16v3"}]
    [:path {:d "M5 20h6"}]
    [:path {:d "M13 4 8 20"}]
    [:path {:d "m15 15 5 5"}]
    [:path {:d "m20 15-5 5"}]]

   :list
   [[:path {:d "M3 5h.01"}]
    [:path {:d "M3 12h.01"}]
    [:path {:d "M3 19h.01"}]
    [:path {:d "M8 5h13"}]
    [:path {:d "M8 12h13"}]
    [:path {:d "M8 19h13"}]]

   :list-ordered
   [[:path {:d "M11 5h10"}]
    [:path {:d "M11 12h10"}]
    [:path {:d "M11 19h10"}]
    [:path {:d "M4 4h1v5"}]
    [:path {:d "M4 9h2"}]
    [:path {:d "M6.5 20H3.4c0-1 2.6-1.925 2.6-3.5a1.5 1.5 0 0 0-2.6-1.02"}]]

   :ellipsis
   [[:circle {:cx "12" :cy "12" :r "1"}]
    [:circle {:cx "19" :cy "12" :r "1"}]
    [:circle {:cx "5" :cy "12" :r "1"}]]

   :list-indent-decrease
   [[:path {:d "M21 5H11"}]
    [:path {:d "M21 12H11"}]
    [:path {:d "M21 19H11"}]
    [:path {:d "m7 8-4 4 4 4"}]]

   :list-indent-increase
   [[:path {:d "M21 5H11"}]
    [:path {:d "M21 12H11"}]
    [:path {:d "M21 19H11"}]
    [:path {:d "m3 8 4 4-4 4"}]]

   :text-align-start
   [[:path {:d "M21 5H3"}]
    [:path {:d "M15 12H3"}]
    [:path {:d "M17 19H3"}]]

   :text-align-center
   [[:path {:d "M21 5H3"}]
    [:path {:d "M17 12H7"}]
    [:path {:d "M19 19H5"}]]

   :text-align-end
   [[:path {:d "M21 5H3"}]
    [:path {:d "M21 12H9"}]
    [:path {:d "M21 19H7"}]]

   :text-align-justify
   [[:path {:d "M3 5h18"}]
    [:path {:d "M3 12h18"}]
    [:path {:d "M3 19h18"}]]

   ;; Right-to-left: a pilcrow with its arrow pointing left.
   :pilcrow-left
   [[:path {:d "M14 3v11"}]
    [:path {:d "M14 9h-3a3 3 0 0 1 0-6h9"}]
    [:path {:d "M18 3v11"}]
    [:path {:d "M22 18H2l4-4"}]
    [:path {:d "m6 22-4-4"}]]

   :pilcrow
   [[:path {:d "M13 4v16"}]
    [:path {:d "M17 4v16"}]
    [:path {:d "M19 4H9.5a4.5 4.5 0 0 0 0 9H13"}]]

   :heading-1
   [[:path {:d "M4 12h8"}]
    [:path {:d "M4 18V6"}]
    [:path {:d "M12 18V6"}]
    [:path {:d "m17 12 3-2v8"}]]

   :heading-2
   [[:path {:d "M4 12h8"}]
    [:path {:d "M4 18V6"}]
    [:path {:d "M12 18V6"}]
    [:path {:d "M21 18h-4c0-4 4-3 4-6 0-1.5-2-2.5-4-1"}]]

   :heading-3
   [[:path {:d "M4 12h8"}]
    [:path {:d "M4 18V6"}]
    [:path {:d "M12 18V6"}]
    [:path {:d "M17.5 10.5c1.7-1 3.5 0 3.5 1.5a2 2 0 0 1-2 2"}]
    [:path {:d "M17 17.5c2 1.5 4 .3 4-1.5a2 2 0 0 0-2-2"}]]

   :code-xml
   [[:path {:d "m18 16 4-4-4-4"}]
    [:path {:d "m6 8-4 4 4 4"}]
    [:path {:d "m14.5 4-5 16"}]]

   :table
   [[:path {:d "M12 3v18"}]
    [:rect {:width "18" :height "18" :x "3" :y "3" :rx "2"}]
    [:path {:d "M3 9h18"}]
    [:path {:d "M3 15h18"}]]

   :image
   [[:rect {:width "18" :height "18" :x "3" :y "3" :rx "2" :ry "2"}]
    [:circle {:cx "9" :cy "9" :r "2"}]
    [:path {:d "m21 15-3.086-3.086a2 2 0 0 0-2.828 0L6 21"}]]

   :volume-2
   [[:path {:d "M11 4.702a.705.705 0 0 0-1.203-.498L6.413 7.587A1.4 1.4 0 0 1 5.416 8H3a1 1 0 0 0-1 1v6a1 1 0 0 0 1 1h2.416a1.4 1.4 0 0 1 .997.413l3.383 3.384A.705.705 0 0 0 11 19.298z"}]
    [:path {:d "M16 9a5 5 0 0 1 0 6"}]
    [:path {:d "M19.364 18.364a9 9 0 0 0 0-12.728"}]]

   ;; Font size: a large and a small A.
   :a-large-small
   [[:path {:d "m15 16 2.536-7.328a1.02 1.02 1 0 1 1.928 0L22 16"}]
    [:path {:d "M15.697 14h5.606"}]
    [:path {:d "m2 16 4.039-9.69a.5.5 0 0 1 .923 0L11 16"}]
    [:path {:d "M3.304 13h6.392"}]]

   ;; NOT Lucide. Cloze deletions: Lucide `braces` with a plus (a new cloze
   ;; number) or an equals sign (the same number) between the braces.
   :cloze-new
   [[:path {:d "M8 3H7a2 2 0 0 0-2 2v5a2 2 0 0 1-2 2 2 2 0 0 1 2 2v5c0 1.1.9 2 2 2h1"}]
    [:path {:d "M16 21h1a2 2 0 0 0 2-2v-5c0-1.1.9-2 2-2a2 2 0 0 1-2-2V5a2 2 0 0 0-2-2h-1"}]
    [:path {:d "M12 9v6"}]
    [:path {:d "M9 12h6"}]]

   :cloze-same
   [[:path {:d "M8 3H7a2 2 0 0 0-2 2v5a2 2 0 0 1-2 2 2 2 0 0 1 2 2v5c0 1.1.9 2 2 2h1"}]
    [:path {:d "M16 21h1a2 2 0 0 0 2-2v-5c0-1.1.9-2 2-2a2 2 0 0 1-2-2V5a2 2 0 0 0-2-2h-1"}]
    [:path {:d "M9 10h6"}]
    [:path {:d "M9 14h6"}]]

   ;; NOT Lucide. Restore a page boundary: a rule with a downward arrow
   ;; through its gap — a boundary being put back.
   :page-boundary
   [[:path {:d "M3 12h5"}]
    [:path {:d "M16 12h5"}]
    [:path {:d "M12 4v12"}]
    [:path {:d "m9 13 3 3 3-3"}]]})

(defn- attrs-markup [attrs]
  (apply str (for [[k v] attrs] (str " " (name k) "=\"" v "\""))))

(defn svg-markup
  "The `<svg>` markup for icon `k`, drawn `size` px square.

   Pre : `k` is a keyword; `size` is a positive number.
   Post: a string that opens with `<svg` and carries Lucide's stroke contract and
         `aria-hidden=\"true\"` (the control that holds it carries the name).
         An unknown `k` yields an empty `<svg>`, so a typo blanks one icon
         instead of throwing — the same invariant `freememo.icons/Icon` keeps.
   Inv : attribute values come only from `paths`, which is static data, so the
         markup needs no escaping. Never pass user text through here."
  [k size]
  (str "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\""
       " width=\"" size "\" height=\"" size "\""
       " fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\""
       " stroke-linecap=\"round\" stroke-linejoin=\"round\" aria-hidden=\"true\">"
       (apply str (for [[tag attrs] (get paths k)]
                    (str "<" (name tag) (attrs-markup attrs) "/>")))
       "</svg>"))
