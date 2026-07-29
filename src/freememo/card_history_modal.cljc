(ns freememo.card-history-modal
  "Read-only edit history for one card — the superseded renditions written by
   the write-behind version log (plans/card-edit-history.md).

   A row is what a save REPLACED, newest first, so the list reads as
   \"what this card used to be\". The current rendition is deliberately absent:
   it lives in the card row and is already on screen behind this modal.

   Occlusion cards show their GROUP's history, because mask placement lives on
   occlusion_groups.geometry and one geometry is shared by every mask card in
   the group. A version therefore renders the whole rect set as it stood —
   including rects for masks the group no longer has.

   Own namespace, not card_modals: that file already carries many e/defns and
   the 64KB method limit is :prod-only, so growth there fails a build every dev
   check passes (CLAUDE.md 'Method code too large!')."
  (:require
   [clojure.string :as str]
   [hyperfiddle.electric3 :as e]
   [hyperfiddle.electric-dom3 :as dom]
   [freememo.card-components :as cc]
   [freememo.modal-shell :as modal]
   [freememo.occlusion-svg :as osvg]
   #?(:clj [freememo.db :as db])
   #?(:clj [freememo.user-state :as us])))

;; Server bridge — same-namespace wrapper keeps `e/server` codepaths short
;; and shields the modal from cross-ns atom-resolution issues.
;;
;; Scope-keyed rather than card-keyed: the occlusion editor knows a group and no
;; single card, and occlusion history genuinely belongs to the group. `scope` is
;; [:card card-id] or [:occlusion-group group-id]; both entry points authorize
;; against user-id server-side.
(defn get-versions* [_refresh user-id scope]
  #?(:clj (let [[scope-type id] scope]
            (case scope-type
              :card (db/get-card-versions user-id id)
              :occlusion-group (db/get-occlusion-group-versions user-id id)))
     :cljs []))

(defn versions-truncated?
  "True when the read hit db/max-versions-read, meaning older versions exist
   that this view is not showing. Retention is unbounded, so the cap must be
   stated in the UI rather than silently trimming the list."
  [versions]
  #?(:clj (= (count versions) db/max-versions-read)
     :cljs false))

;; ── Rendition rendering ────────────────────────────────────────────────────

(defn occlusion-rendition-html
  "Image + masks at the positions this rendition held.

   Built here rather than reused from occlusion-svg because that namespace
   emits Anki field SVGs: exact natural-pixel width/height and per-rect ids
   that bind row ↔ note ↔ rect. A history preview needs the opposite — a
   viewBox so the overlay scales with a responsive img, and no ids to collide
   with. The fill/stroke constants ARE reused, so a past rendition looks like
   the card it came from.

   Pre : `geometry` is {:width :height :rects [{:x :y :w :h} ...]} in natural
         image pixels; `image-media-id` identifies a readable media row.
   Post: returns a self-contained HTML string; masks are neutral-filled (this
         is the group's placement, not one card's front)."
  [image-media-id {:keys [width height rects]}]
  (if-not (and image-media-id width height (pos? width) (pos? height))
    "<span style=\"color:var(--color-text-secondary)\">Mask placement unavailable</span>"
    (str "<div style=\"position:relative;display:inline-block;max-width:100%\">"
      "<img src=\"/api/media/" image-media-id "\""
      " style=\"display:block;max-width:100%;height:auto;border-radius:3px\" />"
      "<svg viewBox=\"0 0 " width " " height "\""
      " preserveAspectRatio=\"none\""
      " style=\"position:absolute;top:0;left:0;width:100%;height:100%\">"
      (str/join
        (map (fn [{:keys [x y w h]}]
               (str "<rect x=\"" x "\" y=\"" y "\" width=\"" w "\" height=\"" h "\""
                 " fill=\"" osvg/other-fill "\" stroke=\"" osvg/stroke-color "\""
                 " stroke-width=\"2\" fill-opacity=\"0.85\" />"))
          rects))
      "</svg></div>")))

(defn rendition-panes
  "The [label html] pairs that constitute `version`'s rendition, in reading
   order. One shape per card kind; an unknown kind degrades to a note rather
   than rendering nothing.
   Post: every pair's html is safe to write into innerHTML — it is either
         already-sanitized stored card HTML or built here."
  [{:keys [kind question answer cloze overlapping geometry occlusion_mode
           occlusion_image_media_id score_direction score_start_ms score_end_ms]}]
  (case kind
    "basic" [["Question" question] ["Answer" answer]]
    "cloze" (cond-> [["Cloze" cloze]]
              (not (str/blank? answer)) (conj ["Back extra" answer]))
    "overlapping" [["List" (cc/overlapping-row-html overlapping)]]
    "occlusion" [[(str "Masks · " (if (= occlusion_mode "hide-one") "Hide One" "Hide All"))
                  (occlusion-rendition-html occlusion_image_media_id geometry)]]
    "score" [["Segment" (cc/score-row-html score_direction score_start_ms score_end_ms)]]
    [["Rendition" (str "<span style=\"color:var(--color-text-secondary)\">"
                    "No preview for kind " kind "</span>")]]))

(e/defn RenditionPane [label html]
  (e/client
    (dom/div
      (dom/props {:style {:margin-top "var(--sp-2)"}})
      (dom/div
        (dom/props {:style {:font-size "11px" :text-transform "uppercase"
                            :letter-spacing "0.04em" :font-weight "500"
                            :color "var(--color-text-secondary)"
                            :margin-bottom "2px"}})
        (dom/text label))
      (dom/div
        (dom/props {:style {:font-size "13px" :line-height "1.5"
                            :overflow-wrap "anywhere"}})
        (cc/set-inner-html! dom/node html)))))

(e/defn VersionEntry
  "One superseded rendition. `ordinal` counts back from the newest (1 = the
   rendition the most recent save replaced)."
  [version ordinal]
  (e/client
    (dom/li
      (dom/props {:style {:border "0.5px solid var(--color-border-light)"
                          :border-radius "var(--radius-md)"
                          :padding "var(--sp-3)"
                          :margin-bottom "var(--sp-2)"
                          :list-style "none"}})
      (dom/div
        (dom/props {:style {:display "flex" :justify-content "space-between"
                            :align-items "baseline" :gap "12px"
                            :font-size "12px"
                            :color "var(--color-text-secondary)"}})
        (dom/span (dom/text "Replaced " (:superseded_label version)))
        (dom/span
          (dom/props {:style {:font-family "ui-monospace, monospace"}})
          (dom/text "−" ordinal
            (when (not= "card" (:scope_type version)) " · group edit"))))
      (e/for [[label html] (e/diff-by first (rendition-panes version))]
        (RenditionPane label html)))))

(e/defn CardHistoryModal
  "Read-only history for `scope`. Renders nothing while `@!open?` is false.

   Pre : `scope` is [:card card-id] or [:occlusion-group group-id], owned by
         `user-id` (enforced server-side); `!open?` is the caller's open/closed
         atom — caller flips true, this modal flips false on dismiss
         (Escape / Close / backdrop).
   Post: lists the scope's superseded renditions newest-first and issues no
         write of any kind. A never-edited card shows the empty state."
  [scope user-id !open?]
  (e/client
    (when (e/watch !open?)
      ;; Server-sited form binding, NOT an e/defn return — an e/defn call would
      ;; materialize the whole log at the call boundary (CLAUDE.md).
      (let [versions (e/server
                       (let [rev (e/watch (us/get-atom user-id :card-mutations))]
                         ;; e/Offload, not a bare call: two DB round-trips plus
                         ;; JSONB parsing would block this session's whole
                         ;; reactive graph, and this re-runs on every
                         ;; :card-mutations bump. Latest-wins is what we want —
                         ;; a newer rev supersedes an in-flight read.
                         (e/Offload #(get-versions* rev user-id scope))))
            n (e/server (count versions))
            truncated? (e/server (versions-truncated? versions))]
        (dom/div
          (dom/props {:class "modal-backdrop" :tabindex "-1"})
          (modal/ModalEscape (fn [] (reset! !open? false)) "Card edit history")
          (dom/On "click" (fn [_] (reset! !open? false)) nil)
          (dom/div
            (dom/props {:class "modal-content"
                        :style {:width "min(620px, 95vw)" :max-height "85vh"
                                :overflow "hidden" :display "flex"
                                :flex-direction "column" :padding "0"}})
            (dom/On "click" (fn [e] (.stopPropagation e)) nil)
            (dom/div
              (dom/props {:style {:padding "16px 20px"
                                  :border-bottom "0.5px solid var(--color-border-light)"
                                  :display "flex" :align-items "center"
                                  :justify-content "space-between" :gap "12px"}})
              (dom/div
                (dom/h3
                  (dom/props {:style {:margin "0" :font-size "16px" :font-weight "500"}})
                  (dom/text "Edit history"))
                (dom/div
                  (dom/props {:style {:font-size "13px" :margin-top "2px"
                                      :color "var(--color-text-secondary)"}})
                  (dom/text n " previous rendition" (when (not= n 1) "s")
                    (when truncated? " (most recent — older ones not shown)"))))
              (dom/button
                (dom/props {:aria-label "Close" :class "btn btn-secondary"})
                (dom/text "Close")
                (dom/On "click" (fn [_] (reset! !open? false)) nil)))
            (dom/div
              (dom/props {:style {:padding "12px 20px 20px" :overflow-y "auto"}})
              (if (zero? n)
                (dom/p
                  (dom/props {:style {:color "var(--color-text-secondary)"
                                      :font-size "13px" :margin "8px 0"}})
                  (dom/text "This card has not been edited yet. Its first edit will record what it looked like beforehand."))
                (dom/ul
                  (dom/props {:style {:margin "0" :padding "0"}})
                  ;; Keyed by index, not by version id: the client never holds
                  ;; the id list — each row pulls its own row across the wire, so
                  ;; index is the only key available without shipping the whole
                  ;; log first. Ordering is newest-first, so a new version shifts
                  ;; every slot's content rather than mounting one row. That is
                  ;; the same slot-recycling trade the virtual-scroll sites make,
                  ;; and it is free at the ten-or-so versions this list holds.
                  (e/for [i (e/diff-by identity (vec (range n)))]
                    (let [version (e/server (nth versions i nil))]
                      ;; nil during the frame where n has arrived but the row
                      ;; has not, and for any index past a shrinking log.
                      (when version
                        (VersionEntry version (inc i))))))))))))))
