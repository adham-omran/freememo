(ns freememo.page-title
  "Writes the browser tab title for the subject a route owner names
   (plans/reactive-page-title.md §3).

   A route owner passes ONE subject. This namespace reads the subject's name,
   watches the channel a rename of it bumps, and writes `document.title`.

   Nothing here clears the title on unmount. The previous title stays until the
   next name arrives, because Electric holds the write until the read resolves.
   So every route needs exactly one owner: `Main` for the tabs outside
   `page-title-format/self-titled-tabs`, and the page for the tabs inside it.
   `TopicPage` is never an owner, since the viewer, the Learn session and subset
   review all mount it and each titles itself differently."
  (:require [hyperfiddle.electric3 :as e]
            [freememo.page-title-format :as fmt]
            #?(:clj [freememo.curriculum :as curriculum])
            #?(:clj [freememo.db :as db])
            #?(:clj [freememo.walkthrough :as walkthrough])
            #?(:clj [freememo.user-state :as us])))

;; Server reads. Each keeps its var on both peers, because its name appears
;; inside an (e/server …) form that CLJS compiles too.

(defn topic-title*
  "Pre: topic-id is whatever the route carried.
   Post: the title of a topic the user owns, or nil for a missing, unowned,
   staged or non-integer id."
  [_rev user-id topic-id]
  #?(:clj (when (int? topic-id) (db/get-topic-title-for-user user-id topic-id))
     :cljs nil))

(defn curriculum-title*
  "Pre: curriculum-id is whatever the route or the exam model carried.
   Post: the title of a curriculum the user owns, or nil for a missing,
   unowned, staged or non-integer id."
  [_rev user-id curriculum-id]
  #?(:clj (when (int? curriculum-id) (curriculum/curriculum-title user-id curriculum-id))
     :cljs nil))

(defn walkthrough-title*
  "Post: the name of a walkthrough the user owns, then its repository
   (plans/repository-home.md §4.3.5), or nil."
  [_rev user-id walkthrough-id]
  #?(:clj (when (int? walkthrough-id)
            (let [ov (walkthrough/overview user-id walkthrough-id)]
              (fmt/walkthrough-name (:display-name ov) (:repo-title ov))))
     :cljs nil))

(defn write-title!
  "Pre: item-name is a page's name, or nil when its item does not exist.
   Post: on the client, `document.title` reads `(fmt/page-title item-name)`."
  [item-name]
  #?(:cljs (set! (.-title js/document) (fmt/page-title item-name))
     :clj nil)
  :written)

(e/defn PageTitle
  "Title the browser tab after one subject.

   Pre: subject is `[:topic id]`, `[:curriculum id]`, `[:walkthrough id]`,
   `[:label tab-keyword]` or `:not-found`, and the caller is the one owner of
   the title on its route.
   Post: once the name is known, `document.title` reads `Name | FreeMemo`. A
   missing or unowned item reads Not found. A topic rename arrives through
   `:tree-mutations`, and a curriculum rename through `:curriculum-mutations`.
   A walkthrough is named after its repository, so it watches
   `:tree-mutations` too.
   Invariant: the call is the LAST form of the caller's body, never inside a
   `case`, so its effect is the body's value and gates no DOM."
  [user-id subject]
  (e/client
    (let [kind (when (vector? subject) (first subject))
          id (when (vector? subject) (second subject))]
      (write-title!
        (case kind
          :topic (e/server
                   (let [rev (e/watch (us/get-atom user-id :tree-mutations))]
                     (e/Offload #(topic-title* rev user-id id))))
          :curriculum (e/server
                        (let [rev (e/watch (us/get-atom user-id :curriculum-mutations))]
                          (e/Offload #(curriculum-title* rev user-id id))))
          ;; A repository rename and a walkthrough rename both change it.
          :walkthrough (e/server
                         (let [rev (+ (e/watch (us/get-atom user-id :tree-mutations))
                                     (e/watch (us/get-atom user-id :walkthrough-mutations)))]
                           (e/Offload #(walkthrough-title* rev user-id id))))
          :label (get fmt/tab-labels id)
          nil)))))
