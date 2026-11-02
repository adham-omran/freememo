(ns freememo.import-waiting
  "Imports that wait for the reader (plans/remove-toasts.md 2.12).

   A link clicked in the editor can fetch a PDF or an EPUB. That import needs a
   confirmation step the click path cannot drive, so the bytes stay in the
   staging area, marked waiting (`freememo.import-staging`). Two surfaces show
   them: a count on the Import nav item, and a list at the top of the Import
   page. This namespace is the thin seam both read through.

   Server reads and writes are body-level reader conditionals, because each is
   named inside an `e/server` form, which both peers compile."
  (:require
   #?(:clj [freememo.import-staging :as staging])))

(defn rev-atom*
  "The atom that moves when `user-id`'s waiting imports change. Watch it inside
   `e/server`, then call `waiting*` with its value."
  [user-id]
  #?(:clj (staging/rev-atom user-id)
     :cljs nil))

(defn waiting*
  "`user-id`'s waiting imports, oldest first, as {:upload-id :filename :flow
   :size :created-at}. `_rev` makes the call depend on `rev-atom*`."
  [_rev user-id]
  #?(:clj (staging/waiting user-id)
     :cljs []))

(defn discard!*
  "Drop one waiting import's bytes.
   Pre : `user-id` is the signed-in user. A forged id for another user's entry
         finds nothing, because the staging area is keyed by user.
   Post: {:ok true}."
  [user-id upload-id]
  #?(:clj (do (staging/claim! user-id upload-id) {:ok true})
     :cljs nil))

(defn kind-label
  "The word for what is waiting: PDF or EPUB."
  [flow]
  (case flow
    :pdf "PDF"
    :epub "EPUB"
    (name flow)))

(defn badge-text
  "The Import nav item's count text for `n` waiting imports."
  [n]
  (if (> n 99) "99+" (str n)))
