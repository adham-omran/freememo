(ns freememo.pdf-cache
  "IndexedDB cache for PDF files. Eliminates redundant server fetches
   for PDFs already loaded once.")

(def ^:private db-name "freememo-pdf-cache")
(def ^:private store-name "pdfs")
(def ^:private db-version 1)

#?(:cljs
   (defonce _persist
     (when (exists? js/navigator.storage)
       (.then (.persist js/navigator.storage)
         (fn [granted]
           (js/console.log "[PDF-CACHE] storage.persist granted?" granted))))))

#?(:cljs
   (defn- open-db
     "Open (or create) the IndexedDB database. Returns a Promise<IDBDatabase>."
     []
     (js/Promise.
       (fn [resolve reject]
         (let [req (.open js/indexedDB db-name db-version)]
           (set! (.-onupgradeneeded req)
             (fn [e]
               (let [^js db (-> e .-target .-result)]
                 (when-not (.contains (.-objectStoreNames db) store-name)
                   (.createObjectStore db store-name)))))
           (set! (.-onsuccess req)
             (fn [e] (resolve (-> e .-target .-result))))
           (set! (.-onerror req)
             (fn [e] (reject (-> e .-target .-error)))))))))

(defn cache-get
  "Get a cached PDF ArrayBuffer by document-id. Returns a Promise that
   resolves to the ArrayBuffer or nil if not cached."
  [document-id]
  #?(:clj nil
     :cljs (-> (open-db)
             (.then (fn [^js db]
                      (js/Promise.
                        (fn [resolve reject]
                          (let [tx (.transaction db store-name "readonly")
                                st (.objectStore tx store-name)
                                req (.get st document-id)]
                            (set! (.-onsuccess req)
                              (fn [e] (resolve (-> e .-target .-result))))
                            (set! (.-onerror req)
                              (fn [e] (reject (-> e .-target .-error)))))))))
             (.catch (fn [err]
                       (js/console.warn "[PDF-CACHE] cache-get failed:" err)
                       nil)))))

(defn cache-put
  "Store a PDF ArrayBuffer by document-id. Returns a Promise<void>.
   Fire-and-forget — callers need not await."
  [document-id array-buffer]
  #?(:clj nil
     :cljs (-> (open-db)
             (.then (fn [^js db]
                      (js/Promise.
                        (fn [resolve reject]
                          (let [tx (.transaction db store-name "readwrite")
                                st (.objectStore tx store-name)
                                req (.put st array-buffer document-id)]
                            (set! (.-onsuccess req) (fn [_] (resolve)))
                            (set! (.-onerror req)
                              (fn [e] (reject (-> e .-target .-error)))))))))
             (.catch (fn [err]
                       (js/console.warn "[PDF-CACHE] cache-put failed:" err))))))

(defn cache-delete
  "Delete a cached PDF by document-id. Returns a Promise<void>."
  [document-id]
  #?(:clj nil
     :cljs (-> (open-db)
             (.then (fn [^js db]
                      (js/Promise.
                        (fn [resolve reject]
                          (let [tx (.transaction db store-name "readwrite")
                                st (.objectStore tx store-name)
                                req (.delete st document-id)]
                            (set! (.-onsuccess req) (fn [_] (resolve)))
                            (set! (.-onerror req)
                              (fn [e] (reject (-> e .-target .-error)))))))))
             (.catch (fn [err]
                       (js/console.warn "[PDF-CACHE] cache-delete failed:" err))))))
