(ns freememo.google-picker
  "The Google Picker, shared by the NeoReader sync (`freememo.neoreader-picker`)
   and the Google Doc import (`freememo.gdoc-import`).

   Google's Picker is the SECOND third-party script this app loads, and the only
   one outside `deployment-hook-impl`. It cannot be vendored: the Picker is a
   Google-hosted widget that talks to Google, exactly like `gtag.js`. Two things
   keep it inside the spirit of the no-CDN rule — it carries NO `<script>` tag
   in either HTML shell, and it loads only when a user asks to pick a file. A
   self-hosted deployment that never uses these features never contacts Google.

   The Picker shows the Drive of the account whose OAuth token it is given, so
   the caller chooses the account by choosing the token.")

(def ^:private picker-api-src "https://apis.google.com/js/api.js")

(defonce ^:private !loaded (atom nil))

(defn load!
  "Load Google's Picker API once.

   Pre : none.
   Post: a promise resolving when `google.picker` is defined, rejecting on a
         load or timeout failure. One shared promise per page — repeated clicks
         reuse it rather than injecting a second script."
  []
  (or @!loaded
    (reset! !loaded
      (js/Promise.
        (fn [resolve reject]
          (let [done (fn []
                       (if (and (exists? js/gapi) (.-load js/gapi))
                         (.load js/gapi "picker"
                           #js {:callback (fn []
                                            (if (and (exists? js/google) (.-picker js/google))
                                              (resolve true)
                                              (reject (js/Error. "Picker API loaded but google.picker is missing"))))
                                :onerror (fn [] (reject (js/Error. "Could not load the Google Picker")))})
                         (reject (js/Error. "Google API script loaded but gapi is missing"))))]
            (if (and (exists? js/gapi) (.-load js/gapi))
              (done)
              (let [el (.createElement js/document "script")]
                (set! (.-src el) picker-api-src)
                (set! (.-async el) true)
                (set! (.-onload el) done)
                (set! (.-onerror el) (fn [] (reject (js/Error. "Could not reach Google to load the Picker"))))
                (.appendChild (.-head js/document) el)))))))))

(defn pick-file!
  "Show the Picker and resolve with the chosen file id, or nil if cancelled.

   Pre : `load!` has resolved. `token` is a live `drive.file` token, and
         `app-id` is the Cloud PROJECT NUMBER. `mime-types` is a
         comma-separated MIME list the view shows.
   Post: a promise of a file-id string, or nil when the user cancelled. The
         picked file is granted to this app, which is what makes the later
         server-side read possible.
   Inv : `setAppId` is REQUIRED under `drive.file`. Without it the pick succeeds,
         the file id looks fine, and every server read of it answers 404 — the
         error names the file and never the missing app id. Do not remove it.
   Note: no `setSelectFolderEnabled` — a folder pick grants nothing about its
         contents, so offering it would hand the user a choice that cannot work."
  [{:keys [token app-id title mime-types]}]
  (js/Promise.
    (fn [resolve _reject]
      (let [picker-ns (.-picker js/google)
            View (.-DocsView picker-ns)
            view (doto (new View (.. picker-ns -ViewId -DOCS))
                   (.setMimeTypes mime-types)
                   (.setIncludeFolders true)
                   (.setSelectFolderEnabled false))
            Builder (.-PickerBuilder picker-ns)
            Action (.-Action picker-ns)
            builder (doto (new Builder)
                      (.setTitle title)
                      (.setAppId app-id)
                      (.setOAuthToken token)
                      (.addView view)
                      (.setCallback
                        (fn [data]
                          (let [action (aget data (.. picker-ns -Response -ACTION))]
                            (cond
                              (= action (.-PICKED Action))
                              (let [docs (aget data (.. picker-ns -Response -DOCUMENTS))
                                    doc (aget docs 0)]
                                (resolve (aget doc (.. picker-ns -Document -ID))))

                              (= action (.-CANCEL Action))
                              (resolve nil)

                              :else nil)))))]
        (.setVisible (.build builder) true)))))
