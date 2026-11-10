(ns freememo.local-account
  "One library, one account (plans/local-install/issues/05-one-library-one-account.md).

   A Local Install holds the library of one account. This namespace is a Ring
   middleware that sits between the gate and the app. It decides four cases.

   | Case                                | Answer                                       |
   |-------------------------------------|----------------------------------------------|
   | No account (after a removal)        | the first-launch sign-in, for any account    |
   | freememo.net revoked the token      | GET / shows a sign-in page for the account   |
   | A sign-in with the same account     | the new token replaces the old one, sync runs |
   | A sign-in with another account      | refused; a page links to /remove-library     |

   GET /remove-library shows the count of changes that did not sync. POST
   /remove-library with `confirm=yes` wipes the library and `account.json`.
   After that, the first-launch sign-in takes any account, and a first download
   follows.

   The sign-in itself is `freememo.local-signin`. Everything that touches the
   database or the sync client is passed in as a function (see
   `freememo.local-library`), so tests load this namespace with PostgreSQL
   stopped."
  (:require [clojure.string :as str]
            [freememo.local-http :as http]
            [freememo.local-signin :as signin]))

(def ^:private page http/page)
(def ^:private esc http/esc)

(defn- count-text [n]
  (cond (zero? n) "All changes on this computer are synced."
        (= 1 n) "<strong>1</strong> change on this computer did not sync. Removing the library loses it."
        :else (str "<strong>" n "</strong> changes on this computer did not sync. Removing the library loses them.")))

(defn remove-library-page
  "Post: the page with the count `n` of changes that did not sync and one
         button, \"Remove the library\"."
  [n]
  (page (str "<h1>Remove the library</h1>"
          "<p role=\"status\">" (count-text n) "</p>"
          "<p>FreeMemo removes the library from this computer only. Your library on freememo.net stays.</p>"
          "<form method=\"post\" action=\"/remove-library\">"
          "<input type=\"hidden\" name=\"confirm\" value=\"yes\">"
          "<button type=\"submit\">Remove the library</button></form>"
          "<p><a href=\"/\">Keep the library</a></p>")))

(defn reauth-page
  "Post: the page that asks the student to sign in again as `username`."
  [username]
  (page (str "<h1>Sign in again</h1>"
          "<p role=\"alert\">freememo.net signed this computer out. Your library is still here.</p>"
          "<p>Sign in as <strong>" (esc username) "</strong> to sync again.</p>"
          "<form method=\"post\" action=\"/first-launch/sign-in\">"
          "<button type=\"submit\">Sign in</button></form>")))

(defn other-account-page
  "Post: the page that refuses a sign-in with another account and links to
         the remove-library page."
  [held-username signing-in-username]
  (page (str "<h1>This library belongs to another account</h1>"
          "<p class=\"err\" role=\"alert\">This computer holds the library of <strong>" (esc held-username)
          "</strong>. You signed in as <strong>" (esc signing-in-username) "</strong>.</p>"
          "<p>A Local Install holds one library. Remove it to use the other account.</p>"
          "<p><a class=\"again\" href=\"/remove-library\">Remove the library</a></p>"
          "<p><a href=\"/\">Keep the library</a></p>")))

(defn- confirmed? [request]
  (boolean (some-> (:body request) slurp (str/includes? "confirm=yes"))))

(defn make-guard
  "Pre:  `account` is the account map (string keys) or nil.
         `save-file!` `(fn [account-map])` writes `account.json` in one step.
         `delete-file!` `(fn [])` deletes it.
         `adopt!` `(fn [account-map])` binds a new account to an empty library.
         `renew!` `(fn [account-map])` gives the link the new token of the same account.
         `wipe!` `(fn [])` removes the library. `unsynced-count` `(fn [])` counts
         the changes that did not sync. `revoked?` `(fn [])` is true after
         freememo.net refused the token. `library-ready?` `(fn [])` is true
         after a download committed.
   Post: `{:wrap f :user-id (fn [])}`. `(wrap handler)` is the middleware.
         `(user-id)` is the account's id, nil after a removal."
  [{:keys [origin local-origin account save-file! delete-file! adopt! renew! wipe!
           unsynced-count revoked? library-ready? online-fn]
    :or {online-fn signin/online?}}]
  (let [!account (atom account)
        !first-download-open (atom false)
        !signin-flow (atom nil)
        save! (fn [acct]
                (let [held @!account
                      held-id (some-> held (get "user_id") long)]
                  (cond
                    (nil? held)
                    (do (save-file! acct)
                        (reset! !account acct)
                        (reset! !first-download-open true)
                        (adopt! acct))

                    (= held-id (long (get acct "user_id")))
                    (let [merged (into held (remove (comp nil? val)) (dissoc acct "first_download"))]
                      (save-file! merged)
                      (reset! !account merged)
                      (renew! merged))

                    :else
                    (throw (ex-info "This library belongs to another account"
                             {:type ::other-account
                              :response (other-account-page (get held "username") (get acct "username"))})))))
        new-flow! (fn []
                    ;; A flow answers one sign-in. A new one waits for the next.
                    (reset! !signin-flow (signin/make-flow {:origin origin :local-origin local-origin
                                                     :save-account! save! :online-fn online-fn})))
        _ (new-flow!)
        wrap (fn [inner]
               (fn [request]
                 (let [method (:request-method request)
                       uri (:uri request)
                       flow-handler (:handler @!signin-flow)
                       held @!account]
                   (cond
                     (#{"/first-launch/sign-in" "/callback"} uri)
                     (let [flow @!signin-flow
                           response ((:handler flow) request)]
                       ;; The sign-in is done: the next one needs a new flow.
                       (when (and (= "/callback" uri) (realized? (:account flow)))
                         (new-flow!))
                       response)

                     (nil? held) (flow-handler request)

                     (and (= "/remove-library" uri) (= :get method))
                     (remove-library-page (unsynced-count))

                     (and (= "/remove-library" uri) (= :post method))
                     (if-not (confirmed? request)
                       {:status 400 :headers {"Content-Type" "text/plain"} :body "Confirm the removal first."}
                       (do (wipe!)
                           (delete-file!)
                           (reset! !account nil)
                           (new-flow!)
                           {:status 303 :headers {"Location" "/" "Cache-Control" "no-store"} :body ""}))

                     (and (= :get method) (= "/" uri) (revoked?))
                     (reauth-page (get held "username"))

                     (and @!first-download-open (= :get method) (= "/" uri))
                     (if (library-ready?)
                       (do (reset! !first-download-open false) (inner request))
                       (signin/progress-page))

                     :else (inner request)))))]
    {:wrap wrap
     :user-id #(some-> @!account (get "user_id"))}))
