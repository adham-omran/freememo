(ns freememo.deployment-hook
  "Optional per-deployment behavior, absent by default.

   Four seams where a deployment may add something the app itself does not need:

   - `on-signup` may annotate the session a new account is created with.
   - `shell-head` may contribute markup to one shell response, and spend whatever
     `on-signup` left there.
   - `well-known-text` may supply the body of a well-known text file, keyed by its
     path, so one entry point serves `/ads.txt` and any later `security.txt`.
   - `site-verification-html` may supply markup that proves site ownership, for
     the crawler document, which carries no other contribution.

   This namespace owns the contract and the no-op fallbacks. `freememo.shell`,
   `freememo.api` and `freememo.crawler-page` call through here.

   The implementation namespace is optional. When it is absent both functions
   return their fallbacks: an unchanged session and an empty head. That is the
   ORDINARY case, not a failure — every build that does not carry the
   implementation exercises it on every request.

   Resolution is dynamic and held for the process lifetime. `requiring-resolve`
   does not memoize a failure — a failed call costs ~68 µs and repeats the
   classpath scan every time — and `shell-head` runs on every page load, so the
   result MUST be cached here rather than re-derived at the call sites. Caching it
   at the two call sites instead would also duplicate the fallback, which is the
   defect `freememo.shell` was created to remove.

   Only `FileNotFoundException` is caught. An absent implementation is expected;
   one that loads but does not provide both entry points is a bug and must
   surface rather than degrade into a silent no-op.

   No namespace other than this one may name the implementation. See
   plans/ad-free-selfhost-public-private-split.md."
  (:require
   [taoensso.telemere :as tel]))

(defonce ^:private impl
  (delay
    (try
      (let [fns {:on-signup       (requiring-resolve 'freememo.deployment-hook-impl/on-signup)
                 :shell-head      (requiring-resolve 'freememo.deployment-hook-impl/shell-head)
                 :well-known-text (requiring-resolve 'freememo.deployment-hook-impl/well-known-text)
                 :site-verification-html (requiring-resolve 'freememo.deployment-hook-impl/site-verification-html)}]
        (when-some [missing (seq (keep (fn [[k v]] (when (nil? v) k)) fns))]
          (throw (ex-info "deployment hook implementation loaded but incomplete"
                   {:missing (vec missing)})))
        (tel/log! {:level :info :id ::impl-present} "Deployment hook implementation loaded")
        fns)
      (catch java.io.FileNotFoundException _
        (tel/log! {:level :info :id ::impl-absent}
          "No deployment hook implementation on the classpath — hooks are no-ops")
        nil))))

(defn on-signup
  "Let this deployment annotate the session of a newly created account.

   Pre: `session` is the map the auth handler is about to return, `new-user?`
   distinguishes a sign-up from a login, and `landing-path` is the path that
   handler is redirecting to — the caller MUST pass the same value it puts in
   `Location`, or an implementation cannot recognize the landing request.
   Post: a session map, never nil. `session` unchanged when no implementation is
   present, and unchanged when the implementation declines."
  [session new-user? landing-path]
  (if-some [f (:on-signup @impl)]
    (f session new-user? landing-path)
    session))

(defn shell-head
  "This deployment's markup contribution for one shell response.

   Pre: `request` passed through `wrap-session`, or carries no `:session` at all
   (the jetty9 entrypoint installs none).
   Post: {:html String :session (or map nil)}. `:html` is \"\" when no
   implementation is present, so the caller always has a value to substitute —
   `freememo.shell/template` leaves an unsupplied placeholder in the output
   verbatim. `:session` is non-nil ONLY when the caller must attach it to the ring
   response: attaching nil DELETES the session, because ring rewrites the cookie
   whenever the response map contains the key."
  [request]
  (if-some [f (:shell-head @impl)]
    (f request)
    {:html "" :session nil}))

(defn well-known-text
  "The body this deployment publishes at well-known text `path`, or nil.

   Pre: `path` is a request URI such as \"/ads.txt\".
   Post: nil, or a string with no HTML in it. nil means the caller MUST answer
   404 — an absent file and an empty file say different things to the crawler
   that reads it, and only 404 says absent.
   nil for every path when no implementation is present, which is the ordinary
   case."
  [path]
  (when-some [f (:well-known-text @impl)]
    (f path)))

(defn site-verification-html
  "Markup that proves this deployment owns the accounts it names.

   Post: a string, never nil, so the caller can substitute it unconditionally.
   \"\" when no implementation is present, so a self-hosted build's crawler
   document names nobody.
   This is deliberately narrower than `shell-head`: the crawler document carries
   no script, so it must not take a contribution that includes one."
  []
  (if-some [f (:site-verification-html @impl)]
    (f)
    ""))
