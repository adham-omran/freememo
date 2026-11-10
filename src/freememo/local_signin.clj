(ns freememo.local-signin
  "First-launch sign-in of a Local Install (plans/local-install/issues/04-sign-in-from-the-browser.md).

   Electron's `signin.js` ran this PKCE flow in Node. Here the local server is
   the loopback redirect target, and the student's own browser does the sign-in
   on freememo.net, with a password or with Google. The freememo.net side is
   unchanged.

   Flow, with the page that the launcher serves before an account exists:

   1. GET /                         the first-launch page. It checks that
                                    freememo.net answers. If not, it says
                                    \"Connect to the internet for the first start\"
                                    and offers \"Try again\".
   2. POST /first-launch/sign-in    makes the PKCE verifier and state, keeps them in
                                    memory, and redirects to /desktop/authorize.
   3. GET /callback?code&state      freememo.net redirects here. The state must match
                                    the pending one, once. The code goes to
                                    /desktop/token. The account map is then handed
                                    to `save-account!`.

   A wrong state, a second callback, or a refused code writes nothing. The
   callback is the only request that the gate lets through without its cookie
   (`public-request?`): a SameSite=Strict cookie does not travel on a redirect
   that freememo.net starts. Its answer is a 200 page, never a redirect,
   because a redirect keeps the cross-site mark and the next request would
   carry no cookie either.

   After the account exists the launcher starts PostgreSQL and the app. Until
   the first download ends, `wrap-first-download` serves the progress page at
   GET / instead of the app.

   NO db or app requires, so tests load it with PostgreSQL stopped."
  (:require [cheshire.core :as json]
            [clojure.string :as str])
  (:import [java.net URI URLEncoder]
           [java.net.http HttpClient HttpClient$Redirect HttpRequest HttpRequest$BodyPublishers
            HttpResponse$BodyHandlers]
           [java.nio.charset StandardCharsets]
           [java.security MessageDigest SecureRandom]
           [java.time Duration]
           [java.util Base64]))

(def pending-ttl-ms (* 10 60 1000))

(def offline-message "Connect to the internet for the first start")

(defn- b64url ^String [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- random-b64 ^String [n]
  (let [bs (byte-array n)]
    (.nextBytes (SecureRandom.) bs)
    (b64url bs)))

(defn challenge-of
  "Post: the S256 code challenge of `verifier`."
  ^String [^String verifier]
  (b64url (.digest (MessageDigest/getInstance "SHA-256") (.getBytes verifier StandardCharsets/UTF_8))))

(defn- enc [s] (URLEncoder/encode (str s) "UTF-8"))

(defn authorize-url
  "Post: the freememo.net URL that starts the sign-in."
  [origin redirect-uri challenge state]
  (str origin "/desktop/authorize?response_type=code&code_challenge_method=S256"
    "&code_challenge=" (enc challenge) "&redirect_uri=" (enc redirect-uri) "&state=" (enc state)))

(def ^:private client
  (delay (-> (HttpClient/newBuilder)
           (.followRedirects HttpClient$Redirect/NEVER)
           (.connectTimeout (Duration/ofSeconds 5))
           (.build))))

(defn online?
  "Post: true when freememo.net at `origin` answers any HTTP status."
  [origin]
  (try (.send ^HttpClient @client
         (-> (HttpRequest/newBuilder (URI. (str origin "/")))
           (.timeout (Duration/ofSeconds 5))
           (.method "HEAD" (HttpRequest$BodyPublishers/noBody))
           (.build))
         (HttpResponse$BodyHandlers/discarding))
       true
       (catch java.io.IOException _ false)
       (catch InterruptedException _ false)))

(defn- device-name []
  (try (.getHostName (java.net.InetAddress/getLocalHost))
       (catch Exception _ "FreeMemo Local")))

(defn exchange-code!
  "Trade the code and its verifier for the token (POST /desktop/token).
   Post: `{:access_token :user_id :username :id_block}` (string keys), or
         throws an ex-info with a message for the student."
  [origin code verifier redirect-uri]
  (let [response (try (.send ^HttpClient @client
                        (-> (HttpRequest/newBuilder (URI. (str origin "/desktop/token")))
                          (.timeout (Duration/ofSeconds 30))
                          (.header "Content-Type" "application/json")
                          (.POST (HttpRequest$BodyPublishers/ofString
                                   (json/generate-string {"code" code "code_verifier" verifier
                                                          "redirect_uri" redirect-uri
                                                          "device_name" (device-name)})))
                          (.build))
                        (HttpResponse$BodyHandlers/ofString))
                      (catch java.io.IOException _
                        (throw (ex-info offline-message {:type ::offline}))))
        body (when (= 200 (.statusCode response)) (json/parse-string (.body response)))]
    (when-not (and (map? body) (get body "access_token") (get body "user_id"))
      (throw (ex-info (str "Sign-in failed: freememo.net refused the code (HTTP " (.statusCode response) ")")
               {:type ::refused})))
    body))

;; ── Pages ──────────────────────────────────────────────────────────

(defn- esc [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
    (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn- page [body-html & [script]]
  {:status 200
   :headers {"Content-Type" "text/html; charset=utf-8" "Cache-Control" "no-store"}
   :body (str "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">"
           "<meta name=\"viewport\" content=\"width=device-width\">"
           "<title>FreeMemo</title>"
           "<style>body{font-family:system-ui,sans-serif;max-width:30rem;margin:5rem auto;padding:0 1rem}"
           "button,.again{font-size:1rem;padding:.6rem 1.2rem;cursor:pointer}"
           ".err{color:#b00020}</style></head><body>" body-html
           (when script (str "<script>" script "</script>"))
           "</body></html>")})

(defn first-launch-page
  "Post: the page with one action, \"Sign in\", or the offline message with
         \"Try again\". `error` is a message from the last try, or nil."
  [{:keys [online? error]}]
  (page
    (str "<h1>Welcome to FreeMemo</h1>"
      (when error (str "<p class=\"err\" role=\"alert\">" (esc error) "</p>"))
      (if online?
        (str "<p>Sign in to get your library on this computer.</p>"
          "<form method=\"post\" action=\"/first-launch/sign-in\">"
          "<button type=\"submit\">Sign in</button></form>")
        (str "<p role=\"alert\">" offline-message ".</p>"
          "<p><a class=\"again\" href=\"/\">Try again</a></p>")))))

(def ^:private progress-script
  "var out=document.getElementById('out'),again=document.getElementById('again');
   function fmt(n){return (n||0).toLocaleString()}
   async function tick(){
     try{
       var r=await fetch('/api/desktop/status',{credentials:'same-origin'});
       var s=await r.json();
       if(s['library-ready']){location.replace('/');return}
       var p=s.progress||{};
       if(s.error||s.state==='error'||s.state==='offline'){
         out.textContent=s.error||'The download stopped. Check the connection.';
         again.hidden=false
       }else{
         again.hidden=true;
         out.textContent=p['total-rows']?('Downloaded '+fmt(p.downloaded)+' of '+fmt(p['total-rows'])+' items'):'Starting the first download'
       }
     }catch(e){}
     setTimeout(tick,1000)
   }
   again.onclick=function(){fetch('/api/desktop/sync-now',{method:'POST',credentials:'same-origin'})};
   tick();")

(defn progress-page
  "Post: the page that shows the first download. It polls
         /api/desktop/status and opens the library when it ends."
  []
  (page (str "<h1>Getting your library</h1>"
          "<p id=\"out\" role=\"status\">Starting</p>"
          "<p><button id=\"again\" type=\"button\" hidden>Try again</button></p>")
    progress-script))

;; ── The flow ───────────────────────────────────────────────────────

(defn public-request?
  "True for the one request that the gate lets through without its cookie."
  [request]
  (and (= :get (:request-method request)) (= "/callback" (:uri request))))

(defn- query-param [request k]
  (some (fn [pair] (let [[pk pv] (str/split pair #"=" 2)]
                     (when (= k (java.net.URLDecoder/decode ^String pk "UTF-8"))
                       (some-> pv (java.net.URLDecoder/decode "UTF-8")))))
    (str/split (or (:query-string request) "") #"&")))

(defn- text [status body]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body body})

(defn- json-response [body]
  {:status 200 :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

(defn make-flow
  "Pre:  `origin` is freememo.net's origin. `local-origin` is
         \"http://127.0.0.1:<port>\", where this server listens.
         `save-account!` is `(fn [account-map])`; it must write the account
         file in one step.
   Post: `{:handler h :account p}`. `h` is a Ring handler for the first-launch
         paths, behind the gate. `p` is a promise that holds the account map
         once the sign-in ends well. Nothing is saved before that."
  [{:keys [origin local-origin save-account! now-ms online-fn]
    :or {now-ms #(System/currentTimeMillis) online-fn online?}}]
  (let [!pending (atom nil)
        !error (atom nil)
        account (promise)
        redirect-uri (str local-origin "/callback")
        take-pending! (fn [state]
                        ;; Only a matching state consumes the pending sign-in,
                        ;; so a stray request cannot cancel the real one.
                        (let [p @!pending]
                          (when (and p (< (now-ms) (:expires p))
                                  (MessageDigest/isEqual (.getBytes ^String (:state p) StandardCharsets/UTF_8)
                                    (.getBytes ^String (str state) StandardCharsets/UTF_8))
                                  (compare-and-set! !pending p nil))
                            p)))
        handler (fn [request]
                  (case [(:request-method request) (:uri request)]
                    [:get "/"]
                    (first-launch-page {:online? (online-fn origin) :error (first (reset-vals! !error nil))})

                    [:post "/first-launch/sign-in"]
                    (if-not (online-fn origin)
                      {:status 303 :headers {"Location" "/"} :body ""}
                      (let [verifier (random-b64 32)
                            state (random-b64 16)]
                        (reset! !pending {:verifier verifier :state state :expires (+ (now-ms) pending-ttl-ms)})
                        {:status 303
                         :headers {"Location" (authorize-url origin redirect-uri (challenge-of verifier) state)
                                   "Cache-Control" "no-store"}
                         :body ""}))

                    [:get "/callback"]
                    (cond
                      (realized? account) (progress-page)
                      (query-param request "error")
                      (do (reset! !error "Sign-in was not allowed. Try again.")
                          (reset! !pending nil)
                          (first-launch-page {:online? true :error @!error}))
                      :else
                      (if-let [{:keys [verifier]} (take-pending! (query-param request "state"))]
                        (try
                          (let [token (exchange-code! origin (query-param request "code") verifier redirect-uri)
                                acct {"origin" origin
                                      "token" (get token "access_token")
                                      "user_id" (get token "user_id")
                                      "username" (get token "username")
                                      "id_block" (get token "id_block")
                                      "first_download" true}]
                            (save-account! acct)
                            (deliver account acct)
                            (progress-page))
                          (catch clojure.lang.ExceptionInfo e
                            ;; `save-account!` may refuse with its own page (ticket 05).
                            (or (:response (ex-data e))
                              (first-launch-page {:online? (not= ::offline (:type (ex-data e)))
                                                :error (when (not= ::offline (:type (ex-data e))) (ex-message e))}))))
                        (text 400 "Sign-in refused: the reply did not match this request.")))

                    [:get "/api/desktop/status"]
                    (json-response {"phase" "starting" "library-ready" false})

                    (text 404 "Not found")))]
    {:handler handler :account account}))

(defn wrap-first-download
  "Pre:  `pending?` is `(fn [])`: true while the first download is not done.
   Post: GET / answers the progress page while `pending?`. Anything else, and
         every request after the download, goes to `handler`."
  [handler pending?]
  (fn [request]
    (if (and (= :get (:request-method request)) (= "/" (:uri request)) (pending?))
      (progress-page)
      (handler request))))
