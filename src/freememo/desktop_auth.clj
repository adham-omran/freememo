(ns freememo.desktop-auth
  "Desktop sign-in and id blocks (plans/electron-local-db-sync.md §3, 1.2).

   The desktop app opens the system browser at `/desktop/authorize` with a
   PKCE S256 challenge and a `http://127.0.0.1:<port>/callback` redirect
   (RFC 8252). The user signs in with whatever the site offers, password or
   Google, and allows the app. The browser then carries a single-use code to
   the app's loopback listener, and the app trades it at `/desktop/token`.

   A token is 32 random bytes. The server keeps only its SHA-256, one row per
   install, so one install can be revoked alone (3.2).

   **Blame.** Every handler answers bad input with a 4xx and a reason, because
   the browser and the network are untrusted. A missing table is a bug."
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs]
            [freememo.db :as db]
            [freememo.sync-schema :as schema])
  (:import [java.security MessageDigest SecureRandom]
           [java.util Base64]))

(def ^:private opts {:builder-fn rs/as-unqualified-lower-maps})

;; ── Primitives ─────────────────────────────────────────────────────

(defn- b64url ^String [^bytes bs]
  (.encodeToString (.withoutPadding (Base64/getUrlEncoder)) bs))

(defn- sha256 ^bytes [^String s]
  (.digest (MessageDigest/getInstance "SHA-256") (.getBytes s "UTF-8")))

(defn sha256-hex
  "Lowercase hex SHA-256 of a string."
  [^String s]
  (apply str (map #(format "%02x" %) (sha256 s))))

(defn- random-secret ^String []
  (let [bs (byte-array 32)]
    (.nextBytes (SecureRandom.) bs)
    (b64url bs)))

(def ^:private loopback-redirect #"http://127\.0\.0\.1:(\d{2,5})/callback")
(def ^:private challenge-shape #"[A-Za-z0-9_-]{43,128}")

(defn- html-escape [s]
  (-> (str s) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
    (str/replace ">" "&gt;") (str/replace "\"" "&quot;")))

(defn- text-response [status body]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body body})

(defn- json-response [status body]
  {:status status :headers {"Content-Type" "application/json"} :body (json/generate-string body)})

;; ── Return to the authorize page after sign-in ─────────────────────

(defn return-path
  "Where a sign-in should land: the stored `/desktop/authorize` URL, or `/`.
   Only that one path is accepted, so the session cannot carry an open
   redirect."
  [session]
  (let [p (:desktop-return session)]
    (if (and (string? p) (str/starts-with? p "/desktop/authorize?")) p "/")))

(defn carry-return
  "`new-session` with the pending desktop return copied from `old-session`.
   The sign-in handlers replace the whole session, so they call this."
  [old-session new-session]
  (if-let [p (:desktop-return old-session)]
    (assoc new-session :desktop-return p)
    new-session))

;; ── /desktop/authorize (3.1) ───────────────────────────────────────

(defn- authorize-params
  "The validated authorize request, or {:error msg}."
  [params]
  (let [{:strs [response_type code_challenge code_challenge_method redirect_uri state]} params]
    (cond
      (not= "code" response_type) {:error "response_type must be code"}
      (not= "S256" code_challenge_method) {:error "code_challenge_method must be S256"}
      (not (and (string? code_challenge) (re-matches challenge-shape code_challenge)))
      {:error "code_challenge is missing or malformed"}
      (not (and (string? redirect_uri) (re-matches loopback-redirect redirect_uri)))
      {:error "redirect_uri must be http://127.0.0.1:<port>/callback"}
      (str/blank? state) {:error "state is required"}
      :else {:challenge code_challenge :redirect-uri redirect_uri :state state})))

(defn- consent-page [username params]
  (let [hidden (apply str (for [k ["response_type" "code_challenge" "code_challenge_method"
                                   "redirect_uri" "state"]]
                            (format "<input type=\"hidden\" name=\"%s\" value=\"%s\">"
                              k (html-escape (get params k)))))]
    (str "<!doctype html><html><head><meta charset=\"utf-8\">"
      "<meta name=\"viewport\" content=\"width=device-width\">"
      "<title>Allow FreeMemo for desktop</title>"
      "<style>body{font-family:system-ui,sans-serif;max-width:28rem;margin:4rem auto;padding:0 1rem}"
      "button{font-size:1rem;padding:.6rem 1.2rem}</style></head><body>"
      "<h1>Allow FreeMemo for desktop?</h1>"
      "<p>The FreeMemo app on this computer will sync with your account <strong>"
      (html-escape username) "</strong>.</p>"
      "<form method=\"post\" action=\"/desktop/authorize\">" hidden
      "<button type=\"submit\">Allow</button></form>"
      "<p>If you did not start this from the FreeMemo app, close this page.</p>"
      "</body></html>")))

(defn authorize-handler
  "GET /desktop/authorize. Signed out: remember this URL and send the browser
   to the sign-in page. Signed in: show the consent page."
  [request]
  (let [params (:params request)
        parsed (authorize-params params)
        user-id (get-in request [:session :user-id])]
    (cond
      (:error parsed) (text-response 400 (:error parsed))
      (nil? user-id) {:status 302
                      :headers {"Location" "/"}
                      :session (assoc (:session request) :desktop-return
                                 (str "/desktop/authorize?" (:query-string request)))}
      :else (let [username (:username (jdbc/execute-one! db/ds ["SELECT username FROM users WHERE id = ?"
                                                               user-id] opts))]
              {:status 200
               :headers {"Content-Type" "text/html; charset=utf-8"
                         "Cache-Control" "no-store"}
               :session (dissoc (:session request) :desktop-return)
               :body (consent-page username params)}))))

(defn authorize-post-handler
  "POST /desktop/authorize — the user allowed the app.
   Post: a code that lives 60 s and works once, delivered to the loopback
         redirect with the app's `state`."
  [request]
  (let [parsed (authorize-params (:params request))
        user-id (get-in request [:session :user-id])]
    (cond
      (:error parsed) (text-response 400 (:error parsed))
      (nil? user-id) (text-response 401 "Sign in first.")
      :else
      (let [code (random-secret)]
        (jdbc/execute! db/ds ["INSERT INTO desktop_auth_codes
                                 (code_hash, user_id, code_challenge, redirect_uri, expires_at)
                               VALUES (?, ?, ?, ?, now() + interval '60 seconds')"
                              (sha256-hex code) user-id (:challenge parsed) (:redirect-uri parsed)])
        {:status 302
         :headers {"Location" (str (:redirect-uri parsed) "?code=" code
                                "&state=" (java.net.URLEncoder/encode ^String (:state parsed) "UTF-8"))}}))))

;; ── Id blocks (1.2.3) ──────────────────────────────────────────────

(defn allocate-block!
  "A fresh block of `schema/block-size` ids, bound to `token-id`.
   Inv: blocks come from one sequence, so two allocations never overlap
        (1.2.3). The sequence never repeats a value, even across a rollback."
  [conn user-id token-id]
  (let [n (:n (jdbc/execute-one! conn ["SELECT nextval('id_block_seq') AS n"] opts))
        start (+ schema/client-id-floor (* n schema/block-size))
        end (+ start schema/block-size)]
    (jdbc/execute! conn ["INSERT INTO id_blocks (block_start, block_end, user_id, token_id)
                          VALUES (?, ?, ?, ?)" start end user-id token-id])
    {:start start :end end}))

;; ── /desktop/token (3.1, 3.2) ──────────────────────────────────────

(defn- request-params [request]
  (let [ct (get-in request [:headers "content-type"] "")]
    (if (str/includes? ct "application/json")
      (some-> (:body request) slurp json/parse-string)
      (:params request))))

(defn token-handler
  "POST /desktop/token — trade a code and its PKCE verifier for a token.
   Post: 200 with the token, the account and a first id block; or 400 for a
         replayed, expired or mismatched code. The code is spent in the same
         statement that checks it, so a replay cannot win a race."
  [request]
  (let [{:strs [code code_verifier redirect_uri device_name]} (request-params request)]
    (if (some str/blank? [code code_verifier redirect_uri])
      (json-response 400 {:error "invalid_request"})
      (jdbc/with-transaction [tx db/ds]
        (let [row (jdbc/execute-one! tx ["UPDATE desktop_auth_codes SET used_at = now()
                                          WHERE code_hash = ? AND used_at IS NULL AND expires_at > now()
                                          RETURNING user_id, code_challenge, redirect_uri"
                                         (sha256-hex code)] opts)]
          (if (or (nil? row)
                (not= redirect_uri (:redirect_uri row))
                (not= (:code_challenge row) (b64url (sha256 code_verifier))))
            (json-response 400 {:error "invalid_grant"})
            (let [user-id (:user_id row)
                  token (random-secret)
                  token-id (:id (jdbc/execute-one! tx ["INSERT INTO desktop_tokens (user_id, token_hash, device_name)
                                                        VALUES (?, ?, ?) RETURNING id"
                                                       user-id (sha256-hex token)
                                                       (some-> device_name (subs 0 (min 80 (count device_name))))]
                                 opts))
                  username (:username (jdbc/execute-one! tx ["SELECT username FROM users WHERE id = ?" user-id] opts))]
              ;; 13.5 — the trial starts at the account's first desktop sign-in.
              (jdbc/execute! tx ["INSERT INTO desktop_accounts (user_id, scm)
                                  VALUES (?, md5(random()::text)) ON CONFLICT (user_id) DO NOTHING" user-id])
              (json-response 200 {:access_token token
                                  :user_id user-id
                                  :username username
                                  :id_block (allocate-block! tx user-id token-id)}))))))))

(defn bearer-token
  "The token-holder of a request: {:user-id :token-id}, or nil for a missing,
   unknown or revoked token (3.2)."
  [request]
  (when-let [auth (get-in request [:headers "authorization"])]
    (when-let [[_ token] (re-matches #"Bearer\s+(\S+)" auth)]
      (when-let [row (jdbc/execute-one! db/ds ["UPDATE desktop_tokens SET last_used_at = now()
                                                WHERE token_hash = ? AND revoked_at IS NULL
                                                RETURNING id, user_id" (sha256-hex token)] opts)]
        {:user-id (:user_id row) :token-id (:id row)}))))

(defn id-block-handler
  "POST /api/desktop/id-block (1.2.5). Allowed in every license state,
   because a desktop that cannot mint ids cannot be used locally (13.4.4)."
  [request]
  (if-let [{:keys [user-id token-id]} (bearer-token request)]
    (json-response 200 (allocate-block! db/ds user-id token-id))
    (json-response 401 {:error "unauthorized"})))

;; ── Devices in Settings (3.2) ──────────────────────────────────────

(defn list-tokens
  "The account's desktop installs, newest first."
  [user-id]
  (jdbc/execute! db/ds ["SELECT id, device_name, created_at, last_used_at, revoked_at
                         FROM desktop_tokens WHERE user_id = ? ORDER BY created_at DESC" user-id] opts))

(defn revoke-token!
  "Revoke one install. The next sync and block request from it get 401.
   Post: true when a live token of this account was revoked."
  [user-id token-id]
  (pos? (:next.jdbc/update-count
         (jdbc/execute-one! db/ds ["UPDATE desktop_tokens SET revoked_at = now()
                                    WHERE id = ? AND user_id = ? AND revoked_at IS NULL"
                                   token-id user-id]))))
