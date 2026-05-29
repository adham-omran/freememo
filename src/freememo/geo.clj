(ns freememo.geo
  "Server-side geo-IP + client-IP extraction.

   `client-ip` precedence: cf-connecting-ip → leftmost x-forwarded-for → :remote-addr.
   `country-of` reads DB-IP Lite Country .mmdb from `resources/geo/`; nil on
   missing/private/loopback/unknown. No exceptions surface. See
   plans/wayl-currency-conversion.md."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [taoensso.telemere :as tel])
  (:import
   [com.maxmind.db Reader]
   [java.net InetAddress]))

(defonce ^:private reader
  (try
    (if-let [url (io/resource "geo/dbip-country-lite.mmdb")]
      (with-open [in (io/input-stream url)]
        (Reader. in))
      (do (tel/log! {:level :warn :id ::mmdb-missing}
            "geo/dbip-country-lite.mmdb not on classpath — country-of always nil")
          nil))
    (catch Exception e
      (tel/error! {:id ::mmdb-init} e)
      nil)))

(defn- skip-lookup?
  "True for addresses we never want to consult the DB for: loopback (127.x, ::1),
   site-local (10/8, 172.16/12, 192.168/16, fc00::/7), link-local, any-local."
  [^InetAddress addr]
  (or (.isLoopbackAddress addr)
      (.isSiteLocalAddress addr)
      (.isLinkLocalAddress addr)
      (.isAnyLocalAddress addr)))

(defn country-of
  "ISO-3166 alpha-2 country code for `ip-string`, or nil.
   Nil on: nil/blank input, private/loopback/link-local address, malformed,
   not-in-DB, reader unavailable. Never throws."
  [ip-string]
  (when (and reader ip-string (not (str/blank? ip-string)))
    (try
      (let [addr (InetAddress/getByName (str/trim ip-string))]
        (when-not (skip-lookup? addr)
          (some-> (.get reader addr java.util.Map)
            (get "country")
            (get "iso_code"))))
      (catch Exception _ nil))))

(defn client-ip
  "Real client IP from a Ring `request`, or nil.
   Precedence: cf-connecting-ip → leftmost trimmed x-forwarded-for → :remote-addr.
   Trusted upstream chain: Cloudflare and/or nginx/Caddy must overwrite (not
   append) x-forwarded-for to prevent client spoofing."
  [request]
  (or (some-> (get-in request [:headers "cf-connecting-ip"]) str/trim not-empty)
      (some-> (get-in request [:headers "x-forwarded-for"])
        (str/split #",")
        first
        str/trim
        not-empty)
      (:remote-addr request)))
