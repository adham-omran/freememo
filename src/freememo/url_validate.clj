(ns freememo.url-validate
  "Block SSRF vectors. Reject non-http(s), loopback, link-local, private,
   multicast, and any-local addresses. Resolves DNS and checks every
   returned address — a host with one private and one public A record fails.

   Known gap (DNS rebinding TOCTOU): `safe-url?` resolves the host once,
   then the actual fetch (clj-http) resolves again. A malicious DNS server
   can return a public address to the validator and a private address to
   the fetcher. Closing this requires resolving once and pinning the IP at
   the HTTP-client level; not implemented here."
  (:require [clojure.string :as str])
  (:import [java.net URI InetAddress UnknownHostException]))

(defn- private-or-loopback?
  [^InetAddress addr]
  (or (.isAnyLocalAddress addr)
    (.isLoopbackAddress addr)
    (.isLinkLocalAddress addr)
    (.isSiteLocalAddress addr)
    (.isMulticastAddress addr)))

(defn safe-url?
  "True iff url-string parses as http/https with a host whose every resolved
   IP is publicly routable. Catches malformed URIs and DNS failures as unsafe."
  [url-string]
  (try
    (let [uri (URI. (str url-string))
          scheme (some-> (.getScheme uri) str/lower-case)
          host (.getHost uri)]
      (boolean
        (and host
          (#{"http" "https"} scheme)
          (let [addrs (InetAddress/getAllByName host)]
            (and (seq addrs)
              (every? (complement private-or-loopback?) addrs))))))
    (catch UnknownHostException _ false)
    (catch Exception _ false)))
