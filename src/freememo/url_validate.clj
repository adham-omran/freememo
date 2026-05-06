(ns freememo.url-validate
  "Block SSRF vectors. Reject non-http(s), loopback, link-local, private,
   multicast, and any-local addresses. Resolves DNS and checks every
   returned address — a host with one private and one public A record fails."
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
