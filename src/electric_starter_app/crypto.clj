(ns electric-starter-app.crypto
  "Password-derived AES-256-GCM encryption for the OpenAI API key.
   Derives a stable 256-bit key from the user's login password using PBKDF2-SHA256.
   The derived key is stored base64-encoded in the server-side Ring session.
   Even with full DB access, the API key cannot be decrypted without the user's password."
  (:import [javax.crypto Cipher SecretKeyFactory]
           [javax.crypto.spec SecretKeySpec GCMParameterSpec PBEKeySpec]
           [java.security SecureRandom]
           [java.util Arrays Base64]))

(def ^:private PBKDF2-ALGO "PBKDF2WithHmacSHA256")
(def ^:private ITERATIONS 100000)
(def ^:private KEY-BYTES 32)  ; 256-bit AES key

(defn derive-key
  "Derive a 256-bit AES key from password + user-id using PBKDF2-SHA256.
   Returns a base64-encoded string of the raw key bytes.
   The same password + user-id always produces the same key, so existing
   encrypted data remains accessible as long as the user's password is unchanged."
  [^String password user-id]
  (when (and password (seq password))
    (let [salt-str (str "ecm-" user-id)  ; Application-specific, per-user salt
          salt     (.getBytes salt-str "UTF-8")
          spec     (PBEKeySpec. (.toCharArray password) salt ITERATIONS (* 8 KEY-BYTES))
          factory  (SecretKeyFactory/getInstance PBKDF2-ALGO)
          key-bytes (-> (.generateSecret factory spec) .getEncoded)]
      (.encodeToString (Base64/getEncoder) key-bytes))))

(defn- str->secret-key
  "Parse a base64-encoded key string into an AES SecretKeySpec."
  [enc-key-str]
  (when enc-key-str
    (let [key-bytes (.decode (Base64/getDecoder) ^String enc-key-str)]
      (SecretKeySpec. key-bytes "AES"))))

(defn encrypt
  "Encrypt plaintext with AES-256-GCM using the derived key.
   enc-key-str: base64-encoded key from derive-key / session.
   Returns base64(nonce || ciphertext), or plaintext unchanged if enc-key-str is nil."
  [^String plaintext enc-key-str]
  (if (and enc-key-str (seq plaintext))
    (let [secret-key (str->secret-key enc-key-str)
          nonce  (byte-array 12)
          _      (.nextBytes (SecureRandom.) nonce)
          cipher (Cipher/getInstance "AES/GCM/NoPadding")
          _      (.init cipher Cipher/ENCRYPT_MODE secret-key (GCMParameterSpec. 128 nonce))
          enc    (.doFinal cipher (.getBytes plaintext "UTF-8"))
          combined (byte-array (+ 12 (alength enc)))]
      (System/arraycopy nonce 0 combined 0 12)
      (System/arraycopy enc   0 combined 12 (alength enc))
      (.encodeToString (Base64/getEncoder) combined))
    plaintext))

(defn decrypt
  "Decrypt base64-encoded AES-256-GCM ciphertext using the derived key.
   enc-key-str: base64-encoded key from derive-key / session.
   Returns plaintext, or ciphertext unchanged if enc-key-str is nil.
   Handles two legacy cases:
   - Not valid base64 (stored as plaintext before encryption was added): return as-is
   - Valid base64 but GCM fails (encrypted with old ENCRYPTION_KEY scheme): return \"\"
     so callers treat it as missing and prompt the user to re-enter the key."
  [^String ciphertext enc-key-str]
  (if (and enc-key-str (seq ciphertext))
    (try
      ;; Attempt base64 decode first
      (let [combined (.decode (Base64/getDecoder) ciphertext)]
        ;; Value IS base64 — attempt GCM decryption
        (try
          (let [secret-key (str->secret-key enc-key-str)
                nonce      (Arrays/copyOfRange combined 0 12)
                enc        (Arrays/copyOfRange combined 12 (alength combined))
                cipher     (Cipher/getInstance "AES/GCM/NoPadding")
                _          (.init cipher Cipher/DECRYPT_MODE secret-key (GCMParameterSpec. 128 nonce))]
            (String. (.doFinal cipher enc) "UTF-8"))
          (catch Exception _
            ;; GCM failed: value was encrypted with a different key (old ENCRYPTION_KEY scheme)
            ;; Return "" so caller treats it as missing — user will see "API key not configured"
            "")))
      (catch IllegalArgumentException _
        ;; Not valid base64: value is plaintext (stored before encryption was added)
        ciphertext))
    ciphertext))
