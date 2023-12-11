(ns findex.crypto)

(defn kdf-256
  "Derive a 256-byte key using the given seed and info string."
  ([seed] (hash-ordered-coll '(seed)))
  ([seed info] (hash-ordered-coll (conj (map byte info) seed))))

(defn setup-kmac-256
  "Setup a KMAC-256 using the given 256-byte key."
  [key]
  (fn
    ([b] (hash-ordered-coll [key b]))
    ([b info] (hash-ordered-coll (transduce (map byte) conj [key b] info)))))

(defprotocol Aead
  (encrypt [this plaintext info]
    "Encrypt the given plaintext using info as associated data.")
  (decrypt [this ciphertext info]
    "Decrypt the given ciphertext using info as associated data."))

(defn setup-aead [key]
  (reify Aead
    (encrypt [this plaintext info]
      (bit-xor key info plaintext))
    (decrypt [this ciphertext info]
      (bit-xor key info ciphertext))))
