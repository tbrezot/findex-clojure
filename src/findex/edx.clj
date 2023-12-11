(ns findex.edx
  (:require [findex.store :as store]
            [findex.crypto :as crypto]))

(defprotocol DictionaryEncryptionScheme
  "An Dictionary Encryption scheme (DX-Enc) allows securely handling
  an encrypted dictionary."
  (tokenize [this tag]
    "Generate the secure token associated to the given tag.")
  (search [this tokens]
    "Search for the encrypted values associated to the given tokens.")
  (encrypt [this plaintext info]
    "Encrypt the given plaintext using info as associated data when provided.")
  (upsert [this token->update]
    "Atomically replace the value currently associated to the given tokens with
  the new value if the old value is equal to the current one. Return the current
  value associated to each tag for which the associated old value did not match.")
  (insert [this token->value]           ;TODO: currently UB if a value already exists
    "Insert the given values for the associated tokens.")
  (delete [this tokens]
    "Remove the values associated to the given tokens from the encrypted dictionary.")
  (decrypt [this ciphertext info]
    "Decrypt the given ciphertext using info as associated data when provided."))

(defn make-edx [seed store]
  (let [kmac (crypto/setup-kmac-256 (crypto/kdf-256 seed "KMAC"))
        aead (crypto/setup-aead     (crypto/kdf-256 seed "AEAD"))]

    (reify DictionaryEncryptionScheme

      ;; Cryptographic interface.
      (tokenize [this tag] (kmac tag))
      (encrypt [this plaintext info]  (crypto/encrypt aead plaintext  info))
      (decrypt [this ciphertext info] (crypto/decrypt aead ciphertext info))

      ;; Non-cryptographic interface.
      (search [this tokens] (store/fetch store tokens))
      (upsert [this token->update] (store/upsert store token->update))
      (insert [this token->value] (store/insert store token->value))
      (delete [this tokens] (store/delete store tokens)))))
