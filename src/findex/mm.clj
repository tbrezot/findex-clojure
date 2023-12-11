(ns findex.mm
  (:require [findex.edx :as edx]
            [findex.crypto :as crypto]))

(defprotocol MultiMap
  (search [this tags]
    "Search for the values associated to the given tags.")
  (insert [this tag->values]
    "Insert the given values to the list of values associated to the
  corresponding tags.")
  (delete [this tag->values]
    "Delete the given values from the list of values associated to the
  corresponding tags."))

(defn instantiate [seed et-builder ct-builder]
  (let [et-seed (crypto/kdf-256 seed "Entry Table")
        ct-seed (crypto/kdf-256 seed "Chain Table")
        entry-edx (et-builder et-seed)
        chain-edx (ct-builder ct-seed)]))
