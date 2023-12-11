(ns findex.store)

(defprotocol StorageInterface
  (fetch [this tokens]
    "Fetch the values associated to the given tokens from the store. Do not
  return tokens with no associated value.")
  (upsert [this token->update]
    "Conditionally update the value currently associated to each given token
  with the new value when the current value is equal to old value given. Return
  the current value associated to each token for which the old value did not
  match the current value.")
  (insert [this token->value]
    "Insert the given token/value pairs in store. Do not update existing values.")
  (delete [this tokens]
    "Delete the token/value pairs from the store for each given token.")
  (dump [this]
    "Return the store.")
  (dump-tokens [this]
    "Return the tokens stored."))


(defn in-memory-store []
  (let [store (atom {})]
    (reify StorageInterface
      (fetch [this tokens]
        (transduce (comp (map (fn [token] [token (@store token)]))
                         (filter (fn [[token value]] ((comp not nil?) value))))
                   (completing (fn [res [token value]] (assoc res token value)))
                   {}
                   tokens))

      (insert [this token->value]
        (loop [token->value token->value]
          (if (empty? token->value)
            nil
            (let [[token value] (first token->value)]
              (swap! store assoc token value)
              (recur (rest token->value))))))

      (upsert [this token->update]
        (loop [token->update token->update
               rejections {}]
          (if (empty? token->update)
            rejections
            (let [[token [old-value new-value]] (first token->update)
                  cur-value (@store token)]
              (if (= old-value cur-value)
                (do (swap! store assoc token new-value)
                    (recur (rest token->update)
                           rejections))
                (recur (rest token->update)
                       (assoc rejections token cur-value)))))))

      (delete [this tokens]
        (loop [success false]
          (if success
            nil
            (recur (compare-and-set! store
                                     @store
                                     (reduce #(dissoc %1 %2)
                                             @store
                                             tokens))))))

      (dump [this]
        @store)

      (dump-tokens [this]
        (keys @store)))))
