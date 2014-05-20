(ns hzerl.hz-type
  (:import
   [java.util.concurrent TimeUnit]
   [com.hazelcast.core IMap]
   [com.hazelcast.client.proxy ClientMapProxy]))

(defprotocol MapProtocol
  (GET [this method key] " Returns the value for the specified key, or null if this map does not contain this key.")
  (SET
    [this method key value]
    [this method key value expire]
    "Associates the specified value with the specified key in this map If the map previously contained a mapping for the key, the old value is replaced by the specified value. Or Puts an entry into this map with a given ttl (time to live) value.")
  (DEL
    [this method key]
    [this method key value]
    "Removes the mapping for a key from this map if it is present.
     Removes the entry for a key only if currently mapped to a given value."))

(extend-protocol MapProtocol
  ClientMapProxy
  (GET [this method key] (.get this key))
  (SET
    ([this method key value]
       (if (= :async method)
         (.putAsync this key value)
         (.put this key value)))
    ([this method key value expire]
       (if (= :async method)
         (.putAsync this key value expire TimeUnit/MILLISECONDS)
         (.put this key value expire TimeUnit/MILLISECONDS))))
  (DEL
    ([this method key]
       (if (= :async method)
         (.removeAsync this key)
         (.remove this key)))
    ([this method key value]
       (if (= :async method)
         (.removeAsync this key value)
         (.remove this key value)))))
