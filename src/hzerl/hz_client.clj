(ns hzerl.hz-client
  (:require
   [taoensso.nippy :as nippy]
   [clj-erl.static :refer :all]
   [hzerl.hz-type  :refer :all])
  (:import
   [clojure.lang PersistentArrayMap Keyword]
   [com.hazelcast.core HazelcastInstance]
   [com.hazelcast.client.config ClientConfig]
   [com.hazelcast.client HazelcastClient HazelcastClientProxy]
   [java.util.concurrent TimeUnit]
   [java.io ByteArrayInputStream ByteArrayOutputStream]
   ))

(def ^{:const true :doc "default constants for connection to Hazelcat cluster"}
  default-config {:hosts ["localhost"]
                  :user "dem"
                  :password "dev-pass"
                  :connAtemptLimit Integer/MAX_VALUE
                  :connAtemptPeriod 2000
                  :connTimeout 5000
                  })

(extend-protocol ^{:doc "return list of node names in cluster"}
  ErlangProto
  HazelcastClientProxy
  (encode [this] (->> this (.getCluster) (.getMembers) (map str) encode)))

(defn- get-defaults
  "return value from default-config var if it havent in args"
  [^Keyword key ^PersistentArrayMap config-map]
  (get config-map key (get default-config key)))

(defn ^ClientConfig init
  "init Hazelcast connection"
  [^PersistentArrayMap config]
  (let [hosts               (get-defaults :hosts config)
        user                (get-defaults :user  config)
        pass                (get-defaults :password config)
        conn-atempt-limit   (get-defaults :connAtemptLimit config)
        conn-atempt-period  (get-defaults :connAtemptPeriod config)
        conn-timeout        (get-defaults :connTimeout config)
        hz-cfg (->
                 (new ClientConfig)
                 (.setAddresses hosts)
                 (.setConnectionAttemptLimit conn-atempt-limit)
                 (.setConnectionAttemptPeriod conn-atempt-period)
                 (.setConnectionTimeout conn-timeout))]
    ;; auth config
    (->
     hz-cfg
     (.getGroupConfig)
     (.setName user)
     (.setPassword pass))

    hz-cfg))

(defn ^HazelcastInstance connect
  "connect to Hazelcast with config params."
  [^PersistentArrayMap config]
  (->> config
       init
       HazelcastClient/newHazelcastClient))

(defn- serialize
  "serialize clojure structs"
  [expr]
  (nippy/freeze expr))

(defn- deserialize
  "deserialize clojure structs"
  [expr]
  (when expr
    (nippy/thaw expr)))


(defn cmd
  [^HazelcastInstance conn ^Keyword hz-type ^String name hz-cmd & args]
  (case hz-type 
    :map (apply hz-cmd  (conj args (.getMap conn name)))
    [:info "indefined type" hz-type name args]))

(defn mput
  [conn map-name key value]
  (->
   conn
   (.getMap map-name)
   (.put key value)))

(defn mput-expire
  [conn map-name key value expire]
  (->
   conn
   (.getMap map-name)
   (.put key value expire TimeUnit/MILLISECONDS)))

(defn mget
  [conn map-name key]
  (->
   conn
   (.getMap  map-name)
   (.get key)))

(defn mclear
  [conn map-name]
  (->
   conn
   (.getMap map-name)
   (.clear)))

