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
   [java.util.concurrent FutureTask]
   ))

(def ^{:const true :doc "default constants for connection to Hazelcat cluster"}
  default-config {:hosts ["localhost"]
                  :user "dem"
                  :password "dev-pass"
                  :connAtemptLimit Integer/MAX_VALUE
                  :connAtemptPeriod 2000
                  :connTimeout 5000
                  :connPoolSize 1000
                  })

(extend-protocol ^{:doc "return list of node names in cluster"}
  ErlangProto
  HazelcastClientProxy
  (encode [this] (->> this (.getCluster) (.getMembers) (map str) encode))
  FutureTask
  (encode [this] (encode :ok)))

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
        conn-pool-size      (get-defaults :connPoolSize config)
        conn-atempt-limit   (get-defaults :connAtemptLimit config)
        conn-atempt-period  (get-defaults :connAtemptPeriod config)
        conn-timeout        (get-defaults :connTimeout config)
        hz-cfg (->
                 (new ClientConfig)
                 (.setAddresses hosts)
                 (.setConnectionAttemptLimit conn-atempt-limit)
                 (.setConnectionAttemptPeriod conn-atempt-period)
                 (.setConnectionTimeout conn-timeout)
                 (.setConnectionPoolSize conn-pool-size)
;                 (.setExecutorPoolSize 100)
                 )]
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
  [^HazelcastInstance conn ^Keyword method ^Keyword hz-type ^String name ^String hz-cmd & args]
  (case hz-type 
    :map (apply (ns-resolve 'hzerl.hz-type (symbol hz-cmd))
                (conj args method (.getMap conn name)))
    [:info "indefined type" hz-type name args]))
