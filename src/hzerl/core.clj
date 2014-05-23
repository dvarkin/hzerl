(ns hzerl.core
  (:require
   [clojure.tools.trace           :refer (trace)]
   [environ.core                  :refer (env)]
   [clojure.string                :refer (split join)]
   [clj-erl.node    :as node      :refer :all]
   [clj-erl.static  :as static    :refer :all]
   [hzerl.hz-client :as hz-client :refer (connect)]
   [clojure.core.async :as async  :refer (>!! >! chan <! <!! go thread)]
   [clj-pid.core    :as pid       :refer (current)])
  (:import [clojure.lang Keyword PersistentArrayMap PersistentVector])
  (:gen-class))

(def ^{:const true :private true} self-node "hzerlnode")
(def ^{:const true :private true} self-mbox "hzerlmbox")

(defn keep-alive
  [self erl-node]
  (Thread/sleep 1000)
  (if-not
      (ping self erl-node 500) 
    (System/exit 0)
    (recur self erl-node)))

(defn ^String make-node-name
  "from received node name, make own node name.
   e.g. dem@localhost -> hzerlnode@localhost"
  [^String erlang-node]
  (->> (split erlang-node #"@")
       second
       (list self-node)
       (join "@" )))

(defn ^PersistentArrayMap vec-to-map
  "very bad solution, but must be for backward comp with erl16 and low, where no Maps"
  [^PersistentVector message]
  (if (vector? message)
    (apply hash-map message)
    message))

(defn make-send-fn
  [self ^PersistentArrayMap  message]
  (fn [response] (send! self (:pid message) response)))

(defn hz-connect
  [send-fn config]
  (let [conn (hz-client/connect config)]
    (send-fn conn)
    (partial hz-client/cmd conn)))

(defn wait-for-config
  "wait for configuration message from erlang node. return make handler func"
  [self]
  (let [message (->> self recv vec-to-map)]
    (if (= :connect  (:cmd message))
      (hz-connect (make-send-fn self message) (-> message :config vec-to-map))
      (recur self))))

(defn hz-cmd
  [send-fn handler args]
  (->>  (apply handler args)
        send-fn))

(defn message-handler
  [self hz-client raw-message]
  (let [message  (vec-to-map raw-message)
        method (:cmd message)
        args (-> message :args vec-to-map (conj method))]
    (try
      (if (= :sync method)
        (->>
         (apply hz-client args)
         (send! self (:pid  message) ))
        (apply hz-client args))
      (catch Exception e
        (println (str "error:" (.getMessage e)))))))

(defn chan-handler [handler ch]
  (while true
    (let [args (<!! ch)]
      (apply handler args))))

(defn echo-handler
  [self hz-handler raw-message]
  (let [v (vec-to-map raw-message)]
    (send! self (:pid v) "ok")
    hz-handler))

(defn cmd-handler
  [^String erl-mbox ^String erl-node self]
  ;; initital self info for erlang node
  (send! self erl-mbox erl-node [:hzerl_node (keyword (:name self))])
  (send! self erl-mbox erl-node [:hzerl_mbox (keyword self-mbox)])
  (send! self erl-mbox erl-node [:hzerl_pid  (pid/current)])
  (future (keep-alive self erl-node))
  (let [hz-client (wait-for-config self)
        chans (repeatedly 10 chan)]
    (doseq [c chans] (thread (chan-handler message-handler c)))
    (while true
      (let [r (recv self)]
        (thread (>!! (rand-nth chans) [self hz-client r]))))
    (System/exit 0)))

(defn -main 
  "main fun"
  [& args]
  (let [remote-mbox (:erlang-mbox env)
        remote-node (:erlang-node env)
        self (node/init-node (make-node-name remote-node) self-mbox)]
    (cmd-handler remote-mbox remote-node self)))
