(ns hzerl.core
  (:require
   [environ.core              :refer (env)]
   [clojure.string            :refer (split join)]
   [clj-erl.node   :as node   :refer :all]
   [clj-erl.static :as static :refer :all])
  (:gen-class))

(def self-node "hzerlnode")
(def self-mbox "hzerlmbox")

(defn ^String make-node-name
  [erlang-node]
  (->> (split erlang-node #"@")
       second
       (list self-node)
       (join "@" )))

(defn echo-handler
  [erl-mbox erl-node self]
  (send! self erl-mbox erl-node [:hzerl_node (:name self)])
  (send! self erl-mbox erl-node [:hzerl_mbox self-mbox])
  (loop [n true]
    (let [message  (recv self)]
      (when-not (= :stop message)
        (send! self erl-mbox erl-node message)
        (recur true)))))

(defn -main 
  "main fun"
  [& args]
  (let [remote-mbox (:erlang-mbox env)
        remote-node (:erlang-node env)
        self (node/init-node (make-node-name remote-node) self-mbox)]
    (echo-handler remote-mbox remote-node self)))
