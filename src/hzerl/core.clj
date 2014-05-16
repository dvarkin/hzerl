(ns hzerl.core
  (:require
   [clojure.tools.trace           :refer (trace)]
   [environ.core                  :refer (env)]
   [clojure.string                :refer (split join)]
   [clj-erl.node    :as node      :refer :all]
   [clj-erl.static  :as static    :refer :all]
   [hzerl.hz-client :as hz-client :refer (connect)])
  (:gen-class))

(def ^{:const true :private true} self-node "hzerlnode")
(def ^{:const true :private true} self-mbox "hzerlmbox")

(defn ^String make-node-name
  "from received node name, make own node name.
   e.g. dem@localhost -> hzerlnode@localhost"
  [^String erlang-node]
  (->> (split erlang-node #"@")
       second
       (list self-node)
       (join "@" )))

(defn vec-to-map
  "very bad solution, but must be for backward comp with erl16 and low, where no Maps"
  [message]
  (if (vector? message)
    (apply hash-map message)
    message))

(defn commands
  [message]
  (case (:cmd message)
    :stop nil
    :connect (hz-client/connect (:config message))
    [:info "undefined cmd" message]
    ))

(defn cmd-handler
  [^String erl-mbox ^String erl-node self]
  (send! self erl-mbox erl-node [:hzerl_node (keyword (:name self))])
  (send! self erl-mbox erl-node [:hzerl_mbox (keyword self-mbox)])
  (println "exti from loop"
   (loop [n true]
     (let [message  (->> (recv self)
                         vec-to-map)]
       (when-let [feedback (commands message)]
         (println "feed" feedback)
         (send! self erl-mbox erl-node feedback)
         (recur true))))
   (System/exit 0)))

(defn -main 
  "main fun"
  [& args]
  (let [remote-mbox (:erlang-mbox env)
        remote-node (:erlang-node env)
        self (node/init-node (make-node-name remote-node) self-mbox)]
    (cmd-handler remote-mbox remote-node self)))
