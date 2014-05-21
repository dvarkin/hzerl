(ns hzerl.core
  (:require
   [clojure.tools.trace           :refer (trace)]
   [environ.core                  :refer (env)]
   [clojure.string                :refer (split join)]
   [clj-erl.node    :as node      :refer :all]
   [clj-erl.static  :as static    :refer :all]
   [hzerl.hz-client :as hz-client :refer (connect)]
   [clojure.core.async :as async :refer (>! chan <! <!! go )])
  (:import [clojure.lang Keyword])
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

(defn vec-to-map
  "very bad solution, but must be for backward comp with erl16 and low, where no Maps"
  [message]
  (if (vector? message)
    (apply hash-map message)
    message))

(defn hz-connect
  [send-fn config]
  (let [conn (hz-client/connect config)]
    (send-fn conn)
    (partial hz-client/cmd conn)))

(defn hz-cmd
  [send-fn handler args]
  (->>  (apply handler args)
        send-fn)
  handler)

(defn commands
  [message hz-handler send-fn]
  (case (:cmd message)
    :stop false
    :connect (hz-connect send-fn (:config message))     
    :hz      (hz-cmd send-fn hz-handler (:args message)) 
    (do (send-fn [:info "undefined hz cmd:)" message]) hz-handler)
    ))

(defn message-handler
  [self hz-handler raw-message]
  (let [message  (vec-to-map raw-message)
        method (:method message)
        pid (:pid message)
        send-fn (partial send! self pid)
        cmd (:hzcmd message)]
    (try
      (-> cmd
          vec-to-map
          (commands hz-handler send-fn))
      (catch Exception e
        (do
          (send-fn (str "error:" (.getMessage e)))
          hz-handler)))))

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
  (send! self erl-mbox erl-node [:hzerl_node (keyword (:name self))])
  (send! self erl-mbox erl-node [:hzerl_mbox (keyword self-mbox)])
  (future (keep-alive self erl-node))
  (let [chans (repeatedly 10 chan)]
    (doseq [c chans] (go (chan-handler echo-handler c)))
    (while true
      (let [r (recv self)]
        (go (>! (rand-nth chans) [self 1 r]))))
    (System/exit 0)))

(defn -main 
  "main fun"
  [& args]
  (let [remote-mbox (:erlang-mbox env)
        remote-node (:erlang-node env)
        self (node/init-node (make-node-name remote-node) self-mbox)]
    (cmd-handler remote-mbox remote-node self)))
