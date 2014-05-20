(ns hzerl.core
  (:require
   [clojure.tools.trace           :refer (trace)]
   [environ.core                  :refer (env)]
   [clojure.string                :refer (split join)]
   [clj-erl.node    :as node      :refer :all]
   [clj-erl.static  :as static    :refer :all]
   [hzerl.hz-client :as hz-client :refer (connect)])
  (:import [clojure.lang Keyword])
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

(defn hz-connect
  [send-fn config]
  (let [conn (hz-client/connect config)]
    (send-fn conn)
    (partial hz-client/cmd conn)))

(defn hz-cmd
  [send-fn handler args]
  (future
    (->> (apply handler args)
         send-fn))
  handler)

(defn commands
  [message hz-handler send-fn]
  (case (:cmd message)
    :stop false
    :connect (hz-connect send-fn (:config message))     
    :hz      (hz-cmd send-fn hz-handler (:args message)) 
    (do (send-fn [:info "undefined hz cmd:)" message]) hz-handler)
    ))

(defn cmd-handler
  [^String erl-mbox ^String erl-node self]
  (send! self erl-mbox erl-node [:hzerl_node (keyword (:name self))])
  (send! self erl-mbox erl-node [:hzerl_mbox (keyword self-mbox)])
  (loop [hz-handler true]
    (when-not (false? hz-handler)
      (let [message  (->>
                      (recv self)
                      vec-to-map)
            method (:method message)
            pid (:pid message)
            send-fn (partial send! self pid)
            cmd (:hzcmd message) 
            feedback (try
                       (-> cmd
                           vec-to-map
                           (commands hz-handler send-fn))
                       (catch Exception e
                         (do
                           (send-fn (str "error:" (.getMessage e)))
                           hz-handler)))]
        (recur feedback))))
  (System/exit 0))

(defn -main 
  "main fun"
  [& args]
  (let [remote-mbox (:erlang-mbox env)
        remote-node (:erlang-node env)
        self (node/init-node (make-node-name remote-node) self-mbox)]
    (cmd-handler remote-mbox remote-node self)))
