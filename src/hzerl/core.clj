(ns hzerl.core
  (:require [environ.core :refer (env)])
  (:gen-class))

(defn -main 
  "main fun"
  [& args]
  (println "erlang node" (:erlang-node env)))
