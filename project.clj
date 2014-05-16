(defproject hzerl "0.1.0-SNAPSHOT"
  :description "Erlang driver for Hazelcast Data Grid"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.trace "0.7.8"]
                 [clj-erl "0.1.0"]
                 [environ "0.5.0"]
                 [com.hazelcast/hazelcast-client "3.1.7"]
                 [com.taoensso/nippy "2.5.2"] 
                 ]
  :plugins [[lein-environ "0.5.0"]]
  :aot [hzerl.core]
  :main hzerl.core
  :uberjar-name "hz.jar")


