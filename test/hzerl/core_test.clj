(ns hzerl.core-test
  (:require [clojure.test :refer :all]
            [hzerl.core :refer :all]))

(deftest maek-node-name-test
  (testing "fix making node name"
    (is (= "hzerlnode@localhost" (make-node-name "someerlangnodename@localhost")))))
