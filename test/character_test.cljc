(ns character-test
  (:require [clojure.test :refer [deftest is testing]]
            [character]))
(deftest namespace-loads
  (testing "the restored CLJC namespace loads"
    (is (some? character))))
