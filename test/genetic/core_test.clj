(ns genetic.core-test
  (:require [clojure.test :as ct]
            [genetic.core :as core]))

(ct/deftest genetic-algorithm-test
  (ct/testing "running the algorithm returns :ok"
    (ct/is (= :ok (core/init! :phrase "burns like FIRE"
                              :mutation-rate 0.01
                              :maximum-population 400)))))
