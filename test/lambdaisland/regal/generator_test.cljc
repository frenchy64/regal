(ns lambdaisland.regal.generator-test
  (:require [clojure.test :refer [deftest is]]
            [lambdaisland.regal.generator :as rg]
            [lambdaisland.regal.code-points :as cp]))

(deftest -not-code-points-test
  (is (= (range 256) (rg/-not-code-points [:not "𓅡"] nil)))
  (is (= (remove #{97} (range 256)) (rg/-not-code-points [:not "a"] nil)))
  (is (= (remove #{97 98 99} (range 256)) (rg/-not-code-points [:not "abc"] nil)))
  (is (= (remove #{97 98 99} (range 256)) (rg/-not-code-points [:not "abc"] nil))))

(defn sample-code-points [r opts]
  (mapv (comp vec cp/code-point-seq)
        (rg/sample r opts)))

(deftest generator-test
  (is (= [[27] [135] [256] [178] [101] [92] [111] [78] [91] [129]]
         (sample-code-points [:not "a"] {:seed 0})))
  (is (= [[27] [134] [256] [178] [100] [92] [110] [78] [91] [129]]
         (sample-code-points [:not "𓅡"] {:seed 0}))))
