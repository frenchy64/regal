(ns lambdaisland.regal.generator-test
  (:require [clojure.test :refer [deftest is]]
            [lambdaisland.regal.generator :as regal-gen]))

(deftest -not-code-points-test
  (is (= (range 256) (regal-gen/-not-code-points [:not "𓅡"] nil)))
  (is (= (remove #{97} (range 256)) (regal-gen/-not-code-points [:not "a"] nil)))
  (is (= (remove #{97 98 99} (range 256)) (regal-gen/-not-code-points [:not "abc"] nil)))
  (is (= (remove #{97 98 99} (range 256)) (regal-gen/-not-code-points [:not "abc"] nil))))
