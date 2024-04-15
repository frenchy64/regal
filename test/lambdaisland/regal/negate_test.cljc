(ns lambdaisland.regal.negate-test
  (:require [clojure.test :refer [deftest is]]
            [lambdaisland.regal.negate :refer [negate]]))

(deftest negate-test
  ;; TODO investigate further
  (is (= [:negative-lookahead [:char 13]] (negate [:alt [:char 13]])))
  ;; TODO investigate further
  (is (= [:alt [:char 13]] (negate [:negative-lookahead [:char 13]])))
  (is (= [:char 13] (negate [:not [:char 13]])))
  (is (= [:not [:char 13]] (negate [:char 13])))
  (is (= [:alt
          [:cat [:char 11] [:not [:char 13]]]
          [:cat [:not [:char 11]] [:char 13]]
          [:cat [:not [:char 11]] [:not [:char 13]]]]
        (negate [:cat [:char 11] [:char 13]])))
  (is (= [:alt] (negate :any)))
  (is (= :any (negate [:alt])))
  )
