(ns lambdaisland.regal.platform
  (:require [clojure.string :as str]
            [lambdaisland.regal.code-points :as cp])) ;; clj

(defn hex->int [hex]
  (Long/parseLong hex 16))

(defn int->hex [i]
  (str/upper-case
   (Integer/toHexString i)))

(defn char->long [ch]
  (if (string? ch)
    (case (count ch)
      1 (long (first ch))
      2 (first (cp/code-point-seq ch)))
    (long ch)))

(defn parse-int [i]
  (Long/parseLong i 10))
