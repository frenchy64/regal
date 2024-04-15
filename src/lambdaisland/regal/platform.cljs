(ns lambdaisland.regal.platform
  (:require [clojure.string :as str]
            [lambdaisland.regal.code-points :as cp])) ;; cljs

(defn hex->int [hex]
  (js/parseInt hex 16))

(defn int->hex [i]
  (str/upper-case
   (.toString i 16)))

(defn char->long [ch]
  (case (count ch)
    1 (.charCodeAt ^String ch)
    2 (first (cp/code-point-seq ch))))

(defn parse-int [i]
  (js/parseInt i 10))
