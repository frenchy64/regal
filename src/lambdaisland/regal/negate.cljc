(ns lambdaisland.regal.negate
  (:require [lambdaisland.regal :as regal]
            [lambdaisland.regal.platform :as platform]
            [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(declare negate)

(defmulti -negate (fn [r opts] (if (keyword? r) r (first r)))
  :default ::default)

(defmethod -negate ::default [_ _])
(defmethod -negate :char [r opts] [:not r])
(defmethod -negate :ctrl [r opts] [:not r])
(defmethod -negate :not [r opts] (second r))
(defmethod -negate :any [r opts] [:alt :newline :return])
(defmethod -negate :newline [r opts] [:not :newline])
(defmethod -negate :return [r opts] [:not :return])

(defmethod -negate :alt [r opts]
  (if (= 1 (count r))
    [:alt :any :newline :return]
    ;;TODO ??
    [:negative-lookahead r]))
;;TODO ??
(defmethod -negate :negative-lookahead [r opts] (second r))

(defmethod -negate :cat [[_ & rs] opts]
  (let [nrs (map #(negate % opts) rs)]
    (when (every? identity nrs)
      (into [:alt]
            (map #(into [:cat] %))
            (next (apply comb/cartesian-product
                         (map list rs nrs)))))))

(defmethod -negate :repeat [[_ r min max] opts]
  (let [fixed (when (not max) min)]
    (if fixed
      (-> [:alt]
          (cond-> (pos? fixed) (conj [:repeat r 0 (dec fixed)]))
          (conj [:cat [:repeat r (inc fixed)] [:* r]]))
      (cond-> [:alt]
        (pos? min) (conj [:repeat r 0 (dec min)])
        max (conj [:repeat r (inc max) nil])))))

(defn negate
  ([r] (negate r {}))
  ([r {:keys [resolver] :as opts}] 
   (regal/normalize
     (cond
       (qualified-keyword? r)
       (-negate
         (if resolver
           (if-let [resolved (resolver r)]
             (-negate resolved opts)
             (throw (ex-info (str "Unable to resolve Regal Expression " r ".")
                             {::unresolved r})))
           (throw (ex-info (str "Regal expression contains qualified keyword, but no resolver was specified.")
                           {::no-resolver-for r})))
         opts)

       :else (-negate r opts)))))
