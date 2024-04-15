(ns lambdaisland.regal.generator
  (:require [clojure.test.check.generators :as gen]
            [clojure.test.check.random :as random]
            [clojure.test.check.rose-tree :as rose]
            [clojure.math.combinatorics :as comb]
            [lambdaisland.regal :as regal]
            [lambdaisland.regal.code-points :as cp]
            [lambdaisland.regal.negate :as negate]
            [lambdaisland.regal.platform :as platform]
            [clojure.string :as str]))

(declare generator)

(defmulti -generator (fn [[op] opts] op))

(defn collapse-double-line-breaks [rs]
  (let [[res prev] (reduce
                    (fn [[res prev] r]
                      (cond
                        (= prev r :line-break)
                        [(conj res :-double-line-break) nil]
                        prev
                        [(conj res prev) r]
                        :else
                        [res r]
                        )
                      )
                    [[] nil]
                    rs)]
    (if prev
      (conj res prev)
      res)))

(defn map-generator [rs opts]
  (map-indexed (fn [idx r]
                 {:post [(some? %)]}
                 (generator r (-> opts
                                  (update ::initial? #(and % (= 0 idx)))
                                  (update ::final? #(and % (= (count rs)
                                                              (inc idx))))))) rs))

(defmethod -generator :cat [[_ & rs] opts]
  (apply gen/tuple (map-generator (collapse-double-line-breaks rs) opts)))


(defmethod -generator :alt [[_ & rs] opts]
  (gen/one-of (map #(generator % opts) rs)))

(defmethod -generator :* [[_ & rs] opts]
  (gen/bind gen/pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator (into [:cat] rs) opts))))))

(defmethod -generator :*? [[_ & rs] opts]
  (-generator (into [:*] rs) opts))

(defmethod -generator :+ [[_ & rs] opts]
  (gen/bind gen/s-pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator (into [:cat] rs) opts))))))

(defmethod -generator :+? [[_ & rs] opts]
  (-generator (into [:+] rs) opts))

(defmethod -generator :? [[_ & rs] opts]
  (gen/one-of [(gen/return "")
               (generator (into [:cat] rs) opts)]))

(defmethod -generator :?? [[_ & rs] opts]
  (-generator (into [:?] rs) opts))

(defn parse-hex
  "
  \"\\xFF\" => 255
  \"\\u0A00\" => 2560
  "
  [h]
  (platform/hex->int (subs h 2)))

(def line-break-strs
  ["\r\n" "\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029"])

(def double-line-break-strs
  "[:cat :line-break :line-break] should not generate \\r\\n, because of how \\R
  works."
  (into [] (comp (map str)
                 (remove #{"\r\n"}))
        (comb/selections line-break-strs 2)))

(defn- token->regal [r opts]
  (case r
    :any
    [:not \return \newline \u0085]

    :digit
    [:class [\0 \9]]

    :non-digit
    [:not [\0 \9]]

    :word
    [:class [\a \z] [\A \Z] [\0 \9] \_]

    :non-word
    [:not [\a \z] [\A \Z] [\0 \9] \_]

    :whitespace
    (into [:class] (map char) regal/whitespace-char-codes)

    :non-whitespace
    (into [:not]
          (map #(mapv char %))
          regal/non-whitespace-ranges-codes)

    :start
    (if (::initial? opts)
      ""
      (throw (ex-info "Can't create generator, :start used in non-initial position."
                      {:type ::impossible-regex})))

    :end
    (if (::final? opts)
      ""
      (throw (ex-info "Can't create generator, :end used in non-final position."
                      {:type ::impossible-regex})))

    :newline
    "\n"

    :return
    "\r"

    :tab
    "\t"

    :form-feed
    "\f"

    :line-break
    (into [:alt] line-break-strs)

    :-double-line-break ;; internal, do not use
    (into [:alt] double-line-break-strs)

    :alert
    "\u0007"

    :escape
    "\u001B"

    :vertical-whitespace
    [:alt "\n" "\u000B" "\f" "\r" "\u0085" "\u2028" "\u2029"]

    :vertical-tab
    "\u000B"

    :null
    "\u0000"

    (throw (ex-info (str "Unrecognized regal token: " r) {::unrecognized-token r}))))

(defn token-gen [r opts]
  (-> r
      (token->regal opts)
      (generator opts)))

(defmethod -generator :class [[_ & cs] opts]
  (gen/one-of
    (vec (for [c cs]
           (cond
             (vector? c)
             (gen/fmap char (gen/choose (platform/char->long (first c)) (platform/char->long (second c))))

             (simple-keyword? c)
             (token-gen c opts)

             ;; Not sure if this should be allowed, can custom tokens be
             ;; used inside a class?
             ;;
             ;; (qualified-keyword? c)
             ;; (generator c opts)

             (string? c)
             (gen/one-of (mapv gen/return (cp/code-point-seq c)))

             (char? c)
             (gen/return c))))))

(defn -not-code-points [r opts]
  {:post [(do (prn %) true)]}
  (let [never-chars (reduce (fn [acc c]
                              (cond
                                (vector? c) (into acc (let [[min max] (map platform/char->long c)]
                                                        (range min (inc max))))
                                (string? c) (into acc (map platform/char->long)
                                                  (cp/code-point-seq c))
                                (char? c) (conj acc (platform/char->long c))
                                :else (throw (ex-info (str "Unknown :not class" r ".")
                                                      {::unknown r}))))
                            #{} (next r))]
    (remove never-chars (range (::min-code-point opts 0)
                               (inc (::max-code-point opts 256))))))

(defmethod -generator :not [r opts]
  (gen/one-of (mapv (comp gen/return cp/code-point->string)
                    (-not-code-points r opts))))

(defmethod -generator :repeat [[_ r min max] opts]
  (if max
    (gen/bind (gen/choose min max)
              (fn [i]
                (apply gen/tuple (repeat i (generator r opts)))))
    (apply gen/tuple (repeat min (generator r opts)))))

(defmethod -generator :lazy-repeat [[_ & rs] opts]
  (-generator (into [:repeat] rs) opts))

(defmethod -generator :capture [[_ & rs] opts]
  (generator (into [:cat] rs) opts))

(defmethod -generator :ctrl [[_ ch] opts]
  (gen/return (str (char (- (platform/char->long
                             (if (char? ch)
                               ch
                               (first ch)))
                            64)))))


(defn- generator [r {:keys [resolver] :as opts}]
  (cond
    (string? r)
    (gen/return r)

    (char? r)
    (gen/return r)

    (qualified-keyword? r)
    (if resolver
      (if-let [resolved (resolver r)]
        (recur resolved opts)
        (throw (ex-info (str "Unable to resolve Regal Expression " r ".")
                        {::unresolved r})))
      (throw (ex-info (str "Regal expression contains qualified keyword, but no resolver was specified.")
                      {::no-resolver-for r})))

    (simple-keyword? r)
    (token-gen r opts)

    :else
    (-generator r opts)))

(defn- grouped->str
  #?@(:clj
      [([g]
        (let [sb (StringBuilder.)]
          (grouped->str g sb)
          (str sb)))
       ([g ^StringBuilder sb]
        (cond
          (string? g)
          (.append sb ^String g)

          (char? g)
          (.append sb ^Character g)

          (or (seq? g) (vector? g))
          (run! #(grouped->str % sb) g)

          :else
          (assert false g)))]
      :cljs
      [([g]
        (cond
          (string? g)
          g
          (or (seq? g) (vector? g))
          (apply str (map grouped->str g))
          :else
          (assert false g)))]))

(defn gen
  ([r]
   (gen r nil))
  ([r {:keys [resolver] :as opts}]
   (gen/fmap grouped->str (generator r (assoc opts
                                              ::initial? true
                                              ::final? true)))))

(defn- -random [seed] (if seed (random/make-random seed) (random/make-random)))

(defn sample
  ([r]
   (sample (gen r) {}))
  ([r num-samples-or-opts]
   (if (number? num-samples-or-opts)
     (gen/sample gen num-samples-or-opts)
     (let [{:keys [seed size] :or {size 10}} num-samples-or-opts
           gen (gen r num-samples-or-opts)]
       (->> (gen/make-size-range-seq size)
            (map #(rose/root (gen/call-gen gen %1 %2))
                 (gen/lazy-random-states (-random seed)))
            (take size))))))

(defn generate
  ([r]
   (gen/generate (gen r)))
  ([r size]
   (gen/generate (gen r) size))
  ([r size seed]
   (gen/generate (gen r) size seed)))

(comment
  (sample [:cat :digit :whitespace :word])

  (sample [:class "aeiou" :whitespace])

  (sample
   [:cat
    :start
    [:alt "http" "https" "ftp"]
    "://"
    [:+ [:+ :word] "."]
    [:+ :word]
    [:? [:+ "/" [:+ [:not "/?#"]]]]
    [:? "?" [:+ [:+ :word] "=" [:+ :word]]
     [:? [:+  "&" [:+ :word] "=" [:+ :word]]]]])

  (require '[lambdaisland.regal :as regal])
  (sample [:cat
           :start
           [:+ :word]
           "="
           [:+ :digit]
           :end])

  (let [pattern [:cat
                 :start
                 [:class [\a \z] [\A \Z] [\0 \9] \_ \-]

                 [:capture
                  [:repeat [:class [\0 \9]] 3 5]
                  [:* [:not \.]]
                  "."
                  [:alt "com" "org" "net"]]
                 :end]]
    (map #(re-find (regal/regex pattern) %) (sample pattern))))
