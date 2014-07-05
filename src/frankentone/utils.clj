(ns frankentone.utils
  (:use clojure.walk
        frankentone.libs.app-icon)
  (:require overtone.music.pitch))


(def TAU (* Math/PI 2))

(def ^:dynamic *sample-rate* 44100.0)

(def ^:dynamic *latency* 0.1)

(defn nows
  "Returns the current time in seconds"
  [] (* 0.001 (System/currentTimeMillis)))


(defn rrand
  "Random value within min and max."
  (^Double [] (rrand 1.0))
  (^Double [range] (rrand (- range) range))
  (^Double [min max]
           (+ (rand (- max min)) min)))


(defn exp-rand
  "Random value with exponential distribution within min and max."
  [min-val  max-val]
  (+ (* (Math/pow (rrand 0.00001 1.0) 2.0) (- max-val min-val)) min-val))


(defmacro next!
  "Returns the next item and resets the atom to the rest of the
  atomized collection."
  [atom]
  `(first (swap! ~atom rest)))


;;; from https://github.com/sjl/roul/blob/master/src/roul/random.clj
(defn rand-nth-weighted
  "Return a random element from the weighted collection.

  A weighted collection can be any seq of [choice, weight] elements.
  The weights can be arbitrary numbers -- they do not need to add up
  to anything specific.

  Examples:

  (rand-nth-weighted [[:a 0.50] [:b 0.20] [:c 0.30]])
  (rand-nth-weighted {:a 10 :b 200})
"
  [coll]
  (let [total (reduce + (map second coll))]
    (loop [i (rand total)
           [[choice weight] & remaining] (seq coll)]
      (if (>= weight i)
        choice
        (recur (- i weight) remaining)))))


(defn has-bad-value?
  "Returns true if in-val is a double and neither NaN or Infinity."
 ^Boolean [in-val]
 (or (not= (type in-val) java.lang.Double)
     (.isInfinite ^Double in-val)
     (.isNaN ^Double in-val)))


(defn filter-bad-value
  "Returns 0.0 if in-val is NaN or Infinity. If not, just returns in-val."
  ^Double [in-val]
  (if (has-bad-value? in-val)
    0.0
    in-val))


(defn clip-to-range [val min-val max-val]
  (max min-val (min max-val val)))


(defn lin->exp [value in-min in-max out-min out-max]
  (Math/pow (/ out-max out-min)
            (* (/ (- value in-min)
                  (- in-max in-min))
               out-min)))


(definline dbg
  "debugging parts of expressions"
  [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(defn nth-prime
  "the nth prime number."
  [n]
  (if (zero? 0)
    2
    (nth (filter
           #(.isProbablePrime (BigInteger/valueOf %) 5)
           (take-nth 2 
                     (range 1 Integer/MAX_VALUE)))
         (int (dec n)))))


(defmacro fn-c
  "EXPERIMENTAL

  Produces a function with all closure-producing functions (ending in
  -c) let'ed automagically.

  Example:

  (fn-c [x] (sin-osc-c 0.0 0.1 440))

  corresponds to:

  (let [sin-osc (sin-osc-c 0.0)] (fn [x] (sin-osc 0.1 440)))"
  [args body]
  (let [vars# (atom [])
        new-body# (postwalk (fn [expr#]
                              (if (and (coll? expr#)
                                       (seq expr#)
                                       ;; is it in the ugen namespace?
                                       ;; (doesn't work in editor so
                                       ;; disabled for now)
                                       ;; (some-> (first expr#) 
                                       ;;         resolve meta :ns
                                       ;;         (= (find-ns 'frankentone.ugens)))
                                       ;; and does it produce a
                                       ;; closure to work?
                                       (> (count (str (first expr#))) 2)
                                       (= (subs (str (first expr#))
                                                (- (count (str (first expr#))) 2))
                                          "-c"))
                                (do (let [symb# (gensym)
                                          num-args# (some-> (first expr#)
                                                            resolve
                                                            meta
                                                            :arglists
                                                            first
                                                            count
                                                            inc)]
                                      (swap! vars# conj symb#
                                             (take num-args# expr#))
                                      (conj (drop num-args# expr#) symb#)))
                                expr#))
                            body)]
    `(let ~(deref vars#) (fn ~args
                           ~new-body#))))

(defn scround
  "Dirty SuperCollider-like round fn.

  Rounds to the nearest integer or optionally to the nearest multiple
  of y."
  ([x]
     (Math/round x))
  ([x y]
     (float (* (Math/round (/ x y))
               y))))

(defn hz->mel [freq]
  (* 1127 (Math/log (+ 1.0 (/ freq 700.0)))))

(defn mel->hz [mel]
  (* 700 (- (Math/pow Math/E (/ mel 1127.0)) 1.0)))

(defn opo->hz [opo]
  (overtone.music.pitch/midi->hz (* 12 opo)))

(defn hz->opo [frequency]
  (/ (overtone.music.pitch/hz->midi frequency)
     12.0))

(defn opo->midi [opo]
  (* 12 opo))

(defn midi->opo [midi]
  (/ midi 12.0))

(defprotocol PGui
  (gui [this]))


(defn clump
  "Wrapper for partition, rounding off any floating point inputs for
  n, step and pad.

  Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n
  items."
  {:added "1.0"
   :static true}
  ([n coll]
     (partition (long n) (long n) coll))
  ([n step coll]
     (partition (long n) (long step) coll))
  ([n step pad coll]
     (partition (long n) (long step) (long pad) coll)))
