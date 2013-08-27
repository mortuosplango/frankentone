(ns frankentone.utils
  (:use clojure.walk
        frankentone.libs.app-icon))


(def TAU (* Math/PI 2))


(def ^:dynamic *sample-rate* 44100.0)


(defn nows
  "Returns the current time in seconds"
  [] (* 0.001 (System/currentTimeMillis)))


(defn rrand
  "Random value within min and max."
  (^Double [] (rrand -1.0 1.0))
  (^Double [range] (rrand (* -1.0 range) range))
  (^Double [min max]
           (+ (rand (- max min)) min)))


(defn exp-rand
  "Random value with exponential distribution within min and max."
  [ min-val  max-val]
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


(def scaling-factor (dec (Math/pow 2 15)))


(defn scale-f->s
  "Scale a float value to a short value.

  Clips value at -1.0 and 1.0 before scaling."
  [f]
  (short (* (clip-to-range f -1.0 1.0) scaling-factor)))


(defn unchecked-scale-f->s 
  "Scale a float value to a short value without range checking."
  [f]
  (unchecked-short (* f scaling-factor)))


(definline dbg
  "debugging parts of expressions"
  [x]
  `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))


(defn nth-prime
  "the nth prime number."
  [n]
  (if (= n 0)
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
                                       (some-> (first expr#) 
                                               resolve meta :ns
                                               (= (find-ns 'frankentone.ugens)))
                                       ;; and does it produce a closure to work?
                                       (= (subs (str (first expr#))
                                                (- (count (str (first expr#))) 2)) "-c"))
                                (do (let [symb# (gensym (str (first expr#)))]
                                      (swap! vars# conj symb#
                                             (take (some-> (first expr#)
                                                           resolve
                                                           meta
                                                           :arglists
                                                           first
                                                           count
                                                           inc) expr#))
                                      (conj (nnext expr#) symb#)))
                                expr#))
                            body)]
    `(let ~(deref vars#) (fn ~args
                           ~new-body#))))


(defn hz->mel [freq]
  (* 1127 (Math/log (+ 1.0 (/ freq 700.0)))))

(defn mel->hz [mel]
  (* 700 (- (Math/pow Math/E (/ mel 1127.0)) 1.0)))

