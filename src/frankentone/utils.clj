(ns frankentone.utils)

(def TAU (* Math/PI 2))

(def ^:dynamic *sample-rate* 44100.0)

(defn nows
  "Returns the current time in seconds"
  [] (* 0.001 (System/currentTimeMillis)))


(defn rrand
  "Random value within min and max."
  (^Double [] (rrand -1.0 1.0))
  (^Double [^Double range] (rrand (* -1.0 range) range))
  (^Double [^Double min ^Double max]
           (+ (rand (- max min)) min)))

(defn exp-rand
  "Random value with exponential distribution within min and max."
  [^Double min-val ^Double max-val]
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
  "Returns true if in-val is NaN or Infinity."
 ^Boolean [in-val]
 (or (not= (type in-val) java.lang.Double)
     (.isInfinite in-val)
     (.isNaN in-val)))

(defn filter-bad-value
  "Returns 0.0 if in-val is NaN or Infinity. If not, just returns in-val."
  ^Double [in-val]
  (if (has-bad-value? in-val)
    0.0
    in-val))

(defn clip-to-range [val min-val max-val]
  (max min-val (min max-val val)))

(def scaling-factor (dec (Math/pow 2 15)))

(defn scale-f->s [f]
  "Scale a float value to a short value.

  Clips value at -1.0 and 1.0 before scaling."
  (short (* (clip-to-range f -1.0 1.0) scaling-factor)))

(defn unchecked-scale-f->s [f]
  "Scale a float value to a short value without range checking."
  (unchecked-short (* f scaling-factor)))
