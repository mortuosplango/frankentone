(ns frankentone.genetic.simplegp-functions
  (:use [frankentone utils]
        [frankentone.genetic utils]))

(defn- -pd-bad-values? ^Boolean  [num denom]
  (or (zero? denom) (= (type num) java.lang.Double) (has-bad-value? denom)))

(defn pmod
  "Protected modulo; returns 0 if the denominator is zero."
    ^Double [num denom]
    (if (-pd-bad-values? num denom)
      0.0
      (mod num denom)))

(defn pd
  "Protected division; returns 0 if the denominator is zero."
  ^Double  [num denom]
  (if (-pd-bad-values? num denom)
    0.0
    (/ num denom)))

(defn pround
  "Protected rounding."
  ^Double  [num denom]
  (if (-pd-bad-values? num denom)
      0.0
      (- num (pmod num denom))))

(defn sin ^Double [x] (Math/sin x))
(defn cos ^Double [x] (Math/cos x))
(defn tanh ^Double [x] (Math/tanh x))

(defn mul-sin ^Double [x y] (Math/sin (* x y)))
(defn mul-cos ^Double [x y] (Math/cos (* x y)))
(defn mul-tanh ^Double [x y] (Math/tanh (* x y)))

(defn if>0 ^Double [x y z] (if (> x 0.0) y z))
(defn if<0 ^Double [x y z] (if (< x 0.0) y z))
(defn mean ^Double [x y] (/ (+ x y) 2.0))
