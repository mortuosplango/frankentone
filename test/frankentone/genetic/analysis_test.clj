(ns frankentone.genetic.analysis-test
  (:use clojure.test
        [frankentone.genetic analysis dspgp]
        [frankentone utils ugens])
  (:require [hiphip.double :as dbl]))


(deftest error-fn-test
  (let [reference (get-reference-map
                   (let [len (* 1024 4)]
                     (dbl/afill!
                      [[idx _] (double-array len)]
                      (Math/tanh
                       (* 2.05 (Math/sin (* (/ idx *sample-rate*) 220.0 2 Math/PI)))))))]
    (dotimes [i 17]
      (is (= (error-fn reference '(Math/tanh
                                   (* 2.05 (Math/sin (* x 220.0 2 Math/PI)))))
             0.0))
      (is (= (dynamic-error-fn (atom reference) '(Math/tanh
                                                  (* 2.05 (Math/sin (* x 220.0 2 Math/PI)))))
             0.0)))))
