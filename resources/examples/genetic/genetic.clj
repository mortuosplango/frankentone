(ns frankentone.examples.genetic.genetic
  (:use [frankentone.genetic analysis simplegp simplegp-functions utils dspgp]
        [frankentone utils ugens dsp instruments patterns samples])
  (:require [incanter core charts stats]
            [hiphip.double :as dbl]
            clojure.java.io))





(do
  (def reference
    (get-reference-map
     (let [len (* 1024 4)]
       (dbl/afill!
        [[idx _] (double-array (* 1024 4))]
        (Math/tanh
         (* 2.05 (Math/sin (* (/ idx *sample-rate*) 220.0 TAU))))))))
  (error-fn reference '(Math/tanh
                                (* 2.05 (Math/sin (* x 220.0 TAU))))))


(do
  (def reference
    (atom (get-reference-map
           (load-sample (clojure.java.io/resource "hihat-open.wav")))))
  (def best-pg
    (atom
     {:program '(pmod (sin (* TAU (sin (* TAU x)))) (sin (* x Math/PI)))
      :error 1000.0
      :changed true})))

(dotimes [i 3]
  (time
   (dotimes [i 10]
     (error-fn @reference
               [:rms]
               '(if>0 (+ (* (* *sample-rate* x prev) (+ (* 4.239626725410776 (sin (+ *sample-rate* (* *sample-rate* x prev))) prev) -0.22132726531217406) prev) (mul-sin TAU x)) (+ x x) (- (mul-cos *sample-rate* (mul-sin TAU x)) 0.023044934598853834))))
   ))


(hiphip.array/amap
 [x (double-array (range 0 20))
  :let [y (< x  7)]
  :while (= y true)]
  y)

;; nothing:
;; 500 ms

;; mfcc:
;; 730 ms

;; spf:
;; 650 ms

;; boz:
;; 630 ms

;; rms:
;; 780 ms



(defn best-callback
  [best best-error]
  (let [value-function (program->fn best)]
    (reset! best-pg
            {:program best
             :error best-error
             :changed true
             :samples 
             (mapv (let [prev-samp (atom 0.0)]
                     (fn ^Double [x]
                       (hardclip
                        (swap!
                         prev-samp
                         #(filter-bad-value (value-function x %))))))
                   (:x @reference))}))
  (program->dsp! best true))


;; (future-done? evol)
;; (reset! *evolution* false)
;; (start-dsp)
;; (stop-dsp)

;;;; Run it:
(def evol
  (future (evolve 100 (memoize
                       (partial dynamic-error-fn
                                reference))
                  :best-callback best-callback
                  :terminals dsp-terminals
                  :functions dsp-functions)))

