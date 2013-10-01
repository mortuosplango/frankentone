(ns frankentone.examples.genetic.genetic
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils dspgp]
        [frankentone utils ugens dsp instruments patterns samples])
  (:require [incanter core charts stats]
            clojure.java.io))


(do
  (def reference
    (atom (get-reference-map
           (load-sample (clojure.java.io/resource "hihat-open.wav")))))
  (def best-pg
    (atom
     {:program '(pmod (sin (* TAU (sin (* TAU x)))) (sin (* x Math/PI)))
      :error 1000.0
      :changed true})))


(do
  (reset! reference
          (get-reference-map
           (amap (double-array (* 1024 4)) idx ret
                 (Math/tanh
                  (* 2.05 (Math/sin (* (/ idx *sample-rate*) 220.0 TAU)))))))
  (dynamic-error-fn reference '(Math/tanh
              (* 2.05 (Math/sin (* x 220.0 TAU))))))


(time
 (dotimes [i 10]
   (error-fn @reference
             [:mfcc]
             '(if>0 (+ (* (* *sample-rate* x prev) (+ (* 4.239626725410776 (sin (+ *sample-rate* (* *sample-rate* x prev))) prev) -0.22132726531217406) prev) (mul-sin TAU x)) (+ x x) (- (mul-cos *sample-rate* (mul-sin TAU x)) 0.023044934598853834)))))


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

