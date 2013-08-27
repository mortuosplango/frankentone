(ns frankentone.examples.sampled
  (:use [frankentone utils ugens dsp instruments patterns samples]
        [overtone.music time])
  (:require [incanter core charts stats]
            [clojure.java io]))


(def samp (load-sample (clojure.java.io/resource "hihat-open.wav")))

(incanter.core/view 
 (incanter.charts/xy-plot
  (range (count samp))
  (vec samp)))

(reset-dsp! (let [len (/ (count samp) *sample-rate*)]
              (fn [x chan]
                (if (< (mod x 0.55) len)
                  (aget ^doubles samp
                        (* *sample-rate* (* 0.5 (mod x len))))
                  (aget ^doubles samp
                        (* *sample-rate* (- len (mod x len))))))))

(start-dsp)
(stop-dsp)
