(ns frankentone.examples.genetic.statistics
  (:use [frankentone.genetic
         analysis
         simplegp
         simplegp-functions
         utils
         dspgp]
        [frankentone utils ugens dsp instruments patterns samples])
  (:require [incanter core stats]
            [incanter.charts :as chrt]
            [hiphip.double :as dbl]
            clojure.java.io))

(def logfile  "/Users/hb/ft-all.log")

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


;; (future-done? evol)
;; (reset! *evolution* false)
;; (start-dsp)
;; (stop-dsp)

;;;; Run it:
(def evol
  (future (evolve 1000 (memoize
                        (partial error-fn
                                 reference))
                  :terminals dsp-terminals
                  :functions dsp-functions
                  :logfile logfile)))

(def err
  (with-open [rdr (clojure.java.io/reader logfile)]
    (mapv
     (fn [line]
       (let [datapoint (load-string line)]
         [(:time datapoint)
          (:average-program-size datapoint)
          (:median-error datapoint)
          (first (:errors datapoint))])) (rest (line-seq rdr)))))

(incanter.core/view err)

(let [xy (chrt/xy-plot nil nil
                       :x-label "Generation"
                       :y-label "Reciprocal Error")]
  (mapv
   (fn [para lbl]
     (chrt/add-lines xy
                     (range (count err))
                     (mapv #(/ 1.0 (nth % para)) (rest err))
                     :series-label lbl))
   [2 3]
   ["Median error" "Best error"])
  (incanter.core/view xy))

(let [data (vec (frequencies (repeatedly 100000 #(scround (/ (rand-nth [-2.0 2.0]) (inc (rand-int 254))) 0.01))))]
  (incanter.core/view
   (chrt/xy-plot
    (mapv first data)
    (mapv second data))))
defgen


(let [xy (chrt/xy-plot nil nil
                       :x-label "Generation"
                       :y-label "Time to calculate")]
  (mapv
   (fn [para lbl]
     (chrt/add-lines xy
                     (range (count err))
                     (mapv #(nth % para) (rest err))
                     :series-label lbl))
   [0]
   ["Time in seconds"])
  (incanter.core/view xy))

(let [xy (chrt/xy-plot nil nil
                       :x-label "Generation"
                       :y-label "Average Program Size")]
  (mapv
   (fn [para lbl]
     (chrt/add-lines xy
                     (range (count err))
                     (mapv #(nth % para) (rest err))
                     :series-label lbl))
   [1]
   ["Program size in points"])
  (incanter.core/view xy))



