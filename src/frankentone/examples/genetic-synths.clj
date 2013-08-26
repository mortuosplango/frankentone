(ns frankentone.examples.genetic-drums
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils dspgp]
        [frankentone utils ugens dsp instruments patterns samples]
        [overtone.music time])
  (:require [incanter core charts stats]))

;; (start-dsp)
;; (stop-dsp)

(defn compute-target [x y]
  (let [len (dec (* 1024 1)) ;; decrement to avoid 2 fft frames
        pink (pink-c)]
    (amap (double-array len) idx ret
          (+
           ;; noise
           (* (- 1.0 x) y (pink))
           ;; saw
           (* x y
              (dec (* 2.0 (mod (/ idx *sample-rate*)
                               (/ 1.0 220.0)) 220.0)) )
           ;; sinus
           (* (- 1.0 y)
              (- 1.0 x)
              (Math/sin
               (* (/ idx *sample-rate*)
                  220.0 TAU)))
           ;; square
           (* (- 1.0 y) x
              (Math/tanh
               (* (inc (* x 30.0)) )))))))

(defn pat [t]
  (play-pattern [
                 s3 - - ||
                 - s2 ||
                 - - s3 ||
                 - s3  ||
                 ] 2.0 0.5 :default (/ t 1000.0))
  (let [next (+ t 2000)]
    (apply-at next #'pat [next])))

(pat (+ (now) 500))

(defn pat [_])

(stop-dsp)
(start-dsp)

(reset-dsp! (let [prev (atom 0.0)]
              (fn [x chan] (if (zero? chan)
                            (reset! prev
                                    (mul-tanh
                                     1.0
                                     (+ 
                                      (s1 x)
                                      (s2 x)
                                      (s3 x)
                                      (default x))))
                            @prev))))


(defn best-callback
  [name sample best best-error]
  (let [instfn
        (binding [buffer (double-array *sample-rate* 0.0)]
          (let [
                val-func (program->fn best)
                prev-samp (atom 0.0)]
            (fn ^Double [x]
              (swap! prev-samp #(val-func x %)))))]
    (swap! note-kernels
           assoc name
           (fn [freq amp dur]
             (fn ^double [x]
               (* amp (instfn x))))))
  (Thread/sleep 7500))


;;;; Run it 
(def evol
  (doall
   (map
    (fn [sample name]
      ;; init instruments
      (eval (conj '((fn [x y z] (fn [x] 0.0)))
                  (symbol name) 'definst))
      (future (evolve 300 (memoize
                           (partial error-fn (get-reference-map sample)))
                      :best-callback (partial best-callback
                                              (keyword name) sample)
                      :terminals dsp-terminals
                      :functions dsp-functions))
      (Thread/sleep 2500))
    [ (compute-target 0.8 0.8)
      (compute-target 0.2 0.8)
      (compute-target 0.2 0.2)]
    ['s1 's2 's3])))


(comment
  ;; stop all evolution processes
  (reset! *evolution* false)
  )
