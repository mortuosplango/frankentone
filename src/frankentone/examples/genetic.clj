(ns frankentone.examples.genetic
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils dspgp]
        [frankentone utils ugens dsp instruments patterns]
        [overtone.music time])
   (:require [incanter core charts stats]))


;; (def plot (atom (incanter.charts/xy-plot (range (count @target-data)) (mapv second @target-data))))

;; (incanter.core/view @plot)
;; (compute-target 0.3 0.5)

(do
  (set! *warn-on-reflection* true)
  (def target-data
    (atom (let [len (* 1 1024)]
            (mapv #(vector (/ % *sample-rate*)
                           (Math/tanh
                            (* 2.05 (Math/sin (* (/ % *sample-rate*) 220.0 TAU)))))
                  (range len)))))
  (def reference (atom (get-reference-map @target-data)))
  (defn compute-target [x y]
  (reset! reference
          (get-reference-map
           (reset! target-data
                   (let [len (* 1 1024)]
                     (mapv #(vector (/ % *sample-rate*)
                                    (+
                                     ;; noise
                                     (* (- 1.0 x) y (white-noise))
                                     ;; sägezahn
                                     (* x y
                                        (dec (* 2.0 (mod (/ % *sample-rate*)
                                                         (/ 1.0 220.0)) 220.0)) )
                                     ;; sinus
                                     (* (- 1.0 y)
                                        (- 1.0 x)
                                        (Math/sin
                                         (* (/ % *sample-rate*)
                                            220.0 TAU)))
                                     ;; square
                                     (* (- 1.0 y) x
                                        (Math/tanh
                                         (* (inc (* x 30.0)) (Math/sin
                                                              (* (/ % *sample-rate*)
                                                                 220.0 TAU)))))))
                           (range len)))))))
  (def best-pg (atom '(pmod (sin (* TAU (sin (* TAU x)))) (sin (* x Math/PI)))))
  (def best-pg-error (atom 1000.0))
  (def best-pg-samples (atom (let [
                                   value-function (program->fn @best-pg)]
                               (mapv (let [prev-samp (atom 0.0)]
                                       (fn ^Double [[^Double x ^Double y]]
                                         (hardclip
                                          (swap!
                                           prev-samp
                                           #(filter-bad-value (value-function x %))
                                           ))))
                                     @target-data))))
  (def best-pg-changed (atom true)))



;; (compute-target 0.5 0.5)

;; (do
;;   (def target-data
;;     (let [len (* 1 1024)]
;;        (mapv #(vector (/ % *sample-rate*)
;;                       (Math/sin (* (/ % len) TAU)))
;;                   (range len))))
;;   (def reference (atom (get-reference-map target-data)))
;;   (error-fn '(mul-sin TAU (* x (/ *sample-rate* 1024.0)))))


(defn error-fn 
  [individual]
  (let [ref-data @reference
        value-function (program->fn individual)
        has-nan (atom false)
        samples (mapv (let [prev-samp (atom 0.0)
                            buffer (double-array *sample-rate* 0.0)]
                        (fn ^Double [[x y]]
                          (when (not @has-nan)
                            (hardclip
                             (swap!
                              prev-samp
                              #(let [new-samp (value-function x %)]
                                 (if
                                     (has-bad-value? new-samp)
                                   (do (reset! has-nan true)
                                       0.0)
                                   new-samp)
                                 ))))))
                      @target-data)
        result (if (and
                    (not @has-nan)
                    (not=
                     ;; penalize silence
                     (apply max samples)
                     (apply min samples)))
                 (let [candidate-fft 
                       (get-fft-mags samples 1024)
                       ;; compare RMS
                       rms  (* 100.0 (Math/abs (- (rms samples)
                                                  (double (:rms ref-data)))))
                       spf ;; penalize noisyness
                       (* 100.0
                          (reduce +
                                  (map
                                   (fn [candidate-frame
                                       ref-spectral-flatness]
                                     (Math/abs ^double
                                               (- (spectral-flatness candidate-frame)
                                                  ref-spectral-flatness)))
                                   candidate-fft
                                   (:spectral-flatness ref-data))))
                       boz
                       ;; Bozkurt: Parallel evolutionary optimization of digital
                       ;; sound synthesis parameters p. 198
                       (reduce +
                               (mapv
                                (fn [candidate-frame
                                    ref-frame
                                    ref-frame-weights]
                                  (reduce +
                                          (mapv
                                           (fn ^Double [^Double x
                                                       ^Double y
                                                       ^Double z]
                                             (* (Math/pow
                                                 (- (Math/abs x)
                                                    (Math/abs y))
                                                 2.0)
                                                z))
                                           candidate-frame
                                           ref-frame
                                           ref-frame-weights))) 
                                candidate-fft
                                (:fft ref-data)
                                (:fft-weights ref-data)))
                       ]
                   ;;(prn rms spf boz)
                   (+ rms spf boz)
                   )
                 Double/NaN)]
    ;; check for NaN
    (if (has-bad-value? result) Double/MAX_VALUE result)))

(defn best-callback
  [best best-error]
  (let [value-function (program->fn best)]
    (reset! best-pg-samples
            (mapv (let [prev-samp (atom 0.0)]
                    (fn ^Double [[x y]]
                      (hardclip
                       (swap!
                        prev-samp
                        #(filter-bad-value (value-function x %))))))
                  @target-data)))
  (reset! best-pg best)
  (reset! best-pg-error best-error)
  (reset! best-pg-changed true)
  (program->dsp! best true))



(let [instfn
      (binding [buffer (double-array *sample-rate* 0.0)]
        (let [
              val-func (program->fn @best-pg)
              prev-samp (atom 0.0)]
          (fn ^Double [x]
            (swap! prev-samp #(val-func x %)))))
      sample-dur (/ 1.0 *sample-rate*)]
  (time (dotimes [i *sample-rate*]
          (instfn (* i sample-dur)))))



(defn pat []
  (play-pattern [:gp 70] 2.0 0.5 :default (+ 0.5 (nows)))
  (apply-at (+ (now) 2000) #'pat))


(defn pat [])
(pat)


(reset-dsp! (let [prev (atom 0.0)]
              (fn [x chan] (if (zero? chan) (reset! prev
                                                   (+ (gp x)
                                                      (default x)
                                                      (* 0.01 (white-noise))))
                              @prev) )))



;;(def evol (future (evolve 500 error-fn best-callback)))
;;(def evol (future (evolve 300 error-fn best-callback)))

;;(def evol (future (evolve 10000 error-fn best-callback)))

;; (future-done? evol)

;; (reset! *evolution* false)
;; (start-dsp)
;; (stop-dsp)

(defn best-callback
  [best best-error]
  (reset! best-pg best)
  (reset! best-pg-error best-error)
  (let [instfn
        (binding [buffer (double-array *sample-rate* 0.0)]
          (let [
                val-func (program->fn best)
                prev-samp (atom 0.0)]
            (fn ^Double [x]
              (swap! prev-samp #(val-func x %)))))]
    (definst
      gp
      (fn [freq amp dur]
        (fn ^double [x] (* amp (instfn x)))
        ))))

;;;; Run it with a population of 1000:
(def evol
  (future (evolve 300 error-fn
                  :best-callback best-callback
                  :terminals dsp-terminals
                  :functions dsp-functions)))
