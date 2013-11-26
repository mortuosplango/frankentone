(ns frankentone.genetic.dspgp
  (:use [frankentone.genetic analysis simplegp simplegp-functions utils mfcc]
        [frankentone utils ugens dsp instruments patterns])
  (:require [hiphip.double :as dbl]
            [hiphip.float :as fl]))


(defn bset!
  "Set the buffer at second x to value y. Returns the former value at
  x.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x y]
  (let [pos (* (pmod x 1.0) (dec *sample-rate*))
        previous-value (dbl/aget buffer pos)]
    (dbl/aset buffer pos ^double y)
    previous-value))


(defn bget
  "Get the value at second x in the buffer.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x] (dbl/aget buffer (* (pmod x 1.0) (dec *sample-rate*))))


(def dsp-terminals
  (concat random-terminals
           '('prev
             'TAU
             (exp-rand 20.0 20000.0)
             (exp-rand 1.0 30.0)
             '*sample-rate*)))


(def dsp-functions
  (conj random-functions
        { :fn 'bset! :arity 2 }
        { :fn 'bget :arity 1 }))


(defn error-fn
  ([ref-data individual]
     (error-fn ref-data [:rms :spf :mfcc :boz] individual))
  ([ref-data features individual]
     (error-fn ref-data features program->fn individual))
  ([ref-data features program->fn individual]
       (let [value-function (program->fn individual)
             no-nan (atom true)
             prev-samp (atom 0.0)
             samples (binding [buffer (double-array *sample-rate* 0.0)]
                       (dbl/amap
                        [ret (:x ref-data)]
                        (if @no-nan
                         (hardclip
                          (swap! prev-samp
                                 #(let [new-samp (value-function
                                                  ret
                                                  %)]
                                    (if-not (has-bad-value? new-samp)
                                      (double new-samp)
                                      (do (reset! no-nan false)
                                          0.0)))))
                         0.0)))
             result (if (and
                         @no-nan
                         (not=
                          ;; penalize silence
                          (dbl/amax samples)
                          (dbl/amin samples)))
                      (let [candidate-fft
                            (when (some #{:mfcc :boz :spf} features)
                              (get-fft-mags samples 1024))
                            ;; compare RMS
                            rms (if (some #{:rms} features)
                                  (* 100.0 (Math/sqrt
                                            (reduce +
                                                    (mapv #(Math/pow
                                                            (- %1 ^double %2)
                                                            2.0)
                                                          (windowed-rms samples
                                                                        1024 0.25)
                                                          (:rms ref-data)))))
                                  0.0)
                            
                            spf ;; penalize noisyness
                            (if (some #{:spf} features)
                              (* 100.0
                                 (Math/sqrt
                                  (reduce +
                                          (mapv
                                           (fn [candidate-frame
                                               ref-spectral-flatness]
                                             (Math/pow
                                              (- (spectral-flatness
                                                  candidate-frame)
                                                 ref-spectral-flatness)
                                              2.0))
                                           candidate-fft
                                           (:spectral-flatness ref-data)))))
                              0.0)

                            boz
                            ;; Bozkurt: Parallel evolutionary optimization of digital
                            ;; sound synthesis parameters p. 198
                            (if (some #{:boz} features)
                              (reduce +
                                      (mapv
                                       (fn [candidate-frame
                                           ref-frame
                                           ref-frame-weights]
                                         (fl/asum
                                          [x candidate-frame
                                           y ref-frame
                                           z ref-frame-weights]
                                          (* (Math/pow
                                              (- (Math/abs x)
                                                 (Math/abs y))
                                              2.0)
                                             z))) 
                                       candidate-fft
                                       (:fft ref-data)
                                       (:fft-weights ref-data)))
                              0.0)
                            mfcc
                            (if (some #{:mfcc} features)
                              (* 1000.0
                                 (reduce +
                                       (mapv
                                        (fn [^doubles candidate-frame
                                            ^doubles ref-frame]
                                          (Math/sqrt
                                           (dbl/asum
                                            [candidate candidate-frame
                                             ref ref-frame]
                                            (Math/pow
                                             (- candidate
                                                ref)
                                             2.0)))) 
                                        (mfcc candidate-fft
                                              (:mfcc-coefs ref-data))
                                        (:mfcc ref-data))))
                              0.0)

                            ]
                        ;;(prn rms spf boz)
                        (+ rms spf boz mfcc)
                        )
                      Double/NaN)]
         ;; check for NaN
         (if (has-bad-value? result) Double/MAX_VALUE result))))


(defn dynamic-error-fn
  ([reference-atom individual]
     (error-fn @reference-atom individual))
  ([reference-atom features individual]
     (error-fn @reference-atom features individual)))
