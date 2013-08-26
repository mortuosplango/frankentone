(ns frankentone.genetic.dspgp
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils]
        [frankentone utils ugens dsp instruments patterns]))


(defn bset!
  "Set the buffer at second x to value y. Returns the former value at
  x.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x y]
  (let [pos (* (pmod x 1.0) (dec *sample-rate*))
        previous-value (aget ^doubles buffer pos)]
    (aset ^doubles buffer pos ^double y)
    previous-value))


(defn bget
  "Get the value at second x in the buffer.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x] (aget ^doubles buffer (* (pmod x 1.0) (dec *sample-rate*))))


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
  [ref-data individual]
  (let [value-function (program->fn individual)
        no-nan (atom true)
        prev-samp (atom 0.0)
        samples (binding [buffer (double-array *sample-rate* 0.0)]
                  (amap
                   ^doubles (:x ref-data)
                   idx
                   ret
                   (if @no-nan
                     (hardclip (swap! prev-samp
                                      #(let [new-samp (value-function
                                                       (aget ret idx)
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
                     (apply max samples)
                     (apply min samples)))
                 (let [candidate-fft 
                       (get-fft-mags samples 1024)
                       ;; compare RMS
                       rms  (* 100.0 (reduce +
                                             (map #(Math/abs (- %1 ^double %2))
                                                  (windowed-rms samples
                                                                1024 0.25)
                                                  (:rms ref-data))))
                       spf ;; penalize noisyness
                       (* 100.0
                          (reduce +
                                  (map
                                   (fn [candidate-frame
                                       ref-spectral-flatness]
                                     (Math/abs
                                      ^double
                                      (- (spectral-flatness
                                          candidate-frame)
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


(defn dynamic-error-fn
  [reference-atom individual]
  (error-fn @reference-atom individual))
