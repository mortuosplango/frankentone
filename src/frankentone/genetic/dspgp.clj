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
        previous-value (hiphip.double/aget buffer pos)]
    (hiphip.double/aset buffer pos ^double y)
    previous-value))


(defn bget
  "Get the value at second x in the buffer.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x] (hiphip.double/aget buffer (* (pmod x 1.0) (dec *sample-rate*))))


(def ^:dynamic dsp-terminals
  (concat random-terminals
           '('prev
             'TAU
             (exp-rand 20.0 20000.0)
             (exp-rand 1.0 30.0)
             '*sample-rate*)))


(def ^:dynamic dsp-functions
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


(defprotocol PEvolution
  (next-gen [this])
  (next-gen- [this])
  (start-auto [this & { :keys [delay] :or {delay 1000}}])
  (stop-auto [this]))


(deftype AudioEvolution
    [instname
     target
     e-state
     auto?]
  PEvolution
  (next-gen [this]
    (println instname "[~] calculating next generation")
    (future (next-gen- this)))
  (next-gen- [this]
    (let [instfn
          (binding [buffer (double-array *sample-rate* 0.0)]
            (let [
                  val-func (program->fn
                            (first
                             (:population
                              (swap! e-state next-generation))))
                  prev-samp (atom 0.0)]
              (fn ^Double [x]
                (swap! prev-samp #(val-func x %)))))
          len (* (count (:samples target)) sample-dur)]
      (when-let [inst (get @instruments (keyword instname))]
        (.setNoteKernel inst
                        (fn [freq amp dur]
                          (fn ^double [x]
                            (if (< x len)
                              (* amp (instfn x))
                              0.0)))))))
  (start-auto [this & { :keys [delay] :or {delay 1000}}]
    (future
      (println instname "[~] starting auto evolution")
      (reset! auto? true)
      (while @auto?
        (next-gen- this)
        (Thread/sleep delay)
        )))
  (stop-auto [this]
    (reset! auto? false)))



(defmacro defgen
  [name instname target]
  `(let [ref-map# (get-reference-map ~target)]
     (when-not (contains? @instruments (keyword ~instname))
       (eval (conj '((fn [x# y# z#] (fn [x#] 0.0)))
                   (symbol ~instname) 'definst)))
     (def ~name
       (AudioEvolution. ~instname
                        ref-map#
                        (atom (next-generation
                               300
                               (memoize
                                (partial error-fn
                                         ref-map#
                                         [:rms :mfcc]))
                               :terminals dsp-terminals
                               :functions dsp-functions))
                        (atom false)))))

