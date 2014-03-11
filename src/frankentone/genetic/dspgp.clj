(ns frankentone.genetic.dspgp
  (:use [frankentone.genetic analysis simplegp simplegp-functions utils mfcc]
        [frankentone utils ugens dsp instruments patterns])
  (:require [hiphip.double :as dbl]
            [hiphip.float :as fl]
            [seesaw.core :as sc])
  (:require [incanter core stats]
            [incanter.charts :as chrt]))


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

(defn plfsaw [freq phase] 
  (+ 1.0 (* (pmod phase (pd 1.0 freq)) -2.0 freq)))


(defn flerp
  "Calculates a number between two numbers at a specific increment. The
  amt parameter is the amount to interpolate between the two values
  where 0.0 equal to the first point, 0.1 is very near the first
  point, 0.5 is half-way in between, etc. The lerp function is
  convenient for creating motion along a straight path and for drawing
  dotted lines."
  [start end amt]
  (+ (* (- end start) amt) start))


(def ^:dynamic dsp-terminals
  (concat random-terminals
           '('prev
             'TAU
             (exp-rand 20.0 20000.0)
             (exp-rand 20.0 2000.0)
             (exp-rand 1.0 30.0)
             '*sample-rate*)))


(def ^:dynamic dsp-functions
  (conj random-functions
        { :fn 'bset! :arity 2 }
        { :fn 'bget :arity 1 }))


(def ^:dynamic dsp-functions+ugens
  (list
   { :fn 'sin :arity 1 }
   { :fn 'cos :arity 1 }
   { :fn 'tanh :arity 1 }
   { :fn 'rrand :arity 1 }
   { :fn 'rrand :arity 2 }
   { :fn '+ :arity 2 }
   { :fn '+ :arity 3 }
   { :fn '- :arity 1 }
   { :fn '- :arity 2 }
   { :fn '* :arity 2 }
   { :fn '* :arity 3 }
   { :fn 'pmod :arity 2 }
   { :fn 'pd :arity 2 }
   { :fn 'pround :arity 2 }
   { :fn 'max :arity 2 }
   { :fn 'min :arity 2 }
   { :fn 'mean :arity 2 }
   { :fn 'if>0 :arity 3 }
   { :fn 'if<0 :arity 3 }
   { :fn 'frankentone.ugens/hardclip :arity 1 }
   { :fn 'frankentone.ugens/lpf-c :arity 3 }
   { :fn 'frankentone.ugens/bpf-c :arity 3 }
   { :fn 'frankentone.ugens/apf-c :arity 3 }
   { :fn 'frankentone.ugens/hpf-c :arity 3 }
   { :fn 'frankentone.ugens/pink-c :arity 0 }
   { :fn 'frankentone.ugens/line-c :arity 3 }
   { :fn 'frankentone.ugens/asr-c :arity 4 }
   { :fn 'frankentone.ugens/delay-c :arity 4}
   { :fn 'frankentone.ugens/impulse-c :arity 3 }
   { :fn 'frankentone.ugens/pulsedpw-c :arity 3 }
   { :fn 'frankentone.ugens/sine :arity 2 }
   { :fn 'flerp :arity 3 }
   { :fn 'plfsaw :arity 2 }))


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
     auto?
     selfmod-cb-fn
     pg->inst-fn]
  PGui
  (gui [this]
    (let [tabs (sc/tabbed-panel)
          update-fn (fn [key ref old-state new-state]
                      (.addTab tabs
                               (str (:generation @e-state))
                               (sc/scrollable
                                (sc/table :model [:columns [:individual
                                                            {:key :error :text "error" :class java.lang.Double}]
                                                  :rows (mapv
                                                         (fn [i e] {:individual (print-str i) :error e})
                                                         (:population new-state)
                                                         (:errors new-state))
                                                  ])))
                      )
          gui-frame (sc/frame :title instname
                              :content (sc/scrollable tabs))]
      (add-watch e-state
                 :gui
                 update-fn)
      (update-fn nil nil nil @e-state)
      (sc/listen gui-frame :window-closed
              (fn [_] (remove-watch e-state :gui)))
      (-> gui-frame
          sc/pack! sc/show!)))
  PEvolution
  (next-gen [this]
    (println instname "[~] calculating next generation")
    (future (next-gen- this)))
  (next-gen- [this]
    (let [best (first (:population
                             (swap! e-state next-generation)))
          len (* (count (:samples target)) sample-dur)]
      (selfmod-cb-fn best)
      (when-let [inst (get @instruments (keyword instname))]
        (setNoteKernel inst
                       (pg->inst-fn best len)))
      (println instname "[~] generation " (:generation @e-state) " done")
      (println instname "[~] best programme: " best)
      (println instname "[~] best error: " (first (:error @e-state)))))
  (start-auto [this & { :keys [delay] :or {delay 1000}}]
    (println instname "[~] starting auto evolution")
    (future
      (reset! auto? true)
      (while @auto?
        (next-gen- this)
        (Thread/sleep delay)
        )))
  (stop-auto [this]
    (reset! auto? false)))


(defmacro defgen
  "Convenience macro to define and control a genetic programming
  process.

  If the seed argument is nil, no seed is used."
  [name instname target seed]
  `(let [ref-map# (get-reference-map ~target)
         selfmod-cb-fn# (partial
                         (frankentone.entropy.selfmod/make-selfmod
                          false
                          :body-pos 4)
                         ~(str "defgen " name " ") 0)
         first-gen# (next-generation
                     300
                     (memoize
                      (partial error-fn
                               ref-map#
                               [:rms :mfcc]))
                     :seed '~seed
                     :terminals dsp-terminals
                     :functions dsp-functions)
         pg->inst-fn# (fn [best# len#]
                          (fn [freq# amp# dur#]
                            (binding [buffer (double-array *sample-rate* 0.0)]
                             (let [prev-samp# (atom 0.0)
                                   val-func# (program->fn best#)
                                   mul-x# (/ freq# 440.0)]
                               (fn ^double [x#]
                                 (if (< (* mul-x# x#) len#)
                                   (* amp# (swap! prev-samp# #(val-func# (* mul-x# x#) %)))
                                   0.0))))))]
     (selfmod-cb-fn# (first (:population first-gen#)))
     (when-not (contains? @instruments (keyword ~instname))
       (eval (conj '((fn [x# y# z#] (fn [x#] 0.0)))
                   (symbol ~instname) 'definst)))
     (when-let [inst# (get @instruments (keyword ~instname))]
       (setNoteKernel inst#
                      (pg->inst-fn# (first (:population first-gen#)) (* (count (:samples ref-map#)) sample-dur))))
     (def ~name
       (AudioEvolution. ~instname
                        ref-map#
                        (atom first-gen#)
                        (atom false)
                        selfmod-cb-fn#
                        pg->inst-fn#))))


(defmacro defgen+ugens
  "Convenience macro to define and control a genetic programming
  process.

  If the seed argument is nil, no seed is used."
  [name instname target seed]
  `(let [ref-map# (get-reference-map ~target)
         selfmod-cb-fn# (partial
                         (frankentone.entropy.selfmod/make-selfmod
                          false
                          :body-pos 4)
                         ~(str "defgen+ugens " name " ") 0)
         first-gen# (next-generation
                     300
                     (memoize
                      (partial error-fn
                               ref-map#
                               [:rms :mfcc]
                               program->fn-c))
                     :seed '~seed
                     :terminals dsp-terminals
                     :functions dsp-functions+ugens
                     )
         pg->inst-fn# (fn [best# len#]
                          (fn [freq# amp# dur#]
                            (let [prev-samp# (atom 0.0)
                                  val-func# (program->fn-c best#)
                                  mul-x# (/ freq# 440.0)]
                              (fn ^double [x#]
                                (if (< (* mul-x# x#) len#)
                                  (* amp# (swap! prev-samp# #(val-func# (* mul-x# x#) %)))
                                  0.0)))))]
     (selfmod-cb-fn# (first (:population first-gen#)))
     (when-not (contains? @instruments (keyword ~instname))
       (eval (conj '((fn [x# y# z#] (fn [x#] 0.0)))
                   (symbol ~instname) 'definst)))
     (when-let [inst# (get @instruments (keyword ~instname))]
       (setNoteKernel inst#
                      (pg->inst-fn# (first (:population first-gen#)) (* (count (:samples ref-map#)) sample-dur))))
     (def ~name
       (AudioEvolution. ~instname
                        ref-map#
                        (atom first-gen#)
                        (atom false)
                        selfmod-cb-fn#
                        pg->inst-fn#))))
