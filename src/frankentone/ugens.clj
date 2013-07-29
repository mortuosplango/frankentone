;; The ugens here are largely ports of extempore dsp code found here:
;; https://github.com/digego/extempore/blob/master/libs/core/audio_dsp.xtm

(ns frankentone.ugens
  (:use frankentone.utils))


(defmacro lerp
  "Calculates a number between two numbers at a specific increment. The
  amt parameter is the amount to interpolate between the two values
  where 0.0 equal to the first point, 0.1 is very near the first
  point, 0.5 is half-way in between, etc. The lerp function is
  convenient for creating motion along a straight path and for drawing
  dotted lines."
  [start end amt]
  `(+ (* (- ~end ~start) ~amt) ~start))


(defn line-c
  ([start end length]
     (let [time (atom 0.0)
           start (double start)
           end (double end)
           step (/ (/ 1.0 length) *sample-rate*)]
       (fn ^double []
         (let [current-time (swap! time + step)]
           (if (< current-time 1.0)
             (lerp start end current-time)
             end))))))


(defn asr-c
  ( [attack-time sustain-time sustain-level release-time]
      (let [
            a-line (line-c 0.0 sustain-level attack-time)
            r-line (line-c sustain-level 0.0 release-time)
            attack-time (long (* attack-time *sample-rate*))
            release-time (long (* release-time *sample-rate*))
            attack+sustain-time (long (+ attack-time
                                         (* sustain-time *sample-rate*)))
            time (atom (double 0.0))]
        (fn ^double []
          (let [current-time (swap! time inc)]
            (if (< current-time attack-time)
              (a-line)
              (if (< current-time attack+sustain-time)
                sustain-level
                (r-line))))))))


(defn sine
  "Sine"
  (^double [freq phase]
           (Math/sin (* freq TAU phase))))


(defn hardclip
  "Hard clip"
  (^double [input]
           (max -1.0 (min 1.0 input)))
  (^double [input min-val max-val]
           (max min-val (min max-val input))))


(defn osc-c
  "a sine oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (let [phase (atom (double in-phase))
        freq-mul (double (/ TAU *sample-rate*))]
    (fn [amp freq]
      (* amp
         (Math/sin
          (swap! phase
                 #(let [new-phase
                        (+ % (* freq freq-mul))]
                    (if (> new-phase Math/PI)
                      (- new-phase TAU)
                      new-phase))))))))

(defn osc-fb-c
  "a sine oscillator that has phase modulation feedback

  Returns a function with the following arguments: [amp freq feedback]"
  [in-phase]
  (let [phase (atom (double in-phase))
        prev (atom 0.0)
        freq-mul (double (/ TAU *sample-rate*))]
    (fn [amp freq feedback]
      (* amp
         (swap! prev
                (fn [prev]
                  (Math/sin
                   (swap! phase
                          #(let [new-phase
                                 (+
                                  (* feedback prev)
                                  % (* freq freq-mul))]
                             (if (> new-phase Math/PI)
                               (- new-phase TAU)
                               new-phase))))))))))

(defn sin-osc-c
  "a sine oscillator using a 8192 point wavetable

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (let [phase (atom (double in-phase))
        table-size (double 8192)
        cycle (double (/ table-size *sample-rate*))
        sine-table (double-array (vec (map
                                       #(Math/sin (* TAU (/ % table-size)))
                                       (range table-size))))]
    (fn [amp freq]
      (* amp
         (aget sine-table
               (unchecked-short
                (swap! phase
                       #(let [new-phase
                              (unchecked-add % (* cycle freq))]
                          (if (> new-phase table-size)
                            (unchecked-subtract new-phase table-size)
                            new-phase)))))))))


(defn square-c
  "a square oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (let [osc (sin-osc-c in-phase)
        n 50.0]
    (fn ^double [amp freq]
      (* amp (Math/tanh (osc n freq))))))


(defn saw-c
  "saw oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (let [phase (atom in-phase)
        dp (atom 1.0)
        leak 0.995
        saw (atom 0.0)
        ]
    (fn [amp freq]
      (let [qmax (* 0.5 (/ *sample-rate* freq))
            dc (/ -0.498 qmax)]
        (swap! phase #(+ % @dp))
        (if (< @phase 0.0)
          (do (swap! phase #(- 0.0 %))
              (swap! dp #(- 0.0 %)))
          (when (> @phase qmax)
            (swap! phase #(+ qmax (- qmax %)))
            (swap! dp #(- 0.0 %))))
        (let [x (max 0.000001 (* Math/PI @phase))]
          (* amp (swap! saw #(* leak
                                (+ %
                                   dc
                                   (/ (Math/sin x) x))))))))))


(defn white-noise
  []
  (- (rand 2.0) 1.0))


(defn delay-c
  "IIR comb without interpolation.

  More efficient than comb if you don't need variable length

  Returns a function with the following arguments: [input wet feedback]"
  [max_delay]
  (let [
        delay (int (Math/ceil (* max_delay *sample-rate*)))
        line (double-array delay 0.0)
        time (atom (long -1))]
    (fn [input wet feedback]
      (let [delayed (aget line (swap! time #(mod (inc %) delay)))]
        (aset-double line @time (* feedback (+ input delayed)))
        (+ input (* delayed wet))))))


(defn general-biquad-c [fn-coef]
  (let [
        y1 (atom 0.0)
        y2 (atom 0.0)
        x1 (atom 0.0)
        x2 (atom 0.0)
        b0 (atom 0.0)
        b1 (atom 0.0)
        b2 (atom 0.0)
        re-a0 (atom 0.0) ;; reciprocal of a0
        a1 (atom 0.0)
        a2 (atom 0.0)
        oldres (atom 0.0)
        oldfreq (atom 0.0)
        omega-factor (double (/ TAU *sample-rate*))]
    (fn [in-x freq res]
      ;; if frequency changes
      ;; recalculate coefficients
      (let [x (double in-x)]
        (when (or (not= freq @oldfreq)
                  (not= res @oldres))
          (reset! oldfreq freq)
          (reset! oldres res)
          (let [omega (* freq omega-factor)
                sino (Math/sin omega)
                coso (Math/cos omega)
                alpha (/ sino (* 2.0 res))]
            (fn-coef sino coso alpha
                     b0 b1 b2
                     re-a0 a1 a2)))
        (let [re-a0 @re-a0
              y (- (+ (* @b0 re-a0 x)
                      (* @b1 re-a0 @x1)
                      (* @b2 re-a0 @x2))
                   (* @a1 re-a0 @y1)
                   (* @a2 re-a0 @y2))]
          (reset! y2 @y1)
          (reset! y1 y)
          (reset! x2 @x1)
          (reset! x1 x)
          y)))))


(def lpf-c
  "Lowpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
  (partial general-biquad-c
             (fn [sino coso alpha
                 b0 b1 b2
                 re-a0 a1 a2]
               (reset! b0 (/ (- 1.0 coso) 2.0))
               (reset! b1 (- 1.0 coso))
               (reset! b2 @b0)
               (reset! re-a0 (/ 1.0 (+ 1.0 alpha)))
               (reset! a1 (* -2.0 coso))
               (reset! a2 (- 1.0 alpha)))))


(def hpf-c
  "Highpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
  (partial general-biquad-c
           (fn [sino coso alpha
                b0 b1 b2
                re-a0 a1 a2]
             (reset! b0 (/ (+ 1.0 coso) 2.0))
             (reset! b1 (* -1.0 (+ 1.0 coso)))
             (reset! b2 @b0)
             (reset! re-a0 (/ 1.0 (+ 1.0 alpha)))
             (reset! a1 (* -2.0 coso))
             (reset! a2 (- 1.0 alpha)))))


(def bpf-c
  "Bandpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
  (partial general-biquad-c
           (fn [sino coso alpha
                b0 b1 b2
                re-a0 a1 a2]
             (reset! b0 alpha)
             (reset! b1 0.0)
             (reset! b2 (* -1.0 @b0))
             (reset! re-a0 (/ 1.0 (+ 1.0 alpha)))
             (reset! a1 (* -2.0 coso))
             (reset! a2 (- 1.0 alpha)))))


(def notch-c
  "Notch filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
  (partial general-biquad-c
           (fn [sino coso alpha
                b0 b1 b2
                re-a0 a1 a2]
             (reset! b0 1.0)
             (reset! b1 (* -2.0 coso))
             (reset! b2 @b0)
             (reset! re-a0 (/ 1.0 (+ 1.0 alpha)))
             (reset! a1 @b1)
             (reset! a2 (- 1.0 alpha)))))


(def apf-c
  "Allpass filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
  (partial general-biquad-c
           (fn [sino coso alpha
                b0 b1 b2
                re-a0 a1 a2]
             (reset! b0 (- 1.0 alpha))
             (reset! b1 (* -2.0 coso))
             (reset! b2 (+ 1.0 alpha))
             (reset! re-a0 (/ 1.0 (+ 1.0 alpha)))
             (reset! a1 @b1)
             (reset! a2 (- 1.0 alpha)))))


(defn pink-c
  "Pink noise generator.

  Uses Paul Kellet's economy method
  http://www.firstpr.com.au/dsp/pink-noise/

  Returns a function with the following arguments: []"
  []
  (let [
        b0 (atom 0.0)
        b1 (atom 0.0)
        b2 (atom 0.0)] 
    (fn []
      (* 0.33 (+
               (swap! b0 #(+ (* % 0.99765) (* (white-noise) 0.0555179)))
               (swap! b1 #(+ (* % 0.96300) (* (white-noise) 0.2965164)))
               (swap! b2 #(+ (* % 0.57000) (* (white-noise) 1.0526913)))
               (* (white-noise) 0.1848))))))


(defmacro sum-fns
  "Sums up a collection of functions"
  [vector]
  `(reduce-kv (fn [val# key# item#] (+ val# (item#))) 0.0 ~vector))

