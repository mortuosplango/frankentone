;; The ugens here are largely ports of extempore dsp code found here:
;; https://github.com/digego/extempore/blob/master/libs/core/audio_dsp.xtm

(ns frankentone.ugens
  (:use frankentone.utils))


(defn lerp
  "Calculates a number between two numbers at a specific increment. The
  amt parameter is the amount to interpolate between the two values
  where 0.0 equal to the first point, 0.1 is very near the first
  point, 0.5 is half-way in between, etc. The lerp function is
  convenient for creating motion along a straight path and for drawing
  dotted lines."
  [start end amt]
  (+ (* (- end start) amt) start))


(defn line_c [start end length]
  (let [time (atom 0)]
    (fn []
      (let [current-time (swap! time + (/ 1.0 *sample-rate*))]
        (if (< current-time length)
          (lerp start end (/ current-time length))
          end)))))


(defn asr_c [attack-time sustain-time sustain-level release-time]
  (let [
        a-line (line_c 0.0 sustain-level attack-time)
        r-line (line_c sustain-level 0.0 release-time)
        time (atom 0.0)]
    (fn []
      (let [current-time (swap! time + (/ 1.0 *sample-rate*))]
        (if (< current-time attack-time)
          (a-line)
          (if (< current-time (+ attack-time sustain-time))
            sustain-level
            (r-line)))))))


(defn sine
  "Sine"
  (^double [^double freq ^double phase]
           (Math/sin (* freq TAU phase))))


(defn hardclip
  "Hard clip"
  (^double [^double input]
           (max -1.0 (min 1.0 input)))
  (^double [^double input ^double min-val ^double max-val]
           (max min-val (min max-val input))))


(defn osc_c
  [^double in-phase]
  (let [phase (atom (double in-phase))]
    (fn ^double [^double amp ^double freq]
      (* amp
         (Math/sin
          (swap! phase
                 #(let [new-phase
                        (+ % (* TAU (/ freq *sample-rate*)))]
                    (if (> new-phase Math/PI)
                      (- new-phase TAU)
                      new-phase))))))))


(defn sin-osc_c
  [in-phase]
  (let [phase (atom (double in-phase))
        table-size (double 8192)
        cycle (double (/ table-size *sample-rate*))
        sine-table (double-array (vec (map
                                       #(Math/sin (* TAU (/ % table-size)))
                                       (range table-size))))]
    (fn ^double [^double amp ^double freq]
      (* amp
         (aget sine-table
               (unchecked-short
                (swap! phase
                       #(let [new-phase
                              (unchecked-add % (* cycle freq))]
                          (if (> new-phase table-size)
                            (unchecked-subtract new-phase table-size)
                            new-phase)))))))))


(defn square_c
  "square oscillator"
  [in-phase]
  (let [osc (sin-osc_c in-phase)
        n 50.0]
    (fn ^double [^double amp ^double freq]
      (* amp (Math/tanh (* n (osc 1.0 freq)))))))


(defn saw_c
  "saw oscillator"
  [in-phase]
  (let [phase (atom in-phase)
        dp (atom 1.0)
        leak 0.995
        saw (atom 0.0)]
    (fn ^double [^double amp ^double freq]
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


(defn delay_c
  "IIR comb without interpolation.

  More efficient than comb if you don't need variable length"
  [max_delay]
  (let [
        delay (int max_delay)
        line (double-array delay 0.0)
        time (atom 0)
        n (atom 0)
        y (atom 0.0)]
    (fn [^Double x wet feedback] 
       (reset! y (aget line (reset! n (mod @time delay))))
       (swap! time inc)
       (aset-double line @n (* feedback (+ x @y)))
       (+ x (* @y wet)))))


(defn general-biquad_c [fn-coef]
  (let [ y1 (atom 0.0)
        y2 (atom 0.0)
        x1 (atom 0.0)
        x2 (atom 0.0)
        b0 (atom 0.0)
        b1 (atom 0.0)
        b2 (atom 0.0)
        a0 (atom 0.0)
        a1 (atom 0.0)
        a2 (atom 0.0)
        oldres (atom 0.0)
        oldfreq (atom 0.0)]
    (fn [x freq res]
      ;; if frequency changes
      ;; recalculate coefficients
      (when (or (not= freq @oldfreq)
                (not= res @oldres))
        (reset! oldfreq freq)
        (reset! oldres res)
        (let [omega (* TAU (/ freq *sample-rate*))
              sino (Math/sin omega)
              coso (Math/cos omega)
              alpha (/ sino (* 2.0 res))]
         (fn-coef sino coso alpha
                  b0 b1 b2
                  a0 a1 a2)))
      (let [y (- (+ (* (/ @b0 @a0) x)
                    (* (/ @b1 @a0) @x1)
                    (* (/ @b2 @a0) @x2))
                 (* (/ @a1 @a0) @y1)
                 (* (/ @a2 @a0) @y2))]
        (reset! y2 @y1)
        (reset! y1 y)
        (reset! x2 @x1)
        (reset! x1 x)
        y))))


(def lpf_c
  "Lowpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt"
  (partial general-biquad_c
           (fn [sino coso alpha
                b0 b1 b2
                a0 a1 a2]
             (reset! b0 (/ (- 1.0 coso) 2.0))
             (reset! b1 (- 1.0 coso))
             (reset! b2 @b0)
             (reset! a0 (+ 1.0 alpha))
             (reset! a1 (* -2.0 coso))
             (reset! a2 (- 1.0 alpha)))))


(def hpf_c
  "Highpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt"
  (partial general-biquad_c
           (fn [sino coso alpha
                b0 b1 b2
                a0 a1 a2]
             (reset! b0 (/ (+ 1.0 coso) 2.0))
             (reset! b1 (* -1.0 (+ 1.0 coso)))
             (reset! b2 @b0)
             (reset! a0 (+ 1.0 alpha))
             (reset! a1 (* -2.0 coso))
             (reset! a2 (- 1.0 alpha)))))


(def bpf_c
  "Bandpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt"
  (partial general-biquad_c
           (fn [sino coso alpha
                b0 b1 b2
                a0 a1 a2]
             (reset! b0 alpha)
             (reset! b1 0.0)
             (reset! b2 (* -1.0 @b0))
             (reset! a0 (+ 1.0 alpha))
             (reset! a1 (* -2.0 coso))
             (reset! a2 (- 1.0 alpha)))))


(def notch_c
  "Notch filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt"
  (partial general-biquad_c
           (fn [sino coso alpha
                b0 b1 b2
                a0 a1 a2]
             (reset! b0 1.0)
             (reset! b1 (* -2.0 coso))
             (reset! b2 @b0)
             (reset! a0 (+ 1.0 alpha))
             (reset! a1 @b1)
             (reset! a2 (- 1.0 alpha)))))


(def apf_c
  "Allpass filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt"
  (partial general-biquad_c
           (fn [sino coso alpha
                b0 b1 b2
                a0 a1 a2]
             (reset! b0 (- 1.0 alpha))
             (reset! b1 (* -2.0 coso))
             (reset! b2 (+ 1.0 alpha))
             (reset! a0 (+ 1.0 alpha))
             (reset! a1 @b1)
             (reset! a2 (- 1.0 alpha)))))


(defn pink_c
  "Pink noise generator.

  Uses Paul Kellet's economy method
  http://www.firstpr.com.au/dsp/pink-noise/"
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


(def sum-fns
  (partial reduce (fn ^double [^double val item] (+ val (item)) ) 0.0))

