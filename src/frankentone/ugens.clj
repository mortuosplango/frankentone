;; The ugens here are largely ports of extempore dsp code found here:
;; https://github.com/digego/extempore/blob/master/libs/core/audio_dsp.xtm

(ns frankentone.ugens
  (:use frankentone.utils))

(def sample-dur (/ 1.0 *sample-rate*))


(definline calc-feedback [delaytime decaytime]
  `(if (> ~decaytime 0.0)
    (Math/exp (/ (* (Math/log 0.001) ~delaytime) ~decaytime))
    (* -1.0 (Math/exp (/ (* (Math/log 0.001) ~delaytime) (* -1.0 ~decaytime))))))


(definline lerp
  "Calculates a number between two numbers at a specific increment. The
  amt parameter is the amount to interpolate between the two values
  where 0.0 equal to the first point, 0.1 is very near the first
  point, 0.5 is half-way in between, etc. The lerp function is
  convenient for creating motion along a straight path and for drawing
  dotted lines."
  [start end amt]
  `(+ (* (- ~end ~start) ~amt) ~start))

(deftype tLine
    [^:unsynchronized-mutable ^double time
     ^double start
     ^double end
     ^double step]
  clojure.lang.IFn
  (invoke ^double [_]
    (set! time (+ time step))
    (if (< time 1.0)
      (lerp start end time)
      end)))

(defn line-c
  [start end length]
  (if (> length 0.0)
    (tLine. 0.0 start end (* (/ 1.0 length) sample-dur))
    (constantly end)))


(deftype tAsr
    [^:unsynchronized-mutable ^double time
     ^tLine a-line
     ^tLine r-line
     ^long attack-time
     ^long release-time
     ^long attack+sustain-time
     ^double sustain-level]
  clojure.lang.IFn
  (invoke ^double [_]
    (set! time (unchecked-inc time))
    (if (< time attack-time)
      (a-line)
      (if (< time attack+sustain-time)
        sustain-level
        (r-line)))))

(defn asr-c
  [attack-time sustain-time sustain-level release-time]
  (let [
        attack-time (max 0.0 attack-time)
        sustain-time (max 0.0 sustain-time)
        release-time (max 0.0 release-time)
        a-line (line-c 0.0 sustain-level attack-time)
        r-line (line-c sustain-level 0.0 release-time)
        attack-time (long (* attack-time *sample-rate*))
        release-time (long (* release-time *sample-rate*))
        attack+sustain-time (long (+ attack-time
                                     (* sustain-time *sample-rate*)))]
    (tAsr. 0.0
           a-line r-line
           attack-time release-time attack+sustain-time
           sustain-level)))


(defn sine
  "Sine"
  ([freq phase]
     (Math/sin (* freq TAU phase))))


(defn lfsaw [freq phase] 
  (let [modu (/ 1.0 freq)]
    (+ 1.0 (* (mod phase modu) -2.0 freq))))

   
(defn hardclip
  "Hard clip"
  ([input]
     (max -1.0 (min 1.0 input)))
  ([input min-val max-val]
     (max min-val (min max-val input))))

(deftype tOsc
    [^:unsynchronized-mutable ^double phase
     ^double freq-mul]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (* amp
       (Math/sin
        (set! phase
              (let [new-phase
                    (+ phase (* freq freq-mul))]
                (if (> new-phase Math/PI)
                  (- new-phase TAU)
                  new-phase)))))))

(defn osc-c
  "a sine oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (tOsc. (double in-phase) (double (/ TAU *sample-rate*)) ))


(deftype tImpulse
    [^:unsynchronized-mutable ^double phase]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (let [out (if (>= phase 1.0)
                (do (set! phase (- phase 1.0))
                    1.0)
                0.0)]
      (set! phase (+ phase (* freq sample-dur)))
      out)))

(defn impulse-c
  "an impulse oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (tImpulse.  (if (= (double in-phase) 0.0)
           1.0
           (double in-phase))))


(deftype tOnePole
    [^:unsynchronized-mutable ^double last]
  clojure.lang.IFn
  (invoke ^double [_ in coef]
    (let [in-1 last]
      (+ (* (- 1.0 (Math/abs (double coef)))
            (set! last (double in)))
         (* coef
            in-1)))))

(defn one-pole-c
  "A one pole filter. Implements the formula:

  out(i) = ((1 - abs(coef)) * in(i)) + (coef * out(i-1))."
  []
  (tOnePole. 0.0))


(deftype tPulseCount
    [^:unsynchronized-mutable ^double last
     ^:unsynchronized-mutable ^double last-reset
     ^:unsynchronized-mutable ^double count]
  clojure.lang.IFn
  (invoke ^double [_ trig reset]
    (if (and (<= last-reset 0.0)
             (> reset 0.0))
      (set! count 0.0)
      (when (and (<= last 0.0)
               (> trig 0.0))
        (set! count (+ count 1.0))))
    (set! last-reset (double reset))
    (set! last (double trig))
    count))

(defn pulse-count-c
  "a pulse counter

  Returns a function with the following arguments: [trig reset]"
  []
  (tPulseCount. 0.0 0.0 0.0))


(deftype tFSinOsc
    [^:unsynchronized-mutable ^double y0
     ^:unsynchronized-mutable ^double y1
     ^:unsynchronized-mutable ^double y2
     ^:unsynchronized-mutable ^double b1
     ^:unsynchronized-mutable ^double prevfreq
     ^double radians-per-sample]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (when-not (= freq prevfreq)
      (set! prevfreq (double freq))
      (set! b1 (* 2.0 (Math/cos
                       (* freq radians-per-sample)))))
    (set! y0 (- (* b1 y1) y2))
    (set! y2 y1)
    (set! y1 y0)
    (* amp y0)))


(defn fsin-osc-c
  "Fast sine wave generator implemented using a ringing filter.
  This generates a much cleaner sine wave than a table lookup
  oscillator and is a lot faster. However, the amplitude of the wave
  will vary with frequency. Generally the amplitude will go down as
  you raise the frequency and go up as you lower the frequency.

  Based on http://www.musicdsp.org/showone.php?id=9

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (tFSinOsc.
   0.0
   0.0
   0.0
   0.0
   0.0
   (* TAU sample-dur)))


(deftype tOscFb
    [^:unsynchronized-mutable ^double phase
     ^:unsynchronized-mutable ^double prev
     ^double freq-mul]
  clojure.lang.IFn
  (invoke ^double [_ amp freq feedback]
    (* amp
       (set! prev
             (Math/sin
              (set! phase
                    (let [new-phase
                          (+
                           (* feedback prev)
                           phase (* freq freq-mul))]
                      (if (> new-phase Math/PI)
                        (- new-phase TAU)
                        new-phase))))))))

(defn osc-fb-c
  "a sine oscillator that has phase modulation feedback

  Returns a function with the following arguments: [amp freq feedback]"
  [in-phase]
  (tOscFb. (double in-phase) 0.0 (double (/ TAU *sample-rate*)) ))

  
(deftype tSinOsc
    [^:unsynchronized-mutable ^double phase
     ^doubles sine-table
     ^double table-size
     ^double cycle]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (* amp
       (aget ^doubles sine-table
             (set! phase 
                   (let [new-phase
                         (+ phase (* cycle freq))]
                     (if (> new-phase table-size)
                       (- new-phase table-size)
                       new-phase)))))))

(let [
      table-size (double 8192)
      cycle (double (/ table-size *sample-rate*))
      sine-table (double-array (vec (map
                                     #(Math/sin (* TAU (/ % table-size)))
                                     (range table-size))))]

  (defn sin-osc-c
    "a sine oscillator using a 8192 point wavetable

  Returns a function with the following arguments: [amp freq]"
    [in-phase]
    (tSinOsc. (mod (* table-size (double in-phase))
                   table-size) sine-table table-size cycle)))


(defn square-c
  "a square oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (let [osc (sin-osc-c in-phase)
        n 50.0]
    (fn ^double [amp freq]
      (* amp (Math/tanh (osc n freq))))))


(deftype tSaw
    [^:unsynchronized-mutable ^double phase
     ^:unsynchronized-mutable ^double dp
     ^:unsynchronized-mutable ^double saw
     ^double re-half-sample-rate]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (let [x (max 0.000001
                 (* Math/PI
                    (set! phase
                          (let [new-phase (+ phase dp)]
                            (if (< new-phase 0.0)
                              (do (set! dp (* dp -1.0))
                                  (- 0.0 new-phase))
                              (let [qmax (/ (* 0.5 *sample-rate*) freq)]
                                (if (> new-phase qmax)
                                  (do (set! dp (* dp -1.0))
                                      (+ qmax (- qmax new-phase)))
                                  new-phase)))))))]
      (* amp (set! saw
                   (* 0.995 ;;leak
                      (+ saw
                         (* -0.498 freq re-half-sample-rate)
                         (/ (Math/sin x) x))))))))

(defn saw-c
  "saw oscillator

  Returns a function with the following arguments: [amp freq]"
  [in-phase]
  (tSaw. (double in-phase) 1.0 0.0 (double (/ 1.0 (* 0.5 *sample-rate*)))))


(definline -calc-phase
  [oldphase freq]
  `(let [newphase#
         (+ ~oldphase
            (* ~freq (* 2.0 sample-dur)))]
     (cond
      (>= newphase# 1.0) (- newphase# 2.0)
      (< newphase# -1.0) (+ newphase# 2.0)
      :default newphase#)))


(deftype tSawDPW
    [
     ;; phase of the oscillator, from -1 to 1.
     ^:unsynchronized-mutable ^double phase
     ;; memory of prev value
     ^:unsynchronized-mutable ^double val
     ^:unsynchronized-mutable ^double prevfreq
     ^:unsynchronized-mutable ^double scalefac]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (let [oldval val
          freq (double freq)]
      (set! phase (double (-calc-phase phase freq)))
      (* amp
         (- (set! val 
                  (Math/pow
                   phase
                   2.0))
            oldval)
         (if (= freq prevfreq)
           scalefac
           (set! scalefac
                 (/ *sample-rate*
                    (* 8.0
                       (set! prevfreq freq)
                       (- 1.0
                          (* freq
                             sample-dur))))))))))

(defn sawdpw-c
  "A sawtooth oscillator using the 'Differentiated Parabolic Wave' technique

The technique is documented in Välimäki (2005) Signal Processing Letters 12(3) pages 214-217.

Port of http://doc.sccode.org/Classes/SawDPW.html.

Returns a function with the following arguments: [amp freq]"
  [inphase]
  (tSawDPW. (double inphase) 0.0 0.0 0.0))


(deftype tPulseDPW
    [
     ;; phase of the oscillator, from -1 to 1.
     ^:unsynchronized-mutable ^double phase-a
     ^:unsynchronized-mutable ^double phase-b
     ;; memory of prev value
     ^:unsynchronized-mutable ^double val-a
     ^:unsynchronized-mutable ^double val-b
     ^:unsynchronized-mutable ^double prevfreq
     ^:unsynchronized-mutable ^double scalefac]
  clojure.lang.IFn
  (invoke ^double [_ amp freq]
    (let [freq (double freq)
          oldval-a val-a
          oldval-b val-b]
      (set! phase-a (double (-calc-phase phase-a freq)))
      (set! phase-b (double (-calc-phase phase-b freq)))
      (* amp
         (- (- (set! val-a 
                     (Math/pow
                      phase-a
                      2))
               oldval-a)
            (- (set! val-b
                     (Math/pow
                      phase-b
                      2))
               oldval-b))
         (if (= freq prevfreq)
           scalefac
           (set! scalefac
                 (/ *sample-rate*
                    (* 8.0
                       (set! prevfreq freq)
                       (- 1.0
                          (* freq
                             sample-dur))))))))))

(defn pulsedpw-c 
  "Use this just like the Pulse UGen - as with SawDPW, this uses the
  'Differentiated Parabolic Wave' technique to create a waveform
  extremely efficiently and with low (but not zero) aliasing.

  width - Pulse width ratio from zero to one. 0.5 makes a square wave.
  This cannot be modulated.

  The technique is documented in Välimäki (2005) Signal Processing
  Letters 12(3) pages 214-217.

  Port of http://doc.sccode.org/Classes/PulseDPW.html.

  Returns a function with the following arguments: [amp freq]"
  [width]
  (tPulseDPW. (double width) 0.0 0.0 0.0 0.0 0.0))


(deftype tDelay
    [^java.nio.DoubleBuffer line
     ^long delay]
  clojure.lang.IFn
  (invoke ^double [_ input wet feedback]
    (let [delayed (.get line (.position line))]
      (.put line ^double (* feedback (+ input delayed)))
      (when-not (.hasRemaining line)
        (.clear line))
      (+ input (* delayed wet)))))

(defn delay-c
  "IIR comb without interpolation.

  More efficient than comb if you don't need variable length

  Returns a function with the following arguments: [input wet feedback]"
  [max_delay]
  (let [delay (long (Math/ceil (* (hardclip max_delay
                                            sample-dur
                                            60.0) *sample-rate*)))]
    (tDelay. (java.nio.DoubleBuffer/allocate delay) delay)))


(defprotocol IBiquad
  "Biquad"
  (lpf [this sino coso alpha])
  (hpf [this sino coso alpha])
  (bpf [this sino coso alpha])
  (notch [this sino coso alpha])
  (apf [this sino coso alpha]))


(deftype GeneralBiquad
    [^:unsynchronized-mutable ^double y1
     ^:unsynchronized-mutable ^double y2
     ^:unsynchronized-mutable ^double x1
     ^:unsynchronized-mutable ^double x2
     ^:unsynchronized-mutable ^double b0
     ^:unsynchronized-mutable ^double b1
     ^:unsynchronized-mutable ^double b2
     ^:unsynchronized-mutable ^double re-a0
     ^:unsynchronized-mutable ^double a1
     ^:unsynchronized-mutable ^double a2
     ^:unsynchronized-mutable ^double oldres
     ^:unsynchronized-mutable ^double oldfreq
     ^int filterid
     ^double omega-factor]
  
  IBiquad
  (lpf [_ sino coso alpha]
    (set! b0 (/ (- 1.0 coso) 2.0))
    (set! b1 (- 1.0 coso))
    (set! b2 b0)
    (set! re-a0 (/ 1.0 (+ 1.0 alpha)))
    (set! a1 (* -2.0 coso))
    (set! a2 (- 1.0 alpha)))
  (hpf [_ sino coso alpha]
    (set! b1 (* -1.0 (+ 1.0 coso)))
    (set! b0 (/ (+ 1.0 coso) 2.0))
    (set! b2 b0)
    (set! re-a0 (/ 1.0 (+ 1.0 alpha)))
    (set! a1 (* -2.0 coso))
    (set! a2 (- 1.0 alpha)))
  (bpf [_ sino coso alpha]
    ;;(set! b1 0.0)
    (set! b2 (* -1.0 (set! b0 (double alpha))))
    (set! re-a0 (/ 1.0 (+ 1.0 alpha)))
    (set! a1 (* -2.0 coso))
    (set! a2 (- 1.0 alpha)))
  (notch [_ sino coso alpha]
    (set! b0 1.0)
    (set! b1 (* -2.0 coso))
    (set! b2 1.0)
    (set! re-a0 (/ 1.0 (+ 1.0 alpha)))
    (set! a1 b1)
    (set! a2 (- 1.0 alpha)))
  (apf [_ sino coso alpha]
    (set! b0 (- 1.0 alpha))
    (set! b2 (+ 1.0 alpha))
    (set! re-a0 (/ 1.0 (+ 1.0 alpha)))
    (set! b1 (* -2.0 coso))
    (set! a1 b1)
    (set! a2 (- 1.0 alpha)))
  
  clojure.lang.IFn
  (invoke ^double [this x freq res]
    ;; if frequency changes
    ;; recalculate coefficients
    (when (or (not= freq oldfreq)
              (not= res oldres))
      (let [omega (* (set! oldfreq (double freq))
                     omega-factor)
            sino (Math/sin omega)
            coso (Math/cos omega)
            alpha (/ sino (* 2.0
                             (set! oldres (double res))))]
        (case filterid
          0 (lpf this sino coso alpha)
          1 (hpf this sino coso alpha)
          2 (bpf this sino coso alpha)
          3 (notch this sino coso alpha)
          4 (apf this sino coso alpha))))
    (let [x (double x)
          y (- (+ (* b0 re-a0 x)
                  (* b1 re-a0 x1)
                  (* b2 re-a0 x2))
               (* a1 re-a0 y1)
               (* a2 re-a0 y2))]
      (set! y2 y1)
      (set! y1 y)
      (set! x2 x1)
      (set! x1 x)
      y)))

(let [omega-factor (double (/ TAU *sample-rate*))]
  (defn lpf-c
    "Lowpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
    []
    (GeneralBiquad. 0.0 0.0
                    0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0
                    omega-factor))

  (defn hpf-c
    "Highpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
    []
    (GeneralBiquad. 0.0 0.0
                    0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 1
                    omega-factor))


  (defn bpf-c
    "Bandpass Filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
    []
    (GeneralBiquad. 0.0 0.0
                    0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 2
                    omega-factor))


  (defn notch-c
    "Notch filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
    []
    (GeneralBiquad. 0.0 0.0
                    0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 3
                    omega-factor))


  (defn apf-c
    "Allpass filter.

  BiQuad coefficient formulae from Audio EQ Cookbook Robert
  Bristow-Johnson

  http://www.musicdsp.org/files/Audio-EQ-Cookbook.txt

  Returns a function with the following arguments: [input freq Q]"
    []
    (GeneralBiquad. 0.0 0.0
                    0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 0.0
                    0.0 0.0 4  omega-factor)))

(deftype tWhiteNoise
    [^java.util.concurrent.ThreadLocalRandom random]
  clojure.lang.IFn
  (invoke ^double [_]
    (.nextDouble random -1.0 1.0)))

(defn white-noise-c
  "white noise"
  []
  (tWhiteNoise. (java.util.concurrent.ThreadLocalRandom/current)))

(defn white-noise
  ^double []
  (- (* (rand) 2.0) 1.0))


(deftype tSIDNoise
    [^:unsynchronized-mutable ^int gx1
     ^:unsynchronized-mutable ^int gx2
     ^double gf-scale]
  clojure.lang.IFn
  (invoke ^double [_]
    (set! gx1 (unchecked-int (bit-xor gx1 gx2)))
    (* (set! gx2
             (unchecked-add-int
              gx2
              gx1))
       gf-scale)))


(defn sidnoise-c
  "SID white noise

  Based on http://www.musicdsp.org/showone.php?id=216

  Returns a function with the following arguments: []"
  []
  (let [gf-scale (/ 1.0 Integer/MAX_VALUE)
        gx1 (int (+ 0x67452301 Integer/MIN_VALUE))
        gx2 (int (+ 0xefcdab89 Integer/MIN_VALUE))]
    (tSIDNoise. gx1 gx2 gf-scale)))


(deftype tPinkNoise
    [^:unsynchronized-mutable ^double b0
     ^:unsynchronized-mutable ^double b1
     ^:unsynchronized-mutable ^double b2
     ^tWhiteNoise white-noise]
  clojure.lang.IFn
  (invoke ^double [_]
    (* 0.33 (+
             (set! b0 (+ (* b0 0.99765) (* (white-noise) 0.0555179)))
             (set! b1 (+ (* b1 0.96300) (* (white-noise) 0.2965164)))
             (set! b2 (+ (* b2 0.57000) (* (white-noise) 1.0526913)))
             (* (white-noise) 0.1848)))))


(defn pink-c
  "Pink noise generator.

  Uses Paul Kellet's economy method
  http://www.firstpr.com.au/dsp/pink-noise/

  Returns a function with the following arguments: []"
  []
  (tPinkNoise. 0.0 0.0 0.0
               (white-noise-c)))


(deftype tMoogff
    [^:unsynchronized-mutable ^double b0
     ^:unsynchronized-mutable ^double a1     
     ^:unsynchronized-mutable ^double s1     
     ^:unsynchronized-mutable ^double s2     
     ^:unsynchronized-mutable ^double s3     
     ^:unsynchronized-mutable ^double s4     
     ^:unsynchronized-mutable ^double prevfreq     
     ^:unsynchronized-mutable ^double reset]
  clojure.lang.IFn
  (invoke ^double [this in freq gain]
    (let [in (double in)
          freq (double freq)
          gain (double gain)]
      (when (not= freq prevfreq)
        (let [TwcD (* sample-dur
                      ;; wcd:
                      (max 0.0
                           (* 2.0
                              *sample-rate*
                              (Math/tan (* sample-dur
                                           Math/PI freq)))))]
          (set! b0 (/ TwcD (+ TwcD 2.0)))
          (set! a1 (/ (- TwcD 2.0)
                      (+ TwcD 2.0))))
        (set! prevfreq freq))

      ;; Reset filter state if requested
      (when (> reset 0.0)
        (set! s1 0.0)
        (set! s2 0.0)
        (set! s3 0.0)
        (set! s4 0.0))
      
      (let [gain (hardclip gain 0.0 4.0)
            b0cubed (Math/pow b0 4.0)
            out (* (+ (* b0cubed
                         in)
                      ;; o
                      (+ s4 (* b0
                               (+ s2
                                  (* b0 s1)))))
                   (/ 1.0
                      (+ 1.0 (* b0cubed gain))))]

        ;; update 1st order filter states
        (set!
         s4
         (- (* b0
               (let [past (double (- in (* gain out)))
                     future (double (+ (* b0 past) s1))]
                 (set! s1
                       (- (* b0 past)
                          (* a1 future)))
                 (let [past future
                       future (+ (* b0 future)
                                 s2)]
                   (set! s2
                         (- (* b0 past)
                            (* a1 future)))
                   (let [past future
                         future (+ (* b0 future)
                                   s3)]
                     (set! s3
                           (- (* b0 past)
                              (* a1 future)))
                     future))))
            (* a1 out)))
        out))))


(defn moogff-c
  "A digital implementation of the Moog VCF (filter).

  The design of this filter is described in the conference paper
  Fontana, F. (2007) Preserving the Digital Structure of the Moog VCF.
  In Proc. ICMC07, Copenhagen, 25-31 August 2007.

  Original Java code created by F. Fontana - August 2007 -
  federico.fontana@univr.it Ported to C++ for SuperCollider by Dan
  Stowell.

  Port of http://doc.sccode.org/Classes/MoogFF.html

  Returns a function with the following arguments: [freq gain]"
  []
  (tMoogff. 0.0 0.0 0.0 0.0
            0.0 0.0
            0.0 0.0))

(deftype tAdcverb
    [^double rev-time
     ^double hf-damping
     ^double  predelay 
     ^int  num-combs
     ^int  num-allpasses
     ^double  in-filter 
     ^double  leak-coef 
     ^double  comb-scale
     ^double  ap-scale 

     comb-times
     allpass-primes
     allpass-times 
     allpass-freqs 

     ^tDelay u-predelay 
     ^tOnePole u-pole 

     ;; TODO round delay times to integer samples
     ;; and add up to half a sample to them: 
     ;; linear interpolation between samples loses 
     ;; high freq energy, with the maximum at 0.5.
     comb-times-scaled

     u-combs 

     ;; allpass decay always is shorter than combs decay
     ^double  ap-decay
     u-apfs 

     ;; Put the output through nOuts parallel chains of allpass delays
     ]
  clojure.lang.IFn
  (invoke ^double [this in]
    (let [
          ;;  TODO block DC, round off and
          ;;  pre-delay reverb input.
          in1 (u-predelay
               (u-pole in in-filter)
               1.0
               0.0)
          
          ;; Create an array of combs, with a special trick to make
          ;; treble decay faster than lows:
          in2 (reduce (fn [val [comb feedback]]
                        (+ val (comb in1 1.0 feedback)))
                      0.0
                      u-combs)]
      ;; Put the output through nOuts parallel chains of allpass
      ;; delays
      
      (reduce (fn [val [apf x]]
                (apf val x 0.5))
              (+ in2 in) u-apfs))))



(defn adcverb-c
  "Very rough and incomplete port of the SuperCollider AdCVerb
  pseudo-ugen.

  See http://quark.sccode.org/SpeakersCorner/AdCVerb.html"
  []
  (let [rev-time 3.0
        hf-damping 0.1
        predelay 0.02
        num-combs 8
        num-allpasses 4
        in-filter 0.6
        leak-coef 0.995
        comb-scale 1.0
        ap-scale 1.0
        
        comb-times 	(take num-combs [0.0797949
                                     0.060825
                                     0.0475902 
                                     0.0854197 
                                     0.0486931
                                     0.0654572
                                     0.0717437
                                     0.0826624
                                     0.0707511
                                     0.0579574
                                     0.0634719
                                     0.0662292])
        allpass-primes (let [prime-range (int (/ 250 num-allpasses))]
                         (mapv
                          (fn [x] (nth-prime (rrand (* prime-range (inc x))
                                                   (* prime-range (+ x 2)))))
                          (range num-allpasses)))
        allpass-times (mapv #(double (/ % 44100)) allpass-primes)
        allpass-freqs (mapv #(double (/ 1.0 %)) allpass-primes)

        u-predelay (delay-c predelay)
        u-pole (one-pole-c)
        
        ;; TODO round delay times to integer samples
		;; and add up to half a sample to them: 
		;; linear interpolation between samples loses 
		;; high freq energy, with the maximum at 0.5.
        comb-times-scaled (mapv (fn [x] (+ (* x comb-scale)
                                          (* -1 (mod x sample-dur))
                                          (* sample-dur 0.5 hf-damping)))
                                comb-times)

        u-combs (mapv (fn [x] [(delay-c x) (calc-feedback x rev-time)]) comb-times)
        
        ;; allpass decay always is shorter than combs decay
        ap-decay (min 1.0 (* rev-time 0.6))
        u-apfs (mapv (fn [x] [(apf-c) x]) allpass-times)
        ]

    (tAdcverb.     rev-time
                   hf-damping
                   predelay 
                   num-combs
                   num-allpasses
                   in-filter 
                   leak-coef 
                   comb-scale
                   ap-scale 
                   
                   comb-times
                   allpass-primes
                   allpass-times 
                   allpass-freqs 

                   u-predelay 
                   u-pole 

                   comb-times-scaled

                   u-combs 
                   
                   ap-decay
                   u-apfs)))


(definline sum-fns
  "Sums up a collection of functions"
  [vector]
  `(reduce-kv (fn [val# key# item#] (+ val# (item#))) 0.0 ~vector))

