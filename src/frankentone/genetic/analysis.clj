(ns frankentone.genetic.analysis
  (:use [frankentone.genetic hanning]
        [frankentone utils])
  (:require [incanter core charts stats])
  (:import [edu.emory.mathcs.jtransforms.fft FloatFFT_1D]
           [ddf.minim.analysis FFT HannWindow]
           [hep.aida.tfloat.bin
            MightyStaticFloatBin1D
            StaticFloatBin1D]
           [cern.colt.list.tfloat FloatArrayList]))


(defn rms 
  "Calculate the root mean square [RMS] value of the given array"
  ^Double [array]
  (let [fbin (StaticFloatBin1D.)]
    (.addAllOf fbin (FloatArrayList. (float-array array)))
    (.rms fbin)))

(defn windowed-rms 
  "Calculate the root mean square [RMS] value of the given array"
  ^Double [array window-size hop]
  (mapv rms
        (partition window-size (* window-size hop) array)))


(defn spectral-flatness
  "Calculate the spectral flatness for the given fft frame.
   https://en.wikipedia.org/wiki/Spectral_flatness
   calculated by dividing the geometric mean
   by the arithmetic mean of the power spectrum"
  [fft-frame]
  (let [fbin (MightyStaticFloatBin1D. true false 0)]
    (.addAllOf fbin (FloatArrayList. ^floats fft-frame))
    (/ (.geometricMean fbin)
       (.mean fbin))))


;;;;;;;; based on http://inovation.tistory.com/archive/20110403
(comment let [mhann-window (memoize hann-window)
      window-1024 (mapv #(mhann-window % 1024) (range 1024))]
  (defn get-fft-mags-old
    "Calculates the fft magnitudes for the given buffer"
    ([buffer] (get-fft-mags buffer 1024 0.25))
    ([buffer window-size] (get-fft-mags buffer 1024 0.25))
    ([buffer window-size hop]
       (let [window (if (= window-size 1024)
                      window-1024
                      (mapv #(mhann-window % window-size) (range window-size)))
             hop-in-samples (* window-size hop)
             num-windows (int (Math/ceil
                               (/ (count buffer) hop-in-samples)))
             fft (FloatFFT_1D. window-size)]
         (mapv
          (fn [buf]
            (let [tmp-buf
                  (float-array
                   (incanter.core/mult
                    buf
                    window)
                   )]
              ;; perform fft
              (.realForward
               fft
               tmp-buf)
              ;; calc magnitudes
              (amap
               (float-array (* 0.5 window-size))
               i
               _
               ;; get magnitudes
               ;; clip on float max/min values to remove
               ;; potential inf values
               (let [real (aget ^floats tmp-buf (* i 2))
                     imag (aget ^floats tmp-buf (inc (* i 2)))]
                 (max
                  Float/MIN_VALUE
                  (min Float/MAX_VALUE
                       (Math/sqrt (+
                                   (* real real)
                                   (* imag imag)))))))))
          (partition window-size 
                     (* window-size hop)
                     (repeat 0.0)
                     buffer))))))

(defn get-fft-mags
  "Calculates the fft magnitudes for the given buffer"
  ([buf]
     (get-fft-mags buf 1024 0.5))
  ([buf window-size]
     (get-fft-mags buf window-size 0.5))
  ([buf window-size hob]
     (let [fft (FFT. window-size *sample-rate*)
           spectrum (float-array (* 0.5 window-size))]
       (.window fft (HannWindow.))
       (mapv (fn [buf]
               (.forward fft (float-array buf))
               (amap spectrum i _
                     (max
                      Float/MIN_VALUE
                      (min Float/MAX_VALUE
                           (.getBand fft i)))))
             (partition window-size (* hob window-size) (repeat 0.0)
                        buf)))))

(defn get-fft-weights
  "Returns the weights for the given fft magnitudes."
  ([fft] (get-fft-weights fft 0.2))
  ([fft O]
      (mapv
       (fn [frame]
         (let [
               log-frame (incanter.core/log
                          (incanter.core/abs (vec frame)))
               min-frame (double (reduce min log-frame))
               max-frame (reduce max log-frame)]
           (println min-frame max-frame)
           (mapv #(+ O
                     (*
                      (- 1 O)
                      (/ (- %1 min-frame)
                         (Math/abs
                          (- min-frame max-frame)))))
                 log-frame)))
       fft)))


(defn get-reference-map
  [samples]
  (let [window-size 1024
        fft (get-fft-mags samples window-size)]
    (hash-map
     :samples samples
     :x (amap ^doubles samples i _ (double (/ i *sample-rate*)))
     :rms (windowed-rms samples window-size 0.25)
     :smps (reduce #(+ (Math/abs ^double %1) (Math/abs ^double %2)) samples)
     :fft fft
     :fft-weights (get-fft-weights fft)
     :spectral-flatness (mapv spectral-flatness fft))))

