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
  (defn get-fft-mags
    "Calculates the fft magnitudes for the given buffer"
    ([buffer] (get-fft-mags buffer 1024 0.25))
    ([buffer windowsize] (get-fft-mags buffer 1024 0.25))
    ([buffer windowsize hop]
       (let [window (if (= windowsize 1024)
                      window-1024
                      (mapv #(mhann-window % windowsize) (range windowsize)))
             hop-in-samples (* windowsize hop)
             num-windows (int (Math/ceil
                               (/ (count buffer) hop-in-samples)))]
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
               (FloatFFT_1D. windowsize)
               tmp-buf)
              ;; calc magnitudes
              (amap
               (float-array (* 0.5 windowsize))
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
          (partition windowsize 
                     (* windowsize hop)
                     (repeat 0.0)
                     buffer))))))

(defn get-fft-mags
  "Calculates the fft magnitudes for the given buffer"
  ([buf]
     (get-fft-mags buf 1024 0.5))
  ([buf windowsize]
     (get-fft-mags buf windowsize 0.5))
  ([buf windowsize hob]
     (let [fft (FFT. windowsize *sample-rate*)
           spectrum (float-array (* 0.5 windowsize))]
       (.window fft (HannWindow.))
       (mapv (fn [buf]
               (.forward fft (float-array buf))
               (amap spectrum i _
                     (max
                      Float/MIN_VALUE
                      (min Float/MAX_VALUE
                           (.getBand fft i)))))
             (partition windowsize (* hob windowsize) (repeat 0.0)
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
  [target-data]
  (let [samples (first target-data)
        fft (get-fft-mags samples 1024)]
    (hash-map
     :samples samples
     :x (second target-data)
     :rms (double (rms samples))
     :smps (reduce #(+ (Math/abs ^double %1) (Math/abs ^double %2)) samples)
     :fft fft
     :fft-weights (get-fft-weights fft)
     :spectral-flatness (mapv spectral-flatness fft))))

