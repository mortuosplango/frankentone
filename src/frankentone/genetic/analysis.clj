(ns frankentone.genetic.analysis
  (:use frankentone.genetic.hanning)
  (:require [incanter core charts stats])
  (:import [edu.emory.mathcs.jtransforms.fft FloatFFT_1D]
           [hep.aida.tfloat.bin
            MightyStaticFloatBin1D
            StaticFloatBin1D]
           [cern.colt.list.tfloat FloatArrayList]))

;; (defn rms 
;;   "Calculate the root mean square [RMS] value of the given array"
;;   ^Double [array]
;;   (p :rms (Math/sqrt (incanter.stats/mean
;;                       (incanter.core/mult array array)))))

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
  ;; (/
  ;;  (Math/pow (reduce * fft-frame) (/ 1.0 (count fft-frame)))
  ;;  (incanter.stats/mean fft-frame))
  (let [fbin (MightyStaticFloatBin1D. true false 0)]
    (.addAllOf fbin (FloatArrayList. (float-array fft-frame)))
    (/ (.geometricMean fbin)
       (.mean fbin))))

;;;;;;;; based on http://inovation.tistory.com/archive/20110403
(defn get-fft-mags
  "Calculates the fft magnitudes for the given buffer"
  ([buffer] (get-fft-mags buffer 1024))
  ([buffer windowsize]
     (let [
           num-windows (int (Math/ceil
                             (/ (count buffer) windowsize)))
           ana-buf (vec (concat buffer
                                (repeat
                                 (mod (count buffer)
                                      windowsize)
                                 0)))]
       (mapv (fn [buf]
               (let [tmp-buf
                     (float-array
                      (incanter.core/mult
                       (vec buf)
                       hann-window))]
                 ;; calc magnitudes
                 (mapv
                  (fn [[real imag]]
                    ;; get magnitudes
                    ;; clip on float max/min values to remove
                    ;; potential inf values
                    (max
                     Float/MIN_VALUE
                     (min Float/MAX_VALUE
                          (Math/sqrt (+
                                      (Math/pow real 2)
                                      (Math/pow imag 2))))))
                  (partition 2
                             ;; perform fft
                             (do
                               (.realForward
                                (FloatFFT_1D. windowsize)
                                tmp-buf)
                               tmp-buf)))))
             (partition windowsize ana-buf)))))

(defn get-fft-weights
  "Returns the weights for the given fft magnitudes."
  ([fft] (get-fft-weights fft 0.2))
  ([fft O]
      (mapv
       (fn [frame]
         (let [
               log-frame (incanter.core/log
                          (incanter.core/abs frame))
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
  (let [samples (mapv last target-data)
        fft (get-fft-mags samples 1024)]
    (hash-map
     :samples samples
     :rms (double (rms samples))
     :smps (reduce #(+ (Math/abs %1) (Math/abs %2)) samples)
     :fft fft
     :fft-weights (get-fft-weights fft)
     :spectral-flatness (mapv spectral-flatness fft))))

