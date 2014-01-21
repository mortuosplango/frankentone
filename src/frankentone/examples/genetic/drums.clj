(ns frankentone.examples.genetic.drums
  (:use [frankentone.genetic analysis simplegp simplegp-functions utils dspgp]
        [frankentone utils ugens dsp instruments patterns samples]
        [overtone.music time])
  (:require clojure.java.io))


(doall (map (fn [name]
              (eval (conj '((fn [x y z] (fn [x] 0.0)))
                          (symbol name) 'definst)))
            ['hh 'bd 'sn]))

(defn pat [t]
  (play-pattern [
                 - - [hh - - -] - ||
                 ;; (repeat 4 [- - hh -]) ||
                 ;; 40 - - - - ||
                 - [sn sn] - - ||
                 ;;- sn - sn ||
                 bd - bd - ||
                 ;; bd ||
                 ;; (repeat 5 [bd -])
                 ] 2.0 0.5 :default (/ t 1000.0))
  (let [next (+ t 2000)]
    (apply-at next #'pat [next])))

(pat (+ (now) 500))

(defn pat [_])
(stop-dsp)
(start-dsp)
(sine!)

(reset-dsp! (let [prev (atom 0.0)
                  lpf (lpf-c)
                  hpf (hpf-c)]
              (fn [x chan] 
              	(if (zero? chan)
                  (reset! prev
                          (lpf
                          	(hpf (mul-tanh
                                5.0
                                (+ 
                                 (sn x)
                                 (bd x)
                                 (hh x)
                                 (default x)))
                                 10.0 1.0)
                               14000.0
                               1.0))
                  @prev))))

;; (start-dsp)
;; (stop-dsp)


(defn best-callback
  [name sample best best-error]
  (let [instfn
        (binding [buffer (double-array *sample-rate* 0.0)]
          (let [
                val-func (program->fn best)
                prev-samp (atom 0.0)]
            (fn ^Double [x]
              (swap! prev-samp #(val-func x %)))))
        len (* (count sample) sample-dur)]
    (when-let [inst (get @instruments name)]
      (.setNoteKernel inst
                      (fn [freq amp dur]
                        (fn ^double [x]
                          (if (< x len)
                            (* amp (instfn x))
                            0.0))))))
  (Thread/sleep (* 5 4 1000)))


;;;; Run it 
(def evol
  (doall
   (map
    (fn [sample name]
      ;; init instruments
      (eval (conj '((fn [x y z] (fn [x] 0.0)))
                  (symbol name) 'definst))
      (future (evolve 300 (memoize
                           (partial error-fn
                                    (get-reference-map sample)
                                    [:rms]))
                      :best-callback (partial best-callback
                                              (keyword name) sample)
                      :terminals dsp-terminals
                      :functions dsp-functions))
      (Thread/sleep (* 5 1000.0))
      )
    [ (load-sample (clojure.java.io/resource "hihat-open.wav"))
      (load-sample (clojure.java.io/resource "kick.wav"))
      (load-sample (clojure.java.io/resource "snare-chili.wav"))]
    ['hh 'bd 'sn])))


(comment
  ;; stop all evolution processes
  (reset! *evolution* false)
  )
