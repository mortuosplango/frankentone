(ns frankentone.examples.genetic.drums
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils dspgp]
        [frankentone utils ugens dsp instruments patterns samples]
        [overtone.music time])
  (:require clojure.java.io))


(defn pat [t]
  (play-pattern [
                 - - [hh - - -] - ||
;;                 40 - - - - ||
                 - sn - sn ||
                 bd - bd - ||
                 (repeat 5 bd)
                 ] 2.0 0.5 :default (/ t 1000.0))
  (let [next (+ t 2000)]
    (apply-at next #'pat [next])))

(pat (+ (now) 500))

(defn pat [_])



(reset-dsp! (let [prev (atom 0.0)]
              (fn [x chan] (if (zero? chan)
                            (reset! prev
                                    (mul-tanh
                                     5.0
                                     (+ 
                                      (sn x)
                                      (bd x)
                                      (hh x)
                                      (default x))))
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
        len (/ (count sample) *sample-rate*)]
    (swap! note-kernels
           assoc name
           (fn [freq amp dur]
             (fn ^double [x]
               (if (< x len)
                 (* amp (instfn x))
                 0.0)))))
  (Thread/sleep 5000))


;;;; Run it 
(def evol
  (doall
   (map
    (fn [sample name]
      ;; init instruments
      (eval (conj '((fn [x y z] (fn [x] 0.0)))
                  (symbol name) 'definst))
      (future (evolve 100 (memoize
                           (partial error-fn (get-reference-map
                                              sample)))
                      :best-callback (partial best-callback
                                              (keyword name) sample)
                      :terminals dsp-terminals
                      :functions dsp-functions)))
    [ (load-sample (clojure.java.io/resource "hihat-open.wav"))
      (load-sample (clojure.java.io/resource "kick.wav"))
      (load-sample (clojure.java.io/resource "snare-chili.wav"))]
    ['hh 'bd 'sn])))


(comment
  ;; stop all evolution processes
  (reset! *evolution* false)
  )
