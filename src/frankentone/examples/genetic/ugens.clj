(ns frankentone.examples.genetic.ugens
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils dspgp mfcc]
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
                 (repeat 4 [- hh]) ||
                 ;;                 40 - - - - ||
             ;;    - - hh ||
                 [sn sn] - - ||
 ;;               - sn - sn ||
;;                 bd - bd - ||
                 (repeat 4 [bd -])
                 ] 2.0 0.5 :default (/ t 1000.0))
  (let [next (+ t 2000)]
    (apply-at next #'pat [next])))

(pat (+ (now) 500))

(defn pat [_])


(def ugen-functions
  (conj random-functions
        { :fn 'lpf-c :arity 3 }
        { :fn 'hpf-c :arity 3 }
        { :fn 'apf-c :arity 3 }
        { :fn 'bpf-c :arity 3 }
        { :fn 'line-c :arity 3 }
        { :fn 'asr-c :arity 4 }
        { :fn 'pink-c :arity 0}
        { :fn 'white-noise-c :arity 0}
        { :fn 'lerp :arity 3 }
        { :fn 'delay-c :arity 4 }
        { :fn 'osc-c :arity 3 }
        { :fn 'sawdpw-c :arity 3 }
        { :fn 'pulsedpw-c :arity 3 }
        { :fn 'impulse-c :arity 3 }
        ))


(def ugen-terminals
  (concat dsp-terminals
           '('prev
             'TAU
             (exp-rand 20.0 20000.0)
             (exp-rand 1.0 30.0)
             '*sample-rate*)))


(reset-dsp! (let [prev (atom 0.0)
                  lpf (lpf-c)]
              (fn [x chan] (if (zero? chan)
                            (reset! prev
                                    (lpf (mul-tanh
                                          5.0
                                          (+ 
                                           (sn x)
                                           (bd x)
                                           (hh x)
                                           (default x)))
                                         14000.0
                                         1.0))
                            @prev))))

;; (start-dsp)
;; (stop-dsp)


(defn ugen-program->fn
  "Converts a program into a runable function."
  [program]
  (eval (list 'fn-c ^Double '[^Double x ^Double prev] program)))


(defn best-callback
  [name sample best best-error]
  (let [instfn
        (let [
              val-func (ugen-program->fn best)
              prev-samp (atom 0.0)]
          (fn ^Double [x]
            (swap! prev-samp #(val-func x %))))
        len (/ (count sample) *sample-rate*)]
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
      (future
        (evolve 200 (memoize
                     (partial error-fn
                              (get-reference-map sample)
                              [:spf :mfcc]
                              ugen-program->fn))
                :best-callback (partial best-callback
                                        (keyword name) sample)
                :terminals ugen-terminals
                :functions ugen-functions))
      (Thread/sleep (* 5 1000.0))
      )
    [ (load-sample (clojure.java.io/resource "hihat.wav"))
      (load-sample (clojure.java.io/resource "kick.wav"))
      (load-sample (clojure.java.io/resource "snare-chili.wav"))]
    ['hh 'bd 'sn])))


(comment
  ;; stop all evolution processes
  (reset! *evolution* false)
  )
