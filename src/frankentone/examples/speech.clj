(ns frankentone.examples.speech
  (:use [frankentone speech utils dsp instruments patterns ugens]
        [overtone.music time]))

;; first start MARY 4.3 server

(definst speak
  (fn [freq amp dur]
    (let [
          spcc (mem-render-string (str freq))
          limit (double (dec (count spcc)))
          len (/ limit 16000.0)]
      (fn [x]
        (if (< x len)
          (* amp (aget ^doubles spcc (min limit (* x 16000.0))))
          0.0)))))


(reset-dsp!
 (let [samp (atom 0.0)]
   (fn ^Double [x chan]
     (if (zero? chan)
       (reset! samp (speak x))
       @samp))))


(start-dsp)

(play-note (+ 0.1 (nows)) :speak "test" 0.2 2.0)

(defn pat [t]
  (play-pattern [
                 "test" "tes" "t" ||
                 ]
                2.0 0.1 :speak)
  (let [next-t (+ t 2000)]
    (apply-at next-t  #'pat [next-t])))

(pat (+ 1000 (now)))

(defn pat [_])


(stop-dsp)
