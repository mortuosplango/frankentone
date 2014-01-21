(ns frankentone.examples.speech
  (:use [frankentone speech utils dsp instruments patterns ugens]
        [overtone.music time]))

;; first start MARY 4.3 server
;; you can get MARY from here:
;; http://mary.dfki.de/Download/mary-4.3.0-released
;; after installing, you'll have a MARY TTS folder
;; in that, start bin/maryserver

;; when it's started, define the instrument
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

;; add it to the dsp function
(reset-dsp!
 (let [samp (atom 0.0)]
   (fn ^Double [x chan]
     (if (zero? chan)
       (reset! samp (speak x))
       @samp))))

;; start the dsp process
(start-dsp)

;; test if it's working
(play-note (+ 0.1 (nows)) :speak "test" 0.2 2.0)


;; play a pattern with it
(defn pat [t]
  (play-pattern [
                 "test" "tes" "t" ||
                 ]
                2.0 0.1 :speak)
  (let [next-t (+ t 2000)]
    (apply-at next-t  #'pat [next-t])))

(pat (+ 1000 (now)))

;; variation
(defn pat [t]
  (play-pattern [
                 (str (shuffle ["this " "is "]) 
                 	(rand-nth [", " " "]) 
                 	(rand-nth ["a" "another" "not another"])
                 	" test" 
                 	(rand-nth [":" "?" "!" "."])) ||
                 (repeat (rrand 3 38) "test") ||
                 (map str (range (rrand 1 12))) ||
                 ]
                2.0 0.1 :speak)
  (let [next-t (+ t 2000)]
    (apply-at next-t  #'pat [next-t])))


(defn pat [_])

(stop-dsp)
