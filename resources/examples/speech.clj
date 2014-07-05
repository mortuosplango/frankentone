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
  (fn [freq amp dur & {:keys [string] :or {string "fail!"}}]
    (let [spcc (mem-render-string (str string))
          factor (* (/ freq 440.0) 16000.0)
          limit (double (dec (count spcc)))
          len (/ limit factor)]
      (fn [x]
        (if (< x len)
          (* amp (aget ^doubles spcc (min limit (* x factor))))
          0.0)))))

;; add it to the dsp function
(reset-dsp!
 (let [samp (atom 0.0)]
   (fn ^Double [x chan]
     (if (zero? chan)
       (reset! samp (speak x))
       @samp))))

;; or use instruments->dsp! to play all instruments
(instruments->dsp!)

;; start the dsp process
(start-dsp)

;; test if it's working
(play-note (+ 0.1 (nows)) :speak 440.0 0.2 2.0 :string "hello")


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

;; stop the pattern
(defn pat [_])


;; use defpat
(instruments->dsp!)

(defpat pat77 ["hello" "how" "are" "you?" :| 
               :bd - - :hh - - :bd - :|
               (repeat 2 [- - - :sn])] :instrument :speak)

;; start the pattern
(start pat77)

;; stop it again
(stop pat77)

(stop-dsp)
