(ns frankentone.examples.getting-started
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.patterns
        frankentone.instruments
        frankentone.examples.instruments
        overtone.music.time
        clojure.repl))

;; start the engines
(start-dsp)

;; by default, the dsp function will produce pink noise
;; stop them again
(stop-dsp)

;; restart
(start-dsp)

;; replace pink noise with a sine wave
(sine!)

;; or white noise
(white-noise!)

;; or 4'33"
(silence!)

;; to reset the dsp function to a custom function
;; you'd use reset-dsp!

;; some lovely clipnoise:
(reset-dsp! (fn [time chan]
	(* 0.1 (rand-nth [-1.0 1.0]))))


;; write your own saw wave
(reset-dsp! 
	(fn [time channel]
	(if (zero? channel) 
		(* (mod time 0.010) 90.0)
		(* (mod time 0.0101) 90.0))))

;; abstract the saw
(defn saw [x freq] 
	(let [modu (/ 1.0 freq)]
		(* (mod x modu) freq)))

;; another way to use the channel argument
(reset-dsp! 
	(fn [x chan]
		(saw x (nth [90 91] chan))))

;; use the provided ugens to filter your custom dsp code
(reset-dsp! 
	(let [lowpass (lpf-c)]
		(fn [x chan]
			(lowpass
				(saw x (nth [80 80.1] chan))
				440
				1.0))))

;; let's play an instrument instead!
(reset-dsp! 
 (let [prev (atom 0.0)] 
   (fn [x chan] 
     (if (zero? chan) 
       (reset! prev (default x))
       @prev))))

(play-note (nows) :default 440 0.5 2.0)

;; play a pattern
(play-pattern [60 61 62 64])

;; add an offset to avoid wonkyness
(play-pattern [60 61 62 67] 2.0 0.5)

;; add a rest
(play-pattern [60 - 62 64 68] 2.0 0.5)

;; add a second layer
(play-pattern [60 - 62 64 68 :|
				36 38] 2.0 0.5)

;; want to play a different instrument?
;; first write your own instrument
(definst my-inst 
	(fn [freq amp dur]
        (fn [time]
           (* amp (mod time 1.0) (saw time freq )))))

;; add instrument to dsp function
(reset-dsp!
 (let [prev (atom 0.0)]
   (fn [x chan]
     (if (zero? chan) 
       (reset! prev (+ (my-inst x) (default x)))
       @prev))))

;; and then play it
(play-pattern [60 - 62 64 68 :|
				36 38] 2.0 0.5 :my-inst)

