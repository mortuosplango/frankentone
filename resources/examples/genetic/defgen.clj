(ns frankentone.examples.genetic.defgen
  (:use [frankentone live]))

;; define some genetic processes and seed them
;; replace the seed with nil if you want to start the process without
(defgen ghh 'hh (load-sample (clojure.java.io/resource "hihat-open.wav")) 
	(mul-cos (mean TAU prev) (mean TAU (mean TAU prev))))
	
(defgen+ugens gbd 'bd (load-sample (clojure.java.io/resource "kick.wav"))
 (frankentone.ugens/sine (frankentone.ugens/delay-c (tanh (- TAU)) Math/PI (frankentone.ugens/bpf-c (tanh 8.39542351397461) prev prev) -0.9215224350650486) -3.9201090149492357))

(defgen+ugens gsn 'sn (load-sample (clojure.java.io/resource "snare-chili.wav")) 
	(frankentone.ugens/impulse-c 22.92067271872942 (min 599.4559957416726 *sample-rate*) -0.7079338992052633))

(defgen+ugens gvl 'vl (load-sample (clojure.java.io/resource "nasty-violin-4.wav")) 
	(tanh (min (frankentone.ugens/pulsedpw-c 42.824309546975826 (frankentone.ugens/pulsedpw-c (plfsaw Math/PI (pd 1089.385530174594 42.824309546975826)) (plfsaw (sin prev) *sample-rate*) 61.254611950597855) (rrand -1.3028660588130894 1089.385530174594)) Math/PI)))

;; play all instruments and start the dsp loop
(instruments->dsp!)
(start-dsp)

;; you can see a table of the individuals and their fitness:
(gui ghh)

;; define a rhythmic pattern with the above defined instruments
(defpat tst90 [(shuffle [vl bd bd hh]) sn [bd]])

;; start and stop it
(start tst90)
(stop tst90)


;; define another pattern with a chord in one of the instruments
(defpat tst88 [#{62.986 42.098 79.9}] 
	:duration 4.0 :instrument :vl)

(start tst88)
(stop tst88)


;; set the volume of an instrument
(setVolume (:vl @instruments) 0.5)

;; evolve the next generation for one or more of the genetic processes
(next-gen gbd)
(next-gen gsn)
(next-gen ghh)
(next-gen gvl)

;; start an auto-evolution 
;; (evolve one generation after the other with a delay of 100ms
;; in between the calculations)
(start-auto gbd :delay 100)
(start-auto ghh :delay 100)
(start-auto gvl :delay 100)

;; stop them again
(stop-auto gbd)
(stop-auto gvl)
