(ns frankentone.examples.genetic.defgen
  (:use [frankentone live]))

(instruments->dsp!)


(defpat tst88 [#{40.751 45.739}] 
	:duration 4.0 :instrument :vl)
	
(defpat tst90 [(shuffle [vl bd bd hh]) sn [bd]])


(setVolume (:blub @instruments) 0.5)

(start tst90)
(start tst88)
(stop tst88)
(stop tst90)

(next-gen gbd)
(next-gen gsn)
(next-gen ghh)
(next-gen gvl)
(next-gen gmg)

(stop-auto gbd)
(stop-auto gvl)

(start-auto gbd :delay 100)
(start-auto ghh :delay 100)
(start-auto gvl :delay 100)

(defgen ghh 'hh (load-sample (clojure.java.io/resource "hihat-open.wav")) 
	(mul-sin (tanh (min x 1387.0946914781023)) (if>0 12.025591277372195 (cos (* 11260.143554536115 prev *sample-rate*)) 582.0211638510966)))
	
(defgen+ugens gbd 'bd (load-sample (clojure.java.io/resource "kick.wav"))
 (frankentone.ugens/sine (frankentone.ugens/delay-c -4.327505680752827 Math/PI x -0.9215224350650486) -3.9201090149492357))

(defgen+ugens gsn 'sn (load-sample (clojure.java.io/resource "snare-chili.wav")) 
	(+ (frankentone.ugens/pink-c) (frankentone.ugens/bpf-c -0.32626803834574947 Math/PI (min 4.692821647267513 197.64356934620585))))

(defgen+ugens gvl 'vl (load-sample 	"/Users/hb/Desktop/nasty-violin-4.wav") 
	(frankentone.ugens/lpf-c (flerp x Math/PI (* (frankentone.ugens/impulse-c *sample-rate* x -0.2650997136200133) 600.0410348445135)) (- 24.5715017794811) Math/PI))
	
(next-gen ghh)
