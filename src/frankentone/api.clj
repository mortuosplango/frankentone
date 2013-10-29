(ns frankentone.api
  (:use [overtone.helpers.ns])
  (:require clojure.stacktrace
            frankentone.dsp
            frankentone.instruments
            frankentone.patterns
            frankentone.ugens
            frankentone.utils
            frankentone.samples
            frankentone.speech

            frankentone.gui.scope

            frankentone.examples.instruments

            frankentone.entropy.entropy
            frankentone.entropy.selfmod

            frankentone.genetic.utils
            frankentone.genetic.analysis
            frankentone.genetic.simplegp
            frankentone.genetic.simplegp-functions
            frankentone.genetic.dspgp

            overtone.algo.chance
            overtone.algo.scaling
            overtone.algo.trig

            overtone.music.time
            overtone.music.pitch
            overtone.music.rhythm
            overtone.music.tuning))

(defn immigrate-frankentone-api []
  (apply immigrate
         (map symbol
              '(
                frankentone.dsp
                frankentone.instruments
                frankentone.patterns
                frankentone.ugens
                frankentone.utils
                frankentone.samples
                frankentone.speech

                frankentone.gui.scope

                frankentone.examples.instruments

                frankentone.entropy.entropy
                frankentone.entropy.selfmod

                frankentone.genetic.utils
                frankentone.genetic.analysis
                frankentone.genetic.simplegp
                frankentone.genetic.simplegp-functions
                frankentone.genetic.dspgp

                overtone.algo.chance
                overtone.algo.scaling
                overtone.algo.trig

                overtone.music.time
                overtone.music.pitch
                overtone.music.rhythm
                overtone.music.tuning))))
