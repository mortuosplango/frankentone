(ns frankentone.api
  (:use [frankentone utils]
        [overtone.helpers.ns])
  (:require clojure.stacktrace
            clojure.repl
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
            
            frankentone.genetic.analysis
            frankentone.genetic.dspgp
            frankentone.genetic.simplegp
            frankentone.genetic.simplegpfunctions
            frankentone.genetic.utils
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
                clojure.repl
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
                
                frankentone.genetic.analysis
                frankentone.genetic.dspgp
                frankentone.genetic.simplegp
                frankentone.genetic.simplegpfunctions
                frankentone.genetic.utils
                frankentone.genetic.dspgp

                overtone.algo.chance
                overtone.algo.scaling
                overtone.algo.trig

                overtone.music.time
                overtone.music.pitch
                overtone.music.rhythm
                overtone.music.tuning))))
