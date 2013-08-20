(ns frankentone.genetic.dspgp
  (:use [frankentone.genetic analysis simplegp simplegpfunctions utils]
        [frankentone utils ugens dsp instruments patterns]))


(defn bset!
  "Set the buffer at second x to value y. Returns the former value at
  x.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x y]
  (let [pos (* (pmod x 1.0) (dec *sample-rate*))
        previous-value (aget ^doubles buffer pos)]
    (aset ^doubles buffer pos ^double y)
    previous-value))


(defn bget
  "Get the value at second x in the buffer.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x] (aget ^doubles buffer (* (pmod x 1.0) (dec *sample-rate*))))


(def dsp-terminals
  (concat random-terminals
           '('prev
             'TAU
             (exp-rand 20.0 20000.0)
             (exp-rand 1.0 30.0)
             '*sample-rate*)))


(def dsp-functions
  (conj random-functions
        { :fn 'bset! :arity 2 }
        { :fn 'bget :arity 1 }))
