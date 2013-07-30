(ns frankentone.genetic.utils
  (:use [frankentone dsp utils]))

(def ^:dynamic buffer
  (double-array *sample-rate* 0.0))

(defn program->fn
  "Converts a program into a runable function."
  [program]
  (eval (list 'fn ^Double '[^Double x ^Double prev] program)))

(defn program->dsp!
  "Try to set the given program as dsp function."
  ([program]
     (reset-dsp!
      (binding [buffer (double-array *sample-rate* 0.0)]
        (let [
              val-func (program->fn program)
              prev-samp (atom 0.0)]
          (fn ^Double [^Double x ^Long chan]
            (if (zero? chan)
              (swap! prev-samp #(val-func x %))
              @prev-samp))))))
  ([program offset-p]
     (reset-dsp!
      (binding [buffer (double-array *sample-rate* 0.0)]
        (let [
              offset @current-time
              value-function (program->fn program)
              prev-samp (atom 0.0)]
          (fn ^Double [^Double x ^Long chan]
            (if (zero? chan)
              (do
                (swap! prev-samp #(value-function (- x offset) %)))
              @prev-samp)))))))
