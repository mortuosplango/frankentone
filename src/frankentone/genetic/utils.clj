(ns frankentone.genetic.utils
  (:use [frankentone dsp utils]))

(def ^:dynamic buffer
  (double-array *sample-rate* 0.0))

(defn program->fn
  "Converts a program into a runable function."
  [program]
  (eval (list 'fn ^Double '[^Double x ^Double prev] program)))

(defn program->fn-c
  "Converts a program into a runable function."
  [program]
  (try
    (eval (list 'fn-c ^Double '[^Double x ^Double prev] program))
    (catch Exception e
      ;; (println "caught exception:\n"
      ;;          (.getMessage e))
      (fn [_ _] 0.0))))

(defn program->dsp!
  "Try to set the given program as dsp function."
  ([program]
     (program->dsp! program false))
  ([program offset-p]
     (reset-dsp!
      (binding [buffer (double-array *sample-rate* 0.0)]
        (let [
              offset (if offset-p (double (current-dsp-time)) 0.0)
              value-function (program->fn program)
              prev-samp (atom 0.0)]
          (fn ^Double [^Double x ^Long chan]
            (if (zero? chan)
              (do
                (swap! prev-samp #(value-function (- x offset) %)))
              @prev-samp)))))))
