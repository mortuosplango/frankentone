(ns frankentone.genetic.hanning)

(defn hann-window [n size]
  (* 0.5
     (- 1 (Math/cos (/ (* 2 Math/PI n) (dec size))))))

