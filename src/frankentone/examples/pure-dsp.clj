(ns frankentone.examples.pure-dsp
  (:use [frankentone utils dsp ugens]))

(reset-dsp! (fn [x chan]
              (* 0.1 (+ (@sine (nth [220 228] chan)
                              (mod x (nth [0.52 0.51] chan)))
                        (@sine (nth [442 445] chan)
                              (mod x (nth [0.2 0.58] chan)))))))


(reset-dsp!
 (let [pink (pink-c)
       prev (atom (double 0.0))]
  (fn [x chan]
    (let [clk  (* (mod x 2.0) 2.0)]
      (reset! prev
              (+
               (* (* 0.125 (mod clk 2.0))
                  (Math/sin (* clk TAU (nth [82 82] chan)))
                  (Math/sin (* clk TAU (nth [40 41] chan))))
               (Math/tanh (* clk 0.125 (- 1.0 (* 0.25 (mod clk 4)))
                             (Math/sin (* TAU (nth [321 333] chan)
                                          (Math/round (/ (mod x 10.0) 4.0))) )))
               (* 0.5 (mod clk 0.25)
                  (* (Math/sin (* clk TAU (nth [1882 1883.1] chan)))
                     (+ (pink)
                        (* 0.99 @prev))))
               (* (* 0.05 (- 4.0 clk))
                  (Math/tanh (Math/abs (+ (* 0.0 @prev)
                                          (Math/sin
                                           (* clk
                                              TAU
                                              (nth [20 80 30 50 60 90 170]
                                                   (/ (mod x 18.0) 3.0))
                                              (nth [30 40] chan)))))))))))))

