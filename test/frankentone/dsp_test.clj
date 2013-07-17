(ns frankentone.dsp-test
  (:use clojure.test
        frankentone.dsp))

(deftest reset-dsp!-test
  (is (= (reset-dsp! 0) false))
  (is (= (reset-dsp! (fn [x] 0.0)) false))
  (is (= (reset-dsp! (fn [x y] 0.0)) true))
  (is (= (reset-dsp! (fn [x y z] 0.0)) false))
  (is (= (reset-dsp! (fn [time chan] (if (zero? chan) 1.0))) nil))
  (is (= (reset-dsp! (fn [time chan] (if (zero? chan) 1))) java.lang.Long))
  (is (= (reset-dsp! (fn [time chan] (if (zero? chan) 1.0 0.0))) true)))

