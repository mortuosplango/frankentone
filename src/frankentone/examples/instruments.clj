(ns frankentone.examples.instruments
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.instruments))


(definst blub (fn [freq amp dur]
                (let [osc (osc-c 0.0)
                      asr (asr-c 0.01 0.1 0.5 dur)
                      square (square-c 0.0)
                      lpf (lpf-c)
                      osc-b (osc-c 0.0)]
                  (fn [time]
                    (*
                     (asr)
                     (+ (lpf
                         (square amp freq)
                         (+ (* freq 5.0)
                            (osc-b (* freq 2.0) 4.0)) 1.5)))))))


(definst hihat (fn [freq amp dur]
                 (let [
                       hpf (hpf-c)
                       lpf (lpf-c)
                       pink (pink-c)
                       lfreq (rrand 4000.0 8000.0)
                       asr (asr-c 0.001 0.0 1.0 dur)
                       ]
                   (fn [_]
                     (let [env (asr)]
                       (*
                        env
                        3.0
                        (* amp (hpf (lpf (pink)
                                         (+ 13000.0
                                            (* env 3500.0)) 3.5)
                                    lfreq 4.0))))))))


(definst sn (fn [freq amp dur]
              (let [
                    hpf (hpf-c)
                    lpf (lpf-c)
                    osc (osc-c 0.0)
                    asr (asr-c 0.01 0.0 1.0 dur)]
                (fn [_]
                  (let [env (asr)]
                    (*
                     env
                     3.0
                     (osc 2.0 230)
                     (* amp (hpf (lpf (white-noise) (+ 200.0
                                                       (* env 3500.0)) 1.5)
                                 30.0 1.0))))))))


(definst kick (fn [freq amp dur]
                (let [
                      osc_a (osc-c 1.4)
                      drop (line-c 3.0 1.0 0.1)
                      asr (asr-c 0.001 0.05 1.0 dur)
                      ]
                  (fn [_]
                    (*
                     (asr)
                     amp
                     (Math/tanh (osc_a 3.0 (* 50 (drop)))))))))
