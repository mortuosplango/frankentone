(ns frankentone.examples.instruments
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.instruments))


(definst old-blub (fn [freq amp dur]
                (let [osc (sin-osc-c 0.0)
                      asr (asr-c 0.01 0.1 0.5 dur)
                      square (square-c 0.0)
                      lpf (lpf-c)
                      osc-b (sin-osc-c 0.0)]
                  (fn [time]
                    (*
                     (asr)
                     (+ (lpf
                         (square amp freq)
                         (+ (* freq 5.0)
                            (osc-b (* freq 2.0) 4.0)) 1.5)))))))

(definst blub (fn [freq amp dur]
                (let [osc (sin-osc-c 0.0)
                      asr (asr-c 0.02 0.2 1.0 (max 0.1 (- dur 0.2)))
                      saw (saw-c 0.0)
                      saw1 (saw-c 0.1)
                      lpf (lpf-c)
                      ffreq (rrand 2.0 3.0)
                      dfreq (rrand 2.0 1.0)
                      osc-b (sin-osc-c 0.0)]
                  (fn [time]
                    (* amp
                       (Math/tanh
                        (*
                         (asr)
                         (lpf
                          (+
                           (saw1 2.0 (+ dfreq freq))
                           (saw 2.0 freq))
                          (+ 6000.0
                             (osc-b 2000.0 ffreq)) 1.5))))))))


(definst hh (fn [freq amp dur]
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
                                         (+ 11000.0
                                            (* env 5500.0)) 1.5)
                                    lfreq 1.0))))))))

(definst chh (fn [freq amp dur]
                 (let [
                       hpf (hpf-c)
                       lpf (lpf-c)
                       pink (pink-c)
                       lfreq (rrand 1400.0 1800.0)
                       asr (asr-c 0.001 0.0 1.0 dur)
                       ]
                   (fn [_]
                     (let [env (asr)]
                       (*
                        env
                        3.0
                        (* amp (hpf (lpf (pink)
                                         (+ 8000.0
                                            (* env 3500.0)) 0.5)
                                    lfreq 1.0))))))))


(definst sn (fn [freq amp dur]
              (let [
                    hpf (hpf-c)
                    lpf (lpf-c)
                    osc (sin-osc-c 0.0)
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


(definst bd (fn [freq amp dur]
                (let [
                      osc_a (sin-osc-c 1.4)
                      drop (line-c 3.0 1.0 0.1)
                      asr (asr-c 0.001 0.05 1.0 dur)
                      ]
                  (fn [_]
                    (*
                     (asr)
                     amp
                     (Math/tanh (osc_a 3.0 (* 50 (drop)))))))))
