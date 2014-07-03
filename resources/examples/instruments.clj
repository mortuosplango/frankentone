(ns frankentone.examples.instruments
  (:use frankentone.dsp
        frankentone.ugens
        frankentone.utils
        frankentone.instruments
        overtone.music.pitch))


(definst old-blub (fn [freq amp dur]
                (let [osc (sin-osc-c 0.0)
                      asr (asr-c 0.01 0.1 0.5 (- dur 0.01 0.1))
                      square (pulsedpw-c 0.5)
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
                      asr (asr-c 0.02 0.2 1.0 (max 0.1 (- dur 0.22)))
                      saw (sawdpw-c 0.0)
                      saw1 (sawdpw-c 0.1)
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
                       lfreq (rrand (* freq 10) (* freq 20))
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
                       lfreq (rrand (* freq 3.5) (* freq 4.5))
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
                    white (white-noise-c)
                    asr (asr-c 0.01 0.0 1.0 dur)]
                (fn [_]
                  (let [env (asr)]
                    (*
                     env
                     3.0
                     (osc 2.0 230)
                     (* amp (hpf (lpf (white) (+ (* freq 0.5)
                                                 (* env 3500.0)) 1.5)
                                 30.0 1.0))))))))

(definst sn (fn [freq amp dur]
              (let [
                    hpf (hpf-c)
                    bpf (bpf-c)
                    lpf (lpf-c)
                    pls1 (pulsedpw-c 0.5)
                    pls2 (pulsedpw-c 0.5)
                    osc (sin-osc-c 0.8)
                    white (white-noise-c)
                    drop1 (line-c 110.0 59.0 0.005)
                    drop2 (line-c (/ freq 440.0) (/ freq (/ 440.0 0.8)) 0.1)
                    env2 (line-c 1.0 0.0 0.18 )
                    asr (asr-c 0.01 0.0 1.0 dur)]
                (fn [_]
                  (let [env (asr)
                        env1m (midi->hz (min 127.0 (* (drop1) (drop2))))]
                    (* amp
                       (hardclip
                        (* env
                           (+
                            (lpf (+ (pls1 0.5 env1m)
                                    (pls2 0.5 (* 1.6 env1m)))
                                 (* 1.6 env1m) 1)
                            (osc 1.0 env1m)
                            (* 6.0 (env2)
                               (bpf (hpf (* 0.2 (white)) 200.0 2.0)
                                    6900.0 1.0)))))))))))


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
                     (Math/tanh (osc_a 3.0 (* (* freq 0.11) (drop)))))))))


(definst bd (fn [freq amp dur]
              (let [
                    osc_a (sin-osc-c 1.4)
                    pls (pulsedpw-c 0.5)
                    white (white-noise-c)
                    lpf (lpf-c)
                    drop1 (line-c 110.0 59.0 0.005)
                    drop2 (line-c (/ freq 440.0) (/ freq 880.0) 0.029)
                    asr (asr-c 0.005 0.06 0.5 (- dur 0.065))
                    ]
                (fn [_]
                  (let [env1 (* (drop2) (drop1))
                        env1m (midi->hz (min 127.0 env1))]
                    (*
                     amp
                     (hardclip
                      (* (asr)
                         1.2
                         (+
                          (lpf (+ (white)
                                  (pls 1.0 env1m)) env1m 1)
                          (osc_a 1.0 env1m))))))))))
