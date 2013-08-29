(ns frankentone.examples.bitwise
  (:use frankentone.dsp
        frankentone.ugens))

;; 8-bit magic
;; http://doc.sccode.org/Guides/News-3_5.html#Bitwise%20ops
(start-dsp)

(reset-dsp!
 (let [prev (atom 0.0)
       mul (* 3.0 (/ 1.0 126.0))
       pc (pulse-count-c)
       impulse (impulse-c 0.0)
       hpf [(hpf-c) (hpf-c)]]
   (fn [x chan]
     (when (zero? chan)
       (reset!
        prev
        (long (pc (impulse 1.0 8000.0) 0.0))))
     (let [t @prev]
       (Math/tanh
        ((nth hpf chan)
         (*
          mul
          (mod (-
                (bit-or
                 (bit-and (* t 15) (bit-shift-right t 5))
                 (bit-and (* t 5) (bit-shift-right t (nth [3 4] chan)))
                 (bit-and (* t 2) (bit-shift-right t 9))
                 (bit-and (* t 8) (bit-shift-right t 11))
                 ) 3) 256))
         20
         1.0))))))

(stop-dsp)
