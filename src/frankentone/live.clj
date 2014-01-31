(ns frankentone.live
  (:require [frankentone api]
            [clojure repl]))

(frankentone.api/immigrate-frankentone-api)

;; Redefine buffer, bset! and bget (defined in frankentone.genetic.dspgp).
;;
;; Seems to be necessary as Clojure otherwise says it's not dynamic
;; though the metadata is correctly copied.

(def ^:dynamic buffer
  (double-array *sample-rate* 0.0))


(defn bset!
  "Set the buffer at second x to value y. Returns the former value at
  x.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x y]
  (let [pos (* (pmod x 1.0) (dec *sample-rate*))
        previous-value (hiphip.double/aget buffer pos)]
    (hiphip.double/aset buffer pos ^double y)
    previous-value))


(defn bget
  "Get the value at second x in the buffer.

  As the buffer is just one second long, other values for x will be
  wrapped."
  ^double [x] (hiphip.double/aget buffer (* (pmod x 1.0) (dec *sample-rate*))))

