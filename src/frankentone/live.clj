(ns frankentone.live
  (:require [frankentone.api]))

(frankentone.api/immigrate-frankentone-api)

;; Redefine buffer (defined in frankentone.genetic.utils).
;;
;; Seems to be necessary as Clojure otherwise says it's not dynamic
;; though the metadata is correctly copied.

(def ^:dynamic buffer
  (double-array *sample-rate* 0.0))

