;; The instrument system is based on the extempore instrument lib found here:
;; https://github.com/digego/extempore/blob/master/libs/core/instruments.xtm

(ns frankentone.instruments
  (:use frankentone.ugens
        frankentone.utils)
  (:import [java.util.concurrent PriorityBlockingQueue]
           [java.lang Comparable]))


(def instruments
  "Map of all registered instruments."
  (atom {}))


(def note-kernels
  "Map of all registered note-kernels."
  (atom {}))


(deftype InstNote
 ;;   "A note played on an instrument."
    [scheduled-time
     note]
  java.lang.Comparable
  (compareTo [this obj]
    (let [other-time (.scheduled-time ^InstNote obj)]
      (cond
       (< scheduled-time other-time) -1
       (= scheduled-time other-time) 0
       :else 1))))


(defprotocol IInstrument
  "An Instrument"
  (clear [this])
  (clear-queue [this])
  (kill-note [this id])
  (new-note [this start-time freq amp dur]))


(deftype Instrument
    [name
     ^PriorityBlockingQueue note-starts
     notes
     ^Long id
     function]

  IInstrument
  (clear [_] 
    (reset! notes {})
    (.clear note-starts))
  (clear-queue [_] (.clear note-starts))
  (kill-note [_ id] (swap! notes dissoc id))
  (new-note [this start-time freq amp dur]
    (let [new-id (keyword (str name "_"
                               (swap! id inc)))
          kernel (((keyword name) @note-kernels)
                                freq amp dur)]
      (.put note-starts
            (InstNote. start-time
                       [new-id
                        (fn ^Double [time]
                          (let [rel-time (- time start-time)]
                            (if (< rel-time dur)
                              (kernel rel-time)
                              (do
                                (swap! notes dissoc new-id)
                                0.0))))]))
      new-id)))


(defn play-note
  "Plays a note at the specified time with the specified parameters"
  [time instrument frequency amplitude duration]
  (if (contains? @instruments instrument)
    (let [instr (get @instruments instrument)]
      (new-note instr
                time
                frequency
                amplitude
                duration))
    (println "no such instrument " instrument "!")))


(defmacro definst
  "Construct an instrument out of a note-kernel.

  The note-kernel is a function that takes frequency, amplitude and
  duration as arguments and returns a function that takes relative
  time, frequency, amplitude and duration and produces sample values."
  ([name note-kernel]
     `(when (let [kernel# ~note-kernel]
              (if-not (fn? kernel#)
                (do (println "Bad note kernel! Is not a function!")
                    false)                             
                (let [test-output# (try
                                     ((kernel# 440.0 1.0 2.0)
                                      0.0)
                                     (catch Exception e#
                                       (str "Caught exception: "
                                            (.getMessage e#))))]
                  (if (has-bad-value? test-output#)
                    (do
                      (if (string? test-output#)
                        (println "Bad note-kernel! "
                                 test-output#)
                        (println "Bad note-kernel! "
                                 "Function returns result of type "
                                 (type test-output#)))
                      false)
                    true))))
        (when-not (get @instruments (keyword '~name))
          (let [id# (atom 0)
                note-starts# (PriorityBlockingQueue.)
                notes# (atom {})
                fn# (fn ^double [time#]
                      (while
                          (and
                           (not (.isEmpty note-starts#))
                           (<= (.scheduled-time ^InstNote (.peek note-starts#))
                               time#))
                        (swap! notes#
                               merge
                               (.note ^InstNote (.poll note-starts#))))
                      (reduce-kv (fn ^double [val# _# item#]
                                   (+ val# (item# time#)))
                                 0.0 @notes#))
                instrument# (Instrument. '~name
                                         note-starts#
                                         notes#
                                         id#
                                         fn#)]
            (def ~name fn#)
            (swap! instruments
                   assoc (keyword '~name)
                   instrument#)))
        (swap! note-kernels assoc (keyword '~name) ~note-kernel))))


(definst default 
  (fn [freq amp dur]
    (let [lpf (lpf-c)
          saw1 (saw-c 0.0)
          saw2 (saw-c 0.0)
          saw3 (saw-c 0.0)
          saw-freq-add2 (rrand 0.4)
          saw-freq-add3 (rrand 0.4)
          line (line-c (rrand 4000 5000) (rrand 2500 3200) 1.0)
          asr (asr-c 0.01 (max 0.0 (- dur 0.11)) 0.6 0.1)]
      (fn [time]
        (+ (*
          amp
          (asr)
          (lpf
           (+ (saw1 0.3 freq)
              (saw2 0.3 (+ freq saw-freq-add2))
              (saw3 0.3 (+ freq saw-freq-add3)))
           (line)
           1.0)))))))

