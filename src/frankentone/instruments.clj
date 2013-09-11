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


(deftype InstNote
    [scheduled-time
     note]
  java.lang.Comparable
  (compareTo [this obj]
    (compare scheduled-time
             (.scheduled-time ^InstNote obj))))


(defprotocol PInstrument
  "An Instrument"
  (clear [this])
  (clear-queue [this])
  (kill-note [this id])
  (new-note [this start-time freq amp dur])
  (play [this time])
  (setFunction [this function])
  (getFunction [this])
  (setNoteKernel [this function])
  (getNoteKernel [this]))


(defn reduce-instruments ^double [time coll]
  (reduce-kv (fn ^double [val _ func] (+ val (func time))) 0.0 coll))


(defn note-due? [note-starts time]
  (some-> ^InstNote (.peek ^PriorityBlockingQueue note-starts)
          .scheduled-time
          (<= time)))


(defn kernel-good? [note-kernel]
  (if-not (fn? note-kernel)
    (do (println "Bad note kernel! Is not a function!")
        false)                             
    (let [test-output (try
                        ((note-kernel 440.0 1.0 2.0)
                         0.0)
                        (catch Exception e
                          (str "Caught exception: "
                               (.getMessage e))))]
      (if (has-bad-value? test-output)
        (do
          (if (string? test-output)
            (println "Bad note-kernel! "
                     test-output)
            (println "Bad note-kernel! "
                     "Function returns result of type "
                     (type test-output)))
          false)
        true))))


(deftype CInstrument
    [name
     ^PriorityBlockingQueue note-starts
     notes
     ^:volatile-mutable function
     ^:volatile-mutable note-kernel]

  PInstrument
  (clear [_] 
    (reset! notes {})
    (.clear note-starts))
  (play ^double [_ current-time] 
    (reduce-instruments
     current-time
     (if-not (note-due? note-starts current-time)
       @notes
       (swap! notes
              #(apply merge %
                      (.note ^InstNote (.poll note-starts))
                      (for [starts note-starts
                            :while (<= (.scheduled-time
                                        ^InstNote starts)
                                       current-time)]
                        (.note ^InstNote (.poll note-starts))))))))
  (clear-queue [_] (.clear note-starts))
  (kill-note [_ id] (swap! notes dissoc id))
  (new-note [this start-time freq amp dur]
    (let [new-id (keyword (gensym (str name "_")))
          kernel (note-kernel
                  freq amp dur)]
      (.put note-starts
            (InstNote. start-time
                       [new-id
                        (fn ^double [time]
                          (let [rel-time (- time start-time)]
                            (if (< rel-time dur)
                              (kernel rel-time)
                              (do
                                (swap! notes dissoc new-id)
                                0.0))))]))
      new-id))
  (setFunction [_ in-func] (set! function in-func))
  (getFunction [_] function)
  (setNoteKernel [_ in-func]
    (when (kernel-good? in-func)
      (set! note-kernel in-func)))
  (getNoteKernel [_] note-kernel))


(defn play-note
  "Plays a note at the specified time with the specified parameters"
  [time instrument frequency amplitude duration]
  (if-let [inst (get @instruments instrument)]
    (new-note inst
              time
              frequency
              amplitude
              duration)
    (println "no such instrument " instrument "!")))


(defmacro definst
  "Construct an instrument out of a note-kernel.

  The note-kernel is a function that takes frequency, amplitude and
  duration as arguments and returns a function that takes relative
  time and produces sample values."
  ([name note-kernel]
     `(if-let [inst# (get @instruments (keyword '~name))]
        (do (.setNoteKernel inst# ~note-kernel)
            (def ~name (.getFunction inst#)))
        (when (kernel-good? ~note-kernel)
          (let [instrument# (CInstrument. '~name
                                         (PriorityBlockingQueue.)
                                         (atom {})
                                         nil
                                         ~note-kernel)
                fn# (fn ^double [time#] (.play ^CInstrument instrument# time#))]
            (.setFunction instrument# fn#)
            (def ~name fn#)
            (swap! instruments
                   assoc (keyword '~name)
                   instrument#))))))


(definst default 
  (fn [freq amp dur]
    (let [lpf (lpf-c)
          saw1 (sawdpw-c 0.0)
          saw2 (sawdpw-c 0.0)
          saw3 (sawdpw-c 0.0)
          saw-freq-add2 (rrand 0.4)
          saw-freq-add3 (rrand 0.4)
          line (line-c (rrand 4000 5000) (rrand 2500 3200) 1.0)
          asr (asr-c 0.01 (max 0.0 (- dur 0.11)) 0.6 0.1)]
      (fn ^double [time]
        (+ (*
            amp
            (asr)
            (lpf
             (+ (saw1 0.3 freq)
                (saw2 0.3 (+ freq saw-freq-add2))
                (saw3 0.3 (+ freq saw-freq-add3)))
             (line)
             1.0)))))))

