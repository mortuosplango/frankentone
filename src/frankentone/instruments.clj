;; The instrument system is based on the extempore instrument lib found here:
;; https://github.com/digego/extempore/blob/master/libs/core/instruments.xtm

(ns frankentone.instruments
  (:use frankentone.ugens)
  (:use frankentone.utils)
  (:import [java.util.concurrent PriorityBlockingQueue])
  (:import [java.lang Comparable]))


(def instruments
  "Map of all registered instruments."
  (atom {}))


(defprotocol IInstNote
  "A note played on an instrument."
  (scheduled-time [this])
  (note [this]))


(defn make-note [time new-note]
  (reify
    java.lang.Comparable
    (compareTo [this obj]
      (let [other-time (scheduled-time obj)]
        (if (< time other-time)
          -1
          (if (= time other-time)
            0
            1))))
    IInstNote
    (scheduled-time [this] time)
    (note [this] new-note)) )


(defprotocol IInstrument
  "An Instrument"
  (clear [this])
  (clear-queue [this])
  (kill-note [this id])
  (note_c [this start-time
           freq amp dur kernel id])
  (new-note [this start-time freq amp dur]))


(deftype Instrument
    [name
     note-starts
     notes
     note_kernel
     id]
  clojure.lang.IFn
  (invoke [_ time]
    (while
        (and
         (not (.isEmpty note-starts))
         (<= (scheduled-time (.peek note-starts)) time))
      (println (swap! notes
              merge
              (note (.poll note-starts)))))
    (reduce-kv (fn [^Double prev _ func]
                 (+ prev (func time))) 0.0 @notes))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args))
  
  IInstrument
  (clear [_] 
    (reset! notes {})
    (.clear note-starts))
  (clear-queue [_] (.clear note-starts))
  (kill-note [_ id] (swap! notes dissoc id) )
  (note_c [this start-time
           freq amp dur kernel id]
                   (fn ^Double [^Double time]
                     (let [rel-time (- time start-time)]
                       (if (< rel-time dur)
                         (kernel rel-time freq amp dur)
                         (do
                           (println (kill-note this id))
                           0.0)))))
  (new-note [this start-time freq amp dur]
    (let [new-id (keyword (str name "_"
                               (swap! id inc)))]
      (.put note-starts
            (make-note start-time
                       [new-id
                        (note_c this start-time
                                freq amp dur
                                (eval note_kernel)
                                new-id)]))
      [new-id @notes note-starts])))


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
  "Construct an instrument out of a note_kernel."
  [name note-kernel]
  `(let [
         old-inst# (get @instruments (keyword '~name))
         id# (atom (if old-inst#
                     @(.id old-inst#)
                     0))
         note-starts#  (if old-inst#
                         (PriorityBlockingQueue.
                          (.note-starts old-inst#))
                         (PriorityBlockingQueue.))
         notes# (atom (if old-inst#
                        @(.notes old-inst#)
                        {}))
         instrument# (Instrument. '~name
                                  note-starts#
                                  notes#
                                  '~note-kernel
                                  id#)]
     (intern 'frankentone.instruments
             (symbol (str '~name)) instrument#)
     (swap! instruments
            assoc (keyword '~name)
            instrument#)))


(definst default 
  (let [lpf (lpf_c)
        saws [ (saw_c 0.0)
               (saw_c 0.0)
               (saw_c 0.0)]
        saw-freq-adds [0 (rrand -0.4 0.0) (rrand -0.4 0.0)]
        line (line_c (rrand 4000 5000) (rrand 2500 3200) 1.0)
        asr (asr_c 0.01 0.2 0.7 0.3)
        samp (atom 0.0)
        ]
    (fn [^Double time ^Double freq ^Double amp ^Double dur]
      (*
       (asr)
       (lpf (reduce + (mapv (fn [saw freq-add]
                              (saw 0.3 (+ freq freq-add)))
                            saws
                            saw-freq-adds))
            (line)
            1.0)))))

