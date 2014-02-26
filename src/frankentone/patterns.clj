;; Heavily inspired by Alex McLean's Tidal.

(ns frankentone.patterns
  (:use [frankentone instruments utils]
        [frankentone.entropy selfmod]
        [clojure walk]
        [overtone.music pitch time]))


(declare play-pattern)


(def || :|)


(defn- do-play-pattern [coll length offset instrument now]
  (let [len (count coll)
        note-length (* length (/ 1 len))]
    (doall (mapv
            (fn [inst beat]
              (let [inst-fn (filter #(= (getFunction ^PInstrument2 (val %)) inst) @instruments)]
                (cond
                 (seq inst-fn)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            (key (first inst-fn))
                            80 0.1 note-length)
                 (keyword? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            inst 80 0.1 note-length)
                 (coll? inst)
                 (play-pattern inst note-length
                               (+ offset (* beat note-length))
                               instrument
                               now)
                 (number? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            instrument (midi->hz inst) 0.2 note-length)
                 (string? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            instrument inst 0.2 note-length)
                 )))
            coll (range len)))))


(defn play-pattern
  "Heavily inspired by Alex McLean's Tidal.

  Plays a given collection with a given cycle length in seconds and a
  given time offset from (nows).

  The item count in the collection corresponds to the length of the
  individual objects. E. g. to play two basedrum sounds with 1/2 cycle
  length after each other: [:bd :bd]

  To separate multiple voices, you can use || or :|

  [bd - || hh hh hh]

  Sets will also be played in parallel:

  #{[bd - ] [hh hh hh]}

  If an object is not a a collection, instrument function, keyword,
  :|, || or number, it will be treated as a break."
  ([coll]
     (play-pattern coll 2.0))
  ([coll length]
     (play-pattern coll length 0.0))
  ([coll length offset]
     (play-pattern coll length offset :default))
  ([coll length offset instrument]
     (play-pattern coll length offset instrument (nows)))
  ([coll length offset instrument now]
     (doall (mapv #(do-play-pattern % length offset instrument now)
                  (remove #(= (first %) :|)
                          (partition-by #(= % :|)
                                        (if (set? coll)
                                          (interpose || coll)
                                          coll)))))))


(defprotocol Pattern ;; playable?
  (start [this])
  (stop [this])
  (gui [this]))


(deftype tPattern
    [pat-name
     pattern-fn
     instrument
     duration
     running?]
  clojure.lang.IFn
  (invoke [this t]
    (play-pattern (pattern-fn)
                  duration *latency* instrument)
    (println pat-name)
    (when @running?
     (let [next-t (+ t (* duration 1000))]
       (apply-at next-t pat-name [next-t]))))
  Pattern
  (start [this]
    (when-not @running?
      (reset! running? true)
      (this (+ (now) *latency*))))
  (stop [this]
    (reset! running? false))
  (gui [this]))


(defmacro defpat
  ([name pattern &{ :keys [duration instrument]
                   :or {duration 2.0
                        instrument :default}}]
     `(def ~name
        (tPattern.
         #'~name
         (frankentone.entropy.entropy/fn->fntropy
          ~(str "defpat " name)
          [] ~pattern false
          (make-selfmod false :body-pos 2))
         ~instrument
         ~duration
         (atom (if
                   (= (class ~name) tPattern)
                 @(.running? ~name)
                 false))))))
