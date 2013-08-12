;; Heavily inspired by Alex McLean's Tidal.

(ns frankentone.patterns
  (:use frankentone.instruments
        frankentone.utils
        overtone.music.pitch))


(declare play-pattern)


(def || :|)


(defn- do-play-pattern [coll length offset instrument now]
  (let [len (count coll)
        note-length (* length (/ 1 len))]
    (doall (mapv
            (fn [inst beat]
              (cond
               (keyword? inst)
               (play-note (+ now offset 0.1
                             (* beat note-length))
                          inst 80 0.1 note-length)
               (coll? inst)
               (play-pattern inst note-length
                             (+ offset (* beat note-length))
                             instrument
                             now)
               (seq (filter #(= (.function (val %)) inst) @instruments))
               (play-note (+ now offset 0.1
                             (* beat note-length))
                          (key (first
                                (filter
                                 #(= (.function (val %)) inst) @instruments)))
                          80 0.1 note-length)
               (number? inst)
                (play-note (+ now offset 0.1
                              (* beat note-length))
                           instrument (midi->hz inst) 0.2 note-length)
                  ))
            coll (range len)))))


(defn play-pattern
  "Heavily inspired by Alex McLean's Tidal.

  Plays a given collection with a given cycle length in seconds and a
  given time offset from (nows).

  The item count in the collection corresponds to the length of the
  individual objects. E. g. to play two basedrum sounds with 1/2 cycle
  length after each other: [:bd :bd]
ö
  To separate multiple voices, you can use || or :|

  [bd - || hh hh hh]

  If the object is not an instrument function, a keyword, || or a
  number, it will be treated as a break."
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
                          (partition-by #(= % :|) coll))))))

