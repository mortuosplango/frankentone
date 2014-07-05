;; Heavily inspired by Alex McLean's Tidal.

(ns frankentone.patterns
  (:use [frankentone instruments utils]
        [frankentone.entropy selfmod]
        [clojure walk]
        [overtone.music pitch time rhythm]))


(declare play-pattern)

(def tempoclock (metronome 128))

(def || :|)

(defn- max-len
  "Finds the length of the longest list in the collection"
  [coll]
  (-> (sort-by (comp count val) >
               (into {}
                     (filter (comp coll? val) coll)))
      first val count))

(defn- inst?->inst
  [x]
  (if (keyword? x)
    x
    (when-let [inst (seq (filter #(= (getFunction ^PInstrument2 (val %)) x) @instruments))]
      (key (first inst)))))


(defn- do-play-pattern
  [coll length offset default-instrument now default-amp default-freq]
  (let [len (count coll)
        note-length (* length (/ 1 len))
        known-keys [:inst :freq :pitch :amp :sustain]]
    (println "do play" coll "len" len "note-length" length)
    (mapv
     (fn [inst beat]
       (let [start-time (+ now offset (* beat note-length))]
         (cond
          (keyword? (inst?->inst inst))
          (play-note start-time
                     inst
                     default-freq default-amp note-length)

          (number? inst)
          (play-note start-time
                     default-instrument (midi->hz inst) default-amp note-length)
          
          (string? inst)
          ;; send the string as optional argument to the
          ;; default synth e. g. for speech synthesis
          (play-note start-time
                     default-instrument default-freq default-amp note-length
                     :string inst)

          ;; if event-style map
          (map? inst)
          ;; if map contains seqs split the map into
          ;; submaps and play-pattern those
          (if (some #(coll? (get inst %)) (keys inst))
            (let [max-len (max-len inst)]
              (play-pattern
               (map (fn [pos]
                      (into {}
                            (map #(when-let [v (get inst %)]
                                    (if (coll? v)
                                      ;; cycle through the others
                                      { % (nth (cycle v) pos) }
                                      { % v }))
                                 (keys inst))))
                    (range max-len))
               note-length (+ offset (* beat note-length))
               default-instrument now default-amp default-freq))
            ;; play only if the values of all known keys except
            ;; :inst are numbers or strings
            ;; i. e. everything else causes a break
            (when-not (some #(not (or (string? %) (number? %)))
                            (vals (select-keys inst (rest known-keys))))
              (apply play-note start-time
                     (or (inst?->inst (:inst inst))
                         default-instrument)
                     (if (contains? inst :pitch)
                       (midi->hz (:pitch inst))
                       (or (:freq inst)
                           default-freq))
                     (or (:amp inst)
                         default-amp)
                     (or (:sustain inst)
                         note-length)
                     (apply dissoc inst known-keys))))
          
          (coll? inst)
          ;; if just collection
          (play-pattern inst note-length
                        (+ offset (* beat note-length))
                        default-instrument
                        now default-amp default-freq))))
     coll (range len))))


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
  ([coll length offset default-instrument]
     (play-pattern coll length offset default-instrument (nows)))
  ([coll length offset default-instrument now]
     (play-pattern coll length offset default-instrument now 0.1 440.0))
  ([coll length offset default-instrument now default-amp default-freq]
     (doall (mapv #(do-play-pattern % length offset default-instrument now
                                    default-amp default-freq)
                  (if (map? coll)
                    [coll]
                    (remove #(= (first %) :|)
                            (partition-by #(= % :|)
                                          (if (set? coll)
                                            (interpose || coll)
                                            coll))))))))


(defprotocol Pattern ;; playable?
  (start [this])
  (stop [this]))


(deftype tPattern
    [pat-name
     pattern-fn
     instrument
     duration
     amp
     freq
     running?
     quant]
  clojure.lang.IFn
  (invoke [this t]
    (play-pattern (pattern-fn)
                  (* duration (/ 60.0 (metro-bpm tempoclock)))
                  *latency* instrument
                  (/ (tempoclock t) 1000.0)
                  amp freq)
    (println pat-name t @running?)
    (when @running?
      (let [next-t (+ t duration)]
        (apply-at (tempoclock next-t) pat-name [next-t]))))
  Pattern
  (start [this]
    (when-not @running?
      (reset! running? true)
      (if quant
        (this (- (tempoclock) (mod (tempoclock) quant) (* quant -2)))
        (this (inc (tempoclock))))))
  (stop [this]
    (reset! running? false)))


(defmacro defpat
  ([name pattern &{ :keys [duration
                           instrument
                           quant
                           amp
                           freq]
                   :or { duration 4
                        instrument :default
                        quant 4
                        amp 0.1
                        freq 440.0}}]
     `(when (try
              ;; check if pattern works
              ~pattern
              (catch Exception e#
                (println "caught exception:\n"
                         (.getMessage e#))
                false))
        (def ~name
          (tPattern.
           #'~name
           (frankentone.entropy.entropy/fn->fntropy
            ~(str "defpat " name " ")
            [] ~pattern false
            (make-selfmod false :body-pos 2))
           ~instrument
           ~duration
           ~amp
           ~freq
           (atom (if
                     (= (class ~name) tPattern)
                   @(.running? ~name)
                   false))
           ~quant)))))
