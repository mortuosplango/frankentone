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


(defn- do-play-pattern [coll length offset default-instrument now default-amp default-pitch]
  (let [len (count coll)
        note-length (* length (/ 1 len))
        known-keys [:inst :freq :pitch :amp :sustain]]
    (doall (mapv
            (fn [inst beat]
              (let [inst (if (var? inst) @inst inst)
                    inst-fn (filter #(= (getFunction ^PInstrument2 (val %)) inst) @instruments)
                    start-time (+ now offset (* beat note-length))]
                (cond
                 (seq inst-fn)
                 (play-note start-time
                            (key (first inst-fn))
                            default-pitch default-amp note-length)

                 (keyword? inst)
                 (play-note start-time
                            inst
                            default-pitch default-amp note-length)

                 (number? inst)
                 (play-note start-time
                            default-instrument (midi->hz inst) default-amp note-length)
                 
                 (string? inst)
                 ;; use the string as freq argument to the synth for
                 ;; speech synthesis
                 (play-note start-time
                            default-instrument inst default-amp note-length)
                 
                 (coll? inst)
                 ;; if event-style map
                 (if (and
                      (map? inst)
                      (some #(contains? inst %) known-keys))
                   ;; reject unknown keys
                   (let [inst (into {} (map #(when-let [v (get inst %)] {% v}) known-keys))]
                     ;; if map contains seqs split the map into submaps and play-pattern those
                     (if (some #(coll? (get inst %)) known-keys)
                       (let [max-len (max-len inst)]
                         (play-pattern
                          (map (fn [pos]
                                 (into {}
                                       (map #(when-let [v (get inst %)]
                                               (if (coll? v)
                                                 ;; cycle through the others
                                                 { key (nth (cycle v) pos) }
                                                 { key v })) known-keys))) (range max-len))
                          note-length (+ offset (* beat note-length))
                          default-instrument now default-amp default-pitch))
                       ;; play only if the values of everything except
                       ;; :inst are numbers or strings
                       (when-not (some #(not (or (string? %) (number? %))) (vals (dissoc inst :inst)))
                         (play-note start-time
                                    (if (contains? inst :inst)
                                      (:inst inst)
                                      default-instrument)
                                    (cond
                                     (contains? inst :pitch) (midi->hz (:pitch inst))
                                     (contains? inst :freq)  (:freq inst)
                                     :default default-pitch)
                                    (if (contains? inst :amp)
                                      (:amp inst)
                                      default-amp)
                                    (if (contains? inst :sustain)
                                      (:sustain inst)
                                      note-length)))))
                   ;; if just collection
                   (play-pattern inst note-length
                                 (+ offset (* beat note-length))
                                 default-instrument
                                 now default-amp default-pitch))
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
  ([coll length offset default-instrument]
     (play-pattern coll length offset default-instrument (nows)))
  ([coll length offset default-instrument now]
     (play-pattern coll length offset default-instrument now 0.1 440.0))
  ([coll length offset default-instrument now default-amp default-pitch]
     (doall (mapv #(do-play-pattern % length offset default-instrument now
                                    default-amp default-pitch)
                  (remove #(= (first %) :|)
                          (partition-by #(= % :|)
                                        (if (set? coll)
                                          (interpose || coll)
                                          coll)))))))


(defprotocol Pattern ;; playable?
  (start [this])
  (stop [this]))


(deftype tPattern
    [pat-name
     pattern-fn
     instrument
     duration
     running?
     quant]
  clojure.lang.IFn
  (invoke [this t]
    (play-pattern (pattern-fn)
                  (* duration (/ 60.0 (metro-bpm tempoclock))) *latency* instrument
                  (/ (tempoclock t) 1000.0))
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
  ([name pattern &{ :keys [duration instrument quant]
                   :or { duration 4
                        instrument :default
                        quant 4}}]
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
           (atom (if
                     (= (class ~name) tPattern)
                   @(.running? ~name)
                   false))
           ~quant)))))
