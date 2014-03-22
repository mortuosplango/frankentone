;; Heavily inspired by Alex McLean's Tidal.

(ns frankentone.patterns
  (:use [frankentone instruments utils]
        [frankentone.entropy selfmod]
        [clojure walk]
        [overtone.music pitch time]))


(declare play-pattern)


(def || :|)


(defn- do-play-pattern [coll length offset default-instrument now default-amp default-pitch]
  (let [len (count coll)
        note-length (* length (/ 1 len))]
    (doall (mapv
            (fn [inst beat]
              (let [inst (if (var? inst) @inst inst)
                    inst-fn (filter #(= (getFunction ^PInstrument2 (val %)) inst) @instruments)]
                (cond
                 (seq inst-fn)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            (key (first inst-fn))
                            440.0 default-amp note-length)
                 (keyword? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            inst 440.0 default-amp note-length)
                 (coll? inst)
                 ;; if event-style map
                 (if (and
                      (map? inst)
                      (some #(contains? inst %)
                            [:inst :freq :pitch :amp :sustain]))
                   ;; ignore unknown keys
                   (let [inst (into {} (map #(when-let [v (get inst %)]
                                               {% v}) [:inst :freq :pitch :amp :sustain]))]
                     ;; if map contains seqs
                     ;; split the map into submaps
                     (if (some #(coll? (get inst %)) [:inst :freq :pitch :amp :sustain])
                       (let [ ;; look for the longest list
                             max-len (-> (sort-by (comp count val) >
                                                  (into {}
                                                        (filter (comp coll? val) inst)))
                                         first val count)]
                         (play-pattern
                          (map (fn [pos]
                                 (into {}
                                       (map (fn [key]
                                              (let [v (get inst key)]
                                                (if v
                                                  {key (if (coll? v)
                                                         ;; cycle through the others
                                                         (nth (cycle v)
                                                              pos)
                                                         v)})))
                                            [:inst :freq :pitch :amp :sustain]))) (range max-len))
                          note-length
                          (+ offset (* beat note-length))
                          default-instrument
                          now default-amp default-pitch))
                       ;; play only if the values of everything except
                       ;; :inst is a number or a string
                       (when-not (some #(not (or (string? %) (number? %))) (vals (dissoc inst :inst)))
                         (play-note (+ now offset 0.1
                                       (* beat note-length))
                                    (if (contains? inst :inst)
                                      (:inst inst)
                                      default-instrument)
                                    (cond
                                     (contains? inst :pitch) (midi->hz (:pitch inst))
                                     (contains? inst :freq) (:freq inst)
                                     :default default-pitch)
                                    (if (contains? inst :amp)
                                      (:amp inst)
                                      default-amp)
                                    (if (contains? inst :sustain)
                                      (:sustain inst)
                                      note-length)))))
                   (play-pattern inst note-length
                                 (+ offset (* beat note-length))
                                 default-instrument
                                 now default-amp default-pitch))
                 (number? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            default-instrument (midi->hz inst) default-amp note-length)
                 (string? inst)
                 (play-note (+ now offset 0.1
                               (* beat note-length))
                            default-instrument inst default-amp note-length)
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
     running?]
  clojure.lang.IFn
  (invoke [this t]
    (play-pattern (pattern-fn)
                  duration *latency* instrument)
    ;; (println pat-name)
    (when @running?
      (let [next-t (+ t (* duration 1000))]
        (apply-at next-t pat-name [next-t]))))
  Pattern
  (start [this]
    (when-not @running?
      (reset! running? true)
      (this (+ (now) *latency*))))
  (stop [this]
    (reset! running? false)))


(defmacro defpat
  ([name pattern &{ :keys [duration instrument]
                   :or {duration 2.0
                        instrument :default}}]
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
                   false)))))))
