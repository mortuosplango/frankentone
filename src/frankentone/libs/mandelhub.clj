(ns frankentone.libs.mandelhub
  (:use [overtone osc]
        [overtone.music time rhythm])
  (:import [java.util Date]
           [java.nio.channels DatagramChannel]
           [java.net DatagramSocket]))


(def mandelhub-instance (atom false))


(def ^:dynamic *latency-compensation* 0.005)
(def ^:dynamic *deviation-threshold* 0.02)
(def ^:dynamic *deviation-mul* 0.3)
(def ^:dynamic *hardness* 0.5)
(def ^:dynamic *quant* 16)


(def mandelclock (metronome 120.0))


(def clock-signal (atom {:clock-serial 0
                         :last-message 0.0
                         :external-tempo 2.0
                         :deviation-gate true
                         :bad-ticks 0}))



(defn metro-exact-beat
  "Get the current beat as a floating point number."
  [metro]
  (inc (/ (- (now) (metro-start metro)) (metro-tick metro))))


(defn metro-bps
  "Get the current beats per second (bps) or change the bps to 'new-bps'."
  ([metro]
     (/ (metro-bpm metro) 60.0))
  ([metro new-bps]
     (metro-bpm metro (* new-bps 60.0))))


(defn- post-chat [name message]
  (println (str name ": " message)))
(defn- post-shout [name message]
  (println name "(shout): " message))


(defn- filter-burst-messages [name id]
  (when (not= @mandelhub-instance false)
    (let [cur-beat (metro-exact-beat mandelclock)
          queue (into {}
                      ;; drop old message ids
                       (remove #(<= (val %) cur-beat)
                               (get (deref
                                     (:burst-guard-dict @mandelhub-instance)) name))) ]
      (if (not= id 0)
        (do
          ;; search for id
          (if (contains? queue (str id))
            false
            (do
              (swap!
               (:burst-guard-dict @mandelhub-instance)
               assoc
               name
               ;; add if not found
               (assoc queue (str id) (+ cur-beat 32)))
              true)))
        true))))


(defn- receive-tick
  [serial beat tempo]
  ;; only interpret a tick if it's a new one.
  (when (> serial (:clock-serial @clock-signal))
    (swap! clock-signal
           (fn [signal]
             (let [
                   last-tick-time (System/currentTimeMillis)
                   ;; compensate network latency (stupid)
                   beat (+ beat (* *latency-compensation* tempo))
                   deviation (- (metro-exact-beat mandelclock) beat)
                   tempo-has-changed (not= (:external-tempo signal) tempo)
                   ;;  if the deviationGate is open it should be more
                   ;;  difficult to close it again
                   this-deviation-threshold (if (:deviation-gate signal)
                                              (* *deviation-threshold* 0.25)
                                              *deviation-threshold*)]
               ;; TODO snap to quant
               (if (or (> (Math/abs deviation) this-deviation-threshold)
                       tempo-has-changed)
                 (do
                   (if tempo-has-changed
                     (metro-bps mandelclock tempo)
                     ;;  if five ticks were bad OR timing is really off
                     (when (or (> (:bad-ticks signal) 5)
                               (> (Math/abs deviation) (* *deviation-threshold* 5)))
                       (println "Large deviation: " deviation)
                       (metro-bps mandelclock
                                  (+ (* (metro-bps mandelclock)
                                        (- 1.0 *hardness*))
                                     (* (+ tempo
                                           (* deviation *deviation-mul* -1))
                                        *hardness*)))))
                   {
                    :deviation-gate true
                    :bad-ticks (inc (:bad-ticks signal))
                    ;; update internal state
                    :clock-serial serial
                    :last-message last-tick-time
                    :external-tempo tempo})
                 (do
                   ;; if our timing is good at the moment
                   (when (not= (metro-bps mandelclock) tempo)
                     (metro-bps mandelclock tempo))
                   { 
                    :bad-ticks 0
                    :deviation-gate false
                    ;; update internal state
                    :clock-serial serial
                    :last-message last-tick-time
                    :external-tempo tempo})))))))

(defn- add-handler [cmd action]
  (when (not= @mandelhub-instance false)
    (osc-handle (:server @mandelhub-instance)
                cmd
                (fn [msg]
                  (let [header {:cmd (first (:args msg))
                                :name (second (:args msg))
                                :id (nth (:args msg) 2)}
                        payload (nthrest (:args msg) 3)]
                    ;; TODO
                    )))))

;; (create-handlers (:server @mandelhub-instance))

(defn- create-handlers [server]
  ;; /mh/chat name msg-id message
  (let [out *out*]
    (osc-handle server "/mh/chat"
                (fn [msg]
                  (binding [*out* out]
                    (when (filter-burst-messages name (second (:args msg)))
                      (post-chat (first (:args msg)) (last (:args msg)))))))
    ;; /mh/shout name msg-id message
    (osc-handle server "/mh/shout"
                (fn [msg]
                  (binding [*out* out]
                    (when (filter-burst-messages name (second (:args msg)))
                      (post-shout (first (:args msg)) (last (:args msg)))))))
    ;; /mh/hello name msg-id
    (osc-handle server "/mh/hello"
                (fn [msg]
                  ;; TODO
                  ))
    ;; /mh/requestHello name msg-id
    (osc-handle server "/mh/requestHello"
                (fn [msg]
                  ;; TODO
                  ))
    ;; /mh/takeLead name msg-id
    (osc-handle server "/mh/takeLead"
                (fn [msg]
                  ;; TODO
                  ))
    ;;  /mh/clock name msg-id clock-serial beats tempo
    (future
      (do (osc-recv server "/mh/clock"
                    (fn [msg]
                      (binding [*out* out]
                        (println "You are now following"
                                 (first (:args msg)))
                        (metro-start mandelclock (dec (nth (:args msg) 3)))
                        (metro-bar-start mandelclock (dec (/ (nth (:args msg) 3) 4.0)))
                        (metro-bps mandelclock (nth (:args msg) 4)))))
          (println "test")
          (osc-handle server "/mh/clock"
                      (fn [msg]
                        (binding [*out* out]
                          (receive-tick
                           (nth (:args msg) 2)
                           (nth (:args msg) 3)
                           (nth (:args msg) 4)))))))))


(defn mandelhub-join
  "To join"
  ([name] (mandelhub-join name 57120))
  ([name port]
     (if (not @mandelhub-instance)
       (do
         (reset! mandelhub-instance
                 {:name name
                  :port port
                  :mandelhub (osc-client "255.255.255.255" port false)
                  :current-message-id (rand-int 5000)
                  :server (osc-server 57224)
                  :burst-guard-dict (atom {})
                  })
         (.setBroadcast (.socket (:chan (:mandelhub @mandelhub-instance))) true)
         (if (not (.getBroadcast (.socket (:chan (:mandelhub @mandelhub-instance)))))
           "Couldn't set broadcast flag! Disaster!")
         ;; to become a follower:
         ;; first: /mh/requestPort name, -1, port
         ;; then: /mh/hello name, 0 (0 can also be a unique id)
         (create-handlers (:server @mandelhub-instance))
         (osc-send (:mandelhub @mandelhub-instance) "/mh/requestPort" name -1.0 57224.0)
         (Thread/sleep 50)
         (osc-send (:mandelhub @mandelhub-instance) "/mh/hello" name 0.0)))))


(defn- next-message-id []
  (let [return (:current-message-id @mandelhub-instance)]
    (swap! mandelhub-instance update-in [:current-message-id] inc)
    return))

(defn- send-message-burst [cmd burst-type message]
  (when (not= @mandelhub-instance false)
    (let [burst (if (coll? burst-type)
                  burst-type
                  (burst-type {:time [2 0.25]
                               :critical [8 4]
                               :important [4 2]
                               :timeCritical [8 0.5]
                               :relaxed [4 8]}))
          message-id (float (next-message-id))
          burst-wait (* 1000 (/ (second burst) (first burst)))]
      (dotimes [i (first burst)]
        (osc-send (:mandelhub @mandelhub-instance) cmd
                  (:name @mandelhub-instance)
                  message-id message)
        (Thread/sleep burst-wait)))))

(defn mandelhub-chat [message]
  (future (send-message-burst "/mh/chat" :important message)))

(defn mandelhub-shout [message]
  (future (send-message-burst "/mh/shout" :important message)))

