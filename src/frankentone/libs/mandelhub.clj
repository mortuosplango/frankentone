(ns frankentone.libs.mandelhub
  (:use [overtone osc]
        [overtone.music time rhythm])
  (:import [java.util Date]
           [java.nio.channels DatagramChannel]
           [java.net DatagramSocket]))


(defonce mandelhub-instance (atom false))


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


(defn metro-spb
  "Get the current seconds per beat (spb)."
  ([metro]
     (/ 60.0 (metro-bpm metro))))


(defn beat-s
  "Convert 'b' beats to seconds at the given 'bpm'."
  [b bpm] (* (/ 60.0 bpm) b))


(defn- post-chat [name message]
  (println (str name ": " message)))
(defn- post-shout [name message]
  (println name "(shout): " message))


(defn- filter-burst-messages [name id]
  (when @mandelhub-instance
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


(defn- next-message-id []
  (let [return (:current-message-id @mandelhub-instance)]
    (swap! mandelhub-instance update-in [:current-message-id] inc)
    return))

(defn- send-message-burst [cmd burst-type & [message]]
  (when @mandelhub-instance
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
        (if message
          (osc-send (:mandelhub @mandelhub-instance) cmd
                    (:name @mandelhub-instance)
                    message-id message)
          (osc-send (:mandelhub @mandelhub-instance) cmd
                    (:name @mandelhub-instance)
                    message-id))
        (Thread/sleep burst-wait)))))


(defn- send-hello []
  (send-message-burst "/hello" :relaxed))
(defn- send-request-hello []
  (send-message-burst "/requestHello" :relaxed))
(defn- send-request-sync []
  (send-message-burst "/requestSync" :critical))


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
  (when @mandelhub-instance
    (osc-handle (:server @mandelhub-instance)
                cmd
                (fn [msg]
                  (let [name (first (:args msg))
                        id (second (:args msg))
                        payload (nthrest (:args msg) 2)]
                    (when (filter-burst-messages name id)
                      (action name id payload)))))))


(defn- create-handlers [server]
  (let [out *out*]
    ;; /mh/chat name msg-id message
    (add-handler "/mh/chat"
                 (fn [name id payload]
                  (binding [*out* out]
                    (post-chat name (first payload)))))
    ;; /mh/shout name msg-id message
    (add-handler "/mh/shout"
                 (fn [name id payload]
                   (binding [*out* out]
                     (post-shout name (first payload)))))
    ;; /mh/hello name msg-id
    (add-handler "/mh/hello"
                 (fn [name id payload]
                   (binding [*out* out]
                     (if (= (:leader @mandelhub-instance)
                            name)
                       (println name "is the leader.")
                       (println name "is following the leader.")))))
    ;; /mh/requestHello name msg-id
    (add-handler "/mh/requestHello"
                 (fn [name id payload]
                   (when (filter-burst-messages name id)
                     (send-hello))))
    ;; /mh/takeLead name msg-id
    (add-handler "/mh/takeLead"
                 (fn [name id payload]
                   (when (filter-burst-messages name id)
                     (println name "is our new leader!")
                     (swap! mandelhub-instance
                            assoc :leader name))))
    ;;  /mh/clock name msg-id clock-serial beats tempo
    (future
      (do (osc-recv server "/mh/clock"
                    (fn [msg]
                      (binding [*out* out]
                        (println "You are now following"
                                 (first (:args msg)))
                        (swap! mandelhub-instance assoc :leader (first (:args msg)))
                        (metro-start mandelclock (dec (nth (:args msg) 3)))
                        (metro-bar-start mandelclock (dec (/ (nth (:args msg) 3) 4.0)))
                        (metro-bps mandelclock (nth (:args msg) 4)))))
          (add-handler "/mh/clock"
                       (fn [_ _ [serial beat tempo]]
                         (binding [*out* out]
                          (receive-tick serial beat tempo))))))))


(defn mandelhub-join
  "To join"
  ([name] (mandelhub-join name 57120))
  ([name port]
     (when-not @mandelhub-instance
       (let [rec-port 57220]
         (reset! mandelhub-instance
                 {:name name
                  :port port
                  :leader ""
                  :mandelhub (osc-client "255.255.255.255" port false)
                  :current-message-id (rand-int 5000)
                  :server (osc-server rec-port)
                  :burst-guard-dict (atom {})
                  })
         (.setBroadcast (.socket (:chan (:mandelhub @mandelhub-instance))) true)
         (when-not (.getBroadcast (.socket (:chan (:mandelhub @mandelhub-instance))))
           (println "Couldn't set broadcast flag! Disaster!"))
         ;; to become a follower:
         ;; first: /mh/requestPort name, -1, port
         ;; then: /mh/hello name, 0 (0 can also be a unique id)
         (create-handlers (:server @mandelhub-instance))
         (osc-send (:mandelhub @mandelhub-instance) "/mh/requestPort" name -1.0 (double rec-port))
         (Thread/sleep 500)
         (send-hello)
         (Thread/sleep 250)
         (send-request-sync)))))


(defn mandelhub-chat [message]
  (future (send-message-burst "/mh/chat" :important message)))

(defn mandelhub-shout [message]
  (future (send-message-burst "/mh/shout" :important message)))

;; (mandelhub-join "holger")
;; (mandelhub-chat "hello")
;; (mandelhub-shout "hello")
;; (create-handlers (:server @mandelhub-instance))
