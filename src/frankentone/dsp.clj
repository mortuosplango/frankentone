(ns frankentone.dsp
  (:use [clj-audio core sampled]
        [frankentone utils ugens])
   (:import [javax.sound.sampled
             AudioFormat
             AudioInputStream
             AudioSystem
             SourceDataLine
             TargetDataLine]
            [java.util Date]))


(defonce ^:dynamic *dsp* (atom nil))
(def ^:dynamic *default-buffer-size* (long (/ 8192 4)))
(defonce cplay (atom nil))


(defonce ^:dynamic *dsp-fun*
  (atom (let [pink (pink-c)
              last-samp (atom 0.0)]
          (fn [x chan]
            (if (zero? chan)
              (reset! last-samp (* 0.1 (pink)))
              @last-samp)))))


(def ^:dynamic *default-output-format*
  (make-format
   {:encoding :pcm-signed
    :sample-rate *sample-rate*
    :sample-size-in-bits 16
    :frame-rate *sample-rate*
    :frame-size 4
    :channels 2
    :endianness :little-endian}))


(defprotocol IDSPLoop
  (setFunc [_ func])
  (setCallbackFunc [_ func])
  (getCurrentTime [_])
  (getDspBuf [_])
  (stopDsp [_]))


(deftype tDSPLoop
    [^:unsynchronized-mutable ^clojure.lang.IFn dsp-func
     ^:unsynchronized-mutable ^java.lang.Boolean playing
     ^:unsynchronized-mutable ^double current-time
     ^:unsynchronized-mutable ^clojure.lang.IFn callback-fn
     ^long buffer-size
     ^java.nio.ByteBuffer bbuffer
     ^java.nio.ShortBuffer sbuffer
     ^java.nio.ShortBuffer dsp-buf]
  IDSPLoop
  (setFunc [_ func]
    (set! dsp-func func))
  (setCallbackFunc [_ func]
    (set! callback-fn func))
  (getCurrentTime [_]
    current-time)
  (getDspBuf [_]
    dsp-buf)
  (stopDsp [_]
    (set! playing false))

  clojure.lang.IFn
  (invoke [_]
     (let [
           num-channels (long (.getChannels ^AudioFormat *default-output-format*))
           num-channels-1 (dec num-channels)
           buffer-size-in-bytes (long (* buffer-size num-channels 2))
           buffer-size-in-bytes-1 (long (dec buffer-size-in-bytes))
           
           buffer-size-in-secs (/ buffer-size *sample-rate*)

           line (make-line :output *default-output-format*
                           buffer-size-in-bytes)

           scaling-factor (dec (Math/pow 2
                                         (dec
                                          (.getSampleSizeInBits
                                           ^AudioFormat
                                           *default-output-format*))))
           
           play-buffer (byte-array buffer-size-in-bytes)

           time-step (double (/ 1.0 *sample-rate*))
           c-time (atom 0.0)]
       
       (prn "dsp thread started")
       (set! current-time (- (+ (* 0.001 (System/currentTimeMillis))
                                buffer-size-in-secs)
                             time-step))
       (reset! c-time current-time)
       (with-data-line [#^SourceDataLine source line]
         (let [audio-stream (AudioInputStream.
                             (java.io.ByteArrayInputStream.
                              (.array bbuffer))
                             *default-format*
                             -1)
               sarray (short-array (* 8192 num-channels))]
           (if (= num-channels 2)
             (while playing
               ;; for timing:
               ;; (let [start (. System (nanoTime))]
               ;;   (prn (str "Elapsed time: "
               ;;             (/ (double (- (. System
               ;;                              (nanoTime)) start)) 1000000.0) " msecs")))
               (dotimes [_ buffer-size]
                 (.put sbuffer
                       (unchecked-short
                        (* scaling-factor
                           (dsp-func
                            (set! current-time
                                  (+ time-step
                                     current-time))
                            0))))
                 (.put sbuffer
                       (unchecked-short
                        (* scaling-factor
                           (dsp-func
                            current-time
                            1)))))
               (.position bbuffer buffer-size-in-bytes-1)
               (when callback-fn
                 (callback-fn))

               ;; play*
               (loop [cnt (long 0)]
                 (when (> cnt -1)
                   (when (> cnt 0)
                     (.write source play-buffer 0 cnt))
                   (recur (.read audio-stream play-buffer 0
                                 buffer-size-in-bytes))))
               (.clear bbuffer)
               (.clear sbuffer)
               (.reset audio-stream))
             (while playing
               (dotimes [_ buffer-size]
                 (set! current-time
                       (+ time-step
                          current-time))
                 (dotimes [chan num-channels]
                   (.put sbuffer
                         (unchecked-short
                          (* scaling-factor
                             (dsp-func
                              current-time
                              chan))))))
               (.position bbuffer buffer-size-in-bytes-1)

               ;; play*
               (loop [cnt (long 0)]
                 (when (> cnt -1)
                   (when (> cnt 0)
                     (.write source play-buffer 0 cnt))
                   (recur (.read audio-stream play-buffer 0
                                 buffer-size-in-bytes))))
               (.clear bbuffer)
               (.reset audio-stream))
             ))
         (prn "dsp thread stopped")))))


(defn current-time []
  (when @cplay
    (.getCurrentTime ^tDSPLoop @cplay)))


(defn constant-play
  "Play the given dsp function."
  ;; Accepts an optional listener function that will be called when a
  ;; line event is raised, taking the event type, the line and the stream
  ;; position as arguments. Returns the number of bytes played.
  ([atom-fn & [listener]]
     (let [num-channels (long (.getChannels ^AudioFormat *default-output-format*))
           buffer-size-in-bytes (long (* *default-buffer-size* num-channels 2))
           bbuffer (.order (java.nio.ByteBuffer/allocate buffer-size-in-bytes)
                           java.nio.ByteOrder/LITTLE_ENDIAN)
           sbuffer (.asShortBuffer bbuffer)
           dsp-buf (.asReadOnlyBuffer sbuffer)]
       (tDSPLoop. @atom-fn true 0.0 nil *default-buffer-size*
                  bbuffer sbuffer dsp-buf))))


(defn reset-dsp!
  "Try to reset the dsp-function to the given function after checking
  that it returns a double for every channel.

  The dsp function takes two arguments: the current system time in
  seconds and the channel"
  [new-dsp-fn]
  (if (fn? new-dsp-fn)
    (loop [i 0]
      (let [test-output (try
                          (new-dsp-fn (nows) i)
                          (catch Exception e
                            (str "caught exception: " (.getMessage e)))
                          )]
        (if (has-bad-value? test-output)
            (do (prn "Can't reset! Function returns result of type "
                     (type test-output))
                (if (string? test-output)
                  false
                  (type test-output)))
            (if (< (inc i) (.getChannels ^AudioFormat *default-output-format*))
              (recur (inc i))
              (do (reset! *dsp-fun* new-dsp-fn)
                  (if @cplay
                    (.setFunc @cplay new-dsp-fn))
                  true)))))
    (do (prn "Can't reset! Is not a function!")
        false)))

(defn silence!
  "Resets the dsp-function to silence."
  []
  (reset-dsp! (fn [x chan] 0.0)))


(defn white-noise!
  "Resets the dsp-function to white noise."
  []
  (reset-dsp! (fn [x chan] (- 1.0 (rand 2.0)))))


(defn sine!
  "Resets the dsp-function to a 440Hz sine wave."
  []
  (reset-dsp! (fn [x chan] (Math/sin (* TAU 440.0 x)))))


(defn start-dsp
  "Start the dsp engine."
  []
  (let [ncplay (constant-play *dsp-fun*)]
   (if (or (nil? @*dsp*) (not (.isAlive ^Thread @*dsp*)))
     (do
       (reset! cplay ncplay)
       (let [thread (Thread. #(ncplay))]
         (.setPriority thread Thread/MAX_PRIORITY)
         (.start thread)
         (reset! *dsp* thread)))
     (prn "dsp already running!"))))


(defn stop-dsp
  "Stop the dsp engine."
  []
  (if @cplay
   (.stopDsp @cplay)))


(defn kill-dsp
  "Kill the dsp engine."
  []
  (stop-dsp)
  (.stop ^Thread @*dsp*))


(comment
  (do
    (def ^:dynamic *dsp-running* (atom true))
    (def current-time (atom 0.0))
    (defn constant-play
      "Play the given dsp function.

  Accepts an optional listener function that will be called when a
  line event is raised, taking the event type, the line and the stream
  position as arguments. Returns the number of bytes played."
      ([atom-fn & [listener]]
         (let [
               buffer-size 8192
               num-channels (long (.getChannels ^AudioFormat *default-output-format*))

               buffer-size-in-bytes (* buffer-size num-channels 2)
               buffer-size-in-secs (/ buffer-size *sample-rate*)

               line (make-line :output *default-output-format*
                               buffer-size-in-bytes)

               scaling-factor (dec (Math/pow 2
                                             (dec
                                              (.getSampleSizeInBits
                                               ^AudioFormat
                                               *default-output-format*))))
               
               play-buffer (byte-array buffer-size-in-bytes)
               bbuffer (java.nio.ByteBuffer/allocate buffer-size-in-bytes)

               time-step (double (/ 1.0 *sample-rate*))

               p #(with-data-line [#^SourceDataLine source line]
                    (.order bbuffer java.nio.ByteOrder/LITTLE_ENDIAN)
                    (let [audio-stream (AudioInputStream.
                                        (java.io.ByteArrayInputStream.
                                         (.array bbuffer))
                                        *default-format*
                                        -1)]
                      (while (deref *dsp-running*)
                        ;;
                        ;; (time)
                        (loop [c-time (double @current-time)]
                          (dotimes [chan num-channels]
                            (.putShort bbuffer
                                       (unchecked-short
                                        (* scaling-factor
                                           (@atom-fn
                                            c-time
                                            chan)))))
                          (if (.hasRemaining bbuffer)
                            (recur (+ c-time time-step))
                            (reset! current-time (+ time-step
                                                    c-time))))

                        ;; play*
                        (loop [cnt (long 0)]
                          (when (> cnt -1)
                            (when (> cnt 0)
                              (.write source play-buffer 0 cnt))
                            (recur (.read audio-stream play-buffer 0
                                          buffer-size-in-bytes))))
                        (.clear bbuffer)
                        (.reset audio-stream)))
                    (prn "dsp thread stopped"))]
           
           (prn "dsp thread started")
           (reset! current-time (+ (* 0.001 (System/currentTimeMillis))
                                   buffer-size-in-secs))
           (if listener
             (with-line-listener line listener (p))
             (p)))
         ))))
