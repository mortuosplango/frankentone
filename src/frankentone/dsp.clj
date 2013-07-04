(ns frankentone.dsp
   (:use clj-audio.core
         clj-audio.sampled
         frankentone.utils
         frankentone.ugens)
   (:import [javax.sound.sampled
             AudioInputStream
             AudioSystem
             SourceDataLine
             TargetDataLine])
   (:import [java.util Date]))


(def ^:dynamic *dsp-running* (atom true))
(def ^:dynamic *dsp* (atom nil))
(def ^:dynamic *default-buffer-size* (/ 8192 4))


(def current-time (atom 0.0))


(def ^:dynamic *dsp-fun* (atom (let [pink (pink_c)
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


(defn constant-play
  "Play the given dsp function.

  Accepts an optional listener function that will be called when a
  line event is raised, taking the event type, the line and the stream
  position as arguments. Returns the number of bytes played."
  ([atom-fn & [listener]]
     (let [
           buffer-size 8192
           num-channels (long (.getChannels *default-output-format*))

           buffer-size-in-bytes (* buffer-size num-channels 2)
           buffer-size-in-secs (/ buffer-size *sample-rate*)

           line (make-line :output *default-output-format*
                           buffer-size-in-bytes)

           play-buffer (byte-array buffer-size-in-bytes)
           bbuffer (java.nio.ByteBuffer/allocate buffer-size-in-bytes)

           time-step (double (/ 1.0 *sample-rate*))
           
           p #(with-data-line [#^SourceDataLine source line]
                (.order bbuffer java.nio.ByteOrder/LITTLE_ENDIAN)
                (while (deref *dsp-running*)
                  (loop [c-time (double @current-time)]
                    (dotimes [chan num-channels]
                      (.putShort bbuffer
                                 (unchecked-scale-f->s
                                  (@atom-fn
                                   c-time
                                   chan))))
                    (if (.hasRemaining bbuffer)
                      (recur (+ c-time time-step))
                      (reset! current-time (+ time-step
                                              c-time))))
                  
                  ;; play*
                  (let [ audio-stream (AudioInputStream.
                                       (java.io.ByteArrayInputStream.
                                        (.array bbuffer))
                                       *default-format*
                                       -1)]
                    (loop [cnt (long 0)]
                      (when (> cnt -1)
                        (when (> cnt 0)
                          (.write source play-buffer 0 cnt))
                        (recur (.read audio-stream play-buffer 0
                                      buffer-size-in-bytes)))))
                  (.clear bbuffer))
                (prn "dsp thread stopped"))]
       
       (prn "dsp thread started")
       (reset! current-time (+ (* 0.001 (System/currentTimeMillis))
                               buffer-size-in-secs))
       (if listener
         (with-line-listener line listener (p))
         (p)))))


(defn reset-dsp!
  "Try to reset the dsp-function to the given function after checking
  that it returns a number.

  The dsp function takes two arguments: the current system time in
  seconds and the channel"
  [new-dsp-fn]
  (if (fn? new-dsp-fn)
    (loop [i 0]
      (let [test-output (new-dsp-fn (nows) i)]
        (if (has-bad-value? test-output)
            (prn "Can't reset! Function returns result of type "
                 (type test-output))
            (if (< (inc i) (.getChannels *default-output-format*))
              (recur (inc i))
              (reset! *dsp-fun* new-dsp-fn)))))
    (prn "Can't reset! Is not a function!")))


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
  (if (or (nil? @*dsp*) (not (.isAlive @*dsp*)))
    (do
      (let [thread (Thread. #(constant-play *dsp-fun*))]
        (.setPriority thread Thread/MAX_PRIORITY)
        (.start thread)
        (reset! *dsp-running* true)
        (reset! *dsp* thread)))
    (prn "dsp already running!")))


(defn stop-dsp
  "Stop the dsp engine."
  []
  (reset! *dsp-running* false))

