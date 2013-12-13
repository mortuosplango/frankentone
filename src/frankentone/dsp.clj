(ns frankentone.dsp
  (:use [frankentone utils ugens])
  (:require [hiphip.impl.core :as hip :only dotimes-int])
  (:import [java.util List]
           [java.nio FloatBuffer])
  (:import [org.jaudiolibs.audioservers
            AudioClient
            AudioConfiguration
            AudioServer]
           [org.jaudiolibs.audioservers.javasound
            JavasoundAudioServer
            JavasoundAudioServer$TimingMode]))


(defonce dsp-thread (atom nil))

(defonce audio-server (atom nil))
(defonce audio-client (atom nil))

(def ^:dynamic *num-channels* 2)
(def ^:dynamic *default-buffer-size* (long (/ 8192 4)))

(defonce ^:dynamic *dsp-fun*
  (atom (let [pink (pink-c)
              last-samp (atom 0.0)]
          (fn [x chan]
            (if (zero? chan)
              (reset! last-samp (* 0.1 (pink)))
              @last-samp)))))


(defprotocol IDSPLoop
  (setFunc [_ func])
  (setCallbackFunc [_ func])
  (getCurrentDspTime [_])
  (getOutputBuffers [_]))


(deftype tDSPLoop
    [^:unsynchronized-mutable ^clojure.lang.IFn dsp-func
     ^:unsynchronized-mutable ^clojure.lang.IFn callback-fn
     ^:unsynchronized-mutable ^Double current-time
     ^double time-step
     ^:unsynchronized-mutable output-buffers]
  IDSPLoop
  (setFunc [_ func]
    (set! dsp-func func))
  (setCallbackFunc [_ func]
    (set! callback-fn func))
  (getOutputBuffers [_]
    output-buffers)
  (getCurrentDspTime [_]
    current-time)
  AudioClient

  ;; TODO DO NOT assume that the AudioConfiguration you passed into the
  ;; server create() method is the same. The AudioServer implementation
  ;; will give you its closest match. Throw an Exception if your code
  ;; cannot handle the provided configuration. 
  (configure [_ context])

  ;; The client process() method will then be called for each audio
  ;; buffer. You can return false from this method to stop the server
  ;; processing audio.
  (process [_ time
            inputs
            [output-l output-r] num-frames]
    (when (zero? current-time)
      (set! current-time (* time 1E-9))
      (set! output-buffers [output-l output-r]))
    (hip/dotimes-int [i num-frames]
                     (.put ^FloatBuffer output-l
                           (float (dsp-func current-time 0)))
                     (.put ^FloatBuffer output-r
                           (float (dsp-func current-time 1)))
                     (set! current-time (+ current-time time-step)))
    (when callback-fn
      (callback-fn))
    true)

  ;; The client shutdown() method will be called when the AudioServer is
  ;; shut down so that the client can clean up after itself. 
  (shutdown [_]))


(defn start-dsp
  "Start the dsp engine."
  []
  (let [naudio-client (tDSPLoop.
                       @*dsp-fun*
                       nil
                       0.0
                       sample-dur
                       nil)
        naudio-server (JavasoundAudioServer/create
                       "" ;; device
                         (AudioConfiguration.
                          *sample-rate*, ;;sample rate
                          0,       ;; input channels
                          *num-channels*,       ;; output channels
                          *default-buffer-size*   ;;buffer size
                          true     ;; is buffer size fixed?
                          )    
                         JavasoundAudioServer$TimingMode/Estimated
                         naudio-client)]
    (if (or (nil? @dsp-thread) (not (.isAlive ^Thread @dsp-thread)))
      (do
        (reset! audio-server naudio-server)
        (reset! audio-client naudio-client)
        (let [thread (Thread. #(.run naudio-server))]
          (.setPriority thread Thread/MAX_PRIORITY)
          (.start thread)
          (reset! dsp-thread thread)))
      (prn "dsp already running!"))))


(defn stop-dsp
  "Stop the dsp engine."
  []
  (if (and @audio-server (.isActive ^JavasoundAudioServer @audio-server))
    (.shutdown ^JavasoundAudioServer @audio-server)))


(defn kill-dsp
  "Kill the dsp engine."
  []
  (stop-dsp)
  (.stop ^Thread @dsp-thread))


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
                            (println "caught exception:\n"
                                     (.getMessage e))))]
        (if (has-bad-value? test-output)
          (do (println "Can't reset! Function returns result of type"
                       (type test-output))
              (if (string? test-output)
                false
                (type test-output)))
          (if (< (inc i) *num-channels*)
            (recur (inc i))
            (do (reset! *dsp-fun* new-dsp-fn)
                (if (and @audio-server (.isActive @audio-server))
                  (.setFunc @audio-client new-dsp-fn))
                true)))))
    (do (println "Can't reset! Is not a function!")
        false)))


(defn silence!
  "Resets the dsp-function to silence."
  []
  (reset-dsp! (fn [x chan] 0.0)))


(defn white-noise!
  "Resets the dsp-function to white noise."
  ([] (white-noise! 0.1))
  ([amp] (reset-dsp! (fn-c [x chan] (* amp (white-noise-c))))))


(defn pink-noise!
  "Resets the dsp-function to white noise."
  ([] (pink-noise! 0.1))
  ([amp] (reset-dsp! (fn-c [x chan] (* amp (if (zero? chan)
                                             (pink-c)
                                             (pink-c)))))))

(defn sine!
  "Resets the dsp-function to a 440Hz sine wave."
  ([] (sine! 0.1))
  ([amp] (reset-dsp! (fn [x chan] (* amp (Math/sin (* TAU 440.0 x)))))))


(defn current-dsp-time []
  (when @audio-client
    (.getCurrentDspTime ^tDSPLoop @audio-client)))
